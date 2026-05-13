"""Unit tests for ccpp_validator."""

import doctest
import os
import tempfile
import textwrap
import unittest

import ccpp_validator as val_mod
from ccpp_validator import (
    _join_continuation,
    _parse_subroutines,
    _load_source_tree,
    validate,
)

_SAMPLE_DIR = os.path.join(os.path.dirname(__file__), 'sample_files')
_CORRECT_F90 = os.path.join(_SAMPLE_DIR, 'scheme_multipart_correct.F90')
_WRONG_F90   = os.path.join(_SAMPLE_DIR, 'scheme_multipart_wrong_args.F90')
_SCHEME_META = os.path.join(_SAMPLE_DIR, 'scheme_multipart.meta')


class TestJoinContinuation(unittest.TestCase):

    def test_no_continuation(self):
        lines = ['  foo\n', '  bar\n']
        self.assertEqual(_join_continuation(lines), ['  foo', '  bar'])

    def test_single_continuation(self):
        lines = ['  foo &\n', '  bar\n']
        result = _join_continuation(lines)
        self.assertEqual(len(result), 1)
        self.assertIn('foo', result[0])
        self.assertIn('bar', result[0])

    def test_multi_continuation(self):
        lines = ['  a &\n', '  b &\n', '  c\n']
        result = _join_continuation(lines)
        self.assertEqual(len(result), 1)
        self.assertIn('a', result[0])
        self.assertIn('c', result[0])

    def test_comment_after_continuation(self):
        lines = ['  foo & ! comment\n', '  bar\n']
        result = _join_continuation(lines)
        self.assertEqual(len(result), 1)
        self.assertNotIn('comment', result[0])

    def test_dual_form_strips_leading_ampersand(self):
        # F77 / fixed-form: ``&`` at column 6 of the continuation line
        # is a continuation marker and must be stripped before the
        # continued expression is appended to the buffer.  Without
        # this, the joined logical line carries ``&`` glued into the
        # middle of the expression.
        lines = ['  foo &\n', '     & bar &\n', '     & baz\n']
        result = _join_continuation(lines)
        self.assertEqual(len(result), 1)
        self.assertNotIn('&', result[0])
        self.assertIn('foo', result[0])
        self.assertIn('bar', result[0])
        self.assertIn('baz', result[0])

    def test_dual_form_then_free_form_mixed(self):
        # Files that use dual-form for one signature and free-form for
        # another (or mix within a single signature) must still join
        # correctly.
        lines = [
            '  start &\n',         # trailing &
            '     & middle &\n',   # leading & + trailing &
            '       end\n',        # no leading &, plain continuation tail
        ]
        result = _join_continuation(lines)
        self.assertEqual(len(result), 1)
        self.assertNotIn('&', result[0])
        for tok in ('start', 'middle', 'end'):
            self.assertIn(tok, result[0])

    def test_comment_line_between_continuation_lines(self):
        # Real-world case (sfc_diff.f::stability): a comment-only line
        # appears between two continuation lines.  Fortran 90+ allows
        # this; the join must skip the comment line, not terminate.
        lines = [
            '      subroutine stability                       &\n',
            '!  ---  inputs:\n',
            '     &     ( z1, zvfun, grav,                    &\n',
            '!  ---  outputs:\n',
            '     &       rb, fm, fh, cm, ch, ustar)\n',
        ]
        result = _join_continuation(lines)
        self.assertEqual(len(result), 1)
        self.assertIn('subroutine stability', result[0])
        self.assertIn('z1', result[0])
        self.assertIn('ustar', result[0])
        # Closing paren survives.
        self.assertIn(')', result[0])
        self.assertNotIn('&', result[0])

    def test_blank_line_between_continuation_lines(self):
        # Blank lines mid-continuation are also legal under F90+.
        lines = [
            '      foo &\n',
            '\n',
            '   &  bar &\n',
            '     \n',
            '   &  baz\n',
        ]
        result = _join_continuation(lines)
        self.assertEqual(len(result), 1)
        for tok in ('foo', 'bar', 'baz'):
            self.assertIn(tok, result[0])

    def test_rrtmg_style_signature_round_trip(self):
        # The real-world failure mode that motivated the fix: a
        # fixed-form subroutine signature with 57 args spread across
        # 16 dual-form continuation lines.  After joining, ``(`` must
        # appear immediately after the subroutine name (no stray
        # ``&`` in between) so the signature regex can pick up the
        # arg list.
        src_lines = [
            '      subroutine rrtmg_lw_run                                           &\n',
            '     &     ( plyr,plvl,tlyr,tlvl,qlyr,olyr,                             &\n',
            '     &       icseed,aeraod,aerssa,                                      &\n',
            '     &       errmsg, errflg                                             &\n',
            '     &     )\n',
        ]
        result = _join_continuation(src_lines)
        self.assertEqual(len(result), 1)
        self.assertNotIn('&', result[0])
        # ``subroutine rrtmg_lw_run`` is followed (after whitespace) by ``(``.
        import re as _re
        self.assertIsNotNone(
            _re.search(r'subroutine\s+rrtmg_lw_run\s*\(', result[0]),
            'joined signature lacks ``subroutine NAME (`` shape — got: {!r}'
            .format(result[0]),
        )


class TestParseSubroutines(unittest.TestCase):

    def test_simple_subroutine(self):
        src = 'subroutine foo(a, b, c)\nend subroutine foo\n'
        result = _parse_subroutines(src)
        self.assertIn('foo', result)
        self.assertEqual(result['foo'].args, ['a', 'b', 'c'])
        self.assertEqual(result['foo'].optional, set())

    def test_case_insensitive_name(self):
        src = 'SUBROUTINE MyScheme_run(errmsg, errflg)\nend subroutine\n'
        result = _parse_subroutines(src)
        self.assertIn('myscheme_run', result)

    def test_no_args(self):
        src = 'subroutine bar()\nend subroutine bar\n'
        result = _parse_subroutines(src)
        self.assertEqual(result['bar'].args, [])

    def test_pure_prefix(self):
        src = 'pure subroutine baz(x)\nend subroutine\n'
        result = _parse_subroutines(src)
        self.assertIn('baz', result)
        self.assertEqual(result['baz'].args, ['x'])

    def test_elemental_prefix(self):
        src = 'elemental subroutine qux(y)\nend subroutine\n'
        result = _parse_subroutines(src)
        self.assertIn('qux', result)

    def test_continuation_args(self):
        src = 'subroutine foo(a, b, &\n  c, d)\nend subroutine\n'
        result = _parse_subroutines(src)
        self.assertIn('foo', result)
        self.assertEqual(sorted(result['foo'].args), ['a', 'b', 'c', 'd'])

    def test_multiple_subroutines(self):
        src = (
            'subroutine foo(a)\nend subroutine foo\n'
            'subroutine bar(x, y)\nend subroutine bar\n'
        )
        result = _parse_subroutines(src)
        self.assertIn('foo', result)
        self.assertIn('bar', result)

    def test_nested_subroutine_ignored_if_same_name(self):
        # first occurrence wins
        src = (
            'subroutine foo(a)\n'
            '  subroutine foo(b, c)\n'
            '  end subroutine\n'
            'end subroutine foo\n'
        )
        result = _parse_subroutines(src)
        self.assertEqual(result['foo'].args, ['a'])

    def test_no_parentheses(self):
        # some Fortran compilers allow omitting () for no-arg subs
        src = 'subroutine foo\nend subroutine foo\n'
        result = _parse_subroutines(src)
        self.assertIn('foo', result)
        self.assertEqual(result['foo'].args, [])

    def test_fixed_form_dual_continuation_signature(self):
        # Regression: rrtmg_lw_run-style F77 signatures use ``&`` at
        # both line ends.  Pre-fix this gave ``args == []``.
        src = (
            '      subroutine rrtmg_lw_run                                  &\n'
            '     &     ( plyr,plvl,tlyr,tlvl,qlyr,olyr,                    &\n'
            '     &       icseed,aeraod,aerssa, sfemis, sfgtmp,             &\n'
            '     &       errmsg, errflg                                    &\n'
            '     &     )\n'
            '      end subroutine rrtmg_lw_run\n'
        )
        result = _parse_subroutines(src)
        self.assertIn('rrtmg_lw_run', result)
        args = result['rrtmg_lw_run'].args
        self.assertEqual(args, [
            'plyr', 'plvl', 'tlyr', 'tlvl', 'qlyr', 'olyr',
            'icseed', 'aeraod', 'aerssa', 'sfemis', 'sfgtmp',
            'errmsg', 'errflg',
        ])


class TestLoadSourceTree(unittest.TestCase):

    def test_loads_correct_file(self):
        tree = _load_source_tree([_CORRECT_F90])
        self.assertIn('temp_calc_adjust_run', tree)
        self.assertIn('temp_calc_adjust_init', tree)
        self.assertIn('temp_calc_adjust_final', tree)

    def test_args_from_correct_file(self):
        tree = _load_source_tree([_CORRECT_F90])
        run_args = tree['temp_calc_adjust_run'].args
        self.assertEqual(sorted(run_args), ['errflg', 'errmsg', 'im', 'temp', 'timestep'])

    def test_merges_multiple_files(self):
        src1 = textwrap.dedent("""\
            subroutine aaa(x)
            end subroutine aaa
        """)
        src2 = textwrap.dedent("""\
            subroutine bbb(y, z)
            end subroutine bbb
        """)
        with tempfile.NamedTemporaryFile(suffix='.F90', mode='w', delete=False) as f1:
            f1.write(src1)
            p1 = f1.name
        with tempfile.NamedTemporaryFile(suffix='.F90', mode='w', delete=False) as f2:
            f2.write(src2)
            p2 = f2.name
        try:
            tree = _load_source_tree([p1, p2])
            self.assertIn('aaa', tree)
            self.assertIn('bbb', tree)
        finally:
            os.unlink(p1)
            os.unlink(p2)


class TestValidateCorrect(unittest.TestCase):

    def test_no_errors_on_correct_source(self):
        errors = validate([_SCHEME_META], [_CORRECT_F90])
        self.assertEqual(errors, [])


class TestValidateWrongArgs(unittest.TestCase):

    def setUp(self):
        self.errors = validate([_SCHEME_META], [_WRONG_F90])

    def test_has_errors(self):
        self.assertGreater(len(self.errors), 0)

    def test_arg_count_error_for_init(self):
        init_errs = [e for e in self.errors if 'temp_calc_adjust_init' in e]
        self.assertGreater(len(init_errs), 0)

    def test_renamed_arg_error_for_run(self):
        run_errs = [e for e in self.errors if 'temp_calc_adjust_run' in e]
        self.assertGreater(len(run_errs), 0)


class TestDegenerateParseHint(unittest.TestCase):
    """When the Fortran signature parser finds a subroutine but extracts
    zero args while metadata declares many, the error message must
    surface a HINT pointing at the parser rather than masquerading as
    a routine mismatch.  Triggered most commonly by an unsupported
    continuation style; reproduced here with a parens-less Fortran
    sub paired with multi-arg metadata."""

    _META = (
        '[ccpp-table-properties]\n'
        '  name = bogus_scheme\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = bogus_scheme_run\n'
        '  type = scheme\n'
        '[ a ]\n'
        '  standard_name = horizontal_dimension\n'
        '  units = count\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '  intent = in\n'
        '[ b ]\n'
        '  standard_name = air_temperature\n'
        '  units = K\n'
        '  dimensions = (horizontal_dimension)\n'
        '  type = real | kind = kind_phys\n'
        '  intent = inout\n'
    )

    _F90 = (
        '! Subroutine has no parentheses → parser yields zero args, but\n'
        '! the metadata declares two.  Hint must fire.\n'
        'subroutine bogus_scheme_run\n'
        'end subroutine bogus_scheme_run\n'
    )

    def setUp(self):
        self.tmpdir = tempfile.mkdtemp()
        self.meta_path = os.path.join(self.tmpdir, 'bogus_scheme.meta')
        self.f90_path  = os.path.join(self.tmpdir, 'bogus_scheme.F90')
        with open(self.meta_path, 'w') as fh:
            fh.write(self._META)
        with open(self.f90_path, 'w') as fh:
            fh.write(self._F90)

    def tearDown(self):
        import shutil
        shutil.rmtree(self.tmpdir, ignore_errors=True)

    def test_hint_fires_on_zero_fortran_args(self):
        errors = validate([self.meta_path], [self.f90_path])
        count_errs = [e for e in errors if 'Argument count mismatch' in e]
        self.assertEqual(len(count_errs), 1, errors)
        msg = count_errs[0]
        self.assertIn('HINT', msg)
        self.assertIn('zero arguments', msg)
        self.assertIn('parser', msg)


class TestOptionalArgsParsing(unittest.TestCase):
    """_parse_subroutines collects optional-attribute arg names."""

    def test_optional_first_attr(self):
        src = textwrap.dedent("""\
            subroutine foo(a, b)
              integer, optional, intent(in) :: a
              integer, intent(in) :: b
            end subroutine foo
        """)
        sig = _parse_subroutines(src)['foo']
        self.assertEqual(sig.args, ['a', 'b'])
        self.assertEqual(sig.optional, {'a'})

    def test_optional_after_intent(self):
        src = textwrap.dedent("""\
            subroutine foo(a, b)
              integer, intent(out), optional :: b
              integer, intent(in) :: a
            end subroutine foo
        """)
        sig = _parse_subroutines(src)['foo']
        self.assertEqual(sig.optional, {'b'})

    def test_multiple_vars_one_decl(self):
        src = textwrap.dedent("""\
            subroutine foo(a, b, c)
              real, optional :: a, b(:,:), c
            end subroutine foo
        """)
        sig = _parse_subroutines(src)['foo']
        self.assertEqual(sig.optional, {'a', 'b', 'c'})

    def test_no_optional(self):
        src = textwrap.dedent("""\
            subroutine foo(a)
              integer, intent(in) :: a
            end subroutine foo
        """)
        sig = _parse_subroutines(src)['foo']
        self.assertEqual(sig.optional, set())

    def test_optional_token_in_string_or_comment_ignored(self):
        src = textwrap.dedent("""\
            subroutine foo(a)
              integer, intent(in) :: a   ! this is optional, but a comment
            end subroutine foo
        """)
        sig = _parse_subroutines(src)['foo']
        self.assertEqual(sig.optional, set())


class TestValidateOptionalArgs(unittest.TestCase):
    """Optional Fortran-only args are silently allowed in validation."""

    _META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_scheme
          type = scheme

        [ccpp-arg-table]
          name = my_scheme_run
          type = scheme
        [ a ]
          standard_name = std_a
          units = none
          dimensions = ()
          type = integer
          intent = in
        [ b ]
          standard_name = std_b
          units = none
          dimensions = ()
          type = integer
          intent = in
    """)

    _F90_OK = textwrap.dedent("""\
        module my_scheme
        contains
          subroutine my_scheme_run(a, b, c, d, e)
            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, optional, intent(in)  :: c
            integer, optional, intent(out) :: d
            integer, optional, intent(in)  :: e
          end subroutine my_scheme_run
        end module my_scheme
    """)

    _F90_REQUIRED_EXTRA = textwrap.dedent("""\
        module my_scheme
        contains
          subroutine my_scheme_run(a, b, c)
            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: c
          end subroutine my_scheme_run
        end module my_scheme
    """)

    def _write_files(self, fortran_src):
        with tempfile.NamedTemporaryFile(suffix='.meta', mode='w', delete=False) as fm:
            fm.write(self._META)
            meta_path = fm.name
        with tempfile.NamedTemporaryFile(suffix='.F90', mode='w', delete=False) as ff:
            ff.write(fortran_src)
            f90_path = ff.name
        self._cleanup = [meta_path, f90_path]
        return meta_path, f90_path

    def tearDown(self):
        for f in getattr(self, '_cleanup', []):
            os.unlink(f)

    def test_optional_fortran_only_args_allowed(self):
        meta, f90 = self._write_files(self._F90_OK)
        errors = validate([meta], [f90])
        self.assertEqual(errors, [], 'unexpected errors: ' + repr(errors))

    def test_non_optional_extra_fortran_arg_errors(self):
        meta, f90 = self._write_files(self._F90_REQUIRED_EXTRA)
        errors = validate([meta], [f90])
        self.assertTrue(any('Non-optional arguments in Fortran' in e
                            for e in errors),
                        'expected non-optional-extra error, got: ' + repr(errors))


class TestValidateMissingSubroutine(unittest.TestCase):

    def setUp(self):
        # Only supply a file with temp_calc_adjust_init, missing run and final.
        src = textwrap.dedent("""\
            module temp_calc_adjust
            contains
              subroutine temp_calc_adjust_init(im, errmsg, errflg)
              end subroutine temp_calc_adjust_init
            end module temp_calc_adjust
        """)
        with tempfile.NamedTemporaryFile(suffix='.F90', mode='w', delete=False) as fh:
            fh.write(src)
            self._f90 = fh.name

    def tearDown(self):
        os.unlink(self._f90)

    def test_missing_subroutine_reported(self):
        errors = validate([_SCHEME_META], [self._f90])
        missing = [e for e in errors if 'not found' in e]
        self.assertGreater(len(missing), 0)


class TestValidateMultipleSources(unittest.TestCase):
    """Subroutines split across multiple files."""

    def setUp(self):
        init_src = textwrap.dedent("""\
            module a
            contains
              subroutine temp_calc_adjust_init(im, errmsg, errflg)
              end subroutine
            end module a
        """)
        run_src = textwrap.dedent("""\
            module b
            contains
              subroutine temp_calc_adjust_run(im, timestep, temp, errmsg, errflg)
              end subroutine
            end module b
        """)
        final_src = textwrap.dedent("""\
            module c
            contains
              subroutine temp_calc_adjust_final(errmsg, errflg)
              end subroutine
            end module c
        """)
        self._files = []
        for src in (init_src, run_src, final_src):
            with tempfile.NamedTemporaryFile(suffix='.F90', mode='w', delete=False) as fh:
                fh.write(src)
                self._files.append(fh.name)

    def tearDown(self):
        for f in self._files:
            os.unlink(f)

    def test_no_errors_split_across_files(self):
        errors = validate([_SCHEME_META], self._files)
        self.assertEqual(errors, [])


class TestSourcePathAutoDiscovery(unittest.TestCase):
    """Validator auto-discovers .F90 when source_files is omitted."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        # Write a .meta file whose source_path points to a subdirectory.
        self._src_subdir = os.path.join(self._tmpdir, 'fortran')
        os.makedirs(self._src_subdir)

        meta_src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = myscheme
              type = scheme
              source_path = fortran
            [ccpp-arg-table]
              name = myscheme_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
            [ errflg ]
              standard_name = ccpp_error_code
              units = 1
              dimensions = ()
              type = integer
              intent = out
        """)
        self._meta = os.path.join(self._tmpdir, 'myscheme.meta')
        with open(self._meta, 'w') as fh:
            fh.write(meta_src)

        # Correct .F90 in the source_path subdirectory.
        fort_correct = textwrap.dedent("""\
            module myscheme
            contains
              subroutine myscheme_run(errmsg, errflg)
              end subroutine myscheme_run
            end module myscheme
        """)
        self._fort = os.path.join(self._src_subdir, 'myscheme.F90')
        with open(self._fort, 'w') as fh:
            fh.write(fort_correct)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_auto_discovers_correct_source(self):
        errors = validate([self._meta])
        self.assertEqual(errors, [])

    def test_auto_discovers_wrong_source(self):
        # Overwrite with wrong arg list.
        with open(self._fort, 'w') as fh:
            fh.write(textwrap.dedent("""\
                module myscheme
                contains
                  subroutine myscheme_run(errmsg)
                  end subroutine myscheme_run
                end module myscheme
            """))
        errors = validate([self._meta])
        self.assertGreater(len(errors), 0)


class TestFortranFileForTable(unittest.TestCase):
    """Tests for _fortran_file_for_table helper."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def _make_table(self, source_path=''):
        from metadata.parse_tools import ParseContext
        from metadata.metadata_table import MetadataTable
        meta = os.path.join(self._tmpdir, 'foo.meta')
        open(meta, 'w').close()
        ctx = ParseContext(0, meta)
        t = MetadataTable('foo', 'scheme', meta, ctx)
        props = {}
        if source_path:
            props['source_path'] = source_path
        t.apply_table_props(props)
        return t

    def test_finds_F90_in_meta_dir(self):
        fort = os.path.join(self._tmpdir, 'foo.F90')
        open(fort, 'w').close()
        t = self._make_table()
        result = val_mod._fortran_file_for_table(t)
        self.assertEqual(result, fort)

    def test_finds_F90_in_source_path(self):
        subdir = os.path.join(self._tmpdir, 'src')
        os.makedirs(subdir)
        fort = os.path.join(subdir, 'foo.F90')
        open(fort, 'w').close()
        t = self._make_table(source_path='src')
        result = val_mod._fortran_file_for_table(t)
        self.assertEqual(result, fort)

    def test_returns_none_when_not_found(self):
        t = self._make_table()
        result = val_mod._fortran_file_for_table(t)
        self.assertIsNone(result)


def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(val_mod))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
