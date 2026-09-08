"""Unit tests for ccpp_validator."""

import doctest
import logging
import os
import tempfile
import textwrap
import unittest

import ccpp_validator as val_mod
from ccpp_validator import (
    _base_local_name,
    _join_continuation,
    _load_modules_tree,
    _load_source_tree,
    _parse_modules,
    _parse_subroutines,
    validate,
)
from metadata.parse_tools import CCPPError

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

    def test_sfc_sice_style_missing_trailing_ampersand(self):
        """Fixed-form continuation where the second-to-last line has NO
        trailing ``&`` but the next line has a column-6 ``&`` is a
        valid F77 continuation.  CCPP physics has this in the wild
        (``SFC_Models/SeaIce/CICE/sfc_sice.f::sfc_sice_run``).  Without
        look-ahead at the next line's column-6 marker the parser ends
        the logical line one step early and the closing ``)`` lands on
        its own — args list never closes, signature regex captures 0
        args, validator reports a bogus arg-count mismatch."""
        src_lines = [
            '      subroutine sfc_sice_run                                           &\n',
            '     &     ( im, kice, ps, t1,                                          &\n',
            '     &       errmsg, errflg\n',          # NO trailing ``&``
            '     &     )\n',                          # column-6 ``&`` only
        ]
        result = _join_continuation(src_lines)
        self.assertEqual(len(result), 1)
        # Closing ``)`` must be present in the joined line.
        self.assertIn(')', result[0])
        # And no stray leading ``&`` remained in the joined output.
        import re as _re
        self.assertIsNotNone(
            _re.search(r'subroutine\s+sfc_sice_run\s*\([^)]*\)', result[0]),
            'joined signature lacks ``subroutine NAME (args)`` shape — got: {!r}'
            .format(result[0]),
        )

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

    def test_sfc_ocean_style_decorated_trailing_ampersand(self):
        """Fixed-form continuation where the trailing ``&`` is followed by
        a stray ``,`` and an inline comment.  In strict F77, columns
        past 72 are ignored, so ``&,  ! --- inputs`` past col-71 is
        invisible to the compiler.  The parser must not glue the ``,``
        into the joined arg list — otherwise a phantom ``&`` token
        appears between args.  Triggers the decoration-repair branch
        (next line's column-6 ``&`` confirms continuation)."""
        src_lines = [
            '      subroutine sfc_ocean_run                                  &\n',
            '     &     ( im, hvap, cp,                                     &\n',
            '     &       wind,                  &,  ! --- inputs\n',
            '     &       errmsg, errflg )\n',
        ]
        result = _join_continuation(src_lines, filename='sfc_ocean.F')
        self.assertEqual(len(result), 1)
        # Phantom ``&`` must not appear in the joined logical line.
        self.assertNotIn('&', result[0])
        # All real args must still be present.
        for tok in ('im', 'hvap', 'cp', 'wind', 'errmsg', 'errflg'):
            self.assertIn(tok, result[0])
        # The stray comma from the decoration must NOT survive past
        # ``wind`` (no double-comma).
        self.assertNotIn(',,', result[0].replace(' ', ''))

    def test_decoration_repair_preserves_real_tokens_past_amp(self):
        """When tokens past ``&`` look like real identifiers, the line
        is returned untouched so the parser surfaces a real error
        instead of silently dropping code."""
        # A pathological line: ``&`` mid-line with an identifier after.
        # Next line is col-6 ``&`` so we enter the look-ahead branch.
        src_lines = [
            '      foo = a & extra_token\n',
            '     &        + b\n',
        ]
        result = _join_continuation(src_lines, filename='/tmp/path.f')
        # The line is left untouched (no repair); ``extra_token`` stays.
        self.assertEqual(len(result), 1)
        self.assertIn('extra_token', result[0])

    def test_decoration_repair_emits_warning(self):
        """The decoration-repair branch must emit a single
        ``logger.warning`` naming the file:line so users see that their
        source has decoration past the statement end."""
        import logging as _logging
        src_lines = [
            '      subroutine foo(                                          &\n',
            '     &       a, b,                                  &,  ! decoration\n',
            '     &       c )\n',
            '      end subroutine foo\n',
        ]
        # Capture warnings from the validator module's logger.
        records = []

        class _Capture(_logging.Handler):
            def emit(self, record):
                records.append(record)

        cap = _Capture(level=_logging.WARNING)
        val_mod._LOGGER.addHandler(cap)
        try:
            _join_continuation(src_lines, filename='/some/path/foo.F')
        finally:
            val_mod._LOGGER.removeHandler(cap)
        self.assertEqual(len(records), 1, "expected exactly one warning")
        msg = records[0].getMessage()
        self.assertIn('/some/path/foo.F', msg)
        self.assertIn(':2:', msg)  # decoration is on line 2 of src_lines


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


class TestArgAttributeChecks(unittest.TestCase):
    """Per-arg type/kind/intent/rank/optional mismatch detection.

    Each test builds a tiny in-memory metadata + Fortran source pair and
    runs ``validate`` end-to-end.  The Fortran source has the same arg
    names as the metadata so the name-set check passes; we deliberately
    perturb one attribute per test to exercise one check at a time.
    """

    _BASE_META = (
        '[ccpp-table-properties]\n'
        '  name = s\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = s_run\n'
        '  type = scheme\n'
        '[ a ]\n'
        '  standard_name = a_std\n'
        '  units = 1\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '  intent = {intent_a}\n'
        '[ b ]\n'
        '  standard_name = b_std\n'
        '  units = K\n'
        '  dimensions = {dims_b}\n'
        '  type = {type_b}\n'
        '  kind = {kind_b}\n'
        '  intent = {intent_b}\n'
        '  optional = {optional_b}\n'
    )

    def _run(self, meta_text, f90_text):
        with tempfile.TemporaryDirectory() as d:
            meta_path = os.path.join(d, 's.meta')
            f90_path  = os.path.join(d, 's.F90')
            with open(meta_path, 'w') as fh:
                fh.write(meta_text)
            with open(f90_path, 'w') as fh:
                fh.write(f90_text)
            return validate([meta_path], [f90_path])

    def _meta(self, **overrides):
        defaults = dict(intent_a='in', dims_b='()', type_b='real',
                        kind_b='kind_phys', intent_b='in', optional_b='False')
        defaults.update(overrides)
        return self._BASE_META.format(**defaults)

    _F90_TEMPLATE = (
        'module m\n'
        'contains\n'
        '  subroutine s_run({sig})\n'
        '    use ccpp_kinds, only: kind_phys\n'
        '{decls}'
        '  end subroutine s_run\n'
        'end module m\n'
    )

    def _f90(self, sig, decls):
        return self._F90_TEMPLATE.format(sig=sig, decls=decls)

    def test_clean_match_no_errors(self):
        errs = self._run(
            self._meta(),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    real(kind=kind_phys), intent(in) :: b\n'
            )),
        )
        self.assertEqual(errs, [])

    def test_intent_mismatch(self):
        errs = self._run(
            self._meta(intent_b='in'),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    real(kind=kind_phys), intent(out) :: b\n'
            )),
        )
        self.assertTrue(any("intent mismatch" in e and "'b'" in e for e in errs),
                        msg=errs)

    def test_type_mismatch(self):
        errs = self._run(
            self._meta(type_b='real'),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    integer, intent(in) :: b\n'
            )),
        )
        self.assertTrue(any("type mismatch" in e and "'b'" in e for e in errs),
                        msg=errs)

    def test_kind_mismatch(self):
        errs = self._run(
            self._meta(kind_b='kind_phys'),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    real, intent(in) :: b\n'   # missing kind
            )),
        )
        self.assertTrue(any("kind mismatch" in e and "'b'" in e for e in errs),
                        msg=errs)

    def test_character_len_star_consistent_passes(self):
        # len=* on BOTH sides is consistent -> no error.
        errs = self._run(
            self._meta(type_b='character', kind_b='len=*'),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    character(len=*), intent(in) :: b\n'
            )),
        )
        char_errs = [e for e in errs if "'b'" in e and 'character' in e]
        self.assertEqual(char_errs, [], msg=errs)

    def test_character_len_star_vs_concrete_is_mismatch(self):
        # len=* must NOT wildcard against a concrete len=N: the metadata must
        # mirror the Fortran exactly.
        errs = self._run(
            self._meta(type_b='character', kind_b='len=512'),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    character(len=*), intent(in) :: b\n'
            )),
        )
        self.assertTrue(
            any("character length mismatch" in e and "'b'" in e for e in errs),
            msg=errs,
        )

    def test_character_concrete_len_match_passes(self):
        # Identical concrete lengths agree.
        errs = self._run(
            self._meta(type_b='character', kind_b='len=64'),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    character(len=64), intent(in) :: b\n'
            )),
        )
        char_errs = [e for e in errs if "'b'" in e and 'character' in e]
        self.assertEqual(char_errs, [], msg=errs)

    def test_character_old_style_len_parsed_and_matched(self):
        # Old-style F77 character*64 in Fortran is normalised to len=64 and
        # matched against the metadata.
        errs = self._run(
            self._meta(type_b='character', kind_b='len=64'),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    character*64, intent(in) :: b\n'
            )),
        )
        char_errs = [e for e in errs if "'b'" in e and 'character' in e]
        self.assertEqual(char_errs, [], msg=errs)

    def test_rank_mismatch(self):
        errs = self._run(
            self._meta(dims_b='(d1, d2)'),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    real(kind=kind_phys), intent(in) :: b\n'  # rank 0, metadata says rank 2
            )),
        )
        self.assertTrue(any("rank mismatch" in e and "'b'" in e for e in errs),
                        msg=errs)

    def test_rank_via_var_attached_dims(self):
        errs = self._run(
            self._meta(dims_b='(d1, d2)'),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    real(kind=kind_phys), intent(in) :: b(:,:)\n'
            )),
        )
        self.assertEqual(errs, [])

    def test_metadata_optional_but_fortran_required_is_error(self):
        # Metadata says optional=True, Fortran doesn't carry the
        # 'optional' attribute -> hard error (cap would emit invalid
        # present() checks on a required dummy).
        errs = self._run(
            self._meta(optional_b='True'),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    real(kind=kind_phys), intent(in) :: b\n'
            )),
        )
        self.assertTrue(
            any("optional=True" in e and "'b'" in e for e in errs),
            msg=errs,
        )

    def test_ddt_metadata_bare_name_matches_fortran_type_wrapper(self):
        # Metadata: type = ty_rad_lw  (bare DDT name).
        # Fortran:  type(ty_rad_lw), intent(in) :: b
        # These should compare equal after type-name normalisation.
        errs = self._run(
            self._meta(type_b='ty_rad_lw', kind_b=''),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    type(ty_rad_lw), intent(in) :: b\n'
            )),
        )
        b_errs = [e for e in errs if "'b'" in e]
        self.assertEqual(b_errs, [], msg=errs)

    def test_ddt_class_wrapper_matches_metadata_bare_name(self):
        # Fortran polymorphic wrapper: class(...) on the Fortran side
        # still matches a bare DDT name in metadata.
        errs = self._run(
            self._meta(type_b='ty_rad_lw', kind_b=''),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    class(ty_rad_lw), intent(in) :: b\n'
            )),
        )
        b_errs = [e for e in errs if "'b'" in e]
        self.assertEqual(b_errs, [], msg=errs)

    def test_ddt_name_mismatch_is_error(self):
        # Different DDT names on each side -> error.
        errs = self._run(
            self._meta(type_b='ty_rad_lw', kind_b=''),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    type(ty_rad_sw), intent(in) :: b\n'
            )),
        )
        self.assertTrue(
            any("type mismatch" in e and "'b'" in e for e in errs),
            msg=errs,
        )

    def test_external_type_matches_fortran_bare_typename(self):
        # Metadata: type = external:mpi_f08:mpi_comm  (module + name).
        # Fortran:  type(mpi_comm), intent(in) :: b
        # The module is metadata-only; Fortran sees the bare typename.
        errs = self._run(
            self._meta(type_b='external:mpi_f08:mpi_comm', kind_b=''),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    type(mpi_comm), intent(in) :: b\n'
            )),
        )
        b_errs = [e for e in errs if "'b'" in e]
        self.assertEqual(b_errs, [], msg=errs)

    def test_external_type_mismatched_typename_is_error(self):
        errs = self._run(
            self._meta(type_b='external:mpi_f08:mpi_comm', kind_b=''),
            self._f90('a, b', (
                '    integer, intent(in) :: a\n'
                '    type(mpi_request), intent(in) :: b\n'
            )),
        )
        self.assertTrue(
            any("type mismatch" in e and "'b'" in e for e in errs),
            msg=errs,
        )

    def test_fortran_optional_but_metadata_required_is_warning(self):
        # Reverse direction: metadata=False (default), Fortran=optional
        # -> NOT an error.  The cap always passes the arg; that's a
        # valid subset of the Fortran contract.  A warning is emitted.
        import io
        log_buf = io.StringIO()
        handler = logging.StreamHandler(log_buf)
        handler.setLevel(logging.WARNING)
        log = logging.getLogger('test_validator_fopt_metareq')
        log.addHandler(handler)
        log.setLevel(logging.WARNING)
        try:
            with tempfile.TemporaryDirectory() as d:
                meta_path = os.path.join(d, 's.meta')
                f90_path  = os.path.join(d, 's.F90')
                with open(meta_path, 'w') as fh:
                    fh.write(self._meta())
                with open(f90_path, 'w') as fh:
                    fh.write(self._f90('a, b', (
                        '    integer, intent(in) :: a\n'
                        '    real(kind=kind_phys), optional, intent(in) :: b\n'
                    )))
                errs = validate([meta_path], [f90_path], logger=log)
        finally:
            log.removeHandler(handler)
        # No errors.
        b_errs = [e for e in errs if "'b'" in e]
        self.assertEqual(b_errs, [], msg=errs)
        # But a warning for 'b'.
        self.assertIn("Fortran argument 'b'", log_buf.getvalue())
        self.assertIn("optional", log_buf.getvalue())


class TestFortranOnlyOptionalWarning(unittest.TestCase):
    """A Fortran-optional arg absent from metadata triggers a logger.warning
    but no validation error."""

    _META = (
        '[ccpp-table-properties]\n'
        '  name = s\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = s_run\n'
        '  type = scheme\n'
        '[ a ]\n'
        '  standard_name = a_std\n'
        '  units = 1\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '  intent = in\n'
    )

    _F90 = (
        'module m\n'
        'contains\n'
        '  subroutine s_run(a, b)\n'
        '    integer, intent(in) :: a\n'
        '    integer, optional, intent(in) :: b\n'
        '  end subroutine s_run\n'
        'end module m\n'
    )

    def test_warning_and_no_error(self):
        import io
        with tempfile.TemporaryDirectory() as d:
            meta_path = os.path.join(d, 's.meta')
            f90_path  = os.path.join(d, 's.F90')
            with open(meta_path, 'w') as fh:
                fh.write(self._META)
            with open(f90_path, 'w') as fh:
                fh.write(self._F90)
            stream = io.StringIO()
            handler = logging.StreamHandler(stream)
            handler.setLevel(logging.WARNING)
            log = logging.getLogger('test_validator_optional_warn')
            log.addHandler(handler)
            log.setLevel(logging.WARNING)
            try:
                errs = validate([meta_path], [f90_path], logger=log)
            finally:
                log.removeHandler(handler)
        self.assertEqual(errs, [])
        self.assertIn("Optional Fortran argument 'b'", stream.getvalue())


class _HostValidationFixture(unittest.TestCase):
    """Shared scaffolding for host/ddt validation tests.

    Builds a tmpdir with one host metadata file and one Fortran source.
    Subclasses set ``META`` and ``F90`` class attributes.
    """

    META: str = ''
    F90: str = ''
    META_NAME: str = 'host.meta'
    F90_NAME: str = 'host.F90'

    def setUp(self):
        self.tmpdir = tempfile.mkdtemp()
        self.meta_path = os.path.join(self.tmpdir, self.META_NAME)
        self.f90_path  = os.path.join(self.tmpdir, self.F90_NAME)
        with open(self.meta_path, 'w') as fh:
            fh.write(self.META)
        with open(self.f90_path, 'w') as fh:
            fh.write(self.F90)

    def tearDown(self):
        import shutil
        shutil.rmtree(self.tmpdir, ignore_errors=True)


class TestParseModulesAndDDTs(unittest.TestCase):
    """Standalone parser-level tests beyond the doctest example."""

    def test_module_var_after_use_only(self):
        src = textwrap.dedent("""\
            module m
              use kinds, only: kind_phys
              implicit none
              integer :: nlev
              real(kind=kind_phys) :: cp
            end module m
        """)
        mods = _parse_modules(src)
        self.assertEqual(sorted(mods['m'].vars.keys()), ['cp', 'nlev'])
        self.assertEqual(mods['m'].vars['cp'].type_, 'real')
        self.assertEqual(mods['m'].vars['cp'].kind_, 'kind_phys')

    def test_subroutine_locals_excluded(self):
        src = textwrap.dedent("""\
            module m
              integer :: mod_var
            contains
              subroutine helper(x)
                integer, intent(in) :: x
                real :: local_only
              end subroutine helper
            end module m
        """)
        mods = _parse_modules(src)
        self.assertIn('mod_var', mods['m'].vars)
        self.assertNotIn('local_only', mods['m'].vars)
        self.assertNotIn('x', mods['m'].vars)

    def test_ddt_block_components(self):
        src = textwrap.dedent("""\
            module types
              type :: physics_t
                real(kind=kind_phys) :: tk(:,:)
                integer :: nlay
                logical :: has_water
              end type physics_t
            end module types
        """)
        mods = _parse_modules(src)
        comps = mods['types'].ddts['physics_t']
        self.assertEqual(sorted(comps.keys()), ['has_water', 'nlay', 'tk'])
        self.assertEqual(comps['tk'].rank, 2)
        self.assertEqual(comps['tk'].kind_, 'kind_phys')
        self.assertEqual(comps['has_water'].type_, 'logical')

    def test_ddt_with_attrs_on_type_decl(self):
        src = textwrap.dedent("""\
            module types
              type, public :: opaque_t
                integer :: a
              end type opaque_t
            end module types
        """)
        mods = _parse_modules(src)
        self.assertIn('opaque_t', mods['types'].ddts)

    def test_ddt_index_collects_across_modules(self):
        src1 = ('module a\n  type :: t1\n    integer :: x\n  end type t1\n'
                'end module a\n')
        src2 = ('module b\n  type :: t2\n    real :: y\n  end type t2\n'
                'end module b\n')
        tmp = tempfile.mkdtemp()
        try:
            p1 = os.path.join(tmp, 'a.F90')
            p2 = os.path.join(tmp, 'b.F90')
            with open(p1, 'w') as fh: fh.write(src1)
            with open(p2, 'w') as fh: fh.write(src2)
            modules, ddt_index = _load_modules_tree([p1, p2])
        finally:
            import shutil
            shutil.rmtree(tmp, ignore_errors=True)
        self.assertEqual(sorted(modules.keys()), ['a', 'b'])
        self.assertEqual(sorted(ddt_index.keys()), ['t1', 't2'])


class TestBaseLocalName(unittest.TestCase):

    def test_plain(self):
        self.assertEqual(_base_local_name('foo'), 'foo')

    def test_case_folded(self):
        self.assertEqual(_base_local_name('Foo_Bar'), 'foo_bar')

    def test_strips_subscript(self):
        self.assertEqual(_base_local_name('tk(:,:)'), 'tk')

    def test_strips_named_subscript(self):
        self.assertEqual(
            _base_local_name('dqdt(:,:,index_of_cloud)'), 'dqdt',
        )


class TestValidateHostCorrect(_HostValidationFixture):
    """type=host with matching Fortran module → no errors."""

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_host
          type = host
        [ccpp-arg-table]
          name = my_host
          type = host
        [ nlev ]
          standard_name = vertical_layer_dimension
          long_name = number of layers
          units = count
          dimensions = ()
          type = integer
        [ cp ]
          standard_name = specific_heat_of_dry_air_at_constant_pressure
          long_name = specific heat
          units = J kg-1 K-1
          dimensions = ()
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module my_host
          implicit none
          integer :: nlev
          real(kind=kind_phys) :: cp
        end module my_host
    """)

    def test_no_errors(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(errs, [])


class TestValidateHostTypeMismatch(_HostValidationFixture):

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_host
          type = host
        [ccpp-arg-table]
          name = my_host
          type = host
        [ cp ]
          standard_name = specific_heat_of_dry_air_at_constant_pressure
          long_name = specific heat
          units = J kg-1 K-1
          dimensions = ()
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module my_host
          implicit none
          integer :: cp
        end module my_host
    """)

    def test_type_error(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        # Type mismatch surfaces; an incidental kind mismatch may also
        # surface (integer Fortran has no kind metadata).
        type_errs = [e for e in errs if 'type mismatch' in e]
        self.assertEqual(len(type_errs), 1, errs)
        self.assertIn("cp", type_errs[0])


class TestValidateHostKindMismatch(_HostValidationFixture):

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_host
          type = host
        [ccpp-arg-table]
          name = my_host
          type = host
        [ cp ]
          standard_name = specific_heat_of_dry_air_at_constant_pressure
          long_name = specific heat
          units = J kg-1 K-1
          dimensions = ()
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module my_host
          implicit none
          real :: cp
        end module my_host
    """)

    def test_kind_error(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(len(errs), 1, errs)
        self.assertIn("kind mismatch", errs[0])


class TestValidateHostCharacterAssumedLength(_HostValidationFixture):
    """A host character variable declared ``kind = len=*`` is rejected even
    when the Fortran side has a concrete length (the metadata defines the
    storage there, so assumed length is illegal)."""

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_host
          type = host
        [ccpp-arg-table]
          name = my_host
          type = host
        [ scheme_name ]
          standard_name = scheme_name
          long_name = scheme name
          units = none
          dimensions = ()
          type = character | kind = len=*
    """)

    F90 = textwrap.dedent("""\
        module my_host
          implicit none
          character(len=512) :: scheme_name
        end module my_host
    """)

    def test_assumed_length_error(self):
        # Two complementary errors now fire: the definition-site rejection of
        # len=* in host/DDT metadata, plus the exact-match inconsistency vs the
        # concrete Fortran (character(len=512)).  Both name scheme_name.
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertTrue(
            any("concrete length" in e and "len=*" in e for e in errs), errs
        )
        self.assertTrue(all("scheme_name" in e for e in errs), errs)


class TestValidateHostCharacterConcreteLengthOK(_HostValidationFixture):
    """A concrete host character length matching the Fortran passes."""

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_host
          type = host
        [ccpp-arg-table]
          name = my_host
          type = host
        [ scheme_name ]
          standard_name = scheme_name
          long_name = scheme name
          units = none
          dimensions = ()
          type = character | kind = len=512
    """)

    F90 = textwrap.dedent("""\
        module my_host
          implicit none
          character(len=512) :: scheme_name
        end module my_host
    """)

    def test_concrete_length_ok(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(errs, [])


class TestValidateHostRankMismatch(_HostValidationFixture):

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_host
          type = host
        [ccpp-arg-table]
          name = my_host
          type = host
        [ tk ]
          standard_name = air_temperature
          long_name = temperature
          units = K
          dimensions = (horizontal_dimension, vertical_layer_dimension)
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module my_host
          implicit none
          real(kind=kind_phys) :: tk(:)
        end module my_host
    """)

    def test_rank_error(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(len(errs), 1, errs)
        self.assertIn("rank mismatch", errs[0])


class TestValidateHostMissingVar(_HostValidationFixture):

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_host
          type = host
        [ccpp-arg-table]
          name = my_host
          type = host
        [ cp ]
          standard_name = specific_heat_of_dry_air_at_constant_pressure
          long_name = specific heat
          units = J kg-1 K-1
          dimensions = ()
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module my_host
          implicit none
          integer :: nlev
        end module my_host
    """)

    def test_missing_decl_error(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(len(errs), 1, errs)
        self.assertIn("not found", errs[0])
        self.assertIn("cp", errs[0])


class TestValidateHostMissingModule(_HostValidationFixture):

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_host
          type = host
        [ccpp-arg-table]
          name = my_host
          type = host
        [ cp ]
          standard_name = specific_heat_of_dry_air_at_constant_pressure
          long_name = specific heat
          units = J kg-1 K-1
          dimensions = ()
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module different_module
          implicit none
          real(kind=kind_phys) :: cp
        end module different_module
    """)

    def test_module_not_found_error(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(len(errs), 1, errs)
        self.assertIn("not found in any source file", errs[0])
        self.assertIn("my_host", errs[0])


class TestValidateHostModuleNameOverride(_HostValidationFixture):
    """module_name override in [ccpp-table-properties] honoured."""

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_table
          type = host
          module_name = host_state
        [ccpp-arg-table]
          name = my_table
          type = host
        [ cp ]
          standard_name = specific_heat_of_dry_air_at_constant_pressure
          long_name = specific heat
          units = J kg-1 K-1
          dimensions = ()
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module host_state
          implicit none
          real(kind=kind_phys) :: cp
        end module host_state
    """)

    def test_no_errors_with_override(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(errs, [])


class TestValidateDDTCorrect(_HostValidationFixture):

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = physics_t
          type = ddt
        [ccpp-arg-table]
          name = physics_t
          type = ddt
        [ tk ]
          standard_name = air_temperature
          long_name = temperature
          units = K
          dimensions = (horizontal_dimension, vertical_layer_dimension)
          type = real | kind = kind_phys
        [ nlay ]
          standard_name = vertical_layer_dimension
          long_name = number of layers
          units = count
          dimensions = ()
          type = integer
    """)

    F90 = textwrap.dedent("""\
        module phys_types
          implicit none
          type :: physics_t
            real(kind=kind_phys) :: tk(:,:)
            integer :: nlay
          end type physics_t
        end module phys_types
    """)

    META_NAME = 'physics_t.meta'
    F90_NAME = 'phys_types.F90'

    def test_no_errors(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(errs, [])


class TestValidateDDTSlicedLocalName(_HostValidationFixture):
    """A sliced metadata local_name expresses a reduced-rank view of a
    higher-rank Fortran component.  The rank check must consult the
    subscript width — not just len(metadata.dimensions) — when computing
    the expected Fortran rank.  Mirrors the
    end-to-end-tests/ddthost/test_host_data shape: rank-3 Fortran ``q``
    bound under TWO standard names — the bare ``q`` (3-D mixing ratio)
    and the sliced ``q(:,:,index_of_water_vapor_specific_humidity)``
    (2-D view of one tracer)."""

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = physics_state
          type = ddt
        [ccpp-arg-table]
          name = physics_state
          type = ddt
        [ q ]
          standard_name = constituent_mixing_ratio
          long_name = q
          units = kg kg-1
          dimensions = (horizontal_dimension, vertical_layer_dimension, number_of_tracers)
          type = real | kind = kind_phys
        [ q(:,:,index_of_water_vapor_specific_humidity) ]
          standard_name = water_vapor_specific_humidity
          long_name = q water vapor
          units = kg kg-1
          dimensions = (horizontal_dimension, vertical_layer_dimension)
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module phys_types
          implicit none
          type :: physics_state
            real(kind=kind_phys), dimension(:, :, :), allocatable :: q
          end type physics_state
        end module phys_types
    """)

    META_NAME = 'physics_state.meta'
    F90_NAME = 'phys_types.F90'

    def test_no_errors_for_sliced_and_bare_view(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(errs, [])


class TestValidateHostArrayInitialiserRank(_HostValidationFixture):
    """Module-level parameter declarations with an array-constructor
    initialiser (``(/ 'a', 'b', 'c' /)``) must not have the
    initialiser's commas counted as dimension entries — that
    regression made the rank-1 ``std_name_array`` on the
    end-to-end-tests/advection fixture look like rank 3.  Mirrors
    that shape exactly."""

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = test_host_data
          type = host
        [ccpp-arg-table]
          name = test_host_data
          type = host
        [ num_consts ]
          standard_name = number_of_constituents
          units = count
          dimensions = ()
          type = integer
        [ std_name_array ]
          standard_name = array_of_constituent_standard_names
          units = none
          dimensions = (number_of_constituents)
          type = character | kind = len=32
    """)

    F90 = textwrap.dedent("""\
        module test_host_data
          implicit none
          integer, public, parameter :: num_consts = 3
          character(len=32), public, parameter :: std_name_array(num_consts) = (/ &
              'specific_humidity            ', &
              'cloud_liquid_dry_mixing_ratio', &
              'cloud_ice_dry_mixing_ratio   ' /)
        end module test_host_data
    """)

    META_NAME = 'test_host_data.meta'
    F90_NAME = 'test_host_data.F90'

    def test_no_errors(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(errs, [])


class TestValidateDDTSlicedRankStillCaught(_HostValidationFixture):
    """A truly wrong rank under a sliced spelling is still caught.  The
    Fortran ``q`` here is rank 2 instead of the rank-3 the metadata
    implies — the subscript declares 3 entries (``(:,:,index_of_X)``)
    but Fortran only carries 2."""

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = physics_state
          type = ddt
        [ccpp-arg-table]
          name = physics_state
          type = ddt
        [ q(:,:,index_of_water_vapor_specific_humidity) ]
          standard_name = water_vapor_specific_humidity
          long_name = q water vapor
          units = kg kg-1
          dimensions = (horizontal_dimension, vertical_layer_dimension)
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module phys_types
          implicit none
          type :: physics_state
            real(kind=kind_phys), dimension(:, :), allocatable :: q
          end type physics_state
        end module phys_types
    """)

    META_NAME = 'physics_state.meta'
    F90_NAME = 'phys_types.F90'

    def test_rank_mismatch_surfaces(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        rank_errs = [e for e in errs if 'rank mismatch' in e]
        self.assertEqual(len(rank_errs), 1, errs)
        self.assertIn("q(:,:,index_of_water_vapor_specific_humidity)",
                      rank_errs[0])
        self.assertIn("Fortran declares rank 2", rank_errs[0])


class TestValidateDDTComponentMismatch(_HostValidationFixture):

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = physics_t
          type = ddt
        [ccpp-arg-table]
          name = physics_t
          type = ddt
        [ tk ]
          standard_name = air_temperature
          long_name = temperature
          units = K
          dimensions = (horizontal_dimension, vertical_layer_dimension)
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module phys_types
          implicit none
          type :: physics_t
            integer :: tk(:,:)
          end type physics_t
        end module phys_types
    """)

    META_NAME = 'physics_t.meta'
    F90_NAME = 'phys_types.F90'

    def test_type_error(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        type_errs = [e for e in errs if 'type mismatch' in e]
        self.assertEqual(len(type_errs), 1, errs)
        self.assertIn("tk", type_errs[0])


class TestValidateDDTMissingComponent(_HostValidationFixture):

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = physics_t
          type = ddt
        [ccpp-arg-table]
          name = physics_t
          type = ddt
        [ tk ]
          standard_name = air_temperature
          long_name = temperature
          units = K
          dimensions = (horizontal_dimension, vertical_layer_dimension)
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module phys_types
          implicit none
          type :: physics_t
            integer :: nlay
          end type physics_t
        end module phys_types
    """)

    META_NAME = 'physics_t.meta'
    F90_NAME = 'phys_types.F90'

    def test_missing_component_error(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(len(errs), 1, errs)
        self.assertIn("not found as a component", errs[0])
        self.assertIn("tk", errs[0])


class TestValidateDDTMissingType(_HostValidationFixture):

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = physics_t
          type = ddt
        [ccpp-arg-table]
          name = physics_t
          type = ddt
        [ tk ]
          standard_name = air_temperature
          long_name = temperature
          units = K
          dimensions = (horizontal_dimension, vertical_layer_dimension)
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module phys_types
          implicit none
          type :: different_type
            real(kind=kind_phys) :: tk(:,:)
          end type different_type
        end module phys_types
    """)

    META_NAME = 'physics_t.meta'
    F90_NAME = 'phys_types.F90'

    def test_type_not_found_error(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(len(errs), 1, errs)
        self.assertIn("not found as a derived-type definition", errs[0])
        self.assertIn("physics_t", errs[0])


class TestValidateControlSilentSkip(_HostValidationFixture):
    """type=control tables are silent-skipped (no Fortran backs control vars)."""

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_control
          type = control
        [ccpp-arg-table]
          name = my_control
          type = control
        [ suite_name ]
          standard_name = suite_name
          long_name = suite name
          units = none
          dimensions = ()
          type = character | kind = len=*
        [ errcode ]
          standard_name = ccpp_error_code
          long_name = error code
          units = 1
          dimensions = ()
          type = integer
    """)

    # F90 is irrelevant — control vars never resolved against source.
    F90 = textwrap.dedent("""\
        module placeholder
          implicit none
        end module placeholder
    """)

    def test_no_errors_for_control(self):
        errs = validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertEqual(errs, [])


class TestValidateSchemeInHostFilesRejected(_HostValidationFixture):
    """type=scheme passed via --host-files is a hard error."""

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_scheme
          type = scheme
        [ccpp-arg-table]
          name = my_scheme_run
          type = scheme
        [ a ]
          standard_name = horizontal_dimension
          long_name = horizontal dim
          units = count
          dimensions = ()
          type = integer
          intent = in
    """)

    F90 = textwrap.dedent("""\
        module placeholder
          implicit none
        end module placeholder
    """)

    def test_raises_ccpperror(self):
        with self.assertRaises(CCPPError) as cm:
            validate([], [self.f90_path], host_files=[self.meta_path])
        self.assertIn("--host-files", str(cm.exception))
        self.assertIn("my_scheme", str(cm.exception))


class TestValidateNonSchemeInSchemeFilesRejected(_HostValidationFixture):
    """type=host / type=control / type=ddt in --scheme-files is rejected.

    Symmetric to the scheme-in-host-files rejection above: each CLI
    flag has a single responsibility so misclassified .meta files fail
    fast with a clear pointer at the right flag.
    """

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_host
          type = host
        [ccpp-arg-table]
          name = my_host
          type = host
        [ nlev ]
          standard_name = vertical_layer_dimension
          long_name = number of layers
          units = count
          dimensions = ()
          type = integer
    """)

    F90 = textwrap.dedent("""\
        module my_host
          implicit none
          integer :: nlev
        end module my_host
    """)

    def test_host_in_scheme_files_rejected(self):
        with self.assertRaises(CCPPError) as cm:
            validate([self.meta_path], [self.f90_path])
        msg = str(cm.exception)
        self.assertIn("--scheme-files", msg)
        self.assertIn("--host-files", msg)
        self.assertIn("my_host", msg)
        self.assertIn("type = host", msg)

    def test_control_in_scheme_files_rejected(self):
        # Replace the host fixture's meta with a control-typed one and
        # check the same rejection path covers control too.
        with open(self.meta_path, 'w') as fh:
            fh.write(textwrap.dedent("""\
                [ccpp-table-properties]
                  name = my_control
                  type = control
                [ccpp-arg-table]
                  name = my_control
                  type = control
                [ suite_name ]
                  standard_name = suite_name
                  long_name = suite name
                  units = none
                  dimensions = ()
                  type = character | kind = len=*
            """))
        with self.assertRaises(CCPPError) as cm:
            validate([self.meta_path], [self.f90_path])
        msg = str(cm.exception)
        self.assertIn("my_control", msg)
        self.assertIn("type = control", msg)

    def test_suite_in_scheme_files_rejected(self):
        # type = suite is the third rejected type (host / control / suite).
        with open(self.meta_path, 'w') as fh:
            fh.write(textwrap.dedent("""\
                [ccpp-table-properties]
                  name = my_suite
                  type = suite
                [ccpp-arg-table]
                  name = my_suite
                  type = suite
            """))
        with self.assertRaises(CCPPError) as cm:
            validate([self.meta_path], [self.f90_path])
        msg = str(cm.exception)
        self.assertIn("my_suite", msg)
        self.assertIn("type = suite", msg)


class TestValidateSchemeWithCoLocatedDDT(_HostValidationFixture):
    """A scheme metadata file may contain its own type = ddt tables —
    schemes routinely co-locate the DDTs they define (e.g. radiation
    schemes carrying ty_rad_lw / ty_rad_sw type definitions in the same
    .meta as the scheme phase blocks).  Both the scheme phase signature
    AND the DDT components must be validated."""

    META = textwrap.dedent("""\
        [ccpp-table-properties]
          name = my_scheme
          type = scheme
        [ccpp-arg-table]
          name = my_scheme_run
          type = scheme
        [ workspace ]
          standard_name = scheme_workspace
          long_name = workspace
          units = none
          dimensions = ()
          type = ty_ws
          intent = inout
        [ errmsg ]
          standard_name = ccpp_error_message
          long_name = error message
          units = none
          dimensions = ()
          type = character | kind = len=*
          intent = out
        [ errflg ]
          standard_name = ccpp_error_code
          long_name = error code
          units = 1
          dimensions = ()
          type = integer
          intent = out
        [ccpp-table-properties]
          name = ty_ws
          type = ddt
        [ccpp-arg-table]
          name = ty_ws
          type = ddt
        [ nlay ]
          standard_name = vertical_layer_dimension
          long_name = number of layers
          units = count
          dimensions = ()
          type = integer
        [ tk ]
          standard_name = air_temperature
          long_name = temperature
          units = K
          dimensions = (horizontal_dimension, vertical_layer_dimension)
          type = real | kind = kind_phys
    """)

    F90 = textwrap.dedent("""\
        module my_scheme
          implicit none
          type :: ty_ws
            integer :: nlay
            real(kind=kind_phys) :: tk(:,:)
          end type ty_ws
        contains
          subroutine my_scheme_run(workspace, errmsg, errflg)
            type(ty_ws), intent(inout) :: workspace
            character(len=*), intent(out) :: errmsg
            integer, intent(out) :: errflg
          end subroutine my_scheme_run
        end module my_scheme
    """)

    META_NAME = 'my_scheme.meta'
    F90_NAME = 'my_scheme.F90'

    def test_no_errors_when_both_match(self):
        errs = validate([self.meta_path], [self.f90_path])
        self.assertEqual(errs, [])

    def test_ddt_component_mismatch_surfaces(self):
        # Break the DDT component type and confirm the validator
        # catches it (proves the DDT pass actually runs on scheme files).
        broken_f90 = textwrap.dedent("""\
            module my_scheme
              implicit none
              type :: ty_ws
                integer :: nlay
                integer :: tk(:,:)
              end type ty_ws
            contains
              subroutine my_scheme_run(workspace, errmsg, errflg)
                type(ty_ws), intent(inout) :: workspace
                character(len=*), intent(out) :: errmsg
                integer, intent(out) :: errflg
              end subroutine my_scheme_run
            end module my_scheme
        """)
        with open(self.f90_path, 'w') as fh:
            fh.write(broken_f90)
        errs = validate([self.meta_path], [self.f90_path])
        type_errs = [e for e in errs if 'type mismatch' in e]
        self.assertEqual(len(type_errs), 1, errs)
        self.assertIn("tk", type_errs[0])
        self.assertIn("ty_ws", type_errs[0])


class TestValidateRequiresAtLeastOneInputs(unittest.TestCase):
    """validate() must error when neither scheme_files nor host_files is supplied."""

    def test_both_empty_raises(self):
        with self.assertRaises(CCPPError) as cm:
            validate([], [], host_files=[])
        self.assertIn("at least one", str(cm.exception))
        self.assertIn("--scheme-files", str(cm.exception))
        self.assertIn("--host-files", str(cm.exception))

    def test_default_host_files_kwarg_also_raises(self):
        # Same condition reached via the default value of host_files.
        with self.assertRaises(CCPPError):
            validate([], [])


def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(val_mod))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
