"""Tests for ``generator.fortran_lines`` — wrapping generated Fortran.

The property that matters is not the exact layout but that wrapping is
*transparent*: every emitted line fits in 132 columns, and joining the
continuations back up gives the statement the emitter asked for.  :func:`_join`
is that inverse and :func:`_same` the comparison, so most tests below are
round-trip tests rather than golden strings.

A continuation line's indent is whitespace between tokens, so :func:`_same`
compares with runs of blanks *outside* a character literal collapsed.  Inside
a literal nothing is collapsed: a continued message has to come back byte for
byte, which is the part a wrapper can silently get wrong.
"""

import doctest
import os
import tempfile
import unittest

from metadata.parse_tools import CCPPError, FREE_FORM_MAX_COLUMNS
from generator.fortran_lines import (
    GENERATED_MAX_COLUMNS,
    wrap_fortran_lines,
    write_fortran,
)

LIMIT = GENERATED_MAX_COLUMNS


def _join(lines):
    """Undo free-form continuation, returning the logical lines.

    A line ending in ``&`` continues onto the next.  When the next line's
    first non-blank character is an ``&`` the blanks before it are not part
    of the text — that is the rule that lets a character literal be
    continued without gaining or losing a blank.
    """
    out = []
    pending = None
    for raw in lines:
        text = raw.rstrip()
        if pending is not None:
            stripped = text.lstrip()
            if stripped.startswith('&'):
                text = stripped[1:]
            text = pending + text
            pending = None
        if text.endswith('&'):
            pending = text[:-1]
            continue
        out.append(text)
    if pending is not None:
        out.append(pending)
    return out


def _normalise(line):
    """Collapse blank runs outside a character literal; keep literals exact."""
    out = []
    quote = None
    idx = 0
    while idx < len(line):
        char = line[idx]
        if quote is not None:
            out.append(char)
            if char == quote:
                if line[idx + 1:idx + 2] == quote:
                    out.append(quote)
                    idx += 2
                    continue
                quote = None
        elif char in ("'", '"'):
            quote = char
            out.append(char)
        elif char in ' \t':
            if out and out[-1] != ' ':
                out.append(' ')
        else:
            out.append(char)
        idx += 1
    return ''.join(out).strip()


def _same(wrapped, original):
    """Return ``(joined, expected)`` ready for an equality assertion.

    Both sides go through :func:`_join`, so a fixture that is itself a
    continuation line compares against its own joined form rather than
    against text that still carries the trailing ampersand.
    """
    return (
        [_normalise(line) for line in _join(wrapped)],
        [_normalise(line) for line in _join(original)],
    )


class TestTheLimitInForce(unittest.TestCase):
    """Generated output is held to `.codee-format`'s column limit, which is
    stricter than the one the standard sets and the validator checks."""

    def test_generated_limit_is_the_codee_column_limit(self):
        self.assertEqual(GENERATED_MAX_COLUMNS, 120)

    def test_generated_limit_is_stricter_than_the_standard(self):
        self.assertLess(GENERATED_MAX_COLUMNS, FREE_FORM_MAX_COLUMNS)

    def test_wrapping_defaults_to_the_generated_limit(self):
        # Between the two limits: wrapped under 120, untouched under 132.
        line = '  use m, only: ' + ', '.join(
            'symbol_{:02d}'.format(i) for i in range(10)
        )
        self.assertGreater(len(line), GENERATED_MAX_COLUMNS)
        self.assertLess(len(line), FREE_FORM_MAX_COLUMNS)
        self.assertGreater(len(wrap_fortran_lines([line])), 1)


class TestLogicalIf(unittest.TestCase):
    """A one-line ``if`` breaks after its condition."""

    def setUp(self):
        self.name = 'a_suite_with_a_rather_long_name_dynamic_constituents'
        self.line = '    if (allocated({0})) deallocate({0})'.format(self.name)
        self.wrapped = wrap_fortran_lines([self.line])

    def test_fixture_is_over_the_limit(self):
        self.assertGreater(len(self.line), LIMIT)

    def test_breaks_after_the_condition(self):
        self.assertEqual(
            self.wrapped[0],
            '    if (allocated({})) &'.format(self.name),
        )
        self.assertEqual(
            self.wrapped[1], '        deallocate({})'.format(self.name)
        )

    def test_round_trips(self):
        self.assertEqual(*_same(self.wrapped, [self.line]))

    def test_block_if_is_left_to_the_comma_path(self):
        # ``then`` has no action to move, so the condition's commas are the
        # only break points.
        line = ('    if (' + ' .and. '.join(
            'flag_{:02d}(index_{:02d}, 1)'.format(i, i) for i in range(8)
        ) + ') then')
        wrapped = wrap_fortran_lines([line])
        self.assertFalse(wrapped[0].rstrip().endswith(') &'))
        self.assertEqual(*_same(wrapped, [line]))

    def test_condition_too_long_falls_through_to_commas(self):
        line = ('    if (check(' + ', '.join(
            'argument_{:02d}'.format(i) for i in range(14)
        ) + ')) return')
        wrapped = wrap_fortran_lines([line])
        for out in wrapped:
            self.assertLessEqual(len(out), LIMIT)
        self.assertEqual(*_same(wrapped, [line]))


class TestShortLinesUntouched(unittest.TestCase):
    """Wrapping only ever touches a line that would overrun."""

    def test_line_at_the_limit_is_unchanged(self):
        line = '  ' + 'x' * (LIMIT - 2)
        self.assertEqual(len(line), LIMIT)
        self.assertEqual(wrap_fortran_lines([line]), [line])

    def test_whole_file_unchanged_when_nothing_overruns(self):
        lines = ['module m', '  implicit none', 'end module m']
        self.assertEqual(wrap_fortran_lines(lines), lines)

    def test_wrapping_is_idempotent(self):
        line = '  use m, only: ' + ', '.join(
            'symbol_number_{:02d}'.format(i) for i in range(20)
        )
        once = wrap_fortran_lines([line])
        self.assertEqual(wrap_fortran_lines(once), once)


class TestEveryLineFits(unittest.TestCase):
    """Each shape the generator actually emits comes back inside 132."""

    def _check(self, line):
        wrapped = wrap_fortran_lines([line])
        self.assertGreater(len(line), LIMIT, 'fixture is not over-long')
        for out in wrapped:
            self.assertLessEqual(
                len(out), LIMIT,
                '{} columns: {!r}'.format(len(out), out),
            )
        return wrapped

    def test_import_list(self):
        self._check(
            '  use ccpp_chunked_data_suite_chunked_data_group_cap, only: '
            'chunked_data_group_init, chunked_data_group_timestep_init, '
            'chunked_data_group_run, chunked_data_group_timestep_final, '
            'chunked_data_group_final, chunked_data_group_state_alloc, '
            'chunked_data_group_state_dealloc'
        )

    def test_call_statement(self):
        self._check(
            "    call ccpp_model_constituents_obj(1)%const_index("
            "index_of_cloud_liquid_dry_mixing_ratio, "
            "'cloud_liquid_dry_mixing_ratio', errcode=errcode, "
            "errmsg=errmsg)"
        )

    def test_long_error_message(self):
        self._check(
            "      errmsg = 'ccpp_initialize_constituents: constituent "
            "''cloud_liquid_dry_mixing_ratio'' is referenced by a scheme "
            "but is not in the registered constituent table; check that "
            "some scheme''s _register routine or the host_constituents "
            "argument to ccpp_register_constituents includes it'"
        )

    def test_statement_with_long_trailing_comment(self):
        self._check(
            '    effrs(col_start:col_end, pver:1:-1) = '
            '1.0E-6_kind_phys*effrs_inout_l  ! type conversion: 8 to '
            'kind_phys; unit conversion: um to m; vertical flip '
            '(top_at_one mismatch) recorded here'
        )

    def test_one_over_long_actual_argument(self):
        self._check(
            '        cld_liq_tend=ccpp_model_constituents_obj(1)%'
            'vars_layer_tend(col_start:col_end, 1:pver, '
            'index_of_cloud_liquid_dry_mixing_ratio_with_a_long_name), &'
        )


class TestRoundTrip(unittest.TestCase):
    """Joining the continuations reproduces what the emitter asked for."""

    def test_import_list_round_trips(self):
        line = '  use m, only: ' + ', '.join(
            'symbol_number_{:02d}'.format(i) for i in range(20)
        )
        self.assertEqual(*_same(wrap_fortran_lines([line]), [line]))

    def test_call_round_trips(self):
        line = '    call sub(' + ', '.join(
            'argument_{:02d}=value_{:02d}'.format(i, i) for i in range(12)
        ) + ')'
        self.assertEqual(*_same(wrap_fortran_lines([line]), [line]))

    def test_literal_content_survives_byte_for_byte(self):
        message = (
            'this message is long enough to need continuing, and it holds '
            "a doubled quote in ''quoted'' text plus a comma, an "
            'exclamation mark! and trailing words to push it past the '
            'free-form column limit for certain'
        )
        line = "      errmsg = '{}'".format(message)
        self.assertEqual(*_same(wrap_fortran_lines([line]), [line]))

    def test_doubled_quote_is_never_split(self):
        # A ``''`` that straddles the cut would otherwise become a closing
        # delimiter followed by a stray opening one.
        for pad in range(40):
            message = 'a' * (LIMIT + pad) + "''" + 'b' * 40
            line = "  msg = '{}'".format(message)
            wrapped = wrap_fortran_lines([line])
            joined, expected = _same(wrapped, [line])
            self.assertEqual(joined, expected, 'pad={}'.format(pad))
            for out in wrapped:
                self.assertLessEqual(len(out), LIMIT, 'pad={}'.format(pad))

    def test_already_continued_line_keeps_its_ampersand(self):
        line = '        argument=' + 'f(' + ', '.join(
            'item_{:02d}'.format(i) for i in range(20)
        ) + '), &'
        wrapped = wrap_fortran_lines([line])
        self.assertTrue(wrapped[-1].endswith(', &'))
        self.assertEqual(*_same(wrapped, [line]))


class TestBreakPointsRespectLiterals(unittest.TestCase):
    """Commas and exclamation marks inside a literal are just text."""

    def test_comma_inside_a_literal_is_not_a_break_point(self):
        line = "    call sub(a, 'one, two, three', " + ', '.join(
            'argument_{:02d}'.format(i) for i in range(12)
        ) + ')'
        wrapped = wrap_fortran_lines([line])
        # No line may end inside the literal, i.e. at one of its commas.
        for out in wrapped:
            self.assertFalse(out.rstrip(' &').rstrip().endswith("'one,"))
            self.assertFalse(out.rstrip(' &').rstrip().endswith("two,"))
        self.assertEqual(*_same(wrapped, [line]))

    def test_exclamation_inside_a_literal_is_not_a_comment(self):
        line = "  msg = 'not a comment! " + 'x' * LIMIT + "'"
        wrapped = wrap_fortran_lines([line])
        self.assertEqual(*_same(wrapped, [line]))
        self.assertFalse(any(out.lstrip().startswith('!') for out in wrapped))


class TestImportListStyle(unittest.TestCase):
    """The import list takes the form the host cap already emits."""

    def setUp(self):
        self.wrapped = wrap_fortran_lines([
            '  use m, only: ' + ', '.join(
                'symbol_number_{:02d}'.format(i) for i in range(20)
            )
        ])

    def test_head_ends_at_only(self):
        self.assertEqual(self.wrapped[0], '  use m, only: &')

    def test_one_symbol_per_continuation_line(self):
        for index, out in enumerate(self.wrapped[1:]):
            self.assertTrue(out.startswith('    symbol_number_'))
            self.assertEqual(out.count(','), 0 if index == 19 else 1)

    def test_intrinsic_use_splits_at_the_import_list(self):
        line = ('  use, intrinsic :: iso_fortran_env, only: '
                + ', '.join('name_{:03d}'.format(i) for i in range(20)))
        wrapped = wrap_fortran_lines([line])
        self.assertEqual(
            wrapped[0], '  use, intrinsic :: iso_fortran_env, only: &'
        )
        self.assertEqual(*_same(wrapped, [line]))


class TestTrailingComment(unittest.TestCase):
    """A comment cannot be continued, so it moves above the statement."""

    def setUp(self):
        self.comment = 'unit conversion: um to m ' + 'and more words ' * 8
        self.code = '    x = 1.0E-6_kind_phys*y'
        self.wrapped = wrap_fortran_lines(
            ['{}  ! {}'.format(self.code, self.comment)]
        )

    def test_comment_comes_first(self):
        self.assertTrue(self.wrapped[0].lstrip().startswith('!'))

    def test_statement_survives_unchanged(self):
        self.assertEqual(self.wrapped[-1], self.code)

    def test_comment_text_is_kept_whole(self):
        text = ' '.join(
            out.lstrip().lstrip('!').strip()
            for out in self.wrapped if out.lstrip().startswith('!')
        )
        self.assertEqual(text.split(), self.comment.split())

    def test_comment_lines_fit(self):
        for out in self.wrapped:
            self.assertLessEqual(len(out), LIMIT)


class TestUnbreakableLineRaises(unittest.TestCase):
    """An over-long line with nowhere to break is a generator bug."""

    def test_raises_naming_the_file(self):
        line = '  ' + 'a' * (LIMIT * 2)
        with self.assertRaises(CCPPError) as caught:
            wrap_fortran_lines([line], path='ccpp_bogus_cap.F90')
        message = str(caught.exception)
        self.assertIn('ccpp_bogus_cap.F90', message)
        self.assertIn(str(LIMIT), message)


class TestWriteFortran(unittest.TestCase):
    """The writer wraps on the way to disk and keeps no-op semantics."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        self.path = os.path.join(self._tmpdir, 'ccpp_sample_cap.F90')
        self.lines = [
            'module m',
            '  use m2, only: ' + ', '.join(
                'symbol_number_{:02d}'.format(i) for i in range(20)
            ),
            'end module m',
        ]

    def test_written_file_is_within_the_limit(self):
        write_fortran(self.path, self.lines)
        with open(self.path) as fh:
            written = fh.read().splitlines()
        for out in written:
            self.assertLessEqual(len(out), LIMIT)
        self.assertEqual(*_same(written, self.lines))

    def test_unchanged_rewrite_is_a_no_op(self):
        write_fortran(self.path, self.lines)
        before = os.stat(self.path).st_mtime_ns
        write_fortran(self.path, self.lines)
        self.assertEqual(os.stat(self.path).st_mtime_ns, before)


def load_tests(loader, tests, ignore):
    import generator.fortran_lines as fl
    tests.addTests(doctest.DocTestSuite(fl))
    return tests


if __name__ == '__main__':
    unittest.main()
