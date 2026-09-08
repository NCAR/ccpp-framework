"""Unit tests for generator.trace."""

import doctest
import unittest

from generator.trace import (
    emit_module_gate,
    emit_trace_block,
    ensure_error_unit_use,
)


class _FakeEntry:
    """Minimal HostVarEntry stand-in for the trace helper."""

    def __init__(self, standard_name, local_name, type_):
        self.standard_name = standard_name
        self.local_name    = local_name
        self.type          = type_


class TestEmitModuleGate(unittest.TestCase):

    def test_default_off(self):
        self.assertEqual(
            emit_module_gate(False, '  '),
            ['  logical, parameter :: trace = .false.'],
        )

    def test_default_on(self):
        self.assertEqual(
            emit_module_gate(True, '  '),
            ['  logical, parameter :: trace = .true.'],
        )

    def test_indent_preserved(self):
        self.assertEqual(
            emit_module_gate(False, '    '),
            ['    logical, parameter :: trace = .false.'],
        )


class TestEnsureErrorUnitUse(unittest.TestCase):

    def test_appends_when_absent(self):
        out = ensure_error_unit_use([], '  ')
        self.assertEqual(
            out,
            ['  use, intrinsic :: iso_fortran_env, only: error_unit'],
        )

    def test_idempotent(self):
        existing = ['  use, intrinsic :: iso_fortran_env, only: error_unit']
        out = ensure_error_unit_use(list(existing), '  ')
        self.assertEqual(out, existing)

    def test_appends_after_other_uses(self):
        out = ensure_error_unit_use(['  use foo, only: bar'], '  ')
        self.assertEqual(out, [
            '  use foo, only: bar',
            '  use, intrinsic :: iso_fortran_env, only: error_unit',
        ])

    def test_no_substring_false_positive(self):
        # ``my_error_unit_proxy`` must not be matched as ``error_unit``.
        out = ensure_error_unit_use(['  use foo, only: my_error_unit_proxy'], '  ')
        self.assertEqual(out, [
            '  use foo, only: my_error_unit_proxy',
            '  use, intrinsic :: iso_fortran_env, only: error_unit',
        ])


class TestEmitTraceBlock(unittest.TestCase):

    def _ctrl_in(self):
        return [
            _FakeEntry('horizontal_loop_begin', 'lb', 'integer'),
            _FakeEntry('horizontal_loop_end',   'ub', 'integer'),
            _FakeEntry('thread_number',         'thread_num', 'integer'),
        ]

    def _ctrl_out(self):
        return [
            _FakeEntry('ccpp_error_code',    'errflg', 'integer'),
            _FakeEntry('ccpp_error_message', 'errmsg', 'character'),
        ]

    def test_emits_for_intent_in_dummies(self):
        out = emit_trace_block('my_sub', self._ctrl_in(), '    ')
        self.assertEqual(out, [
            "    if (trace) write(error_unit, "
            "'(a,a,1x,i0,a,1x,i0,a,1x,i0)') &",
            "        'CCPP TRACE my_sub:', &",
            "        ' lb=', lb, &",
            "        ' ub=', ub, &",
            "        ' thread_num=', thread_num",
        ])

    def test_filters_intent_out(self):
        # An intent(out) entry should not appear in the trace.
        out = emit_trace_block(
            'my_sub', self._ctrl_in() + self._ctrl_out(), '    ',
        )
        joined = '\n'.join(out)
        self.assertNotIn('errflg', joined)
        self.assertNotIn('errmsg', joined)
        self.assertIn('lb', joined)
        self.assertIn('thread_num', joined)

    def test_character_wrapped_in_trim(self):
        entries = [_FakeEntry('suite_name', 'suite_name', 'character')]
        out = emit_trace_block('my_sub', entries, '    ')
        self.assertEqual(out, [
            "    if (trace) write(error_unit, '(a,a,a)') &",
            "        'CCPP TRACE my_sub:', &",
            "        ' suite_name=', trim(suite_name)",
        ])

    def test_empty_when_only_intent_out(self):
        out = emit_trace_block('my_sub', self._ctrl_out(), '    ')
        self.assertEqual(out, [])

    def test_empty_when_no_entries(self):
        out = emit_trace_block('my_sub', [], '    ')
        self.assertEqual(out, [])

    def test_instance_local_appended(self):
        out = emit_trace_block(
            'my_sub', self._ctrl_in(), '    ', instance_local='inst_num',
        )
        joined = '\n'.join(out)
        self.assertIn("' inst_num=', inst_num", joined)

    def test_instance_local_not_duplicated_when_already_in_entries(self):
        entries = self._ctrl_in() + [
            _FakeEntry('instance_number', 'inst_num', 'integer'),
        ]
        out = emit_trace_block(
            'my_sub', entries, '    ', instance_local='inst_num',
        )
        joined = '\n'.join(out)
        # Should appear exactly once.
        self.assertEqual(joined.count(" inst_num=', inst_num"), 1)

    def test_signature_order_preserved(self):
        entries = [
            _FakeEntry('a', 'first',  'integer'),
            _FakeEntry('b', 'second', 'integer'),
            _FakeEntry('c', 'third',  'integer'),
        ]
        out = emit_trace_block('my_sub', entries, '    ')
        first  = out.index("        ' first=', first, &")
        second = out.index("        ' second=', second, &")
        third  = out.index("        ' third=', third")
        self.assertLess(first, second)
        self.assertLess(second, third)

    def test_format_string_mixes_a_and_i0(self):
        entries = [
            _FakeEntry('suite_name', 'suite_name', 'character'),
            _FakeEntry('horizontal_loop_begin', 'lb', 'integer'),
            _FakeEntry('horizontal_loop_end',   'ub', 'integer'),
        ]
        out = emit_trace_block('my_sub', entries, '    ')
        # First line carries the format: ``a`` for trace name, then for
        # each item ``a`` (label) followed by ``a`` (char) or
        # ``1x,i0`` (integer).
        self.assertEqual(
            out[0],
            "    if (trace) write(error_unit, "
            "'(a,a,a,a,1x,i0,a,1x,i0)') &",
        )

    def test_continuation_only_after_last_var_omitted(self):
        entries = [
            _FakeEntry('a', 'one', 'integer'),
            _FakeEntry('b', 'two', 'integer'),
        ]
        out = emit_trace_block('my_sub', entries, '    ')
        # Every line but the last must end with ``&``.
        for line in out[:-1]:
            self.assertTrue(
                line.rstrip().endswith('&'),
                msg='line missing continuation: {!r}'.format(line),
            )
        self.assertFalse(out[-1].rstrip().endswith('&'))


def load_tests(loader, tests, ignore):
    import generator.trace as t
    tests.addTests(doctest.DocTestSuite(t))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
