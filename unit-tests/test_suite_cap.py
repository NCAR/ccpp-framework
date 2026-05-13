"""Unit tests for generator.suite_cap."""

import doctest
import os
import tempfile
import unittest
from unittest.mock import MagicMock

from metadata.metadata_table import parse_metadata_file
from metadata.variable_resolver import build_flat_host_dict, SchemeStore
from generator.suite_resolver import resolve_suite
from generator.suite_cap import (
    _all_suite_scheme_names,
    _schemes_with_register,
    _suite_ctrl_args_for_phase,
    _generate_suite_cap,
    write_suite_cap,
)
from test_suite_resolver import (
    _load_full_host_dict,
    _load_scheme_store,
    _parse_suite,
)


def _resolve():
    hd    = _load_full_host_dict()
    store = _load_scheme_store()
    suite = _parse_suite('suite_test_simple.xml')
    return resolve_suite(suite, store, hd), store


def _generate():
    sr, store = _resolve()
    return _generate_suite_cap('test_simple', sr, store)


class TestAllSuiteSchemeNames(unittest.TestCase):

    def test_single_scheme(self):
        sr, _ = _resolve()
        names = _all_suite_scheme_names(sr)
        self.assertIn('temp_calc_adjust', names)

    def test_no_duplicates(self):
        sr, _ = _resolve()
        names = _all_suite_scheme_names(sr)
        self.assertEqual(len(names), len(set(names)))


class TestSchemesWithRegister(unittest.TestCase):

    def test_none_have_register(self):
        sr, store = _resolve()
        names = _all_suite_scheme_names(sr)
        reg = _schemes_with_register(names, store)
        # temp_calc_adjust has no register phase.
        self.assertEqual(reg, [])

    def test_scheme_with_register(self):
        store = MagicMock()
        store.phases_for.side_effect = (
            lambda n: ['register', 'run'] if n == 'my_scheme' else ['run']
        )
        result = _schemes_with_register(['my_scheme', 'other_scheme'], store)
        self.assertEqual(result, ['my_scheme'])


class TestSuiteCtrlArgsForPhase(unittest.TestCase):

    def test_only_error_ctrl_args_in_test_case(self):
        sr, _ = _resolve()
        # temp_calc_adjust uses errmsg/errflg which are now control vars.
        args = _suite_ctrl_args_for_phase(sr, 'run')
        std_names = {a.standard_name for a in args}
        self.assertEqual(std_names, {'ccpp_error_message', 'ccpp_error_code'})

    def test_unknown_phase_returns_empty(self):
        sr, _ = _resolve()
        args = _suite_ctrl_args_for_phase(sr, 'register')
        self.assertEqual(args, [])


class TestGenerateSuiteCapModule(unittest.TestCase):
    """Suite cap with no register-providing schemes: constituent USE and
    <suite>_register are NOT emitted (conditional emission)."""

    def setUp(self):
        self.lines = _generate()
        self.text  = '\n'.join(self.lines)

    def test_module_header_comment(self):
        self.assertTrue(self.lines[0].startswith('!'))
        self.assertIn('test_simple', self.lines[0])

    def test_module_declaration(self):
        self.assertIn('module ccpp_test_simple_cap', self.text)
        self.assertIn('end module ccpp_test_simple_cap', self.text)

    def test_does_not_use_constituent_mod(self):
        # No register-providing schemes → no constituent module dependency.
        self.assertNotIn('use ccpp_constituent_prop_mod', self.text)
        self.assertNotIn('ccpp_model_constituents_t', self.text)

    def test_uses_group_cap_mod(self):
        self.assertIn('use ccpp_test_simple_physics_cap', self.text)

    def test_implicit_none_private(self):
        self.assertIn('implicit none', self.text)
        self.assertIn('private', self.text)

    def test_public_register_always_emitted(self):
        # <suite>_register is mandatory in the new design — always public.
        self.assertIn('public :: test_simple_register', self.text)

    def test_public_init_final(self):
        self.assertIn('public :: test_simple_init', self.text)
        self.assertIn('public :: test_simple_final', self.text)

    def test_public_all_physics_phases(self):
        for phase in ('init', 'timestep_init', 'run', 'timestep_final', 'final'):
            self.assertIn(
                'public :: test_simple_physics_{}'.format(phase), self.text
            )

    def test_contains_block(self):
        self.assertIn('contains', self.lines)


class TestRegisterSubroutineAlwaysEmitted(unittest.TestCase):
    """``<suite>_register`` is mandatory and emitted unconditionally.  When no
    schemes have a register phase, the body is just the state-alloc + guard +
    state-transition skeleton; no scheme calls."""

    def setUp(self):
        lines = _generate()
        self.text = '\n'.join(lines)

    def test_register_subroutine_present(self):
        self.assertIn('subroutine test_simple_register', self.text)
        self.assertIn('end subroutine test_simple_register', self.text)

    def test_no_constituents_arg(self):
        # Constituents are now opt-in via type=host; not in the cap at all
        # when no register-phase scheme declares ccpp_constituent_properties_t.
        self.assertNotIn('constituents', self.text)
        self.assertNotIn('ccpp_constituent_prop_mod', self.text)

    def test_no_scheme_register_calls(self):
        # temp_calc_adjust has no register phase → no scheme_register call.
        self.assertNotIn('call temp_calc_adjust_register', self.text)

    def test_state_alloc_called(self):
        # Register always allocates state (idempotent) on first call.
        self.assertIn('call test_simple_suite_state_alloc', self.text)

    def test_idempotent_guard(self):
        # Per-instance idempotent skip if already at REGISTERED or beyond.
        self.assertIn('>= CCPP_SUITE_REGISTERED', self.text)

    def test_state_transition(self):
        self.assertIn('= CCPP_SUITE_REGISTERED', self.text)


class TestInitFinalSubroutines(unittest.TestCase):

    def setUp(self):
        self.text = '\n'.join(_generate())

    def test_init_subroutine(self):
        self.assertIn('subroutine test_simple_init(errmsg, errflg)', self.text)
        self.assertIn('end subroutine test_simple_init', self.text)

    def test_final_subroutine(self):
        self.assertIn('subroutine test_simple_final(errmsg, errflg)', self.text)
        self.assertIn('end subroutine test_simple_final', self.text)

    def test_init_calls_group_state_alloc(self):
        # No host_dict passed → single-instance → literal 1 for ninstances.
        self.assertIn(
            'call ccpp_test_simple_physics_state_alloc(1, errmsg, errflg)',
            self.text,
        )

    def test_register_calls_suite_state_alloc(self):
        # Suite state allocation happens in <suite>_register, not <suite>_init.
        # No host_dict → single-instance → literal 1 for ninstances.
        self.assertIn(
            'call test_simple_suite_state_alloc(1, errmsg, errflg)',
            self.text,
        )

    def test_final_calls_state_dealloc(self):
        self.assertIn('call ccpp_test_simple_physics_state_dealloc(errmsg, errflg)', self.text)


class TestPhysicsDispatch(unittest.TestCase):

    def setUp(self):
        self.text = '\n'.join(_generate())

    def test_run_dispatch_present(self):
        # No control vars in test setup → no-arg signature.
        self.assertIn('subroutine test_simple_physics_run()', self.text)
        self.assertIn('end subroutine test_simple_physics_run', self.text)

    def test_run_dispatches_to_group_cap(self):
        # No group_name control var → unconditional call, no select case.
        self.assertIn('call ccpp_test_simple_physics_run()', self.text)

    def test_init_dispatches_to_group_cap(self):
        self.assertIn('call ccpp_test_simple_physics_init()', self.text)

    def test_final_dispatches_to_group_cap(self):
        self.assertIn('call ccpp_test_simple_physics_final()', self.text)

    def test_timestep_init_dispatches_to_group_cap(self):
        # Group phase subroutines are always emitted so the state machine
        # transitions through every phase, even when no scheme has a routine
        # for that phase — so the dispatch must always call into the group cap.
        self.assertIn('subroutine test_simple_physics_timestep_init()', self.text)
        self.assertIn('call ccpp_test_simple_physics_timestep_init()', self.text)

    def test_timestep_final_dispatches_to_group_cap(self):
        self.assertIn('subroutine test_simple_physics_timestep_final()', self.text)
        self.assertIn('call ccpp_test_simple_physics_timestep_final()', self.text)

    def test_no_select_case_without_group_name_ctrl(self):
        # No group_name control var in test setup → no select case dispatch.
        self.assertNotIn('select case(trim(group_name))', self.text)


class TestWriteSuiteCap(unittest.TestCase):

    def test_writes_file(self):
        sr, store = _resolve()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_suite_cap('test_simple', sr, store, tmpdir)
            self.assertTrue(os.path.isfile(path))
            self.assertEqual(os.path.basename(path), 'ccpp_test_simple_cap.F90')

    def test_file_content(self):
        sr, store = _resolve()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_suite_cap('test_simple', sr, store, tmpdir)
            with open(path) as fh:
                content = fh.read()
            self.assertIn('module ccpp_test_simple_cap', content)
            # Register subroutine is always emitted now.
            self.assertIn('subroutine test_simple_register', content)
            self.assertTrue(content.endswith('\n'))

    def test_creates_output_dir(self):
        sr, store = _resolve()
        with tempfile.TemporaryDirectory() as tmpdir:
            subdir = os.path.join(tmpdir, 'caps')
            write_suite_cap('test_simple', sr, store, subdir)
            self.assertTrue(os.path.isdir(subdir))


class TestFinalSubroutineStateMachine(unittest.TestCase):
    """``<suite>_final`` transitions per-instance state to UNREGISTERED and
    triggers a last-to-leave dealloc when every instance has finalized."""

    def setUp(self):
        self.text = '\n'.join(_generate())

    def test_state_check_register_message(self):
        # The not-allocated guard now refers to ccpp_register, not ccpp_init.
        self.assertIn(
            'ccpp_register has not been called', self.text,
        )

    def test_idempotent_unregistered_skip(self):
        self.assertIn('== CCPP_SUITE_UNREGISTERED', self.text)

    def test_state_transition_to_unregistered(self):
        self.assertIn('= CCPP_SUITE_UNREGISTERED', self.text)

    def test_last_to_leave_dealloc(self):
        self.assertIn(
            'all(ccpp_suite_state == CCPP_SUITE_UNREGISTERED)', self.text,
        )


class TestSuiteCapNoConstituentDeclarations(unittest.TestCase):
    """Under option A the suite cap no longer owns constituent state.

    All declarations (constituent obj, pointers, index_of_<X>) live in
    the host-wide ``ccpp_host_constituents`` module.  The suite cap is
    responsible only for packing per-suite dynamic-constituent arrays
    into the shared buffer during ``<suite>_register``.
    """

    def setUp(self):
        from test_suite_resolver import (
            _load_constituent_host_dict,
            _load_constituent_consumer_store,
        )
        self.hd    = _load_constituent_host_dict()
        self.store = _load_constituent_consumer_store()
        self.suite = _parse_suite('suite_consume_constituent.xml')
        self.sr    = resolve_suite(self.suite, self.store, self.hd)
        self.text  = '\n'.join(
            _generate_suite_cap('consume_consts', self.sr, self.store, self.hd)
        )

    def test_no_kind_phys_import(self):
        # Suite cap doesn't need kind_phys — constituent arrays live elsewhere.
        self.assertNotIn('use ccpp_kinds, only: kind_phys', self.text)

    def test_no_constituent_pointer_type_import(self):
        self.assertNotIn('ccpp_constituent_prop_ptr_t', self.text)

    def test_no_module_level_pointers(self):
        self.assertNotIn(
            'pointer, public :: ccpp_constituents', self.text,
        )
        self.assertNotIn(
            'pointer, public :: ccpp_constituent_tendencies', self.text,
        )

    def test_no_index_of_X_in_suite_cap(self):
        self.assertNotIn('index_of_cloud_liquid_water_mixing_ratio', self.text)

    def test_no_const_index_call_in_init(self):
        init_body = self.text.split('subroutine consume_consts_init')[1].split(
            'end subroutine consume_consts_init'
        )[0]
        self.assertNotIn('%const_index(', init_body)
        self.assertNotIn('%vars_layer', init_body)


class TestSuiteCapNoConstituentEmissionWhenAbsent(unittest.TestCase):
    """When the suite does not reference any constituent state, the
    suite cap emits no constituent-related code."""

    def setUp(self):
        self.text = '\n'.join(_generate())

    def test_no_constituent_pointers(self):
        self.assertNotIn('=> null()', self.text)

    def test_no_index_of_declarations(self):
        self.assertNotIn('index_of_', self.text)

    def test_no_constituent_prop_ptr_type_import(self):
        self.assertNotIn('ccpp_constituent_prop_ptr_t', self.text)


def load_tests(loader, tests, ignore):
    import generator.suite_cap as sc
    tests.addTests(doctest.DocTestSuite(sc))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
