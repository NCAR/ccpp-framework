"""Unit tests for generator.suite_cap."""

import doctest
import os
import tempfile
import unittest
from unittest.mock import MagicMock

from metadata.metadata_table import parse_metadata_file
from metadata.variable_resolver import build_flat_host_dict, SchemeStore
from generator.suite_resolver import resolve_suite, ResolvedGroup
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
    _parse,
    _sf,
)


def _resolve():
    hd    = _load_full_host_dict()
    store = _load_scheme_store()
    suite = _parse_suite('suite_test_simple.xml')
    return resolve_suite(suite, store, hd), store


def _generate():
    suite_resolution, store = _resolve()
    return _generate_suite_cap('test_simple', suite_resolution, store)


class TestAllSuiteSchemeNames(unittest.TestCase):

    def test_single_scheme(self):
        suite_resolution, _ = _resolve()
        names = _all_suite_scheme_names(suite_resolution)
        self.assertIn('temp_calc_adjust', names)

    def test_no_duplicates(self):
        suite_resolution, _ = _resolve()
        names = _all_suite_scheme_names(suite_resolution)
        self.assertEqual(len(names), len(set(names)))


class TestSchemesWithRegister(unittest.TestCase):

    def test_none_have_register(self):
        suite_resolution, store = _resolve()
        names = _all_suite_scheme_names(suite_resolution)
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
        suite_resolution, _ = _resolve()
        # temp_calc_adjust uses errmsg/errflg which are now control vars.
        args = _suite_ctrl_args_for_phase(suite_resolution, 'run')
        std_names = {a.standard_name for a in args}
        self.assertEqual(std_names, {'ccpp_error_message', 'ccpp_error_code'})

    def test_unknown_phase_returns_empty(self):
        suite_resolution, _ = _resolve()
        args = _suite_ctrl_args_for_phase(suite_resolution, 'register')
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
            'call physics_state_alloc(1, errmsg, errflg)',
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
        self.assertIn('call physics_state_dealloc(errmsg, errflg)', self.text)


class TestPhysicsDispatch(unittest.TestCase):

    def setUp(self):
        self.text = '\n'.join(_generate())

    def test_run_dispatch_present(self):
        # No control vars in test setup → no-arg signature.
        self.assertIn('subroutine test_simple_physics_run()', self.text)
        self.assertIn('end subroutine test_simple_physics_run', self.text)

    def test_run_dispatches_to_group_cap(self):
        # No group_name control var → unconditional call, no select case.
        self.assertIn('call physics_run()', self.text)

    def test_init_dispatches_to_group_cap(self):
        self.assertIn('call physics_init()', self.text)

    def test_final_dispatches_to_group_cap(self):
        self.assertIn('call physics_final()', self.text)

    def test_timestep_init_dispatches_to_group_cap(self):
        # Group phase subroutines are always emitted so the state machine
        # transitions through every phase, even when no scheme has a routine
        # for that phase — so the dispatch must always call into the group cap.
        self.assertIn('subroutine test_simple_physics_timestep_init()', self.text)
        self.assertIn('call physics_timestep_init()', self.text)

    def test_timestep_final_dispatches_to_group_cap(self):
        self.assertIn('subroutine test_simple_physics_timestep_final()', self.text)
        self.assertIn('call physics_timestep_final()', self.text)

    def test_no_select_case_without_group_name_ctrl(self):
        # No group_name control var in test setup → no select case dispatch.
        self.assertNotIn('select case(trim(group_name))', self.text)


class TestGroupDispatchUnknownGroupError(unittest.TestCase):
    """When the host carries ``group_name`` in its control table the
    suite cap dispatches via ``select case(trim(group_name))``.  Every
    such dispatch MUST end with a ``case default`` that sets errflg=1
    and writes a message naming the unknown group — caller asking for a
    group this suite doesn't define is a runtime error, not a silent
    fall-through.
    """

    def setUp(self):
        suite_resolution, store = _resolve()
        self.text = '\n'.join(
            _generate_suite_cap('test_simple', suite_resolution, store, _load_full_host_dict())
        )

    def _phase_block(self, phase):
        sub = 'subroutine test_simple_physics_{}'.format(phase)
        start = self.text.index(sub)
        end   = self.text.index('end subroutine test_simple_physics_{}'.format(phase), start)
        return self.text[start:end]

    def test_run_dispatch_has_case_default(self):
        block = self._phase_block('run')
        # control_full.meta names the group_name local as grp_name; assert
        # against the local name rather than the standard name.
        self.assertIn('select case(trim(grp_name))', block)
        self.assertIn('case default', block)
        # errflg must be set non-zero in the default branch.
        self.assertRegex(block, r'case default[^!]*?errflg = 1')

    def test_default_message_names_unknown_group(self):
        block = self._phase_block('run')
        self.assertIn(
            "test_simple_physics_run: unknown group: ' // trim(grp_name)",
            block,
        )

    def test_default_branch_returns(self):
        """The default branch must ``return`` after setting errflg —
        otherwise execution falls out of the select and into any code
        that follows the dispatch (state transitions, etc.)."""
        block = self._phase_block('run')
        # Find the case-default section.
        case_idx = block.index('case default')
        end_idx  = block.index('end select', case_idx)
        default_block = block[case_idx:end_idx]
        self.assertIn('return', default_block)

    def test_all_phases_have_default_case(self):
        for phase in ('init', 'timestep_init', 'run', 'timestep_final', 'final'):
            block = self._phase_block(phase)
            self.assertIn('case default', block,
                          "phase '{}' missing case default".format(phase))


class TestGroupDispatchErrorPropagation(unittest.TestCase):
    """A ``group_name='all'`` dispatch must stop and return on the FIRST
    group's error.  Each group phase subroutine resets ``errflg=0`` on entry,
    so without a guard between group calls a later group's success would mask
    an earlier group's failure -- which then resurfaces downstream only as an
    "invalid group state" when ``run`` finds the failed group never reached
    ``IN_TIMESTEP``.  (Regression: CAM-SIMA cam4 physics_before_coupler.)"""

    def _two_group_run_all_block(self):
        sr, store = _resolve()
        g0 = sr.groups[0]
        # Synthesize a second group that shares the first's phase calls so the
        # case('', 'all') path emits two group calls.
        sr.groups.append(ResolvedGroup(
            group_name='physics_second',
            phase_calls=g0.phase_calls,
            dim_uses=g0.dim_uses,
        ))
        text = '\n'.join(
            _generate_suite_cap('test_simple', sr, store, _load_full_host_dict())
        )
        sub = 'subroutine test_simple_physics_run'
        s = text.index(sub)
        e = text.index('end ' + sub, s)
        block = text[s:e]
        a   = block.index("case('', 'all')")
        nxt = block.index("case('physics", a + 1)   # first individual group case
        return block[a:nxt]

    def test_guard_between_group_calls(self):
        all_block = self._two_group_run_all_block()
        # Each of the two group calls is followed by an errflg guard.
        self.assertEqual(
            all_block.count('if (errflg /= 0) return'), 2, all_block
        )
        # The guard after the first call must precede the second call so the
        # second group is unreachable once the first has failed.
        first_call  = all_block.index('call physics_run(')
        guard       = all_block.index('if (errflg /= 0) return', first_call)
        second_call = all_block.index('call physics_second_run(')
        self.assertLess(guard, second_call, all_block)


class TestWriteSuiteCap(unittest.TestCase):

    def test_writes_file(self):
        suite_resolution, store = _resolve()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_suite_cap('test_simple', suite_resolution, store, tmpdir)
            self.assertTrue(os.path.isfile(path))
            self.assertEqual(os.path.basename(path), 'ccpp_test_simple_cap.F90')

    def test_file_content(self):
        suite_resolution, store = _resolve()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_suite_cap('test_simple', suite_resolution, store, tmpdir)
            with open(path) as fh:
                content = fh.read()
            self.assertIn('module ccpp_test_simple_cap', content)
            # Register subroutine is always emitted now.
            self.assertIn('subroutine test_simple_register', content)
            self.assertTrue(content.endswith('\n'))

    def test_creates_output_dir(self):
        suite_resolution, store = _resolve()
        with tempfile.TemporaryDirectory() as tmpdir:
            subdir = os.path.join(tmpdir, 'caps')
            write_suite_cap('test_simple', suite_resolution, store, subdir)
            self.assertTrue(os.path.isdir(subdir))


class TestFinalSubroutineStateMachine(unittest.TestCase):
    """``<suite>_final`` transitions per-instance state to UNREGISTERED and
    triggers a last-to-leave dealloc when every instance has finalized."""

    def setUp(self):
        self.text = '\n'.join(_generate())

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
        self.suite_resolution    = resolve_suite(self.suite, self.store, self.hd)
        self.text  = '\n'.join(
            _generate_suite_cap('consume_consts', self.suite_resolution, self.store, self.hd)
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


class TestTraceEmission(unittest.TestCase):
    """The generated suite cap always carries a module-level ``trace``
    parameter (default .false.) and a gated ``write(error_unit,*)`` in
    every physics-dispatch subroutine; ``trace=True`` flips the default.
    """

    def setUp(self):
        self.suite_resolution, self.store = _resolve()
        self.hd = _load_full_host_dict()

    def test_module_gate_default_off(self):
        text = '\n'.join(_generate_suite_cap(
            'test_simple', self.suite_resolution, self.store, self.hd,
        ))
        self.assertIn('logical, parameter :: trace = .false.', text)

    def test_module_gate_default_on(self):
        text = '\n'.join(_generate_suite_cap(
            'test_simple', self.suite_resolution, self.store, self.hd,
            trace=True,
        ))
        self.assertIn('logical, parameter :: trace = .true.', text)
        self.assertNotIn('logical, parameter :: trace = .false.', text)

    def test_error_unit_use_unconditional(self):
        text = '\n'.join(_generate_suite_cap(
            'test_simple', self.suite_resolution, self.store, self.hd,
        ))
        self.assertIn(
            'use, intrinsic :: iso_fortran_env, only: error_unit', text,
        )

    def test_trace_block_present_in_physics_phases(self):
        text = '\n'.join(_generate_suite_cap(
            'test_simple', self.suite_resolution, self.store, self.hd,
        ))
        for phase in ('init', 'timestep_init', 'run',
                      'timestep_final', 'final'):
            self.assertIn(
                "'CCPP TRACE test_simple_physics_{}:'".format(phase),
                text,
                msg='trace string missing for phase {}'.format(phase),
            )

    def test_write_suite_cap_threads_trace_flag(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            out_path = write_suite_cap(
                'test_simple', self.suite_resolution, self.store, tmpdir,
                self.hd, trace=True,
            )
            with open(out_path) as fh:
                text = fh.read()
        self.assertIn('logical, parameter :: trace = .true.', text)


# A register-phase scheme that consumes a HOST array sliced by a HOST
# dimension (gases(1:n_gases)) and produces a constituent (so the register
# call body is emitted).  Mirrors rrtmgp_constituents_register, whose
# rad_climate(1:rad_climate_dimension) arg triggered the bug.
_REG_HOSTDIM_HOST = '''
[ccpp-table-properties]
  name = gasreg_host
  type = host
[ccpp-arg-table]
  name = gasreg_host
  type = host
[ n_gases ]
  standard_name = gas_list_dimension
  units = count
  dimensions = ()
  type = integer
[ gases ]
  standard_name = list_of_gases
  units = none
  type = character | kind = len=256
  dimensions = (gas_list_dimension)
'''

_REG_HOSTDIM_SCHEME = '''
[ccpp-table-properties]
  name = gas_register
  type = scheme
[ccpp-arg-table]
  name = gas_register_register
  type = scheme
[ gases ]
  standard_name = list_of_gases
  units = none
  type = character | kind = len=256
  dimensions = (gas_list_dimension)
  intent = in
[ dyn_consts ]
  standard_name = gasreg_dyn_consts
  units = none
  type = ccpp_constituent_properties_t
  allocatable = True
  dimensions = (:)
  intent = out
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
'''

_REG_HOSTDIM_SUITE = (
    '<?xml version="1.0"?>\n'
    '<suite name="gasreg" version="1.0">\n'
    '  <group name="phys">\n'
    '    <scheme>gas_register</scheme>\n'
    '  </group>\n'
    '</suite>\n'
)


class TestRegisterHostDimensionImported(unittest.TestCase):
    """A register-phase scheme arg that is a host array sliced by a host
    dimension must import BOTH the array and its dimension symbol into
    <suite>_register.  Regression for the rrtmgp
    'rad_climate_dimension has no IMPLICIT type' suite-cap error."""

    @classmethod
    def setUpClass(cls):
        import logging
        from generator.suite_xml import parse_suite_xml
        host_tbls = _parse(_REG_HOSTDIM_HOST, 'gasreg_host.meta')
        ctrl_tbls = parse_metadata_file(_sf('control_full.meta'))
        hd = build_flat_host_dict(host_tbls, ctrl_tbls, [])
        store = SchemeStore.build_from(
            _parse(_REG_HOSTDIM_SCHEME, 'gas_register.meta'))
        with tempfile.TemporaryDirectory() as tmp:
            sx = os.path.join(tmp, 'suite_gasreg.xml')
            with open(sx, 'w') as fh:
                fh.write(_REG_HOSTDIM_SUITE)
            suite = parse_suite_xml(sx, tmp, logging.getLogger('test'),
                                    skip_validation=True)
            sr = resolve_suite(suite, store, hd)
        cls.text = '\n'.join(_generate_suite_cap('gasreg', sr, store, hd))
        cls.reg = cls.text.split('subroutine gasreg_register')[1].split(
            'end subroutine gasreg_register')[0]

    def test_call_slices_array_by_dimension(self):
        # The call subscript references the host dimension's local name.
        self.assertIn('gases(1:n_gases)', self.reg)

    def test_dimension_symbol_imported(self):
        # Both the array and the dimension must be USE'd from the host module.
        self.assertRegex(self.reg, r'use gasreg_host, only:[^\n]*\bn_gases\b')
        self.assertRegex(self.reg, r'use gasreg_host, only:[^\n]*\bgases\b')


def load_tests(loader, tests, ignore):
    import generator.suite_cap as subcycle
    tests.addTests(doctest.DocTestSuite(subcycle))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
