"""Unit tests for ``generator.host_constituents``."""

import doctest
import os
import unittest

from generator.suite_resolver import resolve_suite
from generator.host_constituents import (
    _any_constituent_state,
    _all_index_names,
    _suites_with_register_consts,
    _generate_host_constituents,
)


def _resolve_consumer():
    """Resolve the consume_constituent fixture; return (suite_resolution, host_dict)."""
    from test_suite_resolver import (
        _load_constituent_host_dict,
        _load_constituent_consumer_store,
        _parse_suite,
    )
    hd    = _load_constituent_host_dict()
    store = _load_constituent_consumer_store()
    suite = _parse_suite('suite_consume_constituent.xml')
    return resolve_suite(suite, store, hd), hd


def _resolve_register():
    """Resolve the register_constituents fixture; return (suite_resolution, host_dict)."""
    from test_suite_resolver import (
        _load_constituent_host_dict,
        _load_constituent_scheme_store,
        _parse_suite,
    )
    hd    = _load_constituent_host_dict()
    store = _load_constituent_scheme_store()
    suite = _parse_suite('suite_register_constituents.xml')
    return resolve_suite(suite, store, hd), hd


def _resolve_simple():
    """Resolve the no-constituent fixture; return (suite_resolution, host_dict)."""
    from test_suite_resolver import (
        _load_full_host_dict,
        _load_scheme_store,
        _parse_suite,
    )
    hd    = _load_full_host_dict()
    store = _load_scheme_store()
    suite = _parse_suite('suite_test_simple.xml')
    return resolve_suite(suite, store, hd), hd


def _render_consumer():
    suite_resolution, hd = _resolve_consumer()
    return '\n'.join(_generate_host_constituents([suite_resolution], host_dict=hd))


def _render_register():
    suite_resolution, hd = _resolve_register()
    return '\n'.join(_generate_host_constituents([suite_resolution], host_dict=hd))


class TestAggregationHelpers(unittest.TestCase):
    """``_any_constituent_state`` / ``_all_index_names`` / ``_suites_with_register_consts``."""

    def test_any_when_consumer_only(self):
        suite_resolution, _hd = _resolve_consumer()
        self.assertTrue(_any_constituent_state([suite_resolution]))

    def test_any_when_register_only(self):
        suite_resolution, _hd = _resolve_register()
        self.assertTrue(_any_constituent_state([suite_resolution]))

    def test_any_when_neither(self):
        suite_resolution, _hd = _resolve_simple()
        self.assertFalse(_any_constituent_state([suite_resolution]))

    def test_index_names_aggregated(self):
        consumer, _ch  = _resolve_consumer()
        register, _rh  = _resolve_register()
        names = _all_index_names([consumer, register])
        # Only the consumer side names a constituent in this fixture
        # (register-phase scheme produces dyn_const3, but that std name
        # is never read back via index_of_<X>).
        self.assertEqual(names, ['cloud_liquid_water_mixing_ratio'])

    def test_register_suites_listed(self):
        consumer, _ch  = _resolve_consumer()
        register, _rh  = _resolve_register()
        self.assertEqual(
            _suites_with_register_consts([consumer, register]),
            ['reg_consts'],
        )

    def test_register_suites_include_auto_cloned_only(self):
        # auto-clone-constituents: the legacy shim populates
        # ``SuiteResolution.auto_cloned_constituents`` but leaves
        # ``constituent_register_calls`` empty (no real register
        # scheme exists).  host_constituents.F90 still has to declare
        # the per-suite ``<suite>_dynamic_constituents`` buffer
        # because the suite cap emits a USE on it.  Regression for
        # CAM-SIMA kessler_test build (2026-06-03).
        from generator.suite_resolver import SuiteResolution, AutoCloneEntry
        sr = SuiteResolution(
            suite_name='kessler_test',
            auto_cloned_constituents=[AutoCloneEntry(
                std_name='water_vapor', long_name='', diag_name='qv',
                units='kg kg-1', vertical_dim='vertical_layer_dimension',
                advected=True, molar_mass=0.0, default_value=None,
                min_value=None, water_species=None, mixing_ratio_type=None,
            )],
        )
        self.assertEqual(
            _suites_with_register_consts([sr]),
            ['kessler_test'],
        )

    def test_register_suites_excludes_pure_consumer(self):
        # auto-clone-constituents: no register calls AND no auto-cloned
        # entries -> suite stays off the list so the buffer is not
        # declared.  Companion to test_register_suites_include_auto_cloned_only;
        # delete together when the shim retires.
        from generator.suite_resolver import SuiteResolution
        sr = SuiteResolution(suite_name='pure_consumer')
        self.assertEqual(
            _suites_with_register_consts([sr]),
            [],
        )


class TestModuleSkippedWhenNoConstituents(unittest.TestCase):
    """``_generate_host_constituents`` returns ``None`` when nothing touches
    constituent state — the module is not emitted at all."""

    def test_returns_none(self):
        suite_resolution, _hd = _resolve_simple()
        self.assertIsNone(_generate_host_constituents([suite_resolution]))


class TestModuleHeaderAndUses(unittest.TestCase):
    """Module declaration and external USEs are correct."""

    def setUp(self):
        self.text = _render_consumer()

    def test_module_name(self):
        self.assertIn('module ccpp_host_constituents', self.text)
        self.assertIn('end module ccpp_host_constituents', self.text)

    def test_use_kind(self):
        self.assertIn('use ccpp_kinds, only: kind_phys', self.text)

    def test_use_constituent_prop_mod(self):
        self.assertIn('use ccpp_constituent_prop_mod', self.text)
        self.assertIn('ccpp_model_constituents_t', self.text)
        self.assertIn('ccpp_constituent_properties_t', self.text)
        self.assertIn('ccpp_constituent_prop_ptr_t', self.text)


class TestStateDeclarations(unittest.TestCase):
    """Module-level state declarations: obj, pointers, integers."""

    def setUp(self):
        self.consumer_text = '\n'.join(
            _render_consumer().splitlines()
        )
        self.register_text = '\n'.join(
            _render_register().splitlines()
        )

    def test_constituent_obj_declared(self):
        # Per-instance allocatable array.
        self.assertIn(
            'type(ccpp_model_constituents_t), target, allocatable :: '
            'ccpp_model_constituents_obj(:)',
            self.consumer_text,
        )

    def test_obj_is_public(self):
        self.assertIn('public :: ccpp_model_constituents_obj', self.consumer_text)

    def test_no_module_level_pointers(self):
        # Under per-instance design, ccpp_constituents / ..._tendencies /
        # ..._properties / number_of_ccpp_constituents are NOT module-level
        # variables — they're accessed as members of the obj(inst) array.
        self.assertNotIn('pointer :: ccpp_constituents', self.consumer_text)
        self.assertNotIn(
            'pointer :: ccpp_constituent_tendencies', self.consumer_text,
        )
        self.assertNotIn(
            'pointer :: ccpp_constituent_properties', self.consumer_text,
        )
        self.assertNotIn(
            'integer :: number_of_ccpp_constituents', self.consumer_text,
        )

    def test_index_of_X_declared(self):
        self.assertIn(
            'integer :: index_of_cloud_liquid_water_mixing_ratio = int_unassigned',
            self.consumer_text,
        )
        self.assertIn(
            'public :: index_of_cloud_liquid_water_mixing_ratio',
            self.consumer_text,
        )

    def test_per_suite_buffer_declared_for_producer(self):
        # The per-suite buffer is a per-instance wrapper-DDT array so each
        # instance owns its own scheme-registered constituent property
        # objects (the wrapper type is emitted once at module scope).
        self.assertIn('type :: ccpp_dyn_const_buffer_t', self.register_text)
        self.assertIn(
            'type(ccpp_constituent_properties_t), allocatable :: items(:)',
            self.register_text,
        )
        self.assertIn(
            'type(ccpp_dyn_const_buffer_t), allocatable, target :: '
            'reg_consts_dynamic_constituents(:)',
            self.register_text,
        )
        self.assertIn(
            'public :: reg_consts_dynamic_constituents',
            self.register_text,
        )

    def test_no_per_suite_buffer_when_no_producer(self):
        # consumer fixture has no register-phase producer schemes — no
        # ``<suite>_dynamic_constituents`` array declaration, no public
        # of any such buffer.  (The ccpp_deallocate_dynamic_constituents
        # subroutine name is unrelated and is expected to appear.)
        self.assertNotIn('allocatable, target :: ', self.consumer_text)
        self.assertNotIn('_dynamic_constituents(:)', self.consumer_text)


class TestRegisterConstituentsRoutine(unittest.TestCase):
    """``ccpp_register_constituents`` merges host + per-suite buffers."""

    def setUp(self):
        # Use a SuiteResolution list containing BOTH consumer and register
        # fixtures so the routine iterates a real per-suite buffer.
        self.text = _render_register()

    def test_takes_host_constituents_and_instance(self):
        # instance_number AND number_of_instances are both in the signature
        # when the host declares the multi-instance pair.
        self.assertIn(
            'subroutine ccpp_register_constituents(host_constituents, '
            'inst_num, ninstances, errcode, errmsg)',
            self.text,
        )
        self.assertIn(
            'type(ccpp_constituent_properties_t), target, intent(in) :: '
            'host_constituents(:)',
            self.text,
        )
        self.assertIn('integer, intent(in) :: inst_num', self.text)

    def test_allocates_obj_array_on_first_call(self):
        # Idempotent allocation: only the first instance to call sees
        # an unallocated array; subsequent instances skip.
        self.assertIn(
            'if (.not. allocated(ccpp_model_constituents_obj)) then',
            self.text,
        )
        self.assertIn(
            'allocate(ccpp_model_constituents_obj(ninstances))', self.text,
        )

    def test_initializes_table_per_instance(self):
        self.assertIn(
            'call ccpp_model_constituents_obj(inst_num)%initialize_table(num_consts)',
            self.text,
        )
        # Count of scheme-registered constituents comes from THIS instance's
        # slot in the wrapper-DDT array, not the (no-longer-shared) buffer.
        self.assertIn(
            'num_consts = num_consts + size('
            'reg_consts_dynamic_constituents(inst_num)%items, 1)',
            self.text,
        )

    def test_iterates_host_then_suite_per_instance(self):
        body = self.text.split('subroutine ccpp_register_constituents')[1].split(
            'end subroutine ccpp_register_constituents'
        )[0]
        host_pos  = body.find('host_constituents(index)')
        suite_pos = body.find(
            'reg_consts_dynamic_constituents(inst_num)%items(index)'
        )
        self.assertGreater(host_pos, 0)
        self.assertGreater(suite_pos, host_pos)
        # All %new_field calls go through obj(inst_num).
        self.assertIn(
            'call ccpp_model_constituents_obj(inst_num)%new_field(const_prop',
            body,
        )

    def test_lock_table_called_per_instance(self):
        self.assertIn(
            'call ccpp_model_constituents_obj(inst_num)%lock_table('
            'errcode=errcode, errmsg=errmsg)',
            self.text,
        )


class TestInitializeConstituentsRoutine(unittest.TestCase):
    """``ccpp_initialize_constituents`` locks data + binds pointers + populates indices."""

    def setUp(self):
        self.text = _render_consumer()

    def test_takes_dimensions_and_instance(self):
        self.assertIn(
            'subroutine ccpp_initialize_constituents(ncols, num_layers, '
            'inst_num, errcode, errmsg)',
            self.text,
        )

    def test_calls_lock_data_per_instance(self):
        self.assertIn(
            'call ccpp_model_constituents_obj(inst_num)%lock_data('
            'ncols, num_layers, errcode=errcode, errmsg=errmsg)',
            self.text,
        )

    def test_uses_scheme_utils(self):
        body = self.text.split('subroutine ccpp_initialize_constituents')[1].split(
            'end subroutine ccpp_initialize_constituents'
        )[0]
        self.assertIn(
            'use ccpp_scheme_utils, only: ccpp_initialize_constituent_ptr',
            body,
        )

    def test_initialises_scheme_utils_pointer_per_instance(self):
        body = self.text.split('subroutine ccpp_initialize_constituents')[1].split(
            'end subroutine ccpp_initialize_constituents'
        )[0]
        # Need a local pointer variable to pass a non-pointer target as a
        # pointer dummy.
        self.assertIn(
            'type(ccpp_model_constituents_t), pointer :: const_obj_ptr',
            body,
        )
        self.assertIn(
            'const_obj_ptr => ccpp_model_constituents_obj(inst_num)', body,
        )
        self.assertIn(
            'call ccpp_initialize_constituent_ptr(const_obj_ptr)', body,
        )

    def test_no_module_pointer_binding(self):
        # The old module-level ccpp_constituents pointers don't exist;
        # this routine must NOT try to bind them.
        body = self.text.split('subroutine ccpp_initialize_constituents')[1].split(
            'end subroutine ccpp_initialize_constituents'
        )[0]
        self.assertNotIn('ccpp_constituents =>', body)
        self.assertNotIn('ccpp_constituent_tendencies =>', body)
        self.assertNotIn('ccpp_constituent_properties =>', body)
        self.assertNotIn(
            'number_of_ccpp_constituents =', body,
        )

    def test_queries_index_of_X_per_instance(self):
        self.assertIn(
            "call ccpp_model_constituents_obj(inst_num)%const_index("
            "index_of_cloud_liquid_water_mixing_ratio, "
            "'cloud_liquid_water_mixing_ratio', errcode=errcode, errmsg=errmsg)",
            self.text,
        )

    def test_int_unassigned_imported(self):
        # Used by the post-const_index validation block; must be in the
        # module-level USE so the contained subroutine can reference it.
        self.assertIn(
            'use ccpp_constituent_prop_mod, only:', self.text,
        )
        self.assertIn('int_unassigned', self.text)

    def test_post_const_index_validation_emitted(self):
        # %const_index doesn't error on a miss — it returns int_unassigned
        # and errcode=0.  The generator must check the integer afterward
        # and fail with a descriptive message so the host sees the bad
        # registration at init time instead of crashing on a -huge(1)
        # subscript later in run-phase scheme calls.
        body = self.text.split('subroutine ccpp_initialize_constituents')[1].split(
            'end subroutine ccpp_initialize_constituents'
        )[0]
        self.assertIn(
            'if (index_of_cloud_liquid_water_mixing_ratio == int_unassigned) then',
            body,
        )
        self.assertIn('errcode = 1', body)
        self.assertIn(
            "errmsg = 'ccpp_initialize_constituents: constituent "
            "''cloud_liquid_water_mixing_ratio'' is referenced by a "
            "scheme but is not in the registered constituent table",
            body,
        )


def _render_long_name_constituent():
    """Resolve a single-suite scheme that consumes ONE advected base
    constituent whose standard name is long enough to force
    ``_index_symbol_name`` to mangle the Fortran symbol, then render the
    host-constituents module.  Returns ``(text, long_std_name, symbol)``.

    Regression guard for the lossy round-trip bug: the mangled
    ``index_of_<X>`` Fortran symbol must NOT leak into the ``%const_index``
    lookup string (the framework keys on the real standard name) nor into
    ``ccpp_model_const_stdnames``.
    """
    import tempfile
    import logging
    from metadata.metadata_table import parse_metadata_file
    from metadata.variable_resolver import build_flat_host_dict, SchemeStore
    from generator.suite_xml import parse_suite_xml

    # 57 chars -> index_of_<name> is 66 > 63 -> mangled (same family as the
    # cam-sima kessler ``*_wrt_moist_air_and_condensed_water`` constituents).
    long_std = 'water_vapor_mixing_ratio_wrt_moist_air_and_condensed_water'
    from generator.suite_resolver import _index_symbol_name
    symbol = _index_symbol_name(long_std)
    assert symbol != 'index_of_' + long_std, 'fixture name must mangle'

    scheme_meta = '''
[ccpp-table-properties]
  name = long_const_scheme
  type = scheme
[ccpp-arg-table]
  name = long_const_scheme_run
  type = scheme
[ ncol ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
  intent = in
[ nz ]
  standard_name = vertical_layer_dimension
  units = count
  dimensions = ()
  type = integer
  intent = in
[ qv ]
  standard_name = %s
  units = kg kg-1
  dimensions = (horizontal_dimension, vertical_layer_dimension)
  type = real | kind = kind_phys
  intent = in
  advected = .true.
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
''' % long_std
    suite_xml = (
        '<?xml version="1.0"?>\n'
        '<suite name="longc" version="1.0">\n'
        '  <group name="phys">\n'
        '    <scheme>long_const_scheme</scheme>\n'
        '  </group>\n'
        '</suite>\n'
    )

    here = os.path.dirname(os.path.abspath(__file__))
    samples = os.path.join(here, 'sample_files')
    host_tbls = parse_metadata_file(os.path.join(samples, 'host_with_constituents.meta'))
    ctrl_tbls = parse_metadata_file(os.path.join(samples, 'control_full.meta'))
    fw_meta = os.path.join(
        os.path.dirname(here), 'capgen', 'src', 'ccpp_constituent_prop_mod.meta',
    )
    ddt_tbls = parse_metadata_file(fw_meta) if os.path.isfile(fw_meta) else []
    hd = build_flat_host_dict(host_tbls, ctrl_tbls, ddt_tbls)

    with tempfile.TemporaryDirectory() as tmp:
        sm = os.path.join(tmp, 'long_const_scheme.meta')
        with open(sm, 'w') as fh:
            fh.write(scheme_meta)
        sx = os.path.join(tmp, 'suite_longc.xml')
        with open(sx, 'w') as fh:
            fh.write(suite_xml)
        store = SchemeStore.build_from(parse_metadata_file(sm))
        suite = parse_suite_xml(sx, tmp, logging.getLogger('test'),
                                skip_validation=True)
        sr = resolve_suite(suite, store, hd)
        text = '\n'.join(_generate_host_constituents([sr], host_dict=hd))
    return text, long_std, symbol


class TestLongConstituentNameRoundTrip(unittest.TestCase):
    """Regression: a constituent std name long enough to mangle the Fortran
    ``index_of_<X>`` symbol must still key ``%const_index`` (and the
    ``ccpp_model_const_stdnames`` array) on the REAL standard name.  The old
    code recovered the name from the mangled symbol suffix, so the lookup
    string was corrupted, ``const_index`` never matched, and the index
    integer stayed at its ``0`` default -> out-of-bounds constituent
    subscript at run time (cam-sima kessler segfault)."""

    @classmethod
    def setUpClass(cls):
        cls.text, cls.long_std, cls.symbol = _render_long_name_constituent()

    def test_const_index_keys_on_real_std_name(self):
        # Mangled symbol as the integer; REAL std name as the lookup string.
        self.assertIn(
            "%const_index({}, '{}', errcode=errcode, errmsg=errmsg)".format(
                self.symbol, self.long_std,
            ),
            self.text,
        )

    def test_mangled_suffix_not_used_as_lookup_string(self):
        # The mangled suffix must never appear as a quoted lookup key.
        mangled_suffix = self.symbol[len('index_of_'):]
        self.assertNotIn("'{}'".format(mangled_suffix), self.text)

    def test_stdnames_array_lists_real_name(self):
        self.assertIn("'{}'".format(self.long_std), self.text)

    def test_subscript_symbol_declared_and_public(self):
        # The (mangled) symbol must still be declared, public, and reset.
        self.assertIn(
            'integer :: {} = int_unassigned'.format(self.symbol), self.text)
        self.assertIn('public :: {}'.format(self.symbol), self.text)

    def test_index_defaults_and_resets_to_unassigned(self):
        # Declaration default is the unbound sentinel (so a never-bound index
        # trips the init guard instead of becoming a 0 subscript)...
        self.assertIn(
            'integer :: {} = int_unassigned'.format(self.symbol), self.text)
        # ...and the deallocate routine resets it to the same sentinel.
        dealloc = self.text.split(
            'subroutine ccpp_deallocate_dynamic_constituents'
        )[1].split('end subroutine ccpp_deallocate_dynamic_constituents')[0]
        self.assertIn('{} = int_unassigned'.format(self.symbol), dealloc)
        self.assertNotIn('{} = 0'.format(self.symbol), dealloc)


class TestIsSchemeConstituent(unittest.TestCase):
    """``ccpp_is_scheme_constituent`` + the module-scope std-name parameter array."""

    def setUp(self):
        self.text = _render_consumer()

    def test_subroutine_signature(self):
        self.assertIn(
            'subroutine ccpp_is_scheme_constituent(var_name, '
            'constituent_exists, errcode, errmsg)',
            self.text,
        )

    def test_uses_known_array(self):
        self.assertIn(
            'constituent_exists = any(ccpp_model_const_stdnames == var_name)',
            self.text,
        )

    def test_param_array_declared(self):
        self.assertIn(
            "character(len=31), parameter :: ccpp_model_const_stdnames(1) = (/ &",
            self.text,
        )
        self.assertIn("'cloud_liquid_water_mixing_ratio'", self.text)

    def test_param_array_public(self):
        self.assertIn('public :: ccpp_model_const_stdnames', self.text)


class TestIsSchemeConstituentNoIndices(unittest.TestCase):
    """When the suite registers constituents but none are referenced via
    ``index_of_<X>``, ``ccpp_is_scheme_constituent`` returns ``.false.``
    unconditionally — no parameter array is emitted."""

    def setUp(self):
        self.text = _render_register()

    def test_falls_back_to_constant_false(self):
        self.assertIn('constituent_exists = .false.', self.text)

    def test_no_param_array(self):
        self.assertNotIn('ccpp_model_const_stdnames', self.text)


class TestWrapperSubroutines(unittest.TestCase):
    """Thin wrappers for number / gather / update / const_index."""

    def setUp(self):
        self.text = _render_consumer()

    def test_number_constituents(self):
        self.assertIn(
            'subroutine ccpp_number_constituents(num_flds, advected, '
            'inst_num, errcode, errmsg)',
            self.text,
        )
        self.assertIn(
            'call ccpp_model_constituents_obj(inst_num)%num_constituents('
            'num_flds, advected=advected, errcode=errcode, errmsg=errmsg)',
            self.text,
        )

    def test_gather_constituents(self):
        self.assertIn(
            'call ccpp_model_constituents_obj(inst_num)%copy_in('
            'const_array, errcode=errcode, errmsg=errmsg)',
            self.text,
        )

    def test_update_constituents(self):
        self.assertIn(
            'call ccpp_model_constituents_obj(inst_num)%copy_out('
            'const_array, errcode=errcode, errmsg=errmsg)',
            self.text,
        )

    def test_const_get_index(self):
        # Keyword args ensure unambiguous mapping to the DDT's signature
        # (index, standard_name, errcode, errmsg).
        self.assertIn(
            'call ccpp_model_constituents_obj(inst_num)%const_index('
            'standard_name=stdname, index=const_index, '
            'errcode=errcode, errmsg=errmsg)',
            self.text,
        )


class TestAccessorFunctions(unittest.TestCase):
    """Pointer-returning accessor functions."""

    def setUp(self):
        self.text = _render_consumer()

    def test_constituents_array(self):
        self.assertIn(
            'function ccpp_constituents_array(inst_num) result(const_ptr)',
            self.text,
        )
        self.assertIn(
            'const_ptr => ccpp_model_constituents_obj(inst_num)%field_data_ptr()',
            self.text,
        )

    def test_advected_constituents_array(self):
        self.assertIn(
            'function ccpp_advected_constituents_array(inst_num) result(const_ptr)',
            self.text,
        )
        self.assertIn(
            'const_ptr => ccpp_model_constituents_obj(inst_num)'
            '%advected_constituents_ptr()',
            self.text,
        )

    def test_model_const_properties(self):
        self.assertIn(
            'function ccpp_model_const_properties(inst_num) result(const_ptr)',
            self.text,
        )
        self.assertIn(
            'const_ptr => ccpp_model_constituents_obj(inst_num)'
            '%constituent_props_ptr()',
            self.text,
        )


class TestDeallocateRoutine(unittest.TestCase):
    """``ccpp_deallocate_dynamic_constituents`` is per-instance with
    last-to-leave teardown of shared buffers + the obj array."""

    def setUp(self):
        self.text = _render_register()
        self.body = self.text.split(
            'subroutine ccpp_deallocate_dynamic_constituents'
        )[1].split('end subroutine ccpp_deallocate_dynamic_constituents')[0]

    def test_takes_instance_number(self):
        self.assertIn(
            'subroutine ccpp_deallocate_dynamic_constituents(inst_num)',
            self.text,
        )
        self.assertIn('integer, intent(in) :: inst_num', self.body)

    def test_per_instance_reset(self):
        self.assertIn(
            'call ccpp_model_constituents_obj(inst_num)%reset()', self.body,
        )

    def test_short_circuit_when_unallocated(self):
        # If no instance has registered yet, the call is a no-op.
        self.assertIn(
            'if (.not. allocated(ccpp_model_constituents_obj)) return',
            self.body,
        )

    def test_last_to_leave_check(self):
        self.assertIn('all_done = .true.', self.body)
        self.assertIn('do i = 1, size(ccpp_model_constituents_obj, 1)', self.body)
        self.assertIn(
            'if (ccpp_model_constituents_obj(i)%const_props_locked()) then',
            self.body,
        )
        self.assertIn('all_done = .false.', self.body)

    def test_last_to_leave_teardown(self):
        self.assertIn('if (all_done) then', self.body)
        self.assertIn('deallocate(ccpp_model_constituents_obj)', self.body)
        # The per-suite buffer is NOT torn down here — that's owned by
        # the suite-cap lifecycle (deallocated in <suite>_final's
        # last-to-leave block).  Tearing it down here would break the
        # next ccpp_register call (suite_state guard skips re-fill).
        self.assertNotIn(
            'deallocate(reg_consts_dynamic_constituents)', self.body,
        )

    def test_no_module_pointer_nullify(self):
        # The module-level pointer set was removed; deallocate routine
        # must not try to nullify them.
        self.assertNotIn('nullify(ccpp_constituents)', self.body)
        self.assertNotIn('nullify(ccpp_constituent_tendencies)', self.body)
        self.assertNotIn('nullify(ccpp_constituent_properties)', self.body)
        self.assertNotIn('number_of_ccpp_constituents = 0', self.body)


def load_tests(loader, tests, ignore):
    import generator.host_constituents as hc
    tests.addTests(doctest.DocTestSuite(hc))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
