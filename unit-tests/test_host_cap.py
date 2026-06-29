"""Unit tests for generator.host_cap."""

import doctest
import os
import tempfile
import unittest
from unittest.mock import MagicMock

from metadata.parse_tools import CCPPError
from generator.suite_resolver import resolve_suite
from generator.host_cap import (
    _all_ctrl_args_for_phase,
    _arg_top_level_name,
    _build_local_to_std_top_level_map,
    _collect_host_io,
    _emit_var_set_loop,
    _generate_host_cap,
    _suite_io_subroutine,
    _suite_list_subroutine,
    _suite_part_list_subroutine,
    _suite_schemes_subroutine,
    write_host_cap,
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
    return resolve_suite(suite, store, hd)


def _generate():
    suite_resolution = _resolve()
    return _generate_host_cap('test_host', ['test_simple'], [suite_resolution])


class TestAllCtrlArgsForPhase(unittest.TestCase):

    def test_only_error_ctrl_args_in_test_case(self):
        suite_resolution   = _resolve()
        # temp_calc_adjust uses errmsg/errflg which are now control vars.
        args = _all_ctrl_args_for_phase([suite_resolution], 'run')
        std_names = {a.standard_name for a in args}
        self.assertEqual(std_names, {'ccpp_error_message', 'ccpp_error_code'})

    def test_mismatched_lengths_raises(self):
        from metadata.parse_tools import CCPPError
        with self.assertRaises(CCPPError):
            _generate_host_cap('test_host', ['a', 'b'], [_resolve()])


class TestGenerateHostCapModule(unittest.TestCase):
    """Static API: ccpp_register/init/final are mandatory entry points and
    are always emitted with the minimal lifecycle signature."""

    def setUp(self):
        self.lines = _generate()
        self.text  = '\n'.join(self.lines)

    def test_module_header_comment(self):
        self.assertTrue(self.lines[0].startswith('!'))
        self.assertIn('test_host_ccpp_cap', self.lines[0])

    def test_module_declaration(self):
        self.assertIn('module test_host_ccpp_cap', self.text)
        self.assertIn('end module test_host_ccpp_cap', self.text)

    def test_does_not_use_constituent_mod(self):
        # Constituent merging is now opt-in via type=host (Task #6 follow-up).
        self.assertNotIn('use ccpp_constituent_prop_mod', self.text)
        self.assertNotIn('ccpp_model_constituents_t', self.text)

    def test_uses_suite_cap(self):
        self.assertIn('use ccpp_test_simple_cap', self.text)
        # Register is now mandatory and always imported.
        self.assertIn('test_simple_register', self.text)
        self.assertIn('test_simple_init', self.text)
        self.assertIn('test_simple_final', self.text)

    def test_implicit_none_private(self):
        self.assertIn('implicit none', self.text)
        self.assertIn('private', self.text)

    def test_ccpp_register_public(self):
        # ccpp_register is mandatory.
        self.assertIn('public :: ccpp_register', self.text)

    def test_other_public_entry_points(self):
        for ep in ('ccpp_init', 'ccpp_final',
                   'ccpp_physics_init', 'ccpp_physics_timestep_init',
                   'ccpp_physics_run', 'ccpp_physics_timestep_final',
                   'ccpp_physics_final'):
            self.assertIn('public :: {}'.format(ep), self.text)

    def test_contains_block(self):
        self.assertIn('contains', self.lines)

    def test_no_constituent_reexport_when_absent(self):
        # The test_simple fixture has no constituents — host_constituents
        # module isn't emitted, so host_cap must not USE or re-export it.
        self.assertNotIn('use ccpp_host_constituents', self.text)
        self.assertNotIn('ccpp_register_constituents', self.text)
        self.assertNotIn('ccpp_initialize_constituents', self.text)


class TestHostCapConstituentReexport(unittest.TestCase):
    """When any suite uses constituent state, host_cap USEs
    ccpp_host_constituents and re-publics every host-facing routine plus
    the constituent object so hosts can ``use <host>_ccpp_cap, only: ...``
    for everything they need from CCPP."""

    def setUp(self):
        from test_suite_resolver import (
            _load_constituent_host_dict,
            _load_constituent_consumer_store,
            _parse_suite,
        )
        hd    = _load_constituent_host_dict()
        store = _load_constituent_consumer_store()
        suite = _parse_suite('suite_consume_constituent.xml')
        suite_resolution    = resolve_suite(suite, store, hd)
        self.text = '\n'.join(
            _generate_host_cap('test_host', ['consume_consts'], [suite_resolution], host_dict=hd,
                                 scheme_store=store),
        )

    def _expected_symbols(self):
        return [
            'ccpp_model_constituents_obj',
            'ccpp_register_constituents',
            'ccpp_initialize_constituents',
            'ccpp_is_scheme_constituent',
            'ccpp_number_constituents',
            'ccpp_gather_constituents',
            'ccpp_update_constituents',
            'ccpp_const_get_index',
            'ccpp_constituents_array',
            'ccpp_advected_constituents_array',
            'ccpp_model_const_properties',
            'ccpp_deallocate_dynamic_constituents',
        ]

    def test_uses_host_constituents_module(self):
        self.assertIn('use ccpp_host_constituents, only:', self.text)

    def test_all_symbols_imported(self):
        for sym in self._expected_symbols():
            self.assertIn(sym, self.text,
                          'symbol not imported: {}'.format(sym))

    def test_all_symbols_re_public(self):
        for sym in self._expected_symbols():
            self.assertIn('public :: {}'.format(sym), self.text)


class TestCcppRegisterMandatory(unittest.TestCase):
    """ccpp_register is always emitted with the minimal lifecycle signature
    and dispatches to every suite's <suite>_register routine."""

    def setUp(self):
        self.text = '\n'.join(_generate())

    def test_subroutine_present(self):
        self.assertIn('subroutine ccpp_register(', self.text)
        self.assertIn('end subroutine ccpp_register', self.text)

    def test_signature_minimal_no_host_dict(self):
        # With no host_dict, fallback local names are used.
        self.assertIn(
            'subroutine ccpp_register(suite_name, errflg, errmsg)', self.text,
        )

    def test_no_constituents_arg(self):
        # Constituents handling is opt-in; not in the signature any longer.
        sig_block = self.text.split('subroutine ccpp_register')[1].split(
            'end subroutine ccpp_register'
        )[0]
        self.assertNotIn('constituents', sig_block)

    def test_dispatches_to_suite_register(self):
        self.assertIn("case('test_simple')", self.text)
        self.assertIn('call test_simple_register(errmsg, errflg)', self.text)

    def test_default_case_error(self):
        self.assertIn("'ccpp_register: unknown suite:", self.text)


class TestCcppInitFinalSubroutines(unittest.TestCase):

    def setUp(self):
        self.text = '\n'.join(_generate())

    def test_init_subroutine(self):
        self.assertIn(
            'subroutine ccpp_init(suite_name, errflg, errmsg)', self.text,
        )
        self.assertIn('call test_simple_init(errmsg, errflg)', self.text)
        self.assertIn("'ccpp_init: unknown suite:", self.text)

    def test_final_subroutine(self):
        self.assertIn(
            'subroutine ccpp_final(suite_name, errflg, errmsg)', self.text,
        )
        self.assertIn('call test_simple_final(errmsg, errflg)', self.text)
        self.assertIn("'ccpp_final: unknown suite:", self.text)


class TestCcppPhysicsSubroutines(unittest.TestCase):

    def setUp(self):
        self.text = '\n'.join(_generate())

    def test_all_physics_subroutines_present(self):
        for phase in ('init', 'timestep_init', 'run', 'timestep_final', 'final'):
            self.assertIn('subroutine ccpp_physics_{}'.format(phase), self.text)

    def test_run_dispatches_to_suite_cap(self):
        # No host_dict passed → no ctrl entries → no-arg call to suite cap.
        self.assertIn('call test_simple_physics_run()', self.text)

    def test_init_dispatches_to_suite_cap(self):
        self.assertIn('call test_simple_physics_init()', self.text)

    def test_final_dispatches_to_suite_cap(self):
        self.assertIn('call test_simple_physics_final()', self.text)

    def test_physics_no_default_error_case_without_host_dict(self):
        # When no host_dict is available the standard error-reporting
        # control vars (ccpp_error_code / ccpp_error_message) aren't in
        # scope, so the physics dispatch has nowhere to write a "unknown
        # suite" message — case default is intentionally omitted.
        run_block_start = self.text.index('subroutine ccpp_physics_run')
        run_block_end   = self.text.index('end subroutine ccpp_physics_run')
        run_block = self.text[run_block_start:run_block_end]
        self.assertNotIn('case default', run_block)

    def test_select_case_on_suite_name(self):
        self.assertIn('select case(trim(suite_name))', self.text)


class TestCcppPhysicsUnknownSuiteErrors(unittest.TestCase):
    """When the host provides ccpp_error_code / ccpp_error_message in
    its control table, the physics dispatch ``select case`` MUST end
    with a ``case default`` that sets errflg=1 and writes a message
    naming the unknown suite — never silently fall through.
    """

    def setUp(self):
        hd  = _load_full_host_dict()
        suite_resolution  = _resolve()
        self.text = '\n'.join(_generate_host_cap('test_host', ['test_simple'], [suite_resolution], hd))

    def test_physics_run_has_default_case_with_errflg(self):
        run_block_start = self.text.index('subroutine ccpp_physics_run')
        run_block_end   = self.text.index('end subroutine ccpp_physics_run')
        run_block = self.text[run_block_start:run_block_end]
        self.assertIn('case default', run_block)
        # errflg must be set non-zero in the default branch.
        self.assertRegex(run_block, r'case default[^!]*?errflg = 1')

    def test_physics_run_default_message_names_suite(self):
        run_block_start = self.text.index('subroutine ccpp_physics_run')
        run_block_end   = self.text.index('end subroutine ccpp_physics_run')
        run_block = self.text[run_block_start:run_block_end]
        self.assertIn(
            "ccpp_physics_run: unknown suite: ' // trim(suite_name)",
            run_block,
        )


class TestMultipleSuites(unittest.TestCase):
    """Static API with two suites uses select case for both."""

    def setUp(self):
        suite_resolution = _resolve()
        from copy import deepcopy
        sr2 = deepcopy(suite_resolution)
        sr2.suite_name = 'suite_b'
        lines = _generate_host_cap('test_host', ['test_simple', 'suite_b'], [suite_resolution, sr2])
        self.text = '\n'.join(lines)

    def test_both_suites_in_register(self):
        # ccpp_register dispatches to all suites.
        self.assertIn("case('test_simple')", self.text)
        self.assertIn("case('suite_b')", self.text)

    def test_both_suite_caps_used(self):
        self.assertIn('use ccpp_test_simple_cap', self.text)
        self.assertIn('use ccpp_suite_b_cap', self.text)


class TestWriteHostCap(unittest.TestCase):

    def test_writes_file(self):
        suite_resolution = _resolve()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_host_cap('test_host', ['test_simple'], [suite_resolution], tmpdir)
            self.assertTrue(os.path.isfile(path))
            self.assertEqual(os.path.basename(path), 'test_host_ccpp_cap.F90')

    def test_file_content(self):
        suite_resolution = _resolve()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_host_cap('test_host', ['test_simple'], [suite_resolution], tmpdir)
            with open(path) as fh:
                content = fh.read()
            self.assertIn('module test_host_ccpp_cap', content)
            # ccpp_register is now mandatory and always emitted.
            self.assertIn('subroutine ccpp_register', content)
            self.assertTrue(content.endswith('\n'))

    def test_creates_output_dir(self):
        suite_resolution = _resolve()
        with tempfile.TemporaryDirectory() as tmpdir:
            subdir = os.path.join(tmpdir, 'api')
            write_host_cap('test_host', ['test_simple'], [suite_resolution], subdir)
            self.assertTrue(os.path.isdir(subdir))

    def test_returns_absolute_path(self):
        suite_resolution = _resolve()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_host_cap('test_host', ['test_simple'], [suite_resolution], tmpdir)
            self.assertTrue(os.path.isabs(path))


class TestCcppInitMultiInstance(unittest.TestCase):
    """ccpp_init includes instance_number when the host provides it; the new
    minimal signature drops number_of_instances entirely."""

    def setUp(self):
        hd = _load_full_host_dict()
        suite_resolution = _resolve()
        lines = _generate_host_cap('test_host', ['test_simple'], [suite_resolution], hd)
        self.text = '\n'.join(lines)

    def test_init_signature_has_instance_pair(self):
        # host_full.meta declares the multi-instance pair (inst_num,
        # ninstances).  Both must appear in the lifecycle signature.
        self.assertIn(
            'subroutine ccpp_init(suite_name, errflg, errmsg, inst_num, ninstances)',
            self.text,
        )

    def test_init_signature_has_ninstances(self):
        init_block = self.text.split('subroutine ccpp_init')[1].split(
            'end subroutine ccpp_init'
        )[0]
        self.assertIn('ninstances', init_block)

    def test_init_passes_inst_pair_to_suite(self):
        self.assertIn(
            'call test_simple_init(inst_num, ninstances, errmsg, errflg)',
            self.text,
        )

    def test_register_signature_has_instance_pair(self):
        self.assertIn(
            'subroutine ccpp_register(suite_name, errflg, errmsg, inst_num, ninstances)',
            self.text,
        )

    def test_final_signature_has_instance_pair(self):
        # Final carries (inst_num, ninstances) for API symmetry with
        # register/init even though the framework doesn't read
        # ninstances at final time.
        self.assertIn(
            'subroutine ccpp_final(suite_name, errflg, errmsg, inst_num, ninstances)',
            self.text,
        )


class TestCcppInitSingleInstance(unittest.TestCase):
    """ccpp_init drops inst_num when the host doesn't declare instance_number."""

    def setUp(self):
        hd = {k: v for k, v in _load_full_host_dict().items()
              if k not in ('number_of_instances', 'instance_number')}
        suite_resolution = _resolve()
        lines = _generate_host_cap('test_host', ['test_simple'], [suite_resolution], hd)
        self.text = '\n'.join(lines)

    def test_init_signature_no_instance_args(self):
        self.assertIn(
            'subroutine ccpp_init(suite_name, errflg, errmsg)', self.text,
        )
        init_block = self.text.split('subroutine ccpp_init')[1].split(
            'end subroutine ccpp_init'
        )[0]
        self.assertNotIn('ninstances', init_block)
        self.assertNotIn('inst_num', init_block)

    def test_init_passes_no_extra_args_to_suite(self):
        self.assertIn('call test_simple_init(errmsg, errflg)', self.text)


########################################################################
# Suite-introspection: helpers
########################################################################

class TestEmitVarSetLoop(unittest.TestCase):

    def test_basic(self):
        out = _emit_var_set_loop('x', ['a', 'b'], '  ')
        self.assertEqual(out, ['  allocate(x(2))', "  x(1) = 'a'", "  x(2) = 'b'"])

    def test_empty(self):
        out = _emit_var_set_loop('x', [], '  ')
        self.assertEqual(out, ['  allocate(x(0))'])

    def test_no_allocate(self):
        out = _emit_var_set_loop('x', ['a'], '    ', allocate=False)
        self.assertEqual(out, ["    x(1) = 'a'"])


class TestBuildLocalToStdTopLevelMap(unittest.TestCase):
    """Reverse map covers top-level host_dict entries only (no DDT-leaf rows)."""

    def test_includes_top_level_entries(self):
        # host_full has only plain leaves (no DDTs) → all entries top-level.
        hd = _load_full_host_dict()
        m  = _build_local_to_std_top_level_map(hd)
        self.assertEqual(m['gt0'], 'air_temperature')
        self.assertEqual(m['ncols'], 'horizontal_dimension')

    def test_excludes_ddt_leaves(self):
        from metadata.metadata_table import parse_metadata_file
        from metadata.variable_resolver import build_flat_host_dict
        hd = build_flat_host_dict(
            parse_metadata_file(os.path.join(
                os.path.dirname(__file__), 'sample_files',
                'host_with_ddt_instance.meta')),
            [],
            parse_metadata_file(os.path.join(
                os.path.dirname(__file__), 'sample_files',
                'ddt_simple.meta')),
        )
        m = _build_local_to_std_top_level_map(hd)
        # The DDT instance itself appears (top-level).
        self.assertEqual(m['gfs_statein'], 'gfs_statein')
        # DDT-leaf local names ('phii', 'phil') are excluded.
        self.assertNotIn('phii', m)
        self.assertNotIn('phil', m)

    def test_none_returns_empty(self):
        self.assertEqual(_build_local_to_std_top_level_map(None), {})


class TestArgTopLevelName(unittest.TestCase):
    """Collapse a flat DDT-leaf back to its top-level DDT instance name."""

    def _make_arg(self, std_name, access_path):
        """Mock ResolvedArg with just the fields _arg_top_level_name reads."""
        host_entry = MagicMock()
        host_entry.access_path = access_path
        arg = MagicMock()
        arg.standard_name = std_name
        arg.host_entry = host_entry
        return arg

    def test_plain_leaf_unchanged(self):
        arg = self._make_arg('air_temperature', 'gt0')
        self.assertEqual(_arg_top_level_name(arg, {}), 'air_temperature')

    def test_ddt_leaf_collapsed(self):
        arg = self._make_arg(
            'geopotential_at_interface',
            'gfs_statein(instance_number)%phii',
        )
        m = {'gfs_statein': 'gfs_statein'}
        self.assertEqual(_arg_top_level_name(arg, m), 'gfs_statein')

    def test_nested_ddt_collapses_to_outermost(self):
        arg = self._make_arg('inner_field', 'outer(2)%inner%fld')
        m = {'outer': 'outer_std_name'}
        self.assertEqual(_arg_top_level_name(arg, m), 'outer_std_name')

    def test_unmapped_root_falls_back_to_arg_std_name(self):
        # If the root local_name isn't in the map (inconsistent metadata),
        # fall back to the arg's own standard_name.
        arg = self._make_arg('foo', 'unknown(1)%bar')
        self.assertEqual(_arg_top_level_name(arg, {}), 'foo')

    def test_no_host_entry(self):
        arg = MagicMock()
        arg.standard_name = 'baz'
        arg.host_entry = None
        self.assertEqual(_arg_top_level_name(arg, {'x': 'y'}), 'baz')


class TestCollectHostIo(unittest.TestCase):
    """_collect_host_io: intent partitioning, control-var exclusion, sort."""

    def setUp(self):
        self.suite_resolution = _resolve()
        self.hd = _load_full_host_dict()

    def test_includes_control_vars(self):
        # temp_calc_adjust declares errflg/errmsg with intent=out — they
        # appear in outputs.  Matches original capgen's introspection.
        inputs, outputs = _collect_host_io(self.suite_resolution, self.hd)
        self.assertIn('ccpp_error_code',    outputs)
        self.assertIn('ccpp_error_message', outputs)
        # …and not in inputs (intent=out only, not inout).
        self.assertNotIn('ccpp_error_code',    inputs)
        self.assertNotIn('ccpp_error_message', inputs)

    def test_includes_host_args(self):
        inputs, outputs = _collect_host_io(self.suite_resolution, self.hd)
        # air_temperature is intent=inout in run phase → both lists.
        self.assertIn('air_temperature', inputs)
        self.assertIn('air_temperature', outputs)

    def test_sorted_outputs(self):
        inputs, outputs = _collect_host_io(self.suite_resolution, self.hd)
        self.assertEqual(inputs, sorted(inputs))
        self.assertEqual(outputs, sorted(outputs))

    def test_collapse_ddts_no_ddts_unchanged(self):
        # host_full has no DDTs → collapse is a no-op.
        flat_in, flat_out = _collect_host_io(self.suite_resolution, self.hd, collapse_ddts=False)
        coll_in, coll_out = _collect_host_io(self.suite_resolution, self.hd, collapse_ddts=True)
        self.assertEqual(flat_in, coll_in)
        self.assertEqual(flat_out, coll_out)

    def test_no_host_dict_collapse_falls_back(self):
        # collapse_ddts=True without host_dict must not raise.
        # With no DDTs the result is identical to the non-collapsed view.
        no_hd_in, _ = _collect_host_io(self.suite_resolution, None, collapse_ddts=True)
        flat_in, _  = _collect_host_io(self.suite_resolution, self.hd, collapse_ddts=False)
        self.assertEqual(no_hd_in, flat_in)


class TestCollectHostIoIncludesNonHostSources(unittest.TestCase):
    """_collect_host_io includes constituent args + register-phase
    ccpp_constituent_properties_t args + control vars in the introspection
    lists (matches original capgen).  Only suite-owned vars are excluded."""

    def setUp(self):
        from test_suite_resolver import (
            _load_constituent_host_dict,
            _load_constituent_consumer_store,
            _load_constituent_scheme_store,
            _parse_suite,
        )
        # Mix: a register-phase producer scheme + a consumer scheme.
        # Build a SuiteResolution for each, pass both to _collect_host_io.
        self.hd = _load_constituent_host_dict()
        consumer_store = _load_constituent_consumer_store()
        register_store = _load_constituent_scheme_store()
        consumer_suite = _parse_suite('suite_consume_constituent.xml')
        register_suite = _parse_suite('suite_register_constituents.xml')
        self.consumer_sr = resolve_suite(consumer_suite, consumer_store, self.hd)
        self.register_sr = resolve_suite(register_suite, register_store, self.hd)

    def test_consumer_base_constituent_in_inputs(self):
        inputs, _ = _collect_host_io(self.consumer_sr, self.hd)
        # cldliq is intent=in advected=true → in inputs.
        self.assertIn('cloud_liquid_water_mixing_ratio', inputs)

    def test_consumer_tendency_in_outputs(self):
        _, outputs = _collect_host_io(self.consumer_sr, self.hd)
        # tend_cldliq is intent=out constituent=true → in outputs.
        self.assertIn(
            'tendency_of_cloud_liquid_water_mixing_ratio', outputs,
        )

    def test_register_phase_properties_t_in_outputs(self):
        # The register-phase scheme declares dyn_const as intent=out
        # ccpp_constituent_properties_t — appears in the output list.
        _, outputs = _collect_host_io(self.register_sr, self.hd)
        self.assertIn('dynamic_constituents_for_register_test', outputs)

    def test_control_vars_in_outputs(self):
        # The register scheme also declares errmsg/errflg with intent=out.
        _, outputs = _collect_host_io(self.register_sr, self.hd)
        self.assertIn('ccpp_error_code',    outputs)
        self.assertIn('ccpp_error_message', outputs)


class TestCollectHostIoIncludesFrameworkDims(unittest.TestCase):
    """``number_of_ccpp_constituents`` (and any other framework-constituent
    dim that appears only as a dim token in scheme metadata) is included
    in the introspection inputs list — matches original capgen.  Host-side
    dims (horizontal_dimension, vertical_layer_dimension) are NOT
    included; they're stable host structure."""

    def setUp(self):
        from test_suite_resolver import (
            _load_full_host_dict, _load_scheme_store, _parse_suite,
        )
        # Use the consume_constituent fixture — the scheme references
        # number_of_ccpp_constituents as a dim of ccpp_constituents.
        # But that fixture only declares 2D constituent vars, not the
        # 3D ccpp_constituents directly.  Build a minimal fixture
        # specifically for this test.
        from metadata.metadata_table import _parse_lines
        from metadata.variable_resolver import SchemeStore
        scheme_text = (
            '[ccpp-table-properties]\n'
            '  name = uses_const_array\n'
            '  type = scheme\n'
            '[ccpp-arg-table]\n'
            '  name = uses_const_array_run\n'
            '  type = scheme\n'
            '[ const ]\n'
            '  standard_name = ccpp_constituents\n'
            '  units = none\n'
            '  dimensions = (horizontal_dimension, vertical_layer_dimension, '
            'number_of_ccpp_constituents)\n'
            '  type = real | kind = kind_phys\n'
            '  intent = inout\n'
        )
        tables = _parse_lines(scheme_text.splitlines(keepends=True), 't.meta')
        store  = SchemeStore.build_from(tables)

        # Use the host-only dict (no ccpp_model_constituents_t DDT
        # instance) so the host-wins rule doesn't fire and the scheme's
        # ccpp_constituents arg routes through capgen's
        # auto-provisioning path — that's the code path that surfaces
        # number_of_ccpp_constituents as an input via used_const_dim_std_names.
        self.hd = _load_full_host_dict()
        # Parse a one-scheme suite XML inline.
        import tempfile, os, logging
        from generator.suite_xml import parse_suite_xml
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="usca" version="1.0">\n'
            '  <group name="g"><scheme>uses_const_array</scheme></group>\n'
            '</suite>\n'
        )
        with tempfile.TemporaryDirectory() as tmp:
            xml_path = os.path.join(tmp, 'suite.xml')
            with open(xml_path, 'w') as fh:
                fh.write(suite_xml)
            suite = parse_suite_xml(xml_path, tmp, logging.getLogger('t'),
                                    skip_validation=True)
        self.suite_resolution = resolve_suite(suite, store, self.hd)

    def test_number_of_ccpp_constituents_in_inputs(self):
        inputs, _ = _collect_host_io(self.suite_resolution, self.hd)
        self.assertIn('number_of_ccpp_constituents', inputs)

    def test_horizontal_dim_not_in_inputs(self):
        inputs, _ = _collect_host_io(self.suite_resolution, self.hd)
        # Sanity: the host-side dims are NOT included even though they
        # appear as scheme arg dimensions.
        self.assertNotIn('horizontal_dimension',     inputs)
        self.assertNotIn('vertical_layer_dimension', inputs)


class TestCollectHostIoIncludesSubcycleLoopBound(unittest.TestCase):
    """A subcycle ``loop="<std_name>"`` bound is supplied by the host
    (it controls the per-cap do-loop count) and must therefore appear
    in the introspection inputs list — otherwise a host comparing its
    declared variables against ``ccpp_physics_suite_variables`` will
    silently miss the dependency."""

    def setUp(self):
        from test_suite_resolver import (
            _load_full_host_dict, _load_scheme_store, _parse_suite,
        )
        from metadata.metadata_table import _parse_lines
        from metadata.variable_resolver import build_flat_host_dict
        from generator.suite_resolver import resolve_suite

        # Pair the standard host_full + scheme_multipart with a small
        # extension that declares the subcycle-count std name.
        helper_src = '''
[ccpp-table-properties]
  name = subcycle_helper
  type = host
[ccpp-arg-table]
  name = subcycle_helper
  type = host
[ n_sub ]
  standard_name = num_subcycles_for_my_scheme
  units = count
  dimensions = ()
  type = integer
'''
        helper_tbls = _parse_lines(
            helper_src.splitlines(keepends=True), 'h.meta',
        )

        from test_suite_resolver import _SAMPLES_DIR
        from metadata.metadata_table import parse_metadata_file
        host_tbls = parse_metadata_file(
            os.path.join(_SAMPLES_DIR, 'host_full.meta')
        )
        ctrl_tbls = parse_metadata_file(
            os.path.join(_SAMPLES_DIR, 'control_full.meta')
        )
        host_only = [t for t in host_tbls if t.table_type == 'host']
        ctrl_only = [t for t in ctrl_tbls if t.table_type == 'control']
        ddt_only  = [t for t in host_tbls if t.table_type == 'ddt']
        self.hd = build_flat_host_dict(host_only + helper_tbls, ctrl_only, ddt_only)

        scheme_store = _load_scheme_store()

        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="1.0">\n'
            '  <group name="g">\n'
            '    <subcycle loop="num_subcycles_for_my_scheme">\n'
            '      <scheme>temp_calc_adjust</scheme>\n'
            '    </subcycle>\n'
            '  </group>\n'
            '</suite>\n'
        )
        import tempfile, logging
        from generator.suite_xml import parse_suite_xml
        with tempfile.TemporaryDirectory() as tmp:
            xml_path = os.path.join(tmp, 's.xml')
            with open(xml_path, 'w') as fh:
                fh.write(suite_xml)
            suite = parse_suite_xml(xml_path, tmp, logging.getLogger('t'),
                                    skip_validation=True)
        self.suite_resolution = resolve_suite(suite, scheme_store, self.hd)

    def test_subcycle_std_name_in_inputs(self):
        inputs, _outputs = _collect_host_io(self.suite_resolution, self.hd)
        self.assertIn('num_subcycles_for_my_scheme', inputs)

    def test_integer_literal_subcycle_does_not_pollute(self):
        """A literal-integer subcycle bound (``loop="3"``) has no
        ``loop_std_name`` set and therefore contributes nothing to
        the inputs list."""
        from test_suite_resolver import (
            _load_scheme_store, _load_full_host_dict,
        )
        from generator.suite_resolver import resolve_suite
        import tempfile, logging
        from generator.suite_xml import parse_suite_xml

        hd = _load_full_host_dict()
        store = _load_scheme_store()
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="1.0">\n'
            '  <group name="g">\n'
            '    <subcycle loop="3">\n'
            '      <scheme>temp_calc_adjust</scheme>\n'
            '    </subcycle>\n'
            '  </group>\n'
            '</suite>\n'
        )
        with tempfile.TemporaryDirectory() as tmp:
            xml_path = os.path.join(tmp, 's.xml')
            with open(xml_path, 'w') as fh:
                fh.write(suite_xml)
            suite = parse_suite_xml(xml_path, tmp, logging.getLogger('t'),
                                    skip_validation=True)
        suite_resolution = resolve_suite(suite, store, hd)
        inputs, _ = _collect_host_io(suite_resolution, hd)
        # No spurious integer / std-name additions from the literal loop.
        self.assertNotIn('3', inputs)


class TestCollectHostIoIncludesActiveExpr(unittest.TestCase):
    """A flag referenced via ``active=(<flag>)`` on a host variable
    must appear in the introspection inputs list — even if no scheme
    declares it as a direct argument.  The host needs to know the
    suite consumes the flag to decide whether the optional variables
    are present."""

    def setUp(self):
        from metadata.metadata_table import _parse_lines
        from metadata.variable_resolver import build_flat_host_dict, SchemeStore
        from generator.suite_resolver import resolve_suite
        import tempfile, logging
        from generator.suite_xml import parse_suite_xml

        # Host has a flag (``flag_for_passive_check``) referenced only as
        # an active=() expression on another host var.  No scheme takes
        # the flag as a direct argument — without the active-expr walk
        # in _collect_host_io it would silently disappear.
        #
        # The matching scheme arg is declared optional, which the
        # resolver's active+optional coherence check requires (host
        # ``active`` means the host's variable is only valid when the
        # condition holds; the cap honors that via the optional/
        # pointer-association pattern).
        host_src = '''
[ccpp-table-properties]
  name = active_helper
  type = host
[ccpp-arg-table]
  name = active_helper
  type = host
[ ncols ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
[ nlev ]
  standard_name = vertical_layer_dimension
  units = count
  dimensions = ()
  type = integer
[ flag_passive ]
  standard_name = flag_for_passive_check
  units = flag
  dimensions = ()
  type = logical
[ dt ]
  standard_name = time_step_for_physics
  units = s
  dimensions = ()
  type = real
  kind = kind_phys
[ gt0 ]
  standard_name = air_temperature
  units = K
  dimensions = (horizontal_dimension, vertical_layer_dimension)
  type = real
  kind = kind_phys
  active = (flag_for_passive_check)
'''
        # Inline scheme whose ``temp`` arg is optional — paired with the
        # host-side active above.
        scheme_src = '''
[ccpp-table-properties]
  name = active_scheme
  type = scheme
[ccpp-arg-table]
  name = active_scheme_run
  type = scheme
[ im ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
  intent = in
[ temp ]
  standard_name = air_temperature
  units = K
  dimensions = (horizontal_dimension, vertical_layer_dimension)
  type = real
  kind = kind_phys
  intent = inout
  optional = True
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
        from test_suite_resolver import _SAMPLES_DIR
        from metadata.metadata_table import parse_metadata_file
        ctrl_tbls = parse_metadata_file(
            os.path.join(_SAMPLES_DIR, 'control_full.meta')
        )
        host_tbls = _parse_lines(host_src.splitlines(keepends=True), 'h.meta')
        ctrl_only = [t for t in ctrl_tbls if t.table_type == 'control']
        self.hd = build_flat_host_dict(host_tbls, ctrl_only, [])

        scheme_tbls = _parse_lines(
            scheme_src.splitlines(keepends=True), 's.meta',
        )
        store = SchemeStore.build_from(scheme_tbls)
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="1.0">\n'
            '  <group name="g"><scheme>active_scheme</scheme></group>\n'
            '</suite>\n'
        )
        with tempfile.TemporaryDirectory() as tmp:
            xml_path = os.path.join(tmp, 's.xml')
            with open(xml_path, 'w') as fh:
                fh.write(suite_xml)
            suite = parse_suite_xml(xml_path, tmp, logging.getLogger('t'),
                                    skip_validation=True)
        self.suite_resolution = resolve_suite(suite, store, self.hd)

    def test_active_flag_in_inputs(self):
        inputs, _outputs = _collect_host_io(self.suite_resolution, self.hd)
        self.assertIn('flag_for_passive_check', inputs)

    def test_active_flag_not_in_outputs(self):
        """The flag is a pure input — it must not leak into outputs."""
        _, outputs = _collect_host_io(self.suite_resolution, self.hd)
        self.assertNotIn('flag_for_passive_check', outputs)


########################################################################
# Suite-introspection: ccpp_physics_suite_list
########################################################################

class TestSuiteListSubroutine(unittest.TestCase):

    def test_single_suite(self):
        text = '\n'.join(_suite_list_subroutine(['test_simple']))
        self.assertIn('subroutine ccpp_physics_suite_list(suites)', text)
        self.assertIn(
            'character(len=*), allocatable, intent(out) :: suites(:)', text,
        )
        self.assertIn('allocate(suites(1))', text)
        self.assertIn("suites(1) = 'test_simple'", text)
        self.assertIn('end subroutine ccpp_physics_suite_list', text)

    def test_multi_suite(self):
        text = '\n'.join(_suite_list_subroutine(['a', 'b', 'c']))
        self.assertIn('allocate(suites(3))', text)
        self.assertIn("suites(1) = 'a'", text)
        self.assertIn("suites(2) = 'b'", text)
        self.assertIn("suites(3) = 'c'", text)


########################################################################
# Suite-introspection: ccpp_physics_suite_part_list
########################################################################

class TestSuitePartListSubroutine(unittest.TestCase):

    def setUp(self):
        self.suite_resolution = _resolve()
        self.text = '\n'.join(_suite_part_list_subroutine(['test_simple'], [self.suite_resolution]))

    def test_signature(self):
        self.assertIn('subroutine ccpp_physics_suite_part_list(', self.text)
        self.assertIn(
            'character(len=*),              intent(in)    :: suite_name', self.text,
        )
        self.assertIn(
            'character(len=*), allocatable, intent(out)   :: part_list(:)', self.text,
        )
        self.assertIn('integer,                       intent(out)   :: errflg', self.text)

    def test_dispatch_and_groups(self):
        self.assertIn("case ('test_simple')", self.text)
        # test_simple has one group named 'physics'.
        group_names = [g.group_name for g in self.suite_resolution.groups]
        for i, gname in enumerate(group_names):
            self.assertIn("part_list({}) = '{}'".format(i + 1, gname), self.text)
        self.assertIn('allocate(part_list({}))'.format(len(group_names)), self.text)

    def test_default_error_case(self):
        self.assertIn('case default', self.text)
        self.assertIn('errflg = 1', self.text)
        self.assertIn(
            "errmsg = 'ccpp_physics_suite_part_list: unknown suite: '",
            self.text,
        )

    def test_initialises_outputs(self):
        # errmsg/errflg cleared at top of routine.
        self.assertIn("errmsg = ''", self.text)
        self.assertIn('errflg = 0', self.text)

    def test_errmsg_is_assumed_length(self):
        """The errmsg dummy must be ``character(len=*)`` so the host can
        pass any character length without copy-in/copy-out (e.g. the
        host might use ``character(len=256)`` while the framework's
        scheme metadata uses ``len=512``)."""
        self.assertIn('character(len=*),              intent(out)   :: errmsg',
                      self.text)
        self.assertNotIn('character(len=512)', self.text)
        self.assertNotIn('character(len=256)', self.text)


########################################################################
# Suite-introspection: ccpp_physics_suite_schemes
########################################################################

class TestSuiteSchemesSubroutine(unittest.TestCase):

    def setUp(self):
        self.suite_resolution = _resolve()
        self.text = '\n'.join(
            _suite_schemes_subroutine(['test_simple'], [self.suite_resolution])
        )

    def test_signature(self):
        self.assertIn('subroutine ccpp_physics_suite_schemes(', self.text)
        self.assertIn(
            'character(len=*), allocatable, intent(out)   :: scheme_list(:)',
            self.text,
        )

    def test_lists_temp_calc_adjust(self):
        # test_simple's only scheme (across all phases) is temp_calc_adjust.
        self.assertIn("scheme_list(1) = 'temp_calc_adjust'", self.text)
        self.assertIn('allocate(scheme_list(1))', self.text)

    def test_dedup_across_phases(self):
        # temp_calc_adjust appears in init, run, and final phases — must
        # appear exactly once in the emitted list.
        n = self.text.count("scheme_list(1) = 'temp_calc_adjust'")
        self.assertEqual(n, 1)
        self.assertNotIn("scheme_list(2) = 'temp_calc_adjust'", self.text)

    def test_default_error_case(self):
        self.assertIn(
            "errmsg = 'ccpp_physics_suite_schemes: unknown suite: '",
            self.text,
        )

    def test_errmsg_is_assumed_length(self):
        self.assertIn('character(len=*),              intent(out)   :: errmsg',
                      self.text)
        self.assertNotIn('character(len=512)', self.text)
        self.assertNotIn('character(len=256)', self.text)


########################################################################
# Suite-introspection: ccpp_physics_suite_variables
########################################################################

class TestSuiteVariablesSubroutine(unittest.TestCase):

    def setUp(self):
        self.suite_resolution = _resolve()
        self.hd = _load_full_host_dict()
        self.text = '\n'.join(_suite_io_subroutine(
            ['test_simple'], [self.suite_resolution], self.hd, collapse_ddts=False,
        ))

    def test_subroutine_name(self):
        self.assertIn('subroutine ccpp_physics_suite_variables(', self.text)
        self.assertIn('end subroutine ccpp_physics_suite_variables', self.text)

    def test_signature_includes_optional_filters(self):
        self.assertIn(
            'logical, optional,             intent(in)    :: input_vars',
            self.text,
        )
        self.assertIn(
            'logical, optional,             intent(in)    :: output_vars',
            self.text,
        )

    def test_no_struct_elements_arg(self):
        # struct_elements is intentionally dropped (was a no-op in capgen).
        self.assertNotIn('struct_elements', self.text)

    def test_errmsg_is_assumed_length(self):
        """``suite_variables`` (and ``suite_host_data`` — they share the
        same emitter) must declare errmsg as ``character(len=*)`` so the
        host can pass any character length."""
        self.assertIn('character(len=*),              intent(out)   :: errmsg',
                      self.text)
        self.assertNotIn('character(len=512)', self.text)
        self.assertNotIn('character(len=256)', self.text)

    def test_three_branch_dispatch(self):
        self.assertIn('if (input_vars_use .and. output_vars_use) then', self.text)
        self.assertIn('else if (input_vars_use) then', self.text)
        self.assertIn('else if (output_vars_use) then', self.text)
        # Empty fall-through branch.
        self.assertIn('allocate(variable_list(0))', self.text)

    def test_includes_control_vars(self):
        # ccpp_error_code and ccpp_error_message DO appear in the emitted
        # variable list literals — they're scheme args (intent=out) and
        # are part of the host-facing surface (matches original capgen).
        self.assertIn("'ccpp_error_code'",    self.text)
        self.assertIn("'ccpp_error_message'", self.text)

    def test_includes_host_data_vars(self):
        # air_temperature is in the host metadata and is intent=inout in run.
        self.assertIn("'air_temperature'", self.text)

    def test_default_present_check(self):
        self.assertIn('if (present(input_vars)) then', self.text)
        self.assertIn('if (present(output_vars)) then', self.text)
        self.assertIn('input_vars_use = .true.', self.text)
        self.assertIn('output_vars_use = .true.', self.text)

    def test_default_error_case(self):
        self.assertIn(
            "errmsg = 'ccpp_physics_suite_variables: unknown suite: '",
            self.text,
        )


########################################################################
# Suite-introspection: ccpp_physics_suite_host_data
########################################################################

class TestSuiteHostDataSubroutine(unittest.TestCase):
    """Same shape as _variables; differs only in DDT collapsing."""

    def setUp(self):
        self.suite_resolution = _resolve()
        self.hd = _load_full_host_dict()
        self.text = '\n'.join(_suite_io_subroutine(
            ['test_simple'], [self.suite_resolution], self.hd, collapse_ddts=True,
        ))

    def test_subroutine_name(self):
        self.assertIn('subroutine ccpp_physics_suite_host_data(', self.text)
        self.assertIn('end subroutine ccpp_physics_suite_host_data', self.text)

    def test_default_error_case(self):
        self.assertIn(
            "errmsg = 'ccpp_physics_suite_host_data: unknown suite: '",
            self.text,
        )

    def test_no_ddts_matches_variables(self):
        # host_full has no DDTs → the routine bodies (modulo the subroutine
        # name and error message) should contain the same variable
        # literals as ..._variables.
        var_text = '\n'.join(_suite_io_subroutine(
            ['test_simple'], [self.suite_resolution], self.hd, collapse_ddts=False,
        ))
        # A spot-check: any host-data variable in one is in the other.
        self.assertIn("'air_temperature'", self.text)
        self.assertIn("'air_temperature'", var_text)


########################################################################
# Suite-introspection: full module wiring
########################################################################

class TestIntrospectionRoutinesInModule(unittest.TestCase):
    """Verify all five introspection routines are emitted and made public."""

    def setUp(self):
        self.text = '\n'.join(_generate())

    def test_all_routines_present(self):
        for sub in (
            'ccpp_physics_suite_list',
            'ccpp_physics_suite_part_list',
            'ccpp_physics_suite_schemes',
            'ccpp_physics_suite_variables',
            'ccpp_physics_suite_host_data',
        ):
            self.assertIn('subroutine {}'.format(sub), self.text)
            self.assertIn('end subroutine {}'.format(sub), self.text)
            self.assertIn('public :: {}'.format(sub), self.text)


########################################################################
# Suite-introspection: --no-host-introspection stub bodies
########################################################################

class TestNoHostIntrospectionStubBodies(unittest.TestCase):
    """When ``--no-host-introspection`` is set, each of the five
    introspection routines retains its signature but the body is
    replaced with an errflg=1 stub (or, for suite_list, an error_unit
    write + empty allocation).  Tests assert the stub shape per routine
    and that signatures remain stable so existing host callers still
    link."""

    _DISABLED_MSG = (
        'suite introspection disabled at code-generation time; '
        'regenerate caps without --no-host-introspection'
    )

    @classmethod
    def setUpClass(cls):
        cls.suite_resolution = _resolve()
        cls.hd = _load_full_host_dict()

    def test_suite_list_writes_error_unit_and_allocates_empty(self):
        text = '\n'.join(_suite_list_subroutine(
            ['a', 'b', 'c'], stub_body=True,
        ))
        # Signature preserved.
        self.assertIn('subroutine ccpp_physics_suite_list(suites)', text)
        # Stub body: error_unit message, empty allocation.
        self.assertIn('write(error_unit,', text)
        self.assertIn('ccpp_physics_suite_list:', text)
        self.assertIn(self._DISABLED_MSG, text)
        self.assertIn('allocate(suites(0))', text)
        # The functional body must NOT appear.
        self.assertNotIn("suites(1) = 'a'", text)
        self.assertNotIn('allocate(suites(3))', text)

    def test_suite_part_list_stub(self):
        text = '\n'.join(_suite_part_list_subroutine(
            ['test_simple'], [self.suite_resolution], stub_body=True,
        ))
        self.assertIn('subroutine ccpp_physics_suite_part_list(', text)
        self.assertIn('errflg = 1', text)
        self.assertIn('ccpp_physics_suite_part_list: ' + self._DISABLED_MSG,
                      text)
        self.assertIn('allocate(part_list(0))', text)
        # Functional dispatch must not appear.
        self.assertNotIn("case ('test_simple')", text)
        self.assertNotIn('select case', text)

    def test_suite_schemes_stub(self):
        text = '\n'.join(_suite_schemes_subroutine(
            ['test_simple'], [self.suite_resolution], stub_body=True,
        ))
        self.assertIn('subroutine ccpp_physics_suite_schemes(', text)
        self.assertIn('errflg = 1', text)
        self.assertIn('ccpp_physics_suite_schemes: ' + self._DISABLED_MSG,
                      text)
        self.assertIn('allocate(scheme_list(0))', text)
        self.assertNotIn("scheme_list(1) =", text)

    def test_suite_variables_stub(self):
        text = '\n'.join(_suite_io_subroutine(
            ['test_simple'], [self.suite_resolution], self.hd,
            collapse_ddts=False, stub_body=True,
        ))
        self.assertIn('subroutine ccpp_physics_suite_variables(', text)
        self.assertIn('errflg = 1', text)
        self.assertIn('ccpp_physics_suite_variables: ' + self._DISABLED_MSG,
                      text)
        self.assertIn('allocate(variable_list(0))', text)
        # The huge case-block must NOT appear.
        self.assertNotIn('select case (trim(suite_name))', text)
        self.assertNotIn("case ('test_simple')", text)
        # Optional dummies are still declared so existing callers still
        # type-check.
        self.assertIn('logical, optional,             intent(in)    :: input_vars',
                      text)
        self.assertIn('logical, optional,             intent(in)    :: output_vars',
                      text)

    def test_suite_host_data_stub(self):
        text = '\n'.join(_suite_io_subroutine(
            ['test_simple'], [self.suite_resolution], self.hd,
            collapse_ddts=True, stub_body=True,
        ))
        self.assertIn('subroutine ccpp_physics_suite_host_data(', text)
        self.assertIn('errflg = 1', text)
        self.assertIn('ccpp_physics_suite_host_data: ' + self._DISABLED_MSG,
                      text)
        self.assertIn('allocate(variable_list(0))', text)
        self.assertNotIn('select case (trim(suite_name))', text)

    def test_module_imports_error_unit_unconditionally(self):
        # error_unit is always imported because every cap subroutine
        # emits a gated ``if (trace) write(error_unit, *) ...`` line.
        # Stub-on and stub-off both include the same USE.
        for stub in (True, False):
            text = '\n'.join(_generate_host_cap(
                'test_host',
                ['test_simple'], [self.suite_resolution], self.hd,
                no_host_introspection=stub,
            ))
            self.assertIn(
                'use, intrinsic :: iso_fortran_env, only: error_unit',
                text,
                msg='no_host_introspection={}'.format(stub),
            )

    def test_public_declarations_unchanged_when_stubbed(self):
        # All five introspection routines remain public — callers must
        # still link against them.
        text = '\n'.join(_generate_host_cap(
            'test_host',
            ['test_simple'], [self.suite_resolution], self.hd,
            no_host_introspection=True,
        ))
        for sub in (
            'ccpp_physics_suite_list',
            'ccpp_physics_suite_part_list',
            'ccpp_physics_suite_schemes',
            'ccpp_physics_suite_variables',
            'ccpp_physics_suite_host_data',
        ):
            self.assertIn('public :: {}'.format(sub), text)
            self.assertIn('subroutine {}'.format(sub), text)
            self.assertIn('end subroutine {}'.format(sub), text)

    def test_line_count_drops_dramatically(self):
        """The motivating case: 33k+ lines → ~800. We don't have 80
        suites in unit-test fixtures, but even with one suite the
        stubbed module must be strictly shorter than the full one."""
        full  = _generate_host_cap('test_host', ['test_simple'], [self.suite_resolution],
                                     self.hd, no_host_introspection=False)
        stub  = _generate_host_cap('test_host', ['test_simple'], [self.suite_resolution],
                                     self.hd, no_host_introspection=True)
        self.assertLess(len(stub), len(full),
                        'stubbed module should be shorter than the full one '
                        '(full={}, stub={})'.format(len(full), len(stub)))

    def test_write_host_cap_passes_flag_through(self):
        """``write_host_cap(no_host_introspection=True, ...)`` must
        produce a file containing stub bodies, not full ones."""
        with tempfile.TemporaryDirectory() as tmpdir:
            out_path = write_host_cap(
                'test_host',
                ['test_simple'], [self.suite_resolution], tmpdir, self.hd,
                no_host_introspection=True,
            )
            with open(out_path) as fh:
                text = fh.read()
        self.assertIn('ccpp_physics_suite_variables: ' + self._DISABLED_MSG,
                      text)
        self.assertIn(
            'use, intrinsic :: iso_fortran_env, only: error_unit', text,
        )


class TestTraceEmission(unittest.TestCase):
    """The generated static API always carries a module-level ``trace``
    parameter (default .false.) and a gated ``write(error_unit,*)`` in
    every cap subroutine that has at least one intent(in) control dummy.
    ``--trace`` flips the parameter default to .true.
    """

    def setUp(self):
        self.hd = _load_full_host_dict()
        self.suite_resolution = _resolve()

    def test_module_gate_default_off(self):
        text = '\n'.join(_generate_host_cap(
            'test_host',
            ['test_simple'], [self.suite_resolution], self.hd,
        ))
        self.assertIn('logical, parameter :: trace = .false.', text)
        self.assertNotIn('logical, parameter :: trace = .true.', text)

    def test_module_gate_default_on(self):
        text = '\n'.join(_generate_host_cap(
            'test_host',
            ['test_simple'], [self.suite_resolution], self.hd,
            trace=True,
        ))
        self.assertIn('logical, parameter :: trace = .true.', text)
        self.assertNotIn('logical, parameter :: trace = .false.', text)

    def test_trace_block_present_in_physics_phases(self):
        text = '\n'.join(_generate_host_cap(
            'test_host',
            ['test_simple'], [self.suite_resolution], self.hd,
        ))
        # Every ccpp_physics_<phase> dispatch has a gated write.
        for phase in ('init', 'timestep_init', 'run',
                      'timestep_final', 'final'):
            self.assertIn(
                "'CCPP TRACE ccpp_physics_{}:'".format(phase),
                text,
                msg='trace string missing for phase {}'.format(phase),
            )

    def test_trace_block_present_in_lifecycle_routines(self):
        text = '\n'.join(_generate_host_cap(
            'test_host',
            ['test_simple'], [self.suite_resolution], self.hd,
        ))
        for sub in ('ccpp_register', 'ccpp_init', 'ccpp_final'):
            self.assertIn(
                "'CCPP TRACE {}:'".format(sub), text,
                msg='trace string missing for {}'.format(sub),
            )

    def test_write_host_cap_threads_trace_flag(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            out_path = write_host_cap(
                'test_host',
                ['test_simple'], [self.suite_resolution], tmpdir, self.hd,
                trace=True,
            )
            with open(out_path) as fh:
                text = fh.read()
        self.assertIn('logical, parameter :: trace = .true.', text)


def load_tests(loader, tests, ignore):
    import generator.host_cap as sa
    tests.addTests(doctest.DocTestSuite(sa))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
