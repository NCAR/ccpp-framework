"""Unit tests for metadata.variable_resolver.

Covers:
- Helper functions (_ddt_typename, _is_intrinsic, _is_external, _is_known_ddt,
  _instance_subscript, _build_ddt_index)
- HostVarEntry construction and properties
- _flatten_ddt_instance (single-level and nested)
- build_flat_host_dict (plain vars, DDT expansion, control vars, duplicates)
- SchemeStore (build_from, has_scheme, phases_for, variables_for, duplicates)
"""

import os
import sys
import unittest
import doctest

# Path setup is handled by conftest.py (pytest) or run_tests.py; the imports
# below are enough for direct invocation via this file's __main__ block.
from metadata.metadata_table import _parse_lines, MetadataTable, MetaVar
from metadata.parse_tools import ParseContext, CCPPError
from metadata.variable_resolver import (
    _ddt_typename,
    _is_intrinsic,
    _is_external,
    _is_known_ddt,
    _instance_subscript,
    _build_ddt_index,
    _flatten_ddt_instance,
    build_ddt_module_map,
    HostVarEntry,
    build_flat_host_dict,
    SchemeStore,
)

# ---------------------------------------------------------------------------
# Sample file directory
# ---------------------------------------------------------------------------
_TESTS_DIR   = os.path.dirname(os.path.abspath(__file__))
_SAMPLES_DIR = os.path.join(_TESTS_DIR, 'sample_files')


def _sample(name: str) -> str:
    return os.path.join(_SAMPLES_DIR, name)


def _parse_file(name: str):
    """Parse a sample metadata file and return its tables."""
    from metadata.metadata_table import parse_metadata_file
    return parse_metadata_file(_sample(name))


def _ctx(lineno: int = 0, filename: str = 'test.meta') -> ParseContext:
    return ParseContext(linenum=lineno, filename=filename)


def _make_simple_var(local_name: str, std_name: str, units: str = '1',
                     dims: str = '()', type_: str = 'integer',
                     kind: str = '') -> MetaVar:
    """Quick helper to build a validated MetaVar."""
    ctx = _ctx()
    v = MetaVar(local_name, ctx)
    v.set_attr('standard_name', std_name, ctx)
    v.set_attr('units', units, ctx)
    v.set_attr('dimensions', dims, ctx)
    v.set_attr('type', type_, ctx)
    if kind:
        v.set_attr('kind', kind, ctx)
    return v


def _make_ddt_instance_var(local_name: str, std_name: str, type_name: str,
                            dims: str = '()') -> MetaVar:
    """Build a DDT instance MetaVar with the given dimensions."""
    ctx = _ctx()
    v = MetaVar(local_name, ctx)
    v.set_attr('standard_name', std_name, ctx)
    v.set_attr('units', 'none', ctx)
    v.set_attr('dimensions', dims, ctx)
    v.set_attr('type', type_name, ctx)
    return v


########################################################################
# Tests: helper functions
########################################################################

class TestDdtTypename(unittest.TestCase):

    def test_plain_name(self):
        self.assertEqual(_ddt_typename('gfs_statein_type'), 'gfs_statein_type')

    def test_type_paren(self):
        self.assertEqual(_ddt_typename('type(gfs_statein_type)'), 'gfs_statein_type')

    def test_type_paren_uppercase(self):
        self.assertEqual(_ddt_typename('TYPE(MY_DDT)'), 'MY_DDT')

    def test_type_paren_whitespace(self):
        self.assertEqual(_ddt_typename('type( my_type )'), 'my_type')

    def test_intrinsic_passthrough(self):
        self.assertEqual(_ddt_typename('real'), 'real')

    def test_external_passthrough(self):
        self.assertEqual(_ddt_typename('external:mpi_f08:mpi_comm'),
                         'external:mpi_f08:mpi_comm')


class TestIsIntrinsic(unittest.TestCase):

    def test_real(self):
        self.assertTrue(_is_intrinsic('real'))

    def test_integer(self):
        self.assertTrue(_is_intrinsic('integer'))

    def test_character(self):
        self.assertTrue(_is_intrinsic('character'))

    def test_logical(self):
        self.assertTrue(_is_intrinsic('logical'))

    def test_complex(self):
        self.assertTrue(_is_intrinsic('complex'))

    def test_ddt_name(self):
        self.assertFalse(_is_intrinsic('gfs_statein_type'))

    def test_external_syntax(self):
        self.assertFalse(_is_intrinsic('external:mpi_f08:mpi_comm'))


class TestIsExternal(unittest.TestCase):

    def test_external_lowercase(self):
        self.assertTrue(_is_external('external:mpi_f08:mpi_comm'))

    def test_external_uppercase(self):
        self.assertTrue(_is_external('EXTERNAL:mpi_f08:mpi_comm'))

    def test_real(self):
        self.assertFalse(_is_external('real'))

    def test_ddt_name(self):
        self.assertFalse(_is_external('my_ddt_type'))


class TestIsKnownDdt(unittest.TestCase):

    def _idx(self, name: str) -> dict:
        ctx = _ctx()
        tbl = MetadataTable(name, 'ddt', 'f.meta', ctx)
        return {name: tbl}

    def test_known_ddt(self):
        self.assertTrue(_is_known_ddt('my_type', self._idx('my_type')))

    def test_type_paren_form(self):
        self.assertTrue(_is_known_ddt('type(my_type)', self._idx('my_type')))

    def test_unknown_name(self):
        self.assertFalse(_is_known_ddt('other_type', self._idx('my_type')))

    def test_intrinsic_not_ddt(self):
        self.assertFalse(_is_known_ddt('real', self._idx('my_type')))

    def test_external_not_ddt(self):
        self.assertFalse(_is_known_ddt('external:mpi_f08:mpi_comm', self._idx('my_type')))

    def test_empty_index(self):
        self.assertFalse(_is_known_ddt('my_type', {}))


class TestInstanceSubscript(unittest.TestCase):

    def test_number_of_instances(self):
        v = _make_ddt_instance_var('gs', 'gst', 'gs_type', '(number_of_instances)')
        self.assertEqual(_instance_subscript(v), '(instance_number)')

    def test_number_of_threads(self):
        """Registered scalar-index dim 'number_of_threads' pairs with
        the host's 'thread_number' control variable.  Regression for
        the per-thread DDT-container pattern (e.g. SCM's
        physics%Interstitial(thread_number))."""
        v = _make_ddt_instance_var('inst', 'inst_std', 'gs_type',
                                   '(number_of_threads)')
        self.assertEqual(_instance_subscript(v), '(thread_number)')

    def test_multiple_registered_dims(self):
        """A DDT instance with two registered scalar-index dims emits
        both index placeholders in declared order."""
        v = _make_ddt_instance_var('foo', 'foo_std', 'foo_type',
                                   '(number_of_instances, number_of_threads)')
        self.assertEqual(
            _instance_subscript(v),
            '(instance_number, thread_number)',
        )

    def test_scalar_no_subscript(self):
        v = _make_ddt_instance_var('gs', 'gst', 'gs_type', '()')
        self.assertEqual(_instance_subscript(v), '')

    def test_horizontal_dim_no_subscript(self):
        v = _make_simple_var('x', 'air_temperature', 'K',
                             '(horizontal_dimension, vertical_layer_dimension)',
                             'real', 'kind_phys')
        self.assertEqual(_instance_subscript(v), '')


########################################################################
# Tests: HostVarEntry
########################################################################

class TestHostVarEntry(unittest.TestCase):

    def _make(self, stdname='air_temperature', local='temp',
              path='temp', module='mymod'):
        return HostVarEntry(
            stdname, local, path, module,
            'real', 'kind_phys', 'K',
            ['horizontal_dimension', 'vertical_layer_dimension'],
            False, False, ''
        )

    def test_basic_attributes(self):
        e = self._make()
        self.assertEqual(e.standard_name, 'air_temperature')
        self.assertEqual(e.local_name, 'temp')
        self.assertEqual(e.access_path, 'temp')
        self.assertEqual(e.module_name, 'mymod')
        self.assertEqual(e.type, 'real')
        self.assertEqual(e.kind, 'kind_phys')
        self.assertEqual(e.units, 'K')
        self.assertEqual(e.dimensions,
                         ['horizontal_dimension', 'vertical_layer_dimension'])
        self.assertFalse(e.protected)
        self.assertFalse(e.optional)
        self.assertEqual(e.active, '')

    def test_is_control_false(self):
        e = self._make()
        self.assertFalse(e.is_control)

    def test_is_control_true(self):
        e = HostVarEntry('thread_number', 'thread_num', 'thread_num', None,
                         'integer', '', '1', [], False, False, '')
        self.assertTrue(e.is_control)

    def test_repr(self):
        e = self._make()
        self.assertIn('air_temperature', repr(e))
        self.assertIn('temp', repr(e))

    def test_dimensions_are_copied(self):
        dims = ['horizontal_dimension']
        e = HostVarEntry('x', 'x', 'x', 'mod', 'integer', '', '1',
                         dims, False, False, '')
        dims.append('extra')
        self.assertEqual(len(e.dimensions), 1)

    def test_equality_by_standard_name(self):
        e1 = self._make('air_temperature')
        e2 = self._make('air_temperature')
        e3 = self._make('pressure')
        self.assertEqual(e1, e2)
        self.assertNotEqual(e1, e3)

    def test_hash(self):
        e1 = self._make('air_temperature')
        e2 = self._make('air_temperature')
        self.assertEqual(hash(e1), hash(e2))


########################################################################
# Tests: _build_ddt_index
########################################################################

class TestBuildDdtIndex(unittest.TestCase):

    def test_single_table(self):
        ctx = _ctx()
        tbl = MetadataTable('gfs_statein_type', 'ddt', 'f.meta', ctx)
        idx = _build_ddt_index([tbl])
        self.assertIn('gfs_statein_type', idx)
        self.assertIs(idx['gfs_statein_type'], tbl)

    def test_multiple_tables(self):
        ctx = _ctx()
        t1 = MetadataTable('type_a', 'ddt', 'a.meta', ctx)
        t2 = MetadataTable('type_b', 'ddt', 'b.meta', ctx)
        idx = _build_ddt_index([t1, t2])
        self.assertEqual(set(idx.keys()), {'type_a', 'type_b'})

    def test_empty(self):
        self.assertEqual(_build_ddt_index([]), {})

    def test_from_file(self):
        tables = _parse_file('ddt_simple.meta')
        idx = _build_ddt_index(tables)
        self.assertIn('gfs_statein_type', idx)


########################################################################
# Tests: build_ddt_module_map
########################################################################

class TestBuildDdtModuleMap(unittest.TestCase):

    def test_ddt_co_located_with_scheme(self):
        ctx = _ctx()
        ddt_tbl = MetadataTable('vmr_type', 'ddt', 'make_ddt.meta', ctx)
        sch_tbl = MetadataTable('make_ddt', 'scheme', 'make_ddt.meta', ctx)
        result = build_ddt_module_map([ddt_tbl, sch_tbl])
        self.assertEqual(result, {'vmr_type': 'make_ddt'})

    def test_ddt_co_located_with_host(self):
        ctx = _ctx()
        ddt_tbl = MetadataTable('payload_t', 'ddt', 'host.meta', ctx)
        host_tbl = MetadataTable('my_host', 'host', 'host.meta', ctx)
        result = build_ddt_module_map([ddt_tbl, host_tbl])
        self.assertEqual(result, {'payload_t': 'my_host'})

    def test_ddt_alone_in_file_skipped(self):
        ctx = _ctx()
        orphan = MetadataTable('lonely_t', 'ddt', 'lonely.meta', ctx)
        self.assertEqual(build_ddt_module_map([orphan]), {})

    def test_multiple_files(self):
        ctx = _ctx()
        d1 = MetadataTable('t1', 'ddt', 'a.meta', ctx)
        s1 = MetadataTable('mod_a', 'scheme', 'a.meta', ctx)
        d2 = MetadataTable('t2', 'ddt', 'b.meta', ctx)
        h2 = MetadataTable('mod_b', 'host', 'b.meta', ctx)
        result = build_ddt_module_map([d1, s1, d2, h2])
        self.assertEqual(result, {'t1': 'mod_a', 't2': 'mod_b'})

    def test_multiple_ddts_in_one_file(self):
        ctx = _ctx()
        d1 = MetadataTable('inner', 'ddt', 'm.meta', ctx)
        d2 = MetadataTable('outer', 'ddt', 'm.meta', ctx)
        s = MetadataTable('host_mod', 'host', 'm.meta', ctx)
        result = build_ddt_module_map([d1, d2, s])
        self.assertEqual(result, {'inner': 'host_mod', 'outer': 'host_mod'})

    def test_empty(self):
        self.assertEqual(build_ddt_module_map([]), {})

    # ---- Precedence cases for the DDT module map ------------------------
    # Truth table (see build_ddt_module_map docstring):
    #
    #   DDT.module_name   | co-located resolved | Result
    #   ------------------|---------------------|--------
    #   X (set)           | Y (any)             | X     (DDT wins)
    #   unset             | Y (any)             | Y
    #   X (set)           | (no co-located tbl) | X
    #   unset             | (no co-located tbl) | (skipped)
    #
    # The "co-located resolved" column is itself the same
    # ``module_name or table_name`` rule used by build_flat_host_dict.

    def test_explicit_module_name_used_when_no_colocated_table(self):
        """Real-world fixture: CCPP-physics ``radsw_param.meta`` declares
        ``cmpfsw_type`` in a file containing only DDT tables, where the
        Fortran module name differs from any table name and is supplied
        via ``module_name = …`` in ``[ccpp-table-properties]``.  Without
        this resolution path the suite_types emitter can't find the
        module and raises CCPPError on pointer-wrapper generation."""
        ctx = _ctx()
        tbl = MetadataTable('cmpfsw_type', 'ddt', 'radsw_param.meta', ctx)
        tbl.module_name = 'module_radsw_parameters'
        self.assertEqual(
            build_ddt_module_map([tbl]),
            {'cmpfsw_type': 'module_radsw_parameters'},
        )

    def test_ddt_module_name_overrides_colocated_table_name(self):
        """``module_name`` on a DDT table beats the implicit co-located
        scheme/host name — the DDT may genuinely live in a different
        Fortran module than the scheme its .meta is paired with."""
        ctx = _ctx()
        ddt = MetadataTable('cmpfsw_type', 'ddt', 'rad.meta', ctx)
        ddt.module_name = 'module_radsw_parameters'
        sch = MetadataTable('radsw_main', 'scheme', 'rad.meta', ctx)
        result = build_ddt_module_map([ddt, sch])
        self.assertEqual(result['cmpfsw_type'], 'module_radsw_parameters')

    def test_ddt_module_name_wins_over_colocated_module_name(self):
        """Both the DDT and a co-located scheme declare module_name; the
        DDT's takes precedence (most-specific-wins).  Documents the
        truth-table row "X / Y / X"."""
        ctx = _ctx()
        ddt = MetadataTable('cmpfsw_type', 'ddt', 'rad.meta', ctx)
        ddt.module_name = 'module_radsw_parameters'
        sch = MetadataTable('radsw_main', 'scheme', 'rad.meta', ctx)
        sch.module_name = 'mod_radsw_main'
        result = build_ddt_module_map([ddt, sch])
        self.assertEqual(result['cmpfsw_type'], 'module_radsw_parameters')

    def test_colocated_module_name_used_when_ddt_has_none(self):
        """When the DDT has no ``module_name`` but the co-located
        scheme/host carries one, the co-located ``module_name``
        (NOT its table name) wins.  Documents "unset / Y / Y" where
        Y comes from the co-located override.  This is the bug we
        fixed when refactoring build_ddt_module_map — the old code
        used the co-located table_name and silently ignored its
        own module_name override."""
        ctx = _ctx()
        ddt = MetadataTable('inner_t', 'ddt', 'a.meta', ctx)
        # No DDT override.
        sch = MetadataTable('scheme_a', 'scheme', 'a.meta', ctx)
        sch.module_name = 'mod_a'   # Fortran module differs from table name
        result = build_ddt_module_map([ddt, sch])
        self.assertEqual(result['inner_t'], 'mod_a')

    def test_colocated_table_name_used_when_neither_has_override(self):
        """When neither carries module_name, the implicit "module = table
        name" convention applies to the co-located scheme/host."""
        ctx = _ctx()
        ddt = MetadataTable('inner_t', 'ddt', 'a.meta', ctx)
        sch = MetadataTable('scheme_a', 'scheme', 'a.meta', ctx)
        result = build_ddt_module_map([ddt, sch])
        self.assertEqual(result['inner_t'], 'scheme_a')


########################################################################
# Tests: _flatten_ddt_instance
########################################################################

class TestFlattenDdtInstance(unittest.TestCase):

    def _load_ddt_gfs(self):
        return _build_ddt_index(_parse_file('ddt_simple.meta'))

    def test_scalar_instance_paths(self):
        """Scalar DDT instance (no dimensions) → field paths with % separator."""
        idx = self._load_ddt_gfs()
        var = _make_ddt_instance_var('gfs_statein', 'gfs_statein', 'gfs_statein_type')
        entries = _flatten_ddt_instance(var, 'CCPP_data', idx)
        std_names = {e.standard_name for e in entries}
        self.assertIn('gfs_statein', std_names)            # instance itself
        self.assertIn('geopotential_at_interface', std_names)
        self.assertIn('geopotential', std_names)

    def test_scalar_instance_access_paths(self):
        idx = self._load_ddt_gfs()
        var = _make_ddt_instance_var('gfs_statein', 'gfs_statein', 'gfs_statein_type')
        entries = {e.standard_name: e
                   for e in _flatten_ddt_instance(var, 'CCPP_data', idx)}
        self.assertEqual(entries['geopotential_at_interface'].access_path,
                         'gfs_statein%phii')
        self.assertEqual(entries['geopotential'].access_path,
                         'gfs_statein%phil')

    def test_arrayed_instance_subscript(self):
        """DDT instance with instance dimension → access path has (instance_number)."""
        idx = self._load_ddt_gfs()
        var = _make_ddt_instance_var('gfs_statein', 'gfs_statein',
                                      'gfs_statein_type', '(number_of_instances)')
        entries = {e.standard_name: e
                   for e in _flatten_ddt_instance(var, 'CCPP_data', idx)}
        self.assertEqual(entries['geopotential_at_interface'].access_path,
                         'gfs_statein(instance_number)%phii')

    def test_module_name_propagated(self):
        idx = self._load_ddt_gfs()
        var = _make_ddt_instance_var('gfs_statein', 'gfs_statein', 'gfs_statein_type')
        for entry in _flatten_ddt_instance(var, 'GFS_typedefs', idx):
            self.assertEqual(entry.module_name, 'GFS_typedefs')

    def test_unknown_ddt_type_raises(self):
        idx = {}  # empty — type not found
        var = _make_ddt_instance_var('gs', 'gs', 'unknown_type')
        with self.assertRaises(CCPPError) as cm:
            _flatten_ddt_instance(var, 'mod', idx)
        self.assertIn('unknown_type', str(cm.exception))

    def test_field_type_and_kind(self):
        idx = self._load_ddt_gfs()
        var = _make_ddt_instance_var('gfs_statein', 'gfs_statein', 'gfs_statein_type')
        entries = {e.standard_name: e
                   for e in _flatten_ddt_instance(var, 'CCPP_data', idx)}
        phii = entries['geopotential_at_interface']
        self.assertEqual(phii.type, 'real')
        self.assertEqual(phii.kind, 'kind_phys')
        self.assertEqual(phii.units, 'm2 s-2')

    def test_prefix_propagated(self):
        """access_prefix is prepended to all paths (used in nested DDT recursion)."""
        idx = self._load_ddt_gfs()
        var = _make_ddt_instance_var('inner', 'inner', 'gfs_statein_type')
        entries = {e.standard_name: e
                   for e in _flatten_ddt_instance(
                       var, 'mod', idx, access_prefix='outer%')}
        self.assertEqual(entries['geopotential_at_interface'].access_path,
                         'outer%inner%phii')

    def test_max_depth_guard(self):
        """A deeply nested structure beyond max_depth raises CCPPError."""
        idx = self._load_ddt_gfs()
        var = _make_ddt_instance_var('x', 'x', 'gfs_statein_type')
        with self.assertRaises(CCPPError) as cm:
            _flatten_ddt_instance(var, 'mod', idx, depth=10, max_depth=8)
        self.assertIn('depth', str(cm.exception))


class TestFlattenNestedDdt(unittest.TestCase):
    """End-to-end nested DDT flattening using sample files."""

    def _load_nested(self):
        outer_tables = _parse_file('ddt_nested_outer.meta')
        inner_tables = _parse_file('ddt_nested_inner.meta')
        return _build_ddt_index(outer_tables + inner_tables)

    def test_nested_field_standard_names(self):
        idx = self._load_nested()
        host_tables = _parse_file('host_with_nested_ddt.meta')
        var = host_tables[0].sections()[0].variables[0]
        entries = {e.standard_name: e
                   for e in _flatten_ddt_instance(var, 'nested_host_mod', idx)}
        expected = {
            'outer_ddt_instance',   # the outer DDT instance itself
            'outer_scalar_field',   # plain field on the outer DDT
            'inner_ddt_instance',   # the inner DDT instance (as a field)
            'inner_real_value',     # field of the inner DDT
            'inner_integer_flag',   # field of the inner DDT
        }
        self.assertEqual(set(entries.keys()), expected)

    def test_nested_access_paths(self):
        idx = self._load_nested()
        host_tables = _parse_file('host_with_nested_ddt.meta')
        var = host_tables[0].sections()[0].variables[0]
        entries = {e.standard_name: e
                   for e in _flatten_ddt_instance(var, 'nested_host_mod', idx)}
        self.assertEqual(entries['outer_scalar_field'].access_path,
                         'outer_inst%scalar_field')
        self.assertEqual(entries['inner_real_value'].access_path,
                         'outer_inst%inner_ddt%inner_value')
        self.assertEqual(entries['inner_integer_flag'].access_path,
                         'outer_inst%inner_ddt%inner_flag')

    def test_nested_module_name(self):
        idx = self._load_nested()
        host_tables = _parse_file('host_with_nested_ddt.meta')
        var = host_tables[0].sections()[0].variables[0]
        for entry in _flatten_ddt_instance(var, 'nested_host_mod', idx):
            self.assertEqual(entry.module_name, 'nested_host_mod')


########################################################################
# Tests: build_flat_host_dict
########################################################################

class TestBuildFlatHostDict(unittest.TestCase):

    def test_plain_host_vars(self):
        host_tables = _parse_file('host_simple.meta')
        d = build_flat_host_dict(host_tables, [], [])
        self.assertIn('horizontal_dimension', d)
        self.assertIn('vertical_layer_dimension', d)
        # number_of_instances is now a control-table var (paired with
        # instance_number), so it is NOT in a host-only dictionary.
        self.assertNotIn('number_of_instances', d)
        # loop bounds and error vars live in the control table, not the host table
        self.assertNotIn('horizontal_loop_begin', d)
        self.assertNotIn('horizontal_loop_end', d)
        self.assertEqual(len(d), 2)

    def test_plain_host_access_paths(self):
        host_tables = _parse_file('host_simple.meta')
        d = build_flat_host_dict(host_tables, [], [])
        # For plain vars the access path equals the local name.
        self.assertEqual(d['horizontal_dimension'].access_path, 'ncols')
        self.assertEqual(d['vertical_layer_dimension'].access_path, 'nlev')

    def test_plain_host_module_names(self):
        host_tables = _parse_file('host_simple.meta')
        d = build_flat_host_dict(host_tables, [], [])
        for entry in d.values():
            self.assertEqual(entry.module_name, 'physics_data')
            self.assertFalse(entry.is_control)

    def test_host_module_name_override(self):
        """A ``type=host`` table that declares ``module_name`` in its
        ``[ccpp-table-properties]`` should override the default convention
        (module name = table name) so subsequent USE statements target the
        actual Fortran module."""
        src = '''
[ccpp-table-properties]
  name = host_data
  type = host
  module_name = mod_host_data

[ccpp-arg-table]
  name = host_data
  type = host
[ ncols ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
'''
        tables = _parse_lines(src.splitlines(keepends=True), 'h.meta')
        d = build_flat_host_dict(tables, [], [])
        self.assertEqual(d['horizontal_dimension'].module_name, 'mod_host_data')

    def test_host_module_name_defaults_to_table_name(self):
        """Without ``module_name`` the module defaults to the table name."""
        src = '''
[ccpp-table-properties]
  name = host_data
  type = host

[ccpp-arg-table]
  name = host_data
  type = host
[ ncols ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
'''
        tables = _parse_lines(src.splitlines(keepends=True), 'h.meta')
        d = build_flat_host_dict(tables, [], [])
        self.assertEqual(d['horizontal_dimension'].module_name, 'host_data')

    def test_control_vars_no_module(self):
        ctrl_tables = _parse_file('control_simple.meta')
        d = build_flat_host_dict([], ctrl_tables, [])
        self.assertIn('suite_name', d)
        self.assertIn('group_name', d)
        self.assertIn('thread_number', d)
        self.assertIn('number_of_physics_threads', d)
        for entry in d.values():
            self.assertIsNone(entry.module_name)
            self.assertTrue(entry.is_control)

    def test_ddt_instance_expansion(self):
        host_tables = _parse_file('host_with_ddt_instance.meta')
        ddt_tables  = _parse_file('ddt_simple.meta')
        d = build_flat_host_dict(host_tables, [], ddt_tables)
        # DDT instance itself + two fields
        self.assertIn('gfs_statein', d)
        self.assertIn('geopotential_at_interface', d)
        self.assertIn('geopotential', d)

    def test_ddt_instance_subscript_in_path(self):
        """DDT instance with (number_of_instances) → (instance_number) in path."""
        host_tables = _parse_file('host_with_ddt_instance.meta')
        ddt_tables  = _parse_file('ddt_simple.meta')
        d = build_flat_host_dict(host_tables, [], ddt_tables)
        self.assertEqual(d['geopotential_at_interface'].access_path,
                         'gfs_statein(instance_number)%phii')
        self.assertEqual(d['geopotential'].access_path,
                         'gfs_statein(instance_number)%phil')

    def test_ddt_instance_module_name(self):
        host_tables = _parse_file('host_with_ddt_instance.meta')
        ddt_tables  = _parse_file('ddt_simple.meta')
        d = build_flat_host_dict(host_tables, [], ddt_tables)
        self.assertEqual(d['geopotential_at_interface'].module_name, 'CCPP_data')

    def test_host_and_control_combined(self):
        host_tables = _parse_file('host_simple.meta')
        ctrl_tables = _parse_file('control_simple.meta')
        d = build_flat_host_dict(host_tables, ctrl_tables, [])
        self.assertIn('horizontal_dimension', d)
        self.assertIn('suite_name', d)
        self.assertFalse(d['horizontal_dimension'].is_control)
        self.assertTrue(d['suite_name'].is_control)

    def test_duplicate_standard_name_raises(self):
        """Same standard name in two host tables must raise CCPPError.
        The message must include both access paths so the user can see
        which two declarations collide."""
        src = '''
[ccpp-table-properties]
  name = mod_a
  type = host
[ccpp-arg-table]
  name = mod_a
  type = host
[ im ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
'''
        tables_a = _parse_lines(src.splitlines(keepends=True), 'a.meta')
        tables_b = _parse_lines(src.replace('mod_a', 'mod_b')
                                   .splitlines(keepends=True), 'b.meta')
        with self.assertRaises(CCPPError) as cm:
            build_flat_host_dict(tables_a + tables_b, [], [])
        msg = str(cm.exception)
        self.assertIn('horizontal_dimension', msg)
        # Both module names must appear so the user can locate the duplicates.
        self.assertIn('mod_a', msg)
        self.assertIn('mod_b', msg)
        # And both access paths.
        self.assertIn('access path', msg)

    def test_duplicate_ddt_component_names_path_collision(self):
        """Two sibling DDT instances of the same type inside one parent
        DDT cause component standard names to collide.  The error must
        show both colliding access paths so the user can spot the issue
        immediately (this is the scm_type_defs / GFS_interstitial_type
        sliced-view-vs-array pattern)."""
        # parent_ddt has two sibling fields of the same inner_ddt type:
        # one bare and one with a slice in the local name.  Both
        # flatten into entries for ``foo_std`` (inner_ddt's only
        # component) — the second insertion triggers the duplicate.
        src = '''
[ccpp-table-properties]
  name = inner_ddt
  type = ddt
[ccpp-arg-table]
  name = inner_ddt
  type = ddt
[ foo ]
  standard_name = foo_std
  units = 1
  dimensions = ()
  type = integer

[ccpp-table-properties]
  name = parent_ddt
  type = ddt
[ccpp-arg-table]
  name = parent_ddt
  type = ddt
[ inner_a ]
  standard_name = inner_ddt_instance_a
  units = ddt
  dimensions = ()
  type = inner_ddt
[ inner_b ]
  standard_name = inner_ddt_instance_b
  units = ddt
  dimensions = ()
  type = inner_ddt

[ccpp-table-properties]
  name = my_host
  type = host
[ccpp-arg-table]
  name = my_host
  type = host
[ parent ]
  standard_name = parent_ddt_instance
  units = ddt
  dimensions = ()
  type = parent_ddt
'''
        tables = _parse_lines(src.splitlines(keepends=True), 'm.meta')
        ddt_tables  = [t for t in tables if t.table_type == 'ddt']
        host_tables = [t for t in tables if t.table_type == 'host']
        with self.assertRaises(CCPPError) as cm:
            build_flat_host_dict(host_tables, [], ddt_tables)
        msg = str(cm.exception)
        # The duplicated standard name appears.
        self.assertIn('foo_std', msg)
        # Both colliding access paths appear so the user can diagnose.
        self.assertIn('parent%inner_a%foo', msg)
        self.assertIn('parent%inner_b%foo', msg)
        # Hint about sibling-DDT-instance pattern is present.
        self.assertIn('sibling DDT instances', msg)

    def test_missing_ddt_table_raises(self):
        """DDT instance without corresponding DDT table → CCPPError."""
        host_tables = _parse_file('host_with_ddt_instance.meta')
        with self.assertRaises(CCPPError) as cm:
            build_flat_host_dict(host_tables, [], [])
        self.assertIn('gfs_statein_type', str(cm.exception))

    def test_host_character_assumed_length_raises(self):
        """A host character variable with kind=len=* is rejected: host
        metadata must give a concrete length (len=* is only valid for a
        scheme dummy argument)."""
        src = '''
[ccpp-table-properties]
  name = host_data
  type = host
[ccpp-arg-table]
  name = host_data
  type = host
[ scheme_name ]
  standard_name = scheme_name
  units = none
  dimensions = ()
  type = character
  kind = len=*
'''
        tables = _parse_lines(src.splitlines(keepends=True), 'h.meta')
        with self.assertRaises(CCPPError) as cm:
            build_flat_host_dict(tables, [], [])
        msg = str(cm.exception)
        self.assertIn('scheme_name', msg)
        self.assertIn('len=*', msg)

    def test_control_character_assumed_length_ok(self):
        """Control-table character variables are EXEMPT: they are pass-through
        dummy arguments (suite_name, errmsg, ...) that the generated caps
        declare ``character(len=*)``, so len=* is valid there."""
        src = '''
[ccpp-table-properties]
  name = ccpp_control
  type = control
[ccpp-arg-table]
  name = ccpp_control
  type = control
[ label ]
  standard_name = some_label
  units = none
  dimensions = ()
  type = character
  kind = len=*
'''
        tables = _parse_lines(src.splitlines(keepends=True), 'c.meta')
        d = build_flat_host_dict([], tables, [])
        self.assertEqual(d['some_label'].kind, 'len=*')

    def test_host_character_concrete_length_ok(self):
        """A concrete host character length is accepted unchanged."""
        src = '''
[ccpp-table-properties]
  name = host_data
  type = host
[ccpp-arg-table]
  name = host_data
  type = host
[ scheme_name ]
  standard_name = scheme_name
  units = none
  dimensions = ()
  type = character
  kind = len=512
'''
        tables = _parse_lines(src.splitlines(keepends=True), 'h.meta')
        d = build_flat_host_dict(tables, [], [])
        self.assertEqual(d['scheme_name'].kind, 'len=512')

    def test_host_vars_not_control(self):
        # Host vars are is_control=False; loop bounds are now control vars (control table).
        host_tables = _parse_file('host_simple.meta')
        d = build_flat_host_dict(host_tables, [], [])
        self.assertFalse(d['horizontal_dimension'].is_control)
        self.assertFalse(d['vertical_layer_dimension'].is_control)
        # Control vars from the control table should be is_control=True.
        ctrl_tables = _parse_file('control_simple.meta')
        dc = build_flat_host_dict([], ctrl_tables, [])
        self.assertTrue(dc['horizontal_loop_begin'].is_control)
        self.assertTrue(dc['horizontal_loop_end'].is_control)

    def test_nested_ddt_full_expansion(self):
        outer_tables = _parse_file('ddt_nested_outer.meta')
        inner_tables = _parse_file('ddt_nested_inner.meta')
        host_tables  = _parse_file('host_with_nested_ddt.meta')
        d = build_flat_host_dict(host_tables, [], outer_tables + inner_tables)
        self.assertIn('outer_ddt_instance', d)
        self.assertIn('outer_scalar_field', d)
        self.assertIn('inner_real_value', d)
        self.assertIn('inner_integer_flag', d)
        self.assertEqual(d['inner_real_value'].access_path,
                         'outer_inst%inner_ddt%inner_value')

    def test_empty_inputs(self):
        d = build_flat_host_dict([], [], [])
        self.assertEqual(d, {})


########################################################################
# Tests: SchemeStore
########################################################################

_SIMPLE_SCHEME_SRC = '''\
[ccpp-table-properties]
  name = my_scheme
  type = scheme

[ccpp-arg-table]
  name = my_scheme_run
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

[ccpp-arg-table]
  name = my_scheme_init
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
'''


class TestRule2LeafScalarDimRejection(unittest.TestCase):
    """Rule 2 of the registered-scalar-index-dimension contract (see
    capgen/metadata/registered_dimensions.py): leaf data variables —
    intrinsic-typed or external-typed, the kind a scheme binds to —
    MUST NOT declare a registered scalar-index dim like
    ``number_of_threads``.  ``build_flat_host_dict`` is the validation
    site; the error must name the variable, the offending dim, the
    paired index, and point at the registered_dimensions module.
    """

    _HOST_SRC = '''
[ccpp-table-properties]
  name = host_data
  type = host

[ccpp-arg-table]
  name = host_data
  type = host
[ leaf_field ]
  standard_name = bad_leaf
  long_name = leaf with a registered scalar-index dim — illegal
  units = K
  dimensions = (number_of_threads, horizontal_dimension)
  type = real
  kind = kind_phys
'''

    def test_leaf_with_registered_dim_rejected(self):
        host_tbls = _parse_lines(self._HOST_SRC.splitlines(keepends=True),
                                 'host_bad.meta')
        with self.assertRaises(CCPPError) as ctx:
            build_flat_host_dict(host_tbls, [], [])
        msg = str(ctx.exception)
        # Names the offending variable.
        self.assertIn("'leaf_field'", msg)
        # Names the offending dim.
        self.assertIn("'number_of_threads'", msg)
        # Names the paired index.
        self.assertIn("'thread_number'", msg)
        # Points at the source module for further reading.
        self.assertIn('registered_dimensions.py', msg)
        # Tells the user how to fix it.
        self.assertIn('container DDT', msg)

    def test_leaf_with_instances_dim_rejected(self):
        src = self._HOST_SRC.replace(
            'number_of_threads, horizontal_dimension',
            'number_of_instances, horizontal_dimension',
        )
        host_tbls = _parse_lines(src.splitlines(keepends=True),
                                 'host_bad.meta')
        with self.assertRaises(CCPPError) as ctx:
            build_flat_host_dict(host_tbls, [], [])
        msg = str(ctx.exception)
        self.assertIn("'number_of_instances'", msg)
        self.assertIn("'instance_number'", msg)

    def test_ddt_instance_with_non_registered_dim_skips_field_flatten(self):
        """A DDT-instance variable dimensioned by a non-registered dim
        (e.g. ``horizontal_dimension`` on a per-column DDT array like
        ``fluxLW(horizontal_dimension)`` of type ``ty_rad_lw``) is a
        legitimate pattern: schemes take the whole sliced DDT array as
        a single arg, not individual flattened inner fields.

        capgen must NOT flatten the inner fields in this case —
        attempting to bake a scalar subscript would emit invalid
        Fortran like ``parent%var%field(...)``.  Instead, only the
        DDT-instance's own entry is recorded; schemes that take it
        whole resolve via that entry, and schemes that ask for inner
        fields by std_name trip the existing "not found" error.

        Regression for the nested_suite + var_compat end-to-end fixtures
        which use exactly this pattern.
        """
        ddt_src = '''
[ccpp-table-properties]
  name = ty_rad_lw
  type = ddt
[ccpp-arg-table]
  name = ty_rad_lw
  type = ddt
[ sfc_up_lw ]
  standard_name = surface_upwelling_longwave_radiation_flux
  units = W m-2
  dimensions = ()
  type = real
  kind = kind_phys
'''
        host_src = '''
[ccpp-table-properties]
  name = phys_state
  type = host
[ccpp-arg-table]
  name = phys_state
  type = host
[ fluxLW ]
  standard_name = longwave_radiation_fluxes
  units = W m-2
  dimensions = (horizontal_dimension)
  type = ty_rad_lw
'''
        ddt_tbls  = _parse_lines(ddt_src.splitlines(keepends=True),
                                 'module_rad_ddt.meta')
        host_tbls = _parse_lines(host_src.splitlines(keepends=True),
                                 'phys_state.meta')
        # No exception — the DDT-instance entry alone is enough for
        # schemes that take the whole sliced DDT as an arg.
        d = build_flat_host_dict(host_tbls, [], ddt_tbls)
        self.assertIn('longwave_radiation_fluxes', d)
        # Inner field is NOT flattened (would have required a scalar
        # subscript capgen can't synthesize).
        self.assertNotIn('surface_upwelling_longwave_radiation_flux', d)

    def test_ddt_instance_with_non_registered_dim_no_fields_accepted(self):
        """An empty DDT (no fields) dimensioned by a non-registered dim
        should NOT trigger the flatten-time error — there's nothing to
        flatten, so no broken access pattern is possible.  Real-world
        case: ``ccpp_constituent_prop_ptr_t(:)`` field on a host's
        constituent object.  This DDT is accessed via the dedicated
        constituent resolver, not via field-flattening."""
        ddt_src = '''
[ccpp-table-properties]
  name = empty_ddt
  type = ddt
[ccpp-arg-table]
  name = empty_ddt
  type = ddt
'''
        host_src = '''
[ccpp-table-properties]
  name = my_host
  type = host
[ccpp-arg-table]
  name = my_host
  type = host
[ payload_arr ]
  standard_name = some_payload_array
  units = DDT
  dimensions = (number_of_ccpp_constituents)
  type = empty_ddt
'''
        ddt_tbls  = _parse_lines(ddt_src.splitlines(keepends=True),
                                 'empty_ddt.meta')
        host_tbls = _parse_lines(host_src.splitlines(keepends=True),
                                 'my_host.meta')
        # Should not raise.
        result = build_flat_host_dict(host_tbls, [], ddt_tbls)
        self.assertIn('some_payload_array', result)

    def test_container_ddt_with_registered_dim_accepted(self):
        """The same dim on a DDT-instance container variable is fine —
        Rule 2 only applies to leaves."""
        src = '''
[ccpp-table-properties]
  name = my_ddt
  type = ddt

[ccpp-arg-table]
  name = my_ddt
  type = ddt
[ field ]
  standard_name = inner_field
  units = K
  dimensions = (horizontal_dimension)
  type = real
  kind = kind_phys
'''
        host_src = '''
[ccpp-table-properties]
  name = host_data
  type = host

[ccpp-arg-table]
  name = host_data
  type = host
[ inst_array ]
  standard_name = instance_array
  units = DDT
  dimensions = (number_of_threads)
  type = my_ddt
'''
        ddt_tbls  = _parse_lines(src.splitlines(keepends=True), 'ddt.meta')
        host_tbls = _parse_lines(host_src.splitlines(keepends=True),
                                 'host.meta')
        # Should not raise — the dim is on a container.
        result = build_flat_host_dict(host_tbls, [], ddt_tbls)
        self.assertIn('inner_field', result)
        # The flattened field's access path carries the (thread_number)
        # placeholder.
        self.assertEqual(
            result['inner_field'].access_path,
            'inst_array(thread_number)%field',
        )


class TestSchemeStore(unittest.TestCase):

    def _build(self, src=_SIMPLE_SCHEME_SRC):
        tables = _parse_lines(src.splitlines(keepends=True), 's.meta')
        return SchemeStore.build_from(tables)

    def test_build_from_single_scheme(self):
        store = self._build()
        self.assertTrue(store.has_scheme('my_scheme'))

    def test_has_scheme_false(self):
        store = self._build()
        self.assertFalse(store.has_scheme('nonexistent'))

    def test_phases_for(self):
        store = self._build()
        self.assertEqual(sorted(store.phases_for('my_scheme')), ['init', 'run'])

    def test_phases_for_unknown(self):
        store = self._build()
        self.assertEqual(store.phases_for('nonexistent'), [])

    def test_variables_for_run(self):
        store = self._build()
        vars_ = store.variables_for('my_scheme', 'run')
        self.assertIsNotNone(vars_)
        std_names = [v.standard_name for v in vars_]
        self.assertEqual(std_names, ['horizontal_dimension', 'air_temperature'])

    def test_variables_for_init(self):
        store = self._build()
        vars_ = store.variables_for('my_scheme', 'init')
        self.assertIsNotNone(vars_)
        std_names = [v.standard_name for v in vars_]
        self.assertIn('ccpp_error_message', std_names)
        self.assertIn('ccpp_error_code', std_names)

    def test_variables_for_absent_phase(self):
        store = self._build()
        self.assertIsNone(store.variables_for('my_scheme', 'final'))

    def test_variables_for_unknown_scheme(self):
        store = self._build()
        self.assertIsNone(store.variables_for('unknown', 'run'))

    def test_module_for_defaults_to_scheme_name(self):
        store = self._build()
        self.assertEqual(store.module_for('my_scheme'), 'my_scheme')

    def test_module_for_honors_table_property(self):
        """``module_name`` in ``[ccpp-table-properties]`` overrides the
        default (scheme-name) module."""
        src = '''
[ccpp-table-properties]
  name = effr_pre
  type = scheme
  module_name = mod_effr_pre

[ccpp-arg-table]
  name = effr_pre_run
  type = scheme
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out
'''
        tables = _parse_lines(src.splitlines(keepends=True), 's.meta')
        store = SchemeStore.build_from(tables)
        self.assertEqual(store.module_for('effr_pre'), 'mod_effr_pre')

    def test_module_for_unknown_scheme_returns_name(self):
        store = self._build()
        self.assertEqual(store.module_for('unknown_scheme'), 'unknown_scheme')

    def test_scheme_names_sorted(self):
        src2 = _SIMPLE_SCHEME_SRC + '''
[ccpp-table-properties]
  name = another_scheme
  type = scheme

[ccpp-arg-table]
  name = another_scheme_run
  type = scheme
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out
'''
        tables = _parse_lines(src2.splitlines(keepends=True), 's.meta')
        store = SchemeStore.build_from(tables)
        self.assertEqual(store.scheme_names(), ['another_scheme', 'my_scheme'])

    def test_non_scheme_tables_skipped(self):
        host_src = '''\
[ccpp-table-properties]
  name = host_mod
  type = host

[ccpp-arg-table]
  name = host_mod
  type = host
[ im ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
'''
        tables = (_parse_lines(_SIMPLE_SCHEME_SRC.splitlines(keepends=True), 's.meta')
                  + _parse_lines(host_src.splitlines(keepends=True), 'h.meta'))
        store = SchemeStore.build_from(tables)
        self.assertFalse(store.has_scheme('host_mod'))
        self.assertTrue(store.has_scheme('my_scheme'))

    def test_duplicate_phase_raises(self):
        src_dup = _SIMPLE_SCHEME_SRC + '''\
[ccpp-arg-table]
  name = my_scheme_run
  type = scheme
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out
'''
        tables = _parse_lines(src_dup.splitlines(keepends=True), 's.meta')
        with self.assertRaises(CCPPError) as cm:
            SchemeStore.build_from(tables)
        self.assertIn('run', str(cm.exception))
        self.assertIn('my_scheme', str(cm.exception))

    def test_duplicate_phase_names_both_source_files(self):
        """When two distinct ``.meta`` files declare the same
        (scheme, phase) pair, the error must name both file paths so
        the user can locate the conflict instead of grepping the
        ``--scheme-files`` list."""
        tables_a = _parse_lines(
            _SIMPLE_SCHEME_SRC.splitlines(keepends=True),
            '/projA/my_scheme.meta',
        )
        tables_b = _parse_lines(
            _SIMPLE_SCHEME_SRC.splitlines(keepends=True),
            '/projB/my_scheme.meta',
        )
        with self.assertRaises(CCPPError) as cm:
            SchemeStore.build_from(tables_a + tables_b)
        msg = str(cm.exception)
        self.assertIn('my_scheme', msg)
        # Both paths appear, in order: original then duplicate.
        idx_a = msg.find('/projA/my_scheme.meta')
        idx_b = msg.find('/projB/my_scheme.meta')
        self.assertGreaterEqual(idx_a, 0, 'original path not in error: ' + msg)
        self.assertGreaterEqual(idx_b, 0, 'duplicate path not in error: ' + msg)
        self.assertLess(idx_a, idx_b,
                        'expected original (A) before duplicate (B)')
        # Different paths → no CMake-list hint.
        self.assertNotIn('--scheme-files', msg)

    def test_duplicate_phase_same_path_hints_at_cmake_list(self):
        """The motivating SCM case: a single ``.meta`` path listed
        twice in the host's ``--scheme-files`` argument (typically a
        stray CMake list entry).  Both reported paths are byte-equal,
        and the message appends an explicit ``--scheme-files`` hint so
        the user knows to look in the build glue, not in the
        metadata."""
        path = '/host/ccpp/physics/GWD/ugwpv1_gsldrag.meta'
        tables_first = _parse_lines(
            _SIMPLE_SCHEME_SRC.splitlines(keepends=True), path,
        )
        tables_again = _parse_lines(
            _SIMPLE_SCHEME_SRC.splitlines(keepends=True), path,
        )
        with self.assertRaises(CCPPError) as cm:
            SchemeStore.build_from(tables_first + tables_again)
        msg = str(cm.exception)
        # The single path appears (at least once; same string both
        # places, so a single substring search suffices).
        self.assertIn(path, msg)
        # CMake-list duplication hint fires when the paths are equal.
        self.assertIn('--scheme-files', msg)
        self.assertIn('identical', msg)

    def test_build_from_scheme_files(self):
        """Integration: build SchemeStore from the multipart scheme sample file."""
        from metadata.metadata_table import parse_metadata_file
        tables = parse_metadata_file(_sample('scheme_multipart.meta'))
        store = SchemeStore.build_from(tables)
        self.assertTrue(store.has_scheme('temp_calc_adjust'))
        self.assertEqual(sorted(store.phases_for('temp_calc_adjust')),
                         ['final', 'init', 'run'])

    def test_repr(self):
        store = self._build()
        self.assertIn('my_scheme', repr(store))

    def test_variable_list_is_copy(self):
        """Mutating the returned list must not affect the store's internal state."""
        store = self._build()
        vars1 = store.variables_for('my_scheme', 'run')
        vars1.append(None)
        vars2 = store.variables_for('my_scheme', 'run')
        self.assertEqual(len(vars2), 2)


########################################################################
# Doctest loader
########################################################################

def load_tests(loader, tests, ignore):
    """Auto-discover doctests from variable_resolver and related modules."""
    import metadata.variable_resolver as vr
    tests.addTests(doctest.DocTestSuite(vr))
    return tests


########################################################################
# main
########################################################################

if __name__ == '__main__':
    unittest.main(verbosity=2)
