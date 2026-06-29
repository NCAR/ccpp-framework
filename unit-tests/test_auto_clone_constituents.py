"""Tests for the transient auto-clone-constituents legacy shim.

This whole file is part of the auto-clone-constituents shim and should
be deleted alongside ``metadata/auto_clone_constituents.py`` when the
shim is retired.  Search ``auto-clone-constituents`` to find every
touchpoint.
"""

from __future__ import annotations

import doctest
import io
import logging
import os
import sys
import unittest

_TESTS_DIR  = os.path.dirname(os.path.abspath(__file__))
_CAPGEN_DIR = os.path.join(os.path.dirname(_TESTS_DIR), 'capgen')
if _CAPGEN_DIR not in sys.path:
    sys.path.insert(0, _CAPGEN_DIR)

from metadata import auto_clone_constituents                # noqa: E402
from metadata.metadata_table import (                       # noqa: E402
    MetaVar, parse_metadata_file, _parse_lines,
)
from metadata.parse_tools import CCPPError, ParseContext    # noqa: E402
from generator.suite_resolver import (                      # noqa: E402
    AutoCloneEntry,
    _make_auto_clone_entry,
    _collect_auto_clone_entries,
    _vertical_dim_of,
    _synthesised_long_name_from_std,
)
from generator.suite_cap import (                           # noqa: E402
    _emit_auto_clone_instantiate,
    _fmt_kind_phys_real,
    _esc_fortran_char,
)


_SAMPLES_DIR = os.path.join(_TESTS_DIR, 'sample_files')


def _sf(name):
    return os.path.join(_SAMPLES_DIR, name)


def _ctx():
    return ParseContext(linenum=1, filename='auto_clone_test.meta')


class _AutoCloneFixture(unittest.TestCase):
    """Mixin that flips the shim on for the duration of a test and
    guarantees it goes back off afterwards (the flag is process state).
    """

    def setUp(self):
        # Sanity: never start a test with the flag set by an earlier
        # test that crashed before cleanup.
        auto_clone_constituents.disable()

    def tearDown(self):
        auto_clone_constituents.disable()


########################################################################
# Module surface
########################################################################

class TestExtraKnownAttrs(_AutoCloneFixture):

    def test_empty_when_disabled(self):
        self.assertFalse(auto_clone_constituents.is_enabled())
        self.assertEqual(auto_clone_constituents.extra_known_attrs(),
                         frozenset())

    def test_populated_when_enabled(self):
        auto_clone_constituents.enable(_stream=io.StringIO())
        extra = auto_clone_constituents.extra_known_attrs()
        self.assertEqual(
            extra,
            frozenset({
                'default_value', 'min_value',
                'water_species', 'mixing_ratio_type',
            }),
        )


class TestEnableDisable(_AutoCloneFixture):

    def test_enable_flips_flag_and_writes_banner(self):
        sink = io.StringIO()
        auto_clone_constituents.enable(_stream=sink)
        self.assertTrue(auto_clone_constituents.is_enabled())
        out = sink.getvalue()
        self.assertIn('LEGACY AUTO-CLONE-CONSTITUENTS ENABLED', out)
        for attr in ('default_value', 'min_value',
                     'water_species', 'mixing_ratio_type'):
            self.assertIn(attr, out)
        self.assertIn('single-instance', out)
        self.assertIn('TRANSIENT', out)
        self.assertIn('REMOVED', out)
        self.assertGreaterEqual(out.count('*' * 10), 2)

    def test_enable_is_idempotent(self):
        sink1 = io.StringIO()
        auto_clone_constituents.enable(_stream=sink1)
        first = sink1.getvalue()
        sink2 = io.StringIO()
        auto_clone_constituents.enable(_stream=sink2)
        self.assertEqual(sink2.getvalue(), '')
        self.assertIn('AUTO-CLONE', first)

    def test_disable_resets(self):
        auto_clone_constituents.enable(_stream=io.StringIO())
        self.assertTrue(auto_clone_constituents.is_enabled())
        auto_clone_constituents.disable()
        self.assertFalse(auto_clone_constituents.is_enabled())

    def test_logger_receives_warning(self):
        logger = logging.getLogger('auto_clone_test_logger')
        records = []

        class _Capture(logging.Handler):
            def emit(self, record):
                records.append(record)

        handler = _Capture(level=logging.WARNING)
        logger.addHandler(handler)
        try:
            auto_clone_constituents.enable(
                logger=logger, _stream=io.StringIO())
            self.assertEqual(len(records), 1)
            self.assertEqual(records[0].levelno, logging.WARNING)
            msg = records[0].getMessage()
            for attr in ('default_value', 'min_value',
                         'water_species', 'mixing_ratio_type'):
                self.assertIn(attr, msg)
        finally:
            logger.removeHandler(handler)


class TestSingleInstanceGuard(_AutoCloneFixture):

    def test_noop_when_disabled(self):
        # Even a multi-instance host should not raise when the shim is off.
        host_dict = {'instance_number': object(),
                     'number_of_instances': object()}
        auto_clone_constituents.require_single_instance_host(host_dict)  # no raise

    def test_single_instance_passes(self):
        auto_clone_constituents.enable(_stream=io.StringIO())
        # No instance pair → fine.
        auto_clone_constituents.require_single_instance_host({})

    def test_instance_number_alone_raises(self):
        auto_clone_constituents.enable(_stream=io.StringIO())
        with self.assertRaises(CCPPError) as cm:
            auto_clone_constituents.require_single_instance_host(
                {'instance_number': object()}
            )
        self.assertIn('single-instance', str(cm.exception))
        self.assertIn('instance_number', str(cm.exception))

    def test_number_of_instances_alone_raises(self):
        auto_clone_constituents.enable(_stream=io.StringIO())
        with self.assertRaises(CCPPError) as cm:
            auto_clone_constituents.require_single_instance_host(
                {'number_of_instances': object()}
            )
        self.assertIn('number_of_instances', str(cm.exception))


########################################################################
# Parser: conditional _KNOWN_ATTRS extension
########################################################################

class TestParserShimOff(_AutoCloneFixture):
    """When the shim is OFF, the four legacy attrs are rejected with
    the standard "Unknown variable attribute" error."""

    def _make_var(self):
        return MetaVar('q', _ctx())

    def test_default_value_rejected(self):
        v = self._make_var()
        with self.assertRaises(CCPPError) as cm:
            v.set_attr('default_value', '0.0', _ctx())
        self.assertIn('Unknown variable attribute', str(cm.exception))
        self.assertIn('default_value', str(cm.exception))

    def test_min_value_rejected(self):
        v = self._make_var()
        with self.assertRaises(CCPPError):
            v.set_attr('min_value', '0.0', _ctx())

    def test_water_species_rejected(self):
        v = self._make_var()
        with self.assertRaises(CCPPError):
            v.set_attr('water_species', 'True', _ctx())

    def test_mixing_ratio_type_rejected(self):
        v = self._make_var()
        with self.assertRaises(CCPPError):
            v.set_attr('mixing_ratio_type', 'dry', _ctx())


class TestParserShimOn(_AutoCloneFixture):
    """When the shim is ON, the four legacy attrs parse and validate."""

    def setUp(self):
        super().setUp()
        auto_clone_constituents.enable(_stream=io.StringIO())

    def _make_var(self):
        return MetaVar('q', _ctx())

    def test_default_value_accepted(self):
        v = self._make_var()
        v.set_attr('default_value', '1.5e-12', _ctx())
        self.assertEqual(v.default_value, 1.5e-12)

    def test_default_value_kind_phys_suffix_accepted(self):
        v = self._make_var()
        v.set_attr('default_value', '0.0_kind_phys', _ctx())
        self.assertEqual(v.default_value, 0.0)

    def test_default_value_d_exponent_accepted(self):
        v = self._make_var()
        v.set_attr('default_value', '1.0d-5', _ctx())
        self.assertEqual(v.default_value, 1.0e-5)

    def test_default_value_invalid_raises(self):
        v = self._make_var()
        with self.assertRaises(CCPPError):
            v.set_attr('default_value', 'not_a_number', _ctx())

    def test_min_value_accepted(self):
        v = self._make_var()
        v.set_attr('min_value', '-3.14', _ctx())
        self.assertEqual(v.min_value, -3.14)

    def test_min_value_kind_suffix_accepted(self):
        v = self._make_var()
        v.set_attr('min_value', '1.0e-12_kind_dyn', _ctx())
        self.assertEqual(v.min_value, 1.0e-12)

    def test_water_species_true(self):
        v = self._make_var()
        v.set_attr('water_species', '.true.', _ctx())
        self.assertIs(v.water_species, True)

    def test_water_species_false(self):
        v = self._make_var()
        v.set_attr('water_species', 'False', _ctx())
        self.assertIs(v.water_species, False)

    def test_water_species_invalid_raises(self):
        v = self._make_var()
        with self.assertRaises(CCPPError):
            v.set_attr('water_species', 'maybe', _ctx())

    def test_mixing_ratio_type_dry(self):
        v = self._make_var()
        v.set_attr('mixing_ratio_type', 'dry', _ctx())
        self.assertEqual(v.mixing_ratio_type, 'dry')

    def test_mixing_ratio_type_lowercased(self):
        v = self._make_var()
        v.set_attr('mixing_ratio_type', 'WRT_MOIST', _ctx())
        self.assertEqual(v.mixing_ratio_type, 'wrt_moist')

    def test_mixing_ratio_type_invalid_raises(self):
        v = self._make_var()
        with self.assertRaises(CCPPError):
            v.set_attr('mixing_ratio_type', 'bogus', _ctx())


class TestParserSchemeOnly(_AutoCloneFixture):
    """The four legacy attrs are scheme-only when the shim is on; the
    parser must reject them on host/control/ddt tables."""

    def setUp(self):
        super().setUp()
        auto_clone_constituents.enable(_stream=io.StringIO())

    def test_default_value_rejected_on_host_table(self):
        src = (
            '[ccpp-table-properties]\n'
            '  name = h\n'
            '  type = host\n'
            '[ccpp-arg-table]\n'
            '  name = h\n'
            '  type = host\n'
            '[ q ]\n'
            '  standard_name = some_field\n'
            '  units = kg kg-1\n'
            '  dimensions = ()\n'
            '  type = real | kind = kind_phys\n'
            '  default_value = 0.0\n'
        )
        with self.assertRaises(CCPPError) as cm:
            _parse_lines(src.splitlines(keepends=True), 't.meta')
        self.assertIn('scheme-only', str(cm.exception))


########################################################################
# Suite-cap helpers
########################################################################

class TestKindPhysFormatter(unittest.TestCase):

    def test_positive(self):
        self.assertTrue(_fmt_kind_phys_real(0.0).endswith('_kind_phys'))
        self.assertIn('e', _fmt_kind_phys_real(1.0e-12))

    def test_negative(self):
        s = _fmt_kind_phys_real(-3.14)
        self.assertTrue(s.endswith('_kind_phys'))
        self.assertTrue(s.startswith('-'))


class TestFortranCharEscape(unittest.TestCase):

    def test_no_quote_passthrough(self):
        self.assertEqual(_esc_fortran_char("CLDLIQ"), "CLDLIQ")

    def test_single_quote_doubled(self):
        self.assertEqual(_esc_fortran_char("don't"), "don''t")


class TestEmitAutoCloneInstantiate(unittest.TestCase):
    """Render one synthesised %instantiate call and check the lines."""

    def _entry(self, **kw):
        defaults = dict(
            std_name='cloud_liquid_dry_mixing_ratio',
            long_name='cloud liquid water',
            diag_name='CLDLIQ',
            units='kg kg-1',
            vertical_dim='vertical_layer_dimension',
            advected=False,
            molar_mass=0.0,
            default_value=None,
            min_value=None,
            water_species=None,
            mixing_ratio_type=None,
        )
        defaults.update(kw)
        return AutoCloneEntry(**defaults)

    def _emit(self, entry):
        lines = []
        _emit_auto_clone_instantiate(
            entry, buf='suite_dynamic_constituents',
            inst_idx='1', indent='    ',
            errflg_local='errflg', errmsg_local='errmsg',
            lines=lines,
        )
        return lines

    def test_required_kwargs_always_emitted(self):
        lines = self._emit(self._entry())
        joined = '\n'.join(lines)
        self.assertIn("std_name     = 'cloud_liquid_dry_mixing_ratio'", joined)
        self.assertIn("long_name    = 'cloud liquid water'", joined)
        self.assertIn("diag_name    = 'CLDLIQ'", joined)
        self.assertIn("units        = 'kg kg-1'", joined)
        self.assertIn("vertical_dim = 'vertical_layer_dimension'", joined)
        self.assertIn("errcode      = errflg", joined)
        self.assertIn("errmsg       = errmsg", joined)
        # Error guard.
        self.assertIn('if (errflg /= 0) return', joined)
        # Counter increment.
        self.assertEqual(lines[0].strip(), 'num_consts = num_consts + 1')

    def test_optional_kwargs_omitted_when_unset(self):
        lines = self._emit(self._entry())
        joined = '\n'.join(lines)
        for kw in ('advected', 'default_value', 'min_value',
                   'water_species', 'mixing_ratio_type', 'molar_mass'):
            self.assertNotIn(kw + '   ', joined,
                             "unset optional '{}' leaked into output".format(kw))

    def test_advected_only_emitted_when_true(self):
        lines = self._emit(self._entry(advected=True))
        joined = '\n'.join(lines)
        self.assertIn('advected     = .true.', joined)

    def test_real_kwargs_emit_kind_phys_literal(self):
        lines = self._emit(self._entry(
            default_value=0.0, min_value=1.0e-12, molar_mass=18.015,
        ))
        joined = '\n'.join(lines)
        self.assertIn('default_value=', joined)
        self.assertIn('min_value    =', joined)
        self.assertIn('molar_mass   =', joined)
        # Each real kwarg uses the kind_phys suffix.
        self.assertEqual(joined.count('_kind_phys'), 3)

    def test_water_species_true_and_false(self):
        on  = self._emit(self._entry(water_species=True))
        off = self._emit(self._entry(water_species=False))
        self.assertIn('water_species= .true.',  '\n'.join(on))
        self.assertIn('water_species= .false.', '\n'.join(off))

    def test_mixing_ratio_type_quoted(self):
        lines = self._emit(self._entry(mixing_ratio_type='wrt_moist'))
        self.assertIn("mixing_ratio_type = 'wrt_moist'", '\n'.join(lines))

    def test_quoted_long_name_escaped(self):
        # Fortran character literal escape: every single quote is
        # doubled.
        lines = self._emit(self._entry(long_name="don't"))
        self.assertIn("long_name    = 'don''t'", '\n'.join(lines))


########################################################################
# Resolver helpers
########################################################################

class TestVerticalDimOf(unittest.TestCase):

    class _StubVar:
        def __init__(self, dims):
            self.dimensions = dims

    def test_extracts_layer_dim(self):
        v = self._StubVar(['horizontal_dimension', 'vertical_layer_dimension'])
        self.assertEqual(_vertical_dim_of(v), 'vertical_layer_dimension')

    def test_extracts_interface_dim(self):
        v = self._StubVar(
            ['horizontal_dimension', 'vertical_interface_dimension'])
        self.assertEqual(_vertical_dim_of(v), 'vertical_interface_dimension')

    def test_no_vertical_dim_returns_default(self):
        v = self._StubVar(['horizontal_dimension'])
        self.assertEqual(_vertical_dim_of(v), 'vertical_layer_dimension')

    def test_explicit_range_form_supported(self):
        v = self._StubVar([
            'ccpp_constant_one:horizontal_dimension',
            'ccpp_constant_one:vertical_layer_dimension',
        ])
        self.assertEqual(_vertical_dim_of(v), 'vertical_layer_dimension')


class TestMakeAutoCloneEntry(_AutoCloneFixture):
    """``_make_auto_clone_entry`` snapshots a scheme MetaVar."""

    def setUp(self):
        super().setUp()
        auto_clone_constituents.enable(_stream=io.StringIO())

    def _scheme_var(self, **set_attrs):
        ctx = _ctx()
        v = MetaVar('qv', ctx)
        v.set_attr('standard_name', 'water_vapor_specific_humidity', ctx)
        v.set_attr('long_name', 'water vapor', ctx)
        v.set_attr('units', 'kg kg-1', ctx)
        v.set_attr('dimensions',
                   '(horizontal_dimension, vertical_layer_dimension)', ctx)
        v.set_attr('type', 'real', ctx)
        v.set_attr('kind', 'kind_phys', ctx)
        v.set_attr('intent', 'inout', ctx)
        for k, val in set_attrs.items():
            v.set_attr(k, val, ctx)
        return v

    def test_diag_name_defaults_to_local_name(self):
        v = self._scheme_var()
        entry = _make_auto_clone_entry(v)
        self.assertEqual(entry.diag_name, 'qv')   # MetaVar.diagnostic_name fallback

    def test_explicit_diagnostic_name_wins(self):
        v = self._scheme_var(diagnostic_name='QV')
        entry = _make_auto_clone_entry(v)
        self.assertEqual(entry.diag_name, 'QV')

    def test_optional_kwargs_passthrough(self):
        v = self._scheme_var(
            advected='.true.',
            molar_mass='18.015',
            default_value='1.0e-12',
            min_value='0.0',
            water_species='.true.',
            mixing_ratio_type='wrt_moist',
        )
        entry = _make_auto_clone_entry(v)
        self.assertTrue(entry.advected)
        self.assertAlmostEqual(entry.molar_mass, 18.015)
        self.assertEqual(entry.default_value, 1.0e-12)
        self.assertEqual(entry.min_value, 0.0)
        self.assertIs(entry.water_species, True)
        self.assertEqual(entry.mixing_ratio_type, 'wrt_moist')

    def test_vertical_dim_lifted_from_dimensions(self):
        v = self._scheme_var()
        entry = _make_auto_clone_entry(v)
        self.assertEqual(entry.vertical_dim, 'vertical_layer_dimension')

    def test_explicit_long_name_wins(self):
        # When the metadata supplies a long_name, the entry carries it
        # verbatim (no synthesis).  The shared _scheme_var helper
        # already sets long_name='water vapor', so we re-use that
        # fixture as the "explicit long_name set" case.
        v = self._scheme_var()
        entry = _make_auto_clone_entry(v)
        self.assertEqual(entry.long_name, 'water vapor')

    def test_long_name_synthesised_when_missing(self):
        # The _scheme_var helper sets long_name='water vapor'; override
        # to a no-op so the helper sees an empty long_name.  capgen
        # then synthesises from std_name: 'water_vapor_specific_humidity'
        # → 'Water vapor specific humidity'.
        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        v = MetaVar('qv', ctx)
        v.set_attr('standard_name',
                   'water_vapor_specific_humidity', ctx)
        v.set_attr('units', 'kg kg-1', ctx)
        v.set_attr('dimensions',
                   '(horizontal_dimension, vertical_layer_dimension)', ctx)
        v.set_attr('type', 'real', ctx)
        v.set_attr('kind', 'kind_phys', ctx)
        v.set_attr('intent', 'inout', ctx)
        # No long_name attribute set.
        entry = _make_auto_clone_entry(v)
        self.assertEqual(entry.long_name, 'Water vapor specific humidity')


class TestSynthesisedLongName(unittest.TestCase):
    """The ``_synthesised_long_name_from_std`` helper mirrors original
    capgen's behaviour: replace each underscore with a space and
    capitalise the first character."""

    def test_typical_constituent(self):
        self.assertEqual(
            _synthesised_long_name_from_std('cloud_liquid_dry_mixing_ratio'),
            'Cloud liquid dry mixing ratio',
        )

    def test_single_underscore(self):
        self.assertEqual(
            _synthesised_long_name_from_std('specific_humidity'),
            'Specific humidity',
        )

    def test_no_underscore(self):
        self.assertEqual(
            _synthesised_long_name_from_std('temperature'),
            'Temperature',
        )

    def test_mixed_case_lowercased_after_first(self):
        # Python's ``.capitalize()`` lowercases everything after the
        # first character.  That's a deliberate match for original
        # capgen's behaviour and means a std_name with embedded
        # uppercase (atypical) gets normalised.
        self.assertEqual(
            _synthesised_long_name_from_std('Foo_Bar'),
            'Foo bar',
        )


class TestCollectAutoCloneEntriesSkips(_AutoCloneFixture):
    """``_collect_auto_clone_entries`` must skip framework-named std
    names (``ccpp_constituents``, ``ccpp_constituent_tendencies``,
    ``ccpp_constituent_properties``, ``number_of_ccpp_constituents``,
    ``index_of_*``) — those resolve through ``source='constituent'``
    too but reference the framework-provided buffers, not individual
    species, so synthesising a ``%instantiate`` for them would
    register bogus duplicate constituents (e.g. an entry named
    ``ccpp_constituents`` in the dynamic-constituents buffer).
    """

    def _resolved_arg(self, std_name):
        from generator.suite_resolver import ResolvedArg
        return ResolvedArg(
            standard_name=std_name,
            scheme_local_name='dummy',
            intent='inout',
            is_optional=False,
            active='', active_local='',
            source='constituent',
            host_entry=None, suite_var=None,
            base_expr='', subscript='', call_expr='',
            used_dim_std_names=set(),
            needs_unit_transform=False,
            needs_kind_transform=False,
            unit_forward='', unit_backward='',
            kind_scheme='', kind_host='',
            temp_name='', ptr_name='',
            transform_case=1,
            scheme_dimensions=[],
        )

    def _resolved_groups_with(self, std_names):
        from generator.suite_resolver import (
            ResolvedCall, ResolvedGroup,
        )
        call = ResolvedCall(
            scheme_name='dummy_scheme', phase='run',
            args=[self._resolved_arg(s) for s in std_names],
        )
        group = ResolvedGroup(group_name='g')
        group.phase_calls['run'] = [call]
        return [group]

    def test_framework_array_names_skipped(self):
        auto_clone_constituents.enable(_stream=io.StringIO())
        groups = self._resolved_groups_with([
            'ccpp_constituents',
            'ccpp_constituent_tendencies',
            'ccpp_constituent_properties',
            'number_of_ccpp_constituents',
        ])
        # No matching scheme metadata exists, but the framework-name
        # filter must fire BEFORE the scheme-var lookup so this never
        # gets that far.  Returns [] without raising.
        entries = _collect_auto_clone_entries(groups, scheme_store=None)
        self.assertEqual(entries, [])

    def test_index_of_names_skipped(self):
        auto_clone_constituents.enable(_stream=io.StringIO())
        groups = self._resolved_groups_with([
            'index_of_water_vapor_specific_humidity',
            'index_of_cloud_liquid_dry_mixing_ratio',
        ])
        entries = _collect_auto_clone_entries(groups, scheme_store=None)
        self.assertEqual(entries, [])


########################################################################
# Integration: full parse + resolver pass on the auto-clone fixture
########################################################################

class TestFixtureParse(_AutoCloneFixture):
    """The sample fixture parses cleanly with the shim on and carries
    every legacy attr on the right scheme args."""

    def setUp(self):
        super().setUp()
        auto_clone_constituents.enable(_stream=io.StringIO())

    def test_fixture_parses(self):
        tables = parse_metadata_file(_sf('scheme_auto_clone_consumer.meta'))
        self.assertEqual(len(tables), 1)
        section = tables[0].sections()[0]
        by_name = {v.local_name: v for v in section.variables}
        self.assertIn('qv', by_name)
        self.assertIn('qc', by_name)
        # qv carries the full legacy attr set.
        qv = by_name['qv']
        self.assertTrue(qv.is_constituent)
        self.assertTrue(qv.advected)
        self.assertEqual(qv.default_value, 1.0e-12)
        self.assertEqual(qv.min_value, 0.0)
        self.assertIs(qv.water_species, True)
        self.assertEqual(qv.mixing_ratio_type, 'wrt_moist')
        # qc only sets default_value.
        qc = by_name['qc']
        self.assertEqual(qc.default_value, 0.0)
        self.assertIsNone(qc.min_value)
        self.assertIsNone(qc.water_species)
        self.assertIsNone(qc.mixing_ratio_type)

    def test_fixture_rejected_without_shim(self):
        # Drop the flag; the same .meta file now fails.
        auto_clone_constituents.disable()
        with self.assertRaises(CCPPError) as cm:
            parse_metadata_file(_sf('scheme_auto_clone_consumer.meta'))
        # The first rejected attr is ``default_value`` (qv's first
        # legacy attr in declaration order).  Any of the four would
        # be acceptable; check the generic "Unknown variable
        # attribute" wording.
        self.assertIn('Unknown variable attribute', str(cm.exception))



########################################################################
# Doctest loader
########################################################################

def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(auto_clone_constituents))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
