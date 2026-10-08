"""Tests for the transient legacy-mode shim.

This whole file is part of the legacy-compat migration shim and should
be deleted alongside ``metadata/legacy_compat.py`` when the migration
is complete.  Search ``legacy-compat`` to find every touchpoint.
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

from metadata import legacy_compat                      # noqa: E402
from metadata.metadata_table import (                   # noqa: E402
    MetaVar, _parse_dimensions, _parse_lines,
)
from metadata.parse_tools import CCPPError, ParseContext  # noqa: E402


def _ctx():
    return ParseContext(linenum=1, filename='legacy_compat_test.meta')


class _LegacyModeFixture(unittest.TestCase):
    """Mixin that flips legacy mode on for the duration of a test and
    guarantees it goes back off afterwards (the flag is process state).
    """

    def setUp(self):
        # Sanity: never start a test with the flag set by an earlier
        # test that crashed before cleanup.
        legacy_compat.disable()

    def tearDown(self):
        legacy_compat.disable()


class TestTranslateOff(_LegacyModeFixture):
    """When legacy mode is disabled, ``translate`` is a strict identity."""

    def test_identity_on_legacy_name_when_disabled(self):
        self.assertFalse(legacy_compat.is_enabled())
        self.assertEqual(
            legacy_compat.translate('horizontal_loop_extent'),
            'horizontal_loop_extent',
        )

    def test_identity_on_unknown_name(self):
        self.assertEqual(
            legacy_compat.translate('air_temperature'), 'air_temperature',
        )


class TestEnableDisable(_LegacyModeFixture):

    def test_enable_flips_flag_and_writes_banner(self):
        sink = io.StringIO()
        legacy_compat.enable(_stream=sink)
        self.assertTrue(legacy_compat.is_enabled())
        out = sink.getvalue()
        # Bold banner: starred border, the deprecated names, and the
        # canonical replacements all appear.
        self.assertIn('LEGACY-MODE ENABLED', out)
        self.assertIn('horizontal_loop_extent', out)
        self.assertIn('horizontal_dimension', out)
        # Banner also enumerates the number_of_openmp_threads pair.
        self.assertIn('number_of_openmp_threads', out)
        self.assertIn('number_of_threads', out)
        self.assertIn('TRANSIENT', out)
        self.assertIn('REMOVED', out)
        self.assertGreaterEqual(out.count('*' * 10), 2)

    def test_enable_is_idempotent(self):
        sink1 = io.StringIO()
        legacy_compat.enable(_stream=sink1)
        first = sink1.getvalue()
        sink2 = io.StringIO()
        legacy_compat.enable(_stream=sink2)  # second call — no banner
        self.assertEqual(sink2.getvalue(), '')
        self.assertIn('LEGACY-MODE', first)

    def test_disable_resets(self):
        legacy_compat.enable(_stream=io.StringIO())
        self.assertTrue(legacy_compat.is_enabled())
        legacy_compat.disable()
        self.assertFalse(legacy_compat.is_enabled())

    def test_logger_receives_warning(self):
        logger = logging.getLogger('legacy_compat_test_logger')
        records = []

        class _Capture(logging.Handler):
            def emit(self, record):
                records.append(record)

        handler = _Capture(level=logging.WARNING)
        logger.addHandler(handler)
        try:
            legacy_compat.enable(logger=logger, _stream=io.StringIO())
            self.assertEqual(len(records), 1)
            self.assertEqual(records[0].levelno, logging.WARNING)
            msg = records[0].getMessage()
            self.assertIn('horizontal_loop_extent', msg)
            self.assertIn('number_of_openmp_threads', msg)
        finally:
            logger.removeHandler(handler)


class TestTranslateOn(_LegacyModeFixture):
    """When legacy mode is enabled, the documented map applies."""

    def setUp(self):
        super().setUp()
        legacy_compat.enable(_stream=io.StringIO())

    def test_horizontal_loop_extent_rewritten(self):
        self.assertEqual(
            legacy_compat.translate('horizontal_loop_extent'),
            'horizontal_dimension',
        )

    def test_number_of_openmp_threads_rewritten(self):
        """Legacy CCPP-physics hosts (and SCM 17p8) size per-thread DDT
        containers by ``number_of_openmp_threads``.  The capgen
        convention is ``number_of_threads`` (matching the
        ``thread_number`` control variable name).  Legacy mode rewrites
        both as a standard_name attribute AND as a dimension token."""
        self.assertEqual(
            legacy_compat.translate('number_of_openmp_threads'),
            'number_of_threads',
        )

    def test_unknown_name_passes_through(self):
        self.assertEqual(
            legacy_compat.translate('air_temperature'), 'air_temperature',
        )

    def test_uppercase_legacy_passes_through_translate(self):
        # ``translate`` itself is case-sensitive — case folding is the
        # caller's responsibility (it happens upstream in
        # ``check_cf_standard_name``).  This pins that contract so a
        # future refactor doesn't accidentally widen it.
        self.assertEqual(
            legacy_compat.translate('Horizontal_Loop_Extent'),
            'Horizontal_Loop_Extent',
        )


########################################################################
# Integration through metadata_table hook points
########################################################################

class TestMetaVarStandardNameHook(_LegacyModeFixture):
    """``MetaVar.set_attr('standard_name', ...)`` runs the value through
    ``check_cf_standard_name`` (which lowercases) AND through
    ``legacy_compat.translate``.  When legacy mode is enabled, a scheme
    declaring ``standard_name = horizontal_loop_extent`` ends up with
    ``standard_name == 'horizontal_dimension'``."""

    def _make_var(self, std_name_value):
        ctx = _ctx()
        v = MetaVar('foo', ctx)
        v.set_attr('standard_name', std_name_value, ctx)
        return v

    def test_disabled_keeps_legacy_name(self):
        # Default mode: the parser accepts the legacy name (it's a
        # valid CF identifier) but does NOT rewrite it.  Downstream
        # consumers reject it just like they would in a non-legacy
        # build.
        v = self._make_var('horizontal_loop_extent')
        self.assertEqual(v.standard_name, 'horizontal_loop_extent')

    def test_enabled_rewrites_lowercase_legacy_name(self):
        legacy_compat.enable(_stream=io.StringIO())
        v = self._make_var('horizontal_loop_extent')
        self.assertEqual(v.standard_name, 'horizontal_dimension')

    def test_enabled_rewrites_mixedcase_legacy_name(self):
        # check_cf_standard_name lowercases first; translate runs
        # against the lowercase form.  Mixed-case legacy spellings
        # therefore still get rewritten.
        legacy_compat.enable(_stream=io.StringIO())
        v = self._make_var('Horizontal_Loop_Extent')
        self.assertEqual(v.standard_name, 'horizontal_dimension')

    def test_enabled_leaves_non_legacy_unchanged(self):
        legacy_compat.enable(_stream=io.StringIO())
        v = self._make_var('air_temperature')
        self.assertEqual(v.standard_name, 'air_temperature')


class TestDimensionHook(_LegacyModeFixture):
    """``_parse_dimensions`` runs each non-integer token through
    ``legacy_compat.translate`` after lowercasing."""

    def test_disabled_keeps_legacy_dim_token(self):
        dims = _parse_dimensions(
            '(horizontal_loop_extent, vertical_layer_dimension)', _ctx(),
        )
        self.assertEqual(dims,
                         ['horizontal_loop_extent', 'vertical_layer_dimension'])

    def test_enabled_rewrites_legacy_dim_token(self):
        legacy_compat.enable(_stream=io.StringIO())
        dims = _parse_dimensions(
            '(horizontal_loop_extent, vertical_layer_dimension)', _ctx(),
        )
        self.assertEqual(dims,
                         ['horizontal_dimension', 'vertical_layer_dimension'])

    def test_enabled_rewrites_inside_range_form(self):
        legacy_compat.enable(_stream=io.StringIO())
        dims = _parse_dimensions(
            '(ccpp_constant_one:horizontal_loop_extent)', _ctx(),
        )
        self.assertEqual(dims, ['ccpp_constant_one:horizontal_dimension'])

    def test_enabled_passes_integer_literals_through(self):
        legacy_compat.enable(_stream=io.StringIO())
        dims = _parse_dimensions('(1:8)', _ctx())
        self.assertEqual(dims, ['1:8'])

    def test_enabled_mixed_case_legacy_in_dim(self):
        legacy_compat.enable(_stream=io.StringIO())
        dims = _parse_dimensions('(Horizontal_Loop_Extent)', _ctx())
        self.assertEqual(dims, ['horizontal_dimension'])


class TestEndToEndMetadataParse(_LegacyModeFixture):
    """Full parse of a small scheme metadata snippet via ``_parse_lines``
    confirms that the legacy name flows through both hook points
    (standard_name on a scalar arg + dim token on an array arg)."""

    _META = (
        '[ccpp-table-properties]\n'
        '  name = legacy_test_scheme\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = legacy_test_scheme_run\n'
        '  type = scheme\n'
        '[ ncols ]\n'
        '  standard_name = horizontal_loop_extent\n'
        '  units = count\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '  intent = in\n'
        '[ t ]\n'
        '  standard_name = air_temperature\n'
        '  units = K\n'
        '  dimensions = (horizontal_loop_extent, vertical_layer_dimension)\n'
        '  type = real | kind = kind_phys\n'
        '  intent = inout\n'
    )

    def _parse_and_get_section(self):
        tables = _parse_lines(self._META.splitlines(keepends=True), 't.meta')
        return tables[0].sections()[0]

    def test_legacy_off_preserves_legacy_name(self):
        section = self._parse_and_get_section()
        std_names = [v.standard_name for v in section.variables]
        self.assertIn('horizontal_loop_extent', std_names)
        dim_tokens = section.variables[1].dimensions
        self.assertEqual(dim_tokens[0], 'horizontal_loop_extent')

    def test_legacy_on_rewrites_both_sites(self):
        legacy_compat.enable(_stream=io.StringIO())
        section = self._parse_and_get_section()
        std_names = [v.standard_name for v in section.variables]
        self.assertIn('horizontal_dimension', std_names)
        self.assertNotIn('horizontal_loop_extent', std_names)
        dim_tokens = section.variables[1].dimensions
        self.assertEqual(dim_tokens[0], 'horizontal_dimension')


########################################################################
# Doctest loader for legacy_compat module
########################################################################

def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(legacy_compat))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
