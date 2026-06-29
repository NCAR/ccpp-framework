"""Tests for the transient GFS dim-aliases shim.

This whole file is part of the dim-aliases shim and should be deleted
alongside ``metadata/dim_aliases.py`` when the GFS-physics rename is
complete.  Search ``dim-aliases`` / ``gfs-dim-aliases`` to find every
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

from metadata import dim_aliases                          # noqa: E402
from generator.suite_resolver import _canonical_dim       # noqa: E402


class _DimAliasesFixture(unittest.TestCase):
    """Mixin that flips the shim on for the duration of a test and
    guarantees it goes back off afterwards (the flag is process state).
    """

    def setUp(self):
        # Sanity: never start a test with the flag set by an earlier
        # test that crashed before cleanup.
        dim_aliases.disable()

    def tearDown(self):
        dim_aliases.disable()


class TestCanonicalOff(_DimAliasesFixture):
    """When the shim is disabled, ``canonical`` is a strict identity."""

    def test_identity_on_aliased_name_when_disabled(self):
        self.assertFalse(dim_aliases.is_enabled())
        self.assertEqual(
            dim_aliases.canonical(
                'adjusted_vertical_layer_dimension_for_radiation'),
            'adjusted_vertical_layer_dimension_for_radiation',
        )
        self.assertEqual(
            dim_aliases.canonical('vertical_composition_dimension'),
            'vertical_composition_dimension',
        )

    def test_identity_on_unknown_name(self):
        self.assertEqual(
            dim_aliases.canonical('air_temperature'), 'air_temperature',
        )


class TestEnableDisable(_DimAliasesFixture):

    def test_enable_flips_flag_and_writes_banner(self):
        sink = io.StringIO()
        dim_aliases.enable(_stream=sink)
        self.assertTrue(dim_aliases.is_enabled())
        out = sink.getvalue()
        # Bold banner: starred border, both alias pairs, transient marker.
        self.assertIn('GFS DIM-ALIASES ENABLED', out)
        self.assertIn('adjusted_vertical_layer_dimension_for_radiation', out)
        self.assertIn('vertical_composition_dimension', out)
        self.assertIn('vertical_layer_dimension', out)
        self.assertIn('TRANSIENT', out)
        self.assertIn('REMOVED', out)
        self.assertGreaterEqual(out.count('*' * 10), 2)

    def test_enable_is_idempotent(self):
        sink1 = io.StringIO()
        dim_aliases.enable(_stream=sink1)
        first = sink1.getvalue()
        sink2 = io.StringIO()
        dim_aliases.enable(_stream=sink2)  # second call — no banner
        self.assertEqual(sink2.getvalue(), '')
        self.assertIn('GFS DIM-ALIASES', first)

    def test_disable_resets(self):
        dim_aliases.enable(_stream=io.StringIO())
        self.assertTrue(dim_aliases.is_enabled())
        dim_aliases.disable()
        self.assertFalse(dim_aliases.is_enabled())

    def test_logger_receives_warning(self):
        logger = logging.getLogger('dim_aliases_test_logger')
        records = []

        class _Capture(logging.Handler):
            def emit(self, record):
                records.append(record)

        handler = _Capture(level=logging.WARNING)
        logger.addHandler(handler)
        try:
            dim_aliases.enable(logger=logger, _stream=io.StringIO())
            self.assertEqual(len(records), 1)
            self.assertEqual(records[0].levelno, logging.WARNING)
            msg = records[0].getMessage()
            self.assertIn('adjusted_vertical_layer_dimension_for_radiation',
                          msg)
            self.assertIn('vertical_composition_dimension', msg)
            self.assertIn('vertical_layer_dimension', msg)
        finally:
            logger.removeHandler(handler)


class TestCanonicalOn(_DimAliasesFixture):
    """When the shim is enabled, the documented map applies."""

    def setUp(self):
        super().setUp()
        dim_aliases.enable(_stream=io.StringIO())

    def test_adjusted_radiation_collapses(self):
        self.assertEqual(
            dim_aliases.canonical(
                'adjusted_vertical_layer_dimension_for_radiation'),
            'vertical_layer_dimension',
        )

    def test_composition_collapses(self):
        self.assertEqual(
            dim_aliases.canonical('vertical_composition_dimension'),
            'vertical_layer_dimension',
        )

    def test_unknown_name_passes_through(self):
        self.assertEqual(
            dim_aliases.canonical('air_temperature'), 'air_temperature',
        )

    def test_canonical_target_is_idempotent(self):
        # The representative itself is not in the alias map; calling
        # canonical on it must be a no-op so repeated normalisation
        # never drifts.
        self.assertEqual(
            dim_aliases.canonical('vertical_layer_dimension'),
            'vertical_layer_dimension',
        )


########################################################################
# Integration through suite_resolver._canonical_dim
########################################################################

class TestCanonicalDimHook(_DimAliasesFixture):
    """``_canonical_dim`` calls ``dim_aliases.canonical`` on the upper
    bound only.  Disabled mode keeps the original spelling; enabled
    mode collapses every member of an alias group to one
    representative so the per-position dim-identity check accepts the
    pairing.
    """

    def test_disabled_keeps_aliased_dim_distinct(self):
        a = _canonical_dim('adjusted_vertical_layer_dimension_for_radiation')
        b = _canonical_dim('vertical_layer_dimension')
        self.assertNotEqual(a, b)

    def test_enabled_collapses_radiation_alias(self):
        dim_aliases.enable(_stream=io.StringIO())
        a = _canonical_dim('adjusted_vertical_layer_dimension_for_radiation')
        b = _canonical_dim('vertical_layer_dimension')
        self.assertEqual(a, b)
        # And the collapsed form is the canonical representative.
        self.assertEqual(a, 'ccpp_constant_one:vertical_layer_dimension')

    def test_enabled_collapses_composition_alias(self):
        dim_aliases.enable(_stream=io.StringIO())
        a = _canonical_dim('vertical_composition_dimension')
        b = _canonical_dim('vertical_layer_dimension')
        self.assertEqual(a, b)
        self.assertEqual(a, 'ccpp_constant_one:vertical_layer_dimension')

    def test_enabled_collapses_in_explicit_range_form(self):
        # Aliasing applies on the *upper* bound of an explicit lower:upper
        # form too.  Lower bound never aliases.
        dim_aliases.enable(_stream=io.StringIO())
        a = _canonical_dim(
            'ccpp_constant_one:vertical_composition_dimension')
        b = _canonical_dim('vertical_layer_dimension')
        self.assertEqual(a, b)

    def test_enabled_does_not_alias_unrelated_dim(self):
        dim_aliases.enable(_stream=io.StringIO())
        a = _canonical_dim('horizontal_dimension')
        b = _canonical_dim('vertical_layer_dimension')
        self.assertNotEqual(a, b)

    def test_enabled_does_not_alias_lower_bound(self):
        # Only the upper bound is rewritten.  If somebody wrote
        # 'adjusted_vertical_layer_dimension_for_radiation:foo' as a
        # range, the lower bound stays verbatim — there is no host
        # context in which an alias name appears as a loop-begin
        # control var, so we keep this strict.
        dim_aliases.enable(_stream=io.StringIO())
        a = _canonical_dim(
            'adjusted_vertical_layer_dimension_for_radiation:foo')
        self.assertEqual(
            a,
            'adjusted_vertical_layer_dimension_for_radiation:foo',
        )


########################################################################
# Doctest loader for dim_aliases module
########################################################################

def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(dim_aliases))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
