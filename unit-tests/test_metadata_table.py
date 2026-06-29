#!/usr/bin/env python3

"""Unit tests for :mod:`metadata.metadata_table`.

Run with::

    python -m pytest capgen/tests/test_metadata_table.py -v

or::

    python -m unittest capgen.tests.test_metadata_table

All test methods follow the ``test_<topic>`` naming convention and are
documented inline to explain both what is being tested and *why* it matters
for the redesigned generator.
"""

import os
import sys
import textwrap
import tempfile
import unittest

# ---- locate the package root -----------------------------------------------
_TESTS_DIR  = os.path.dirname(os.path.abspath(__file__))
_PKG_ROOT   = os.path.join(os.path.dirname(_TESTS_DIR), 'capgen')
if _PKG_ROOT not in sys.path:
    sys.path.insert(0, _PKG_ROOT)

# ---- imports from the package -----------------------------------------------
from metadata.parse_tools import CCPPError, ParseSyntaxError
from metadata.metadata_table import (
    MetaVar,
    MetadataSection,
    MetadataTable,
    parse_metadata_file,
    _parse_lines,
    VALID_TABLE_TYPES,
    VALID_SCHEME_PHASES,
    VALID_INTENTS,
    SCHEME_TABLE_TYPE,
    SINGLETON_TABLE_TYPES,
    _is_blank,
    _parse_bool,
    _parse_dimensions,
    _check_var_type,
    _parse_config_line,
    _strip_inline_comment,
)
from metadata.parse_tools.parse_source import ParseContext

# ---- sample file directory --------------------------------------------------
_SAMPLE_DIR = os.path.join(_TESTS_DIR, 'sample_files')


########################################################################
# Helper utilities
########################################################################

def _ctx(lineno=0):
    """Return a minimal :class:`ParseContext` for testing."""
    return ParseContext(linenum=lineno, filename='test_file.meta')


def _parse_text(text: str):
    """Parse a metadata string and return the list of MetadataTable objects."""
    lines = textwrap.dedent(text).splitlines(keepends=True)
    return _parse_lines(lines, '<test_string>')


########################################################################
# Helper function tests
########################################################################

class TestIsBlank(unittest.TestCase):
    """Tests for :func:`_is_blank`."""

    def test_empty_string(self):
        self.assertTrue(_is_blank(''))

    def test_whitespace_only(self):
        self.assertTrue(_is_blank('   \t  '))

    def test_hash_comment(self):
        self.assertTrue(_is_blank('# this is a comment'))

    def test_semicolon_comment(self):
        self.assertTrue(_is_blank('; comment'))

    def test_hash_with_leading_spaces(self):
        self.assertTrue(_is_blank('   # indented comment'))

    def test_real_content(self):
        self.assertFalse(_is_blank('name = foo'))

    def test_bracket_header(self):
        self.assertFalse(_is_blank('[ccpp-table-properties]'))


class TestStripInlineComment(unittest.TestCase):
    """Tests for :func:`_strip_inline_comment` — trailing ``# ...`` is
    a comment that the parser must discard before any other handling.
    """

    def test_plain_line_unchanged(self):
        self.assertEqual(
            _strip_inline_comment('dimensions = (horizontal_dimension)'),
            'dimensions = (horizontal_dimension)',
        )

    def test_strips_trailing_hash_comment(self):
        self.assertEqual(
            _strip_inline_comment('dimensions = () # (nap_indices)'),
            'dimensions = ()',
        )

    def test_strips_section_header_comment(self):
        self.assertEqual(
            _strip_inline_comment('[ ap_indices ]   # legacy'),
            '[ ap_indices ]',
        )

    def test_full_line_comment_collapses_to_empty(self):
        self.assertEqual(_strip_inline_comment('# whole line'), '')

    def test_hash_at_column_zero(self):
        self.assertEqual(_strip_inline_comment('#x'), '')


class TestInlineCommentInParser(unittest.TestCase):
    """End-to-end check that the parser ignores trailing ``#`` comments
    on any metadata line — the user-reported bug surfaced on a
    ``dimensions =`` attribute value but the fix is universal."""

    _SRC = (
        '[ccpp-table-properties]\n'
        '  name = mod   # the host module\n'
        '  type = host\n'
        '[ccpp-arg-table]\n'
        '  name = mod\n'
        '  type = host\n'
        '[ ap_indices ]   # legacy index slot\n'
        '  standard_name = ap_indices\n'
        '  units = index\n'
        '  dimensions = () # (nap_indices)\n'
        '  type = integer\n'
    )

    def test_parses_cleanly(self):
        tables = _parse_text(self._SRC)
        self.assertEqual(len(tables), 1)
        sec = tables[0].sections()[0]
        var = sec.variables[0]
        self.assertEqual(var.local_name, 'ap_indices')
        self.assertEqual(var.dimensions, [])
        self.assertEqual(var.type, 'integer')
        self.assertEqual(tables[0].table_name, 'mod')


class TestParseBool(unittest.TestCase):
    """Tests for :func:`_parse_bool`."""

    def test_true_variants(self):
        ctx = _ctx()
        for val in ('True', 'true', 'TRUE', '.true.', '.TRUE.', 't', '1'):
            with self.subTest(val=val):
                self.assertTrue(_parse_bool(val, ctx))

    def test_false_variants(self):
        ctx = _ctx()
        for val in ('False', 'false', 'FALSE', '.false.', '.FALSE.', 'f', '0'):
            with self.subTest(val=val):
                self.assertFalse(_parse_bool(val, ctx))

    def test_invalid(self):
        ctx = _ctx()
        with self.assertRaises(CCPPError):
            _parse_bool('yes', ctx)


class TestParseDimensions(unittest.TestCase):
    """Tests for :func:`_parse_dimensions`."""

    def test_scalar(self):
        self.assertEqual(_parse_dimensions('()', _ctx()), [])

    def test_one_dim(self):
        self.assertEqual(
            _parse_dimensions('(horizontal_dimension)', _ctx()),
            ['horizontal_dimension']
        )

    def test_two_dims(self):
        result = _parse_dimensions(
            '(horizontal_dimension, vertical_layer_dimension)', _ctx()
        )
        self.assertEqual(result, ['horizontal_dimension', 'vertical_layer_dimension'])

    def test_whitespace_inside(self):
        result = _parse_dimensions(' ( dim1 , dim2 ) ', _ctx())
        self.assertEqual(result, ['dim1', 'dim2'])

    def test_missing_parens(self):
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_dimensions('dim1, dim2', _ctx())

    def test_empty_entry(self):
        """An empty entry like ``(,dim2)`` must be rejected."""
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_dimensions('(, dim2)', _ctx())

    def test_mixed_case_normalised_to_lower(self):
        """CCPP standard names are case-insensitive; the parser must
        normalise dimension tokens to lower-case so downstream
        host_dict lookups (which store std names lower-cased per
        :func:`check_cf_standard_name`) succeed."""
        result = _parse_dimensions(
            '(number_of_aerosol_tracers_MG)', _ctx(),
        )
        self.assertEqual(result, ['number_of_aerosol_tracers_mg'])

    def test_mixed_case_in_range_normalised(self):
        """Range form ``lower:upper`` lowercases both halves."""
        result = _parse_dimensions(
            '(ccpp_constant_one:Vertical_LAYER_dimension)', _ctx(),
        )
        self.assertEqual(
            result, ['ccpp_constant_one:vertical_layer_dimension'],
        )

    def test_integer_literal_passes_through(self):
        """Integer-literal dim entries (used in DDT field shapes) are
        unaffected by the lower-case normalisation."""
        result = _parse_dimensions('(8)', _ctx())
        self.assertEqual(result, ['8'])


class TestCheckVarType(unittest.TestCase):
    """Tests for :func:`_check_var_type`."""

    def test_intrinsic_types(self):
        for t in ('real', 'integer', 'logical', 'character', 'complex'):
            with self.subTest(t=t):
                self.assertEqual(_check_var_type(t, _ctx()), t)

    def test_ddt_identifier(self):
        self.assertEqual(_check_var_type('gfs_statein_type', _ctx()),
                         'gfs_statein_type')

    def test_type_parens_form(self):
        result = _check_var_type('type(my_ddt)', _ctx())
        self.assertEqual(result, 'type(my_ddt)')

    def test_external_type(self):
        result = _check_var_type('external:mpi_f08:mpi_comm', _ctx())
        self.assertEqual(result, 'external:mpi_f08:mpi_comm')

    def test_invalid_type(self):
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _check_var_type('123invalid', _ctx())


class TestParseConfigLine(unittest.TestCase):
    """Tests for :func:`_parse_config_line`."""

    def test_simple_pair(self):
        result = _parse_config_line('  name = foo  ', _ctx())
        self.assertEqual(result, [('name', 'foo')])

    def test_pipe_separator(self):
        result = _parse_config_line('units = 1 | dimensions = ()', _ctx())
        self.assertEqual(result, [('units', '1'), ('dimensions', '()')])

    def test_blank_line(self):
        self.assertEqual(_parse_config_line('', _ctx()), [])

    def test_comment_line(self):
        self.assertEqual(_parse_config_line('# comment', _ctx()), [])

    def test_bad_line(self):
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_config_line('no_equals_sign', _ctx())


########################################################################
# MetaVar tests
########################################################################

class TestMetaVar(unittest.TestCase):
    """Tests for :class:`MetaVar`."""

    def _make_var(self, local_name='my_var', **attrs):
        """Build a MetaVar with sensible defaults plus any overrides."""
        ctx = _ctx()
        var = MetaVar(local_name, ctx)
        defaults = {
            'standard_name': 'my_standard_name',
            'units': '1',
            'dimensions': '()',
            'type': 'integer',
        }
        defaults.update(attrs)
        for k, v in defaults.items():
            var.set_attr(k, v, ctx)
        return var

    def test_creation(self):
        var = self._make_var()
        self.assertEqual(var.local_name, 'my_var')
        self.assertEqual(var.standard_name, 'my_standard_name')

    def test_standard_name_lowercased(self):
        """Standard names must be CF names (lowercased by the checker)."""
        ctx = _ctx()
        var = MetaVar('v', ctx)
        var.set_attr('standard_name', 'Horizontal_Dimension', ctx)
        self.assertEqual(var.standard_name, 'horizontal_dimension')

    def test_dimensions_scalar(self):
        var = self._make_var(dimensions='()')
        self.assertEqual(var.dimensions, [])

    def test_dimensions_array(self):
        var = self._make_var(dimensions='(horizontal_dimension, vertical_layer_dimension)')
        self.assertEqual(var.dimensions,
                         ['horizontal_dimension', 'vertical_layer_dimension'])

    def test_intent_valid(self):
        ctx = _ctx()
        for intent in ('in', 'out', 'inout'):
            with self.subTest(intent=intent):
                var = self._make_var()
                var.set_attr('intent', intent, ctx)
                self.assertEqual(var.intent, intent)

    def test_intent_invalid(self):
        ctx = _ctx()
        var = self._make_var()
        with self.assertRaises(CCPPError):
            var.set_attr('intent', 'banana', ctx)

    def test_invalid_attribute_error_carries_full_context(self):
        """When a check_X helper rejects a value, the resulting CCPPError
        MUST name the offending variable, the attribute, the raw value,
        AND the source location.  Without this enrichment the user sees
        a bare ``'' is not a valid unit`` with no clue which file/line/var
        is at fault — every check_X helper is unaware of context.
        Regression for the SCM ccpp-physics 61-file parse where the user
        couldn't locate the offending metadata.
        """
        ctx = ParseContext(linenum=42, filename='broken_scheme.meta')
        var = MetaVar('my_bad_var', ctx)
        with self.assertRaises(CCPPError) as raised:
            var.set_attr('units', '', ctx)
        msg = str(raised.exception)
        self.assertIn("'my_bad_var'", msg)        # variable name
        self.assertIn("'units'", msg)              # attribute name
        self.assertIn("broken_scheme.meta", msg)   # file
        self.assertIn(":43", msg)                  # line (1-based)
        self.assertIn("not a valid unit", msg)     # inner reason

    def test_invalid_attribute_error_does_not_double_wrap(self):
        """Helpers that already include the location (``_parse_dimensions``,
        ``_check_var_type``) should not have their location duplicated in
        the wrapper message.  Test confirms the wrapper detects the
        already-present location and re-raises unchanged."""
        ctx = ParseContext(linenum=5, filename='dim_broken.meta')
        var = MetaVar('v', ctx)
        with self.assertRaises(CCPPError) as raised:
            var.set_attr('dimensions', '(this is malformed', ctx)
        msg = str(raised.exception)
        # Location appears exactly once (no nested duplication).
        self.assertEqual(msg.count('dim_broken.meta'), 1)

    def test_protected_bool(self):
        var = self._make_var(protected='True')
        self.assertTrue(var.protected)

    def test_optional_bool(self):
        var = self._make_var(optional='False')
        self.assertFalse(var.optional)

    def test_local_name_accepts_long_subscript_index(self):
        """Sliced local names like ``dqdt(:,:,index_of_<long_std_name>)``
        carry CCPP standard names as subscript tokens; those routinely
        exceed the 63-char Fortran identifier limit.  Only the base
        identifier (``dqdt``) needs to fit the limit — the subscript is
        a standard-name reference resolved separately.
        """
        long_std = ('index_of_cloud_liquid_water_mixing_ratio_'
                    'in_tracer_concentration_array')  # 67 chars
        self.assertGreater(len(long_std), 63)
        long_local = 'dqdt(:,:,{})'.format(long_std)
        # Must NOT raise.
        var = self._make_var(local_name=long_local)
        self.assertEqual(var.local_name, long_local)

    def test_local_name_base_still_length_checked(self):
        """The base identifier (everything before the first ``(``) must
        still fit the 63-char limit — only subscript tokens are exempt.
        """
        from metadata.parse_tools import ParseSyntaxError
        long_base = 'a' * 64  # 64 > FORTRAN_MAX_IDENT_LEN
        with self.assertRaises(ParseSyntaxError):
            MetaVar(long_base, _ctx())
        # Same long base inside a slice — still rejected.
        with self.assertRaises(ParseSyntaxError):
            MetaVar('{}(:)'.format(long_base), _ctx())

    def test_local_name_rejects_malformed_reference(self):
        """The form check still applies — only the per-token length
        limit was relaxed.  A malformed reference is still an error."""
        from metadata.parse_tools import ParseSyntaxError
        with self.assertRaises(ParseSyntaxError):
            MetaVar('not a valid id', _ctx())

    def test_top_at_one_default_false(self):
        var = self._make_var()
        self.assertFalse(var.top_at_one)

    def test_top_at_one_true(self):
        for value in ('True', '.true.', 'true'):
            with self.subTest(value=value):
                var = self._make_var(top_at_one=value)
                self.assertTrue(var.top_at_one)

    def test_top_at_one_false(self):
        for value in ('False', '.false.', 'false'):
            with self.subTest(value=value):
                var = self._make_var(top_at_one=value)
                self.assertFalse(var.top_at_one)

    def test_diagnostic_name_explicit(self):
        var = self._make_var(diagnostic_name='temperature')
        self.assertEqual(var.diagnostic_name, 'temperature')
        self.assertEqual(var.diagnostic_name_fixed, '')

    def test_diagnostic_name_template_accepted(self):
        var = self._make_var(diagnostic_name='foo_${scheme_name}')
        self.assertEqual(var.diagnostic_name, 'foo_${scheme_name}')

    def test_diagnostic_name_fixed_explicit(self):
        var = self._make_var(diagnostic_name_fixed='Q')
        self.assertEqual(var.diagnostic_name_fixed, 'Q')
        # When _fixed is set, diagnostic_name returns '' (no local_name fallback).
        self.assertEqual(var.diagnostic_name, '')

    def test_diagnostic_name_defaults_to_local_name(self):
        """Neither attribute set: diagnostic_name defaults to local_name."""
        var = self._make_var(local_name='my_local_var')
        self.assertEqual(var.diagnostic_name, 'my_local_var')
        self.assertEqual(var.diagnostic_name_fixed, '')

    def test_diagnostic_name_invalid(self):
        ctx = _ctx()
        var = self._make_var()
        with self.assertRaises(CCPPError):
            var.set_attr('diagnostic_name', 'pref_${scheme}_suff', ctx)

    def test_diagnostic_name_fixed_invalid(self):
        ctx = _ctx()
        var = self._make_var()
        with self.assertRaises(CCPPError):
            var.set_attr('diagnostic_name_fixed', '2bad', ctx)

    def test_diagnostic_name_mutually_exclusive(self):
        """Setting both diagnostic_name and diagnostic_name_fixed is an error."""
        ctx = _ctx()
        var = self._make_var(diagnostic_name='temperature')
        with self.assertRaises(CCPPError):
            var.set_attr('diagnostic_name_fixed', 'Q', ctx)

    def test_diagnostic_name_fixed_then_name_mutually_exclusive(self):
        """Order independence: fixed first, then name, also rejected."""
        ctx = _ctx()
        var = self._make_var(diagnostic_name_fixed='Q')
        with self.assertRaises(CCPPError):
            var.set_attr('diagnostic_name', 'temperature', ctx)

    def test_duplicate_attr(self):
        """Setting the same attribute twice is an error."""
        ctx = _ctx()
        var = MetaVar('v', ctx)
        var.set_attr('standard_name', 'foo', ctx)
        with self.assertRaises(CCPPError):
            var.set_attr('standard_name', 'bar', ctx)

    def test_unknown_attr(self):
        ctx = _ctx()
        var = MetaVar('v', ctx)
        with self.assertRaises(CCPPError):
            var.set_attr('banana', 'value', ctx)

    def test_validate_requires_intent_for_scheme(self):
        """Scheme variables need intent; missing it must raise CCPPError."""
        ctx = _ctx()
        var = self._make_var()  # no intent set
        with self.assertRaises(CCPPError):
            var.validate(require_intent=True, context=ctx)

    def test_validate_no_intent_for_host(self):
        """Host variables do not need intent; validate must pass without it."""
        ctx = _ctx()
        var = self._make_var()
        var.validate(require_intent=False, context=ctx)  # should not raise

    def test_validate_missing_units_defaults_to_none(self):
        """Omitting ``units`` is allowed; it defaults to ``'none'``."""
        ctx = _ctx()
        var = MetaVar('v', ctx)
        var.set_attr('standard_name', 'foo', ctx)
        var.set_attr('dimensions', '()', ctx)
        var.set_attr('type', 'integer', ctx)
        var.validate(require_intent=False, context=ctx)  # should not raise
        self.assertEqual(var.units, 'none')

    def test_external_ddt_helpers(self):
        var = self._make_var(type='external:mpi_f08:mpi_comm')
        self.assertTrue(var.is_external_ddt())
        self.assertEqual(var.external_ddt_module(), 'mpi_f08')
        self.assertEqual(var.external_ddt_typename(), 'mpi_comm')

    def test_non_external_ddt_helpers(self):
        var = self._make_var(type='real')
        self.assertFalse(var.is_external_ddt())
        self.assertIsNone(var.external_ddt_module())
        self.assertIsNone(var.external_ddt_typename())

    def test_invalid_local_name(self):
        """Local name must be a valid Fortran identifier."""
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            MetaVar('123bad', _ctx())

    def test_local_name_too_long(self):
        """Local name must not exceed 63 characters (Fortran limit)."""
        long_name = 'a' * 64
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            MetaVar(long_name, _ctx())


########################################################################
# MetadataSection tests
########################################################################

class TestMetadataSection(unittest.TestCase):
    """Tests for :class:`MetadataSection`."""

    def _make_scheme_section(self, phase='run', scheme_name='my_scheme'):
        ctx = _ctx()
        return MetadataSection(
            section_name='{}__{}'.format(scheme_name, phase).replace('__', '_'),
            section_type='scheme',
            table_name=scheme_name,
            context=ctx,
        )

    def test_scheme_phase_extraction(self):
        for phase in VALID_SCHEME_PHASES:
            with self.subTest(phase=phase):
                sec = self._make_scheme_section(phase=phase)
                self.assertEqual(sec.phase, phase)

    def test_finalize_rejected(self):
        """'finalize' was renamed to 'final'; the old name must be rejected."""
        ctx = _ctx()
        with self.assertRaises(CCPPError) as cm:
            MetadataSection('my_scheme_finalize', 'scheme', 'my_scheme', ctx)
        self.assertIn('final', str(cm.exception))

    def test_host_section_has_no_phase(self):
        ctx = _ctx()
        sec = MetadataSection('physics_data', 'host', 'physics_data', ctx)
        self.assertIsNone(sec.phase)

    def test_duplicate_standard_name(self):
        """Adding two variables with the same standard name must raise."""
        ctx = _ctx()
        sec = MetadataSection('my_scheme_run', 'scheme', 'my_scheme', ctx)
        var1 = MetaVar('a_var', ctx)
        for attr, val in [('standard_name', 'foo'), ('units', '1'),
                          ('dimensions', '()'), ('type', 'integer'),
                          ('intent', 'in')]:
            var1.set_attr(attr, val, ctx)
        var1.validate(require_intent=True, context=ctx)
        sec.add_variable(var1)

        var2 = MetaVar('b_var', ctx)
        for attr, val in [('standard_name', 'foo'), ('units', '1'),
                          ('dimensions', '()'), ('type', 'integer'),
                          ('intent', 'in')]:
            var2.set_attr(attr, val, ctx)
        var2.validate(require_intent=True, context=ctx)
        with self.assertRaises(CCPPError):
            sec.add_variable(var2)

    def test_invalid_table_type(self):
        ctx = _ctx()
        with self.assertRaises(CCPPError):
            MetadataSection('t', 'banana', 't', ctx)

    def test_scheme_name_mismatch(self):
        """Section name not starting with scheme name must raise."""
        ctx = _ctx()
        with self.assertRaises(CCPPError):
            MetadataSection('other_scheme_run', 'scheme', 'my_scheme', ctx)


########################################################################
# MetadataTable tests
########################################################################

class TestMetadataTable(unittest.TestCase):
    """Tests for :class:`MetadataTable`."""

    def test_valid_types(self):
        for ttype in VALID_TABLE_TYPES:
            with self.subTest(ttype=ttype):
                ctx = _ctx()
                tbl = MetadataTable('tbl', ttype, 'f.meta', ctx)
                self.assertEqual(tbl.table_type, ttype)

    def test_invalid_type(self):
        ctx = _ctx()
        with self.assertRaises(CCPPError):
            MetadataTable('t', 'module', 'f.meta', ctx)

    def test_is_scheme(self):
        ctx = _ctx()
        self.assertTrue(MetadataTable('s', 'scheme', 'f.meta', ctx).is_scheme)
        self.assertFalse(MetadataTable('h', 'host', 'f.meta', ctx).is_scheme)

    def test_singleton_allows_one_section(self):
        """Singleton table types (host, control, ddt, suite) allow only one section."""
        for ttype in SINGLETON_TABLE_TYPES:
            with self.subTest(ttype=ttype):
                ctx = _ctx()
                tbl = MetadataTable('t', ttype, 'f.meta', ctx)
                sec1 = MetadataSection('t', ttype, 't', ctx)
                tbl.add_section(sec1)
                sec2 = MetadataSection('t', ttype, 't', ctx)
                with self.assertRaises(CCPPError):
                    tbl.add_section(sec2)

    def test_scheme_allows_multiple_sections(self):
        """Scheme tables allow one section per phase."""
        ctx = _ctx()
        tbl = MetadataTable('s', 'scheme', 'f.meta', ctx)
        for phase in ('init', 'run', 'final'):
            sec = MetadataSection('s_{}'.format(phase), 'scheme', 's', ctx)
            tbl.add_section(sec)
        self.assertEqual(len(tbl.sections()), 3)

    def test_section_type_mismatch(self):
        """Section type must match table type."""
        ctx = _ctx()
        tbl = MetadataTable('my_host', 'host', 'f.meta', ctx)
        # The section name must be valid for scheme (scheme_name_phase);
        # use a different scheme name so MetadataSection construction succeeds,
        # and the CCPPError is raised only at add_section() due to type mismatch.
        sec = MetadataSection('some_scheme_run', 'scheme', 'some_scheme', ctx)
        with self.assertRaises(CCPPError):
            tbl.add_section(sec)

    def test_section_for_phase(self):
        ctx = _ctx()
        tbl = MetadataTable('s', 'scheme', 'f.meta', ctx)
        run_sec = MetadataSection('s_run', 'scheme', 's', ctx)
        tbl.add_section(run_sec)
        self.assertIs(tbl.section_for_phase('run'), run_sec)
        self.assertIsNone(tbl.section_for_phase('init'))


########################################################################
# parse_metadata_file / _parse_lines tests
########################################################################

class TestParseLines(unittest.TestCase):
    """Tests for the actual ini-file parser via :func:`_parse_lines`."""

    # ---- valid cases -------------------------------------------------------

    def test_host_table(self):
        text = """
            [ccpp-table-properties]
              name = my_host
              type = host

            [ccpp-arg-table]
              name = my_host
              type = host
            [ im ]
              standard_name = horizontal_dimension
              units = count
              dimensions = ()
              type = integer
        """
        tables = _parse_text(text)
        self.assertEqual(len(tables), 1)
        tbl = tables[0]
        self.assertEqual(tbl.table_name, 'my_host')
        self.assertEqual(tbl.table_type, 'host')
        self.assertEqual(len(tbl.sections()), 1)
        sec = tbl.sections()[0]
        self.assertEqual(len(sec.variables), 1)
        var = sec.variables[0]
        self.assertEqual(var.local_name, 'im')
        self.assertEqual(var.standard_name, 'horizontal_dimension')
        self.assertEqual(var.dimensions, [])
        self.assertIsNone(var.intent)

    def test_scheme_three_phases(self):
        text = """
            [ccpp-table-properties]
              name = my_scheme
              type = scheme

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

            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ im ]
              standard_name = horizontal_dimension
              units = count
              dimensions = ()
              type = integer
              intent = in

            [ccpp-arg-table]
              name = my_scheme_final
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
        """
        tables = _parse_text(text)
        self.assertEqual(len(tables), 1)
        tbl = tables[0]
        self.assertTrue(tbl.is_scheme)
        self.assertEqual(len(tbl.sections()), 3)
        phases = {sec.phase for sec in tbl.sections()}
        self.assertEqual(phases, {'init', 'run', 'final'})

    def test_two_tables_in_one_file(self):
        """A .meta file may contain one DDT table followed by a scheme table."""
        text = """
            [ccpp-table-properties]
              name = my_ddt
              type = ddt

            [ccpp-arg-table]
              name = my_ddt
              type = ddt
            [ field1 ]
              standard_name = some_field
              units = 1
              dimensions = ()
              type = real

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
        """
        tables = _parse_text(text)
        self.assertEqual(len(tables), 2)
        self.assertEqual(tables[0].table_type, 'ddt')
        self.assertEqual(tables[1].table_type, 'scheme')

    def test_pipe_separated_attributes(self):
        """Multiple attributes on one line with ``|`` separator."""
        text = """
            [ccpp-table-properties]
              name = s
              type = scheme

            [ccpp-arg-table]
              name = s_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none | dimensions = () | type = character | kind = len=512
              intent = out
        """
        tables = _parse_text(text)
        var = tables[0].sections()[0].variables[0]
        self.assertEqual(var.units, 'none')
        self.assertEqual(var.dimensions, [])
        self.assertEqual(var.type, 'character')
        self.assertEqual(var.kind, 'len=512')
        self.assertEqual(var.intent, 'out')

    def test_control_table(self):
        text = """
            [ccpp-table-properties]
              name = ctrl
              type = control

            [ccpp-arg-table]
              name = ctrl
              type = control
            [ tnum ]
              standard_name = thread_number
              units = 1
              dimensions = ()
              type = integer
        """
        tables = _parse_text(text)
        self.assertEqual(tables[0].table_type, 'control')

    def test_ddt_instance_in_host_table(self):
        """Host table declaring a DDT instance variable (array of DDT)."""
        text = """
            [ccpp-table-properties]
              name = CCPP_data
              type = host

            [ccpp-arg-table]
              name = CCPP_data
              type = host
            [ gfs_statein ]
              standard_name = gfs_statein
              units = none
              dimensions = (number_of_instances)
              type = gfs_statein_type
        """
        tables = _parse_text(text)
        var = tables[0].sections()[0].variables[0]
        self.assertEqual(var.type, 'gfs_statein_type')
        self.assertEqual(var.dimensions, ['number_of_instances'])

    def test_comments_and_blank_lines_ignored(self):
        """Comment lines (``#``) and blank lines must be skipped silently."""
        text = """
            # This is a comment

            [ccpp-table-properties]
              name = h
              type = host

            ########################################
            [ccpp-arg-table]
              # section comment
              name = h
              type = host
            ; semicolon comment
            [ x ]
              standard_name = some_var
              units = 1
              dimensions = ()
              type = integer
        """
        tables = _parse_text(text)
        self.assertEqual(len(tables), 1)
        self.assertEqual(len(tables[0].sections()[0].variables), 1)

    # ---- error cases -------------------------------------------------------

    def test_module_type_rejected(self):
        """``type = module`` must produce a CCPPError mentioning 'host'."""
        text = """
            [ccpp-table-properties]
              name = physics_mod
              type = module
        """
        with self.assertRaises(CCPPError) as cm:
            _parse_text(text)
        self.assertIn('host', str(cm.exception).lower())

    def test_banana_type_rejected(self):
        """An unknown table type must raise CCPPError."""
        text = """
            [ccpp-table-properties]
              name = bad
              type = banana
        """
        with self.assertRaises(CCPPError):
            _parse_text(text)

    def test_finalize_phase_rejected(self):
        """``_finalize`` phase name must raise CCPPError mentioning 'final'."""
        text = """
            [ccpp-table-properties]
              name = s
              type = scheme

            [ccpp-arg-table]
              name = s_finalize
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              intent = out
        """
        with self.assertRaises(CCPPError) as cm:
            _parse_text(text)
        self.assertIn('final', str(cm.exception))

    def test_duplicate_standard_name_in_section(self):
        """Two variables with the same standard name in one section must raise."""
        text = """
            [ccpp-table-properties]
              name = s
              type = scheme

            [ccpp-arg-table]
              name = s_run
              type = scheme
            [ a ]
              standard_name = horizontal_dimension
              units = count
              dimensions = ()
              type = integer
              intent = in
            [ b ]
              standard_name = horizontal_dimension
              units = count
              dimensions = ()
              type = integer
              intent = in
        """
        with self.assertRaises(CCPPError):
            _parse_text(text)

    def test_missing_intent_for_scheme_variable(self):
        """Scheme variables without ``intent`` must raise at validation time."""
        text = """
            [ccpp-table-properties]
              name = s
              type = scheme

            [ccpp-arg-table]
              name = s_run
              type = scheme
            [ im ]
              standard_name = horizontal_dimension
              units = count
              dimensions = ()
              type = integer
        """
        with self.assertRaises(CCPPError):
            _parse_text(text)

    def test_invalid_intent_value(self):
        text = """
            [ccpp-table-properties]
              name = s
              type = scheme

            [ccpp-arg-table]
              name = s_run
              type = scheme
            [ im ]
              standard_name = horizontal_dimension
              units = count
              dimensions = ()
              type = integer
              intent = banana
        """
        with self.assertRaises(CCPPError):
            _parse_text(text)

    def test_bad_dimensions(self):
        """Dimensions not enclosed in parentheses must raise."""
        text = """
            [ccpp-table-properties]
              name = h
              type = host

            [ccpp-arg-table]
              name = h
              type = host
            [ x ]
              standard_name = foo
              units = 1
              dimensions = no_parens
              type = integer
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_missing_required_attribute(self):
        """Missing ``type`` must raise at variable validation."""
        text = """
            [ccpp-table-properties]
              name = h
              type = host

            [ccpp-arg-table]
              name = h
              type = host
            [ x ]
              standard_name = foo
              units = 1
              dimensions = ()
        """
        with self.assertRaises(CCPPError):
            _parse_text(text)

    def test_missing_units_defaults_to_none(self):
        """Omitting ``units`` is accepted; the variable's units default to ``'none'``."""
        text = """
            [ccpp-table-properties]
              name = h
              type = host

            [ccpp-arg-table]
              name = h
              type = host
            [ x ]
              standard_name = foo
              dimensions = ()
              type = integer
        """
        tables = _parse_text(text)
        self.assertEqual(len(tables), 1)
        section = tables[0].sections()[0]
        var = section.variables[0]
        self.assertEqual(var.units, 'none')

    def test_section_type_mismatch_raises(self):
        """Section type different from table type must raise."""
        text = """
            [ccpp-table-properties]
              name = h
              type = host

            [ccpp-arg-table]
              name = h
              type = scheme
        """
        with self.assertRaises(CCPPError):
            _parse_text(text)

    def test_singleton_table_second_section_raises(self):
        """A ``host`` table with two sections must raise."""
        text = """
            [ccpp-table-properties]
              name = h
              type = host

            [ccpp-arg-table]
              name = h
              type = host

            [ccpp-arg-table]
              name = h
              type = host
        """
        with self.assertRaises(CCPPError):
            _parse_text(text)


########################################################################
# Attribute ownership enforcement tests
########################################################################

class TestAttributeOwnership(unittest.TestCase):
    """Parse-time rejection of active/optional in wrong table types."""

    def test_active_in_scheme_raises(self):
        """'active' attribute in scheme metadata must raise ParseSyntaxError."""
        text = """
            [ccpp-table-properties]
              name = my_scheme
              type = scheme

            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              active = .true.
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_optional_in_host_raises(self):
        """'optional' attribute in host metadata must raise ParseSyntaxError."""
        text = """
            [ccpp-table-properties]
              name = my_host
              type = host

            [ccpp-arg-table]
              name = my_host
              type = host
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              optional = true
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_optional_in_control_raises(self):
        """'optional' attribute in control metadata must raise ParseSyntaxError."""
        text = """
            [ccpp-table-properties]
              name = my_ctrl
              type = control

            [ccpp-arg-table]
              name = my_ctrl
              type = control
            [ x ]
              standard_name = foo
              units = 1
              dimensions = ()
              type = integer
              optional = true
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_optional_in_ddt_raises(self):
        """'optional' attribute in ddt metadata must raise ParseSyntaxError."""
        text = """
            [ccpp-table-properties]
              name = my_ddt_type
              type = ddt

            [ccpp-arg-table]
              name = my_ddt_type
              type = ddt
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              optional = false
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_active_in_host_allowed(self):
        """'active' attribute in host metadata is valid."""
        text = """
            [ccpp-table-properties]
              name = my_host
              type = host

            [ccpp-arg-table]
              name = my_host
              type = host
            [ flag ]
              standard_name = my_flag
              units = flag
              dimensions = ()
              type = logical
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              active = my_flag
        """
        tables = _parse_text(text)
        self.assertEqual(len(tables), 1)

    def test_active_expression_is_lowercased(self):
        """Mixed-case identifiers in 'active' must be normalised to lowercase
        so they match the canonical lowercase standard names stored on host
        variables (see check_cf_standard_name).
        """
        text = """
            [ccpp-table-properties]
              name = my_host
              type = host

            [ccpp-arg-table]
              name = my_host
              type = host
            [ flag ]
              standard_name = flag_for_aerosol_input_mg_radiation
              units = flag
              dimensions = ()
              type = logical
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              active = (flag_for_aerosol_input_MG_radiation)
        """
        tables = _parse_text(text)
        x_var = next(
            v for v in tables[0].sections()[0].variables
            if v.local_name == 'x'
        )
        self.assertEqual(x_var.active, '(flag_for_aerosol_input_mg_radiation)')

    def test_intent_in_host_raises(self):
        """'intent' on a host table is now rejected (scheme-only)."""
        text = """
            [ccpp-table-properties]
              name = my_host
              type = host

            [ccpp-arg-table]
              name = my_host
              type = host
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              intent = in
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_intent_in_control_raises(self):
        """'intent' on a control table is now rejected (scheme-only)."""
        text = """
            [ccpp-table-properties]
              name = my_ctrl
              type = control

            [ccpp-arg-table]
              name = my_ctrl
              type = control
            [ x ]
              standard_name = foo
              units = 1
              dimensions = ()
              type = integer
              intent = in
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_intent_in_ddt_raises(self):
        """'intent' on a ddt table is rejected (scheme-only)."""
        text = """
            [ccpp-table-properties]
              name = my_ddt_type
              type = ddt

            [ccpp-arg-table]
              name = my_ddt_type
              type = ddt
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              intent = inout
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_active_in_control_raises(self):
        """'active' on a control table is rejected — control vars are
        unconditionally framework-injected, no active expression makes
        sense for them."""
        text = """
            [ccpp-table-properties]
              name = my_ctrl
              type = control

            [ccpp-arg-table]
              name = my_ctrl
              type = control
            [ x ]
              standard_name = foo
              units = 1
              dimensions = ()
              type = integer
              active = my_flag
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_active_in_ddt_allowed(self):
        """'active' on a ddt table is allowed — a host DDT component is a
        valid origin for the active-conditional storage contract."""
        text = """
            [ccpp-table-properties]
              name = my_ddt_type
              type = ddt

            [ccpp-arg-table]
              name = my_ddt_type
              type = ddt
            [ flag ]
              standard_name = my_flag
              units = flag
              dimensions = ()
              type = logical
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              active = my_flag
        """
        tables = _parse_text(text)
        self.assertEqual(len(tables), 1)

    def test_optional_in_scheme_allowed(self):
        """'optional' attribute in scheme metadata is valid."""
        text = """
            [ccpp-table-properties]
              name = my_scheme
              type = scheme

            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              intent = in
              optional = true
        """
        tables = _parse_text(text)
        self.assertEqual(len(tables), 1)

    def test_constituent_in_host_raises(self):
        """'constituent' attribute in host metadata must raise."""
        text = """
            [ccpp-table-properties]
              name = my_host
              type = host

            [ccpp-arg-table]
              name = my_host
              type = host
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              constituent = true
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_advected_in_host_raises(self):
        """'advected' attribute in host metadata must raise."""
        text = """
            [ccpp-table-properties]
              name = my_host
              type = host

            [ccpp-arg-table]
              name = my_host
              type = host
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              advected = .true.
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)

    def test_molar_mass_in_ddt_raises(self):
        """'molar_mass' attribute in ddt metadata must raise."""
        text = """
            [ccpp-table-properties]
              name = my_ddt_type
              type = ddt

            [ccpp-arg-table]
              name = my_ddt_type
              type = ddt
            [ x ]
              standard_name = foo
              units = m
              dimensions = ()
              type = real
              molar_mass = 18.0
        """
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            _parse_text(text)


########################################################################
# Constituent attribute tests (scheme metadata only)
########################################################################

class TestConstituentAttributes(unittest.TestCase):
    """Parsing and is_constituent rollup for scheme-only constituent hints."""

    def _scheme_var(self, *, extra_attrs: str = '') -> MetaVar:
        text = """
            [ccpp-table-properties]
              name = my_scheme
              type = scheme

            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ x ]
              standard_name = foo
              units = kg kg-1
              dimensions = ()
              type = real
              intent = inout
              {extra}
        """.format(extra=extra_attrs)
        tables = _parse_text(text)
        return tables[0].sections()[0].variables[0]

    def test_constituent_true_accepted(self):
        var = self._scheme_var(extra_attrs='constituent = True')
        self.assertTrue(var.constituent)
        self.assertTrue(var.is_constituent)

    def test_advected_dot_true_accepted(self):
        var = self._scheme_var(extra_attrs='advected = .true.')
        self.assertTrue(var.advected)
        self.assertTrue(var.is_constituent)

    def test_molar_mass_accepted(self):
        var = self._scheme_var(extra_attrs='molar_mass = 18.0')
        self.assertEqual(var.molar_mass, 18.0)
        self.assertTrue(var.is_constituent)

    def test_defaults_make_non_constituent(self):
        var = self._scheme_var()
        self.assertFalse(var.constituent)
        self.assertFalse(var.advected)
        self.assertEqual(var.molar_mass, 0.0)
        self.assertFalse(var.is_constituent)

    def test_negative_molar_mass_rejected(self):
        with self.assertRaises((CCPPError, ParseSyntaxError)):
            self._scheme_var(extra_attrs='molar_mass = -1.0')


########################################################################
# File-based tests (use sample_files/)
########################################################################

class TestParseMetadataFiles(unittest.TestCase):
    """Integration tests using real ``.meta`` files in ``sample_files/``."""

    def _sample(self, name):
        return os.path.join(_SAMPLE_DIR, name)

    # ---- valid files -------------------------------------------------------

    def test_host_simple_file(self):
        tables = parse_metadata_file(self._sample('host_simple.meta'))
        self.assertEqual(len(tables), 1)
        tbl = tables[0]
        self.assertEqual(tbl.table_name, 'physics_data')
        self.assertEqual(tbl.table_type, 'host')
        snames = {v.standard_name for v in tbl.variables()}
        self.assertIn('horizontal_dimension', snames)
        self.assertIn('vertical_layer_dimension', snames)
        # loop bounds and error vars are control vars (control_simple.meta), not host vars
        self.assertNotIn('horizontal_loop_begin', snames)
        self.assertNotIn('horizontal_loop_end', snames)

    def test_control_simple_file(self):
        tables = parse_metadata_file(self._sample('control_simple.meta'))
        self.assertEqual(len(tables), 1)
        self.assertEqual(tables[0].table_type, 'control')

    def test_ddt_simple_file(self):
        tables = parse_metadata_file(self._sample('ddt_simple.meta'))
        self.assertEqual(len(tables), 1)
        tbl = tables[0]
        self.assertEqual(tbl.table_type, 'ddt')
        snames = [v.standard_name for v in tbl.variables()]
        self.assertIn('geopotential_at_interface', snames)
        self.assertIn('geopotential', snames)
        # DDT field variables have no intent
        for var in tbl.variables():
            self.assertIsNone(var.intent)

    def test_scheme_multipart_file(self):
        """Scheme with init, run, final phases from sample file."""
        tables = parse_metadata_file(self._sample('scheme_multipart.meta'))
        self.assertEqual(len(tables), 1)
        tbl = tables[0]
        self.assertTrue(tbl.is_scheme)
        phases = {sec.phase for sec in tbl.sections()}
        self.assertEqual(phases, {'init', 'run', 'final'})
        run_sec = tbl.section_for_phase('run')
        self.assertIsNotNone(run_sec)
        run_stdnames = {v.standard_name for v in run_sec.variables}
        self.assertIn('air_temperature', run_stdnames)

    def test_host_with_ddt_instance_file(self):
        """Host table declaring a DDT instance variable."""
        tables = parse_metadata_file(self._sample('host_with_ddt_instance.meta'))
        self.assertEqual(len(tables), 1)
        var = tables[0].sections()[0].variables[0]
        self.assertEqual(var.local_name, 'gfs_statein')
        self.assertEqual(var.type, 'gfs_statein_type')
        self.assertEqual(var.dimensions, ['number_of_instances'])

    # ---- error files -------------------------------------------------------

    def test_bad_module_type_file(self):
        """``type = module`` must raise with a message mentioning 'host'."""
        with self.assertRaises(CCPPError) as cm:
            parse_metadata_file(self._sample('bad_module_type.meta'))
        self.assertIn('host', str(cm.exception).lower())

    def test_bad_finalize_phase_file(self):
        """``_finalize`` phase must raise with a message mentioning 'final'."""
        with self.assertRaises(CCPPError) as cm:
            parse_metadata_file(self._sample('bad_finalize_phase.meta'))
        self.assertIn('final', str(cm.exception))

    def test_bad_invalid_type_file(self):
        """Completely invalid table type raises CCPPError."""
        with self.assertRaises(CCPPError):
            parse_metadata_file(self._sample('bad_invalid_type.meta'))

    def test_bad_duplicate_stdname_file(self):
        """Duplicate standard name in one section raises CCPPError."""
        with self.assertRaises(CCPPError):
            parse_metadata_file(self._sample('bad_duplicate_stdname.meta'))

    def test_nonexistent_file(self):
        """Parsing a non-existent file raises CCPPError."""
        with self.assertRaises(CCPPError):
            parse_metadata_file('/nonexistent/path/file.meta')


########################################################################
# Variables() de-duplication across scheme phases
########################################################################

class TestTableVariables(unittest.TestCase):
    """Tests for :meth:`MetadataTable.variables` cross-phase de-duplication."""

    def test_dedup_across_phases(self):
        """``ccpp_error_message`` appears in init and run; variables() returns it once."""
        text = """
            [ccpp-table-properties]
              name = sch
              type = scheme

            [ccpp-arg-table]
              name = sch_init
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              intent = out

            [ccpp-arg-table]
              name = sch_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              intent = out
            [ im ]
              standard_name = horizontal_dimension
              units = count
              dimensions = ()
              type = integer
              intent = in
        """
        tables = _parse_text(text)
        all_vars = tables[0].variables()
        snames = [v.standard_name for v in all_vars]
        # ccpp_error_message appears in both phases but should be returned once
        self.assertEqual(snames.count('ccpp_error_message'), 1)
        self.assertIn('horizontal_dimension', snames)


########################################################################
# CLI helper tests
########################################################################

class TestCLIHelpers(unittest.TestCase):
    """Tests for CLI utility functions in ccpp_capgen."""

    def setUp(self):
        import importlib
        import importlib.util
        script = os.path.join(_PKG_ROOT, 'ccpp_capgen.py')
        spec = importlib.util.spec_from_file_location('ccpp_capgen', script)
        self.mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.mod)

    def test_split_file_list(self):
        result = self.mod._split_file_list('a.meta, b.meta, c.meta')
        self.assertEqual(result, ['a.meta', 'b.meta', 'c.meta'])

    def test_split_file_list_empty(self):
        self.assertEqual(self.mod._split_file_list(''), [])

    def test_parse_kind_types_valid_iso_default(self):
        result = self.mod._parse_kind_types(['kind_phys=REAL64', 'kind_dyn=REAL32'])
        self.assertEqual(result, {
            'kind_phys': ('iso_fortran_env', 'REAL64'),
            'kind_dyn':  ('iso_fortran_env', 'REAL32'),
        })

    def test_parse_kind_types_explicit_module(self):
        result = self.mod._parse_kind_types(['kind_phys=my_kinds:kind_r8'])
        self.assertEqual(result, {'kind_phys': ('my_kinds', 'kind_r8')})

    def test_parse_kind_types_mixed(self):
        result = self.mod._parse_kind_types([
            'kind_iso=REAL64',
            'kind_host=my_kinds:kind_r4',
        ])
        self.assertEqual(result, {
            'kind_iso':  ('iso_fortran_env', 'REAL64'),
            'kind_host': ('my_kinds', 'kind_r4'),
        })

    def test_parse_kind_types_malformed(self):
        with self.assertRaises(CCPPError):
            self.mod._parse_kind_types(['bad_entry'])

    def test_parse_kind_types_duplicate(self):
        with self.assertRaises(CCPPError):
            self.mod._parse_kind_types(['kind_phys=REAL64', 'kind_phys=REAL32'])

    def test_parse_kind_types_non_iso_without_module_rejected(self):
        """Bare non-ISO spec must error -- the default module only applies to ISO names."""
        with self.assertRaises(CCPPError) as cm:
            self.mod._parse_kind_types(['kind_phys=kind_r8'])
        self.assertIn('ISO_FORTRAN_ENV', str(cm.exception))

    def test_parse_kind_types_too_many_colons_rejected(self):
        with self.assertRaises(CCPPError):
            self.mod._parse_kind_types(['kind_phys=mod:sub:spec'])

    def test_parse_kind_types_empty_module_rejected(self):
        with self.assertRaises(CCPPError):
            self.mod._parse_kind_types(['kind_phys=:REAL64'])

    # ---- _collect_metadata_kind_specs --------------------------------------

    def _make_table_with_specs(self, file_path, name, specs):
        ctx = ParseContext(0, file_path)
        t   = MetadataTable(name, 'scheme', file_path, ctx)
        t.kind_specs = list(specs)
        return t

    def test_collect_metadata_kind_specs_empty(self):
        self.assertEqual(self.mod._collect_metadata_kind_specs([]), {})

    def test_collect_metadata_kind_specs_single_table(self):
        t = self._make_table_with_specs(
            '/p/a.meta', 'a',
            [('kind_temp', 'temp_kinds', 'temp_r8')],
        )
        self.assertEqual(
            self.mod._collect_metadata_kind_specs([t]),
            {'kind_temp': ('temp_kinds', 'temp_r8')},
        )

    def test_collect_metadata_kind_specs_identical_duplicates_collapsed(self):
        spec = ('kind_temp', 'temp_kinds', 'temp_r8')
        a = self._make_table_with_specs('/p/a.meta', 'a', [spec])
        b = self._make_table_with_specs('/p/b.meta', 'b', [spec])
        self.assertEqual(
            self.mod._collect_metadata_kind_specs([a, b]),
            {'kind_temp': ('temp_kinds', 'temp_r8')},
        )

    def test_collect_metadata_kind_specs_conflict_raises(self):
        a = self._make_table_with_specs(
            '/p/a.meta', 'a',
            [('kind_temp', 'temp_kinds', 'temp_r8')],
        )
        b = self._make_table_with_specs(
            '/p/b.meta', 'b',
            [('kind_temp', 'other_kinds', 'r8')],
        )
        with self.assertRaises(CCPPError) as cm:
            self.mod._collect_metadata_kind_specs([a, b])
        msg = str(cm.exception)
        self.assertIn("kind 'kind_temp'", msg)
        self.assertIn('/p/a.meta', msg)
        self.assertIn('/p/b.meta', msg)

    def test_collect_metadata_kind_specs_multiple_distinct_kinds(self):
        a = self._make_table_with_specs(
            '/p/a.meta', 'a',
            [('kind_temp', 'temp_kinds', 'temp_r8'),
             ('kind_aux',  'aux_kinds',  'aux_r4')],
        )
        self.assertEqual(
            self.mod._collect_metadata_kind_specs([a]),
            {
                'kind_temp': ('temp_kinds', 'temp_r8'),
                'kind_aux':  ('aux_kinds',  'aux_r4'),
            },
        )

    # ---- _merge_cli_and_metadata_kinds -------------------------------------

    def test_merge_cli_and_metadata_no_overlap(self):
        cli  = {'kind_phys': ('iso_fortran_env', 'REAL64')}
        meta = {'kind_temp': ('temp_kinds', 'temp_r8')}
        self.assertEqual(
            self.mod._merge_cli_and_metadata_kinds(cli, meta),
            {
                'kind_phys': ('iso_fortran_env', 'REAL64'),
                'kind_temp': ('temp_kinds',      'temp_r8'),
            },
        )

    def test_merge_cli_and_metadata_identical_collapsed(self):
        cli  = {'kind_temp': ('temp_kinds', 'temp_r8')}
        meta = {'kind_temp': ('temp_kinds', 'temp_r8')}
        self.assertEqual(
            self.mod._merge_cli_and_metadata_kinds(cli, meta),
            {'kind_temp': ('temp_kinds', 'temp_r8')},
        )

    def test_merge_cli_and_metadata_conflict_raises(self):
        cli  = {'kind_temp': ('cli_kinds',  'r8')}
        meta = {'kind_temp': ('meta_kinds', 'r8')}
        with self.assertRaises(CCPPError) as cm:
            self.mod._merge_cli_and_metadata_kinds(cli, meta)
        self.assertIn("kind 'kind_temp'", str(cm.exception).lower()
                      .replace('Kind', 'kind'))
        self.assertIn('cli_kinds',  str(cm.exception))
        self.assertIn('meta_kinds', str(cm.exception))

    def test_merge_then_default_kind_phys_injected_when_neither_provides(self):
        """Default kind_phys is injected after the merge when neither side declares it."""
        import logging
        log = logging.getLogger('ccpp_capgen_test')
        merged = self.mod._merge_cli_and_metadata_kinds({}, {})
        merged = self.mod._ensure_kind_phys_default(merged, log)
        self.assertEqual(
            merged, {'kind_phys': ('iso_fortran_env', 'REAL64')}
        )

    def test_metadata_kind_phys_suppresses_default(self):
        """Metadata declaring kind_phys keeps the default from being injected."""
        import logging
        log    = logging.getLogger('ccpp_capgen_test')
        meta   = {'kind_phys': ('host_kinds', 'kind_r8')}
        merged = self.mod._merge_cli_and_metadata_kinds({}, meta)
        merged = self.mod._ensure_kind_phys_default(merged, log)
        self.assertEqual(merged, {'kind_phys': ('host_kinds', 'kind_r8')})


########################################################################
# Tests: apply_table_props (source_path, dependencies, dependencies_path)
########################################################################

class TestApplyTableProps(unittest.TestCase):
    """Tests for MetadataTable.apply_table_props and parsing of table-level props."""

    def _make_table(self, file_path='/project/src/foo.meta'):
        ctx = ParseContext(0, file_path)
        t = MetadataTable('foo', 'scheme', file_path, ctx)
        return t

    def test_defaults_when_no_props(self):
        t = self._make_table()
        t.apply_table_props({})
        self.assertEqual(t.dependencies, [])
        # source_path defaults to the meta file's directory
        self.assertEqual(t.source_path, os.path.dirname(os.path.abspath(t.file_path)))

    def test_source_path_resolved(self):
        t = self._make_table('/project/src/foo.meta')
        t.apply_table_props({'source_path': 'fortran'})
        self.assertEqual(t.source_path, '/project/src/fortran')

    def test_source_path_with_dotdot(self):
        t = self._make_table('/project/src/foo.meta')
        t.apply_table_props({'source_path': '../other'})
        self.assertEqual(t.source_path, '/project/other')

    def test_single_dependency_no_dep_path(self):
        t = self._make_table('/project/src/foo.meta')
        t.apply_table_props({'dependencies': 'util.F90'})
        self.assertEqual(t.dependencies, ['/project/src/util.F90'])

    def test_multiple_dependencies(self):
        t = self._make_table('/project/src/foo.meta')
        t.apply_table_props({'dependencies': 'a.F90, b.F90'})
        self.assertIn('/project/src/a.F90', t.dependencies)
        self.assertIn('/project/src/b.F90', t.dependencies)
        self.assertEqual(len(t.dependencies), 2)

    def test_dependencies_path_as_base(self):
        t = self._make_table('/project/src/foo.meta')
        t.apply_table_props({'dependencies': 'util.F90', 'dependencies_path': 'lib'})
        self.assertEqual(t.dependencies, ['/project/src/lib/util.F90'])

    def test_dependencies_none_string(self):
        t = self._make_table('/project/src/foo.meta')
        t.apply_table_props({'dependencies': 'none'})
        self.assertEqual(t.dependencies, [])

    def test_dependency_relative_dotdot(self):
        t = self._make_table('/project/src/foo.meta')
        t.apply_table_props({'dependencies': '../../shared/helper.F90'})
        self.assertEqual(t.dependencies, ['/shared/helper.F90'])

    def test_dependencies_list_form_accumulates(self):
        """When ``dependencies`` appears more than once in a single
        table header, the parser passes a list to apply_table_props;
        each entry can itself be a comma-separated list of paths."""
        t = self._make_table('/project/src/foo.meta')
        t.apply_table_props({'dependencies': [
            'a.F90',
            'b.F90, c.F90',
            'sub/d.F90',
        ]})
        self.assertEqual(t.dependencies, [
            '/project/src/a.F90',
            '/project/src/b.F90',
            '/project/src/c.F90',
            '/project/src/sub/d.F90',
        ])

    def test_dependencies_list_form_honors_dep_path(self):
        """``dependencies_path`` applies to every entry in a list,
        whether the entry is a single path or a comma-separated
        bundle."""
        t = self._make_table('/project/src/foo.meta')
        t.apply_table_props({
            'dependencies_path': '../../',
            'dependencies': [
                'tools/mpiutil.F90',
                'Radiation/RRTMG/radlw_main.F90,Radiation/RRTMG/radsw_main.F90',
            ],
        })
        self.assertEqual(t.dependencies, [
            '/tools/mpiutil.F90',
            '/Radiation/RRTMG/radlw_main.F90',
            '/Radiation/RRTMG/radsw_main.F90',
        ])

    def test_dependencies_list_form_with_none_skip(self):
        """A ``none`` entry inside a list is silently skipped (matches
        the single-string ``none`` shorthand)."""
        t = self._make_table('/project/src/foo.meta')
        t.apply_table_props({'dependencies': [
            'a.F90',
            'none',
            'b.F90',
        ]})
        self.assertEqual(t.dependencies, [
            '/project/src/a.F90',
            '/project/src/b.F90',
        ])

    def test_unrecognised_props_silently_ignored(self):
        t = self._make_table()
        t.apply_table_props({'unknown_key': 'value'})
        self.assertEqual(t.dependencies, [])
        self.assertEqual(t.kind_specs, [])

    def test_kind_spec_explicit_form(self):
        t = self._make_table()
        t.apply_table_props({'kind_spec': 'temp_kinds:kind_temp=>temp_r8'})
        self.assertEqual(
            t.kind_specs, [('kind_temp', 'temp_kinds', 'temp_r8')]
        )

    def test_kind_spec_shorthand_form(self):
        t = self._make_table()
        t.apply_table_props({'kind_spec': 'host_kinds:kind_r8'})
        self.assertEqual(
            t.kind_specs, [('kind_r8', 'host_kinds', 'kind_r8')]
        )

    def test_kind_spec_list_accumulates(self):
        t = self._make_table()
        t.apply_table_props({'kind_spec': [
            'temp_kinds:kind_temp=>temp_r8',
            'host_kinds:kind_r4',
        ]})
        self.assertEqual(t.kind_specs, [
            ('kind_temp', 'temp_kinds', 'temp_r8'),
            ('kind_r4',   'host_kinds', 'kind_r4'),
        ])

    def test_kind_spec_malformed_raises(self):
        t = self._make_table()
        with self.assertRaises(CCPPError):
            t.apply_table_props({'kind_spec': 'real8'})

    def test_kind_spec_extra_arrow_segment_rejected(self):
        t = self._make_table()
        with self.assertRaises(CCPPError):
            t.apply_table_props(
                {'kind_spec': 'mod:a=>b=>c'}
            )

    def test_module_name_default_empty(self):
        t = self._make_table()
        t.apply_table_props({})
        self.assertEqual(t.module_name, '')

    def test_module_name_explicit(self):
        """``module_name`` override for cases where the Fortran module
        name differs from the metadata table name (e.g. ``effr_pre`` table
        whose Fortran module is ``mod_effr_pre``)."""
        t = self._make_table()
        t.apply_table_props({'module_name': 'mod_effr_pre'})
        self.assertEqual(t.module_name, 'mod_effr_pre')

    def test_module_name_whitespace_stripped(self):
        t = self._make_table()
        t.apply_table_props({'module_name': '  mod_foo  '})
        self.assertEqual(t.module_name, 'mod_foo')

    def test_module_name_empty_string_keeps_default(self):
        t = self._make_table()
        t.apply_table_props({'module_name': '   '})
        self.assertEqual(t.module_name, '')


class TestTablePropsParseIntegration(unittest.TestCase):
    """Verify that source_path/dependencies are parsed from actual meta text."""

    def _parse(self, src, fname='/project/src/my_scheme.meta'):
        return _parse_lines(src.splitlines(keepends=True), fname)

    def test_source_path_parsed(self):
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = my_scheme
              type = scheme
              source_path = fortran
            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
        """)
        tbls = self._parse(src)
        self.assertEqual(len(tbls), 1)
        self.assertEqual(tbls[0].source_path, '/project/src/fortran')

    def test_dependencies_parsed(self):
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = my_scheme
              type = scheme
              dependencies = util.F90, helper.F90
            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
        """)
        tbls = self._parse(src)
        self.assertIn('/project/src/util.F90', tbls[0].dependencies)
        self.assertIn('/project/src/helper.F90', tbls[0].dependencies)

    def test_dependencies_path_applied(self):
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = my_scheme
              type = scheme
              dependencies = qux.F90
              dependencies_path = adjust
            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
        """)
        tbls = self._parse(src)
        self.assertEqual(tbls[0].dependencies, ['/project/src/adjust/qux.F90'])

    def test_dependencies_repeated_in_header(self):
        """``dependencies`` may appear multiple times in a single table
        header — each line contributes its (comma-separated) paths.
        Real-world example from CCPP physics: dependencies_path = ../../
        followed by ~7 dependencies lines that each list a comma-
        separated bundle in a different subtree.
        """
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = GFS_rrtmg_setup
              type = scheme
              dependencies_path = ../../
              dependencies = tools/mpiutil.F90
              dependencies = hooks/machine.F
              dependencies = Radiation/radiation_aerosols.f
              dependencies = Radiation/radiation_astronomy.f, Radiation/radiation_clouds.f, Radiation/radiation_gases.f
              dependencies = Radiation/RRTMG/radlw_main.F90,Radiation/RRTMG/radlw_param.f,Radiation/RRTMG/radsw_main.F90,Radiation/RRTMG/radsw_param.f
            [ccpp-arg-table]
              name = GFS_rrtmg_setup_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
        """)
        tbls = self._parse(src, fname='/project/physics/src/GFS_rrtmg_setup.meta')
        self.assertEqual(len(tbls), 1)
        # dependencies_path = ../../  → base is /project/
        self.assertEqual(tbls[0].dependencies, [
            '/project/tools/mpiutil.F90',
            '/project/hooks/machine.F',
            '/project/Radiation/radiation_aerosols.f',
            '/project/Radiation/radiation_astronomy.f',
            '/project/Radiation/radiation_clouds.f',
            '/project/Radiation/radiation_gases.f',
            '/project/Radiation/RRTMG/radlw_main.F90',
            '/project/Radiation/RRTMG/radlw_param.f',
            '/project/Radiation/RRTMG/radsw_main.F90',
            '/project/Radiation/RRTMG/radsw_param.f',
        ])

    def test_dependencies_path_still_rejects_duplicates(self):
        """Even with ``dependencies`` now accumulating, the
        ``dependencies_path`` attribute itself remains single-valued —
        repeating it is a metadata error."""
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = my_scheme
              type = scheme
              dependencies_path = ../
              dependencies_path = ../../
            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
        """)
        with self.assertRaises(CCPPError):
            self._parse(src)

    def test_source_path_still_rejects_duplicates(self):
        """``source_path`` is single-valued too."""
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = my_scheme
              type = scheme
              source_path = fortran
              source_path = src
            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
        """)
        with self.assertRaises(CCPPError):
            self._parse(src)

    def test_kind_spec_single_line_parsed(self):
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = my_scheme
              type = scheme
              kind_spec = temp_kinds:kind_temp=>temp_r8
            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
        """)
        tbls = self._parse(src)
        self.assertEqual(
            tbls[0].kind_specs,
            [('kind_temp', 'temp_kinds', 'temp_r8')],
        )

    def test_kind_spec_multiple_lines_accumulate(self):
        """Repeat ``kind_spec`` lines accumulate without a duplicate-key error."""
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = my_scheme
              type = scheme
              kind_spec = temp_kinds:kind_temp=>temp_r8
              kind_spec = host_kinds:kind_r4
            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
        """)
        tbls = self._parse(src)
        self.assertEqual(tbls[0].kind_specs, [
            ('kind_temp', 'temp_kinds', 'temp_r8'),
            ('kind_r4',   'host_kinds', 'kind_r4'),
        ])

    def test_kind_spec_malformed_value_raises(self):
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = my_scheme
              type = scheme
              kind_spec = not_a_kind_spec
            [ccpp-arg-table]
              name = my_scheme_run
              type = scheme
            [ errmsg ]
              standard_name = ccpp_error_message
              units = none
              dimensions = ()
              type = character
              kind = len=512
              intent = out
        """)
        with self.assertRaises(CCPPError):
            self._parse(src)

    def test_props_at_eof_no_arg_table(self):
        """source_path parsed even if there are no [ccpp-arg-table] sections."""
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = my_scheme
              type = scheme
              source_path = src
        """)
        tbls = self._parse(src)
        self.assertEqual(len(tbls), 1)
        self.assertEqual(tbls[0].source_path, '/project/src/src')

    def test_multiple_tables_independent_deps(self):
        src = textwrap.dedent("""\
            [ccpp-table-properties]
              name = scheme_a
              type = scheme
              dependencies = a.F90
            [ccpp-table-properties]
              name = scheme_b
              type = scheme
              dependencies = b.F90
        """)
        tbls = self._parse(src)
        self.assertEqual(len(tbls), 2)
        self.assertEqual(tbls[0].dependencies, ['/project/src/a.F90'])
        self.assertEqual(tbls[1].dependencies, ['/project/src/b.F90'])


########################################################################
# Doctest loader
########################################################################

def load_tests(loader, tests, ignore):
    """Auto-discover and run all doctests in the metadata subpackage."""
    import doctest
    import metadata.metadata_table as _mt
    import metadata.parse_tools.parse_source as _ps
    import metadata.parse_tools.parse_log as _pl
    tests.addTests(doctest.DocTestSuite(_mt))
    tests.addTests(doctest.DocTestSuite(_ps))
    tests.addTests(doctest.DocTestSuite(_pl))
    return tests


########################################################################

if __name__ == '__main__':
    unittest.main()
