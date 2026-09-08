"""Tests for Phase 1 validation: required control variables and forbidden dimensions.

The generator must reject host metadata that is missing required control
variables, has them in the wrong table type, declares the wrong Fortran type,
or declares them as non-scalars.  All errors in a single run must be reported
together so the host developer can fix them in one pass.

It must also reject any metadata (host or scheme) that uses
horizontal_loop_begin, horizontal_loop_end, or horizontal_loop_extent as a
variable dimension — these are control scalars and their use as dimensions
indicates a porting error from the legacy toolchain.
"""

import os
import sys
import tempfile
import types
import unittest

_TESTS_DIR  = os.path.dirname(os.path.abspath(__file__))
_REPO_ROOT  = os.path.dirname(_TESTS_DIR)
_CAPGEN_DIR = os.path.join(_REPO_ROOT, 'capgen')
if _CAPGEN_DIR not in sys.path:
    sys.path.insert(0, _CAPGEN_DIR)

from ccpp_capgen import _validate_required_control_vars, _check_no_loop_dimensions
from metadata.parse_tools import CCPPError
from metadata.variable_resolver import build_flat_host_dict, HostVarEntry
from metadata.metadata_table import parse_metadata_file


_SAMPLES_DIR  = os.path.join(_TESTS_DIR, 'sample_files')
_SUITE_DIR    = os.path.join(_TESTS_DIR, 'sample_suite_files')


def _sf(name):
    return os.path.join(_SAMPLES_DIR, name)


def _build_host_dict(host_files, control_files, ddt_files=None):
    """Parse metadata and build a flat host dict from the given files."""
    host_tables    = []
    control_tables = []
    ddt_tables     = []
    for f in host_files:
        for tbl in parse_metadata_file(f):
            if tbl.table_type == 'host':
                host_tables.append(tbl)
            elif tbl.table_type == 'ddt':
                ddt_tables.append(tbl)
    for f in control_files:
        for tbl in parse_metadata_file(f):
            if tbl.table_type == 'control':
                control_tables.append(tbl)
    for f in (ddt_files or []):
        for tbl in parse_metadata_file(f):
            if tbl.table_type == 'ddt':
                ddt_tables.append(tbl)
    return build_flat_host_dict(host_tables, control_tables, ddt_tables)


# ---------------------------------------------------------------------------
# Unit tests for _validate_required_control_vars
# ---------------------------------------------------------------------------

class TestMissingControlVars(unittest.TestCase):
    """All required vars missing except suite_name → 6 errors collected.

    Neither pair member is in the required list: ``instance_number`` /
    ``number_of_instances`` and ``thread_number`` / ``number_of_threads`` are
    both paired-optional opt-ins, so their absence does NOT raise here.
    """

    def setUp(self):
        self._host_dict = _build_host_dict(
            host_files=[_sf('host_simple.meta')],
            control_files=[_sf('bad_ctrl_missing_vars.meta')],
        )

    def test_raises_ccpp_error(self):
        with self.assertRaises(CCPPError):
            _validate_required_control_vars('test_host', self._host_dict)

    def test_all_missing_vars_reported(self):
        """All 6 missing required standard names appear in the error message.

        ``thread_number`` and ``number_of_threads`` are intentionally absent
        from this list: they form a paired-optional control pair (symmetric
        with the instance pair), not required vars, so their omission is not
        reported as a missing required var."""
        missing = [
            'group_name',
            'horizontal_loop_begin', 'horizontal_loop_end',
            'number_of_physics_threads',
            'ccpp_error_code', 'ccpp_error_message',
        ]
        try:
            _validate_required_control_vars('test_host', self._host_dict)
            self.fail("Expected CCPPError")
        except CCPPError as exc:
            msg = str(exc)
            for name in missing:
                self.assertIn(name, msg,
                    "Expected '{}' to appear in error message".format(name))

    def test_instance_number_not_reported_missing(self):
        """instance_number is paired-optional; absence is not an error here."""
        try:
            _validate_required_control_vars('test_host', self._host_dict)
        except CCPPError as exc:
            msg = str(exc)
            # The error block lists missing REQUIRED vars; instance_number
            # must not be flagged as missing on its own.
            self.assertNotIn("'instance_number' not found", msg)

    def test_suite_name_not_in_error(self):
        """suite_name is present and must NOT appear as a missing variable."""
        try:
            _validate_required_control_vars('test_host', self._host_dict)
            self.fail("Expected CCPPError")
        except CCPPError as exc:
            # "suite_name" may appear in context text but should not be
            # reported as missing (it is present).
            msg = str(exc)
            self.assertNotIn("'suite_name' not found", msg)


class TestWrongFortranType(unittest.TestCase):
    """horizontal_loop_begin declared as real instead of integer."""

    def setUp(self):
        self._host_dict = _build_host_dict(
            host_files=[_sf('host_simple.meta')],
            control_files=[_sf('bad_ctrl_wrong_type.meta')],
        )

    def test_raises_ccpp_error(self):
        with self.assertRaises(CCPPError):
            _validate_required_control_vars('test_host', self._host_dict)

    def test_wrong_type_reported(self):
        try:
            _validate_required_control_vars('test_host', self._host_dict)
            self.fail("Expected CCPPError")
        except CCPPError as exc:
            msg = str(exc)
            self.assertIn('horizontal_loop_begin', msg)
            self.assertIn('real', msg.lower())
            self.assertIn('integer', msg.lower())


class TestNonscalarControlVar(unittest.TestCase):
    """ccpp_error_code declared with dimensions instead of being a scalar."""

    def setUp(self):
        self._host_dict = _build_host_dict(
            host_files=[_sf('host_simple.meta')],
            control_files=[_sf('bad_ctrl_nonscalar.meta')],
        )

    def test_raises_ccpp_error(self):
        with self.assertRaises(CCPPError):
            _validate_required_control_vars('test_host', self._host_dict)

    def test_nonscalar_reported(self):
        try:
            _validate_required_control_vars('test_host', self._host_dict)
            self.fail("Expected CCPPError")
        except CCPPError as exc:
            msg = str(exc)
            self.assertIn('ccpp_error_code', msg)
            self.assertIn('scalar', msg.lower())


class TestControlVarInHostTable(unittest.TestCase):
    """suite_name declared in a type=host table instead of type=control."""

    def setUp(self):
        self._host_dict = _build_host_dict(
            host_files=[_sf('bad_ctrl_in_host_table.meta')],
            control_files=[_sf('bad_ctrl_missing_suite_name.meta')],
        )

    def test_raises_ccpp_error(self):
        with self.assertRaises(CCPPError):
            _validate_required_control_vars('test_host', self._host_dict)

    def test_wrong_table_reported(self):
        try:
            _validate_required_control_vars('test_host', self._host_dict)
            self.fail("Expected CCPPError")
        except CCPPError as exc:
            msg = str(exc)
            self.assertIn('suite_name', msg)
            self.assertIn('type=control', msg)


class TestValidControlVars(unittest.TestCase):
    """Full, correct control table passes without error."""

    def test_full_control_table_passes(self):
        host_dict = _build_host_dict(
            host_files=[_sf('host_simple.meta')],
            control_files=[_sf('control_full.meta')],
        )
        # Must not raise.
        _validate_required_control_vars('test_host', host_dict)

    def test_control_simple_passes(self):
        host_dict = _build_host_dict(
            host_files=[_sf('host_simple.meta')],
            control_files=[_sf('control_simple.meta')],
        )
        _validate_required_control_vars('test_host', host_dict)

    def test_no_instance_pair_passes(self):
        """Host omitting both instance_number AND number_of_instances passes
        — the multi-instance API is opt-in."""
        host_dict = _build_host_dict(
            host_files=[_sf('host_no_instance.meta')],
            control_files=[_sf('control_no_instance.meta')],
        )
        _validate_required_control_vars('test_host', host_dict)
        self.assertNotIn('instance_number', host_dict)
        self.assertNotIn('number_of_instances', host_dict)

    def test_no_thread_pair_passes(self):
        """Host omitting BOTH thread_number AND number_of_threads passes —
        the multi-threading API is opt-in, symmetric with the instance pair."""
        host_dict = _build_host_dict(
            host_files=[_sf('host_simple.meta')],
            control_files=[_sf('control_full.meta')],
        )
        host_dict.pop('thread_number', None)
        host_dict.pop('number_of_threads', None)
        # Must not raise even though the thread pair is absent.
        _validate_required_control_vars('test_host', host_dict)
        self.assertNotIn('thread_number', host_dict)
        self.assertNotIn('number_of_threads', host_dict)


class TestInstanceNumberPairing(unittest.TestCase):
    """instance_number and number_of_instances both live in type=control
    and are paired-optional: declaring exactly one is an error.
    """

    def test_instance_alone_raises(self):
        """control declares instance_number but not number_of_instances."""
        host_dict = _build_host_dict(
            host_files=[_sf('host_no_instance.meta')],
            control_files=[_sf('control_inst_only.meta')],
        )
        with self.assertRaises(CCPPError) as ctx:
            _validate_required_control_vars('test_host', host_dict)
        msg = str(ctx.exception)
        self.assertIn('instance_number', msg)
        self.assertIn('number_of_instances', msg)
        self.assertIn('paired', msg.lower())

    def test_ninstances_alone_raises(self):
        """control declares number_of_instances but not instance_number."""
        host_dict = _build_host_dict(
            host_files=[_sf('host_no_instance.meta')],
            control_files=[_sf('control_ninst_only.meta')],
        )
        with self.assertRaises(CCPPError) as ctx:
            _validate_required_control_vars('test_host', host_dict)
        msg = str(ctx.exception)
        self.assertIn('instance_number', msg)
        self.assertIn('number_of_instances', msg)
        self.assertIn('paired', msg.lower())


class TestThreadNumberPairing(unittest.TestCase):
    """thread_number and number_of_threads are a paired-optional control pair,
    fully symmetric with the instance pair: declaring exactly one is an error.

    These manipulate a parsed host_dict directly (rather than carrying extra
    sample .meta files) since the only thing under test is the XOR check.
    """

    def _full_host_dict(self):
        return _build_host_dict(
            host_files=[_sf('host_simple.meta')],
            control_files=[_sf('control_full.meta')],
        )

    def test_thread_number_alone_raises(self):
        """control declares thread_number but not number_of_threads."""
        host_dict = self._full_host_dict()
        host_dict.pop('number_of_threads', None)
        with self.assertRaises(CCPPError) as ctx:
            _validate_required_control_vars('test_host', host_dict)
        msg = str(ctx.exception)
        self.assertIn('thread_number', msg)
        self.assertIn('number_of_threads', msg)
        self.assertIn('paired', msg.lower())

    def test_number_of_threads_alone_raises(self):
        """control declares number_of_threads but not thread_number."""
        host_dict = self._full_host_dict()
        host_dict.pop('thread_number', None)
        with self.assertRaises(CCPPError) as ctx:
            _validate_required_control_vars('test_host', host_dict)
        msg = str(ctx.exception)
        self.assertIn('thread_number', msg)
        self.assertIn('number_of_threads', msg)
        self.assertIn('paired', msg.lower())


class TestControlAllowlist(unittest.TestCase):
    """A type=control table may declare ONLY the known framework control
    variables (the required set plus the paired-optional pair members).
    Any other variable in a type=control table is a hard error."""

    def _full_host_dict(self):
        return _build_host_dict(
            host_files=[_sf('host_simple.meta')],
            control_files=[_sf('control_full.meta')],
        )

    def test_unknown_control_var_rejected(self):
        host_dict = self._full_host_dict()
        host_dict['some_random_host_quantity'] = types.SimpleNamespace(
            is_control=True)
        with self.assertRaises(CCPPError) as ctx:
            _validate_required_control_vars('test_host', host_dict)
        msg = str(ctx.exception)
        self.assertIn('some_random_host_quantity', msg)
        self.assertIn('type=host', msg)

    def test_unknown_host_table_var_not_flagged(self):
        """A non-control (type=host) variable with an unknown name is fine —
        the allowlist only governs type=control declarations."""
        host_dict = self._full_host_dict()
        host_dict['some_random_host_quantity'] = types.SimpleNamespace(
            is_control=False)
        # Must not raise.
        _validate_required_control_vars('test_host', host_dict)

# ---------------------------------------------------------------------------
# Tests for forbidden dimension names
# ---------------------------------------------------------------------------

class TestForbiddenDimensions(unittest.TestCase):
    """horizontal_loop_extent/begin/end must not appear in any dimension attribute."""

    def _tables_from(self, fname):
        return parse_metadata_file(_sf(fname))

    def test_loop_extent_in_host_table_raises(self):
        tables = self._tables_from('bad_dim_loop_extent.meta')
        with self.assertRaises(CCPPError):
            _check_no_loop_dimensions(tables)

    def test_loop_extent_error_names_variable(self):
        tables = self._tables_from('bad_dim_loop_extent.meta')
        try:
            _check_no_loop_dimensions(tables)
            self.fail("Expected CCPPError")
        except CCPPError as exc:
            msg = str(exc)
            self.assertIn('horizontal_loop_extent', msg)
            self.assertIn('some_data', msg)

    def test_loop_begin_in_scheme_table_raises(self):
        tables = self._tables_from('bad_dim_loop_begin.meta')
        with self.assertRaises(CCPPError):
            _check_no_loop_dimensions(tables)

    def test_loop_begin_error_names_variable(self):
        tables = self._tables_from('bad_dim_loop_begin.meta')
        try:
            _check_no_loop_dimensions(tables)
            self.fail("Expected CCPPError")
        except CCPPError as exc:
            msg = str(exc)
            self.assertIn('horizontal_loop_begin', msg)
            self.assertIn('some_data', msg)

    def test_all_three_forbidden_names_detected(self):
        """Verify all three names are in the forbidden set."""
        from ccpp_capgen import _FORBIDDEN_DIMENSION_NAMES
        self.assertIn('horizontal_loop_extent', _FORBIDDEN_DIMENSION_NAMES)
        self.assertIn('horizontal_loop_begin',  _FORBIDDEN_DIMENSION_NAMES)
        self.assertIn('horizontal_loop_end',    _FORBIDDEN_DIMENSION_NAMES)

    def test_clean_host_table_passes(self):
        tables = self._tables_from('host_simple.meta')
        _check_no_loop_dimensions(tables)

    def test_clean_scheme_table_passes(self):
        tables = self._tables_from('scheme_multipart.meta')
        _check_no_loop_dimensions(tables)

    def test_multiple_violations_reported_together(self):
        """A file with two forbidden dimensions produces one error listing both."""
        tables = (
            self._tables_from('bad_dim_loop_extent.meta') +
            self._tables_from('bad_dim_loop_begin.meta')
        )
        try:
            _check_no_loop_dimensions(tables)
            self.fail("Expected CCPPError")
        except CCPPError as exc:
            msg = str(exc)
            self.assertIn('horizontal_loop_extent', msg)
            self.assertIn('horizontal_loop_begin',  msg)


if __name__ == '__main__':
    unittest.main()
