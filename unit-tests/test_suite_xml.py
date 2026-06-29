#!/usr/bin/env python3

"""Unit tests for :mod:`generator.suite_xml`.

Tests cover:

1. The in-memory data model: :class:`~generator.suite_xml.SuiteScheme`,
   :class:`~generator.suite_xml.SuiteSubcycle`,
   :class:`~generator.suite_xml.SuiteSubcol`,
   :class:`~generator.suite_xml.SuiteGroup`,
   :class:`~generator.suite_xml.Suite`.
2. The XML-to-object builder :func:`~generator.suite_xml._build_suite`.
3. The public API :func:`~generator.suite_xml.parse_suite_xml` — including
   nested-suite expansion, expanded-XML writing, and error detection.

The test cases for parsing and expansion are ported from
``test/unit_tests/test_sdf.py`` in the legacy capgen test suite, updated
for the new package layout.

Run with::

    python -m pytest capgen/tests/test_suite_xml.py -v

or include ``--doctest-modules capgen/generator/suite_xml.py``.
"""

import filecmp
import glob
import logging
import os
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET

# ---- path setup ------------------------------------------------------------
_TESTS_DIR  = os.path.dirname(os.path.abspath(__file__))
_REPO_ROOT  = os.path.dirname(_TESTS_DIR)
_PKG_ROOT   = os.path.join(_REPO_ROOT, 'capgen')
for _p in (_PKG_ROOT, _REPO_ROOT):
    if _p not in sys.path:
        sys.path.insert(0, _p)

# ---- imports ----------------------------------------------------------------
from metadata.parse_tools import (
    CCPPError,
    init_log,
    set_log_to_null,
    read_xml_file,
    find_schema_version,
    expand_nested_suites,
    write_xml_file,
)
from metadata.parse_tools.xml_tools import validate_xml_file
from generator.suite_xml import (
    Suite,
    SuiteGroup,
    SuiteScheme,
    SuiteSubcol,
    SuiteSubcycle,
    _build_suite,
    _parse_group_items,
    parse_suite_xml,
)

_SAMPLE_DIR = os.path.join(_TESTS_DIR, 'sample_suite_files')
_SCHEMA_DIR = os.path.join(_PKG_ROOT, 'schema')


########################################################################
# Helper — shared logger (quiet by default)
########################################################################

def _make_logger(name='test_suite_xml'):
    log = init_log(name, level=logging.WARNING)
    set_log_to_null(log)
    return log


########################################################################
# Helper — XML tree comparison (ported from test_sdf.py)
########################################################################

def _compare_text(name, txt1, txt2, typ):
    """Return an error string if the text items differ, else None."""
    if txt1 and txt2:
        if txt1.strip() != txt2.strip():
            return f"{name} {typ}, '{txt1}', does not match {typ}, '{txt2}'"
    elif txt1:
        return f"{name} {typ} is missing from string2"
    elif txt2:
        return f"{name} {typ} is missing from string1"
    return None


def xml_diff(xt1, xt2):
    """Return a list of difference strings between two ElementTree subtrees.

    Returns an empty list when the trees are identical.
    """
    diffs = []
    if xt1.tag != xt2.tag:
        diffs.append(f"Tags do not match: {xt1.tag} != {xt2.tag}")
        return diffs
    for name, value in xt1.attrib.items():
        if name not in xt2.attrib:
            diffs.append(f"xt1 attribute, {name}, is missing in xt2")
        elif xt2.attrib[name] != value:
            diffs.append(f"Attributes for {name} do not match: "
                         f"{value!r} != {xt2.attrib[name]!r}")
    for name in xt2.attrib:
        if name not in xt1.attrib:
            diffs.append(f"xt2 attribute, {name}, is missing in xt1")
    tdiff = _compare_text(xt1.tag, xt1.text, xt2.text, "text")
    if tdiff:
        diffs.append(tdiff)
    tdiff = _compare_text(xt1.tag, xt1.tail, xt2.tail, "tail")
    if tdiff:
        diffs.append(tdiff)
    if len(xt1) != len(xt2):
        diffs.append(f"Number of children differs: {len(xt1)} != {len(xt2)}")
    else:
        for c1, c2 in zip(xt1, xt2):
            diffs.extend(xml_diff(c1, c2))
    return diffs


########################################################################
# Data model tests
########################################################################

class TestSuiteScheme(unittest.TestCase):
    """Tests for :class:`SuiteScheme`."""

    def test_creation(self):
        s = SuiteScheme('my_scheme')
        self.assertEqual(s.name, 'my_scheme')

    def test_leading_trailing_spaces_stripped(self):
        s = SuiteScheme('  spaced  ')
        self.assertEqual(s.name, 'spaced')

    def test_scheme_names(self):
        self.assertEqual(SuiteScheme('foo').scheme_names(), ['foo'])

    def test_repr(self):
        self.assertIn('my_scheme', repr(SuiteScheme('my_scheme')))


class TestSuiteSubcycle(unittest.TestCase):
    """Tests for :class:`SuiteSubcycle`."""

    def test_literal_integer_loop(self):
        subcycle = SuiteSubcycle(loop='2', items=[])
        self.assertEqual(subcycle.loop, '2')
        self.assertTrue(subcycle.is_literal_count)

    def test_stdname_loop(self):
        subcycle = SuiteSubcycle(loop='num_subcycles_for_ag', items=[])
        self.assertFalse(subcycle.is_literal_count)

    def test_none_loop_is_literal(self):
        subcycle = SuiteSubcycle(loop=None, items=[])
        self.assertIsNone(subcycle.loop)
        self.assertTrue(subcycle.is_literal_count)

    def test_scheme_names_from_items(self):
        subcycle = SuiteSubcycle(loop='2', items=[
            SuiteScheme('scheme_a'),
            SuiteScheme('scheme_b'),
        ])
        self.assertEqual(subcycle.scheme_names(), ['scheme_a', 'scheme_b'])

    def test_nested_subcycle_scheme_names(self):
        inner = SuiteSubcycle(loop='3', items=[SuiteScheme('inner_sch')])
        outer = SuiteSubcycle(loop='2', items=[SuiteScheme('outer_sch'), inner])
        self.assertEqual(outer.scheme_names(), ['outer_sch', 'inner_sch'])


class TestSuiteSubcol(unittest.TestCase):
    """Tests for :class:`SuiteSubcol`."""

    def test_creation(self):
        subcycle = SuiteSubcol('gen_routine', 'avg_routine', [SuiteScheme('sch')])
        self.assertEqual(subcycle.gen_routine, 'gen_routine')
        self.assertEqual(subcycle.avg_routine, 'avg_routine')

    def test_scheme_names(self):
        subcycle = SuiteSubcol('g', 'a', [SuiteScheme('s1'), SuiteScheme('s2')])
        self.assertEqual(subcycle.scheme_names(), ['s1', 's2'])


class TestSuiteGroup(unittest.TestCase):
    """Tests for :class:`SuiteGroup`."""

    def test_creation(self):
        g = SuiteGroup('physics', [SuiteScheme('sch1'), SuiteScheme('sch2')])
        self.assertEqual(g.name, 'physics')
        self.assertEqual(len(g.items), 2)

    def test_scheme_names(self):
        g = SuiteGroup('g', [
            SuiteScheme('a'),
            SuiteSubcycle('2', [SuiteScheme('b'), SuiteScheme('a')]),
        ])
        self.assertEqual(g.scheme_names(), ['a', 'b', 'a'])

    def test_unique_scheme_names(self):
        g = SuiteGroup('g', [
            SuiteScheme('a'),
            SuiteSubcycle('2', [SuiteScheme('b'), SuiteScheme('a')]),
        ])
        self.assertEqual(g.unique_scheme_names(), ['a', 'b'])


class TestSuite(unittest.TestCase):
    """Tests for :class:`Suite`."""

    def _make_suite(self, groups=None, init=None, final=None):
        groups = groups or [SuiteGroup('g', [SuiteScheme('s')])]
        return Suite('my_suite', [2, 0], '/f.xml', groups, init, final)

    def test_name(self):
        self.assertEqual(self._make_suite().name, 'my_suite')

    def test_group_names(self):
        s = self._make_suite([
            SuiteGroup('g1', [SuiteScheme('s1')]),
            SuiteGroup('g2', [SuiteScheme('s2')]),
        ])
        self.assertEqual(s.group_names(), ['g1', 'g2'])

    def test_get_group_found(self):
        grp = SuiteGroup('dynamics', [SuiteScheme('dyn')])
        s = self._make_suite([grp])
        self.assertIs(s.get_group('dynamics'), grp)

    def test_get_group_not_found(self):
        self.assertIsNone(self._make_suite().get_group('nope'))

    def test_all_scheme_names_deduped(self):
        s = self._make_suite([
            SuiteGroup('g1', [SuiteScheme('common'), SuiteScheme('a')]),
            SuiteGroup('g2', [SuiteScheme('b'), SuiteScheme('common')]),
        ])
        self.assertEqual(s.all_scheme_names(), ['common', 'a', 'b'])

    def test_init_final_schemes(self):
        s = self._make_suite(init='suite_init', final='suite_final')
        self.assertEqual(s.init_scheme, 'suite_init')
        self.assertEqual(s.final_scheme, 'suite_final')

    def test_expanded_file_default_none(self):
        self.assertIsNone(self._make_suite().expanded_file)


########################################################################
# _parse_group_items tests
########################################################################

class TestParseGroupItems(unittest.TestCase):
    """Tests for :func:`_parse_group_items`."""

    def test_single_scheme(self):
        xml = '<group name="g"><scheme>my_scheme</scheme></group>'
        el = ET.fromstring(xml)
        items = _parse_group_items(el)
        self.assertEqual(len(items), 1)
        self.assertIsInstance(items[0], SuiteScheme)
        self.assertEqual(items[0].name, 'my_scheme')

    def test_subcycle_with_loop(self):
        xml = '<group name="g"><subcycle loop="3"><scheme>s</scheme></subcycle></group>'
        el = ET.fromstring(xml)
        items = _parse_group_items(el)
        self.assertEqual(len(items), 1)
        subcycle = items[0]
        self.assertIsInstance(subcycle, SuiteSubcycle)
        self.assertEqual(subcycle.loop, '3')
        self.assertEqual(len(subcycle.items), 1)

    def test_subcol(self):
        xml = ('<group name="g">'
               '<subcol gen="gen_r" avg="avg_r">'
               '<scheme>s</scheme></subcol></group>')
        el = ET.fromstring(xml)
        items = _parse_group_items(el)
        self.assertIsInstance(items[0], SuiteSubcol)
        self.assertEqual(items[0].gen_routine, 'gen_r')

    def test_subcol_missing_avg_raises(self):
        xml = ('<group name="g">'
               '<subcol gen="gen_r">'
               '<scheme>s</scheme></subcol></group>')
        el = ET.fromstring(xml)
        with self.assertRaises(CCPPError):
            _parse_group_items(el)

    def test_empty_scheme_raises(self):
        xml = '<group name="g"><scheme>  </scheme></group>'
        el = ET.fromstring(xml)
        with self.assertRaises(CCPPError):
            _parse_group_items(el)

    def test_nested_subcycles(self):
        xml = '''<group name="g">
          <subcycle loop="2">
            <subcycle loop="3">
              <scheme>inner</scheme>
            </subcycle>
          </subcycle>
        </group>'''
        el = ET.fromstring(xml)
        items = _parse_group_items(el)
        outer = items[0]
        self.assertIsInstance(outer, SuiteSubcycle)
        inner = outer.items[0]
        self.assertIsInstance(inner, SuiteSubcycle)
        self.assertEqual(inner.loop, '3')
        self.assertEqual(inner.items[0].name, 'inner')


########################################################################
# _build_suite tests
########################################################################

class TestBuildSuite(unittest.TestCase):
    """Tests for :func:`_build_suite`."""

    def _parse(self, xml_str, source='test.xml', version=None):
        root = ET.fromstring(xml_str)
        v = version or [2, 0]
        return _build_suite(root, source, v, _make_logger())

    def test_basic_suite(self):
        xml = '''<suite name="my_suite" version="2.0">
          <group name="physics">
            <scheme>sch1</scheme>
          </group>
        </suite>'''
        suite = self._parse(xml)
        self.assertEqual(suite.name, 'my_suite')
        self.assertEqual(suite.group_names(), ['physics'])

    def test_suite_with_init_final(self):
        xml = '''<suite name="s" version="2.0">
          <init>suite_init_scheme</init>
          <group name="g"><scheme>sch</scheme></group>
          <final>suite_final_scheme</final>
        </suite>'''
        suite = self._parse(xml)
        self.assertEqual(suite.init_scheme, 'suite_init_scheme')
        self.assertEqual(suite.final_scheme, 'suite_final_scheme')

    def test_deprecated_finalize_rejected(self):
        """<finalize> (old long form) is rejected with a clear error
        pointing at the canonical <final> short form."""
        xml = '''<suite name="s" version="2.0">
          <group name="g"><scheme>sch</scheme></group>
          <finalize>final_scheme</finalize>
        </suite>'''
        with self.assertRaises(CCPPError) as cm:
            self._parse(xml)
        msg = str(cm.exception)
        self.assertIn('finalize', msg)
        self.assertIn('<final>', msg)

    def test_deprecated_initalize_typo_rejected(self):
        """<initalize> (the old schema's typo) is rejected with a clear
        error pointing at the canonical <init> short form."""
        xml = '''<suite name="s" version="2.0">
          <initalize>init_scheme</initalize>
          <group name="g"><scheme>sch</scheme></group>
        </suite>'''
        with self.assertRaises(CCPPError) as cm:
            self._parse(xml)
        msg = str(cm.exception)
        self.assertIn('initalize', msg)
        self.assertIn('<init>', msg)

    def test_deprecated_initialize_correct_spelling_rejected(self):
        """<initialize> (the correctly-spelled long form) is also rejected.
        Only the short <init> is accepted."""
        xml = '''<suite name="s" version="2.0">
          <initialize>init_scheme</initialize>
          <group name="g"><scheme>sch</scheme></group>
        </suite>'''
        with self.assertRaises(CCPPError) as cm:
            self._parse(xml)
        msg = str(cm.exception)
        self.assertIn('initialize', msg)
        self.assertIn('<init>', msg)

    def test_missing_suite_name_raises(self):
        xml = '<suite version="2.0"><group name="g"><scheme>s</scheme></group></suite>'
        with self.assertRaises(CCPPError):
            self._parse(xml)

    def test_duplicate_group_name_raises(self):
        xml = '''<suite name="s" version="2.0">
          <group name="dup"><scheme>a</scheme></group>
          <group name="dup"><scheme>b</scheme></group>
        </suite>'''
        with self.assertRaises(CCPPError) as cm:
            self._parse(xml)
        self.assertIn('dup', str(cm.exception))

    def test_empty_init_raises(self):
        xml = '''<suite name="s" version="2.0">
          <init>  </init>
          <group name="g"><scheme>s</scheme></group>
        </suite>'''
        with self.assertRaises(CCPPError):
            self._parse(xml)

    def test_empty_final_raises(self):
        xml = '''<suite name="s" version="2.0">
          <group name="g"><scheme>s</scheme></group>
          <final></final>
        </suite>'''
        with self.assertRaises(CCPPError):
            self._parse(xml)

    def test_subcycle_in_group(self):
        xml = '''<suite name="s" version="2.0">
          <group name="g">
            <subcycle loop="num_subcycles_for_scheme6">
              <scheme>scheme6</scheme>
            </subcycle>
          </group>
        </suite>'''
        suite = self._parse(xml)
        grp = suite.groups[0]
        subcycle = grp.items[0]
        self.assertIsInstance(subcycle, SuiteSubcycle)
        self.assertEqual(subcycle.loop, 'num_subcycles_for_scheme6')
        self.assertFalse(subcycle.is_literal_count)


########################################################################
# parse_suite_xml — valid files (ported from test_sdf.py)
########################################################################

def _sample(name):
    return os.path.join(_SAMPLE_DIR, name)


class TestParseSuiteXmlValid(unittest.TestCase):
    """Integration tests for :func:`parse_suite_xml` with valid SDFs.

    These tests port the ``test_good_v*`` cases from the legacy
    ``test_sdf.py`` test suite.  Expanded XML output is compared against
    the ``*_exp.xml`` reference files.
    """

    def setUp(self):
        self._tmp = tempfile.mkdtemp()
        self._log = _make_logger()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmp, ignore_errors=True)

    def _parse(self, filename, skip_validation=True):
        return parse_suite_xml(
            _sample(filename), self._tmp,
            logger=self._log,
            schema_path=_SCHEMA_DIR,
            skip_validation=skip_validation,
        )

    def _compare_expanded(self, suite, exp_filename):
        """Assert that the expanded XML matches the reference file."""
        self.assertIsNotNone(suite.expanded_file)
        self.assertTrue(os.path.isfile(suite.expanded_file))
        _, ref_root  = read_xml_file(_sample(exp_filename), self._log)
        _, got_root  = read_xml_file(suite.expanded_file,   self._log)
        diffs = xml_diff(ref_root, got_root)
        sep = '\n'
        self.assertFalse(
            diffs,
            msg=f"Expanded XML differs from {exp_filename}:\n{sep.join(diffs)}"
        )

    # ---- v1 suites ---------------------------------------------------------

    def test_v1_suite_01(self):
        """V1 SDF is read and written without expansion."""
        suite = self._parse('suite_good_v1_test01.xml')
        self.assertEqual(suite.version[0], 1)
        self.assertTrue(os.path.isfile(suite.expanded_file))

    def test_v1_suite_02(self):
        suite = self._parse('suite_good_v1_test02.xml')
        self.assertEqual(suite.version[0], 1)

    # ---- v2 suites ---------------------------------------------------------

    def test_v2_suite_01_expand_group_of_nested_suite(self):
        """Expand one group from a simple nested suite at group level."""
        suite = self._parse('suite_good_v2_test01.xml')
        self.assertEqual(suite.name, 'ver_test_suite')
        self.assertEqual(suite.version, [2, 0])
        self.assertEqual(suite.group_names(), ['group1'])
        self._compare_expanded(suite, 'suite_good_v2_test01_exp.xml')

    def test_v2_suite_01_scheme_names(self):
        """After expansion, correct scheme names are in group1."""
        suite = self._parse('suite_good_v2_test01.xml')
        names = suite.get_group('group1').scheme_names()
        # Expected after expansion: scheme5, scheme1i, scheme2i, scheme1i, scheme9
        self.assertEqual(names, ['scheme5', 'scheme1i', 'scheme2i', 'scheme1i', 'scheme9'])

    def test_v2_suite_02_expand_one_group_of_multigroup_nested_suite(self):
        """Expand one group from a multi-group nested suite at group level."""
        suite = self._parse('suite_good_v2_test02.xml')
        self.assertEqual(suite.name, 'v2_suite')
        self._compare_expanded(suite, 'suite_good_v2_test02_exp.xml')

    def test_v2_suite_02_subcycle_preserved(self):
        """Subcycle loop attribute survives the expansion."""
        suite = self._parse('suite_good_v2_test02.xml')
        grp = suite.get_group('main_group')
        # First item is the subcycle from the original suite
        subcycle = grp.items[0]
        self.assertIsInstance(subcycle, SuiteSubcycle)
        self.assertEqual(subcycle.loop, 'num_subcycles_for_scheme6')

    def test_v2_suite_03_expand_multiple_nested_suites(self):
        """Expand two nested suites at group level + full suite at suite level."""
        suite = self._parse('suite_good_v2_test03.xml')
        self.assertEqual(suite.name, 'main_suite')
        # Should have 3 groups after expansion: groupp, nested_group1, nested_group2
        self.assertEqual(len(suite.groups), 3)
        self._compare_expanded(suite, 'suite_good_v2_test03_exp.xml')

    def test_v2_suite_04_expand_group_from_nested_full_suite(self):
        """Expand two group-level nested suites + one group from nested suite at suite level."""
        suite = self._parse('suite_good_v2_test04.xml')
        self.assertEqual(suite.name, 'ver_test_suite')
        # 2 groups: main11 + nested_group2 (only group from nested_full_suite)
        self.assertEqual(len(suite.groups), 2)
        self._compare_expanded(suite, 'suite_good_v2_test04_exp.xml')

    def test_expanded_xml_written_to_output_root(self):
        """The expanded XML must be written to the correct output path."""
        suite = self._parse('suite_good_v2_test01.xml')
        expected_name = f"ccpp_{suite.name}_expanded.xml"
        expected_path = os.path.join(self._tmp, expected_name)
        self.assertEqual(suite.expanded_file, expected_path)
        self.assertTrue(os.path.isfile(expected_path))

    def test_all_scheme_names_unique(self):
        """all_scheme_names() deduplicates across groups."""
        suite = self._parse('suite_good_v2_test03.xml')
        all_names = suite.all_scheme_names()
        self.assertEqual(len(all_names), len(set(all_names)))


########################################################################
# parse_suite_xml — error cases
########################################################################

class TestParseSuiteXmlErrors(unittest.TestCase):
    """Error handling tests for :func:`parse_suite_xml`."""

    def setUp(self):
        self._tmp = tempfile.mkdtemp()
        self._log = _make_logger()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmp, ignore_errors=True)

    def _parse(self, filename, skip_validation=True):
        return parse_suite_xml(
            _sample(filename), self._tmp,
            logger=self._log,
            schema_path=_SCHEMA_DIR,
            skip_validation=skip_validation,
        )

    def test_nonexistent_file_raises(self):
        with self.assertRaises(CCPPError):
            parse_suite_xml('/nonexistent/suite.xml', self._tmp,
                            logger=self._log, skip_validation=True)

    def test_bad_schema_version_formats(self):
        """Malformed version attributes are detected on file read."""
        for fname in ('suite_bad_version01.xml', 'suite_bad_version02.xml',
                      'suite_bad_version03.xml', 'suite_bad_version04.xml'):
            with self.subTest(fname=fname):
                with self.assertRaises(CCPPError):
                    self._parse(fname)

    def test_missing_version_raises(self):
        with self.assertRaises(CCPPError) as cm:
            self._parse('suite_missing_version.xml')
        self.assertIn('suite_missing_version.xml', str(cm.exception))
        self.assertIn('Version attribute required', str(cm.exception))

    def test_infinite_group_recursion_detected(self):
        """Circular nested-suite references at group level are caught."""
        with self.assertRaises(CCPPError) as cm:
            self._parse('suite_recurse_top1.xml')
        self.assertIn('iterations', str(cm.exception))

    def test_infinite_suite_recursion_detected(self):
        """Circular nested-suite references at suite level are caught."""
        with self.assertRaises(CCPPError) as cm:
            self._parse('suite_recurse_top2.xml')
        self.assertIn('iterations', str(cm.exception))

    def test_missing_nested_group_raises(self):
        """Referencing a group that doesn't exist in the target suite raises."""
        with self.assertRaises(CCPPError) as cm:
            self._parse('suite_missing_group.xml')
        self.assertIn('not found', str(cm.exception))

    def test_missing_loaded_suite_raises(self):
        """Referencing a suite name that doesn't exist in the target file raises."""
        with self.assertRaises(CCPPError) as cm:
            self._parse('suite_missing_loaded_suite.xml')
        self.assertIn('not found', str(cm.exception))


########################################################################
# Schema validation tests (require xmllint)
########################################################################

class TestSchemaValidation(unittest.TestCase):
    """Tests that exercise xmllint-based XML schema validation.

    These tests are skipped automatically when ``xmllint`` is not installed.
    """

    @classmethod
    def setUpClass(cls):
        import shutil
        if not shutil.which('xmllint'):
            raise unittest.SkipTest("xmllint not installed — skipping schema validation tests")
        cls._log = _make_logger()
        cls._tmp = tempfile.mkdtemp()

    @classmethod
    def tearDownClass(cls):
        import shutil
        shutil.rmtree(cls._tmp, ignore_errors=True)

    def test_good_v2_suite_validates(self):
        _, root = read_xml_file(_sample('suite_good_v2_test01.xml'), self._log)
        version = find_schema_version(root)
        result = validate_xml_file(
            _sample('suite_good_v2_test01.xml'), 'suite', version, self._log,
            schema_path=_SCHEMA_DIR
        )
        self.assertTrue(result)

    def test_bad_suite_tag_rejected(self):
        """A nested <suite> element violates the schema."""
        _, root = read_xml_file(_sample('suite_bad_v2_suite_tag.xml'), self._log)
        version = find_schema_version(root)
        try:
            result = validate_xml_file(
                _sample('suite_bad_v2_suite_tag.xml'), 'suite', version,
                self._log, schema_path=_SCHEMA_DIR
            )
            # Some xmllint versions return True even on error
        except CCPPError as exc:
            self.assertIn("not expected", str(exc))

    def test_invalid_fortran_id_scheme_rejected(self):
        """A scheme name that is not a valid Fortran ID is rejected."""
        _, root = read_xml_file(
            _sample('suite_invalid_scheme_fortran_id.xml'), self._log
        )
        version = find_schema_version(root)
        with self.assertRaises(CCPPError) as cm:
            validate_xml_file(
                _sample('suite_invalid_scheme_fortran_id.xml'), 'suite',
                version, self._log, schema_path=_SCHEMA_DIR
            )
        self.assertIn("scheme-1", str(cm.exception))

    def test_invalid_fortran_id_group_rejected(self):
        _, root = read_xml_file(
            _sample('suite_invalid_group_fortran_id.xml'), self._log
        )
        version = find_schema_version(root)
        with self.assertRaises(CCPPError) as cm:
            validate_xml_file(
                _sample('suite_invalid_group_fortran_id.xml'), 'suite',
                version, self._log, schema_path=_SCHEMA_DIR
            )
        self.assertIn("group-1", str(cm.exception))

    def test_invalid_fortran_id_suite_rejected(self):
        _, root = read_xml_file(
            _sample('suite_invalid_suite_fortran_id.xml'), self._log
        )
        version = find_schema_version(root)
        with self.assertRaises(CCPPError) as cm:
            validate_xml_file(
                _sample('suite_invalid_suite_fortran_id.xml'), 'suite',
                version, self._log, schema_path=_SCHEMA_DIR
            )
        self.assertIn("ver-test-suite", str(cm.exception))

    def test_duplicate_group_name_rejected_after_expansion(self):
        """After nested-suite expansion a duplicate group name fails validation."""
        _, root = read_xml_file(_sample('suite_bad_v2_duplicate_group.xml'), self._log)
        version = find_schema_version(root)
        # Initial file validates OK
        result = validate_xml_file(
            _sample('suite_bad_v2_duplicate_group.xml'), 'suite', version,
            self._log, schema_path=_SCHEMA_DIR
        )
        self.assertTrue(result)
        # After expansion the duplicated xs:ID triggers a validation error
        expand_nested_suites(root, _SAMPLE_DIR, logger=self._log)
        expanded_path = os.path.join(self._tmp, 'dup_group_expanded.xml')
        write_xml_file(root, expanded_path, self._log)
        with self.assertRaises(CCPPError) as cm:
            validate_xml_file(expanded_path, 'suite', version, self._log,
                              schema_path=_SCHEMA_DIR)
        self.assertIn('group1', str(cm.exception))


########################################################################
# xml_diff helper tests (ported from test_sdf.py)
########################################################################

class TestXmlDiff(unittest.TestCase):
    """Tests for the :func:`xml_diff` helper."""

    def test_matching_trees(self):
        r1 = ET.fromstring('<tag>text</tag>')
        r2 = ET.fromstring('<tag>text</tag>')
        self.assertEqual(xml_diff(r1, r2), [])

    def test_tag_mismatch(self):
        diffs = xml_diff(ET.fromstring('<tag1>x</tag1>'),
                         ET.fromstring('<tag2>x</tag2>'))
        self.assertEqual(len(diffs), 1)
        self.assertIn('Tags', diffs[0])

    def test_text_mismatch(self):
        diffs = xml_diff(ET.fromstring('<t>a</t>'), ET.fromstring('<t>b</t>'))
        self.assertEqual(len(diffs), 1)
        self.assertIn('does not match', diffs[0])

    def test_attrib_mismatch(self):
        r1 = ET.fromstring('<t a="1" b="2"/>')
        r2 = ET.fromstring('<t a="X" c="2"/>')
        diffs = xml_diff(r1, r2)
        self.assertEqual(len(diffs), 3)

    def test_child_count_mismatch(self):
        r1 = ET.fromstring('<p><c/></p>')
        r2 = ET.fromstring('<p><c/><c/></p>')
        diffs = xml_diff(r1, r2)
        self.assertEqual(len(diffs), 1)
        self.assertIn('children', diffs[0])


########################################################################

if __name__ == '__main__':
    unittest.main()
