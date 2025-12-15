#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Contains unit tests for parsing Suite Definition Files (SDFs)
               in scripts/parse_tools/xml_tools.py

 Assumptions:

 Command line arguments: none

 Usage: python3 test_sdf.py         # run the unit tests
-----------------------------------------------------------------------
"""

import filecmp
import glob
import logging
import os
import sys
import unittest
import xml.etree.ElementTree as ET

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_SCRIPTS_DIR = os.path.abspath(os.path.join(_TEST_DIR, os.pardir,
                                            os.pardir, "scripts"))
_SAMPLE_FILES_DIR = os.path.join(_TEST_DIR, "sample_suite_files")
_PRE_TMP_DIR = os.path.join(_TEST_DIR, "tmp")
_TMP_DIR = os.path.join(_PRE_TMP_DIR, "suite_files")

if not os.path.exists(_SCRIPTS_DIR):
    raise ImportError(f"Cannot find scripts directory, {_SCRIPTS_DIR}")

sys.path.append(_SCRIPTS_DIR)

# pylint: disable=wrong-import-position
from parse_tools import init_log
from parse_tools import read_xml_file, validate_xml_file, write_xml_file
from parse_tools import find_schema_version, expand_nested_suites
# pylint: enable=wrong-import-position

class SDFParseTestCase(unittest.TestCase):

    """Tests for `expand_nested_suites` and related functions."""

    logger = None

    @classmethod
    def setUpClass(cls):
        """Clean output directory (tmp) before running tests"""
        # Does "tmp" directory exist?  If not then create it:
        if not os.path.exists(_PRE_TMP_DIR):
            os.makedirs(_PRE_TMP_DIR)
        # end if

        # We need a logger
        cls.logger = init_log(cls.__name__, level=logging.WARNING)

        #Does "tmp" directory exist?  If not then create it:
        # Ensure the "tmp/suite_files" directory exists and is empty
        if os.path.exists(_TMP_DIR):
            # Clear out all files:
            for fpath in glob.iglob(os.path.join(_TMP_DIR, '*.*')):
                if os.path.exists(fpath):
                    os.remove(fpath)
                # End if
            # End for
        else:
            os.makedirs(_TMP_DIR)
        # end if

        # Run inherited setup method:
        super().setUpClass()

    @classmethod
    def get_logger(cls):
        return cls.logger

    @classmethod
    def compare_text(cls, name, txt1, txt2, typ):
        """Compare two XML text or tail items (which may be None).
        Return None if items match, otherwise, return an error string"""
        res = None
        if txt1 and txt2:
            if txt1.strip() != txt2.strip():
                res = f"{name} {typ}, '{txt1}', does not match {typ}, '{txt2}'"
            # end if
        elif txt1:
            res = f"{name} {typ} is missing from string2"
        elif txt2:
            res = f"{name} {typ} is missing from string1"
        else:
            res = None
        # end if
        return res

    @classmethod
    def xml_diff(cls, xt1, xt2):
        """
        Compares two xml etrees, xt1 and xt2
        Return None if the trees match, otherwise, return a difference string
        """

        diffs = []
        # First, compare the XML tags
        if xt1.tag != xt2.tag:
            diffs.append(f"Tags do not match: {xt1.tag} != {xt2.tag}")
        else:
            # Compare the attributes
            for name, value in xt1.attrib.items():
                if name not in xt2.attrib:
                    diffs.append(f"xt1 attribute, {name}, is missing in xt2")
                else:
                    xt2v = xt2.attrib.get(name)
                    if xt2v != value:
                        diffs.append(f"Attributes for {name} do not match: {str(value)} != {str(xt2v)}")
                    # end if
                # end if
            # end for
            for name in xt2.attrib.keys():
                if name not in xt1.attrib:
                    diffs.append(f"xt2 attribute, {name}, is missing in xt1")
                # end if
            # end for
            # Compare the text bodies (if any)
            tdiff = cls.compare_text(xt1.tag, xt1.text, xt2.text, "text")
            if tdiff:
                diffs.append(tdiff)
            # end if
            tdiff = cls.compare_text(xt1.tag, xt1.tail, xt2.tail, "tail")
            if tdiff:
                diffs.append(tdiff)
            # end if
            # Compare children
            if len(xt1) != len(xt2):
                diffs.append(f"Number of children length differs, {len(xt1)} != {len(xt2)}")
            else:
                for child1, child2 in zip(xt1, xt2):
                    kid_diffs = cls.xml_diff(child1, child2)
                    if kid_diffs:
                        diffs.extend(kid_diffs)
                    # end if
                # end for
            # end if
        # end if
        return diffs

    def test_xml_diff(self):
        """Test that xml_diff catches xml differences"""
        root1 = ET.fromstring("<tag1>item</tag1>")
        root2 = ET.fromstring("<taga>item</taga>")
        diffs = self.xml_diff(root1, root2)
        self.assertTrue(diffs)
        self.assertEqual(len(diffs), 1)
        self.assertTrue("Tags do not match" in diffs[0],
                        msg="tag1 should not match taga")
        root1 = ET.fromstring("<tag1>item1</tag1>")
        root2 = ET.fromstring("<tag1>item2</tag1>")
        diffs = self.xml_diff(root1, root2)
        self.assertTrue(diffs)
        self.assertEqual(len(diffs), 1)
        self.assertTrue("does not match" in diffs[0],
                        msg="item1 should not match item2")
        root1 = ET.fromstring('<tag1 attrib1="hi" attrib2="hi">item1</tag1>')
        root2 = ET.fromstring('<tag1 attrib1="mom" attrib3="low">item1</tag1>')
        diffs = self.xml_diff(root1, root2)
        self.assertTrue(diffs)
        self.assertEqual(len(diffs), 3)
        self.assertTrue("Attributes for" in diffs[0] and "do not match" in diffs[0],
                        msg="attrib1 values should not match")
        self.assertTrue("xt1 attribute, attrib2, is missing in xt2" in diffs[1],
                        msg=f"attrib2 is missing in root2")
        self.assertTrue("xt2 attribute, attrib3, is missing in xt1" in diffs[2],
                        msg=f"attrib3 is missing in root1")
        root1 = ET.fromstring('<tag1 attrib1="hi"><subtag attrib2="hi"></subtag></tag1>')
        root2 = ET.fromstring('<tag1 attrib1="hi"><subtag attrib2="mom"></subtag></tag1>')
        diffs = self.xml_diff(root1, root2)
        self.assertEqual(len(diffs), 1)
        self.assertTrue("Attributes for" in diffs[0] and "do not match" in diffs[0],
                        msg=f"attrib2 values should not match")

    def test_good_v1_sdf(self):
        """Test that the parser recognizes a V1 SDF and parses it correctly
        """
        num_tests = 2
        header = "Test of parsing of good V1 SDF"
        for test_num in range(num_tests):
            # Setup
            testname = f"suite_good_v1_test{test_num+1:{0}{2}}"
            source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
            compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
            logger = self.get_logger()
            # Exercise
            _, xml_root = read_xml_file(source, logger)
            schema_version = find_schema_version(xml_root)
            self.assertEqual(schema_version[0], 1)
            self.assertEqual(schema_version[1], 0)
            res = validate_xml_file(source, 'suite', schema_version, logger,
                                    error_on_noxmllint=True)
            self.assertTrue(res)
            write_xml_file(xml_root, compare, logger)
            amsg = f"{compare} does not exist"
            self.assertTrue(os.path.exists(compare), msg=amsg)
            _, compare_root = read_xml_file(compare, logger)
            diffs = self.xml_diff(xml_root, compare_root)
            lsep = '\n'
            amsg = f"{source} does not match {compare}\n{lsep.join(diffs)}"
            self.assertFalse(diffs, msg=amsg)
        # end for

    def test_good_v2_sdf_01(self):
        """Test that the parser recognizes a V2 SDF and parses and
        expands it correctly.
        Test the expansion of one group of a simple nested suite at group level.
        """
        header = "Test of parsing of good V2 SDF"
        # Setup
        testname = "suite_good_v2_test01"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        source_exp = os.path.join(_SAMPLE_FILES_DIR, f"{testname}_exp.xml")
        compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        self.assertEqual(schema_version[0], 2)
        self.assertEqual(schema_version[1], 0)
        expand_nested_suites(xml_root, _SAMPLE_FILES_DIR, logger=logger)
        write_xml_file(xml_root, compare, logger)
        res = validate_xml_file(source, 'suite', schema_version, logger,
                                error_on_noxmllint=True)
        self.assertTrue(res)
        amsg = f"{compare} does not exist"
        self.assertTrue(os.path.exists(compare), msg=amsg)
        _, xml_root = read_xml_file(source_exp, logger)
        _, compare_root = read_xml_file(compare, logger)
        diffs = self.xml_diff(xml_root, compare_root)
        lsep = '\n'
        amsg = f"{source_exp} does not match {compare}\n{lsep.join(diffs)}"
        self.assertFalse(diffs, msg=amsg)

    def test_good_v2_sdf_02(self):
        """Test that the parser recognizes a V2 SDF and parses and
        expands it correctly
        Test the expansion of one group of a multiple group nested suite at group level.
        """
        header = "Test of parsing of good V2 SDF"
        # Setup
        testname = "suite_good_v2_test02"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        source_exp = os.path.join(_SAMPLE_FILES_DIR, f"{testname}_exp.xml")
        compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        self.assertEqual(schema_version[0], 2)
        self.assertEqual(schema_version[1], 0)
        expand_nested_suites(xml_root, _SAMPLE_FILES_DIR, logger=logger)
        write_xml_file(xml_root, compare, logger)
        res = validate_xml_file(source, 'suite', schema_version, logger,
                                error_on_noxmllint=True)
        self.assertTrue(res)
        amsg = f"{compare} does not exist"
        self.assertTrue(os.path.exists(compare), msg=amsg)
        _, xml_root = read_xml_file(source_exp, logger)
        _, compare_root = read_xml_file(compare, logger)
        diffs = self.xml_diff(xml_root, compare_root)
        lsep = '\n'
        amsg = f"{source_exp} does not match {compare}\n{lsep.join(diffs)}"
        self.assertFalse(diffs, msg=amsg)

    def test_good_v2_sdf_03(self):
        """Test that the parser recognizes a V2 SDF and parses and
        expands it correctly
        Test expansion of two nested suites at group level plus a full nested suite at
        suite level.
        """
        header = "Test of parsing of good V2 SDF"
        # Setup
        testname = "suite_good_v2_test03"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        source_exp = os.path.join(_SAMPLE_FILES_DIR, f"{testname}_exp.xml")
        compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        self.assertEqual(schema_version[0], 2)
        self.assertEqual(schema_version[1], 0)
        expand_nested_suites(xml_root, _SAMPLE_FILES_DIR, logger=logger)
        write_xml_file(xml_root, compare, logger)
        res = validate_xml_file(source, 'suite', schema_version, logger,
                                error_on_noxmllint=True)
        self.assertTrue(res)
        amsg = f"{compare} does not exist"
        self.assertTrue(os.path.exists(compare), msg=amsg)
        _, xml_root = read_xml_file(source_exp, logger)
        _, compare_root = read_xml_file(compare, logger)
        diffs = self.xml_diff(xml_root, compare_root)
        lsep = '\n'
        amsg = f"{source_exp} does not match {compare}\n{lsep.join(diffs)}"
        self.assertFalse(diffs, msg=amsg)

    def test_good_v2_sdf_04(self):
        """Test that the parser recognizes a V2 SDF and parses and
        expands it correctly
        Test expansion of two nested suites at group level plus one group from a
        nested suite at suite level.
        """
        header = "Test of parsing of good V2 SDF"
        # Setup
        testname = "suite_good_v2_test04"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        source_exp = os.path.join(_SAMPLE_FILES_DIR, f"{testname}_exp.xml")
        compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        self.assertEqual(schema_version[0], 2)
        self.assertEqual(schema_version[1], 0)
        expand_nested_suites(xml_root, _SAMPLE_FILES_DIR, logger=logger)
        write_xml_file(xml_root, compare, logger)
        res = validate_xml_file(source, 'suite', schema_version, logger,
                                error_on_noxmllint=True)
        self.assertTrue(res)
        amsg = f"{compare} does not exist"
        self.assertTrue(os.path.exists(compare), msg=amsg)
        _, xml_root = read_xml_file(source_exp, logger)
        _, compare_root = read_xml_file(compare, logger)
        diffs = self.xml_diff(xml_root, compare_root)
        lsep = '\n'
        amsg = f"{source_exp} does not match {compare}\n{lsep.join(diffs)}"
        self.assertFalse(diffs, msg=amsg)

    def test_bad_v2_suite_tag_sdf(self):
        """Test that verification system recognizes a misplaced suite tag"""
        header = "Test trapping of version attribute on a v2 suite tag"
        # Setup
        testname = f"suite_bad_v2_suite_tag"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        # Some versions of xmllint return an exit code 0 even if the
        # validation fails. "Good" versions return an exit code /= 0,
        # which then raises a CCPPError internally. The following
        # logic handles the correct behavior (validation fails ==>
        # exit code /= 0 ==> CCPPError).
        try:
            res = validate_xml_file(source, 'suite', schema_version, logger,
                                    error_on_noxmllint=True)
        except Exception as e:
            emsg = "Schemas validity error : Element 'suite': This element is not expected."
            msg = str(e)
            self.assertTrue(emsg in msg)

    def test_bad_v2_suite_duplicate_group1(self):
        """Test that verification system recognizes a duplicate group name"""
        header = "Test trapping of expanded suite duplicate group name"
        # Setup
        testname = f"suite_bad_v2_duplicate_group"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        self.assertEqual(schema_version[0], 2)
        self.assertEqual(schema_version[1], 0)
        res = validate_xml_file(source, 'suite', schema_version, logger,
                                error_on_noxmllint=True)
        self.assertTrue(res, msg="Initial suite file should be valid")
        with self.assertRaises(Exception) as context:
            expand_nested_suites(xml_root, _SAMPLE_FILES_DIR, logger=logger)
            write_xml_file(xml_root, compare, logger)
            _ = validate_xml_file(compare, 'suite', schema_version, logger)
        # end with
        emsg = "Schemas validity error : Element 'group', attribute 'name': " + \
            "'group1' is not a valid value of the atomic type 'xs:ID'"
        fmsg = str(context.exception)
        self.assertTrue(emsg in fmsg, msg=fmsg)
        if not emsg in fmsg:
            raise context

    def test_bad_v2_suite_missing_group(self):
        """Test that verification system recognizes a missing group name"""
        header = "Test trapping of expanded suite missing group name"
        # Setup
        testname = f"suite_missing_group"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        self.assertEqual(schema_version[0], 2)
        self.assertEqual(schema_version[1], 0)
        res = validate_xml_file(source, 'suite', schema_version, logger,
                                error_on_noxmllint=True)
        self.assertTrue(res, msg="Initial suite file should be valid")
        with self.assertRaises(Exception) as context:
            expand_nested_suites(xml_root, _SAMPLE_FILES_DIR, logger=logger)
            write_xml_file(xml_root, compare, logger)
        # end with
        emsg = "Nested suite subsuite_1, group group2, not found"
        fmsg = str(context.exception)
        self.assertTrue(emsg in fmsg, msg=fmsg)

    def test_bad_v2_suite_missing_file(self):
        """Test that verification system recognizes a missing file argument"""
        header = "Test trapping of missing file for nested suite"
        # Setup
        testname = f"suite_missing_file"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        self.assertEqual(schema_version[0], 2)
        self.assertEqual(schema_version[1], 0)
        # See note about different behavior of xmllint versions
        # in test test_bad_v2_suite_tag_sdf above.
        try:
            res = validate_xml_file(source, 'suite', schema_version, logger,
                                    error_on_noxmllint=True)
        except Exception as e:
            emsg = "Schemas validity error : Element 'nested_suite': " + \
                "The attribute 'file' is required but missing."
            msg = str(e)
            self.assertTrue(emsg in msg)

    def test_bad_v2_suite_missing_loaded_suite(self):
        """Test that verification system recognizes a missing suite loaded
        from another file"""
        header = "Test trapping of expanded suite missing a subsuite in a different file"
        # Setup
        testname = f"suite_missing_loaded_suite"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        self.assertEqual(schema_version[0], 2)
        self.assertEqual(schema_version[1], 0)
        res = validate_xml_file(source, 'suite', schema_version, logger,
                                error_on_noxmllint=True)
        self.assertTrue(res, msg="Initial suite file should be valid")
        with self.assertRaises(Exception) as context:
            expand_nested_suites(xml_root, _SAMPLE_FILES_DIR, logger=logger)
            write_xml_file(xml_root, compare, logger)
        # end with
        emsg = "Nested suite v12_suite, group main_group, not found in file"
        fmsg = str(context.exception)
        self.assertTrue(emsg in fmsg, msg=fmsg)

    def test_bad_v2_suite_infinite_group_recursion(self):
        """Test that verification system recognizes infinite recursion when
        including at the group level"""
        header = "Test trapping of expanded suite with infinite group recursion"
        # Setup
        testname = f"suite_recurse_top1"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        self.assertEqual(schema_version[0], 2)
        self.assertEqual(schema_version[1], 0)
        res = validate_xml_file(source, 'suite', schema_version, logger,
                                error_on_noxmllint=True)
        self.assertTrue(res, msg="Initial suite file should be valid")
        with self.assertRaises(Exception) as context:
            expand_nested_suites(xml_root, _SAMPLE_FILES_DIR, logger=logger)
            write_xml_file(xml_root, compare, logger)
        # end with
        emsg = ("Exceeded number of iterations while expanding nested suites")
        fmsg = str(context.exception)
        self.assertTrue(emsg in fmsg, msg=fmsg)

    def test_bad_v2_suite_infinite_suite_recursion(self):
        """Test that verification system recognizes infinite recursion when
        including at the imported suite level"""
        header = "Test trapping of expanded suite with infinite suite recursion"
        # Setup
        testname = f"suite_recurse_top2"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        compare = os.path.join(_TMP_DIR, f"{testname}_out.xml")
        logger = self.get_logger()
        # Exercise
        _, xml_root = read_xml_file(source, logger)
        schema_version = find_schema_version(xml_root)
        self.assertEqual(schema_version[0], 2)
        self.assertEqual(schema_version[1], 0)
        res = validate_xml_file(source, 'suite', schema_version, logger,
                                error_on_noxmllint=True)
        self.assertTrue(res, msg="Initial suite file should be valid")
        with self.assertRaises(Exception) as context:
            expand_nested_suites(xml_root, _SAMPLE_FILES_DIR, logger=logger)
            write_xml_file(xml_root, compare, logger)
        # end with
        emsg = ("Exceeded number of iterations while expanding nested suites")
        fmsg = str(context.exception)
        self.assertTrue(emsg in fmsg, msg=fmsg)

    def test_bad_schema_version(self):
        """Test that verification system recognizes a bad version entry"""
        num_tests = 4
        header = "Test trapping of invalid SDF version"
        exc_strings = ["Format must be <integer>.<integer>",
                       "Format must be <integer>.<integer>",
                       "Major version must be at least 1",
                       "Minor version must be non-negative"]
        for test_num in range(num_tests):
            # Setup
            testname = f"suite_bad_version{test_num+1:{0}{2}}"
            source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
            logger = self.get_logger()
            # Exercise
            with self.assertRaises(Exception) as context:
                _, xml_root = read_xml_file(source, logger)
                schema_version = find_schema_version(xml_root)
            # end with
            # Check exception for expected error messages
            exp_str = str(context.exception)
            self.assertTrue(exc_strings[test_num] in exp_str,
                            msg=f"Bad exception in test {test_num + 1}, '{exp_str}'")
        # end for

    def test_missing_schema_version(self):
        """Test that verification system recognizes a missing version num"""
        header = "Test trapping of missing SDF version"
        # Setup
        testname = f"suite_missing_version"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.xml")
        logger = self.get_logger()
        # Exercise
        with self.assertRaises(Exception) as context:
            _, xml_root = read_xml_file(source, logger)
            schema_version = find_schema_version(xml_root)
        # end with
        # Check exception for expected error messages
        self.assertTrue("version attribute required" in str(context.exception),
                        msg=f"Bad exception for missing suite version")
