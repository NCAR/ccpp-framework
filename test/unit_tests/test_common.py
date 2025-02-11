#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Contains unit tests for functions in common.py

 Assumptions:

 Command line arguments: none

 Usage: python test_common.py         # run the unit tests
-----------------------------------------------------------------------
"""
import sys
import os
import logging
import unittest

TEST_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_FILE = os.path.abspath(os.path.abspath(__file__))
SCRIPTS_DIR = os.path.abspath(os.path.join(TEST_DIR, os.pardir, os.pardir, "scripts"))
SAMPLE_FILES_DIR = os.path.join(TEST_DIR, "sample_files")

if not os.path.exists(SCRIPTS_DIR):
    raise ImportError("Cannot find scripts directory")

sys.path.append(SCRIPTS_DIR)
logging.disable(level=logging.CRITICAL)

# pylint: disable=wrong-import-position
import common
# pylint: enable=wrong-import-position

class CommonTestCase(unittest.TestCase):

    """Tests functionality of functions in common.py"""

    def test_split_var_name_and_array_reference(self):
        """Test split_var_name_and_array_reference() function"""

        self.assertEqual(common.split_var_name_and_array_reference("foo(:,a,1:ddt%ngas)"),
                         ("foo","(:,a,1:ddt%ngas)"))

    def test_encode_container(self):
        """Test encode_container() function"""

        modulename = "ABCD1234"
        typename = "COMPLEX"
        schemename = "testscheme"
        subroutinename = "testsubroutine"
        self.assertEqual(common.encode_container(modulename),f"MODULE_{modulename}")
        self.assertEqual(common.encode_container(modulename,typename),f"MODULE_{modulename} TYPE_{typename}")
        self.assertEqual(common.encode_container(modulename,schemename,subroutinename),
                         f"MODULE_{modulename} SCHEME_{schemename} SUBROUTINE_{subroutinename}")
        self.assertRaises(Exception,common.encode_container,modulename,typename,schemename,subroutinename)
        self.assertRaises(Exception,common.encode_container)

    def test_decode_container(self):
        """Test decode_container() function"""

        modulename = "ABCD1234"
        typename = "COMPLEX"
        schemename = "testscheme"
        subroutinename = "testsubroutine"
        self.assertEqual(common.decode_container(f"MODULE_{modulename}"),f"MODULE {modulename}")
        self.assertEqual(common.decode_container(f"MODULE_{modulename} TYPE_{typename}"),
                                                 f"MODULE {modulename} TYPE {typename}")
        self.assertEqual(common.decode_container(f"MODULE_{modulename} SCHEME_{schemename} SUBROUTINE_{subroutinename}"),
                                                 f"MODULE {modulename} SCHEME {schemename} SUBROUTINE {subroutinename}")
        self.assertRaises(Exception,common.decode_container,
                          f"MODULE_{modulename} TYPE_{typename} SCHEME_{schemename} SUBROUTINE_{subroutinename}")
        self.assertRaises(Exception,common.decode_container,"That dog won't hunt, Monsignor")
        self.assertRaises(Exception,common.decode_container)

    def test_string_to_python_identifier(self):
        """Test string_to_python_identifier() function"""

        # Test various successful combinations
        self.assertEqual(common.string_to_python_identifier("Test 1"),"Test_1")
        self.assertEqual(common.string_to_python_identifier("Test.2"),"Test_p_2")
        self.assertEqual(common.string_to_python_identifier("Test-3"),"Test_minus_3")
        self.assertEqual(common.string_to_python_identifier("Test+4"),"Test_plus_4")
        self.assertEqual(common.string_to_python_identifier("1"),"one")
        self.assertEqual(common.string_to_python_identifier(" Test all--even +."),
                                                            "_Test_all_minus__minus_even__plus__p_")
        # Test expected failures
        self.assertRaises(Exception,common.string_to_python_identifier,"else")
        self.assertRaises(Exception,common.string_to_python_identifier,"1 ")
        self.assertRaises(Exception,common.string_to_python_identifier,"0")
        self.assertRaises(Exception,common.string_to_python_identifier,"Disallowed character!")

if __name__ == '__main__':
    unittest.main()
