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
SCRIPTS_DIR = os.path.abspath(os.path.join(TEST_DIR, os.pardir, os.pardir, "scripts"))

if not os.path.exists(SCRIPTS_DIR):
    raise ImportError("Cannot find scripts directory")

sys.path.append(SCRIPTS_DIR)

# pylint: disable=wrong-import-position
import common
# pylint: enable=wrong-import-position

class CommonTestCase(unittest.TestCase):

    """Tests functionality of functions in common.py"""

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
