#! /usr/bin/env python
#-----------------------------------------------------------------------
# Description:  Contains unit tests for parse_metadata_file in the class
#               MetadataTable in scripts file metadata_table.py
#
# Assumptions:  
#
# Command line arguments: none
#
# Usage: python test_metadata_table.py         # run the unit tests
#-----------------------------------------------------------------------
import sys
import os
import logging
import unittest

unit_test_dir = os.path.dirname(os.path.abspath(__file__))
scripts_dir = os.path.join(unit_test_dir, "../../scripts")
sample_files_dir =  os.path.join(unit_test_dir, "sample_files")

if not os.path.exists(scripts_dir):
    raise ImportError("Cannot find scripts directory")

sys.path.append(scripts_dir)

from metadata_table import MetadataTable

'''Test parse_metadata_file in metadata_table.py'''

class MetadataTableTestCase(unittest.TestCase):

   """Tests for `parse_metadata_file`."""

   def test_good_host_file(self):
       """Test that good host file test_host.meta returns one header named test_host"""
       #Setup
       known_ddts = list()
       logger = None
       filename= sample_files_dir + "/test_host.meta"
       #Exercise
       result = MetadataTable.parse_metadata_file(filename, known_ddts, logger)
       #Verify that size of returned list equals number of headers in the test file
       #       and that header name is 'test_host'
       self.assertEqual(len(result), 1)
       listToStr = " ".join([str(elem) for elem in result])
       self.assertIn('test_host', listToStr, msg="Header name is not expected 'test_host'")

   def test_bad_type_name(self):
       """Test that `type = banana` returns expected error"""
       #Setup
       known_ddts = list()
       logger = None
       filename= sample_files_dir + "/test_bad_type_name.meta"

       #Exercise
       with self.assertRaises(Exception) as context:
           MetadataTable.parse_metadata_file(filename, known_ddts, logger)

       #Verify
       #print("The exception is", context.exception)
       self.assertTrue('Invalid metadata table type, \'banana' in str(context.exception))

if __name__ == '__main__':
    unittest.main()
