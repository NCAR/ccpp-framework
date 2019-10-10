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
print("scripts_dir: " + scripts_dir)
if not os.path.exists(scripts_dir):
    raise ImportError("Cannot find scripts directory")

sys.path.append(scripts_dir)

from metadata_table import MetadataTable

'''Test parse_metadata_file in metadata_table.py'''

class MetadataTableTestCase(unittest.TestCase):
   """Tests for `parse_metadata_file`."""

   def test_bad_file(self):
       """I dont know what im doing"""
       #Setup
       #known_ddts = None
       #logger = None
       known_ddts = list()
       logger = None
       filename="/scratch1/BMC/gmtb/Julie.Schramm/ccpp-framework-fork/test/capgen_test/test_host.meta"
       #Exercise
       result = MetadataTable.parse_metadata_file(filename, known_ddts, logger)
       #Verify
       for x in result: 
           print x 

if __name__ == '__main__':
    unittest.main()
