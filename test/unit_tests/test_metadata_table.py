#! /usr/bin/env python
"""
-----------------------------------------------------------------------
 Description:  Contains unit tests for parse_metadata_file in the class
               MetadataTable in scripts file metadata_table.py

 Assumptions:

 Command line arguments: none

 Usage: python test_metadata_table.py         # run the unit tests
-----------------------------------------------------------------------
"""
import sys
import os
import unittest

test_dir = os.path.dirname(os.path.abspath(__file__))
scripts_dir = os.path.abspath(os.path.join(test_dir, os.pardir, os.pardir, "scripts"))
sample_files_dir = os.path.join(test_dir, "sample_files")

if not os.path.exists(scripts_dir):
    raise ImportError("Cannot find scripts directory")

sys.path.append(scripts_dir)

# pylint: disable=wrong-import-position
from metadata_table import MetadataTable
# pylint: enable=wrong-import-position

class MetadataTableTestCase(unittest.TestCase):

    """Tests for `parse_metadata_file`."""

    def test_good_host_file(self):
        """Test that good host file test_host.meta returns one header named test_host"""
        #Setup
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_host.meta")
        #Exercise
        result = MetadataTable.parse_metadata_file(filename, known_ddts, logger)
        #Verify that size of returned list equals number of headers in the test file
        #       and that header name is 'test_host'
        self.assertEqual(len(result), 1)
        titles = [elem.title for elem in result]
        self.assertIn('test_host', titles, msg="Header name 'test_host' is expected but not found")

    def test_good_multi_ccpp_arg_table(self):
        """Test that good file with 4 ccpp-arg-table returns 4 headers"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_multi_ccpp_arg_tables.meta")

        result = MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #Verify that size of returned list equals number of headers in the test file
        self.assertEqual(len(result), 4)
        titles = [elem.title for elem in result]
        #print(titles)
        self.assertIn('vmr_type', titles, msg="Header name 'vmr_type' is expected but not found")
        self.assertIn('make_ddt_run', titles, msg="Header name 'make_ddt_run' is expected but not found")
        self.assertIn('make_ddt_init', titles, msg="Header name 'make_ddt_init' is expected but not found")
        self.assertIn('make_ddt_finalize', titles, msg="Header name 'make_ddt_finalize' is expected but not found")

    def test_bad_type_name(self):
        """Test that `type = banana` returns expected error"""
        #Setup
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_bad_type_name.meta")

        #Exercise
        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #Verify
        #print("The exception is", context.exception)
        self.assertTrue('Invalid metadata table type, \'banana' in str(context.exception))

    def test_double_header(self):
        """Test that a duplicate header returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "double_header.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        self.assertTrue('Duplicate metadata header, test_host' in str(context.exception))

    def test_bad_dimension(self):
        """Test that `dimension = banana` returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_bad_dimension.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        self.assertTrue('Invalid \'dimensions\' property value, \'' in str(context.exception))

    def test_duplicate_variable(self):
        """Test that a duplicate variable returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_duplicate_variable.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        self.assertTrue('Invalid (duplicate) standard name in temp_calc_adjust_run, defined at ' in str(context.exception))

    def test_invalid_intent(self):
        """Test that an invalid intent returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_invalid_intent.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        self.assertTrue('Invalid \'intent\' property value, \'banana\', at ' in str(context.exception))

    def test_missing_intent(self):
        """Test that a missing intent returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_missing_intent.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Required property, 'intent', missing, at "
        self.assertTrue(emsg in str(context.exception))

    def test_missing_table_type(self):
        """Test that a missing table type returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_missing_table_type.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid metadata header start, no table type"
        self.assertTrue(emsg in str(context.exception))

    def test_missing_table_name(self):
        """Test that a missing table name returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_missing_table_name.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid metadata header start, no table name"
        self.assertTrue(emsg in str(context.exception))

    def test_bad_table_key(self):
        """Test that a bad table key returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_bad_table_key.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid metadata table start property, 'something', at "
        self.assertTrue(emsg in str(context.exception))

    def test_bad_line_split(self):
        """Test that a bad split line with | returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_bad_line_split.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid variable property syntax, \'\', at "
        self.assertTrue(emsg in str(context.exception))

    def test_unknown_ddt_type(self):
        """Test that a ddt_type = banana returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_unknown_ddt_type.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Unknown DDT type, banana, at "
        self.assertTrue(emsg in str(context.exception))

    def test_bad_var_property_name(self):
        """Test that a ddt_type = None returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_bad_var_property_name.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid variable property name, 'none', at "
        self.assertTrue(emsg in str(context.exception))

    def test_no_input(self):
        """Test that no input returns expected error"""
        with self.assertRaises(Exception) as context:
            MetadataTable()

        #print("The exception is", context.exception)
        emsg = "MetadataTable requires a title"
        self.assertTrue(emsg in str(context.exception))

    def test_no_header_type(self):
        """Test that __init__ with type_in=None returns expected error"""
        with self.assertRaises(Exception) as context:
            MetadataTable(parse_object=None, title="something", type_in=None, \
                module=None, var_dict=None, known_ddts=None, logger=None)

       #print("The exception is", context.exception)
        emsg = "MetadataTable requires a header type"
        self.assertTrue(emsg in str(context.exception))

    def test_bad_header_type(self):
        """Test that __init__ with type_in=banana returns expected error"""
        with self.assertRaises(Exception) as context:
            MetadataTable(parse_object=None, title="something", type_in="banana", \
                module=None, var_dict=None, known_ddts=None, logger=None)

        #print("The exception is", context.exception)
        emsg = "Invalid metadata table type, 'banana'"
        self.assertTrue(emsg in str(context.exception))

    def test_no_module(self):
        """Test that __init__ with module=None returns expected error"""
        with self.assertRaises(Exception) as context:
            MetadataTable(parse_object=None, title="something", type_in="scheme", \
                module=None, var_dict=None, known_ddts=None, logger=None)

        #print("The exception is", context.exception)
        emsg = "MetadataTable requires a module name"
        self.assertTrue(emsg in str(context.exception))

    def test_bad_1st_ccpp_arg_table(self):
        """Test that first arg table named ccpp-farg-table returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_bad_1st_arg_table_header.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid CCPP metadata line, '[ccpp-farg-table]', at "
        self.assertTrue(emsg in str(context.exception))

    def test_bad_2nd_ccpp_arg_table(self):
        """Test that second arg table named ccpp-farg-table returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(sample_files_dir, "test_bad_2nd_arg_table_header.meta")

        with self.assertRaises(Exception) as context:
            MetadataTable.parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid variable property syntax, '[ccpp-farg-table]', at "
        self.assertTrue(emsg in str(context.exception))

if __name__ == '__main__':
    unittest.main()
