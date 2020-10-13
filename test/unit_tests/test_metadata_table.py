#! /usr/bin/env python
"""
-----------------------------------------------------------------------
 Description:  Contains unit tests for parse_metadata_file
               in scripts file metadata_table.py

 Assumptions:

 Command line arguments: none

 Usage: python test_metadata_table.py         # run the unit tests
-----------------------------------------------------------------------
"""
import sys
import os
import unittest

TEST_DIR = os.path.dirname(os.path.abspath(__file__))
SCRIPTS_DIR = os.path.abspath(os.path.join(TEST_DIR, os.pardir, os.pardir, "scripts"))
SAMPLE_FILES_DIR = os.path.join(TEST_DIR, "sample_files")

if not os.path.exists(SCRIPTS_DIR):
    raise ImportError("Cannot find scripts directory")

sys.path.append(SCRIPTS_DIR)

# pylint: disable=wrong-import-position
from metadata_table import parse_metadata_file, MetadataTable
# pylint: enable=wrong-import-position

class MetadataTableTestCase(unittest.TestCase):

    """Tests for `parse_metadata_file`."""

    def test_good_host_file(self):
        """Test that good host file test_host.meta returns one header named test_host"""
        #Setup
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_host.meta")
        #Exercise
        result = parse_metadata_file(filename, known_ddts, logger)
        #Verify that:
        #       no dependencies is returned as ''
        #       rel_path is returned as None
        #       size of returned list equals number of headers in the test file
        #       ccpp-table-properties name is 'test_host'
        dependencies = result[0].dependencies
        rel_path = result[0].relative_path
        self.assertTrue('' in dependencies)
        self.assertIsNone(rel_path)
        self.assertEqual(len(result), 1)
        titles = [elem.table_name for elem in result]
        self.assertIn('test_host', titles, msg="Header name 'test_host' is expected but not found")

    def test_good_multi_ccpp_arg_table(self):
        """Test that good file with 4 ccpp-arg-table returns 4 headers"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_multi_ccpp_arg_tables.meta")
        #Exercise
        result = parse_metadata_file(filename, known_ddts, logger)
        #Verify that size of returned list equals number of ccpp-table-properties in the test file
        # ccpp-arg-tables are returned in result[0].sections() and result[1].sections()
        self.assertEqual(len(result), 2)

        titles = list()
        for table in result:
            titles.extend([x.title for x in table.sections()])

        self.assertIn('vmr_type', titles, msg="Header name 'vmr_type' is expected but not found")
        self.assertIn('make_ddt_run', titles, msg="Header name 'make_ddt_run' is expected but not found")
        self.assertIn('make_ddt_init', titles, msg="Header name 'make_ddt_init' is expected but not found")
        self.assertIn('make_ddt_finalize', titles, msg="Header name 'make_ddt_finalize' is expected but not found")

    def test_bad_type_name(self):
        """Test that `type = banana` returns expected error"""
        #Setup
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_bad_type_name.meta")

        #Exercise
        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #Verify
        #print("The exception is", context.exception)
        self.assertTrue('Invalid metadata table type, \'banana' in str(context.exception))

    def test_double_header(self):
        """Test that a duplicate header returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "double_header.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        self.assertTrue('table already contains \'test_host\'' in str(context.exception))

    def test_bad_dimension(self):
        """Test that `dimension = banana` returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_bad_dimension.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        self.assertTrue('Invalid \'dimensions\' property value, \'' in str(context.exception))

    def test_duplicate_variable(self):
        """Test that a duplicate variable returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_duplicate_variable.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        self.assertTrue('Invalid (duplicate) standard name in temp_calc_adjust_run, defined at ' in str(context.exception))

    def test_invalid_intent(self):
        """Test that an invalid intent returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_invalid_intent.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        self.assertTrue('Invalid \'intent\' property value, \'banana\', at ' in str(context.exception))

    def test_missing_intent(self):
        """Test that a missing intent returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_missing_intent.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Required property, 'intent', missing, at "
        self.assertTrue(emsg in str(context.exception))

    def test_missing_table_type(self):
        """Test that a missing table type returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_missing_table_type.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid metadata header start, no table type"
        self.assertTrue(emsg in str(context.exception))

    def test_missing_table_name(self):
        """Test that a missing table name returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_missing_table_name.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid metadata header start, no table name"
        self.assertTrue(emsg in str(context.exception))

    def test_bad_table_key(self):
        """Test that a bad table key returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_bad_table_key.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid metadata table start property, 'something', at "
        self.assertTrue(emsg in str(context.exception))

    def test_bad_line_split(self):
        """Test that a bad split line with | returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_bad_line_split.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid variable property syntax, \'\', at "
        self.assertTrue(emsg in str(context.exception))

    def test_unknown_ddt_type(self):
        """Test that a ddt_type = banana returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_unknown_ddt_type.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Unknown DDT type, banana, at "
        self.assertTrue(emsg in str(context.exception))

    def test_bad_var_property_name(self):
        """Test that a ddt_type = None returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_bad_var_property_name.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid variable property name, 'none', at "
        self.assertTrue(emsg in str(context.exception))

    def test_no_input(self):
        """Test that no input returns expected error"""
        with self.assertRaises(Exception) as context:
            MetadataTable()

        #print("The exception is", context.exception)
        emsg = "MetadataTable requires a name"
        self.assertTrue(emsg in str(context.exception))

    def test_no_table_type(self):
        """Test that __init__ with table_type_in=None returns expected error"""
        with self.assertRaises(Exception) as context:
            MetadataTable(table_name_in="something", table_type_in=None, dependencies=None, \
                relative_path=None, known_ddts=None, var_dict=None, module=None, \
                parse_object=None, logger=None)

        #print("The exception is", context.exception)
        emsg = "MetadataTable requires a table type"
        self.assertTrue(emsg in str(context.exception))

    def test_bad_header_type(self):
        """Test that __init__ with table_type_in=banana returns expected error"""
        with self.assertRaises(Exception) as context:
            MetadataTable(table_name_in="something", table_type_in="banana", dependencies=None, \
                relative_path=None, known_ddts=None, var_dict=None, module=None, \
                parse_object=None, logger=None)

        #print("The exception is", context.exception)
        emsg = "Invalid metadata arg table type, 'banana'"
        self.assertTrue(emsg in str(context.exception))

    def test_no_module(self):
        """Test that __init__ with module=None returns expected error"""
        with self.assertRaises(Exception) as context:
            MetadataTable(table_name_in=None, table_type_in=None, dependencies=None, \
                relative_path=None, known_ddts=None, var_dict=None, module=None, \
                parse_object=None, logger=None)

        #print("The exception is", context.exception)
        emsg = "MetadataTable requires a name"
        self.assertTrue(emsg in str(context.exception))

    def test_bad_1st_ccpp_arg_table(self):
        """Test that first arg table named ccpp-farg-table returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_bad_1st_arg_table_header.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid variable property syntax, '[ccpp-farg-table]', at "
        self.assertTrue(emsg in str(context.exception))

    def test_bad_2nd_ccpp_arg_table(self):
        """Test that second arg table named ccpp-farg-table returns expected error"""
        known_ddts = list()
        logger = None
        filename = os.path.join(SAMPLE_FILES_DIR, "test_bad_2nd_arg_table_header.meta")

        with self.assertRaises(Exception) as context:
            parse_metadata_file(filename, known_ddts, logger)

        #print("The exception is", context.exception)
        emsg = "Invalid variable property syntax, '[ccpp-farg-table]', at "
        self.assertTrue(emsg in str(context.exception))

if __name__ == '__main__':
    unittest.main()
