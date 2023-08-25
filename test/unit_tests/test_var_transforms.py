#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Contains unit tests for variable transforms involving
               a VarCompatObj object

 Assumptions:

 Command line arguments: none

 Usage: python test_var_transform.py         # run the unit tests
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
from framework_env import CCPPFrameworkEnv
from metavar import Var
from parse_tools import ParseContext, ParseSource, ParseSyntaxError
from var_props import VarCompatObj
# pylint: enable=wrong-import-position

class VarCompatTestCase(unittest.TestCase):

    """Tests for variable transforms."""

    def _new_var(self, standard_name, units, dimensions, vtype, vkind=''):
        """Create and return a new Var object with the requested properties"""
        context = ParseContext(linenum=self.__linenum, filename="foo.meta")
        source = ParseSource("foo", "host", context)
        prop_dict = {'local_name' : f"foo{self.__linenum}",
                     'standard_name' : standard_name,
                     'units' : units,
                     'dimensions' : f"({', '.join(dimensions)})",
                     'type' : vtype, 'kind' : vkind}
        self.__linenum += 5
        return Var(prop_dict, source, self.__run_env)

    def setUp(self):
        """Setup variables for testing"""
        self.__run_env = CCPPFrameworkEnv(None, ndict={'host_files':'',
                                                       'scheme_files':'foo.meta',
                                                       'suites':''},
                                          kind_types=["kind_phys=REAL64",
                                                      "kind_dyn=REAL32",
                                                      "kind_host=REAL64"])
        # For making variables unique
        self.__linenum = 2
        # For assert messages
        self.__inst_emsg = "Var.compatible returned a '{}', not a VarCompatObj"

    def test_equiv_vars(self):
        """Test that equivalent variables are reported as equivalent"""
        int_scalar1 = self._new_var('int_stdname1', 'm s-1', [], 'integer')
        int_array1 = self._new_var('int_stdname2', 'm s-1', ['hdim'],
                                   'real', vkind='kind_phys')
        int_array2 = self._new_var('int_stdname2', 'm s-1', ['hdim'],
                                   'real', vkind='kind_host')
        int_array3 = self._new_var('int_stdname2', 'm s-1', ['hdim'],
                                   'real', vkind='REAL64')
        compat = int_scalar1.compatible(int_scalar1, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertTrue(compat)
        self.assertTrue(compat.compat)
        self.assertEqual(compat.incompat_reason, '')
        self.assertFalse(compat.has_kind_transforms)
        self.assertFalse(compat.has_dim_transforms)
        self.assertFalse(compat.has_unit_transforms)
        compat = int_array1.compatible(int_array2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertTrue(compat)
        self.assertTrue(compat.compat)
        self.assertEqual(compat.incompat_reason, '')
        self.assertFalse(compat.has_kind_transforms)
        self.assertFalse(compat.has_dim_transforms)
        self.assertFalse(compat.has_unit_transforms)
        compat = int_array3.compatible(int_array2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertTrue(compat)
        self.assertTrue(compat.compat)
        self.assertEqual(compat.incompat_reason, '')
        self.assertFalse(compat.has_kind_transforms)
        self.assertFalse(compat.has_dim_transforms)
        self.assertFalse(compat.has_unit_transforms)

    def test_incompatible_vars(self):
        """Test that incompatible variables are reported correctly"""
        int_scalar1 = self._new_var('int_stdname1', 'm s-1', [], 'integer')
        int_scalar2 = self._new_var('int_stdname2', 'm s-1', [], 'integer')
        int_array1 = self._new_var('int_stdname1', 'm s-1', ['hdim'],
                                   'integer')
        real_array1 = self._new_var('int_stdname1', 'm s-1', ['hdim'],
                                    'real', vkind='kind_phys')
        # Array and scalar
        compat = int_scalar1.compatible(int_array1, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertFalse(compat)
        self.assertFalse(compat.compat)
        self.assertEqual(compat.incompat_reason, 'dimensions')
        # Variables with different standard names
        compat = int_scalar1.compatible(int_scalar2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertFalse(compat)
        self.assertFalse(compat.compat)
        self.assertEqual(compat.incompat_reason, 'standard names')
        # Variables with different types
        compat = int_array1.compatible(real_array1, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertFalse(compat)
        self.assertFalse(compat.compat)
        self.assertEqual(compat.incompat_reason, 'types')

    def test_valid_unit_change(self):
        """Test that valid unit changes are detected"""
        real_scalar1 = self._new_var('real_stdname1', 'm', [],
                                     'real', vkind='kind_phys')
        real_scalar2 = self._new_var('real_stdname1', 'mm', [],
                                     'real', vkind='kind_phys')
        compat = real_scalar1.compatible(real_scalar2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertFalse(compat)
        self.assertTrue(compat.compat)
        self.assertEqual(compat.incompat_reason, '')
        self.assertFalse(compat.has_kind_transforms)
        self.assertFalse(compat.has_dim_transforms)
        self.assertTrue(compat.has_unit_transforms)

        real_array1 = self._new_var('real_stdname1', 'm s-1', ['hdim', 'vdim'],
                                    'real', vkind='kind_phys')
        real_array2 = self._new_var('real_stdname1', 'km h-1', ['hdim', 'vdim'],
                                    'real', vkind='kind_phys')
        compat = real_scalar1.compatible(real_scalar2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertFalse(compat)
        self.assertTrue(compat.compat)
        self.assertEqual(compat.incompat_reason, '')
        self.assertFalse(compat.has_kind_transforms)
        self.assertFalse(compat.has_dim_transforms)
        self.assertTrue(compat.has_unit_transforms)

    def test_unsupported_unit_change(self):
        """Test that unsupported unit changes are detected"""
        real_scalar1 = self._new_var('real_stdname1', 'min', [],
                                     'real', vkind='kind_phys')
        real_scalar2 = self._new_var('real_stdname1', 'd', [],
                                     'real', vkind='kind_phys')
        char_nounit1 = self._new_var('char_stdname1', 'none', [],
                                     'character', vkind='len=256')
        char_nounit2 = self._new_var('char_stdname1', '1', [],
                                     'character', vkind='len=256')
        with self.assertRaises(ParseSyntaxError) as context:
            compat = real_scalar1.compatible(real_scalar2, self.__run_env)
        # end with
        #Test bad conversion for real time variables
        #Verify correct error message returned
        emsg = "Unsupported unit conversion, 'min' to 'd' for 'real_stdname1'"
        self.assertTrue(emsg in str(context.exception))
        #Test bad conversion for unitless variables
        with self.assertRaises(ParseSyntaxError) as context:
            compat = char_nounit1.compatible(char_nounit2, self.__run_env)
        # end with
        #Verify correct error message returned
        emsg = "Unsupported unit conversion, 'none' to '1' for 'char_stdname1'"
        self.assertTrue(emsg in str(context.exception))

    def test_valid_kind_change(self):
        """Test that valid kind changes are detected"""
        real_scalar1 = self._new_var('real_stdname1', 'mm', [],
                                     'real', vkind='kind_phys')
        real_scalar2 = self._new_var('real_stdname1', 'mm', [],
                                     'real', vkind='kind_dyn')
        compat = real_scalar1.compatible(real_scalar2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertFalse(compat)
        self.assertTrue(compat.compat)
        self.assertEqual(compat.incompat_reason, '')
        self.assertTrue(compat.has_kind_transforms)
        self.assertFalse(compat.has_dim_transforms)
        self.assertFalse(compat.has_unit_transforms)

        real_scalar1 = self._new_var('real_stdname1', 'm', [],
                                     'real', vkind='kind_phys')
        real_scalar2 = self._new_var('real_stdname1', 'mm', [],
                                   'real', vkind='REAL32')
        compat = real_scalar1.compatible(real_scalar2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertFalse(compat)
        self.assertTrue(compat.compat)
        self.assertEqual(compat.incompat_reason, '')
        self.assertTrue(compat.has_kind_transforms)
        self.assertFalse(compat.has_dim_transforms)
        self.assertTrue(compat.has_unit_transforms)

    def test_valid_dim_change(self):
        """Test that valid dimension changes are detected"""
        real_array1 = self._new_var('real_stdname1', 'C',
                                    ['horizontal_dimension',
                                     'vertical_layer_dimension'],
                                     'real', vkind='kind_phys')
        real_array2 = self._new_var('real_stdname1', 'K',
                                    ['ccpp_constant_one:horizontal_loop_extent',
                                     'vertical_layer_dimension'],
                                    'real', vkind='kind_dyn')
        compat = real_array1.compatible(real_array2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertFalse(compat)
        self.assertTrue(compat.compat)
        self.assertEqual(compat.incompat_reason, '')
        self.assertTrue(compat.has_kind_transforms)
        self.assertTrue(compat.has_dim_transforms)
        self.assertTrue(compat.has_unit_transforms)

        real_array1 = self._new_var('real_stdname1', 'C',
                                    ['ccpp_constant_one:horizontal_dimension',
                                     'vertical_layer_dimension'],
                                     'real', vkind='kind_phys')
        real_array2 = self._new_var('real_stdname1', 'K',
                                    ['vertical_layer_dimension',
                                     'horizontal_loop_extent'],
                                    'real', vkind='kind_dyn')
        compat = real_array1.compatible(real_array2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        self.assertFalse(compat)
        self.assertTrue(compat.compat)
        self.assertEqual(compat.incompat_reason, '')
        self.assertTrue(compat.has_kind_transforms)
        self.assertTrue(compat.has_dim_transforms)
        self.assertTrue(compat.has_unit_transforms)

    def test_valid_dim_transforms(self):
        """Test that valid variable transform code is created"""
        real_array1 = self._new_var('real_stdname1', 'C',
                                    ['horizontal_dimension',
                                     'vertical_layer_dimension'],
                                     'real', vkind='kind_phys')
        real_array2 = self._new_var('real_stdname1', 'C',
                                    ['ccpp_constant_one:horizontal_loop_extent',
                                     'vertical_layer_dimension'],
                                    'real', vkind='kind_phys')
        real_array3 = self._new_var('real_stdname1', 'K',
                                    ['ccpp_constant_one:horizontal_loop_extent',
                                     'vertical_layer_dimension'],
                                    'real', vkind='kind_phys')
        real_array4 = self._new_var('real_stdname1', 'K',
                                    ['ccpp_constant_one:horizontal_loop_extent',
                                     'vertical_layer_dimension'],
                                    'real', vkind='kind_dyn')
        real_array5 = self._new_var('real_stdname1', 'K',
                                    ['vertical_layer_dimension',
                                     'ccpp_constant_one:horizontal_dimension'],
                                    'real', vkind='kind_phys')
        v1_lname = real_array1.get_prop_value('local_name')
        v2_lname = real_array2.get_prop_value('local_name')
        v3_lname = real_array3.get_prop_value('local_name')
        v4_lname = real_array4.get_prop_value('local_name')
        v5_lname = real_array5.get_prop_value('local_name')
        # Comparison between equivalent variables
        compat = real_array1.compatible(real_array2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        rindices = ("hind", "vind")
        fwd_stmt = compat.forward_transform(v2_lname, v1_lname, rindices,
                                            adjust_hdim=None, flip_vdim=None)
        ind_str = ','.join(rindices)
        expected = f"{v2_lname}({ind_str}) = {v1_lname}({ind_str})"
        self.assertEqual(fwd_stmt, expected)
        rev_stmt = compat.reverse_transform(v1_lname, v2_lname, rindices,
                                            adjust_hdim=None, flip_vdim=None)
        expected = f"{v1_lname}({ind_str}) = {v2_lname}({ind_str})"
        self.assertEqual(rev_stmt, expected)

        # Comparison between equivalent variables with loop correction
        compat = real_array1.compatible(real_array2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        rindices = ("hind", "vind")
        lindices = ("hind-col_start+1", "vind")
        fwd_stmt = compat.forward_transform(v2_lname, v1_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim=None)
        lind_str = ','.join(lindices)
        rind_str = ','.join(rindices)
        expected = f"{v2_lname}({lind_str}) = {v1_lname}({rind_str})"
        self.assertEqual(fwd_stmt, expected)
        rev_stmt = compat.reverse_transform(v1_lname, v2_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim=None)
        lindices = ("hind+col_start-1", "vind")
        lind_str = ','.join(lindices)
        expected = f"{v1_lname}({lind_str}) = {v2_lname}({rind_str})"
        self.assertEqual(rev_stmt, expected)

        # Comparison between equivalent variables with loop correction
        #   plus vertical flip
        compat = real_array1.compatible(real_array2, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        rindices = ("hind", "vind")
        lindices = ("hind-col_start+1", "pver-vind+1")
        fwd_stmt = compat.forward_transform(v2_lname, v1_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim='pver')
        lind_str = ','.join(lindices)
        rind_str = ','.join(rindices)
        expected = f"{v2_lname}({lind_str}) = {v1_lname}({rind_str})"
        self.assertEqual(fwd_stmt, expected)
        rev_stmt = compat.reverse_transform(v1_lname, v2_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim='pver')
        lindices = ("hind+col_start-1", "pver-vind+1")
        lind_str = ','.join(lindices)
        expected = f"{v1_lname}({lind_str}) = {v2_lname}({rind_str})"
        self.assertEqual(rev_stmt, expected)

        # Comparison between variables with different units
        compat = real_array1.compatible(real_array3, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        rindices = ("hind", "vind")
        lindices = ("hind-col_start+1", "vind")
        conv = f"273.15_{real_array1.get_prop_value('kind')}"
        fwd_stmt = compat.forward_transform(v3_lname, v1_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim=None)
        lind_str = ','.join(lindices)
        rind_str = ','.join(rindices)
        expected = f"{v3_lname}({lind_str}) = {v1_lname}({rind_str})+{conv}"
        self.assertEqual(fwd_stmt, expected)
        rev_stmt = compat.reverse_transform(v1_lname, v3_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim=None)
        lindices = ("hind+col_start-1", "vind")
        lind_str = ','.join(lindices)
        conv = f"273.15_{real_array2.get_prop_value('kind')}"
        expected = f"{v1_lname}({lind_str}) = {v3_lname}({rind_str})-{conv}"
        self.assertEqual(rev_stmt, expected)

        # Comparison between variables with different kind
        compat = real_array4.compatible(real_array3, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        rindices = ("hind", "vind")
        lindices = ("hind", "vind")
        fwd_stmt = compat.forward_transform(v4_lname, v3_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim=None)
        lind_str = ','.join(lindices)
        rind_str = ','.join(rindices)
        rkind = real_array3.get_prop_value('kind')
        expected = f"{v4_lname}({lind_str}) = real({v3_lname}({rind_str}), {rkind})"
        self.assertEqual(fwd_stmt, expected)
        rev_stmt = compat.reverse_transform(v3_lname, v4_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim=None)
        lindices = ("hind", "vind")
        lind_str = ','.join(lindices)
        rkind = real_array4.get_prop_value('kind')
        expected = f"{v3_lname}({lind_str}) = real({v4_lname}({rind_str}), {rkind})"
        self.assertEqual(rev_stmt, expected)

        # Comparison between variables with different units and kind
        compat = real_array1.compatible(real_array4, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        rindices = ("hind", "vind")
        lindices = ("hind-col_start+1", "vind")
        rkind = real_array4.get_prop_value('kind')
        conv = f"273.15_{rkind}"
        fwd_stmt = compat.forward_transform(v2_lname, v1_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim=None)
        lind_str = ','.join(lindices)
        rind_str = ','.join(rindices)
        expected = f"{v2_lname}({lind_str}) = real({v1_lname}({rind_str}), {rkind})+{conv}"
        self.assertEqual(fwd_stmt, expected)
        rev_stmt = compat.reverse_transform(v1_lname, v2_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim=None)
        lindices = ("hind+col_start-1", "vind")
        lind_str = ','.join(lindices)
        rkind = real_array1.get_prop_value('kind')
        conv = f"273.15_{rkind}"
        expected = f"{v1_lname}({lind_str}) = real({v2_lname}({rind_str}), {rkind})-{conv}"
        self.assertEqual(rev_stmt, expected)

        # Comparison between variables with different dimension ordering
        #   and horizontal loop adjustment and vertical flip
        compat = real_array5.compatible(real_array3, self.__run_env)
        self.assertIsInstance(compat, VarCompatObj,
                              msg=self.__inst_emsg.format(type(compat)))
        rindices = ("hind", "vind")
        lindices = ("pver-vind+1", "hind-col_start+1")
        fwd_stmt = compat.forward_transform(v4_lname, v5_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim='pver')
        lind_str = ','.join(lindices)
        rind_str = ','.join(rindices)
        rkind = real_array3.get_prop_value('kind')
        expected = f"{v4_lname}({lind_str}) = {v5_lname}({rind_str})"
        self.assertEqual(fwd_stmt, expected)
        rindices = ("vind", "hind")
        rind_str = ','.join(rindices)
        rev_stmt = compat.reverse_transform(v5_lname, v4_lname, rindices,
                                            adjust_hdim='col_start',
                                            flip_vdim='pver')
        lindices = ("hind+col_start-1", "pver-vind+1")
        lind_str = ','.join(lindices)
        rkind = real_array4.get_prop_value('kind')
        expected = f"{v5_lname}({lind_str}) = {v4_lname}({rind_str})"
        self.assertEqual(rev_stmt, expected)

if __name__ == "__main__":
    unittest.main()

