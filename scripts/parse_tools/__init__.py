"""Public API for the parse_tools library
"""
from __future__ import absolute_import
import sys
import os.path
sys.path.insert(0, os.path.dirname(__file__))

# pylint: disable=wrong-import-position
from parse_source import ParseContext, ParseSource
from parse_source import ParseSyntaxError, ParseInternalError
from parse_source import CCPPError, context_string, type_name
from parse_source import unique_standard_name, reset_standard_name_counter
from parse_object import ParseObject
from parse_checkers import check_fortran_id, FORTRAN_ID
from parse_checkers import FORTRAN_DP_RE
from parse_checkers import FORTRAN_SCALAR_REF, FORTRAN_SCALAR_REF_RE
from parse_checkers import check_fortran_ref, check_fortran_literal
from parse_checkers import check_fortran_intrinsic, check_local_name
from parse_checkers import check_diagnostic_id, check_diagnostic_fixed
from parse_checkers import check_fortran_type, check_balanced_paren
from parse_checkers import fortran_list_match
from parse_checkers import registered_fortran_ddt_name
from parse_checkers import register_fortran_ddt_name
from parse_checkers import check_units, check_dimensions, check_cf_standard_name
from parse_checkers import check_default_value, check_valid_values, check_molar_mass
from parse_log import init_log, set_log_level, flush_log
from parse_log import set_log_to_stdout, set_log_to_null
from parse_log import set_log_to_file, debug_enabled
from preprocess import PreprocStack
from xml_tools import find_schema_file, find_schema_version
from xml_tools import read_xml_file, validate_xml_file
from xml_tools import PrettyElementTree
# pylint: enable=wrong-import-position

__all__ = [
    'CCPPError',
    'check_balanced_paren',
    'check_cf_standard_name',
    'check_default_value',
    'check_diagnostic_id',
    'check_diagnostic_fixed',
    'check_dimensions',
    'check_fortran_id',
    'check_fortran_intrinsic',
    'check_fortran_literal',
    'check_fortran_ref',
    'check_fortran_type',
    'check_local_name',
    'check_valid_values',
    'check_molar_mass',
    'context_string',
    'find_schema_file',
    'find_schema_version',
    'flush_log',
    'FORTRAN_DP_RE',
    'FORTRAN_ID',
    'FORTRAN_SCALAR_REF',
    'FORTRAN_SCALAR_REF_RE',
    'init_log',
    'ParseContext',
    'ParseInternalError',
    'ParseSource',
    'ParseSyntaxError',
    'ParseObject',
    'PreprocStack',
    'PrettyElementTree',
    'register_fortran_ddt_name',
    'read_xml_file',
    'registered_fortran_ddt_name',
    'reset_standard_name_counter',
    'set_log_level',
    'set_log_to_file',
    'set_log_to_null',
    'set_log_to_stdout',
    'type_name',
    'unique_standard_name',
    'validate_xml_file'
]
