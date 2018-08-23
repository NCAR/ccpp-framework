"""Public API for the fortran_parser library
"""

from fortran_parser import parse_fortran_file
from fortran_utils import is_variable_name

__all__ = [
    'parse_fortran_file',
    'is_variable_name',
]
