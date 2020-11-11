"""Public API for the fortran_parser library
"""

try:
    from parse_fortran_file import parse_fortran_file
    from parse_fortran import parse_fortran_var_decl, fortran_type_definition
    from fortran_write import FortranWriter
except ModuleNotFoundError:
    from .parse_fortran_file import parse_fortran_file
    from .parse_fortran import parse_fortran_var_decl, fortran_type_definition
    from .fortran_write import FortranWriter
# end try

__all__ = [
    'fortran_type_definition',
    'parse_fortran_file',
    'parse_fortran_var_decl',
    'FortranWriter'
]
