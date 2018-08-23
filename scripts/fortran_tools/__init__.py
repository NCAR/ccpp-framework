"""Public API for the fortran_parser library
"""

__all__ = [
    'parse_fortran_file',
    'FortranWriter'
]

import six
if six.PY3:
    from fortran_tools.parse_fortran_file import parse_fortran_file
    from fortran_tools.fortran_write      import FortranWriter
else:
    from parse_fortran_file import parse_fortran_file
    from fortran_write      import FortranWriter
# End if
