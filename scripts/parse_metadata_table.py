#!/usr/bin/env python
"""
The argument tables for schemes and variable definitions should
have a section header line followed by variable declaration sections
followed by a line containing only a blank comment line
Each line begins with a special token. In Fortran, the token is a
double exclamation mark.

There are two styles of section header line:
!! \section arg_table_<name>
!> \section arg_table_<name>

The second form is used to begin a new doxygen section

<name> is the name of the file object which immediately follows the
argument table. It is one of the following possibilities:
- SubroutineName: the name of a subroutine (i.e., the name of
                   a scheme interface function such as SchemeName_run)
- DerivedTypeName: a derived type name for a type which will be used
                   somewhere in the CCPP interface.
- ModuleName: the name of the module whose module variables will be
              used somewhere in the CCPP interface

A variable declaration section begins with a variable name line (a local
variable name enclosed in square brackets) followed by one or more
variable attribute statements.
A variable attribute statement is an attribute name and the value for
that attribute separated by an equal sign. Whitespace is not
significant except inside of strings.
Variable attribute statements may be combined on a line if separated by
a vertical bar.

An example argument table is shown below (aside from the python comment
character at the start of each line).

!> \section arg_table_<name>
!! [ im ]
!! standard_name = horizontal_loop_extent
!! description = horizontal loop extent, start at 1
!! units = index
!! type = integer
!! dimensions = ()
!! intent = in
!! [ ix ]
!! standard_name = horizontal_loop_dimension
!! description = horizontal dimension
!! units = index | type = integer | dimensions = ()
!! intent = in
!! ...
!! [ errmsg]
!! standard_name = ccpp_error_message
!! description = error message for error handling in CCPP
!! units = none
!! type = character
!! len = *
!! dimensions = ()
!! intent = out
!! [ ierr ]
!! standard_name = ccpp_error_flag
!! description = error flag for error handling in CCPP
!! type = integer
!! dimensions = ()
!! intent=out
!!
Notes on the input format:
- SubroutineName must match the name of the subroutine that the argument
  table describes
- DerivedTypeName must match the name of the derived type that the argument
  table describes
- ModuleName must match the name of the module whose variables the argument
  table describes
- the table must be placed immediately before the subroutine / derived
  data type, or immediately before the module variables (but within the
  module structure)
- each line of the table must begin with the language-dependent
  doxygen-delimiter ('!!' for Fortran)
- after the last row of the table, there must be a blank doxygen line
  (only the delimiter) to denote the end of the table
- for variable type definitions and module variables, the intent and
  optional columns are not functional and should be omitted
- each argument table (and its subroutine) must accept the following two arguments for error handling:
     - character(len=512), intent(out) :: errmsg
         - errmsg must be initialized as '' and contains the error message in case an error occurs
     - integer, intent(out) :: ierr
         - ierr must be initialized as 0 and set to >1 in case of errors
Output: This routine converts the argument tables for all subroutines / typedefs / module variables into an XML file
suitable to be used with mkcap.py (which generates the fortran code for the scheme cap)
- the script generates a separate file for each module within the given files
"""

from __future__ import print_function
import re
import collections
import logging
from metavar import VariableProps, Var
from parse_tools import ParseSyntaxError, ParseObject
from parse_tools import MetadataSyntax, FortranMetadataSyntax

logger = logging.getLogger(__name__)

########################################################################

########################################################################

class MetadataHeader(ParseObject):
    """Class to hold all information from a metadata header
    >>> MetadataHeader("foobar.txt",                                      \
                      ["!> \section arg_table_foobar", "!! [ im ]",           \
                       "!! standard_name = horizontal_loop_extent",           \
                       "!! description = horizontal loop extent, start at 1", \
                       "!! units = index | type = integer",                   \
                       "!! dimensions = () |  intent = in"], 0,               \
                       syntax=FortranMetadataSyntax).get_var('horizontal_loop_extent') #doctest: +ELLIPSIS
    <metavar.Var object at 0x...>
    >>> MetadataHeader("foobar.txt",                                      \
                      ["!> \section arg_table_foobar", "!! [ im ]",           \
                       "!! standard_name = horizontal_loop_extent",           \
                       "!! description = horizontal loop extent, start at 1", \
                       "!! units = index | type = integer",                   \
                       "!! dimensions = () |  intent = in"], 0,               \
                       syntax=FortranMetadataSyntax).get_var('horizontal_loop_extent').get_prop_value('local_name')
    'im'
    >>> MetadataHeader("foobar.txt",                                      \
                      ["!> \section arg_table_foobar", "!! [ im ]",           \
                       "!! standard_name = horizontal loop extent",           \
                       "!! description = horizontal loop extent, start at 1", \
                       "!! units = index | type = integer",                   \
                       "!! dimensions = () |  intent = in"], 0,               \
                       syntax=FortranMetadataSyntax).get_var('horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid variable property value, 'horizontal loop extent', at foobar.txt:2
"""

    _header_start = re.compile(r"\\section\s+arg_table_([A-Za-z][A-Za-z0-9_]*)")

    _var_start = re.compile(r"^\[\s*(\w+)\s*\]$")

    def __init__(self, filename, lines, first_line,
                 syntax=FortranMetadataSyntax):
        super(MetadataHeader, self).__init__(filename, lines, line_start=first_line, syntax=syntax)
        "Initialize from the <lines> of <filename> beginning at <first_line>"
        _llevel = logger.getEffectiveLevel()
        logger.setLevel(logging.ERROR)
        self._variables = {}
        # Read the table preamble, assume the caller already figured out
        #  the first line of the header using the metadata_table_start method.
        curr_line, curr_line_num = self.curr_line()
        self._table_title = MetadataHeader.metadata_table_start(curr_line, syntax=self._syntax)
        if self._table_title is None:
            raise ParseSyntaxError("metadata header start",
                                   token=curr_line, context=self._context)
        # End if
        curr_line, curr_line_num = self.next_line()
        # Skip past any 'blank' lines
        while syntax.blank_line(curr_line):
            curr_line, curr_line_num = self.next_line()
        # End while
        # Read the variables
        valid_lines = True
        self._variables = {}
        while valid_lines:
            newvar = self.parse_variable(lines)
            valid_lines = newvar is not None
            if valid_lines:
                new_sn = newvar.get_prop_value('standard_name')
                if new_sn in self._variables:
                    raise ParseSyntaxError("Duplicate standard name",
                                           token=newvar.standard_name,
                                           context=self._context)
                else:
                    self._variables[new_sn] = newvar
                # End if
            # End if
        # End while

        logger.setLevel(logging.WARNING if _llevel == logging.NOTSET else _llevel)

    def parse_variable(self, lines):
        # Make sure first_line is a valid variable start
        curr_line, curr_line_num = self.curr_line()
        # The header line has the format [ <valid_fortran_symbol> ]
        # Parse header
        valid_line = curr_line is not None
        if valid_line:
            self._local_name = self.variable_start(curr_line) # caller handles exception
        else:
            self._local_name = None
        # End if
        if valid_line and (self._local_name is None):
            raise ParseSyntaxError("invalid metadata variable start",
                                   token=curr_line, context=self._context)
        # End if
        # Parse lines until invalid line is found
        # NB: Header variables cannot have embedded blank lines
        if valid_line:
            var_props = {}
            var_props['local_name'] = self._local_name
        else:
            var_props = None
        # End if
        while valid_line:
            curr_line, curr_line_num = self.next_line()
            valid_line = (curr_line is not None) and (not self.blank_line(curr_line))
            # A valid line may have multiple properties (separated by '|')
            if valid_line:
                properties = self._syntax.strip(curr_line).split('|')
                for property in properties:
                    pitems = property.split('=')
                    if len(pitems) != 2:
                        raise ParseSyntaxError("variable property syntax",
                                               token=property,
                                               context=self._context)
                    # End if
                    try:
                        pname = pitems[0].strip()
                        pval_str = pitems[1].strip()
                        # Make sure this is a match
                        hp = Var.get_prop(pname)
                        if hp is not None:
                            pval = hp.valid_value(pval_str)
                        else:
                            raise ParseSyntaxError("variable property name",
                                                   token=pname,
                                                   context=self._context)
                        # End if
                        if pval is None:
                            raise ParseSyntaxError("variable property value",
                                                   token=pval_str,
                                                   context=self._context)
                        # End if
                    except ParseSyntaxError as p:
                        raise p
                    # If we get this far, we have a valid property.
                    var_props[pname] = pval
                # End for
            # End if
        # End while
        if var_props is None:
            return None
        else:
            return Var(var_props)

    def get_var(self, standard_name):
        if standard_name in self._variables:
            return self._variables[standard_name]
        else:
            return None

    def variable_start(self, line):
        """Return variable name if <line> is an interface metadata table header
        """
        match = MetadataHeader._var_start.match(self._syntax.strip(line))
        if match is not None:
            name = match.group(1)
            if not self._syntax.is_variable_name(name):
                raise ParseSyntaxError("Invalid local variable name",
                                       token=name, context=self._context)
            # End if
        else:
            name = None
        # End if
        return name

    @classmethod
    def metadata_table_start(cls, line, context=None, syntax=FortranMetadataSyntax):
        """Return variable name if <line> is an interface metadata table header
        """
        ts = syntax.table_start(line)
        if ts is not None:
            match = MetadataHeader._header_start.match(syntax.strip(line))
            if match is not None:
                name = match.group(1)
                if not syntax.is_variable_name(name):
                    raise ParseSyntaxError("Invalid arg_table name",
                                           token=name, context=context)
                # End if
            else:
                name = None
                raise ValueError("Match is none")
            # End if
        else:
            name = None
            raise ValueError(line)
        # End if
        return name

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
