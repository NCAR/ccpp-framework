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
!! long_name = horizontal loop extent, start at 1
!! units = index
!! type = integer
!! dimensions = ()
!! intent = in
!! [ ix ]
!! standard_name = horizontal_loop_dimension
!! long_name = horizontal dimension
!! units = index | type = integer | dimensions = ()
!! intent = in
!! ...
!! [ errmsg]
!! standard_name = ccpp_error_message
!! long_name = error message for error handling in CCPP
!! units = none
!! type = character
!! len = *
!! dimensions = ()
!! intent = out
!! [ ierr ]
!! standard_name = ccpp_error_flag
!! long_name = error flag for error handling in CCPP
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

# Python library imports
from __future__ import print_function
import re
# CCPP framework imports
from metavar import Var, VarDictionary
from parse_tools import ParseObject, ParseSource, register_fortran_ddt_name
from parse_tools import ParseInternalError, ParseSyntaxError, CCPPError
from parse_tools import MetadataSyntax, FortranMetadataSyntax
from parse_tools import FORTRAN_ID, FORTRAN_SCALAR_REF

########################################################################

########################################################################

class MetadataHeader(ParseSource):
    """Class to hold all information from a metadata header
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["!> \section arg_table_foobar", "!! [ im ]",           \
                       "!! standard_name = horizontal_loop_extent",           \
                       "!! long_name = horizontal loop extent, start at 1",   \
                       "!! units = index | type = integer",                   \
                       "!! dimensions = () |  intent = in"])).get_var(standard_name='horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: End of file parsing Metadata table, at foobar.txt:7
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["!> \section arg_table_foobar", "!! [ im ]",           \
                       "!! standard_name = horizontal_loop_extent",           \
                       "!! long_name = horizontal loop extent, start at 1",   \
                       "!! units = index | type = integer",                   \
                       "!! dimensions = () |  intent = in",                   \
                       "  subroutine foo()"])).get_var(standard_name='horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid Metadata table ending, '  subroutine foo()', at foobar.txt:7
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["!> \section arg_table_foobar", "!! [ im ]",           \
                       "!! standard_name = horizontal_loop_extent",           \
                       "!! long_name = horizontal loop extent, start at 1",   \
                       "!! units = index | type = integer",                   \
                       "!! dimensions = () |  intent = in",                   \
                       "!! "])).get_var(standard_name='horizontal_loop_extent') #doctest: +ELLIPSIS
    <metavar.Var object at 0x...>
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["!> \section arg_table_foobar", "!! [ im ]",           \
                       "!! standard_name = horizontal_loop_extent",           \
                       "!! long_name = horizontal loop extent, start at 1",   \
                       "!! units = index | type = integer",                   \
                       "!! dimensions = () |  intent = in",                   \
                       "!! "], line_start=0),                                 \
                       syntax=FortranMetadataSyntax).get_var(standard_name='horizontal_loop_extent').get_prop_value('local_name')
    'im'
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["!> \section arg_table_foobar", "!! [ im ]",           \
                       "!! standard_name = horizontal loop extent",           \
                       "!! long_name = horizontal loop extent, start at 1",   \
                       "!! units = index | type = integer",                   \
                       "!! dimensions = () |  intent = in",                   \
                       "!! "], line_start=0)).get_var(standard_name='horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid variable property value, 'horizontal loop extent', at foobar.txt:2
    >>> MetadataHeader._var_start.match('[ qval ]') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> MetadataHeader._var_start.match('[ qval(hi_mom) ]') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
"""

    _header_start = re.compile(r"\\section\s+arg_table_([A-Za-z][A-Za-z0-9_]*)")

    _var_start = re.compile(r"^\[\s*("+FORTRAN_ID+r"|"+FORTRAN_SCALAR_REF+r")\s*\]$")

    def __init__(self, parse_object, syntax=FortranMetadataSyntax, spec_name=None, logger=None):
        self._pobj = parse_object
        self._spec_name = spec_name
        if syntax is None:
            raise CCPPError('syntax may not be None')
        # End if
        self._syntax = syntax
        "Initialize from the <lines> of <filename> beginning at <first_line>"
        # Read the table preamble, assume the caller already figured out
        #  the first line of the header using the metadata_table_start method.
        curr_line, curr_line_num = self._pobj.curr_line()
        self._table_title = MetadataHeader.metadata_table_start(curr_line, context=self._pobj, syntax=self._syntax)
        if self._table_title is None:
            raise ParseSyntaxError("metadata header start",
                                   token=curr_line, context=self._pobj)
        # End if
        # Figure out the header type
        if self._spec_name is not None:
            if self._spec_name.lower() == self._table_title.lower():
                # This is a module or program data header
                self._header_type = 'MODULE'
            else:
                # This should be a derived data type
                self._header_type = 'DDT'
                register_fortran_ddt_name(self.title)
            # End if
        else:
            # This has to be a scheme name
            self._header_type = 'SCHEME'
        # End if
        # Tiem to initialize our ParseSource parent
        super(MetadataHeader, self).__init__(self._table_title,
                                             self._header_type, self._pobj)
        curr_line, curr_line_num = self._pobj.next_line()
        # Skip past any 'blank' lines
        blanks_found = False
        while self._syntax.blank_line(curr_line):
            blanks_found = True
            curr_line, curr_line_num = self._pobj.next_line()
        # End while
        # Read the variables
        valid_lines =  True
        self._variables = VarDictionary(self.title, logger=logger)
        self._var_intents = {'in' : list(), 'out' : list(), 'inout' : list()}
        while valid_lines:
            newvar, curr_line = self.parse_variable(curr_line, spec_name)
            valid_lines = newvar is not None
            if valid_lines:
                intent = newvar.get_prop_value('intent')
                if intent is not None:
                    self._var_intents[intent].append(newvar)
                # End if
                self._variables.add_variable(newvar)
            else:
                # We have hit the end of the table, check for blank line
                if curr_line is None:
                    raise ParseSyntaxError("End of file parsing Metadata table",
                                           context=self._pobj)
                elif not self._syntax.blank_line(curr_line):
                    # First check if we have a valid empty table
                    if (not blanks_found) or (len(self._variables) > 0):
                        raise ParseSyntaxError("Metadata table ending",
                                               curr_line, context=self._pobj)
                    # End if
                # End if
            # End if
        # End while

    def parse_variable(self, curr_line, spec_name):
        # The header line has the format [ <valid_fortran_symbol> ]
        # Parse header
        valid_line = curr_line is not None
        if valid_line:
            local_name = self.variable_start(curr_line) # caller handles exception
        else:
            local_name = None
        # End if
        if local_name is None:
            # This is not a valid variable line, punt (should be end of table)
            valid_line = False
        # End if
        # Parse lines until invalid line is found
        # NB: Header variables cannot have embedded blank lines
        if valid_line:
            var_props = {}
            var_props['local_name'] = local_name
        else:
            var_props = None
        # End if
        while valid_line:
            curr_line, curr_line_num = self._pobj.next_line()
            valid_line = ((curr_line is not None) and
                          (not self._syntax.blank_line(curr_line)) and
                          (self._syntax.strip(curr_line) is not None) and
                          (self.variable_start(curr_line) is None))
            # A valid line may have multiple properties (separated by '|')
            if valid_line:
                properties = self._syntax.strip(curr_line).split('|')
                for property in properties:
                    pitems = property.split('=', 1)
                    if len(pitems) < 2:
                        raise ParseSyntaxError("variable property syntax",
                                               token=property,
                                               context=self._pobj)
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
                                                   context=self._pobj)
                        # End if
                        if pval is None:
                            raise ParseSyntaxError("'{}' property value".format(pname),
                                                   token=pval_str,
                                                   context=self._pobj)
                        # End if
                    except ParseSyntaxError as p:
                        raise p
                    # If we get this far, we have a valid property.
                    var_props[pname] = pval
                # End for
            # End if
        # End while
        if var_props is None:
            return None, curr_line
        else:
            try:
                newvar = Var(var_props, source=self)
            except CCPPError as ve:
                raise ParseSyntaxError(ve, context=self._pobj)
            return newvar, curr_line
        # End if

    def variable_list(self):
        "Return an ordered list of the header's variables"
        return self._variables.variable_list()

    def get_var(self, standard_name=None, intent=None):
        if standard_name is not None:
            var = self._variables.find_variable(standard_name)
            return var
        elif intent is not None:
            if intent not in self._var_intents:
                raise ParseInternalError("Illegal intent type, '{}', in {}".format(intent, self._table_title), context=self._pobj)
            # End if
            return self._var_intents[intent]
        else:
            return None

    def prop_list(self, prop_name):
        "Return list of <prop_name> values for this scheme's arguments"
        return self._variables.prop_list(prop_name)

    def variable_start(self, line):
        """Return variable name if <line> is an interface metadata table header
        """
        sline = self._syntax.strip(line)
        if sline is None:
            match = None
        else:
            match = MetadataHeader._var_start.match(sline)
        # End if
        if match is not None:
            name = match.group(1)
            if not self._syntax.is_scalar_reference(name):
                raise ParseSyntaxError("local variable name",
                                       token=name, context=self._pobj)
            # End if
        else:
            name = None
        # End if
        return name

    @property
    def title(self):
        'Return the name of the metadata arg_table'
        return self._table_title

    @property
    def module(self):
        'Return the module name for this header (if it exists)'
        return self._spec_name

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
                    raise ParseSyntaxError("arg_table name",
                                           token=name, context=context)
                # End if
            else:
                name = None
            # End if
        else:
            name = None
        # End if
        return name

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
