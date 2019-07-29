 #!/usr/bin/env python
"""
Metadata headers are in config file format.
The argument tables for schemes and variable definitions should
have a special section followed by variable declaration sections.
The special section name is ccpp-arg-table. The entries in this section are:
name = <name> : the name of the file object which immediately follows the
                argument table. It is one of the following possibilities:
   - SubroutineName: the name of a subroutine (i.e., the name of
                     a scheme interface function such as SchemeName_run)
   - DerivedTypeName: a derived type name for a type which will be used
                      somewhere in the CCPP interface.
   - ModuleName: the name of the module whose module variables will be
                 used somewhere in the CCPP interface
type = <type> : The type of header, one of:
    - scheme: A CCPP subroutine
    - ddt: A header for a derived data type
    - module: A header on some module data

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

[ccpp-arg-table]
  name = <name>
  type = scheme
[ im ]
  standard_name = horizontal_loop_extent
  long_name = horizontal loop extent, start at 1
  units = index
  type = integer
  dimensions = ()
  intent = in
[ ix ]
  standard_name = horizontal_loop_dimension
  long_name = horizontal dimension
  units = index | type = integer | dimensions = ()
  intent = in
  ...
[ errmsg]
  standard_name = ccpp_error_message
  long_name = error message for error handling in CCPP
  units = none
  type = character
  len = *
  dimensions = ()
  intent = out
[ ierr ]
  standard_name = ccpp_error_flag
  long_name = error flag for error handling in CCPP
  type = integer
  dimensions = ()
  intent=out

Notes on the input format:
- SubroutineName must match the name of the subroutine that the argument
  table describes
- DerivedTypeName must match the name of the derived type that the argument
  table describes
- ModuleName must match the name of the module whose variables the argument
  table describes
- for variable type definitions and module variables, the intent and
  optional columns are not functional and should be omitted
- each argument table (and its subroutine) must accept the following two arguments for error handling (the local name can vary):
[ errmsg ]
  standard_name = ccpp_error_message
  long_name = Error message for error handling in CCPP
  units = 1
  dimensions = ()
  type = character
  kind = len=512
  intent = out
  optional = F
[ errflg ]
  standard_name = ccpp_error_flag
  long_name = Error flag for error handling in CCPP
  units = flag
  dimensions = ()
  type = integer
  intent = out
  optional = F
"""

# Python library imports
from __future__ import print_function
import re
# CCPP framework imports
from metavar     import Var, VarDictionary
from parse_tools import ParseObject, ParseSource, register_fortran_ddt_name
from parse_tools import ParseInternalError, ParseSyntaxError, CCPPError
from parse_tools import LITERAL, FORTRAN_ID, FORTRAN_SCALAR_REF
from parse_tools import check_fortran_ref

########################################################################

########################################################################

class MetadataHeader(ParseSource):
    """Class to hold all information from a metadata header
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["name = foobar", "type = scheme", "module = foo",      \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in"])) #doctest: +ELLIPSIS
    <__main__.MetadataHeader foo / foobar at 0x...>
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["name = foobar", "type = scheme", "module = foobar",   \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in"])).get_var(standard_name='horizontal_loop_extent') #doctest: +ELLIPSIS
    <metavar.Var horizontal_loop_extent: im at 0x...>
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["name = foobar", "module = foo",                       \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       "  subroutine foo()"])).get_var(standard_name='horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Missing metadata header type, at foobar.txt:7
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["name = foobar", "type = scheme", "module=foobar",     \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).get_var(standard_name='horizontal_loop_extent').get_prop_value('local_name')
    'im'
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["name = foobar", "type = scheme"                       \
                       "[ im ]", "standard_name = horizontal loop extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).get_var(standard_name='horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid variable property value, 'horizontal loop extent', at foobar.txt:2
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["[ccpp-arg-table]", "name = foobar", "type = scheme"   \
                       "[ im ]", "standard_name = horizontal loop extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).get_var(standard_name='horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid property syntax, '[ccpp-arg-table]', at foobar.txt:1
    >>> MetadataHeader(ParseObject("foobar.txt",                              \
                      ["name = foobar", "module = foo"                        \
                       "[ im ]", "standard_name = horizontal loop extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).get_var(standard_name='horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid metadata header start, no table type, at foobar.txt:2
    >>> MetadataHeader.__var_start__.match('[ qval ]') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> MetadataHeader.__var_start__.match('[ qval(hi_mom) ]') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
"""

    __header_start__ = re.compile(r"(?i)\s*\[\s*ccpp-arg-table\s*\]")

    __var_start__ = re.compile(r"^\[\s*("+FORTRAN_ID+r"|"+LITERAL+r"|"+FORTRAN_SCALAR_REF+r")\s*\]$")

    __blank_line__ = re.compile(r"\s*[#;]")

    def __init__(self, parse_object=None,
                 title=None, type_in=None, module=None, var_dict=None,
                 logger=None):
        self._pobj = parse_object
        """If <parse_object> is not None, initialize from the current file and
        location in <parse_object>.
        If <parse_object> is None, initialize from <title>, <type>, <module>,
        and <var_dict>. Note that if <parse_object> is not None, <title>,
        <type>, <module>, and <var_dict> are ignored.
        """
        if parse_object is None:
            if title is None:
                raise ParseInternalError('MetadataHeader requires a title')
            else:
                self._table_title = title
            # End if
            if type_in is None:
                raise ParseInternalError('MetadataHeader requires a header type')
            else:
                self._header_type = type
            # End if
            if module is None:
                raise ParseInternalError('MetadataHeader requires a module name')
            else:
                self._module_name = module
            # End if
            #  Initialize our ParseSource parent
            super(MetadataHeader, self).__init__(self.title,
                                                 self.header_type, self._pobj)
            self._variables = VarDictionary(self.title, logger=logger)
            for var in var_dict.variable_list(): # Let this crash if no dict
                self._variables.add_variable(var)
            # End for
        else:
            self.__init_from_file__(parse_object, logger)
        # End if
        # Categorize the variables
        self._var_intents = {'in' : list(), 'out' : list(), 'inout' : list()}
        for var in self.variable_list():
            intent = var.get_prop_value('intent')
            if intent is not None:
                self._var_intents[intent].append(var)
            # End if
        # End for

    def __init_from_file__(self, parse_object, logger):
        # Read the table preamble, assume the caller already figured out
        #  the first line of the header using the table_start method.
        curr_line, curr_line_num = self._pobj.next_line()
        self._table_title = None
        self._header_type = None
        self._module_name = None
        while (curr_line is not None) and (not self.variable_start(curr_line)) and (not MetadataHeader.table_start(curr_line)):
            for property in self.parse_config_line(curr_line):
                # Manually parse name, type, and module properties
                key = property[0].strip().lower()
                value = property[1].strip()
                if key == 'name':
                    self._table_title = value
                elif key == 'type':
                    if value not in ['module', 'scheme', 'ddt']:
                        raise ParseSyntaxError("metadata table type",
                                               token=value,
                                               context=self._pobj)
                    # End if
                    self._header_type = value
                elif key == 'module':
                    if value == "None":
                        raise ParseSyntaxError("metadata table, no module",
                                               context=self._pobj)
                    else:
                        self._module_name = value
                    # End if
                else:
                    raise ParseSyntaxError("metadata table start property",
                                           token=value, context=self._pobj)
                # End if
            # End for
            curr_line, curr_line_num = self._pobj.next_line()
        # End while
        if self.title is None:
            raise ParseSyntaxError("metadata header start, no table name",
                                   token=curr_line, context=self._pobj)
        elif self._header_type is None:
            raise ParseSyntaxError("metadata header start, no table type",
                                   token=curr_line, context=self._pobj)
        elif self.header_type == "ddt":
            register_fortran_ddt_name(self.title)
        # End if
        #  Initialize our ParseSource parent
        super(MetadataHeader, self).__init__(self.title,
                                             self.header_type, self._pobj)
        # Read the variables
        valid_lines =  True
        self._variables = VarDictionary(self.title, logger=logger)
        while valid_lines:
            newvar, curr_line = self.parse_variable(curr_line)
            valid_lines = newvar is not None
            if valid_lines:
                self._variables.add_variable(newvar)
                # Check to see if we hit the end of the table
                valid_lines = not MetadataHeader.table_start(curr_line)
            # No else, we just run off the end of the table
            # End if
        # End while

    def parse_config_line(self, line):
        "Parse a config line and return a list of keyword value pairs."
        parse_items = list()
        if line is None:
            pass # No properties on this line
        elif MetadataHeader.is_blank(line):
            pass # No properties on this line
        else:
            properties = line.strip().split('|')
            for property in properties:
                pitems = property.split('=', 1)
                if len(pitems) < 2:
                    raise ParseSyntaxError("variable property syntax",
                                           token=property,
                                           context=self._pobj)
                else:
                    parse_items.append(pitems)
                # End if
            # End for
        # End if
        return parse_items

    def parse_variable(self, curr_line):
        # The header line has the format [ <valid_fortran_symbol> ]
        # Parse header
        valid_line = (curr_line is not None) and (not MetadataHeader.table_start(curr_line))
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
                          (not MetadataHeader.is_blank(curr_line)) and
                          (not MetadataHeader.table_start(curr_line)) and
                          (self.variable_start(curr_line) is None))
            # A valid line may have multiple properties (separated by '|')
            if valid_line:
                properties = self.parse_config_line(curr_line)
                for property in properties:
                    try:
                        pname = property[0].strip()
                        pval_str = property[1].strip()
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
                raise ParseInternalError("Illegal intent type, '{}', in {}".format(intent, self.title), context=self._pobj)
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
        if line is None:
            match = None
        else:
            match = MetadataHeader.__var_start__.match(line)
        # End if
        if match is not None:
            name = match.group(1)
            if not MetadataHeader.is_scalar_reference(name):
                raise ParseSyntaxError("local variable name",
                                       token=name, context=self._pobj)
            # End if
        else:
            name = None
        # End if
        return name

    def __repr__(self):
        base = super(MetadataHeader, self).__repr__()
        pind = base.find(' object ')
        if pind >= 0:
            pre = base[0:pind]
        else:
            pre = '<MetadataHeader'
        # End if
        bind = base.find('at 0x')
        if bind >= 0:
            post = base[bind:]
        else:
            post = '>'
        # End if
        return '{} {} / {} {}'.format(pre, self.module, self.title, post)

    def __del__(self):
        try:
            del self._variables
            super(MetadataHeader, self).__del__()
        except Exception as e:
            pass # Python does not guarantee much about __del__ conditions
        # End try

    @property
    def title(self):
        'Return the name of the metadata arg_table'
        return self._table_title

    @property
    def module(self):
        'Return the module name for this header (if it exists)'
        return self._module_name

    @property
    def header_type(self):
        'Return the type of structure this header documents'
        return self._header_type

    @classmethod
    def is_blank(cls, line):
        "Return True iff <line> is a valid config format blank or comment line"
        return (len(line) == 0) or (cls.__blank_line__.match(line) is not None)

    @classmethod
    def table_start(cls, line):
        """Return variable name if <line> is an interface metadata table header
        """
        if (line is None) or cls.is_blank(line):
            match = None
        else:
            match = MetadataHeader.__header_start__.match(line)
        # End if
        return match is not None

    @classmethod
    def is_scalar_reference(cls, test_val):
        return check_fortran_ref(test_val) is not None

    @classmethod
    def parse_metadata_file(cls, filename):
        "Parse <filename> and return list of parsed metadata headers"
        # Read all lines of the file at once
        mheaders = list()
        with open(filename, 'r') as file:
            fin_lines = file.readlines()
            for index in xrange(len(fin_lines)):
                fin_lines[index] = fin_lines[index].rstrip('\n')
            # End for
        # End with
        # Look for a header start
        parse_obj = ParseObject(filename, fin_lines)
        curr_line, curr_line_num = parse_obj.curr_line()
        while curr_line is not None:
            if MetadataHeader.table_start(curr_line):
                mheaders.append(MetadataHeader(parse_obj))
                curr_line, curr_line_num = parse_obj.curr_line()
            else:
                curr_line, curr_line_num = parse_obj.next_line()
            # End if
        # End while
        return mheaders

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
