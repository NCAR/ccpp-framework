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
import os.path
import re
# CCPP framework imports
from metavar     import Var, VarDictionary, CCPP_CONSTANT_VARS
from parse_tools import ParseObject, ParseSource, ParseContext, context_string
from parse_tools import ParseInternalError, ParseSyntaxError, CCPPError
from parse_tools import FORTRAN_ID, FORTRAN_SCALAR_REF, FORTRAN_SCALAR_REF_RE
from parse_tools import check_fortran_ref, check_fortran_id
from parse_tools import register_fortran_ddt_name, unique_standard_name

########################################################################

########################################################################

class MetadataTable(ParseSource):
    """Class to hold all information from a metadata header
    >>> MetadataTable(ParseObject("foobar.txt",                               \
                      ["name = foobar", "type = scheme", "module = foo",      \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in"])) #doctest: +ELLIPSIS
    <__main__.MetadataTable foo / foobar at 0x...>
    >>> MetadataTable(ParseObject("foobar.txt",                               \
                      ["name = foobar", "type = scheme", "module = foobar",   \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in"])).find_variable('horizontal_loop_extent') #doctest: +ELLIPSIS
    <metavar.Var horizontal_loop_extent: im at 0x...>
    >>> MetadataTable(ParseObject("foobar.txt",                               \
                      ["name = foobar", "module = foo",                       \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       "  subroutine foo()"])).find_variable('horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Missing metadata header type, at foobar.txt:7
    >>> MetadataTable(ParseObject("foobar.txt",                               \
                      ["name = foobar", "type = scheme", "module=foobar",     \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).find_variable('horizontal_loop_extent').get_prop_value('local_name')
    'im'
    >>> MetadataTable(ParseObject("foobar.txt",                               \
                      ["name = foobar", "type = scheme"                       \
                       "[ im ]", "standard_name = horizontal loop extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).find_variable('horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid variable property value, 'horizontal loop extent', at foobar.txt:2
    >>> MetadataTable(ParseObject("foobar.txt",                               \
                      ["[ccpp-arg-table]", "name = foobar", "type = scheme"   \
                       "[ im ]", "standard_name = horizontal loop extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).find_variable('horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid property syntax, '[ccpp-arg-table]', at foobar.txt:1
    >>> MetadataTable(ParseObject("foobar.txt",                               \
                      ["name = foobar", "module = foo"                        \
                       "[ im ]", "standard_name = horizontal loop extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).find_variable('horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid metadata header start, no table type, at foobar.txt:2
    >>> MetadataTable.__var_start__.match('[ qval ]') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> MetadataTable.__var_start__.match('[ qval(hi_mom) ]') #doctest: +ELLIPSIS

    >>> MetadataTable.__vref_start__.match('[ qval(hi_mom) ]') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> MetadataTable.__var_start__.match('[ qval ]').group(1)
    'qval'
    >>> MetadataTable.__vref_start__.match('[ qval(hi_mom) ]').group(1)
    'qval'
    >>> MetadataTable.__vref_start__.match('[ qval(hi_mom) ]').group(2)
    'hi_mom'
"""

    __header_start__ = re.compile(r"(?i)\s*\[\s*ccpp-arg-table\s*\]")

    __var_start__ = re.compile(r"^\[\s*"+FORTRAN_ID+r"\s*\]$")

    __vref_start__ = re.compile(r"^\[\s*"+FORTRAN_SCALAR_REF+r"\s*\]$")

    __blank_line__ = re.compile(r"\s*[#;]")

    __header_types__ = ['ddt', 'host', 'module', 'scheme', 'local']

    def __init__(self, parse_object=None,
                 title=None, type_in=None, module=None, var_dict=None,
                 known_ddts=None, logger=None):
        self._pobj = parse_object
        """If <parse_object> is not None, initialize from the current file and
        location in <parse_object>.
        If <parse_object> is None, initialize from <title>, <type>, <module>,
        and <var_dict>. Note that if <parse_object> is not None, <title>,
        <type>, <module>, and <var_dict> are ignored.
        """
        if parse_object is None:
            if title is None:
                raise ParseInternalError('MetadataTable requires a title')
            else:
                self._table_title = title
            # End if
            if type_in is None:
                raise ParseInternalError('MetadataTable requires a header type')
            elif type_in not in MetadataTable.__header_types__:
                raise ParseSyntaxError("metadata table type",
                                       token=type_in,
                                       context=self._pobj)
            else:
                self._header_type = type_in
            # End if
            if module is None:
                raise ParseInternalError('MetadataTable requires a module name')
            else:
                self._module_name = module
            # End if
            #  Initialize our ParseSource parent
            super(MetadataTable, self).__init__(self.title,
                                                self.header_type, self._pobj)
            self._variables = VarDictionary(self.title, logger=logger)
            for var in var_dict.variable_list(): # Let this crash if no dict
                self._variables.add_variable(var)
            # End for
            self._start_context = None
        else:
            if known_ddts is None:
                known_ddts = list()
            # End if
            self.__init_from_file__(known_ddts, logger)
            self._start_context = ParseContext(context=self._pobj)
        # End if
        # Register this header if it is a DDT
        if self.header_type == 'ddt':
            register_fortran_ddt_name(self.title)
        # End if
        # Categorize the variables
        self._var_intents = {'in' : list(), 'out' : list(), 'inout' : list()}
        for var in self.variable_list():
            intent = var.get_prop_value('intent')
            if intent is not None:
                self._var_intents[intent].append(var)
            # End if
        # End for

    def __init_from_file__(self, known_ddts, logger):
        # Read the table preamble, assume the caller already figured out
        #  the first line of the header using the table_start method.
        curr_line, curr_line_num = self._pobj.next_line()
        self._table_title = None
        self._header_type = None
        self._module_name = None
        while (curr_line is not None) and (not self.variable_start(curr_line)) and (not MetadataTable.table_start(curr_line)):
            for property in self.parse_config_line(curr_line):
                # Manually parse name, type, and module properties
                key = property[0].strip().lower()
                value = property[1].strip()
                if key == 'name':
                    self._table_title = value
                elif key == 'type':
                    if value not in MetadataTable.__header_types__:
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
            known_ddts.append(self.title)
        # End if
        # We need a default module if none was listed
        if self._module_name is None:
            mfile = self._pobj.file_name
            if mfile[-5:] == '.meta':
                # Default value is a Fortran module that matches the filename
                self._module_name = os.path.basename(mfile)[:-5]
            # End if
        # End if
        #  Initialize our ParseSource parent
        super(MetadataTable, self).__init__(self.title,
                                             self.header_type, self._pobj)
        # Read the variables
        valid_lines =  True
        self._variables = VarDictionary(self.title, logger=logger)
        while valid_lines:
            newvar, curr_line = self.parse_variable(curr_line, known_ddts)
            valid_lines = newvar is not None
            if valid_lines:
                self._variables.add_variable(newvar)
                # Check to see if we hit the end of the table
                valid_lines = not MetadataTable.table_start(curr_line)
            # No else, we just run off the end of the table
            # End if
        # End while

    def parse_config_line(self, line):
        "Parse a config line and return a list of keyword value pairs."
        parse_items = list()
        if line is None:
            pass # No properties on this line
        elif MetadataTable.is_blank(line):
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

    def parse_variable(self, curr_line, known_ddts):
        # The header line has the format [ <valid_fortran_symbol> ]
        # Parse header
        valid_line = (curr_line is not None) and (not MetadataTable.table_start(curr_line))
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
            # Grab context that points at beginning of definition
            context = ParseContext(context=self.context)
        else:
            var_props = None
        # End if
        while valid_line:
            curr_line, curr_line_num = self._pobj.next_line()
            valid_line = ((curr_line is not None) and
                          (not MetadataTable.is_blank(curr_line)) and
                          (not MetadataTable.table_start(curr_line)) and
                          (self.variable_start(curr_line) is None))
            # A valid line may have multiple properties (separated by '|')
            if valid_line:
                properties = self.parse_config_line(curr_line)
                for property in properties:
                    try:
                        pname = property[0].strip().lower()
                        pval_str = property[1].strip()
                        if pname == 'ddt_type':
                            if pval_str in known_ddts:
                                pval = pval_str
                            else:
                                errmsg = "Unknown DDT type, {}"
                                raise ParseSyntaxError(errmsg.format(pval_str),
                                                       context=self._pobj)
                            # End if
                        else:
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
                                errmsg = "'{}' property value"
                                raise ParseSyntaxError(errmsg.format(pname),
                                                       token=pval_str,
                                                       context=self._pobj)
                            # End if
                        # End if
                    except ParseSyntaxError as p:
                        raise p
                    # If we get this far, we have a valid property.
                    # Special case for dimensions, turn them into ranges
                    if pname == 'dimensions':
                        porig = pval
                        pval = list()
                        for dim in porig:
                            if ':' in dim:
                                pval.append(dim)
                            else:
                                pval.append('ccpp_constant_one:{}'.format(dim))
                            # End if
                        # End for
                    # End if
                    # Add the property to our Var dictionary
                    var_props[pname] = pval
                # End for
            # End if
        # End while
        if var_props is None:
            return None, curr_line
        else:
            # Check for array reference
            sub_name = MetadataTable.check_array_reference(local_name,
                                                           var_props, context)
            if sub_name:
                var_props['local_name'] = sub_name
            # End if (else just leave the local name alone)
            try:
                newvar = Var(var_props, source=self, context=context)
            except CCPPError as ve:
                raise ParseSyntaxError(ve, context=self._pobj)
            return newvar, curr_line
        # End if

    @classmethod
    def check_array_reference(cls, local_name, var_dict, context):
        """If <local_name> is an array reference, check it against
        the 'dimensions' property in <var_dict>. If <local_name> is an
        array reference, return it with the colons filled in with the
        dictionary dimensions, otherwise, return None.
        >>> MetadataTable.check_array_reference('foo', {'dimensions':['ccpp_constant_one:bar', 'ccpp_constant_one:baz']}, ParseContext(filename='foo.meta'))

        >>> MetadataTable.check_array_reference('foo', {}, ParseContext(filename='foo.meta'))

        >>> MetadataTable.check_array_reference('foo(qux', {'dimensions':['ccpp_constant_one:bar', 'ccpp_constant_one:baz']}, ParseContext(filename='foo.meta')) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ParseInternalError: Invalid scalar reference, foo(qux, in foo.meta
        >>> MetadataTable.check_array_reference('foo(qux)', {'dimensions':['ccpp_constant_one:bar', 'ccpp_constant_one:baz']}, ParseContext(filename='foo.meta')) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ParseInternalError: foo has rank 2 but foo(qux) has 0, in foo.meta
        >>> MetadataTable.check_array_reference('foo(:,qux)', {'dimensions':['ccpp_constant_one:bar', 'ccpp_constant_one:baz']}, ParseContext(filename='foo.meta')) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ParseInternalError: foo has rank 2 but foo(:,qux) has 1, in foo.meta
        >>> MetadataTable.check_array_reference('foo(:,qux)', {'foo':['ccpp_constant_one:bar', 'ccpp_constant_one:baz']}, ParseContext(filename='foo.meta')) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ParseInternalError: Missing variable dimensions, foo(:,qux), in foo.meta
        >>> MetadataTable.check_array_reference('foo(:,:,qux)', {'dimensions':['ccpp_constant_one:bar']}, ParseContext(filename='foo.meta')) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ParseInternalError: foo has rank 1 but foo(:,:,qux) has 2, in foo.meta
        >>> MetadataTable.check_array_reference('foo(:,:,qux)', {'dimensions':['ccpp_constant_one:bar','ccpp_constant_one:baz']}, ParseContext(filename='foo.meta'))
        'foo(:, :, qux)'
        """
        if check_fortran_id(local_name, var_dict, False) is None:
            rmatch = FORTRAN_SCALAR_REF_RE.match(local_name)
            if rmatch is None:
                errmsg = 'Invalid scalar reference, {}{}'
                ctx = context_string(context)
                raise ParseInternalError(errmsg.format(local_name, ctx))
            # End if
            rname = rmatch.group(1)
            rdims = [x.strip() for x in rmatch.group(2).split(',')]
            if 'dimensions' in var_dict:
                vdims = [x.strip() for x in var_dict['dimensions']]
            else:
                errmsg = 'Missing variable dimensions, {}{}'
                ctx = context_string(context)
                raise ParseInternalError(errmsg.format(local_name, ctx))
            # End if
            colon_rank = len([x for x in rdims if x == ':'])
            if colon_rank != len(vdims):
                errmsg = '{} has rank {} but {} has {}{}'
                ctx = context_string(context)
                raise ParseInternalError(errmsg.format(rname, len(vdims),
                                                       local_name, colon_rank,
                                                       ctx))
            # End if
            sub_dims = list()
            sindex = 0
            for rind in rdims:
                if rind == ':':
                    sub_dims.append(':')
                    sindex += 1
                else:
                    sub_dims.append(rind)
                # End if
            # End for
            return '{}({})'.format(rname, ', '.join(sub_dims))
        else:
            return None
        # End if

    def variable_list(self, std_vars=True, loop_vars=True, consts=True):
        "Return an ordered list of the header's variables"
        return self._variables.variable_list(recursive=False,
                                             std_vars=std_vars,
                                             loop_vars=loop_vars, consts=consts)

    def find_variable(self, std_name, use_local_name=False):
        "Find a variable in this header's dictionary"
        var = None
        if use_local_name:
            var = self._variables.find_local_name(std_name)
        else:
            var = self._variables.find_variable(std_name, any_scope=False)
        # End if
        return var

    def convert_dims_to_standard_names(self, var, logger=None, context=None):
        """Convert the dimension elements in <var> to standard names by
        by using other variables in this header.
        """
        std_dims = list()
        for dim in var.get_dimensions():
            std_dim = list()
            if ':' not in dim:
                # Metadata dimensions always have an explicit start
                var_one = CCPP_CONSTANT_VARS.find_local_name('1')
                if var_one is not None:
                    std = var_one.get_prop_value('standard_name')
                    std_dim.append(std)
                # End if
            # End if
            for item in dim.split(':'):
                try:
                    int_item = int(item)
                    dvar = CCPP_CONSTANT_VARS.find_local_name(item)
                    if dvar is not None:
                        # If this integer value is a CCPP standard int, use that
                        dname = dvar.get_prop_value('standard_name')
                    else:
                        # Some non-standard integer value
                        dname = item
                    # End if
                except ValueError as ve:
                    # Not an integer, try to find the standard_name
                    if len(item) == 0:
                        # Naked colons are okay
                        dname = ''
                    else:
                        dvar = self.find_variable(item, use_local_name=True)
                        if dvar is not None:
                            dname = dvar.get_prop_value('standard_name')
                        else:
                            dname = None
                        # End if
                    # End if
                    if dname is None:
                        errmsg = "Unknown dimension element, {}, in {}{}"
                        std = var.get_prop_value('local_name')
                        ctx = context_string(context)
                        if logger is not None:
                            errmsg = "WARNING: " + errmsg
                            logger.error(errmsg.format(item, std, ctx))
                            dname = unique_standard_name()
                        else:
                            raise CCPPError(errmsg.format(item, std, ctx))
                        # End if
                    # End if
                # End try
                if dname is None:
                    std_dim = None
                    break
                else: # Should be okay because if not, we broke out of loop
                    std_dim.append(dname)
                # End if
            # End for
            if std_dim is None:
                break
            else: # Should be okay because if not, we broke out of loop
                std_dims.append(':'.join(std_dim))
            # End if
        # End for

        return std_dims

    def prop_list(self, prop_name):
        "Return list of <prop_name> values for this scheme's arguments"
        return self._variables.prop_list(prop_name)

    def variable_start(self, line):
        """Return variable name if <line> is an interface metadata table header
        """
        if line is None:
            match = None
        else:
            match = MetadataTable.__var_start__.match(line)
            if match is None:
                match = MetadataTable.__vref_start__.match(line)
                if match is not None:
                    name = match.group(1)+'('+match.group(2)+')'
                # End if
            else:
                name = match.group(1)
            # End if
        # End if
        if match is not None:
            if not MetadataTable.is_scalar_reference(name):
                raise ParseSyntaxError("local variable name",
                                       token=name, context=self._pobj)
            # End if
        else:
            name = None
        # End if
        return name

    def write_to_file(self, filename, append=False):
        """Write this metadata table to <filename>. If <append> is True,
        append this table to the end of <filename>, otherwise, create
        or truncate the file."""
        if append:
            oflag = 'a'
        else:
            oflag = 'w'
        # end if
        with open(filename, oflag) as mfile:
            mfile.write("[ccpp-arg-table]")
            mfile.write("  name = {}".format(self.title))
            mfile.write("  type = {}".format(self.header_type))
            for var in self.variable_list():
                var.write_metadata(mfile)
            # End for
        # End with

    def __repr__(self):
        base = super(MetadataTable, self).__repr__()
        pind = base.find(' object ')
        if pind >= 0:
            pre = base[0:pind]
        else:
            pre = '<MetadataTable'
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
            super(MetadataTable, self).__del__()
        except Exception as e:
            pass # Python does not guarantee much about __del__ conditions
        # End try

    def start_context(self, with_comma=True, nodir=True):
        'Return a context string for the beginning of the table'
        return context_string(self._start_context,
                              with_comma=with_comma, nodir=nodir)

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

    @property
    def has_variables(self):
        "Convenience function for finding empty headers"
        return self._variables

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
            match = MetadataTable.__header_start__.match(line)
        # End if
        return match is not None

    @classmethod
    def is_scalar_reference(cls, test_val):
        return check_fortran_ref(test_val, None, False) is not None

    @classmethod
    def parse_metadata_file(cls, filename, known_ddts, logger):
        "Parse <filename> and return list of parsed metadata headers"
        # Read all lines of the file at once
        meta_headers = list()
        header_titles = list() # Keep track of names in file
        with open(filename, 'r') as file:
            fin_lines = file.readlines()
            for index in range(len(fin_lines)):
                fin_lines[index] = fin_lines[index].rstrip('\n')
            # End for
        # End with
        # Look for a header start
        parse_obj = ParseObject(filename, fin_lines)
        curr_line, curr_line_num = parse_obj.curr_line()
        while curr_line is not None:
            if MetadataTable.table_start(curr_line):
                new_header = MetadataTable(parse_object=parse_obj,
                                           known_ddts=known_ddts, logger=logger)
                ntitle = new_header.title
                if ntitle in header_titles:
                    errmsg = 'Duplicate metadata header, {}, at {}:{}'
                    ctx = curr_line_num + 1
                    raise CCPPError(errmsg.format(ntitle, filename, ctx))
                else:
                    meta_headers.append(new_header)
                    header_titles.append(ntitle)
                    if new_header.header_type == 'ddt':
                        known_ddts.append(ntitle)
                    # End if
                # End if
                curr_line, curr_line_num = parse_obj.curr_line()
            else:
                curr_line, curr_line_num = parse_obj.next_line()
            # End if
        # End while
        return meta_headers

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
