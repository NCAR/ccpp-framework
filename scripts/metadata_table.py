#!/usr/bin/env python3
"""
There are four types of CCPP metadata tables, scheme, module, ddt, and host.
A metadata file contains one or more metadata tables.
A metadata file SHOULD NOT mix metadata table types. The exception is a
  metadata file which contains one or more ddt tables followed by a module
  or host table.

Each metadata table begins with a 'ccpp-table-properties' section followed by
  one or more 'ccpp-arg-table' sections. These sections are described below.
A 'ccpp-arg-table' section is followed by one or more variable declaration
  sections, also described below.

Metadata headers are in config file format.

A 'ccpp-table-properties' section entries are:
name = <name> : the name of the following ccpp-arg-table entries (required).
                It is one of the following possibilities:
   - SchemeName: the name of a scheme (i.e., the name of
                  a scheme interface (related to SubroutineName below).
   - DerivedTypeName: a derived type name for a type which will be used
                      somewhere in the CCPP interface.
   - ModuleName: the name of the module whose module variables will be
                 used somewhere in the CCPP interface
   - HostName: the name of the host model. Variables in this section become
                 part of the CCPP UI, the CCPP routines called by the
                 host model (e.g., <HostName>_ccpp_physics_run).
type = <type> : The type of header (required), one of:
    - scheme: A CCPP subroutine
    - ddt: A header for a derived data type
    - module: A header on some module data
    - host: A header on data which will be part of the CCPP UI

The ccpp-arg-table section entries in this section are:
name = <name> : the name of the file object which immediately follows the
                argument table (required).
                It is one of the following possibilities:
   - SubroutineName: the name of a subroutine (i.e., the name of
                     a scheme interface function such as SchemeName_run)
   - DerivedTypeName: a derived type name for a type which will be used
                      somewhere in the CCPP interface.
   - ModuleName: the name of the module whose module variables will be
                 used somewhere in the CCPP interface
   - HostName: the name of the host model. Variables in this section become
                 part of the CCPP UI, the CCPP routines called by the
                 host model (e.g., <HostName>_ccpp_physics_run).
type = <type> : The type of header (required). It must match the type of the
                associated ccpp-table-properties section (see above).

A variable declaration section begins with a variable name line (a local
variable name enclosed in square brackets) followed by one or more
variable attribute statements.
A variable attribute statement is an attribute name and the value for
that attribute separated by an equal sign. Whitespace is not
significant except inside of strings.
Variable attribute statements may be combined on a line if separated by
a vertical bar.

An example argument table is shown below.

[ccpp-table-properties]
  name = <name>
  type = scheme
  relative_path = <relative path>
  dependencies = <dependencies>

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
  standard_name = ccpp_error_code
  long_name = error flag for error handling in CCPP
  type = integer
  units = 1
  dimensions = ()
  intent=out

Notes on the input format:
- SubroutineName must match the name of the subroutine that the argument
  table describes
- DerivedTypeName must match the name of the derived type that the argument
  table describes
- ModuleName must match the name of the module whose variables the argument
  table describes
- for variable type definitions and module variables, the intent keyword
  is not functional and should be omitted
- each argument table (and its subroutine) must accept the following two arguments for error handling (the local name can vary):
[ errmsg ]
  standard_name = ccpp_error_message
  long_name = Error message for error handling in CCPP
  units = 1
  dimensions = ()
  type = character
  kind = len=512
  intent = out
[ errflg ]
  standard_name = ccpp_error_code
  long_name = Error flag for error handling in CCPP
  units = 1
  dimensions = ()
  type = integer
  intent = out
"""

# Python library imports
import difflib
import logging
import os.path
import re
# CCPP framework imports
from ccpp_state_machine  import CCPP_STATE_MACH
from metavar     import Var, VarDictionary, CCPP_CONSTANT_VARS
from parse_tools import ParseObject, ParseSource, ParseContext, context_string
from parse_tools import ParseInternalError, ParseSyntaxError, CCPPError
from parse_tools import FORTRAN_ID, FORTRAN_SCALAR_REF, FORTRAN_SCALAR_REF_RE
from parse_tools import check_fortran_ref, check_fortran_id
from parse_tools import check_fortran_intrinsic
from parse_tools import register_fortran_ddt_name, unique_standard_name

########################################################################

SCHEME_HEADER_TYPE = 'scheme'
_SINGLETON_TABLE_TYPES = ['ddt', 'host', 'module'] # Only one section per table
TABLE_TYPES = _SINGLETON_TABLE_TYPES + [SCHEME_HEADER_TYPE]
HEADER_TYPES = TABLE_TYPES + ['local']
UNKNOWN_PROCESS_TYPE = 'UNKNOWN'

_BLANK_LINE = re.compile(r"\s*[#;]")

def blank_metadata_line(line):
    """Return True if <line> is a valid config format blank or comment
    line. Also return True if we have reached the end of the file
    (no line)"""
    return (not line) or (_BLANK_LINE.match(line) is not None)

########################################################################

def _parse_config_line(line, context):
    """Parse a config line and return a list of keyword value pairs."""
    parse_items = list()
    if line is None:
        pass # No properties on this line
    elif blank_metadata_line(line):
        pass # No properties on this line
    else:
        properties = line.strip().split('|')
        for prop in properties:
            pitems = prop.split('=', 1)
            if len(pitems) >= 2:
                parse_items.append(pitems)
            else:
                raise ParseSyntaxError("variable property syntax",
                                       token=prop, context=context)
            # end if
        # end for
    # end if
    return parse_items

########################################################################

def parse_metadata_file(filename, known_ddts, run_env):
    """Parse <filename> and return list of parsed metadata tables"""
    # Read all lines of the file at once
    meta_tables = list()
    table_titles = list() # Keep track of names in file
    with open(filename, 'r') as infile:
        fin_lines = infile.readlines()
        for index, fin_line in enumerate(fin_lines):
            fin_lines[index] = fin_line.rstrip('\n')
        # end for
    # end with
    # Look for a header start
    parse_obj = ParseObject(filename, fin_lines)
    curr_line, curr_line_num = parse_obj.curr_line()
    while curr_line is not None:
        if MetadataTable.table_start(curr_line):
            new_table = MetadataTable(run_env, parse_object=parse_obj,
                                      known_ddts=known_ddts)
            ntitle = new_table.table_name
            if ntitle not in table_titles:
                meta_tables.append(new_table)
                table_titles.append(ntitle)
                if new_table.table_type == 'ddt':
                    known_ddts.append(ntitle)
                # end if
            else:
                errmsg = 'Duplicate metadata table, {}, at {}:{}'
                ctx = curr_line_num + 1
                raise CCPPError(errmsg.format(ntitle, filename, ctx))
            # end if
            curr_line, curr_line_num = parse_obj.curr_line()
        elif blank_metadata_line(curr_line):
            curr_line, curr_line_num = parse_obj.next_line()
        else:
            raise ParseSyntaxError('CCPP metadata line', token=curr_line,
                                   context=parse_obj)
        # end if
    # end while
    return meta_tables

########################################################################

def find_scheme_names(filename):
    """Find and return a list of all the physics scheme names in
    <filename>. A scheme is identified by its ccpp-table-properties name.
    """
    scheme_names = list()
    with open(filename, 'r') as infile:
        fin_lines = infile.readlines()
    # end with
    num_lines = len(fin_lines)
    context = ParseContext(linenum=1, filename=filename)
    while context.line_num <= num_lines:
        if MetadataTable.table_start(fin_lines[context.line_num - 1]):
            found_start = False
            while not found_start:
                line = fin_lines[context.line_num].strip()
                context.line_num += 1
                if line and (line[0] == '['):
                    found_start = True
                elif line:
                    props = _parse_config_line(line, context)
                    for prop in props:
                        # Look for name property
                        key = prop[0].strip().lower()
                        value = prop[1].strip()
                        if key == 'name':
                            scheme_names.append(value)
                        # end if
                    # end for
                # end if
                if context.line_num > num_lines:
                    break
                # end if
            # end while
        else:
            context.line_num += 1
        # end if
    # end while
    return scheme_names

########################################################################

class MetadataTable():
    """Class to hold a CCPP Metadata table including the table header
    (ccpp-table-properties section) and all of the associated table
    sections (ccpp-arg-table sections)."""

    __table_start = re.compile(r"(?i)\s*\[\s*ccpp-table-properties\s*\]")

    def __init__(self, run_env, table_name_in=None, table_type_in=None,
                 dependencies=None, relative_path=None, known_ddts=None,
                 var_dict=None, module=None, parse_object=None):
        """Initialize a MetadataTable, either with a name, <table_name_in>, and
        type, <table_type_in>, or with information from a file (<parse_object>).
        if <parse_object> is None, <dependencies> and <relative_path> are
          also stored.
        If <var_dict> and / or module are passed (not allowed with
          <parse_object), then a single MetadataSection is added with
          that information.
        """
        self.__pobj = parse_object
        self.__dependencies = dependencies
        self.__relative_path = relative_path
        self.__sections = list()
        self.__run_env = run_env
        if parse_object is None:
            if table_name_in is not None:
                self.__table_name = table_name_in
            else:
                raise ParseInternalError('MetadataTable requires a name')
            # end if
            if table_type_in is None:
                perr = 'MetadataTable requires a table type'
                raise ParseInternalError(perr)
            # end if
            if table_type_in in HEADER_TYPES:
                self.__table_type = table_type_in
            else:
                raise ParseSyntaxError("metadata arg table type",
                                       token=table_type_in,
                                       context=self.__pobj)
            # end if
            self.__start_context = None
            if (var_dict is not None) or module:
                if var_dict is None:
                    var_dict = VarDictionary(self.table_name, run_env)
                # end if
                # This interface is used by the Fortran parser so strip off
                #    the phase from the table name but not the section title
                stitle = self.table_name
                if self.table_type == SCHEME_HEADER_TYPE:
                    fnam, _, _ = CCPP_STATE_MACH.function_match(self.table_name)
                    self.__table_name = fnam or stitle
                # end if
                sect = MetadataSection(self.table_name, self.table_type,
                                       run_env, title=stitle,
                                       type_in=self.table_type, module=module,
                                       var_dict=var_dict, known_ddts=known_ddts)
                self.__sections.append(sect)
            # end if
        else:
            self.__table_name = None
            self.__table_type = None
            if dependencies is not None:
                perr = "dependencies not allowed as argument when reading file"
                raise ParseInternalError(perr)
            # end if
            if relative_path:
                perr = "relative_path not allowed as argument when reading file"
                raise ParseInternalError(perr)
            # end if
            if var_dict is not None: # i.e., not even an empty dict
                perr = "var_dict not allowed as argument when reading file"
                raise ParseInternalError(perr)
            if module:
                perr = "module not allowed as argument when reading file"
                raise ParseInternalError(perr)
            # end if
            if known_ddts is None:
                known_ddts = list()
            # end if
            self.__start_context = ParseContext(context=self.__pobj)
            self.__init_from_file(known_ddts, self.__run_env)
        # end if

    def __init_from_file(self, known_ddts, run_env):
        """ Read the table preamble, assume the caller already figured out
        the first line of the header using the header_start method."""
        curr_line, _ = self.__pobj.next_line()
        in_properties_header = True
        skip_rest_of_section = False
        self.__dependencies = list() # Default is no dependencies
        # Process lines until the end of the file or start of the next table.
        while ((curr_line is not None) and
               (not MetadataTable.table_start(curr_line))):
            if MetadataSection.header_start(curr_line):
                in_properties_header = False
            # end if
            if blank_metadata_line(curr_line):
                curr_line, _ = self.__pobj.next_line()
            elif in_properties_header:
                # Process the properties in this table header line
                for prop in _parse_config_line(curr_line, self.__pobj):
                    # Manually parse name, type, and table properties
                    key = prop[0].strip().lower()
                    value = prop[1].strip()
                    if key == 'name':
                        self.__table_name = value
                    elif key == 'type':
                        if value not in TABLE_TYPES:
                            self.__pobj.add_syntax_err("metadata table type",
                                                       token=value)
                        # end if
                        self.__table_type = value
                    elif key == 'dependencies':
                        if value.lower() == "none":
                            if self.__dependencies:
                                emsg = "dependencies = {} is ".format(value)
                                depends = ", ".join(self.__dependencies)
                                emsg += "incompatible with {}".format(depends)
                                self.__pobj.add_syntax_err(emsg)
                            else:
                                self.__dependencies = None
                            # end if
                        elif self.__dependencies is None:
                            emsg = "Cannot add dependencies, they have "
                            emsg += "already been declared as None"
                            self.__pobj.add_syntax_err(emsg)
                        else:
                            depends = [x.strip() for x in value.split(',')
                                       if x.strip()]
                            self.__dependencies.extend(depends)
                        # end if
                    elif key == 'relative_path':
                        self.__relative_path = value
                    else:
                        tok_type = "metadata table start property"
                        self.__pobj.add_syntax_err(tok_type, token=value)
                    # end if
                # end for
                curr_line, _ = self.__pobj.next_line()
            else:
                # Process a metadata section
                if MetadataSection.header_start(curr_line):
                    skip_rest_of_section = False
                    section = MetadataSection(self.table_name, self.table_type,
                                              run_env, parse_object=self.__pobj,
                                              known_ddts=known_ddts)
                    # Some table types only allow for one associated section
                    if ((len(self.__sections) == 1) and
                        (self.table_type in _SINGLETON_TABLE_TYPES)):
                        prev_title = self.__sections[0].title
                        emsg = "{}, '{}', table already contains '{}'"
                        self.__pobj.add_syntax_err(emsg.format(self.table_type,
                                                               section.title,
                                                               prev_title))
                    # end if
                    self.__sections.append(section)
                    # Note: Do not read next line, we are already on it.
                    curr_line, _ = self.__pobj.curr_line()
                elif not blank_metadata_line(curr_line):
                    if not skip_rest_of_section:
                        self.__pobj.add_syntax_err("metadata file line",
                                                   token=curr_line)
                        skip_rest_of_section = True
                    # end if
                    curr_line, _ = self.__pobj.next_line()
                else:
                    curr_line, _ = self.__pobj.next_line()
                # end if
            # end if
        # end while
        if self.__pobj.error_message:
            # Time to dump out error messages
            raise CCPPError(self.__pobj.error_message)
        # end if
        if self.table_type == "ddt":
            known_ddts.append(self.table_name)
        # end if
        if self.__dependencies is None:
            self.__dependencies = list()
        # end if

    def start_context(self, with_comma=True, nodir=True):
        """Return a context string for the beginning of the table"""
        return context_string(self.__start_context,
                              with_comma=with_comma, nodir=nodir)

    def sections(self):
        """Return the metadata header sections for this table"""
        if self.__sections:
            # Return a copy so it cannot be modified
            return list(self.__sections)
        return self.__sections

    @property
    def table_name(self):
        'Return the name of the metadata table'
        return self.__table_name

    @property
    def table_type(self):
        'Return the type of structure this header documents'
        return self.__table_type

    @property
    def dependencies(self):
        """Return the dependencies for this table"""
        return self.__dependencies

    @property
    def relative_path(self):
        """Return the relative path for the table's dependencies"""
        return self.__relative_path

    @property
    def run_env(self):
        """Return this table's CCPPFrameworkEnv object"""
        return self.__run_env

    def __repr__(self):
        '''Print representation for MetadataTable objects'''
        return "<{} {} @ 0X{:X}>".format(self.__class__.__name__,
                                         self.table_name, id(self))

    def __str__(self):
        '''Print string for MetadataTable objects'''
        return "<{} {}>".format(self.__class__.__name__, self.table_name)

    @classmethod
    def table_start(cls, line):
        """Return True iff <line> is a ccpp-table-properties header statement.
        """
        if (line is None) or blank_metadata_line(line):
            match = None
        else:
            match = cls.__table_start.match(line)
        # end if
        return match is not None

########################################################################

class MetadataSection(ParseSource):
    """Class to hold all information from a metadata header
    >>> MetadataSection("footable", "scheme", _DUMMY_RUN_ENV,                 \
                      parse_object=ParseObject("foobar.txt",                  \
                      ["name = footable", "type = scheme", "module = foo",    \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in"])) #doctest: +ELLIPSIS
    <__main__.MetadataSection foo / footable at 0x...>
    >>> MetadataSection("footable", "scheme", _DUMMY_RUN_ENV,                 \
                      parse_object=ParseObject("foobar.txt",                  \
                      ["name = footable", "type = scheme", "module = foobar", \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in"])).find_variable('horizontal_loop_extent') #doctest: +ELLIPSIS
    <metavar.Var horizontal_loop_extent: im at 0x...>
    >>> MetadataSection("footable", "scheme", _DUMMY_RUN_ENV,                 \
                      parse_object=ParseObject("foobar.txt",                  \
                      ["name = footable", "type = scheme", "module = foobar", \
                       "process = microphysics", "[ im ]",                    \
                       "standard_name = horizontal_loop_extent",              \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in"])).find_variable('horizontal_loop_extent') #doctest: +ELLIPSIS
    <metavar.Var horizontal_loop_extent: im at 0x...>
    >>> MetadataSection("footable", "scheme", _DUMMY_RUN_ENV,                 \
                      parse_object=ParseObject("foobar.txt",                  \
                      ["name = footable", "type=scheme", "module = foo",      \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       "  subroutine foo()"])).find_variable('horizontal_loop_extent') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    parse_source.ParseSyntaxError: Invalid variable property syntax, 'subroutine foo()', at foobar.txt:9
    >>> MetadataSection("footable", "scheme", _DUMMY_RUN_ENV,                 \
                      parse_object=ParseObject("foobar.txt",                  \
                      ["name = footable", "type = scheme", "module=foobar",   \
                       "[ im ]", "standard_name = horizontal_loop_extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).find_variable('horizontal_loop_extent').get_prop_value('local_name')
    'im'
    >>> MetadataSection("footable", "scheme", _DUMMY_RUN_ENV,                 \
                      parse_object=ParseObject("foobar.txt",                  \
                      ["name = footable", "type = scheme"                     \
                       "[ im ]", "standard_name = horizontalloop extent",     \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).find_variable('horizontal_loop_extent')

    >>> MetadataSection("footable", "scheme", _DUMMY_RUN_ENV,                 \
                      parse_object=ParseObject("foobar.txt",                  \
                      ["[ccpp-arg-table]", "name = foobar", "type = scheme"   \
                       "[ im ]", "standard_name = horizontal loop extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).find_variable('horizontal_loop_extent')

    >>> MetadataSection("foobar", "scheme", _DUMMY_RUN_ENV,                   \
                      parse_object=ParseObject("foobar.txt",                  \
                      ["name = foobar", "module = foo"                        \
                       "[ im ]", "standard_name = horizontal loop extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).find_variable('horizontal_loop_extent')

    >>> MetadataSection("foobar", "scheme", _DUMMY_RUN_ENV,                   \
                      parse_object=ParseObject("foobar.txt",                  \
                      ["name = foobar", "foo = bar"                           \
                       "[ im ]", "standard_name = horizontal loop extent",    \
                       "long_name = horizontal loop extent, start at 1",      \
                       "units = index | type = integer",                      \
                       "dimensions = () |  intent = in",                      \
                       ""], line_start=0)).find_variable('horizontal_loop_extent')

    >>> MetadataSection.header_start('[ ccpp-arg-table ]')
    True
    >>> MetadataSection.header_start('[ qval ]')
    False
    >>> MetadataSection.header_start('  local_name = foo')
    False
    >>> MetadataSection.variable_start('[ qval ]', ParseObject('foo.meta', []))
    'qval'
    >>> MetadataSection.variable_start('[ qval(hi_mom) ]', ParseObject('foo.meta', []))
    'qval(hi_mom)'
    >>> MetadataSection.variable_start('  local_name = foo', ParseContext(filename='foo.meta', linenum=1))

"""

    __header_start = re.compile(r"(?i)\s*\[\s*ccpp-arg-table\s*\]")

    __var_start = re.compile(r"^\[\s*"+FORTRAN_ID+r"\s*\]$")

    __vref_start = re.compile(r"^\[\s*"+FORTRAN_SCALAR_REF+r"\s*\]$")

    def __init__(self, table_name, table_type, run_env, parse_object=None,
                 title=None, type_in=None, module=None, process_type=None,
                 var_dict=None, known_ddts=None):
        """Initialize a new MetadataSection object.
        If <parse_object> is not None, initialize from the current file and
        location in <parse_object>.
        If <parse_object> is None, initialize from <title>, <type>, <module>,
        and <var_dict>. Note that if <parse_object> is not None, <title>,
        <type>, <module>, and <var_dict> are ignored.
        <table_name> and <table_type> are the name and type of the
        metadata header of which this section is a part. They must match
        the type and name of this section (once the name action has been
        removed, e.g., name = foo_init matches type foo).
        """
        self.__pobj = parse_object
        self.__variables = None # In case __init__ crashes
        self.__section_title = None
        self.__header_type = None
        self.__module_name = None
        self.__process_type = UNKNOWN_PROCESS_TYPE
        self.__section_valid = True
        self.__run_env = run_env
        if parse_object is None:
            if title is not None:
                self.__section_title = title
            else:
                raise ParseInternalError('MetadataSection requires a title')
            # end if
            if type_in is None:
                perr = 'MetadataSection requires a header type'
                raise ParseInternalError(perr)
            # end if
            if type_in in HEADER_TYPES:
                self.__header_type = type_in
            else:
                self.__pobj.add_syntax_err("metadata arg table type",
                                           token=type_in)
                self.__section_valid = False
            # end if
            mismatch = self.section_table_mismatch(table_name, table_type)
            if mismatch:
                self.__pobj.add_syntax_err(mismatch)
                self.__section_valid = False
            # end if
            mismatch = self.section_table_mismatch(table_name, table_type)
            if mismatch:
                raise CCPPError(mismatch)
            # end if
            if module is not None:
                self.__module_name = module
            else:
                perr = "MetadataSection requires a module name"
                self.__pobj.add_syntax_err(perr)
                self.__section_valid = False
            # end if
            if process_type is None:
                self.__process_type = UNKNOWN_PROCESS_TYPE
            else:
                self.__process_type = process_type
            # end if
            #  Initialize our ParseSource parent
            super().__init__(self.title, self.header_type, self.__pobj)
            self.__variables = VarDictionary(self.title, run_env)
            for var in var_dict.variable_list(): # Let this crash if no dict
                self.__variables.add_variable(var, run_env)
            # end for
            self.__start_context = None
        else:
            if known_ddts is None:
                known_ddts = list()
            # end if
            self.__start_context = ParseContext(context=self.__pobj)
            self.__init_from_file(table_name, table_type, known_ddts, run_env)
        # end if
        # Register this header if it is a DDT
        if self.header_type == 'ddt':
            register_fortran_ddt_name(self.title)
        # end if
        # Categorize the variables
        self._var_intents = {'in' : list(), 'out' : list(), 'inout' : list()}
        for var in self.variable_list():
            intent = var.get_prop_value('intent')
            if intent is not None:
                self._var_intents[intent].append(var)
            # end if
        # end for

    def _default_module(self):
        """Set a default module for this header"""
        mfile = self.__pobj.file_name
        if mfile[-5:] == '.meta':
            # Default value is a Fortran module that matches the filename
            def_mod = os.path.basename(mfile)[:-5]
        else:
            def_mod = os.path.basename(mfile)
            last_dot = def_mod.rfind('.')
            if last_dot >= 0:
                ldef = len(def_mod)
                def_mod = def_mod[:last_dot-ldef]
            # end if
        # end if
        return def_mod

    def __init_from_file(self, table_name, table_type, known_ddts, run_env):
        """ Read the section preamble, assume the caller already figured out
        the first line of the header using the header_start method."""
        start_ctx = context_string(self.__pobj)
        curr_line, _ = self.__pobj.next_line() # Skip past [ccpp-arg-table]
        while ((curr_line is not None) and
               (not MetadataSection.variable_start(curr_line, self.__pobj)) and
               (not MetadataSection.header_start(curr_line)) and
               (not MetadataTable.table_start(curr_line))):
            for prop in _parse_config_line(curr_line, self.__pobj):
                # Manually parse name, type, and module properties
                key = prop[0].strip().lower()
                value = prop[1].strip()
                if key == 'name':
                    self.__section_title = value
                elif key == 'type':
                    if value not in HEADER_TYPES:
                        self.__pobj.add_syntax_err("metadata table type",
                                                   token=value)
                        self.__section_valid = False
                        close = difflib.get_close_matches(value, HEADER_TYPES)
                        if close:
                            self.__header_type = close[0] # Allow error continue
                        # end if
                    # end if
                    # Set value even if error so future error msgs make sense
                    self.__header_type = value
                elif key == 'module':
                    if value != "None":
                        self.__module_name = value
                    else:
                        self.__pobj.add_syntax_err("metadata table, no module")
                        self.__module_name = 'INVALID' # Allow error continue
                        self.__section_valid = False
                    # end if
                elif key == 'process':
                    self.__process_type = value
                else:
                    self.__pobj.add_syntax_err("metadata table start property",
                                               token=value)
                    self.__process_type = 'INVALID' # Allow error continue
                    self.__section_valid = False
                # end if
            # end for
            curr_line, _ = self.__pobj.next_line()
        # end while
        if self.title is None:
            self.__pobj.add_syntax_err("metadata header start, no table name",
                                       token=curr_line)
            self.__section_valid = False
        # end if
        if self.header_type is None:
            self.__pobj.add_syntax_err("metadata header start, no table type",
                                       token=curr_line)
            self.__section_valid = False
        # end if
        if ((self.header_type != SCHEME_HEADER_TYPE) and
            (self.process_type != UNKNOWN_PROCESS_TYPE)):
            emsg = "process keyword only allowed for a scheme"
            self.__pobj.add_syntax_err(emsg, token=curr_line)
            self.__process_type = UNKNOWN_PROCESS_TYPE # Allow error continue
            self.__section_valid = False
        # end if
        mismatch = self.section_table_mismatch(table_name, table_type)
        if mismatch:
            self.__pobj.add_syntax_err(mismatch)
            self.__section_valid = False
        # end if
        if run_env.logger and run_env.logger.isEnabledFor(logging.INFO):
            run_env.logger.info("Parsing {} {}{}".format(self.header_type,
                                                         self.title, start_ctx))
        # end if
        if self.header_type == "ddt":
            known_ddts.append(self.title)
        # end if
        # We need a default module if none was listed
        if self.module is None:
            self.__module_name = self._default_module()
        # end if
        #  Initialize our ParseSource parent
        super().__init__(self.title, self.header_type, self.__pobj)
        # Read the variables
        valid_lines = True
        self.__variables = VarDictionary(self.title, run_env)
        while valid_lines:
            newvar, curr_line = self.parse_variable(curr_line, known_ddts)
            valid_lines = newvar is not None
            if valid_lines:
                if run_env.logger and run_env.logger.isEnabledFor(logging.DEBUG):
                    dmsg = 'Adding {} to {}'
                    lname = newvar.get_prop_value('local_name')
                    run_env.logger.debug(dmsg.format(lname, self.title))
                # end if
                self.__variables.add_variable(newvar, run_env)
                # Check to see if we hit the end of the table
                valid_lines = not MetadataSection.header_start(curr_line)
            else:
                # We have a bad variable, see if we have more variables
                lname = MetadataSection.variable_start(curr_line, self.__pobj)
                valid_lines = lname is not None
                # end while
            # end if
        # end while

    def parse_variable(self, curr_line, known_ddts):
        """Parse a new metadata variable beginning on <curr_line>.
        The header line has the format [ <valid_fortran_symbol> ].
        """
        newvar = None
        var_ok = True # Set to False if an error is detected
        valid_line = ((curr_line is not None) and
                      (not MetadataSection.header_start(curr_line)) and
                      (not MetadataTable.table_start(curr_line)))
        if valid_line:
             # variable_start handles exception
            local_name = MetadataSection.variable_start(curr_line, self.__pobj)
        else:
            local_name = None
        # end if
        if local_name is None:
            # This is not a valid variable line, punt (should be end of table)
            valid_line = False
        # end if
        # Parse lines until invalid line is found
        # NB: Header variables cannot have embedded blank lines
        if valid_line:
            var_props = {}
            var_props['local_name'] = local_name
            # Grab context that points at beginning of definition
            context = ParseContext(context=self.__pobj)
        else:
            var_props = None
        # end if
        while valid_line:
            curr_line, _ = self.__pobj.next_line()
            valid_line = ((curr_line is not None) and
                          (not MetadataSection.header_start(curr_line)) and
                          (not MetadataTable.table_start(curr_line)) and
                          (MetadataSection.variable_start(curr_line,
                                                          self.__pobj) is None))
            # A valid line may have multiple properties (separated by '|')
            if valid_line:
                properties = _parse_config_line(curr_line, self.__pobj)
                for prop in properties:
                    pname = prop[0].strip().lower()
                    pval_str = prop[1].strip()
                    if ((pname == 'type') and
                        (not check_fortran_intrinsic(pval_str, error=False))):
                        if pval_str in known_ddts:
                            pval = pval_str
                            pname = 'ddt_type'
                        else:
                            errmsg = "Unknown DDT type, {}".format(pval_str)
                            self.__pobj.add_syntax_err(errmsg)
                            self.__section_valid = False
                            var_ok = False
                        # end if
                    else:
                        # Make sure this is a match
                        check_prop = Var.get_prop(pname)
                        if check_prop is not None:
                            pval = check_prop.valid_value(pval_str)
                        else:
                            emsg = "variable property name"
                            self.__pobj.add_syntax_err(emsg, token=pname)
                            self.__section_valid = False
                            var_ok = False
                        # end if
                        if pval is None:
                            errmsg = "'{}' property value"
                            self.__pobj.add_syntax_err(errmsg.format(pname),
                                                       token=pval_str)
                            self.__section_valid = False
                            var_ok = False
                        # end if
                    # end if
                    if var_ok:
                        # If we get this far, we have a valid property.
                        # Special case for dimensions, turn them into ranges
                        if pname == 'dimensions':
                            porig = pval
                            pval = list()
                            for dim in porig:
                                if ':' in dim:
                                    pval.append(dim)
                                else:
                                    cone_str = 'ccpp_constant_one:{}'
                                    pval.append(cone_str.format(dim))
                                # end if
                            # end for
                        # end if
                        # Add the property to our Var dictionary
                        var_props[pname] = pval
                    # end if
                # end for
            # end if
        # end while
        if var_ok and (var_props is not None):
            # Check for array reference
            sub_name = MetadataSection.check_array_reference(local_name,
                                                             var_props, context)
            if sub_name:
                var_props['local_name'] = sub_name
            # end if (else just leave the local name alone)
            try:
                newvar = Var(var_props, self, self.run_env, context=context)
            except CCPPError as verr:
                self.__pobj.add_syntax_err(verr, skip_context=True)
                var_ok = False
                self.__section_valid = False
            # end try
        # No else, will return None for newvar
        # end if
        return newvar, curr_line

    @staticmethod
    def check_array_reference(local_name, var_dict, context):
        """If <local_name> is an array reference, check it against
        the 'dimensions' property in <var_dict>. If <local_name> is an
        array reference, return it with the colons filled in with the
        dictionary dimensions, otherwise, return None.
        >>> MetadataSection.check_array_reference('foo', {'dimensions':['ccpp_constant_one:bar', 'ccpp_constant_one:baz']}, ParseContext(filename='foo.meta'))

        >>> MetadataSection.check_array_reference('foo', {}, ParseContext(filename='foo.meta'))

        >>> MetadataSection.check_array_reference('foo(qux', {'dimensions':['ccpp_constant_one:bar', 'ccpp_constant_one:baz']}, ParseContext(filename='foo.meta')) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ParseInternalError: Invalid scalar reference, foo(qux, in foo.meta
        >>> MetadataSection.check_array_reference('foo(qux)', {'dimensions':['ccpp_constant_one:bar', 'ccpp_constant_one:baz']}, ParseContext(filename='foo.meta')) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ParseInternalError: foo has rank 2 but foo(qux) has 0, in foo.meta
        >>> MetadataSection.check_array_reference('foo(:,qux)', {'dimensions':['ccpp_constant_one:bar', 'ccpp_constant_one:baz']}, ParseContext(filename='foo.meta')) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ParseInternalError: foo has rank 2 but foo(:,qux) has 1, in foo.meta
        >>> MetadataSection.check_array_reference('foo(:,qux)', {'foo':['ccpp_constant_one:bar', 'ccpp_constant_one:baz']}, ParseContext(filename='foo.meta')) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ParseInternalError: Missing variable dimensions, foo(:,qux), in foo.meta
        >>> MetadataSection.check_array_reference('foo(:,:,qux)', {'dimensions':['ccpp_constant_one:bar']}, ParseContext(filename='foo.meta')) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ParseInternalError: foo has rank 1 but foo(:,:,qux) has 2, in foo.meta
        >>> MetadataSection.check_array_reference('foo(:,:,qux)', {'dimensions':['ccpp_constant_one:bar','ccpp_constant_one:baz']}, ParseContext(filename='foo.meta'))
        'foo(:, :, qux)'
        """
        retval = None
        if check_fortran_id(local_name, var_dict, False) is None:
            rmatch = FORTRAN_SCALAR_REF_RE.match(local_name)
            if rmatch is None:
                errmsg = 'Invalid scalar reference, {}{}'
                ctx = context_string(context)
                raise ParseInternalError(errmsg.format(local_name, ctx))
            # end if
            rname = rmatch.group(1)
            rdims = [x.strip() for x in rmatch.group(2).split(',')]
            if 'dimensions' in var_dict:
                vdims = [x.strip() for x in var_dict['dimensions']]
            else:
                errmsg = 'Missing variable dimensions, {}{}'
                ctx = context_string(context)
                raise ParseInternalError(errmsg.format(local_name, ctx))
            # end if
            colon_rank = len([x for x in rdims if x == ':'])
            if colon_rank != len(vdims):
                errmsg = '{} has rank {} but {} has {}{}'
                ctx = context_string(context)
                raise ParseInternalError(errmsg.format(rname, len(vdims),
                                                       local_name, colon_rank,
                                                       ctx))
            # end if
            sub_dims = list()
            sindex = 0
            for rind in rdims:
                if rind == ':':
                    sub_dims.append(':')
                    sindex += 1
                else:
                    sub_dims.append(rind)
                # end if
            # end for
            retval = '{}({})'.format(rname, ', '.join(sub_dims))
        # end if
        return retval

    def variable_list(self, std_vars=True, loop_vars=True, consts=True):
        """Return an ordered list of the header's variables"""
        return self.__variables.variable_list(recursive=False,
                                              std_vars=std_vars,
                                              loop_vars=loop_vars,
                                              consts=consts)

    def find_variable(self, std_name, use_local_name=False):
        """Find a variable in this header's dictionary"""
        var = None
        if use_local_name:
            var = self.__variables.find_local_name(std_name)
        else:
            var = self.__variables.find_variable(std_name, any_scope=False)
        # end if
        return var

    def convert_dims_to_standard_names(self, var, logger=None, context=None):
        """Convert the dimension elements in <var> to standard names by
        by using other variables in this header.
        """
        std_dims = list()
        vdims = var.get_dimensions()
        # Check for bad dimensions
        if vdims is None:
            vdim_prop = var.get_prop_value('dimensions').strip()
            if vdim_prop[0] == '(':
                vdim_prop = vdim_prop[1:]
            # end if
            if vdim_prop[-1] == ')':
                vdim_prop = vdim_prop[0:-1]
            # end if
            vdim_strs = [x.strip() for x in vdim_prop.split(',')]
            lname = var.get_prop_value('local_name')
            ctx = context_string(var.context)
            sep = ''
            errstr = "{}{}: Invalid dimension, '{}'{}"
            errmsg = ''
            for vdim in vdim_strs:
                if not check_fortran_id(vdim, None, False):
                    errmsg += errstr.format(sep, lname, vdim, ctx)
                    sep = '\n'
                # end if
            # end for
            raise CCPPError("{}".format(errmsg))
        # end if
        for dim in vdims:
            std_dim = list()
            if ':' not in dim:
                # Metadata dimensions always have an explicit start
                var_one = CCPP_CONSTANT_VARS.find_local_name('1')
                if var_one is not None:
                    std = var_one.get_prop_value('standard_name')
                    std_dim.append(std)
                # end if
            # end if
            for item in dim.split(':'):
                try:
                    _ = int(item)
                    dvar = CCPP_CONSTANT_VARS.find_local_name(item)
                    if dvar is not None:
                        # If this integer value is a CCPP standard int, use that
                        dname = dvar.get_prop_value('standard_name')
                    else:
                        # Some non-standard integer value
                        dname = item
                    # end if
                except ValueError:
                    # Not an integer, try to find the standard_name
                    if not item:
                        # Naked colons are okay
                        dname = ''
                    else:
                        dvar = self.find_variable(item, use_local_name=True)
                        if dvar is not None:
                            dname = dvar.get_prop_value('standard_name')
                        else:
                            dname = None
                        # end if
                    # end if
                    if dname is None:
                        errmsg = "Unknown dimension element, {}, in {}{}"
                        std = var.get_prop_value('local_name')
                        ctx = context_string(context)
                        if logger is not None:
                            errmsg = "ERROR: " + errmsg
                            logger.error(errmsg.format(item, std, ctx))
                            dname = unique_standard_name()
                        else:
                            raise CCPPError(errmsg.format(item, std, ctx))
                        # end if
                    # end if
                # end try
                if dname is not None:
                    std_dim.append(dname)
                else:
                    std_dim = None
                    break
                # end if
            # end for
            if std_dim is not None:
                std_dims.append(':'.join(std_dim))
            else:
                break
            # end if
        # end for

        return std_dims

    def prop_list(self, prop_name):
        """Return list of <prop_name> values for this scheme's arguments"""
        return self.__variables.prop_list(prop_name)

    def section_table_mismatch(self, table_title, table_type):
        """Return  an error string if this arg table does not match its
        metadata table parent. If they match , return an empty string."""
        mismatch = ""
        # The header type must match its table's type
        if self.header_type is None:
            mstr = "Invalid section type, 'None'"
            mismatch += mstr.format(self.header_type, table_type)
        elif table_type != self.header_type:
            mstr = "Section type, '{}', does not match table type, '{}'"
            mismatch += mstr.format(self.header_type, table_type)
        # end if
        if self.header_type == SCHEME_HEADER_TYPE:
            # For schemes, strip off the scheme function phase (e.g., _init)
            sect_func, _, _ = CCPP_STATE_MACH.function_match(self.title)
        else:
            sect_func = self.title
        # end if
        # The Fortran parser cannot tell a scheme from a host subroutine
        # Detect this and adjust
        if sect_func is None:
            sect_func = self.title
        # end if
        # The header name (minus phase) must match its table's name
        if table_title != sect_func:
            if mismatch:
                mismatch += '\n'
            # end if
            mstr = "Section name, '{}', does not match table title, '{}'"
            mismatch += mstr.format(self.title, table_title)
        # end if
        if mismatch:
            mismatch += context_string(self.__pobj)
        # end if
        return mismatch

    @staticmethod
    def variable_start(line, pobj):
        """Return variable name if <line> is an interface metadata table header
        """
        if line is None:
            match = None
        else:
            match = MetadataSection.__var_start.match(line)
            if match is None:
                match = MetadataSection.__vref_start.match(line)
                if match is not None:
                    name = match.group(1)+'('+match.group(2)+')'
                # end if
            else:
                name = match.group(1)
            # end if
        # end if
        if match is not None:
            if not MetadataSection.is_scalar_reference(name):
                pobj.add_syntax_err("local variable name", token=name)
                name = None
            # end if
        else:
            name = None
        # end if
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
            # end for
        # end with

    def __repr__(self):
        base = super().__repr__()
        pind = base.find(' object ')
        if pind >= 0:
            pre = base[0:pind]
        else:
            pre = '<MetadataSection'
        # end if
        bind = base.find('at 0x')
        if bind >= 0:
            post = base[bind:]
        else:
            post = '>'
        # end if
        return '{} {} / {} {}'.format(pre, self.module, self.title, post)

    def __del__(self):
        try:
            del self.__variables
        except AttributeError:
            pass

    def start_context(self, with_comma=True, nodir=True):
        """Return a context string for the beginning of the table"""
        return context_string(self.__start_context,
                              with_comma=with_comma, nodir=nodir)

    @property
    def title(self):
        """Return the name of the metadata arg_table"""
        return self.__section_title

    @property
    def module(self):
        """Return the module name for this header (if it exists)"""
        return self.__module_name

    @property
    def header_type(self):
        """Return the type of structure this header documents"""
        return self.__header_type

    @property
    def process_type(self):
        """Return the type of physical process this header documents"""
        return self.__process_type

    @property
    def has_variables(self):
        """Convenience function for finding empty headers"""
        return self.__variables

    @property
    def run_env(self):
        """Return this section's CCPPFrameworkEnv object"""
        return self.__run_env

    @property
    def valid(self):
        """Return True iff we did not encounter an error creating
        this section"""
        return self.__section_valid

    def __str__(self):
        '''Print string for MetadataSection objects'''
        return "<{} {}>".format(self.__class__.__name__, self.title)

    @classmethod
    def header_start(cls, line):
        """Return True iff <line> is a Metadata section header (ccpp-arg-table).
        """
        if (line is None) or blank_metadata_line(line):
            match = None
        else:
            match = cls.__header_start.match(line)
        # end if
        return match is not None

    @staticmethod
    def is_scalar_reference(test_val):
        """Return True iff <test_val> refers to a Fortran scalar."""
        return check_fortran_ref(test_val, None, False) is not None

########################################################################

if __name__ == "__main__":
    import doctest
    from framework_env import CCPPFrameworkEnv
    _DUMMY_RUN_ENV = CCPPFrameworkEnv(None, {'host_files':'',
                                             'scheme_files':'',
                                             'suites':''})
    doctest.testmod()
