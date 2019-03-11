#!/usr/bin/env python

if __name__ == '__main__' and __package__ is None:
    import sys
    import os.path
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import re
from parse_tools import ParseSyntaxError, ParseInternalError
from parse_tools import check_fortran_intrinsic, check_fortran_type
from parse_tools import ParseContext, ParseSource, context_string
from metavar import Var

# A collection of types and tools for parsing Fortran code to support
# CCPP metadata parsing. The purpose of this code is limited to type
# checking of routines with CCPP metadata caps, therefore full routines are
# not parsed and a full Fortran syntax tree is not warranted.

########################################################################

# Fortran ID specifier (do not want a group like FORTRAN_ID from parse_tools)
_fortran_id_ = r"(?:[A-Za-z][A-Za-z0-9_]*)"
# Regular expression for a dimension specifier
_dimid_ = r"(?:"+_fortran_id_+r"|[0-9]+)"
_dimcolon_ = r"(?:\s*:\s*"+_dimid_+r"?\s*)"
_dimcolons_ = r"(?:"+_dimid_+r"?"+_dimcolon_+_dimcolon_+r"?)"
_dimspec_ = r"(?:"+_dimid_+r"|"+_dimcolons_+r")"
_dims_list_ = _dimspec_+r"(?:\s*,\s*"+_dimspec_+r"){0,6}"
# Regular expression for a variable name with optional dimensions
_var_id_re_ = re.compile(r"("+_fortran_id_+r")\s*(\(\s*"+_dims_list_+r"\s*\))?$")

########################################################################

class Ftype(object):
    """Ftype is the base class for all Fortran types
    It is also the final type for intrinsic types except for character
    >>> Ftype('integer').typestr
    'integer'
    >>> Ftype('integer', kind_in='( kind= I8').__str__() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid kind_selector, '( kind= I8', at <standard input>:1
    >>> Ftype('integer', kind_in='(kind=I8)').__str__()
    'integer(kind=I8)'
    >>> Ftype('integer', kind_in='(I8)').__str__()
    'integer(kind=I8)'
    >>> Ftype('real', kind_in='(len=*,R8)').__str__() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid kind_selector, '(len=*,R8)', at <standard input>:1
    >>> Ftype(typestr_in='real', line_in='real(kind=kind_phys)') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseInternalError: typestr_in and line_in cannot both be used in a single call, at <standard input>:1
    >>> Ftype(typestr_in='real', line_in='real(kind=kind_phys)', context=ParseContext(linenum=37, filename="foo.F90")) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseInternalError: typestr_in and line_in cannot both be used in a single call, at foo.F90:37
    >>> Ftype(line_in='real(kind=kind_phys)').__str__()
    'real(kind=kind_phys)'
    >>> Ftype(line_in="integer").__str__()
    'integer'
    >>> Ftype(line_in="INTEGER").__str__()
    'INTEGER'
    """

    # Note that "character" is not in intrinsic_types even though it is a
    # Fortran intrinsic. This is because character has its own type.
    __intrinsic_types__ = [ r"integer", r"real", r"logical",
                        r"double\s*precision", r"complex" ]

    itype_re = re.compile(r"(?i)({})\s*(\([A-Za-z0-9,=_\s]+\))?".format(r"|".join(__intrinsic_types__)))
    kind_re = re.compile(r"(?i)kind\s*(\()?\s*([\'\"])?(.+?)([\'\"])?\s*(\))?")

    __attr_spec__ = ['allocatable', 'asynchronous', 'dimension', 'external',
                     'intent', 'intrinsic', 'bind', 'optional', 'parameter',
                     'pointer', 'private', 'protected', 'public', 'save',
                     'target', 'value', 'volatile']

    __sname_num__ = 0 # Counter for unique standard names

    def __init__(self, typestr_in=None, kind_in=None, line_in=None, context=None):
        if context is None:
            self._context = ParseContext()
        else:
            self._context = ParseContext(context=context)
        # We have to distinguish which type of initialization we have
        if typestr_in is not None:
            if line_in is not None:
                raise ParseInternalError("typestr_in and line_in cannot both be used in a single call", self._context)
            # End if
            self._typestr = typestr_in
            self.default_kind = kind_in is None
            if kind_in is None:
                self._kind = None
            elif kind_in[0] == '(':
                # Parse an explicit kind declaration
                self._kind = self.parse_kind_selector(kind_in)
            else:
                # The kind has already been parsed for us (e.g., by character)
                self._kind = kind_in
        elif kind_in is not None:
            raise ParseInternalError("kind_in cannot be passed without typestr_in", self._context)
        elif line_in is not None:
            match = Ftype.type_match(line_in)
            self._match_len = len(match.group(0))
            if match is None:
                raise ParseSyntaxError("type declaration", token=line_in, context=self._context)
            elif check_fortran_intrinsic(match.group(1)):
                self._typestr = match.group(1)
                if match.group(2) is not None:
                    # Parse kind section
                    self._kind = self.parse_kind_selector(match.group(2).strip())
                else:
                    self._kind = None
                # End if
                self.default_kind = self._kind is None
            else:
                raise ParseSyntaxError("type declaration", token=line_in, context=self._context)
        else:
            raise ParseInternalError("At least one of typestr_in or line must be passed", self._context)

    def parse_kind_selector(self, kind_selector, context=None):
        if context is None:
            if hasattr(self, 'context'):
                context = self._context
            else:
                context = ParseContext()
            # End if
        kind = None
        if (kind_selector[0] == '(') and (kind_selector[-1] == ')'):
            args = kind_selector[1:-1].split('=')
        else:
            args = kind_selector.split('=')
        # End if
        if (len(args) > 2) or (len(args) < 1):
            raise ParseSyntaxError("kind_selector", token=kind_selector, context=context)
        elif len(args) == 1:
            kind = args[0].strip()
        elif args[0].strip().lower() != 'kind':
            # We have two args, the first better be kind
            raise ParseSyntaxError("kind_selector", token=kind_selector, context=context)
        else:
            # We have two args and the second is our kind string
            kind = args[1].strip()
        # End if
        # One last check for missing right paren
        match = Ftype.kind_re.search(kind)
        if match is not None:
            if match.group(2) is not None:
                if match.group(2) != match.group(4):
                    raise ParseSyntaxError("kind_selector", token=kind_selector, context=context)
                elif (match.group(1) is None) and (match.group(5) is not None):
                    raise ParseSyntaxError("kind_selector", token=kind_selector, context=context)
                elif (match.group(1) is not None) and (match.group(5) is None):
                    raise ParseSyntaxError("kind_selector", token=kind_selector, context=context)
                else:
                    pass
            else:
                pass
        elif kind[0:4].lower() == "kind":
            raise ParseSyntaxError("kind_selector", token=kind_selector, context=context)
        else:
            pass
        return kind

    @classmethod
    def type_match(cls, line):
        """Return an RE match if <line> represents an Ftype declaration"""
        match = Ftype.itype_re.match(line.strip())
        return match

    @classmethod
    def reassemble_parens(cls, propstr, errstr, context, splitstr=','):
        """Return list of <propstr> split by top-level instances of <splitstr>.
        Occurrences of <splitstr> in character contexts or in parentheses are
        ignored.
        >>> Ftype.reassemble_parens("a(b, c),d,e()", 'spec', ParseContext())
        ['a(b, c)', 'd', 'e()']
        >>> Ftype.reassemble_parens("dimension(size(Grid%xlon,1),NSPC1),  intent(in)", 'spec', ParseContext())
        ['dimension(size(Grid%xlon,1),NSPC1)', 'intent(in)']
        """
        vars = list()
        proplist = propstr.split(splitstr)
        while len(proplist) > 0:
            var = proplist.pop(0)
            while var.count('(') != var.count(')'):
                if len(proplist) == 0:
                    raise ParseSyntaxError(errstr, token=propstr, context=context)
                # End if
                var = var + ',' + proplist.pop(0)
            # End while
            var = var.strip()
            if len(var) > 0:
                vars.append(var)
            # End if
        # End while
        return vars

    @classmethod
    def parse_attr_specs(cls, propstring, context):
        'Return a list of variable properties'
        properties = list()
        # Remove leading comma
        propstring = propstring.strip()
        if (len(propstring) > 0) and (propstring[0] == ','):
            propstring = propstring[1:].lstrip()
        # End if
        proplist = cls.reassemble_parens(propstring, 'attr_spec', context)
        for prop in proplist:
            prop = prop.strip().lower()
            if '(' in prop:
                # Strip out value from dimensions, bind, or intent
                pval = prop[0:prop.index('(')].strip()
            else:
                pval = prop
            # End if
            if pval not in cls.__attr_spec__:
                raise ParseSyntaxError('attr_spec', token=prop, context=context)
            # End if
            properties.append(prop)
        # End for
        return properties

    @classmethod
    def unique_standard_name(cls):
        cls.__sname_num__ = cls.__sname_num__ + 1
        return 'enter_standard_name_{}'.format(cls.__sname_num__)

    @property
    def typestr(self):
        return self._typestr

    @property
    def kind(self):
        return self._kind

    @property
    def type_len(self):
        return self._match_len

    def __str__(self):
        """Return a string of the declaration of the type"""
        if self.default_kind:
            return self.typestr
        elif check_fortran_intrinsic(self.typestr):
            return "{}(kind={})".format(self.typestr, self._kind)
        else:
            # Derived type
            return "{}({})".format(self.typestr, self._kind)

########################################################################

class Ftype_character(Ftype):
    """Ftype_character is a type that represents character types
    >>> Ftype_character.type_match('character') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> Ftype_character.type_match('CHARACTER') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> Ftype_character.type_match('chaRActer (len=*)') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> Ftype_character.type_match('integer')

    >>> Ftype_character('character', ParseContext(169, 'foo.F90')).__str__()
    Traceback (most recent call last):
    ParseSyntaxError: Invalid character declaration, 'character', at foo.F90:170
    >>> Ftype_character('character ::', ParseContext(171, 'foo.F90')).__str__()
    'character(len=1)'
    >>> Ftype_character('CHARACTER(len=*)', ParseContext(174, 'foo.F90')).__str__()
    'CHARACTER(len=*)'
    >>> Ftype_character('CHARACTER(len=:)', None).__str__()
    'CHARACTER(len=:)'
    >>> Ftype_character('character(*)', None).__str__()
    'character(len=*)'
    >>> Ftype_character('character*7', None).__str__() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid character declaration, 'character*7', at <standard input>:1
    >>> Ftype_character('character*7,', None).__str__()
    'character(len=7)'
    >>> Ftype_character("character (kind=kind('a')", None).__str__() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid kind_selector, 'kind=kind('a'', at <standard input>:1
    >>> Ftype_character("character (kind=kind('a'))", None).__str__()
    "character(len=1, kind=kind('a'))"
    >>> Ftype_character("character  (13, kind=kind('a'))", None).__str__()
    "character(len=13, kind=kind('a'))"
    >>> Ftype_character("character  (len=13, kind=kind('a'))", None).__str__()
    "character(len=13, kind=kind('a'))"
    >>> Ftype_character("character  (kind=kind('b'), len=15)", None).__str__()
    "character(len=15, kind=kind('b'))"
    """

    char_re = re.compile(r"(?i)(character)\s*(\([A-Za-z0-9,=*:\s\'\"()]+\))?")
    chartrail_re = re.compile(r"\s*[,:]|\s+[A-Z]")
    oldchar_re = re.compile(r"(?i)(character)\s*(\*)\s*([0-9]+\s*)")
    oldchartrail_re = re.compile(r"\s*[,]|\s+[A-Z]")
    len_token_re = re.compile(r"(?i)([:]|[*]|[0-9]+|[A-Z][A-Z0-9_]*)$")

    @classmethod
    def type_match(cls, line):
        """Return an RE match if <line> represents an Ftype_character declaration"""
        # Try old style first to eliminate as a possibility
        match = Ftype_character.oldchar_re.match(line.strip())
        if match is None:
            match = Ftype_character.char_re.match(line.strip())
        # End if
        return match

    def __init__(self, line, context):
        """Initialize a character type from a declaration line"""

        clen = None
        kind = None # This will be interpreted as default kind
        match = Ftype_character.type_match(line)
        if match is None:
            raise ParseSyntaxError("character declaration", token=line, context=context)
        elif len(match.groups()) == 3:
            self._match_len = len(match.group(0))
            # We have an old style character declaration
            if match.group(2) != '*':
                raise ParseSyntaxError("character declaration", token=line, context=context)
            elif Ftype_character.oldchartrail_re.match(line.strip()[len(match.group(0)):]) is None:
                raise ParseSyntaxError("character declaration", token=line, context=context)
            else:
                clen = match.group(3)
            # End if
        elif match.group(2) is not None:
            self._match_len = len(match.group(0))
            # Parse attributes (strip off parentheses)
            attrs = [ x.strip() for x in match.group(2)[1:-1].split(',') ]
            if len(attrs) == 0:
                # Empty parentheses is not allowed
                raise ParseSyntaxError("char_selector", token=match.group(2), context=context)
            if len(attrs) > 2:
                # Too many attributes!
                raise ParseSyntaxError("char_selector", token=match.group(2), context=context)
            elif attrs[0][0:4].lower() == "kind":
                # The first arg is kind, try to parse it
                kind = self.parse_kind_selector(attrs[0], context=context)
                # If there is a second arg, it must be of form len=<length_selector>
                if len(attrs) == 2:
                    clen = self.parse_len_select(attrs[1], context, len_optional=False)
            elif len(attrs) == 2:
                # We have both a len and a kind, len first
                clen = self.parse_len_select(attrs[0], context, len_optional=True)
                kind = self.parse_kind_selector(attrs[1], context)
            else:
                # We just a len argument
                clen = self.parse_len_select(attrs[0], context, len_optional=True)
            # End if
        else:
            self._match_len = len(match.group(0))
            # We had better check the training characters
            if Ftype_character.chartrail_re.match(line.strip()[len(match.group(0)):]) is None:
                raise ParseSyntaxError("character declaration", token=line, context=context)
        # End if
        if clen is None:
            clen = 1
        # End if
        self.lenstr = "{}".format(clen)
        super(Ftype_character, self).__init__(typestr_in=match.group(1), kind_in=kind, context=context)

    def parse_len_token(self, token, context):
        """Check to make sure token is a valid length identifier"""
        match = Ftype_character.len_token_re.match(token)
        if match is not None:
            return match.group(1)
        else:
            raise ParseSyntaxError("length type-param-value", token=token, context=context)

    def parse_len_select(self, lenselect, context, len_optional=True):
        """Parse a character type length_selector"""
        largs = [ x.strip() for x in lenselect.split('=') ]
        if len(largs) > 2:
            raise ParseSyntaxError("length_selector", token=lenselect, context=context)
        elif (not len_optional) and ((len(largs) != 2) or (largs[0].lower() != 'len')):
            raise ParseSyntaxError("length_selector when len= is required", token=lenselect, context=context)
        elif len(largs) == 2:
            if largs[0].lower() != 'len':
                raise ParseSyntaxError("length_selector", token=lenselect, context=context)
            else:
                return self.parse_len_token(largs[1], context)
        elif len_optional:
            return self.parse_len_token(largs[0], context)
        else:
            raise ParseSyntaxError("length_selector when len= is required", token=lenselect, context=context)

    def __str__(self):
        """Return a string of the declaration of the type
        For characters, we will always print an explicit len modifier
        """
        if self.default_kind:
            kind_str = ""
        else:
            kind_str = ", kind={}".format(self.kind)
        # End if
        return "{}(len={}{})".format(self.typestr, self.lenstr, kind_str)

########################################################################

class Ftype_type_decl(Ftype):
    """Ftype_type_decl is a type that represents derived Fortran type
    declarations.
    >>> Ftype_type_decl.type_match('character')

    >>> Ftype_type_decl.type_match('type(foo)') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> Ftype_type_decl.type_def_line('type GFS_statein_type')
    ['GFS_statein_type', None, None]
    >>> Ftype_type_decl.type_def_line('type GFS_statein_type (n, m) ')
    ['GFS_statein_type', None, '(n, m)']
    >>> Ftype_type_decl.type_def_line('type, public, extends(foo) :: GFS_statein_type')
    ['GFS_statein_type', ['public', 'extends(foo)'], None]
    >>> Ftype_type_decl.type_def_line('type(foo) :: bar')

    """

    __type_decl_re__ = re.compile(r"(?i)(type)\s*\(\s*([A-Z][A-Z0-9_]*)\s*\)?")

    __type_attr_spec__ = ['abstract', 'bind', 'extends', 'private', 'public']

    def __init__(self, line, context):
        """Initialize an extended type from a declaration line"""
        match = Ftype_type_decl.type_match(line)
        if match is None:
            raise ParseSyntaxError("type declaration", token=line, context=context)
        else:
            self._match_len = len(match.group(0))
            self._class = match.group(1)
            self._typestr = match.group(2)
            self._kind = self.typestr
        # End if

    @classmethod
    def type_match(cls, line):
        """Return an RE match if <line> represents an Ftype_type_decl declaration"""
        match = Ftype_type_decl.__type_decl_re__.match(line.strip())
        # End if
        return match

    @classmethod
    def type_def_line(cls, line):
        """Return a type information if <line> represents the start
        of a type definition"""
        type_def = None
        if not cls.type_match(line):
            sline = line.strip()
            if sline.lower()[0:4] == 'type':
                if '::' in sline:
                    elements = sline.split('::')
                    type_name = elements[1].strip()
                    type_props = [x.strip() for x in elements[0].split(',')[1:]]
                else:
                    # Plain type decl
                    type_name = sline.split(' ', 1)[1].strip()
                    type_props = None
                # End if
                if '(' in type_name:
                    tnstr = type_name.split('(')
                    type_name = tnstr[0].strip()
                    type_params = '(' + tnstr[1].rstrip()
                else:
                    type_params = None
                # End if
                type_def = [type_name, type_props, type_params]
            # End if
        # End if
        return type_def

    def __str__(self):
        return '{}({})'.format(self._class, self.typestr)

########################################################################
def Ftype_factory(line, context):
########################################################################
    "Return an appropriate type object if there is a match, otherwise None"
    # We have to cut off the line at the end of any possible type info
    # Strip comments first (might have an = character)
    if '!' in line:
        line = line[0:line.index('!')].rstrip()
    # End if
    ppos = line.find('(')
    cpos = line.find(',')
    if ppos >= 0:
        if (cpos >= 0) and (cpos < ppos):
            # Whatever parentheses there are, they are not part of type
            line = line[0:cpos]
        else:
            # Find matching right parenthesis
            depth = 1
            epos = len(line)
            pepos = ppos + 1
            while (depth > 0) and (pepos < epos):
                if line[pepos] == '(':
                    depth = depth + 1
                elif line[pepos] == ')':
                    depth = depth - 1
                # End if
                pepos = pepos + 1
            # End while
            line = line[0:pepos+1]
        # End if
    elif cpos >= 0:
        line = line[0:cpos]
    # End if
    tmatch = Ftype.type_match(line)
    if tmatch is None:
        tobj = None
    else:
        tobj = Ftype(line_in=line, context=context)
    # End if
    if tmatch is None:
        tmatch = Ftype_character.type_match(line)
        if tmatch is not None:
            tobj = Ftype_character(line, context)
        # End if
    # End if
    if tmatch is None:
        tmatch = Ftype_type_decl.type_match(line)
        if tmatch is not None:
            tobj = Ftype_type_decl(line, context)
        # End if
    # End if
    return tobj

########################################################################
def fortran_type_definition(line):
########################################################################
    return Ftype_type_decl.type_def_line(line)

########################################################################
def parse_fortran_var_decl(line, source, logger=None):
########################################################################
    """Parse a Fortran variable declaration line and return a list of
    Var objects representing the variables declared on <line>.
    >>> _var_id_re_.match('foo') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> _var_id_re_.match("si(levr+1)") #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> _var_id_re_.match("foo()")

    >>> _var_id_re_.match('foo').group(1)
    'foo'
    >>> _var_id_re_.match('foo').group(2)

    >>> _var_id_re_.match("foo(bar)").group(1)
    'foo'
    >>> _var_id_re_.match("foo(bar)").group(2)
    '(bar)'
    >>> _var_id_re_.match("foo(bar)").group(2)
    '(bar)'
    >>> _var_id_re_.match("foo(bar, baz)").group(2)
    '(bar, baz)'
    >>> _var_id_re_.match("foo(bar : baz)").group(2)
    '(bar : baz)'
    >>> _var_id_re_.match("foo(bar:)").group(2)
    '(bar:)'
    >>> _var_id_re_.match("foo(: baz)").group(2)
    '(: baz)'
    >>> _var_id_re_.match("foo(:, :,:)").group(2)
    '(:, :,:)'
    >>> _var_id_re_.match("foo(8)").group(2)
    '(8)'
    >>> _var_id_re_.match("foo(::,a:b,a:,:b)").group(2)
    '(::,a:b,a:,:b)'
    >>> parse_fortran_var_decl("integer :: foo", ParseSource('foo.F90', 'MODULE', ParseContext()))[0].get_prop_value('local_name')
    'foo'
    >>> parse_fortran_var_decl("integer :: foo = 0", ParseSource('foo.F90', 'MODULE', ParseContext()))[0].get_prop_value('local_name')
    'foo'
    >>> parse_fortran_var_decl("integer :: foo", ParseSource('foo.F90', 'MODULE', ParseContext()))[0].get_prop_value('optional')
    False
    >>> parse_fortran_var_decl("integer, optional :: foo", ParseSource('foo.F90', 'MODULE', ParseContext()))[0].get_prop_value('optional')
    'True'
    >>> parse_fortran_var_decl("integer, dimension(:) :: foo", ParseSource('foo.F90', 'MODULE', ParseContext()))[0].get_prop_value('dimensions')
    '(:)'
    >>> parse_fortran_var_decl("integer, dimension(:) :: foo(bar)", ParseSource('foo.F90', 'MODULE', ParseContext()))[0].get_prop_value('dimensions')
    '(bar)'
    >>> parse_fortran_var_decl("integer, dimension(:) :: foo(:,:), baz", ParseSource('foo.F90', 'MODULE', ParseContext()))[0].get_prop_value('dimensions')
    '(:,:)'
    >>> parse_fortran_var_decl("integer, dimension(:) :: foo(:,:), baz", ParseSource('foo.F90', 'MODULE', ParseContext()))[1].get_prop_value('dimensions')
    '(:)'
    >>> parse_fortran_var_decl("real (kind=kind_phys), pointer :: phii  (:,:) => null()   !< interface geopotential height", ParseSource('foo.F90', 'MODULE', ParseContext()))[0].get_prop_value('dimensions')
    '(:,:)'
    >>> parse_fortran_var_decl("real(kind=kind_phys), dimension(im, levs, ntrac), intent(in) :: qgrs", ParseSource('foo.F90', 'MODULE', ParseContext()))[0].get_prop_value('dimensions')
    '(im, levs, ntrac)'
    >>> parse_fortran_var_decl("character(len=*), intent(out) :: errmsg", ParseSource('foo.F90', 'MODULE', ParseContext()))[0].get_prop_value('local_name')
    'errmsg'
    """
    context = source.context
    sline = line.strip()
    # Strip comments first
    if '!' in sline:
        sline = sline[0:sline.index('!')].rstrip()
    # End if
    tobject = Ftype_factory(sline, context)
    newvars = list()
    if tobject is not None:
        varprops = sline[tobject.type_len:].strip()
        def_dims = None # Default dimensions
        intent = None
        dimensions = None
        if '::' in varprops:
            elements = varprops.split('::')
            varlist = elements[1].strip()
            varprops = Ftype.parse_attr_specs(elements[0].strip(), context)
            for prop in varprops:
                if prop[0:6] == 'intent':
                    intent = prop[6:].strip()[1:-1].strip()
                elif prop[0:9:] == 'dimension':
                    dimensions = prop[9:].strip()
                # End if
            # End for
        else:
            # No attr_specs
            varlist = varprops
            varprops = list()
        # End if
        # Create Vars from these pieces
        # We may need to reassemble multi-dimensional specs
        vars = Ftype.reassemble_parens(varlist, 'variable_list', context)
        for var in vars:
            prop_dict = {}
            if '=' in var:
                # We do not care about initializers
                var = var[0:var.rindex('=')].rstrip()
            # End if
            match = _var_id_re_.match(var)
            if match is None:
                if logger is not None:
                    ctx = context_string(context)
                    logger.warning("WARNING: Invalid variable declaration, {}{}".format(var, ctx))
# XXgoldyXX: v debug only
# Remove this!!
                    raise ParseSyntaxError('variable declaration', token=var, context=context)
# XXgoldyXX: ^ debug only
                else:
                    raise ParseSyntaxError('variable declaration', token=var, context=context)
                # End if
            # End if
            prop_dict['local_name'] = match.group(1).strip()
            prop_dict['standard_name'] = Ftype.unique_standard_name()
            prop_dict['units'] = ''
            prop_dict['type'] = tobject.typestr
            if tobject.kind is not None:
                prop_dict['kind'] = tobject.kind
            # End if
            if 'optional' in varprops:
                prop_dict['optional'] = 'True'
            # End if
            if 'allocatable' in varprops:
                prop_dict['allocatable'] = 'True'
            # End if
            if intent is not None:
                prop_dict['intent'] = intent
            # End if
            if match.group(2) is not None:
                prop_dict['dimensions'] = match.group(2)
            elif dimensions is not None:
                prop_dict['dimensions'] = dimensions
            else:
                prop_dict['dimensions'] = '()'
            # End if
            # XXgoldyXX: I am nervous about allowing invalid Var objects here
            newvars.append(Var(prop_dict, source,
                               invalid_ok=(logger is not None), logger=logger))
        # End for
    # No else (not a variable declaration)
    # End if
    return newvars

########################################################################
# Future classes
#class Ftype_type_def(Ftype_type_decl) # Not sure about that super class
#class Fmodule_spec(object) # vars and types from a module specification part
# Fmodule_spec will contain a list of documented variables and a list of
# documented type definitions
#class Fmodule_subprog(object) # routines from a module subprogram part
#class Fmodule(object) # Info about and parsing for a Fortran module
#Fmodule will contain an Fmodule_spec and a Fmodule_subprog
########################################################################

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
