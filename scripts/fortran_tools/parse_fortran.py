#!/usr/bin/env python3

"""Types and code for parsing Fortran source code.
"""

# pylint: disable=wrong-import-position
if __name__ == '__main__' and __package__ is None:
    import sys
    import os.path
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import re
from parse_tools import ParseSyntaxError, ParseInternalError
from parse_tools import ParseContext, context_string
from parse_tools import check_fortran_intrinsic
from parse_tools import check_balanced_paren, unique_standard_name
#pylint: disable=unused-import
from parse_tools import ParseSource # Used in doctest
#pylint: enable=unused-import
from metavar import FortranVar
# pylint: enable=wrong-import-position

# A collection of types and tools for parsing Fortran code to support
# CCPP metadata parsing. The purpose of this code is limited to type
# checking of routines with CCPP metadata caps, therefore full routines are
# not parsed and a full Fortran syntax tree is not warranted.

########################################################################

# Fortran ID specifier (do not want a group like FORTRAN_ID from parse_tools)
_FORTRAN_ID = r"(?:[A-Za-z][A-Za-z0-9_]*)"
# Regular expression for a dimension specifier
_DIMID = r"(?:"+_FORTRAN_ID+r"|[0-9]+)"
_DIMCOLON = r"(?:\s*:\s*"+_DIMID+r"?\s*)"
_DIMCOLONS = r"(?:"+_DIMID+r"?"+_DIMCOLON+_DIMCOLON+r"?)"
_DIMSPEC = r"(?:"+_DIMID+r"|"+_DIMCOLONS+r")"
_dims_list_ = _DIMSPEC+r"(?:\s*,\s*"+_DIMSPEC+r"){0,6}"
# Regular expression for a variable name with optional dimensions
_VAR_ID_RE = re.compile(r"("+_FORTRAN_ID+r")\s*(\(\s*"+_dims_list_+r"\s*\))?$")

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
    __intrinsic_types__ = [r"integer", r"real", r"logical",
                           r"double\s*precision", r"complex"]

    __itype_re = re.compile(r"(?i)({})\s*(\([A-Za-z0-9,=_\s]+\))?".format(r"|".join(__intrinsic_types__)))
    __kind_re = re.compile(r"(?i)kind\s*(\()?\s*([\'\"])?(.+?)([\'\"])?\s*(\))?")

    __attr_spec = ['allocatable', 'asynchronous', 'dimension', 'external',
                   'intent', 'intrinsic', 'bind', 'optional', 'parameter',
                   'pointer', 'private', 'protected', 'public', 'save',
                   'target', 'value', 'volatile']

    def __init__(self, typestr_in=None, kind_in=None, match_len_in=None,
                 line_in=None, context=None):
        """Initialize  this FType object, either using <typestr_in> and
        <kind_in>, OR using line_in."""
        if context is None:
            self.__context = ParseContext()
        else:
            self.__context = ParseContext(context=context)
        # end if
        # We have to distinguish which type of initialization we have
        self.__typestr = typestr_in
        if typestr_in is not None:
            if line_in is not None:
                emsg = "Cannot pass both typestr_in and line_in as arguments"
                raise ParseInternalError(emsg, self.__context)
            # end if
            self.__default_kind = kind_in is None
            if kind_in is None:
                self.__kind = None
            elif kind_in[0] == '(':
                # Parse an explicit kind declaration
                self.__kind = self.parse_kind_selector(kind_in)
            else:
                # The kind has already been parsed for us (e.g., by character)
                self.__kind = kind_in
            # end if
            if match_len_in is not None:
                self.__match_len = match_len_in
            else:
                self.__match_len = len(self.typestr)
                if kind_in is not None:
                    self.__match_len += len(self.__kind) + 2
                # end if
            # end if
        elif kind_in is not None:
            emsg = "kind_in cannot be passed without typestr_in"
            raise ParseInternalError(emsg, self.__context)
        elif line_in is not None:
            match = Ftype.type_match(line_in)
            if match is None:
                emsg = "type declaration"
                raise ParseSyntaxError(emsg, token=line_in,
                                       context=self.__context)
            # end if
            if match_len_in is not None:
                self.__match_len = match_len_in
            else:
                self.__match_len = len(match.group(0))
            # end if
            if check_fortran_intrinsic(match.group(1)):
                self.__typestr = match.group(1)
                if match.group(2) is not None:
                    # Parse kind section
                    kmatch = match.group(2).strip()
                    self.__kind = self.parse_kind_selector(kmatch)
                else:
                    self.__kind = None
                # end if
                self.__default_kind = self.__kind is None
            else:
                raise ParseSyntaxError("type declaration",
                                       token=line_in, context=self.__context)
            # end if
        else:
            emsg = "At least one of typestr_in or line_in must be passed"
            raise ParseInternalError(emsg, self.__context)
        # end if

    def parse_kind_selector(self, kind_selector, context=None):
        """Find and return the 'kind' value from <kind_selector>
        '(foo)' and '(kind=foo)' both return 'foo'"""
        if context is None:
            if hasattr(self, 'context'):
                context = self.__context
            else:
                context = ParseContext()
            # end if
        kind = None
        if (kind_selector[0] == '(') and (kind_selector[-1] == ')'):
            args = kind_selector[1:-1].split('=')
        else:
            args = kind_selector.split('=')
        # end if
        if (len(args) > 2) or (len(args) < 1):
            raise ParseSyntaxError("kind_selector",
                                   token=kind_selector, context=context)
        # end if
        if len(args) == 1:
            kind = args[0].strip()
        elif args[0].strip().lower() != 'kind':
            # We have two args, the first better be kind
            raise ParseSyntaxError("kind_selector",
                                   token=kind_selector, context=context)
        # end if
        if kind is None:
            # We have two args and the second is our kind string
            kind = args[1].strip()
        # end if
        # One last check for missing right paren
        match = Ftype.__kind_re.search(kind)
        if match is not None:
            if match.group(2) is not None:
                if match.group(2) != match.group(4):
                    raise ParseSyntaxError("kind_selector",
                                           token=kind_selector, context=context)
                # end if
                if (match.group(1) is None) and (match.group(5) is not None):
                    raise ParseSyntaxError("kind_selector",
                                           token=kind_selector, context=context)
                # end if
                if (match.group(1) is not None) and (match.group(5) is None):
                    raise ParseSyntaxError("kind_selector",
                                           token=kind_selector, context=context)
                # end if
            else:
                pass
        elif kind[0:4].lower() == "kind":
            # Got 'something' == 'kind'??
            raise ParseSyntaxError("kind_selector",
                                   token=kind_selector, context=context)
        # end if
        return kind

    @classmethod
    def type_match(cls, line):
        """Return an RE match if <line> represents an Ftype declaration"""
        match = Ftype.__itype_re.match(line.strip())
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
        var_list = list()
        proplist = propstr.split(splitstr)
        while len(proplist) > 0:
            var = proplist.pop(0)
            while var.count('(') != var.count(')'):
                if len(proplist) == 0:
                    raise ParseSyntaxError(errstr, token=propstr, context=context)
                # end if
                var = var + ',' + proplist.pop(0)
            # end while
            var = var.strip()
            if len(var) > 0:
                var_list.append(var)
            # end if
        # end while
        return var_list

    @classmethod
    def parse_attr_specs(cls, propstring, context):
        """Return a list of variable properties"""
        properties = list()
        # Remove leading comma
        propstring = propstring.strip()
        if propstring and (propstring[0] == ','):
            propstring = propstring[1:].lstrip()
        # end if
        proplist = cls.reassemble_parens(propstring, 'attr_spec', context)
        for prop in proplist:
            prop = prop.strip().lower()
            if '(' in prop:
                # Strip out value from dimensions, bind, or intent
                pval = prop[0:prop.index('(')].strip()
            else:
                pval = prop
            # end if
            if pval not in cls.__attr_spec:
                raise ParseSyntaxError('attr_spec', token=prop, context=context)
            # end if
            properties.append(prop)
        # end for
        return properties

    @property
    def typestr(self):
        """ Return this FType object's type string"""
        return self.__typestr

    @property
    def default_kind(self):
        """Return True iff this FType object is of default kind."""
        return self.__default_kind

    def kind(self):
        """ Return this FType's kind string"""
        return self.__kind

    @property
    def type_len(self):
        """ Return the length of this FType's kind string"""
        return self.__match_len

    def __str__(self):
        """Return a string of the declaration of the type"""
        if self.default_kind:
            return self.typestr
        # end if
        if check_fortran_intrinsic(self.typestr):
            return "{}(kind={})".format(self.typestr, self.__kind)
        # end if
        # Derived type
        return "{}({})".format(self.typestr, self.__kind)

########################################################################

class FtypeCharacter(Ftype):
    """FtypeCharacter is a type that represents character types
    >>> FtypeCharacter.type_match('character') #doctest: +ELLIPSIS
    <re.Match object; span=(0, 9), match='character'>
    >>> FtypeCharacter.type_match('CHARACTER') #doctest: +ELLIPSIS
    <re.Match object; span=(0, 9), match='CHARACTER'>
    >>> FtypeCharacter.type_match('chaRActer (len=*)') #doctest: +ELLIPSIS
    <re.Match object; span=(0, 17), match='chaRActer (len=*)'>
    >>> FtypeCharacter.type_match('integer')

    >>> FtypeCharacter('character', ParseContext(169, 'foo.F90')).__str__()
    Traceback (most recent call last):
    parse_source.ParseSyntaxError: Invalid character declaration, 'character', at foo.F90:170
    >>> FtypeCharacter('character ::', ParseContext(171, 'foo.F90')).__str__()
    'character(len=1)'
    >>> FtypeCharacter('CHARACTER(len=*)', ParseContext(174, 'foo.F90')).__str__()
    'CHARACTER(len=*)'
    >>> FtypeCharacter('CHARACTER(len=:)', None).__str__()
    'CHARACTER(len=:)'
    >>> FtypeCharacter('Character(len=512)', None).__str__()
    'Character(len=512)'
    >>> FtypeCharacter('character(*)', None).__str__()
    'character(len=*)'
    >>> FtypeCharacter('character*7', None).__str__() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid character declaration, 'character*7', at <standard input>:1
    >>> FtypeCharacter('character*7,', None).__str__()
    'character(len=7)'
    >>> FtypeCharacter("character (kind=kind('a')", None).__str__() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid kind_selector, 'kind=kind('a'', at <standard input>:1
    >>> FtypeCharacter("character (kind=kind('a'))", None).__str__()
    "character(len=1, kind=kind('a'))"
    >>> FtypeCharacter("character  (13, kind=kind('a'))", None).__str__()
    "character(len=13, kind=kind('a'))"
    >>> FtypeCharacter("character  (len=13, kind=kind('a'))", None).__str__()
    "character(len=13, kind=kind('a'))"
    >>> FtypeCharacter("character  (kind=kind('b'), len=15)", None).__str__()
    "character(len=15, kind=kind('b'))"
    """

    char_re = re.compile(r"(?i)(character)\s*(\([A-Za-z0-9,=*:\s\'\"()]+\))?")
    chartrail_re = re.compile(r"\s*[,:]|\s+[A-Z]")
    oldchar_re = re.compile(r"(?i)(character)\s*(\*)\s*([0-9]+\s*)")
    oldchartrail_re = re.compile(r"\s*[,]|\s+[A-Z]")
    len_token_re = re.compile(r"(?i)([:]|[*]|[0-9]+|[A-Z][A-Z0-9_]*)$")

    @classmethod
    def type_match(cls, line):
        """Return an RE match if <line> represents an FtypeCharacter
        declaration"""
        # Try old style first to eliminate as a possibility
        match = FtypeCharacter.oldchar_re.match(line.strip())
        if match is None:
            match = FtypeCharacter.char_re.match(line.strip())
        # end if
        return match

    def __init__(self, line, context):
        """Initialize a character type from a declaration line"""

        clen = None
        kind = None # This will be interpreted as default kind
        match = FtypeCharacter.type_match(line)
        if match is None:
            raise ParseSyntaxError("character declaration", token=line, context=context)
        # end if
        match_len = len(match.group(0))
        if len(match.groups()) == 3:
            # We have an old style character declaration
            if match.group(2) != '*':
                raise ParseSyntaxError("character declaration", token=line, context=context)
            # end if
            if FtypeCharacter.oldchartrail_re.match(line.strip()[len(match.group(0)):]) is None:
                raise ParseSyntaxError("character declaration",
                                       token=line, context=context)
            # end if
            clen = match.group(3)
        elif match.group(2) is not None:
            # Parse attributes (strip off parentheses)
            attrs = [x.strip() for x in match.group(2)[1:-1].split(',')]
            if not attrs:
                # Empty parentheses is not allowed
                raise ParseSyntaxError("char_selector",
                                       token=match.group(2), context=context)
            # end if
            if len(attrs) > 2:
                # Too many attributes!
                raise ParseSyntaxError("char_selector",
                                       token=match.group(2), context=context)
            # end if
            if attrs[0][0:4].lower() == "kind":
                # The first arg is kind, try to parse it
                kind = self.parse_kind_selector(attrs[0], context=context)
                # If there is a second arg, it must be of form len=<length_selector>
                if len(attrs) == 2:
                    clen = self.parse_len_select(attrs[1],
                                                 context, len_optional=False)
            elif len(attrs) == 2:
                # We have both a len and a kind, len first
                clen = self.parse_len_select(attrs[0],
                                             context, len_optional=True)
                kind = self.parse_kind_selector(attrs[1], context)
            else:
                # We just a len argument
                clen = self.parse_len_select(attrs[0],
                                             context, len_optional=True)
            # end if
        else:
            # We had better check the training characters
            if FtypeCharacter.chartrail_re.match(line.strip()[len(match.group(0)):]) is None:
                raise ParseSyntaxError("character declaration",
                                       token=line, context=context)
            # end if
        # end if
        if clen is None:
            clen = 1
        # end if
        self.lenstr = "{}".format(clen)
        super(FtypeCharacter, self).__init__(typestr_in=match.group(1),
                                             kind_in=kind,
                                             match_len_in=match_len,
                                             context=context)

    def parse_len_token(self, token, context):
        """Check to make sure token is a valid length identifier"""
        match = FtypeCharacter.len_token_re.match(token)
        if match is not None:
            return match.group(1)
        # end if
        raise ParseSyntaxError("length type-param-value",
                               token=token, context=context)
        # end if

    def parse_len_select(self, lenselect, context, len_optional=True):
        """Parse a character type length_selector"""
        largs = [x.strip() for x in lenselect.split('=')]
        if len(largs) > 2:
            raise ParseSyntaxError("length_selector", token=lenselect, context=context)
        # end if
        if (not len_optional) and ((len(largs) != 2) or (largs[0].lower() != 'len')):
            raise ParseSyntaxError("length_selector when len= is required", token=lenselect, context=context)
        # end if
        if len(largs) == 2:
            if largs[0].lower() != 'len':
                raise ParseSyntaxError("length_selector", token=lenselect, context=context)
            # end if
            return self.parse_len_token(largs[1], context)
        elif len_optional:
            return self.parse_len_token(largs[0], context)
        else:
            raise ParseSyntaxError("length_selector when len= is required", token=lenselect, context=context)
        # end if

    def kind(self):
        """Return a kind metadata declaration if this Ftype object is of
        a non-default kind.
        Otherwise, return an empty string."""
        if self.default_kind:
            kind_str = ""
        else:
            kind_str = ", kind={}".format(super(FtypeCharacter, self).kind())
        # end if
        return "len={}{}".format(self.lenstr, kind_str)

    def __str__(self):
        """Return a string of the declaration of the type
        For characters, we will always print an explicit len modifier
        """
        return "{}({})".format(self.typestr, self.kind())

########################################################################

class FtypeTypeDecl(Ftype):
    """FtypeTypeDecl is a type that represents derived Fortran type
    declarations.
    >>> FtypeTypeDecl.type_match('character')

    >>> FtypeTypeDecl.type_match('type(foo)') #doctest: +ELLIPSIS
    <re.Match object; span=(0, 9), match='type(foo)'>
    >>> FtypeTypeDecl.type_match('class(foo)') #doctest: +ELLIPSIS

    >>> FtypeTypeDecl.class_match('class(foo)') #doctest: +ELLIPSIS
    <re.Match object; span=(0, 10), match='class(foo)'>
    >>> FtypeTypeDecl.type_def_line('type GFS_statein_type')
    ['GFS_statein_type', None, None]
    >>> FtypeTypeDecl.type_def_line('type GFS_statein_type (n, m) ')
    ['GFS_statein_type', None, '(n, m)']
    >>> FtypeTypeDecl.type_def_line('type, public, extends(foo) :: GFS_statein_type')
    ['GFS_statein_type', ['public', 'extends(foo)'], None]
    >>> FtypeTypeDecl.type_def_line('type(foo) :: bar')

    >>> FtypeTypeDecl.type_def_line('type foo ! This is a comment')
    ['foo', None, None]
    """

    __type_decl_re__ = re.compile(r"(?i)(type)\s*\(\s*([A-Z][A-Z0-9_]*)\s*\)?")

    __type_attr_spec__ = ['abstract', 'bind', 'extends', 'private', 'public']

    __class_decl_re__ = re.compile(r"(?i)(class)\s*\(\s*([A-Z][A-Z0-9_]*)\s*\)")

    def __init__(self, line, context):
        """Initialize an extended type from a declaration line"""
        match = FtypeTypeDecl.type_match(line)
        if match is None:
            match = FtypeTypeDecl.class_match(line)
        # end if
        if match is None:
            raise ParseSyntaxError("type declaration",
                                   token=line, context=context)
        # end if
        super(FtypeTypeDecl, self).__init__(typestr_in=match.group(2),
                                            kind_in=match.group(2),
                                            match_len_in=len(match.group(0)),
                                            context=context)
        self.__class = match.group(1)

    @classmethod
    def type_match(cls, line):
        """Return an RE match if <line> represents an FtypeTypeDecl declaration
        """
        match = FtypeTypeDecl.__type_decl_re__.match(line.strip())
        # end if
        return match

    @classmethod
    def class_match(cls, line):
        """Return an RE match if <line> represents an FtypeTypeDecl declaration
        representing the declaration of a polymorphic variable
        """
        match = FtypeTypeDecl.__class_decl_re__.match(line.strip())
        # end if
        return match

    @classmethod
    def type_def_line(cls, line):
        """Return a type information if <line> represents the start
        of a type definition"""
        type_def = None
        if not cls.type_match(line):
            if '!' in line:
                sline = line[0:line.index('!')].strip()
            else:
                sline = line.strip()
            # end if
            if sline.lower()[0:4] == 'type':
                if '::' in sline:
                    elements = sline.split('::')
                    type_name = elements[1].strip()
                    type_props = [x.strip() for x in elements[0].split(',')[1:]]
                else:
                    # Plain type decl
                    type_name = sline.split(' ', 1)[1].strip()
                    type_props = None
                # end if
                if '(' in type_name:
                    tnstr = type_name.split('(')
                    type_name = tnstr[0].strip()
                    type_params = '(' + tnstr[1].rstrip()
                else:
                    type_params = None
                # end if
                type_def = [type_name, type_props, type_params]
            # end if
        # end if
        return type_def

    def __str__(self):
        """Return a printable string for this Ftype object"""
        return '{}({})'.format(self.__class, self.typestr)

########################################################################
def ftype_factory(line, context):
########################################################################
    "Return an appropriate type object if there is a match, otherwise None"
    # We have to cut off the line at the end of any possible type info
    # Strip comments first (might have an = character)
    if '!' in line:
        line = line[0:line.index('!')].rstrip()
    # end if
    ppos = line.find('(')
    cpos = line.find(',')
    if ppos >= 0:
        if 0 <= cpos < ppos:
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
                # end if
                pepos = pepos + 1
            # end while
            line = line[0:pepos+1]
        # end if
    elif cpos >= 0:
        line = line[0:cpos]
    # end if
    tmatch = Ftype.type_match(line)
    if tmatch is None:
        tobj = None
    else:
        tobj = Ftype(line_in=line, context=context)
    # end if
    if tmatch is None:
        tmatch = FtypeCharacter.type_match(line)
        if tmatch is not None:
            tobj = FtypeCharacter(line, context)
        # end if
    # end if
    if tmatch is None:
        tmatch = FtypeTypeDecl.type_match(line)
        if tmatch is not None:
            tobj = FtypeTypeDecl(line, context)
        # end if
    # end if
    if tmatch is None:
        tmatch = FtypeTypeDecl.class_match(line)
        if tmatch is not None:
            tobj = FtypeTypeDecl(line, context)
        # end if
    # end if
    return tobj

########################################################################
def fortran_type_definition(line):
########################################################################
    """Return a type information if <line> represents the start
        of a type definition"""
    return FtypeTypeDecl.type_def_line(line)

########################################################################
def parse_fortran_var_decl(line, source, run_env):
########################################################################
    """Parse a Fortran variable declaration line and return a list of
    Var objects representing the variables declared on <line>.
    >>> _VAR_ID_RE.match('foo') #doctest: +ELLIPSIS
    <re.Match object; span=(0, 3), match='foo'>
    >>> _VAR_ID_RE.match("foo()")

    >>> _VAR_ID_RE.match('foo').group(1)
    'foo'
    >>> _VAR_ID_RE.match('foo').group(2)

    >>> _VAR_ID_RE.match("foo(bar)").group(1)
    'foo'
    >>> _VAR_ID_RE.match("foo(bar)").group(2)
    '(bar)'
    >>> _VAR_ID_RE.match("foo(bar)").group(2)
    '(bar)'
    >>> _VAR_ID_RE.match("foo(bar, baz)").group(2)
    '(bar, baz)'
    >>> _VAR_ID_RE.match("foo(bar : baz)").group(2)
    '(bar : baz)'
    >>> _VAR_ID_RE.match("foo(bar:)").group(2)
    '(bar:)'
    >>> _VAR_ID_RE.match("foo(: baz)").group(2)
    '(: baz)'
    >>> _VAR_ID_RE.match("foo(:, :,:)").group(2)
    '(:, :,:)'
    >>> _VAR_ID_RE.match("foo(8)").group(2)
    '(8)'
    >>> _VAR_ID_RE.match("foo(::,a:b,a:,:b)").group(2)
    '(::,a:b,a:,:b)'
    >>> parse_fortran_var_decl("integer :: foo", ParseSource('foo.F90', 'module', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('local_name')
    'foo'
    >>> parse_fortran_var_decl("integer :: foo = 0", ParseSource('foo.F90', 'module', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('local_name')
    'foo'
    >>> parse_fortran_var_decl("integer :: foo", ParseSource('foo.F90', 'module', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('optional')

    >>> parse_fortran_var_decl("integer, optional :: foo", ParseSource('foo.F90', 'module', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('optional')
    'True'
    >>> parse_fortran_var_decl("integer, dimension(:) :: foo", ParseSource('foo.F90', 'module', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('dimensions')
    '(:)'
    >>> parse_fortran_var_decl("integer, dimension(:) :: foo(bar)", ParseSource('foo.F90', 'module', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('dimensions')
    '(bar)'
    >>> parse_fortran_var_decl("integer, dimension(:) :: foo(:,:), baz", ParseSource('foo.F90', 'module', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('dimensions')
    '(:,:)'
    >>> parse_fortran_var_decl("integer, dimension(:) :: foo(:,:), baz", ParseSource('foo.F90', 'module', ParseContext()), _DUMMY_RUN_ENV)[1].get_prop_value('dimensions')
    '(:)'
    >>> parse_fortran_var_decl("real (kind=kind_phys), pointer :: phii  (:,:) => null()   !< interface geopotential height", ParseSource('foo.F90', 'module', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('dimensions')
    '(:,:)'
    >>> parse_fortran_var_decl("real(kind=kind_phys), dimension(im, levs, ntrac), intent(in) :: qgrs", ParseSource('foo.F90', 'scheme', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('dimensions')
    '(im, levs, ntrac)'
    >>> parse_fortran_var_decl("character(len=*), intent(out) :: errmsg", ParseSource('foo.F90', 'scheme', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('local_name')
    'errmsg'
    >>> parse_fortran_var_decl("character(len=512), intent(out) :: errmsg", ParseSource('foo.F90', 'scheme', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('kind')
    'len=512'
    >>> parse_fortran_var_decl("real(kind_phys), intent(out) :: foo(8)", ParseSource('foo.F90', 'scheme', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('dimensions')
    '(8)'
    >>> parse_fortran_var_decl("real(kind_phys), intent(out) :: foo(8)", ParseSource('foo.F90', 'scheme', ParseContext()), _DUMMY_RUN_ENV)[0].get_dimensions()
    ['8']
    >>> parse_fortran_var_decl("character(len=*), intent(out) :: errmsg", ParseSource('foo.F90', 'module', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('local_name') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid variable declaration, character(len=*), intent(out) :: errmsg, intent not allowed in module variable, in <standard input>

    ## NB: Expressions (including function calls) not currently supported here
    #>>> parse_fortran_var_decl("real(kind_phys), intent(out) :: foo(size(bar))", ParseSource('foo.F90', 'scheme', ParseContext()), _DUMMY_RUN_ENV)[0].get_prop_value('dimensions')
    #'(size(bar))'
    """
    context = source.context
    sline = line.strip()
    # Strip comments first
    if '!' in sline:
        sline = sline[0:sline.index('!')].rstrip()
    # end if
    tobject = ftype_factory(sline, context)
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
                    if source.type != 'scheme':
                        typ = source.type
                        errmsg = 'Invalid variable declaration, {}, intent'
                        errmsg = errmsg + ' not allowed in {} variable'
                        if run_env.logger is not None:
                            ctx = context_string(context)
                            errmsg = "WARNING: " + errmsg + "{}"
                            run_env.logger.warning(errmsg.format(sline,
                                                                 typ, ctx))
                        else:
                            raise ParseSyntaxError(errmsg.format(sline, typ),
                                                   context=context)
                        # end if
                    else:
                        intent = prop[6:].strip()[1:-1].strip()
                    # end if
                elif prop[0:9:] == 'dimension':
                    dimensions = prop[9:].strip()
                # end if
            # end for
        else:
            # No attr_specs
            varlist = varprops
            varprops = list()
        # end if
        # Create Vars from these pieces
        # We may need to reassemble multi-dimensional specs
        var_list = Ftype.reassemble_parens(varlist, 'variable_list', context)
        for var in var_list:
            prop_dict = {}
            if '=' in var:
                # We do not care about initializers
                var = var[0:var.rindex('=')].rstrip()
            # end if
            # Scan <var> and gather variable pieces
            inchar = None # Character context
            var_len = len(var)
            ploc = var.find('(')
            if ploc < 0:
                varname = var.strip()
                dimspec = None
            else:
                varname = var[0:ploc].strip()
                begin, end = check_balanced_paren(var)
                if (begin < 0) or (end < 0):
                    if run_env.logger is not None:
                        ctx = context_string(context)
                        errmsg = "WARNING: Invalid variable declaration, {}{}"
                        run_env.logger.warning(errmsg.format(var, ctx))
                    else:
                        raise ParseSyntaxError('variable declaration',
                                               token=var, context=context)
                    # end if
                else:
                    dimspec = var[begin:end+1]
                # end if
            # end if
            prop_dict['local_name'] = varname
            prop_dict['standard_name'] = unique_standard_name()
            prop_dict['units'] = ''
            if isinstance(tobject, FtypeTypeDecl):
                prop_dict['ddt_type'] = tobject.typestr
            else:
                prop_dict['type'] = tobject.typestr
            # end if
            if tobject.kind() is not None:
                prop_dict['kind'] = tobject.kind()
            # end if
            if 'optional' in varprops:
                prop_dict['optional'] = 'True'
            # end if
            if 'allocatable' in varprops:
                prop_dict['allocatable'] = 'True'
            # end if
            if intent is not None:
                prop_dict['intent'] = intent
            # end if
            if dimspec is not None:
                prop_dict['dimensions'] = dimspec
            elif dimensions is not None:
                prop_dict['dimensions'] = dimensions
            else:
                prop_dict['dimensions'] = '()'
            # end if
            # XXgoldyXX: I am nervous about allowing invalid Var objects here
            # Also, this tends to cause an exception that ends up back here
            # which is not a good idea.
            var = FortranVar(prop_dict, source, run_env)
            newvars.append(var)
        # end for
    # No else (not a variable declaration)
    # end if
    return newvars

########################################################################
# Future classes
#class Ftype_type_def(FtypeTypeDecl) # Not sure about that super class
#class Fmodule_spec(object) # vars and types from a module specification part
# Fmodule_spec will contain a list of documented variables and a list of
# documented type definitions
#class Fmodule_subprog(object) # routines from a module subprogram part
#class Fmodule(object) # Info about and parsing for a Fortran module
#Fmodule will contain an Fmodule_spec and a Fmodule_subprog
########################################################################

########################################################################

if __name__ == "__main__":
    # pylint: disable=ungrouped-imports
    import doctest
    # pylint: enable=ungrouped-imports
    from framework_env import CCPPFrameworkEnv
    _DUMMY_RUN_ENV = CCPPFrameworkEnv(None, ndict={'host_files':'',
                                                   'scheme_files':'',
                                                   'suites':''})
    fail, _ = doctest.testmod()
    sys.exit(fail)
# end if
