#!/usr/bin/env python

import re
if __name__ == '__main__' and __package__ is None:
    import sys
    import os.path
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from parse_tools import ParseContext, ParseSyntaxError, ParseInternalError

# A collection of types and tools for parsing Fortran code to support
# CCPP metadata parsing. The purpose of this code is limited to type
# checking of routines with CCPP metadata caps, therefore full routines are
# not parsed and a full Fortran syntax tree is not warranted.

class Ftype(object):
    """Ftype is the base class for all Fortran types
    It is also the final type for intrinsic types except for character
    >>> Ftype.is_intrinsic("integer")
    True
    >>> Ftype.is_intrinsic("inTegER")
    True
    >>> Ftype.is_intrinsic("double  precision")
    True
    >>> Ftype.is_intrinsic("doubleprecision")
    True
    >>> Ftype.is_intrinsic("type")
    False
    >>> Ftype.type_match("character")

    >>> Ftype.type_match("type")

    >>> Ftype.type_match("real") #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> Ftype.type_match("InteGer") #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> Ftype.type_match("complex(kind=r8)") #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> Ftype.type_match("double precision") #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    >>> Ftype('integer').typestr
    'integer'
    >>> Ftype('integer', kind_in='( kind= I8').print_decl() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid kind_selector, '( kind= I8', at <standard input>:1
    >>> Ftype('integer', kind_in='(kind=I8)').print_decl()
    'integer(kind=I8)'
    >>> Ftype('integer', kind_in='(I8)').print_decl()
    'integer(kind=I8)'
    >>> Ftype('real', kind_in='(len=*,R8)').print_decl() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid kind_selector, '(len=*,R8)', at <standard input>:1
    >>> Ftype(typestr_in='real', line_in='real(kind=kind_phys)') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseInternalError: typestr_in and line_in cannot both be used in a single call, at <standard input>:1
    >>> Ftype(typestr_in='real', line_in='real(kind=kind_phys)', context=ParseContext(linenum=37, filename="foo.F90")) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseInternalError: typestr_in and line_in cannot both be used in a single call, at foo.F90:37
    >>> Ftype(line_in='real(kind=kind_phys)').print_decl()
    'real(kind=kind_phys)'
    >>> Ftype(line_in="integer").print_decl()
    'integer'
    >>> Ftype(line_in="INTEGER").print_decl()
    'INTEGER'
    """

    # Note that "character" is not in intrinsic_types even though it is a
    # Fortran intrinsic. This is because character has its own type.
    intrinsic_types = [ r"integer", r"real", r"logical",
                        r"double\s*precision", r"complex" ]

    re_dp = re.compile(r"(?i)double\s*precision")
    itype_re = re.compile(r"(?i)({})\s*(\([A-Za-z0-9,=_\s]+\))?".format(r"|".join(intrinsic_types)))
    kind_re = re.compile(r"(?i)kind\s*(\()?\s*([\'\"])?(.+?)([\'\"])?\s*(\))?")

    @classmethod
    def is_intrinsic(cls, typestr):
        if typestr.strip().lower() in Ftype.intrinsic_types:
            return True
        elif typestr.lower()[0:6] == 'double':
            # Special case for double precision
            return Ftype.re_dp.match(typestr.strip()) is not None
        else:
            return False

    @classmethod
    def type_match(cls, line):
        """Return an RE match if <line> represents an Ftype declaration"""
        match = Ftype.itype_re.match(line.strip())
        if match is not None:
            return match

    def __init__(self, typestr_in=None, kind_in=None, line_in=None, context=None):
        if context is None:
            self.context = ParseContext()
        else:
            self.context = ParseContext(context=context)
        # We have to distinguish which type of initialization we have
        if typestr_in is not None:
            if line_in is not None:
                raise ParseInternalError("typestr_in and line_in cannot both be used in a single call", self.context)
            # End if
            self.typestr = typestr_in
            self.default_kind = kind_in is None
            if kind_in is None:
                self.kind = None
            elif kind_in[0] == '(':
                # Parse an explicit kind declaration
                self.kind = self.parse_kind_selector(kind_in)
            else:
                # The kind has already been parsed for us (e.g., by character)
                self.kind = kind_in
        elif kind_in is not None:
            raise ParseInternalError("kind_in cannot be passed without typestr_in", self.context)
        elif line_in is not None:
            match = Ftype.itype_re.match(line_in.strip())
            if match is None:
                raise ParseSyntaxError("type declaration", line_in, self.context)
            elif Ftype.is_intrinsic(match.group(1)):
                self.typestr = match.group(1)
                if match.group(2) is not None:
                    # Parse kind section
                    self.kind = self.parse_kind_selector(match.group(2).strip())
                else:
                    self.kind = None
                # End if
                self.default_kind = self.kind is None
            else:
                raise ParseSyntaxError("type declaration", line_in, self.context)
        else:
            raise ParseInternalError("At least one of typestr_in or line must be passed", self.context)

    def parse_kind_selector(self, kind_selector, context=None):
        if context is None:
            if hasattr(self, 'context'):
                context = self.context
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
            raise ParseSyntaxError("kind_selector", kind_selector, context)
        elif len(args) == 1:
            kind = args[0].strip()
        elif args[0].strip().lower() != 'kind':
            # We have two args, the first better be kind
            raise ParseSyntaxError("kind_selector", kind_selector, context)
        else:
            # We have two args and the second is our kind string
            kind = args[1].strip()
        # End if
        # One last check for missing right paren
        match = Ftype.kind_re.search(kind)
        if match is not None:
            if match.group(2) is not None:
                if match.group(2) != match.group(4):
                    raise ParseSyntaxError("kind_selector", kind_selector, context)
                elif (match.group(1) is None) and (match.group(5) is not None):
                    raise ParseSyntaxError("kind_selector", kind_selector, context)
                elif (match.group(1) is not None) and (match.group(5) is None):
                    raise ParseSyntaxError("kind_selector", kind_selector, context)
                else:
                    pass
            else:
                pass
        elif kind[0:4].lower() == "kind":
            raise ParseSyntaxError("kind_selector", kind_selector, context)
        else:
            pass
        return kind

    def print_decl(self):
        """Return a string of the declaration of the type"""
        if self.default_kind:
            return self.typestr
        elif Ftype.is_intrinsic(self.typestr):
            return "{}(kind={})".format(self.typestr, self.kind)
        else:
            # Derived type
            return "{}({})".format(self.typestr, self.kind)

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

    >>> Ftype_character('character', ParseContext(169, 'foo.F90')).print_decl()
    Traceback (most recent call last):
    ParseSyntaxError: Invalid character declaration, 'character', at foo.F90:169
    >>> Ftype_character('character ::', ParseContext(171, 'foo.F90')).print_decl()
    'character(len=1)'
    >>> Ftype_character('CHARACTER(len=*)', ParseContext(174, 'foo.F90')).print_decl()
    'CHARACTER(len=*)'
    >>> Ftype_character('CHARACTER(len=:)', None).print_decl()
    'CHARACTER(len=:)'
    >>> Ftype_character('character(*)', None).print_decl()
    'character(len=*)'
    >>> Ftype_character('character*7', None).print_decl() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid character declaration, 'character*7', at <standard input>:1
    >>> Ftype_character('character*7,', None).print_decl()
    'character(len=7)'
    >>> Ftype_character("character (kind=kind('a')", None).print_decl() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid kind_selector, 'kind=kind('a'', at <standard input>:1
    >>> Ftype_character("character (kind=kind('a'))", None).print_decl()
    "character(len=1, kind=kind('a'))"
    >>> Ftype_character("character  (13, kind=kind('a'))", None).print_decl()
    "character(len=13, kind=kind('a'))"
    >>> Ftype_character("character  (len=13, kind=kind('a'))", None).print_decl()
    "character(len=13, kind=kind('a'))"
    >>> Ftype_character("character  (kind=kind('b'), len=15)", None).print_decl()
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
            raise ParseSyntaxError("character declaration", line, context)
        elif len(match.groups()) == 3:
            # We have an old style character declaration
            if match.group(2) != '*':
                raise ParseSyntaxError("character declaration", line, context)
            elif Ftype_character.oldchartrail_re.match(line.strip()[len(match.group(0)):]) is None:
                raise ParseSyntaxError("character declaration", line, context)
            else:
                clen = match.group(3)
            # End if
        elif match.group(2) is not None:
            # Parse attributes (strip off parentheses
            attrs = [ x.strip() for x in match.group(2)[1:-1].split(',') ]
            if len(attrs) == 0:
                # Empty parentheses is not allowed
                raise ParseSyntaxError("char_selector", match.group(2), context)
            if len(attrs) > 2:
                # Too many attributes!
                raise ParseSyntaxError("char_selector", match.group(2), context)
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
            # We had better check the training characters
            if Ftype_character.chartrail_re.match(line.strip()[len(match.group(0)):]) is None:
                raise ParseSyntaxError("character declaration", line, context)
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
            raise ParseSyntaxError("length type-param-value", token, context)

    def parse_len_select(self, lenselect, context, len_optional=True):
        """Parse a character type length_selector"""
        largs = [ x.strip() for x in lenselect.split('=') ]
        if len(largs) > 2:
            raise ParseSyntaxError("length_selector", lenselect, context)
        elif (not len_optional) and ((len(largs) != 2) or (largs[0].lower() != 'len')):
            raise ParseSyntaxError("length_selector when len= is required", lenselect, context)
        elif len(largs) == 2:
            if largs[0].lower() != 'len':
                raise ParseSyntaxError("length_selector", lenselect, context)
            else:
                return self.parse_len_token(largs[1], context)
        elif len_optional:
            return self.parse_len_token(largs[0], context)
        else:
            raise ParseSyntaxError("length_selector when len= is required", lenselect, context)

    def print_decl(self):
        """Return a string of the declaration of the type
        For characters, we will always print an explicit len modifier
        """
        if self.default_kind:
            kind_str = ""
        else:
            kind_str = ", kind={}".format(self.kind)
        # End if
        return "{}(len={}{})".format(self.typestr, self.lenstr, kind_str)

    def to_xml(self, element, include_context=False):
        """Output a variable's character type information to an XML element"""
        super(Ftype_character, self).to_xml(element, include_context)
        sub_element = ET.SubElement(element, 'len')
        sub_element.text = self.lenstr

########################################################################

class Ftype_type_decl(Ftype):
    """Ftype_type_decl is a type that represents derived Fortran type
    declarations.
    >>> Ftype_type_decl.type_match('character')

    >>> Ftype_type_decl.type_match('type') #doctest: +ELLIPSIS
    <_sre.SRE_Match object at 0x...>
    """

    type_decl_re = re.compile(r"(?i)(type)\s*(\(\s*[A-Z][A-Z0-9_]*\s*\))?")

    @classmethod
    def type_match(cls, line):
        """Return an RE match if <line> represents an Ftype_type_decl declaration"""
        match = Ftype_type_decl.type_decl_re.match(line.strip())
        # End if
        return match

    def __init__(self, line, context):
        """Initialize an extended type from a declaration line"""
        match = Ftype_character.type_match(line)
        if match is None:
            raise ParseSyntaxError("character declaration", line, context)
        elif len(match.groups()) == 3:
            # We have an old style character declaration
            if match.group(2) != '*':
                raise ParseSyntaxError("character declaration", line, context)
            elif Ftype_character.oldchartrail_re.match(line.strip()[len(match.group(0)):]) is None:
                raise ParseSyntaxError("character declaration", line, context)
            else:
                clen = match.group(3)
            # End if
        elif match.group(2) is not None:
            # Parse attributes (strip off parentheses
            attrs = [ x.strip() for x in match.group(2)[1:-1].split(',') ]

    def to_xml(self, element, include_context=False):
        """Output a variable's extended type information to an XML element"""
        super(Ftype_type_decl, self).to_xml(element, include_context)
        sub_element = ET.SubElement(element, 'len')
        sub_element.text = self.lenstr

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

if __name__ == "__main__":
    import doctest
    doctest.testmod()
