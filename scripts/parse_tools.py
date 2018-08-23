#!/usr/bin/env python

import logging
import re
import fortran_parser

## Classes to aid the parsing process

class ParseContext(object):
    """A class for keeping track of a parsing position
    >>> ParseContext(32, "source.F90") #doctest: +ELLIPSIS
    <__main__.ParseContext object at 0x...>
    >>> ParseContext("source.F90", 32)
    Traceback (most recent call last):
    ValueError: ParseContext linenum must be an int
    >>> ParseContext(32, 90)
    Traceback (most recent call last):
    ValueError: ParseContext filenum must be an int
    >>> ParseContext(32, "source.F90").ctx_str()
    'source.F90:32'
    >>> ParseContext().ctx_str()
    '<standard input>:1'
    >>> ParseContext(32, "source.F90").increment(13)

    """

    def __init__(self, linenum=None, filename=None, context=None):
        if context is not None:
            # If context is passed, ignore linenum
            linenum = context.linenum
        elif linenum is None:
            linenum = 1
        elif type(linenum) != int:
            raise ValueError('ParseContext linenum must be an int')
        if context is not None:
            # If context is passed, ignore filename
            filename = context.filename
        elif filename is None:
            filename = "<standard input>"
        elif type(filename) != str:
            raise ValueError('ParseContext filenum must be an int')
        self.linenum = linenum
        self.filename = filename

    @property
    def line_num(self):
        'Return the current line'
        return self.linenum

    @line_num.setter
    def line_num(self, newnum):
        self.linenum = newnum

    def ctx_str(self):
        return "{}:{}".format(self.filename, self.linenum)

    def increment(self, inc=1):
        self.linenum = self.linenum + inc

########################################################################

class ParseSyntaxError(ValueError):
    """Exception that is aware of parsing context"""
    def __init__(self, token_type, token, context):
        if context is None:
            message = "Invalid {}, '{}'".format(token_type, token)
        else:
            message = "Invalid {}, '{}', at {}".format(token_type, token, context.ctx_str())
        # End if
        super(ParseSyntaxError, self).__init__(message)

class ParseInternalError(StandardError):
    """Exception for internal parser use errors"""
    def __init__(self, errmsg, context):
        if context is None:
            message = errmsg
        else:
            message = "{}, at {}".format(errmsg, context.ctx_str())
        # End if
        super(ParseInternalError, self).__init__(message)

########################################################################

## classes to check language-dependent metadata syntax
class MetadataSyntax():
    """MetadataSyntax serves as a base class for metadata syntax checking.
    It uses the most common comment type used by Python and shell languages.
    Note that this is not a complete class (missing functions).
    >>> MetadataSyntax.line_start("## Hi mom")
    True
    >>> MetadataSyntax.line_start("##Hi mom")
    False
    >>> MetadataSyntax.line_start("#> Hi mom")
    False
    >>> MetadataSyntax.table_start("#>\")
    True
    >>> MetadataSyntax.table_start("#>Hi mom")
    True
    >>> MetadataSyntax.table_start("## Hi mom")
    False
    >>> MetadataSyntax.line_start("!! Hi mom")
    False
    >>> MetadataSyntax.table_start("!> Hi mom")
    False
    >>> MetadataSyntax.blank_line("##")
    True
    >>> MetadataSyntax.blank_line("##   ")
    True
    >>> MetadataSyntax.blank_line("## Hi mom")
    False
    >>> MetadataSyntax.blank_line("!!")
    False
    >>> MetadataSyntax.strip("##   ")
    ''
    >>> MetadataSyntax.strip("##  Hi mom")
    'Hi mom'
    >>> MetadataSyntax.strip("#>\defgroup")
    '\\\\defgroup'
    >>> MetadataSyntax.strip("#  Hi mom") #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Line, '#  Hi mom', is not a valid metadata line
    >>> foo = MetadataSyntax() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    NotImplementedError: Cannot create MetadataSyntax instances
    >>> MetadataSyntax.is_variable_name('hi_mom') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    NotImplementedError: MetadataSyntax.is_variable_name is not implemented
    """
    _blank_line = re.compile(r"^##\s*$")
    _table_start = re.compile(r"^(#[#>]\s+)")
    _line_start  = re.compile(r"^(##\s+)")

    @classmethod
    def line_start(cls, line):
        """Return True iff line begins with the proper metadata syntax"""
        match = cls._line_start.match(line)
        if match is not None:
            return match.group(1)
        else:
            return None

    @classmethod
    def table_start(cls, line):
        """Return True iff line begins with the proper metadata table syntax"""
        match = cls._table_start.match(line)
        if match is not None:
            return match.group(1)
        else:
            return None

    @classmethod
    def blank_line(cls, line):
        """Return True iff line is the proper metadata blank format"""
        return (len(line) == 0) or (cls._blank_line.match(line) is not None)

    @classmethod
    def strip(cls, line):
        """Strip line-beginning syntax from line"""
        sline = None
        ls = cls.line_start(line)
        if ls is not None:
            sline = line[len(ls):].strip()
        # End if
        if sline is None:
            ts = cls.table_start(line)
            if ts is not None:
                sline = line[len(ts):].strip()
            # End if
        # End if
        return sline

    @classmethod
    def __init__(cls):
        raise NotImplementedError("Cannot create {} instances".format(cls.__name__))

    @classmethod
    def is_variable_name(cls, name):
        raise NotImplementedError('{}.is_variable_name is not implemented'.format(cls.__name__))

########################################################################

class FortranMetadataSyntax(MetadataSyntax):
    """FortranMetadataSyntax is a class for Fortran metadata syntax checking.
    >>> FortranMetadataSyntax.line_start("!! Hi mom")
    True
    >>> FortranMetadataSyntax.line_start("!!Hi mom")
    False
    >>> FortranMetadataSyntax.line_start("!> Hi mom")
    False
    >>> FortranMetadataSyntax.table_start("!>Hi mom")
    True
    >>> FortranMetadataSyntax.table_start("#>Hi mom")
    False
    >>> FortranMetadataSyntax.table_start("!! Hi mom")
    False
    >>> FortranMetadataSyntax.line_start("## Hi mom")
    False
    >>> FortranMetadataSyntax.table_start("#>Hi mom")
    False
    >>> FortranMetadataSyntax.blank_line("!!")
    True
    >>> FortranMetadataSyntax.blank_line("!!   ")
    True
    >>> FortranMetadataSyntax.blank_line("!! Hi mom")
    False
    >>> FortranMetadataSyntax.blank_line("##")
    False
    >>> FortranMetadataSyntax.strip("!!   ")
    ''
    >>> FortranMetadataSyntax.strip("!!  Hi mom")
    'Hi mom'
    >>> FortranMetadataSyntax.strip("!>\defgroup")
    '\\\\defgroup'
    >>> FortranMetadataSyntax.strip("!  Hi mom") #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Line, '!  Hi mom', is not a valid metadata line
    >>> foo = FortranMetadataSyntax() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    NotImplementedError: Cannot create FortranMetadataSyntax instances
    >>> FortranMetadataSyntax.is_variable_name('V_123')
    True
    >>> FortranMetadataSyntax.is_variable_name('v_v_a2')
    True
    >>> FortranMetadataSyntax.is_variable_name('i')
    True
    >>> FortranMetadataSyntax.is_variable_name('')
    False
    >>> FortranMetadataSyntax.is_variable_name('_hi_mom')
    False
    >>> FortranMetadataSyntax.is_variable_name('2i')
    False
    """

    _blank_line = re.compile(r"^!!\s*$")
    _table_start = re.compile(r"(![!>]\s+)")
    _line_start  = re.compile(r"(!!\s+)")

    @classmethod
    def is_variable_name(cls, name):
        return fortran_parser.is_variable_name(name)

########################################################################

class PythonMetadataSyntax(MetadataSyntax):
    """PythonMetadataSyntax is a class for Python metadata syntax checking.
    >>> PythonMetadataSyntax.line_start("!! Hi mom")
    True
    >>> PythonMetadataSyntax.line_start("!!Hi mom")
    False
    >>> PythonMetadataSyntax.line_start("!> Hi mom")
    False
    >>> PythonMetadataSyntax.table_start("!>Hi mom")
    True
    >>> PythonMetadataSyntax.table_start("#>Hi mom")
    False
    >>> PythonMetadataSyntax.table_start("!! Hi mom")
    False
    >>> PythonMetadataSyntax.line_start("## Hi mom")
    False
    >>> PythonMetadataSyntax.table_start("#>Hi mom")
    False
    >>> PythonMetadataSyntax.blank_line("!!")
    True
    >>> PythonMetadataSyntax.blank_line("!!   ")
    True
    >>> PythonMetadataSyntax.blank_line("!! Hi mom")
    False
    >>> PythonMetadataSyntax.blank_line("##")
    False
    >>> PythonMetadataSyntax.strip("!!   ")
    ''
    >>> PythonMetadataSyntax.strip("!!  Hi mom")
    'Hi mom'
    >>> PythonMetadataSyntax.strip("!>\defgroup")
    '\\\\defgroup'
    >>> PythonMetadataSyntax.strip("!  Hi mom") #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Line, '!  Hi mom', is not a valid metadata line
    >>> foo = PythonMetadataSyntax() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    NotImplementedError: Cannot create PythonMetadataSyntax instances
    >>> PythonMetadataSyntax.is_variable_name('V_123')
    True
    >>> PythonMetadataSyntax.is_variable_name('v_v_a2')
    True
    >>> PythonMetadataSyntax.is_variable_name('i')
    True
    >>> PythonMetadataSyntax.is_variable_name('')
    False
    >>> PythonMetadataSyntax.is_variable_name('_hi_mom')
    True
    >>> PythonMetadataSyntax.is_variable_name('2i')
    False
    """

    _blank_line = re.compile(r"^!!\s*$")
    _variable_name = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")
    _table_start = re.compile(r"(#[#>]\s+)")
    _line_start  = re.compile(r"(##\s+)")

    @classmethod
    def is_variable_name(cls, name):
        # In python 3, we would just use isidentifier
        return PythonMetadataSyntax._variable_name.match(name) is not None

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
