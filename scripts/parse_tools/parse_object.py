#!/usr/bin/env python
"""A module for the base, ParseObject class"""

import logging
import re
from parse_tools import ParseContext

logger = logging.getLogger(__name__)

########################################################################

## classes to check language-dependent metadata syntax
class MetadataSyntax():
    """MetadataSyntax serves as a base class for metadata syntax checking.
    It uses the most common comment type used by Python and shell languages.
    Note that this is not a complete class (missing functions).
    >>> MetadataSyntax.line_start("## Hi mom")
    '## '
    >>> MetadataSyntax.line_start("##Hi mom")

    >>> MetadataSyntax.line_start("#> Hi mom")

    >>> MetadataSyntax.table_start("#> ")
    '#> '
    >>> MetadataSyntax.table_start("#>Hi mom")

    >>> MetadataSyntax.table_start("#> Hi mom")
    '#> '
    >>> MetadataSyntax.table_start("## Hi mom")
    '## '
    >>> MetadataSyntax.table_start("##Hi mom")

    >>> MetadataSyntax.line_start("!! Hi mom")

    >>> MetadataSyntax.table_start("!> Hi mom")

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

    >>> MetadataSyntax.strip("#  Hi mom")

    >>> foo = MetadataSyntax() #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    NotImplementedError: Cannot create MetadataSyntax instances
    >>> MetadataSyntax.is_variable_name('V_123')
    True
    >>> MetadataSyntax.is_variable_name('v_v_a2')
    True
    >>> MetadataSyntax.is_variable_name('i')
    True
    >>> MetadataSyntax.is_variable_name('')
    False
    >>> MetadataSyntax.is_variable_name('_hi_mom')
    True
    >>> MetadataSyntax.is_variable_name('2i')
    False
    """
    _blank_line = re.compile(r"^##\s*$")
    _variable_name = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")
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
    def is_variable_name(cls, name):
        return cls._variable_name.match(name) is not None

    @classmethod
    def __init__(cls):
        raise NotImplementedError("Cannot create {} instances".format(cls.__name__))

########################################################################

class FortranMetadataSyntax(MetadataSyntax):
    """FortranMetadataSyntax is a class for Fortran metadata syntax checking.
    >>> FortranMetadataSyntax.line_start("!! Hi mom")
    '!! '
    >>> FortranMetadataSyntax.line_start("!!Hi mom")

    >>> FortranMetadataSyntax.line_start("!> Hi mom")

    >>> FortranMetadataSyntax.table_start("!>Hi mom")

    >>> FortranMetadataSyntax.table_start("#>Hi mom")

    >>> FortranMetadataSyntax.table_start("!! Hi mom")
    '!! '
    >>> FortranMetadataSyntax.line_start("## Hi mom")

    >>> FortranMetadataSyntax.table_start("#>Hi mom")

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

    >>> FortranMetadataSyntax.strip("!  Hi mom")

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

    _blank_line = re.compile(r"!!\s*$")
    _table_start = re.compile(r"(![!>]\s+)")
    _line_start  = re.compile(r"(!!\s+)")
    _variable_name = re.compile(r"^[A-Za-z][A-Za-z0-9_]*$")

########################################################################

class ParseObject(ParseContext):
    """ParseObject is a simple class that keeps track of an object's
    place in a file and safely produces lines from an array of lines
    >>> ParseObject('foobar.F90', 1) #doctest: +ELLIPSIS
    <__main__.ParseObject object at 0x...>
    >>> ParseObject('foobar.F90', 1).filename
    'foobar.F90'
    >>> ParseObject('foobar.F90', ["##hi mom",], line_start=1).curr_line()
    (None, 1)
    >>> ParseObject('foobar.F90', ["first line","## hi mom"], line_start=1).curr_line()
    ('## hi mom', 1)
    >>> ParseObject('foobar.F90', ["##hi mom",], line_start=1).next_line()
    (None, 1)
    >>> ParseObject('foobar.F90', ["##first line","## hi mom"], line_start=1).next_line()
    ('## hi mom', 1)
    >>> ParseObject('foobar.F90', ["## hi \\\\","## mom"], line_start=0).next_line()
    ('## hi mom', 0)
    >>> ParseObject('foobar.F90', ["line1","##line2","## hi mom"], line_start=2).next_line()
    ('## hi mom', 2)
    >>> ParseObject('foobar.F90', ["## hi \\\\","## there \\\\","## mom"], line_start=0).next_line()
    ('## hi there mom', 0)
    >>> ParseObject('foobar.F90', ["!! line1","!! hi mom"], line_start=1).next_line()
    ('!! hi mom', 1)
    """

    def __init__(self, filename, lines_in, line_start=0, syntax=MetadataSyntax):
        _llevel = logger.getEffectiveLevel()
        # Turn off property warnings
        logger.setLevel(logging.ERROR)
        self._filename = filename
        self._lines = lines_in
        self._line_start = line_start
        self._line_end = line_start
        self._line_next = line_start
        self._syntax = syntax
        super(ParseObject, self).__init__(linenum=line_start, filename=filename)
        # Turn logging back on
        logger.setLevel(logging.WARNING if _llevel == logging.NOTSET else _llevel)

    @property
    def first_line_num(self):
        'Return the first line parsed'
        return self._first_line

    @first_line_num.setter
    def first_line_num(self):
        'Do not allow the first_line to be set'
        logger.warning('Cannot set value of first_line')

    @property
    def last_line_num(self):
        'Return the last line parsed'
        return self._line_end

    @last_line_num.setter
    def last_line_num(self):
        'Do not allow the last_line_num to be set'
        logger.warning('Cannot set value of last_line_num')

    @property
    def syntax(self):
        'Return the syntax class for this ParseObject'
        if self._syntax is None:
            return "None"
        else:
            return self._syntax.__name__

    @syntax.setter
    def syntax(self):
        'Do not allow the syntax to be set'
        logger.warning('Cannot set value of syntax')

    def curr_line(self):
        valid_line = self.line_num < len(self._lines)
        _curr_line = None
        _my_curr_lineno = self.line_num
        if valid_line:
            try:
                # Do not strip the comment syntax from first line
                _curr_line = self._lines[self.line_num].rstrip()
                self._line_next = self.line_num + 1
                self._line_end = self._line_next
            except ValueError as exc:
                valid_line = False
        # End if
        # We allow continuation self._lines (ending with a single backslash)
        if valid_line and _curr_line.endswith('\\'):
            if self._syntax is None:
                next_line, lnum = self.next_line()
            else:
                next_line, nlnum = self.next_line()
                if next_line is not None:
                    next_line = self._syntax.strip(next_line)
            # End if
            if next_line is None:
                # We ran out of lines, just strip the backslash
                _curr_line = _curr_line[0:len(_curr_line)-1]
            else:
                _curr_line = _curr_line[0:len(_curr_line)-1] + next_line
            # End if
        # End if
        # curr_line should not change the line number
        self.line_num = _my_curr_lineno
        return _curr_line, self.line_num

    def next_line(self):
        self.line_num = self._line_next
        return self.curr_line()

    def peek_line(self, line_num):
        if line_num < len(self._lines):
            return self._lines[line_num]
        else:
            return None
        # End if

    def reset_pos(self, line_start=0):
        self.line_num = line_start
        self._line_next = line_start

    def write_line(self, line_num, line):
        "Overwrite line, <line_num> with <line>"
        self._lines[line_num] = line


########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
