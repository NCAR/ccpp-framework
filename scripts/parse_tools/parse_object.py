#!/usr/bin/env python
"""A module for the base, ParseObject class"""

import logging
from parse_tools import MetadataSyntax, FortranMetadataSyntax, ParseContext

logger = logging.getLogger(__name__)

class ParseObject(object):
    """ParseObject is a simple class that keeps track of an object's
    place in a file and safely produces lines from an array of lines
    >>> ParseObject('foobar.F90', 1) #doctest: +ELLIPSIS
    <__main__.ParseObject object at 0x...>
    >>> ParseObject('foobar.F90', 1).filename
    'foobar.F90'
    >>> ParseObject('foobar.F90', 1).curr_line(["##hi mom",])

    >>> ParseObject('foobar.F90', 1).curr_line(["first line","## hi mom"])
    '## hi mom'
    >>> ParseObject('foobar.F90', 1).next_line(["##hi mom",])

    >>> ParseObject('foobar.F90', 1).next_line(["##first line","## hi mom"])
    '## hi mom'
    >>> ParseObject('foobar.F90', 0).next_line(["## hi \\\\","## mom"])
    '## hi mom'
    >>> ParseObject('foobar.F90', 2).next_line(["line1","##line2","## hi mom"])
    '## hi mom'
    >>> ParseObject('foobar.F90', 0).next_line(["## hi \\\\","## there \\\\","## mom"])
    '## hi there mom'
    >>> ParseObject('foobar.F90', 1, syntax=FortranMetadataSyntax).next_line(["!! line1","!! hi mom"])
    '!! hi mom'
    >>> ParseObject('foobar.F90', 2).syntax
    'MetadataSyntax'
    >>> ParseObject('foobar.F90', 1, syntax=FortranMetadataSyntax).syntax
    'FortranMetadataSyntax'
    """

    def __init__(self, filename, line_start, syntax=MetadataSyntax):
        _llevel = logger.getEffectiveLevel()
        # Turn off property warnings
        logger.setLevel(logging.ERROR)
        self._filename = filename
        self._line_start = line_start
        self._line_end = line_start
        self._line_curr = line_start
        self._line_next = line_start
        self._syntax = syntax
        self._context = ParseContext(linenum=line_start, filename=filename)
        # Turn logging back on
        logger.setLevel(logging.WARNING if _llevel == logging.NOTSET else _llevel)

    @property
    def filename(self):
        "'Return the object's filename"
        return self._filename

    @filename.setter
    def filename(self):
        'Do not allow the filename to be set'
        logger.warning('Cannot set value of filename')

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
        'Return the syntax class for this HeaderVariable'
        return self._syntax.__name__

    @syntax.setter
    def syntax(self):
        'Do not allow the syntax to be set'
        logger.warning('Cannot set value of syntax')

    def blank_line(self, line):
        return self._syntax.blank_line(line)

    def curr_line(self, lines):
        valid_line = self._line_curr < len(lines)
        _curr_line = None
        _my_curr_lineno = self._line_curr
        if valid_line:
            try:
                # Do not strip the comment syntax from first line
                _curr_line = lines[self._line_curr].rstrip()
                self._line_next = self._line_curr + 1
                self._line_end = self._line_next
            except ValueError as exc:
                valid_line = False
        # End if
        # We allow continuation lines (ending with a single backslash)
        if valid_line and _curr_line.endswith('\\'):
            next_line = self._syntax.strip(self.next_line(lines))
            if next_line is None:
                # We ran out of lines, just strip the backslash
                _curr_line = _curr_line[0:len(_curr_line)-1]
            else:
                _curr_line = _curr_line[0:len(_curr_line)-1] + next_line
            # End if
        # End if
        # curr_line should not change the line number
        self._line_curr = _my_curr_lineno
        return _curr_line

    def next_line(self, lines):
        self._line_curr = self._line_next
        # For now, just reset the context
        self._context.line_num = self._line_curr
        return self.curr_line(lines)

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
