#!/usr/bin/env python
"""A module for the base, ParseObject class"""

# Python library imports
import re
# CCPP framework imports
from .parse_source    import ParseContext, CCPPError

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
    >>> ParseObject('foobar.F90', ["## hi \\\\","mom"], line_start=0).next_line()
    ('## hi mom', 0)
    >>> ParseObject('foobar.F90', ["line1","##line2","## hi mom"], line_start=2).next_line()
    ('## hi mom', 2)
    >>> ParseObject('foobar.F90', ["## hi \\\\","there \\\\","mom"], line_start=0).next_line()
    ('## hi there mom', 0)
    >>> ParseObject('foobar.F90', ["!! line1","!! hi mom"], line_start=1).next_line()
    ('!! hi mom', 1)
    """

    def __init__(self, filename, lines_in, line_start=0):
        self._filename = filename
        self._lines = lines_in
        self._line_start = line_start
        self._line_end = line_start
        self._line_next = line_start
        super(ParseObject, self).__init__(linenum=line_start, filename=filename)

    @property
    def first_line_num(self):
        'Return the first line parsed'
        return self._first_line

    @property
    def last_line_num(self):
        'Return the last line parsed'
        return self._line_end

    @property
    def file_name(self):
        "Return this object's filename"
        return self._filename

    def curr_line(self):
        valid_line = self.line_num < len(self._lines)
        _curr_line = None
        _my_curr_lineno = self.line_num
        if valid_line:
            try:
                _curr_line = self._lines[self.line_num].rstrip()
                self._line_next = self.line_num + 1
                self._line_end = self._line_next
            except CCPPError as exc:
                valid_line = False
        # End if
        # We allow continuation self._lines (ending with a single backslash)
        if valid_line and _curr_line.endswith('\\'):
            next_line, lnum = self.next_line()
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
        if (line_num >= 0) and (line_num < len(self._lines)):
            return self._lines[line_num]
        else:
            return None
        # End if

    def reset_pos(self, line_start=0):
        if (line_start < 0) or (line_start >= len(self._lines)):
            raise CCPPError('Attempt to reset_pos to non-existent line, {}'.format(line_start))
        else:
            self.line_num = line_start
            self._line_next = line_start
        # End if

    def write_line(self, line_num, line):
        "Overwrite line, <line_num> with <line>"
        if (line_num < 0) or (line_num >= len(self._lines)):
            raise CCPPError('Attempt to write non-existent line, {}'.format(line_num))
        else:
            self._lines[line_num] = line
        # End if

    def __del__(self):
        try:
            del self._lines
            del self.regions
        except Exception as e:
            pass # Python does not guarantee much about __del__ conditions
        # End try

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
