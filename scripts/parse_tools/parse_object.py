#!/usr/bin/env python3
"""A module for the base, ParseObject class"""

# CCPP framework imports
from parse_source    import ParseContext, CCPPError, context_string

########################################################################

class ParseObject(ParseContext):
    """ParseObject is a simple class that keeps track of an object's
    place in a file and safely produces lines from an array of lines
    >>> ParseObject('foobar.F90', []) #doctest: +ELLIPSIS
    <parse_tools.parse_object.ParseObject object at 0x...>
    >>> ParseObject('foobar.F90', []).filename
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

    _max_errors = 32

    def __init__(self, filename, lines_in, line_start=0):
        """Initialize this ParseObject"""
        self.__filename = filename
        self.__lines = lines_in
        self.__line_start = line_start
        self.__line_end = line_start
        self.__line_next = line_start
        self.__num_lines = len(self.__lines)
        self.__error_message = ""
        self.__num_errors = 0
        super(ParseObject, self).__init__(linenum=line_start, filename=filename)

    @property
    def first_line_num(self):
        """Return the first line parsed"""
        return self.__line_start

    @property
    def last_line_num(self):
        """Return the last line parsed"""
        return self.__line_end

    def valid_line(self):
        """Return True if the current line is valid"""
        return (self.line_num >= 0) and (self.line_num < self.__num_lines)

    @property
    def file_name(self):
        """Return this object's filename"""
        return self.__filename

    @property
    def error_message(self):
        """Return this object's error message"""
        return self.__error_message

    def curr_line(self):
        """Return the current line (if valid) and the current line number.
        If the current line is invalid, return None"""
        valid_line = self.valid_line()
        _curr_line = None
        _my_curr_lineno = self.line_num
        if valid_line:
            try:
                _curr_line = self.__lines[self.line_num].rstrip()
                self.__line_next = self.line_num + 1
                self.__line_end = self.__line_next
            except CCPPError:
                self.add_syntax_err("line", self.line_num)
                valid_line = False
        # end if
        # We allow continuation self.__lines (ending with a single backslash)
        if valid_line and _curr_line.endswith('\\'):
            next_line, _ = self.next_line()
            if next_line is None:
                # We ran out of lines, just strip the backslash
                _curr_line = _curr_line[0:len(_curr_line)-1]
            else:
                _curr_line = _curr_line[0:len(_curr_line)-1] + next_line
            # end if
        # end if
        # curr_line should not change the line number
        self.line_num = _my_curr_lineno
        return _curr_line, self.line_num

    def next_line(self):
        """Return the next line in our file (if valid) and the next line's
        line number. If the next line is not valid, return None"""
        self.line_num = self.__line_next
        return self.curr_line()

    def peek_line(self, line_num):
        """Return the text of <line_num> without advancing to that line.
        if <line_num> is out of bounds, return None."""
        if (line_num >= 0) and (line_num < len(self.__lines)):
            return self.__lines[line_num]
        # end if
        return None

    def add_syntax_err(self, token_type, token=None, skip_context=False):
        """Add a ParseSyntaxError-type message to this object's error
        log, separating it from any previous messages with a newline."""
        if self.__error_message:
            if self.__num_errors == self._max_errors:
                self.__error_message += '\nMaximum number of errors exceeded'
                self.line_num = self.__num_lines # Intentionally walk off end
                self.__line_next = self.line_num
            elif self.__num_errors > self._max_errors:
                # Oops, something went wrong, panic!
                raise CCPPError(self.error_message)
            # end if
            self.__error_message += '\n'
        # end if
        if self.__num_errors < self._max_errors:
            if skip_context:
                cstr = ""
            else:
                cstr = context_string(self)
            # end if
            if token is None:
                self.__error_message += "{}{}".format(token_type, cstr)
            else:
                self.__error_message += "Invalid {}, '{}'{}".format(token_type,
                                                                    token, cstr)
            # end if
        # end if
        self.__num_errors += 1

    def reset_pos(self, line_start=0):
        """Attempt to set the current file position to <line_start>.
        If <line_start> is out of bounds, raise an exception."""
        if (line_start < 0) or (line_start >= self.__num_lines):
            emsg = 'Attempt to reset_pos to non-existent line, {}'
            raise CCPPError(emsg.format(line_start))
        # end if
        self.line_num = line_start
        self.__line_next = line_start

    def write_line(self, line_num, line):
        """Overwrite line, <line_num> with <line>.
        If <line_start> is out of bounds, raise an exception."""
        if (line_num < 0) or (line_num >= len(self.__lines)):
            emsg = 'Attempt to write non-existent line, {}'
            raise CCPPError(emsg.format(line_num))
        # end if
        self.__lines[line_num] = line

    def __del__(self):
        """Attempt to cleanup memory used by this object"""
        try:
            del self.__lines
            del self.regions
        except Exception:
            pass # Python does not guarantee much about __del__ conditions
        # end try

########################################################################
