#!/usr/bin/env python

import logging

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

if __name__ == "__main__":
    import doctest
    doctest.testmod()
