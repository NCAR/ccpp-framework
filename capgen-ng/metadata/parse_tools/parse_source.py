"""Parsing primitives: parse context and exception types."""

import logging
import os.path


def context_string(context=None, with_comma=True, nodir=False):
    """Return a human-readable location string from *context*.

    Parameters
    ----------
    context : ParseContext or None
        Parsing location. ``None`` returns an empty string.
    with_comma : bool
        Prepend ``', at '`` or ``', in '`` when *context* is given.
    nodir : bool
        Strip the directory portion of the filename.

    >>> context_string()
    ''
    >>> context_string(context=ParseContext(linenum=32, filename="dir/source.F90"), with_comma=False)
    'dir/source.F90:33'
    >>> context_string(context=ParseContext(linenum=32, filename="dir/source.F90"), with_comma=True)
    ', at dir/source.F90:33'
    >>> context_string(context=ParseContext(filename="dir/source.F90"), with_comma=False)
    'dir/source.F90'
    >>> context_string(context=ParseContext(linenum=32, filename="dir/source.F90"), with_comma=False, nodir=True)
    'source.F90:33'
    """
    if context is None:
        return ''
    if context.line_num < 0:
        where_str = 'in '
    else:
        where_str = 'at '
    comma = ', ' if with_comma else ''
    if not with_comma:
        where_str = ''
    spec = '{ctx:nodir}' if nodir else '{ctx}'
    return ('{comma}{where_str}' + spec).format(comma=comma, where_str=where_str, ctx=context)


class CCPPError(ValueError):
    """User-facing error with a plain message and no traceback noise."""

    def __init__(self, message):
        logging.shutdown()
        super().__init__(message)


class ParseSyntaxError(CCPPError):
    """Syntax error that includes parsing context in the message."""

    def __init__(self, token_type, token=None, context=None):
        logging.shutdown()
        cstr = context_string(context)
        if token is None:
            message = "{}{}".format(token_type, cstr)
        else:
            message = "Invalid {}, '{}'{}".format(token_type, token, cstr)
        super().__init__(message)


class ParseInternalError(Exception):
    """Internal parser logic error — not caught by normal user-error handlers."""

    def __init__(self, errmsg, context=None):
        logging.shutdown()
        message = "{}{}".format(errmsg, context_string(context))
        super().__init__(message)


class ParseContext:
    """File-position record used as the location anchor for parse errors.

    Holds a filename and a zero-based line number (negative means «file
    level, no specific line»); formats as ``filename:line``.

    >>> str(ParseContext(linenum=0, filename="foo.F90"))
    'foo.F90:1'
    >>> str(ParseContext(filename="foo.F90"))
    'foo.F90'
    >>> ParseContext(linenum="bad", filename="f.F90") #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
        ...
    CCPPError: ParseContext linenum must be an int
    """

    def __init__(self, linenum=None, filename=None):
        if linenum is None:
            linenum = -1
        elif not isinstance(linenum, int):
            raise CCPPError('ParseContext linenum must be an int')

        if filename is None:
            filename = "<standard input>"
        elif not isinstance(filename, str):
            raise CCPPError('ParseContext filename must be a string')

        self.__linenum = linenum
        self.__filename = filename

    @property
    def line_num(self):
        return self.__linenum

    @property
    def filename(self):
        return self.__filename

    def __format__(self, spec):
        fname = os.path.basename(self.__filename) if spec == 'nodir' else self.__filename
        if self.__linenum >= 0:
            return "{}:{}".format(fname, self.__linenum + 1)
        return fname

    def __str__(self):
        return format(self)
