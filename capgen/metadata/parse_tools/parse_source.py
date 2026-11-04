"""Parsing primitives: parse context and exception types."""


class CCPPError(ValueError):
    """User-facing error with a plain message and no traceback noise."""


class ParseSyntaxError(CCPPError):
    """Syntax error that includes parsing context in the message.

    >>> str(ParseSyntaxError("dimension", token="foo", context=ParseContext(32, "s.F90")))
    "Invalid dimension, 'foo', at s.F90:33"
    >>> str(ParseSyntaxError("End of file", context=ParseContext(filename="s.F90")))
    'End of file, in s.F90'
    """

    def __init__(self, token_type, token=None, context=None):
        if context is None:
            cstr = ''
        else:
            where_str = 'at' if context.line_num >= 0 else 'in'
            cstr = ", {} {}".format(where_str, context)
        if token is None:
            message = "{}{}".format(token_type, cstr)
        else:
            message = "Invalid {}, '{}'{}".format(token_type, token, cstr)
        super().__init__(message)


class ParseContext:
    """File-position record used as the location anchor for parse errors.

    Holds a filename and a zero-based line number (negative means «file
    level, no specific line»); formats as ``filename:line`` (1-based).

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

        self.line_num = linenum
        self.filename = filename

    def __str__(self):
        if self.line_num >= 0:
            return "{}:{}".format(self.filename, self.line_num + 1)
        return self.filename
