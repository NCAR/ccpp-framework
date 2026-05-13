#!/usr/bin/env python3

"""Parsing primitives: context tracking, exception types, and source tagging.

Copied from scripts/parse_tools/parse_source.py and adapted for the
capgen-ng package structure.
"""

from collections.abc import Iterable
import copy
import os.path
import logging


class _StdNameCounter:
    """Global counter for generating unique placeholder standard names."""

    __SNAME_NUM = 0

    @classmethod
    def new_stdname_number(cls):
        """Increment and return the counter."""
        _StdNameCounter.__SNAME_NUM += 1
        return _StdNameCounter.__SNAME_NUM

    @classmethod
    def reset_stdname_counter(cls, reset_val=0):
        """Reset the counter to *reset_val*."""
        _StdNameCounter.__SNAME_NUM = reset_val


def unique_standard_name():
    """Return a unique placeholder standard name.

    Used during parsing when a real standard name is not yet known.

    >>> n1 = unique_standard_name()
    >>> n2 = unique_standard_name()
    >>> n1 != n2
    True
    >>> n1.startswith('enter_standard_name_')
    True
    """
    return 'enter_standard_name_{}'.format(_StdNameCounter.new_stdname_number())


def reset_standard_name_counter():
    """Reset the unique_standard_name counter to zero."""
    _StdNameCounter.reset_stdname_counter()


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

    Returns
    -------
    str

    >>> context_string()
    ''
    >>> context_string(context=ParseContext(linenum=32, filename="dir/source.F90"), with_comma=False)
    'dir/source.F90:33'
    >>> context_string(context=ParseContext(linenum=32, filename="dir/source.F90"), with_comma=True)
    ', at dir/source.F90:33'
    >>> context_string(context=ParseContext(filename="dir/source.F90"), with_comma=False)
    'dir/source.F90'
    >>> context_string(context=ParseContext(filename="dir/source.F90"), with_comma=True)
    ', in dir/source.F90'
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


def type_name(obj):
    """Return the class name of *obj*.

    >>> type_name(42)
    'int'
    >>> type_name("hello")
    'str'
    """
    return type(obj).__name__


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


class ParseContextError(CCPPError):
    """Error arising from mis-use of ParseContext."""

    def __init__(self, errmsg, context):
        logging.shutdown()
        message = "{}{}".format(errmsg, context_string(context))
        super().__init__(message)


class ContextRegion(Iterable):
    """LIFO stack of (region_type, region_name) pairs."""

    def __init__(self):
        self._lifo = []

    def push(self, rtype, rname):
        """Push a new region onto the stack."""
        self._lifo.append([rtype, rname])

    def pop(self):
        """Remove and return the top item."""
        return self._lifo.pop()

    def type_list(self):
        """Return a list of just the region types."""
        return [x[0] for x in self._lifo]

    def __iter__(self):
        for item in self._lifo:
            yield item[0]

    def __len__(self):
        return len(self._lifo)

    def __getitem__(self, index):
        return self._lifo[index]


class ParseContext:
    """Tracks a parser's current position inside a source file.

    Parameters
    ----------
    linenum : int, optional
        Zero-based line number. Negative means «file level, no specific line».
    filename : str, optional
        Path to the source file.
    context : ParseContext, optional
        Copy position from an existing context (overrides *linenum*/*filename*).

    Examples
    --------
    >>> ctx = ParseContext(linenum=0, filename="foo.F90")
    >>> str(ctx)
    'foo.F90:1'
    >>> ctx.increment(4)
    >>> str(ctx)
    'foo.F90:5'
    >>> ParseContext(linenum="bad", filename="f.F90")
    Traceback (most recent call last):
        ...
    metadata.parse_tools.parse_source.CCPPError: ParseContext linenum must be an int
    """

    def __init__(self, linenum=None, filename=None, context=None):
        if context is not None:
            self.__regions = copy.deepcopy(context.regions)
        else:
            self.__regions = ContextRegion()

        if context is not None:
            linenum = context.line_num
        elif linenum is None:
            linenum = -1
        elif not isinstance(linenum, int):
            raise CCPPError('ParseContext linenum must be an int')

        if context is not None:
            filename = context.filename
        elif filename is None:
            filename = "<standard input>"
        elif not isinstance(filename, str):
            raise CCPPError('ParseContext filename must be a string')

        self.__linenum = linenum
        self.__filename = filename

    def default_module_name(self):
        """Return the base name (without extension) of the source file."""
        return os.path.splitext(os.path.basename(self.__filename))[0]

    @property
    def line_num(self):
        """Current zero-based line number (negative = file level)."""
        return self.__linenum

    @line_num.setter
    def line_num(self, newnum):
        self.__linenum = newnum

    @property
    def filename(self):
        """Path to the source file being parsed."""
        return self.__filename

    @property
    def regions(self):
        """The nested-region stack."""
        return self.__regions

    def __format__(self, spec):
        """Format the context as ``filename:line``.

        Supported format specs: ``'nodir'`` strips the directory component.
        """
        fname = os.path.basename(self.__filename) if spec == 'nodir' else self.__filename
        if self.__linenum >= 0:
            return "{}:{}".format(fname, self.__linenum + 1)
        return fname

    def __str__(self):
        if self.__linenum >= 0:
            return "{}:{}".format(self.__filename, self.__linenum + 1)
        return self.__filename

    def increment(self, inc=1):
        """Advance the line counter by *inc* (default 1)."""
        if self.__linenum < 0:
            self.__linenum = 0
        self.__linenum += inc

    def enter_region(self, region_type, region_name=None, nested_ok=True):
        """Record entering a named region (module, DDT, subroutine, …).

        If *nested_ok* is False, raises :exc:`ParseContextError` when already
        inside a region of the same type.
        """
        if (region_type not in self.__regions.type_list()) or nested_ok:
            self.__regions.push(region_type, region_name)
        else:
            raise ParseContextError(
                "Cannot enter a nested {} region".format(region_type), self
            )

    def leave_region(self, region_type, region_name=None):
        """Record leaving a region, with optional name verification."""
        if self.__regions:
            curr_type, curr_name = self.__regions.pop()
            if curr_type != region_type:
                raise ParseContextError(
                    "Trying to exit {} region while currently in {} region".format(
                        region_type, curr_type
                    ),
                    self,
                )
            if region_name is not None and curr_name is not None:
                if region_name != curr_name:
                    raise ParseContextError(
                        "Trying to exit {} {} while currently in {} {}".format(
                            region_type, region_name, curr_type, curr_name
                        ),
                        self,
                    )
            elif region_name is not None and curr_name is None:
                raise ParseContextError(
                    "Trying to exit {} {} while currently in unnamed {} region".format(
                        region_type, region_name, curr_type
                    ),
                    self,
                )
        else:
            raise ParseContextError("Cannot exit, not currently in any region", self)

    def curr_region(self):
        """Return the innermost (type, name) pair, or None if not in any region."""
        return self.__regions[-1] if self.__regions else None

    def in_region(self, region_type, region_name=None):
        """Return True iff currently inside *region_type* (optionally *region_name*)."""
        return self.curr_region() == [region_type, region_name]


class ParseSource:
    """Lightweight tag associating a name and type with a parse context.

    >>> src = ParseSource("my_func", "subroutine", ParseContext(0, "src.F90"))
    >>> src.name
    'my_func'
    >>> src.ptype
    'subroutine'
    >>> str(src.context)
    'src.F90:1'
    """

    def __init__(self, name_in, type_in, context_in):
        self.__name = name_in
        self.__type = type_in
        self.__context = context_in

    @property
    def ptype(self):
        """The type label (e.g. 'scheme', 'host', 'subroutine')."""
        return self.__type

    @property
    def name(self):
        """The name of the parsed entity."""
        return self.__name

    @property
    def context(self):
        """The :class:`ParseContext` where this entity was found."""
        return self.__context
