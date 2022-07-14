#!/usr/bin/env python3

"""Classes to aid the parsing process"""

import sys
# Find python version
PY3 = sys.version_info[0] > 2

# pylint: disable=wrong-import-position
# Python library imports
if PY3:
    from collections.abc import Iterable
else:
    from collections import Iterable
# end if
import copy
import os.path
import logging
# CCPP framework imports
# pylint: enable=wrong-import-position

class _StdNameCounter:
    """Class to hold a global counter to avoid using global keyword"""
    __SNAME_NUM = 0 # Counter for unique standard names

    @classmethod
    def new_stdname_number(cls):
        """Increment and return the global counter."""
        _StdNameCounter.__SNAME_NUM += 1
        return _StdNameCounter.__SNAME_NUM

    @classmethod
    def reset_stdname_counter(cls, reset_val=0):
        """Reset the global counter to <reset_val>"""
        _StdNameCounter.__SNAME_NUM = reset_val

###############################################################################
def unique_standard_name():
###############################################################################
    """
    Return a unique standard name.
    """
    return 'enter_standard_name_{}'.format(_StdNameCounter.new_stdname_number())

###############################################################################
def reset_standard_name_counter():
###############################################################################
    """
    Reset the unique_standard_name counter so that future calls to
    unique_standard name will restart.
    """
    _StdNameCounter.reset_stdname_counter()

###############################################################################
def context_string(context=None, with_comma=True, nodir=False):
###############################################################################
    """Return a context string if <context> is not None otherwise, return
    an empty string.
    if with_comma is True, prepend string with ', at ' or ', in '.
    >>> context_string()
    ''
    >>> context_string(with_comma=True)
    ''
    >>> context_string(context= ParseContext(linenum=32, filename="dir/source.F90"), with_comma=False)
    'dir/source.F90:33'
    >>> context_string(context= ParseContext(linenum=32, filename="dir/source.F90"), with_comma=True)
    ', at dir/source.F90:33'
    >>> context_string(context= ParseContext(linenum=32, filename="dir/source.F90"))
    ', at dir/source.F90:33'
    >>> context_string(context= ParseContext(filename="dir/source.F90"), with_comma=False)
    'dir/source.F90'
    >>> context_string(context= ParseContext(filename="dir/source.F90"), with_comma=True)
    ', in dir/source.F90'
    >>> context_string(context= ParseContext(filename="dir/source.F90"))
    ', in dir/source.F90'
    >>> context_string(nodir=True)
    ''
    >>> context_string(with_comma=True, nodir=True)
    ''
    >>> context_string(context= ParseContext(linenum=32, filename="dir/source.F90"), with_comma=False, nodir=True)
    'source.F90:33'
    >>> context_string(context= ParseContext(linenum=32, filename="dir/source.F90"), with_comma=True, nodir=True)
    ', at source.F90:33'
    >>> context_string(context= ParseContext(linenum=32, filename="dir/source.F90"), nodir=True)
    ', at source.F90:33'
    >>> context_string(context= ParseContext(filename="dir/source.F90"), with_comma=False, nodir=True)
    'source.F90'
    >>> context_string(context= ParseContext(filename="dir/source.F90"), with_comma=True, nodir=True)
    ', in source.F90'
    >>> context_string(context= ParseContext(filename="dir/source.F90"), nodir=True)
    ', in source.F90'
    """
    if context is None:
        where_str = ''
    elif context.line_num < 0:
        where_str = 'in '
    else:
        where_str = 'at '
    # End if
    if (context is not None) and with_comma:
        comma = ', '
    else:
        comma = ''
        where_str = '' # Override previous setting
    # End if
    if context is None:
        spec = ''
    elif nodir:
        spec = '{ctx:nodir}'
    else:
        spec = '{ctx}'
    # End if
    if context is None:
        cstr = ""
    else:
        cstr = '{comma}{where_str}' + spec
    # End if
    return cstr.format(comma=comma, where_str=where_str, ctx=context)

###############################################################################
def type_name(obj):
###############################################################################
    """Return the name of the type of <obj>"""
    return type(obj).__name__

###############################################################################
class CCPPError(ValueError):
    """Class so programs can log user errors without backtrace"""
    def __init__(self, message):
        """Initialize this exception"""
        logging.shutdown()
        super(CCPPError, self).__init__(message)

########################################################################

class ParseSyntaxError(CCPPError):
    """Exception that is aware of parsing context"""
    def __init__(self, token_type, token=None, context=None):
        """Initialize this exception"""
        logging.shutdown()
        cstr = context_string(context)
        if token is None:
            message = "{}{}".format(token_type, cstr)
        else:
            message = "Invalid {}, '{}'{}".format(token_type, token, cstr)
        # End if
        super(ParseSyntaxError, self).__init__(message)

########################################################################

class ParseInternalError(Exception):
    """Exception for internal parser use errors
    Note that this error will not be trapped by programs such as ccpp_capgen
    """
    def __init__(self, errmsg, context=None):
        """Initialize this exception"""
        logging.shutdown()
        message = "{}{}".format(errmsg, context_string(context))
        super(ParseInternalError, self).__init__(message)

########################################################################

class ParseContextError(CCPPError):
    """Exception for errors using ParseContext"""
    def __init__(self, errmsg, context):
        """Initialize this exception"""
        logging.shutdown()
        message = "{}{}".format(errmsg, context_string(context))
        super(ParseContextError, self).__init__(message)

########################################################################

class ContextRegion(Iterable):
    """Class to imitate the LIFO nature of program language blocks"""

    def __init__(self):
        """Initialize this ContextRegion"""
        self._lifo = list()

    def push(self, rtype, rname):
        """Push a new region onto the stack"""
        self._lifo.append([rtype, rname])

    def pop(self):
        """Remove the top item from the stack"""
        return self._lifo.pop()

    def type_list(self):
        """Return just the types in the list"""
        return [x[0] for x in self._lifo]

    def __iter__(self):
        """Local version of iterator"""
        for item in self._lifo:
            yield item[0]

    def __len__(self):
        """Local implementation of len builtin"""
        return len(self._lifo)

    def __getitem__(self, index):
        """Special item getter for a ContextRegion"""
        return self._lifo[index]

########################################################################

class ParseContext:
    """A class for keeping track of a parsing position
    >>> ParseContext(32, "source.F90") #doctest: +ELLIPSIS
    <__main__.ParseContext object at 0x...>
    >>> ParseContext("source.F90", 32)
    Traceback (most recent call last):
    CCPPError: ParseContext linenum must be an int
    >>> ParseContext(32, 90) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: ParseContext filenum must be a string
    >>> "{}".format(ParseContext(32, "source.F90"))
    'source.F90:33'
    >>> "{}".format(ParseContext())
    '<standard input>'
    >>> ParseContext(linenum=32, filename="source.F90").increment(13)

    """

    def __init__(self, linenum=None, filename=None, context=None):
        """Initialize this ParseContext"""
        # Set regions first in case of exception
        if context is not None:
            self.__regions = copy.deepcopy(context.regions)
        else:
            self.__regions = ContextRegion()
        # End if
        if context is not None:
            # If context is passed, ignore linenum
            linenum = context.line_num
        elif linenum is None:
            linenum = -1
        elif not isinstance(linenum, int):
            raise CCPPError('ParseContext linenum must be an int')
        # No else, everything is okay
        # End if
        if context is not None:
            # If context is passed, ignore filename
            filename = context.filename
        elif filename is None:
            filename = "<standard input>"
        elif not isinstance(filename, str):
            raise CCPPError('ParseContext filename must be a string')
        # No else, everything is okay
        # End if
        self.__linenum = linenum
        self.__filename = filename

    @property
    def line_num(self):
        """Return the current line"""
        return self.__linenum

    @line_num.setter
    def line_num(self, newnum):
        """Set a new line number for this context"""
        self.__linenum = newnum

    @property
    def filename(self):
        """Return the object's filename"""
        return self.__filename

    @property
    def regions(self):
        """Return the object's region list"""
        return self.__regions

    def __format__(self, spec):
        """Return a string representing the location in a file
        Note that self.__linenum is zero based.
        <spec> can be 'dir' (show filename directory) or 'nodir' filename only.
        Any other spec entry is ignored.
        """
        if spec == 'dir':
            fname = self.__filename
        elif spec == 'nodir':
            fname = os.path.basename(self.__filename)
        else:
            fname = self.__filename
        # End if
        if self.__linenum >= 0:
            fmt_str = "{}:{}".format(fname, self.__linenum+1)
        else:
            fmt_str = "{}".format(fname)
        # End if
        return fmt_str

    def __str__(self):
        """Return a string representing the location in a file
        Note that self.__linenum is zero based.
        """
        if self.__linenum >= 0:
            retstr = "{}:{}".format(self.__filename, self.__linenum+1)
        else:
            retstr = "{}".format(self.__filename)
        # End if
        return retstr

    def increment(self, inc=1):
        """Increment the location within a file"""
        if self.__linenum < 0:
            self.__linenum = 0
        # End if
        self.__linenum = self.__linenum + inc

    def enter_region(self, region_type, region_name=None, nested_ok=True):
        """Mark the entry of a region (e.g., DDT, module, function).
        If nested_ok == False, throw an exception if the context is already
        inside a region with the same type."""
        if (region_type not in self.__regions.type_list()) or nested_ok:
            self.__regions.push(region_type, region_name)
        else:
            emsg = "Cannot enter a nested {} region"
            raise ParseContextError(emsg.format(region_type), self)
        # End if

    def leave_region(self, region_type, region_name=None):
        """Mark the exit from a region. Check region name if possible"""
        if self.__regions:
            curr_type, curr_name = self.__regions.pop()
            if curr_type != region_type:
                emsg = "Trying to exit {} region while currently in {} region"
                raise ParseContextError(emsg.format(region_type, curr_type),
                                        self)
            # End if
            if (region_name is not None) and (curr_name is not None):
                if region_name != curr_name:
                    emsg = "Trying to exit {} {} while currently in {} {}"
                    raise ParseContextError(emsg.format(region_type,
                                                        region_name,
                                                        curr_type,
                                                        curr_name), self)
                # End if
            elif (region_name is not None) and (curr_name is None):
                emsg = "Trying to exit {} {} while currently in unnamed {} region"
                raise ParseContextError(emsg.format(region_type, region_name,
                                                    curr_type), self)
            # End if
        else:
            raise ParseContextError("Cannot exit, not currently in any region",
                                    self)
        # End if

    def curr_region(self):
        """Return the innermost current region"""
        curr = None
        if self.__regions:
            curr = self.__regions[-1]
        # No else, will return None
        # End if
        return curr

    def in_region(self, region_type, region_name=None):
        """Return True iff we are currently in <region_type> <region_name>"""
        return self.curr_region() == [region_type, region_name]

    def region_str(self):
        """Create a string describing the current region"""
        rgn_str = ""
        for index in len(self.__regions):
            rtype, rname = self.__regions[index]
            if rgn_str:
                rgn_str += " ==> "
            # End if
            rgn_str += "{}".format(rtype)
            if rname is not None:
                rgn_str += " {}".format(rname)
            # End if
        # End for
        return rgn_str

########################################################################

class ParseSource:
    """
    A simple object for providing source information
    >>> ParseSource("myname", "mytype", ParseContext(13, "foo.F90")) #doctest: +ELLIPSIS
    <__main__.ParseSource object at 0x...>
    >>> ParseSource("myname", "mytype", ParseContext(13, "foo.F90")).ptype
    'mytype'
    >>> ParseSource("myname", "mytype", ParseContext(13, "foo.F90")).name
    'myname'
    >>> print("{}".format(ParseSource("myname", "mytype", ParseContext(13, "foo.F90")).context))
    foo.F90:14
    """

    def __init__(self, name_in, type_in, context_in):
        """Initialize this ParseSource object."""
        self.__name = name_in
        self.__type = type_in
        self.__context = context_in

    @property
    def ptype(self):
        """Return this source's type"""
        return self.__type

    @property
    def name(self):
        """Return this source's name"""
        return self.__name

    @property
    def context(self):
        """Return this source's context"""
        return self.__context

########################################################################

if __name__ == "__main__":
    # pylint: disable=ungrouped-imports
    import doctest
    # pylint: enable=ungrouped-imports
    fail, _ = doctest.testmod()
    sys.exit(fail)
# end if
