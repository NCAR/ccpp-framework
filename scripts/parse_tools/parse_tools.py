#!/usr/bin/env python

"""Classes to aid the parsing process"""

# Python library imports
import collections
import copy
# CCPP framework imports

###############################################################################
def context_string(context=None, with_comma=True):
###############################################################################
    """Return a context string if <context> is not None otherwise, return
    an empty string.
    if with_comma is True, prepend string with ', at ' or ', in '.
    >>> context_string()
    ''
    >>> context_string(with_comma=True)
    ''
    >>> context_string(context= ParseContext(linenum=32, filename="source.F90"), with_comma=False)
    'source.F90:33'
    >>> context_string(context= ParseContext(linenum=32, filename="source.F90"), with_comma=True)
    ', at source.F90:33'
    >>> context_string(context= ParseContext(linenum=32, filename="source.F90"))
    ', at source.F90:33'
    >>> context_string(context= ParseContext(filename="source.F90"), with_comma=False)
    'source.F90'
    >>> context_string(context= ParseContext(filename="source.F90"), with_comma=True)
    ', in source.F90'
    >>> context_string(context= ParseContext(filename="source.F90"))
    ', in source.F90'
    """
    if context is None:
        cstr = ""
    elif with_comma:
        if context.line_num < 0:
            cstr = ", in {}".format(context)
        else:
            cstr = ", at {}".format(context)
        # End if
    else:
        cstr = "{}".format(context)
    # End if
    return cstr

###############################################################################
class CCPPError(ValueError):
    "Class so programs can log user errors without backtrace"
    def __init__(self, message):
        super(CCPPError, self).__init__(message)

########################################################################

class ParseSyntaxError(CCPPError):
    """Exception that is aware of parsing context"""
    def __init__(self, token_type, token=None, context=None):
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
        message = "{}{}".format(errmsg, context_string(context))
        super(ParseInternalError, self).__init__(message)

########################################################################

class ParseContextError(CCPPError):
    """Exception for errors using ParseContext"""
    def __init__(self, errmsg, context):
        message = "{}{}".format(errmsg, context_string(context))
        super(ParseContextError, self).__init__(message)

########################################################################

class ContextRegion(collections.Iterable):
    """Class to imitate the LIFO nature of program language blocks"""

    def __init__(self):
        self._lifo = list()

    def push(self, rtype, rname):
        """Push a new region onto the stack"""
        self._lifo.append([rtype, rname])

    def pop(self):
        """Remove the top item from the stack"""
        return self._lifo.pop()

    def __iter__(self):
        for item in self._lifo:
            yield item[0]

    def __len__(self):
        return len(self._lifo)

    def __getitem__(self, index):
        return self._lifo[index]

########################################################################

class ParseContext(object):
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
        if context is not None:
            # If context is passed, ignore linenum
            linenum = context.line_num
        elif linenum is None:
            linenum = -1
        elif not isinstance(linenum, int):
            raise CCPPError('ParseContext linenum must be an int')
        # End if
        if context is not None:
            # If context is passed, ignore filename
            filename = context.filename
        elif filename is None:
            filename = "<standard input>"
        elif not isinstance(filename, str):
            raise CCPPError('ParseContext filename must be a string')
        # End if
        self._linenum = linenum
        self._filename = filename
        if context is not None:
            self.regions = copy.deepcopy(context.regions)
        else:
            self.regions = ContextRegion()
        # End if

    @property
    def line_num(self):
        'Return the current line'
        return self._linenum

    @line_num.setter
    def line_num(self, newnum):
        self._linenum = newnum

    @property
    def filename(self):
        "'Return the object's filename"
        return self._filename

    def __str__(self):
        """Return a string representing the location in a file
        Note that self._linenum is zero based.
        """
        if self._linenum >= 0:
            return "{}:{}".format(self._filename, self._linenum+1)
        else:
            return "{}".format(self._filename)

    def increment(self, inc=1):
        "Increment the location within a file"
        if self._linenum < 0:
            self._linenum = 0
        # End if
        self._linenum = self._linenum + inc

    def enter_region(self, region_type, region_name=None, nested_ok=True):
        """Mark the entry of a region (e.g., DDT, module, function).
        If nested_ok == False, throw an exception if the context is already
        inside a region with the same type."""
        if (not nested_ok) and (region_type in self.regions):
            raise ParseContextError("Cannot enter a nested {} region".format(region_type), self)
        else:
            self.regions.push(region_type, region_name)

    def leave_region(self, region_type, region_name=None):
        """Mark the exit from a region. Check region name if possible"""
        if len(self.regions) == 0:
            raise ParseContextError("Cannot exit, not currently in any region", self)
        else:
            curr_type, curr_name = self.regions.pop()
            if curr_type != region_type:
                raise ParseContextError("Trying to exit {} region while currently in {} region".format(region_type, curr_type), self)
            elif (region_name is not None) and (curr_name is not None):
                if region_name != curr_name:
                    raise ParseContextError("Trying to exit {} {} while currently in {} {}".format(region_type, region_name, curr_type, curr_name), self)
                # End if
            elif (region_name is not None) and (curr_name is None):
                raise ParseContextError("Trying to exit {} {} while currently in unnamed {} region".format(region_type, region_name, curr_type), self)
            # End if
        # End if

    def curr_region(self):
        """Return the innermost current region"""
        return self.regions[-1]

    def region_str(self):
        """Create a string describing the current region"""
        rgn_str = ""
        for index in len(self.regions):
            rtype, rname = self.regions[index]
            if len(rgn_str) > 0:
                rgn_str = rgn_str + " ==> "
            # End if
            rgn_str = rgh_str + "{}".format(rtype)
            if rname is not None:
                rgn_str = rgh_str + "{}".format(rname)
            # End if
        # End for
        return rgn_str

########################################################################

class ParseSource(object):
    """
    A simple object for providing source information
    >>> ParseSource("myname", "mytype", ParseContext(13, "foo.F90")) #doctest: +ELLIPSIS
    <__main__.ParseSource object at 0x...>
    >>> ParseSource("myname", "mytype", ParseContext(13, "foo.F90")).type
    'mytype'
    >>> ParseSource("myname", "mytype", ParseContext(13, "foo.F90")).name
    'myname'
    >>> print("{}".format(ParseSource("myname", "mytype", ParseContext(13, "foo.F90")).context))
    foo.F90:14
    """

    def __init__(self, name_in, type_in, context_in):
        self._name = name_in
        self._type = type_in
        self._context = context_in

    @property
    def type(self):
        return self._type

    @property
    def name(self):
        return self._name

    @property
    def context(self):
        return self._context

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
