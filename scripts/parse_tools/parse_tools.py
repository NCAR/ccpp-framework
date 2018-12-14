#!/usr/bin/env python

"""Classes to aid the parsing process"""

# Python library imports
import collections
import copy
# CCPP framework imports

###############################################################################
class CCPPError(ValueError):
    "Class so programs can log user errors without backtrace"
    def __init__(self, message):
        super(CCPPError, self).__init__(message)

########################################################################

class ParseSyntaxError(CCPPError):
    """Exception that is aware of parsing context"""
    def __init__(self, token_type, token=None, context=None):
        if context is None:
            if token is None:
                message = "{}".format(token_type)
            else:
                message = "Invalid {}, '{}'".format(token_type, token)
        else:
            if token is None:
                message = "{}, at {}".format(token_type, context)
            else:
                message = "Invalid {}, '{}', at {}".format(token_type, token, context)
        # End if
        super(ParseSyntaxError, self).__init__(message)

########################################################################

class ParseInternalError(StandardError):
    """Exception for internal parser use errors
    Note that this error will not be trapped by programs such as ccpp_capgen
    """
    def __init__(self, errmsg, context):
        if context is None:
            message = errmsg
        else:
            message = "{}, at {}".format(errmsg, context)
        # End if
        super(ParseInternalError, self).__init__(message)

########################################################################

class ParseContextError(CCPPError):
    """Exception for errors using ParseContext"""
    def __init__(self, errmsg, context):
        if context is None:
            message = "{}".format(errmsg)
        else:
            message = "{}, at {}".format(errmsg, context)
        # End if
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
    '<standard input>:1'
    >>> ParseContext(32, "source.F90").increment(13)

    """

    def __init__(self, linenum=None, filename=None, context=None):
        if context is not None:
            # If context is passed, ignore linenum
            linenum = context.line_num
        elif linenum is None:
            linenum = 0
        elif type(linenum) != int:
            raise CCPPError('ParseContext linenum must be an int')
        # End if
        if context is not None:
            # If context is passed, ignore filename
            filename = context.filename
        elif filename is None:
            filename = "<standard input>"
        elif type(filename) != str:
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
        return "{}:{}".format(self._filename, self._linenum+1)

    def increment(self, inc=1):
        "Increment the location within a file"
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
