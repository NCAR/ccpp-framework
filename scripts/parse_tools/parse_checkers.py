#!/usr/bin/env python

"""Helper functions to validate parsed input"""

import re

# FORTRAN_ID is a string representing the regular expression for Fortran names
FORTRAN_ID = r"[A-Za-z][A-Za-z0-9_]*"
__FID_RE = re.compile(FORTRAN_ID+r"$")

########################################################################

def check_fortran_id(test_val, max_len=0, error=False):
    """Return <test_val> if a valid Fortran identifier, otherwise, None
    If <max_len> > 0, <test_val> must not be longer than <max_len>.
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_fortran_id("hi_mom")
    'hi_mom'
    >>> check_fortran_id("hi_mom", max_len=5)

    >>> check_fortran_id("hi_mom", max_len=5, error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: 'hi_mom' is too long (> 5 chars)
    >>> check_fortran_id("hi mom")

    >>> check_fortran_id("hi mom", error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: 'hi_mom' is not a valid Fortran identifier
    >>> check_fortran_id("")

    >>> check_fortran_id("_hi_mom")

    >>> check_fortran_id("2pac")

    >>> check_fortran_id("Agood4tranID")
    'Agood4tranID'
    """
    match = __FID_RE.match(test_val)
    if match is None:
        if error:
            raise ValueError("'{}' is not a valid Fortran identifier".format(test_val))
        else:
            test_val = None
        # End if
    elif (max_len > 0) and (len(test_val) > max_len):
        if error:
            raise ValueError("'{}' is too long (> {} chars)".format(test_val, max_len))
        test_val = None
    # End if
    return test_val

########################################################################

def check_dimensions(test_val, max_len=0, error=False):
    """Return <test_val> if a valid dimensions list, otherwise, None
    If <max_len> > 0, each string in <test_val> must not be longer than
    <max_len>.
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_dimensions(["dim1", "dim2name"])
    ['dim1', 'dim2name']
    >>> check_dimensions(["dim1", "dim2name"], max_len=5)

    >>> check_dimensions(["dim1", "dim2name"], error=True, max_len=5)
    Traceback (most recent call last):
    ValueError: 'dim2name' is too long (> 5 chars)
    >>> check_dimensions("hi_mom", error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: 'hi_mom' is invalid; not a list
    """
    if type(test_val) != list:
        if error:
            raise ValueError("'{}' is invalid; not a list".format(test_val))
        else:
            test_val = None
        # End if
    else:
        for item in test_val:
            tv = check_fortran_id(item, max_len=max_len, error=error)
            if tv is None:
                test_val = None
                break
            # End if
        # End for
    # End if
    return test_val

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
