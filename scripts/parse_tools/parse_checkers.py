#!/usr/bin/env python

"""Helper functions to validate parsed input"""

import re
import copy

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
                if error:
                    raise ValueError("'{}' is an invalid dimension name".format(item))
                else:
                    test_val = None
                # End if
                break
            # End if
        # End for
    # End if
    return test_val

########################################################################

# CF_ID is a string representing the regular expression for CF Standard Names
CF_ID = r"[a-z][a-z0-9_]*"
__CFID_RE = re.compile(CF_ID+r"$")

def check_cf_standard_name(test_val, error=False):
    """Return <test_val> if a valid CF Standard Name, otherwise, None
    http://cfconventions.org/Data/cf-standard-names/docs/guidelines.html
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_cf_standard_name("hi_mom")
    'hi_mom'
    >>> check_cf_standard_name("hi mom")

    >>> check_cf_standard_name("hi mom", error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: 'hi_mom' is not a valid CF Standard Name
    >>> check_cf_standard_name("")

    >>> check_cf_standard_name("_hi_mom")

    >>> check_cf_standard_name("2pac")

    >>> check_cf_standard_name("Agood4tranID")

    >>> check_cf_standard_name("agoodcfid")
    'agoodcfid'
    """
    match = __CFID_RE.match(test_val)
    if match is None:
        if error:
            raise ValueError("'{}' is not a valid CF Standard Name".format(test_val))
        else:
            test_val = None
        # End if
    # End if
    return test_val

########################################################################

### Fortran-specific parsing helper variables and functions

########################################################################

# FORTRAN_ID is a string representing the regular expression for Fortran names
FORTRAN_ID = r"[A-Za-z][A-Za-z0-9_]*"
__FID_RE = re.compile(FORTRAN_ID+r"$")
FORTRAN_INTRINSIC_TYPES = [ "integer", "real", "logical", "complex",
                            "double precision", "character" ]
FORTRAN_DP_RE = re.compile(r"(?i)double\s*precision")
FORTRAN_TYPE_RE = re.compile(r"(?i)type\s*\(\s*("+FORTRAN_ID+r")\s*\)")

_REGISTERED_FORTRAN_DDT_NAMES = []

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

def check_fortran_intrinsic(typestr, error=False):
    """Return <test_val> if a valid Fortran intrinsic type, otherwise, None
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_fortran_intrinsic("real")
    'real'
    >>> check_fortran_intrinsic("complex")
    'complex'
    >>> check_fortran_intrinsic("integer")
    'integer'
    >>> check_fortran_intrinsic("InteGer")
    'InteGer'
    >>> check_fortran_intrinsic("logical")
    'logical'
    >>> check_fortran_intrinsic("character")
    'character'
    >>> check_fortran_intrinsic("double precision")
    'double precision'
    >>> check_fortran_intrinsic("double   precision")
    'double   precision'
    >>> check_fortran_intrinsic("doubleprecision")
    'doubleprecision'
    >>> check_fortran_intrinsic("char", error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: 'char' is not a valid Fortran type
    >>> check_fortran_intrinsic("int")

    >>> check_fortran_intrinsic("char", error=False)

    >>> check_fortran_intrinsic("type")

    >>> check_fortran_intrinsic("complex(kind=r8)")

    """
    match = typestr.strip().lower() in FORTRAN_INTRINSIC_TYPES
    if (not match) and (typestr.lower()[0:6] == 'double'):
        # Special case for double precision
        match = FORTRAN_DP_RE.match(typestr.strip()) is not None
    # End if
    if not match:
        if error:
            raise ValueError("'{}' is not a valid Fortran type".format(typestr))
        else:
            typestr = None
        # End if
    # End if
    return typestr

########################################################################

def check_fortran_type(typestr, error=False):
    """Return <typestr> if a valid Fortran type, otherwise, None
    if <error> is True, raise an Exception if <typestr> is not valid.
    >>> check_fortran_type("real")
    'real'
    >>> check_fortran_type("integer")
    'integer'
    >>> check_fortran_type("InteGer")
    'InteGer'
    >>> check_fortran_type("character")
    'character'
    >>> check_fortran_type("double precision")
    'double precision'
    >>> check_fortran_type("double   precision")
    'double   precision'
    >>> check_fortran_type("doubleprecision")
    'doubleprecision'
    >>> check_fortran_type("complex")
    'complex'
    >>> check_fortran_type("char", error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: 'char' is not a valid Fortran type
    >>> check_fortran_type("int")

    >>> check_fortran_type("char", error=False)

    >>> check_fortran_type("type")

    >>> check_fortran_type("type", error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: 'char' is not a valid derived Fortran type
    >>> check_fortran_type("type(hi mom)", error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: 'type(hi mom)' is not a valid derived Fortran type
    """
    dt = ""
    match = check_fortran_intrinsic(typestr, error)
    if match is None:
        match = registered_fortran_ddt_name(typestr)
        dt = " derived"
    # End if
    if match is None:
        if error:
            raise ValueError("'{}' is not a valid{} Fortran type".format(typestr, dt))
        else:
            typestr = None
        # End if
    # End if
    return typestr

########################################################################

def registered_fortran_ddt_name(name):
    if name in _REGISTERED_FORTRAN_DDT_NAMES:
        return name
    else:
        return None

########################################################################

def register_fortran_ddt_name(name):
    if name not in _REGISTERED_FORTRAN_DDT_NAMES:
        _REGISTERED_FORTRAN_DDT_NAMES.append(name)

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
