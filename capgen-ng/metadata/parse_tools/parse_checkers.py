#!/usr/bin/env python3

"""Helper functions to validate parsed input."""

import re

from .parse_source import CCPPError

_UNITLESS_REGEX                = "1"
_NON_LEADING_ZERO_NUM          = r"[1-9]\d*"
_CHAR_WITH_UNDERSCORE          = "([a-zA-Z]+_[a-zA-Z]+)+"
_NEGATIVE_NON_LEADING_ZERO_NUM = f"[-]{_NON_LEADING_ZERO_NUM}"
_POSITIVE_NON_LEADING_ZERO_NUM = f"[+]{_NON_LEADING_ZERO_NUM}"
_UNIT_EXPONENT                 = f"({_NEGATIVE_NON_LEADING_ZERO_NUM}|{_POSITIVE_NON_LEADING_ZERO_NUM}|{_NON_LEADING_ZERO_NUM})"
_UNIT_REGEX                    = f"[a-zA-Z]+{_UNIT_EXPONENT}?"
_UNITS_REGEX                   = rf"^({_CHAR_WITH_UNDERSCORE}|{_UNIT_REGEX}(\s{_UNIT_REGEX})*|{_UNITLESS_REGEX})$"
_UNITS_RE                      = re.compile(_UNITS_REGEX)
_MAX_MOLAR_MASS                = 10000.0


def check_units(test_val, prop_dict, error):
    """Return <test_val> if a valid unit, otherwise, None
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_units('m s-1', None, True)
    'm s-1'
    >>> check_units('kg m-3', None, True)
    'kg m-3'
    >>> check_units('m2 s-2', None, True)
    'm2 s-2'
    >>> check_units('m+2 s-2', None, True)
    'm+2 s-2'
    >>> check_units('1', None, True)
    '1'
    >>> check_units('', None, False)

    >>> check_units('', None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: '' is not a valid unit
    >>> check_units(['foo'], None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: ['foo'] is invalid; not a string
    """
    if isinstance(test_val, str):
        if _UNITS_RE.match(test_val.strip()) is None:
            if error:
                raise CCPPError("'{}' is not a valid unit".format(test_val))
            test_val = None
    else:
        if error:
            raise CCPPError("'{}' is invalid; not a string".format(test_val))
        test_val = None
    return test_val


def check_dimensions(test_val, prop_dict, error, max_len=0):
    """Return <test_val> if a valid dimensions list, otherwise, None
    If <max_len> > 0, each string in <test_val> must not be longer than
    <max_len>.
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_dimensions(["dim1", "dim2name"], None, False)
    ['dim1', 'dim2name']
    >>> check_dimensions([":", ":"], None, False)
    [':', ':']
    >>> check_dimensions(["8", "::"], None, False)
    ['8', '::']
    >>> check_dimensions(['start1:end1', 'start2:end2'], None, False)
    ['start1:end1', 'start2:end2']
    >>> check_dimensions(['size(foo)'], None, False)
    ['size(foo)']
    >>> check_dimensions(['size(foo,1'], None, False) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Invalid dimension component, size(foo,1
    >>> check_dimensions(["dim1", "dim2name"], None, True, max_len=5) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'dim2name' is too long (> 5 chars)
    >>> check_dimensions("hi_mom", None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'hi_mom' is invalid; not a list
    >>> check_dimensions(["ccpp_constant_one:1", "dim2name"], None, True)
    ['ccpp_constant_one:1', 'dim2name']
    """
    if not isinstance(test_val, list):
        if error:
            raise CCPPError("'{}' is invalid; not a list".format(test_val))
        return None
    for item in test_val:
        isplit = item.split(':')
        if len(isplit) > 3:
            if error:
                raise CCPPError("'{}' is an invalid dimension range".format(item))
            return None
        # Integer literals are valid in any bound position; semantic
        # restrictions (e.g. horizontal_dimension lower bound must be 1)
        # are enforced by the resolver, not here.
        tdims = [x.strip() for x in isplit if len(x) > 0]
        for tdim in tdims:
            try:
                int(tdim)
                valid = True
            except ValueError:
                valid = check_fortran_id(tdim, None, error,
                                         max_len=max_len) is not None
                if not valid and tdim.strip().lower()[0:4] == 'size':
                    if -1 in check_balanced_paren(tdim[4:]):
                        raise CCPPError(
                            'Invalid dimension component, {}'.format(tdim))
                    valid = True
            if not valid:
                if error:
                    raise CCPPError(f"'{item}' is an invalid dimension name")
                return None
    return test_val


CF_ID = r"(?i)[a-z][a-z0-9_]*"
__CFID_RE = re.compile(CF_ID + r"$")


def check_cf_standard_name(test_val, prop_dict, error):
    """Return <test_val> if a valid CF Standard Name, otherwise, None.
    http://cfconventions.org/Data/cf-standard-names/docs/guidelines.html
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_cf_standard_name("hi_mom", None, False)
    'hi_mom'
    >>> check_cf_standard_name("hi mom", None, False)

    >>> check_cf_standard_name("", None, False) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: CCPP Standard Name cannot be blank
    >>> check_cf_standard_name("Agood4tranID", None, False)
    'agood4tranid'
    """
    if len(test_val) == 0:
        raise CCPPError("CCPP Standard Name cannot be blank")
    if __CFID_RE.match(test_val) is None:
        if error:
            raise CCPPError(
                "'{}' is not a valid CCPP Standard Name".format(test_val))
        return None
    return test_val.lower()


FORTRAN_ID = r"([A-Za-z][A-Za-z0-9_]*)"
__FID_RE = re.compile(FORTRAN_ID + r"$")
# Scalar array-reference pattern below allows `:` placeholders, unlike
# real Fortran code, so the same regex covers both scalar refs and
# slice descriptors in metadata.
__FORTRAN_AID = r"(?:[A-Za-z][A-Za-z0-9_]*)"
__FORT_INT = r"[0-9]+"
__FORT_DIM = r"(?:" + __FORTRAN_AID + r"|[:]|" + __FORT_INT + r")"
__REPEAT_DIM = r"(?:,\s*" + __FORT_DIM + r"\s*)"
__FORTRAN_SCALAR_ARREF = r"[(]\s*(" + __FORT_DIM + r"\s*" + __REPEAT_DIM + r"{0,6})[)]"
FORTRAN_SCALAR_REF_RE = re.compile(
    r"(?:" + FORTRAN_ID + r"\s*" + __FORTRAN_SCALAR_ARREF + r")$")
FORTRAN_INTRINSIC_TYPES = ["integer", "real", "logical", "complex",
                           "double precision", "character"]
FORTRAN_DP_RE = re.compile(r"(?i)double\s*precision")

_REGISTERED_FORTRAN_DDT_NAMES = ["ccpp_constituent_prop_ptr_t"]


def check_fortran_id(test_val, prop_dict, error, max_len=0):
    """Return <test_val> if a valid Fortran identifier, otherwise, None
    If <max_len> > 0, <test_val> must not be longer than <max_len>.
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_fortran_id("hi_mom", None, False)
    'hi_mom'
    >>> check_fortran_id("hi_mom", None, True, max_len=5) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'hi_mom' is too long (> 5 chars)
    >>> check_fortran_id("hi mom", None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'hi_mom' is not a valid Fortran identifier
    >>> check_fortran_id("2pac", None, False)

    >>> check_fortran_id("Agood4tranID", None, False)
    'Agood4tranID'
    """
    if __FID_RE.match(test_val) is None:
        if error:
            raise CCPPError(
                "'{}' is not a valid Fortran identifier".format(test_val))
        return None
    if max_len > 0 and len(test_val) > max_len:
        if error:
            raise CCPPError(
                "'{}' is too long (> {} chars)".format(test_val, max_len))
        return None
    return test_val


def check_fortran_ref(test_val, prop_dict, error, max_len=0):
    """Return <test_val> if a valid simple Fortran variable reference,
    otherwise, None. A simple Fortran variable reference is defined as
    a scalar id or a scalar array reference.
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_fortran_ref("hi_mom", None, False)
    'hi_mom'
    >>> check_fortran_ref("hi mom", None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'hi_mom' is not a valid Fortran identifier
    >>> check_fortran_ref("foo(bar)", None, False)
    'foo(bar)'
    >>> check_fortran_ref("foo( bar, baz )", None, False)
    'foo( bar, baz )'
    >>> check_fortran_ref("foo( :, baz )", None, False)
    'foo( :, baz )'
    >>> check_fortran_ref("foo( bar, )", None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'foo( bar, )' is not a valid Fortran scalar reference
    >>> check_fortran_ref("foo(bar, bazz)", None, True, max_len=3) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'bazz' is too long (> 3 chars) in foo(bar, bazz)
    """
    idval = check_fortran_id(test_val, prop_dict, False, max_len=max_len)
    if idval is not None:
        return test_val
    if FORTRAN_SCALAR_REF_RE.match(test_val) is None:
        if error:
            raise CCPPError(
                "'{}' is not a valid Fortran scalar reference".format(test_val))
        return None
    if max_len > 0:
        tokens = test_val.strip().rstrip(')').split('(')
        tokens = [tokens[0].strip()] + [x.strip() for x in tokens[1].split(',')]
        for token in tokens:
            if len(token) > max_len:
                if error:
                    raise CCPPError(
                        "'{}' is too long (> {} chars) in {}".format(
                            token, max_len, test_val))
                return None
    return test_val


def check_fortran_intrinsic(typestr, error=False):
    """Return <typestr> if a valid Fortran intrinsic type, otherwise, None
    if <error> is True, raise an Exception if <typestr> is not valid.
    >>> check_fortran_intrinsic("real", error=False)
    'real'
    >>> check_fortran_intrinsic("InteGer")
    'InteGer'
    >>> check_fortran_intrinsic("double precision")
    'double precision'
    >>> check_fortran_intrinsic("doubleprecision")
    'doubleprecision'
    >>> check_fortran_intrinsic("char", error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'char' is not a valid Fortran type
    >>> check_fortran_intrinsic("complex(kind=r8)")

    """
    chk_type = typestr.strip().lower()
    match = chk_type in FORTRAN_INTRINSIC_TYPES
    if not match and chk_type[0:6] == 'double':
        match = FORTRAN_DP_RE.match(chk_type) is not None
    if not match:
        if error:
            raise CCPPError("'{}' is not a valid Fortran type".format(typestr))
        return None
    return typestr


def check_fortran_type(typestr, prop_dict, error):
    """Return <typestr> if a valid Fortran type, otherwise, None
    if <error> is True, raise an Exception if <typestr> is not valid.
    >>> check_fortran_type("real", None, False)
    'real'
    >>> check_fortran_type("char", {}, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'char' is not a valid Fortran type
    >>> check_fortran_type("type", {}, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'type' is not a valid derived Fortran type
    """
    dt = ""
    match = check_fortran_intrinsic(typestr, error=False)
    if match is None:
        match = registered_fortran_ddt_name(typestr)
        dt = " derived"
    if match is None:
        if error:
            raise CCPPError(
                "'{}' is not a valid{} Fortran type".format(typestr, dt))
        return None
    return typestr


def check_diagnostic_fixed(test_val, prop_dict, error):
    """Return <test_val> if a valid descriptor for a CCPP diagnostic,
         otherwise, None.
    If <error> is True, raise an Exception if <value> is not valid.
    A fixed diagnostic name is any Fortran identifier, however, it is
    an error to specify both 'diagnostic_name' and 'diagnostic_name_fixed'.
    >>> check_diagnostic_fixed("foo", {'diagnostic_name_fixed' : 'foo'}, False)
    'foo'
    >>> check_diagnostic_fixed("foo", {'diagnostic_name':'foo','local_name':'hi','standard_name':'mom'}, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: hi (mom) cannot have both 'diagnostic_name' and 'diagnostic_name_fixed' attributes
    >>> check_diagnostic_fixed("2foo", {'diagnostic_name_fixed':'foo','local_name':'hi','standard_name':'mom'}, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: '2foo' (hi) is not a valid fixed diagnostic name
    """
    if (prop_dict and ('diagnostic_name' in prop_dict) and
        prop_dict['diagnostic_name']):
        if error:
            lname = prop_dict.get('local_name', 'UNKNOWN')
            sname = prop_dict.get('standard_name', 'UNKNOWN')
            raise CCPPError(
                "{} ({}) cannot have both 'diagnostic_name' and "
                "'diagnostic_name_fixed' attributes".format(lname, sname))
        return None
    if check_fortran_id(test_val, prop_dict, False) is None:
        if error:
            lname = prop_dict.get('local_name', 'UNKNOWN')
            raise CCPPError(
                "'{}' ({}) is not a valid fixed diagnostic name".format(
                    test_val, lname))
        return None
    return test_val


_DIAG_PRE = r"(" + FORTRAN_ID + ")?"
_DIAG_SUFF = r"([_0-9A-Za-z]+)?"
_DIAG_PROP = r"((\${process}|\${scheme_name})" + _DIAG_SUFF + r")"
_DIAG_RE = re.compile(_DIAG_PRE + _DIAG_PROP + r"?$")


def check_diagnostic_id(test_val, prop_dict, error):
    """Return <test_val> if a valid descriptor for a CCPP diagnostic,
        otherwise, None.
    If <error> is True, raise an Exception if <value> is not valid.
    A diagnostic name is a Fortran identifier with the optional
       addition of one variable substitution.
    A variable substitution is a substring of the form of either:
       ${process}: The scheme process name will be substituted for this
          substring. If this substring is included, it is an error for
          there to be no process specified by the scheme (although this
          error cannot be detected by this routine).
       ${scheme_name}: The scheme name will be substituted for this substring.
    It is an error to specify both 'diagnostic_name' and
       'diagnostic_name_fixed'.
    >>> check_diagnostic_id("foo", {'diagnostic_name' : 'foo'}, False)
    'foo'
    >>> check_diagnostic_id("foo_${process}", {}, False)
    'foo_${process}'
    >>> check_diagnostic_id("foo_${scheme_name}_2bad", {}, False)
    'foo_${scheme_name}_2bad'
    >>> check_diagnostic_id("pref_${scheme}_suff", {}, False)

    >>> check_diagnostic_id("foo", {'diagnostic_name_fixed':'foo','local_name':'hi','standard_name':'mom'}, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: hi (mom) cannot have both 'diagnostic_name' and 'diagnostic_name_fixed' attributes
    """
    if (prop_dict and ('diagnostic_name_fixed' in prop_dict) and
        prop_dict['diagnostic_name_fixed']):
        if error:
            lname = prop_dict.get('local_name', 'UNKNOWN')
            sname = prop_dict.get('standard_name', 'UNKNOWN')
            raise CCPPError(
                "{} ({}) cannot have both 'diagnostic_name' and "
                "'diagnostic_name_fixed' attributes".format(lname, sname))
        return None
    if _DIAG_RE.match(test_val) is None:
        if error:
            raise CCPPError(
                "'{}' is not a valid diagnostic_name value".format(test_val))
        return None
    return test_val


def check_molar_mass(test_val, prop_dict, error):
    """Return <test_val> if valid molar mass, otherwise, None
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_molar_mass('1', None, True)
    1.0
    >>> check_molar_mass('1.0', None, False)
    1.0
    >>> check_molar_mass('-1', None, False)

    >>> check_molar_mass('-1', None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: '-1' is not a valid molar mass
    >>> check_molar_mass(10001, None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: '10001' is not a valid molar mass
    """
    try:
        test_val = float(test_val)
    except (TypeError, ValueError):
        if error:
            raise CCPPError(f"{test_val} is invalid; not a float or int")
        return None
    if test_val < 0.0 or test_val > _MAX_MOLAR_MASS:
        if error:
            raise CCPPError(f"{test_val} is not a valid molar mass")
        return None
    return test_val


def check_balanced_paren(string, start=0, error=False):
    """Return <string> indices delineating a balance set of parentheses.
    Parentheses in character context do not count.
    Left parenthesis search begins at <start>.
    Return start and end indices if found
    If no parentheses are found, return (-1, -1).
    If a left parenthesis is found but no balancing right, return (begin, -1)
    where begin is the index where the left parenthesis was found.
    If error is True, raise a CCPPError.
    >>> check_balanced_paren("foo")
    (-1, -1)
    >>> check_balanced_paren("(foo, bar)")
    (0, 9)
    >>> check_balanced_paren("(size(foo,1), qux)")
    (0, 17)
    >>> check_balanced_paren("(foo('bar()'))")
    (0, 13)
    >>> check_balanced_paren("(foo('bar()')")
    (0, -1)
    >>> check_balanced_paren("(foo('bar()')", error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: ERROR: Unbalanced parenthesis in '(foo('bar()')'
    """
    index = start
    begin = -1
    end = -1
    depth = 0
    inchar = None
    str_len = len(string)
    while index < str_len:
        c = string[index]
        if c in ('"', "'"):
            if inchar == c:
                inchar = None
            elif inchar is None:
                inchar = c
        elif inchar is not None:
            pass
        elif c == '(':
            if depth == 0:
                begin = index
            depth += 1
        elif c == ')':
            depth -= 1
            if depth == 0:
                end = index
                break
        index += 1
    if begin >= 0 and end < 0 and error:
        raise CCPPError("ERROR: Unbalanced parenthesis in '{}'".format(string))
    return begin, end


def registered_fortran_ddt_name(name):
    if name in _REGISTERED_FORTRAN_DDT_NAMES:
        return name
    return None
