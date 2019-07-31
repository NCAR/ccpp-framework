#!/usr/bin/env python

"""Helper functions to validate parsed input"""

# Python library imports
import re
# CCPP framework imports
from .parse_source import CCPPError, ParseInternalError

########################################################################

def check_dimensions(test_val, prop_dict, error, max_len=0):
    """Return <test_val> if a valid dimensions list, otherwise, None
    If <max_len> > 0, each string in <test_val> must not be longer than
    <max_len>.
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_dimensions(["dim1", "dim2name"], None, False)
    ['dim1', 'dim2name']
    >>> check_dimensions([":", ":"], None, False)
    [':', ':']
    >>> check_dimensions([":", "dim2"], None, False)
    [':', 'dim2']
    >>> check_dimensions(["dim1", ":"], None, False)
    ['dim1', ':']
    >>> check_dimensions(["8", "::"], None, False)
    ['8', '::']
    >>> check_dimensions(['start1:end1', 'start2:end2'], None, False)
    ['start1:end1', 'start2:end2']
    >>> check_dimensions(['start1:', 'start2:end2'], None, False)
    ['start1:', 'start2:end2']
    >>> check_dimensions(["dim1", "dim2name"], None, False, max_len=5)

    >>> check_dimensions(["dim1", "dim2name"], None, True, max_len=5)
    Traceback (most recent call last):
    CCPPError: 'dim2name' is too long (> 5 chars)
    >>> check_dimensions("hi_mom", None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'hi_mom' is invalid; not a list
    """
    if not isinstance(test_val, list):
        if error:
            raise CCPPError("'{}' is invalid; not a list".format(test_val))
        else:
            test_val = None
        # End if
    else:
        for item in test_val:
            isplit = item.split(':')
            # Check for too many colons
            if (len(isplit) > 3):
                if error:
                    errmsg = "'{}' is an invalid dimension range"
                    raise CCPPError(errmsg.format(item))
                else:
                    test_val = None
                # End if
                break
            # End if
            # Check possible dim styles (a, a:b, a:, :b, :, ::, a:b:c, a::c)
            tdims = [x for x in isplit if len(x) > 0]
            for tdim in tdims:
                # Check numeric value first
                try:
                    valid = isinstance(int(tdim), int)
                except ValueError as ve:
                    # Not an integer, try a Fortran ID
                    valid = check_fortran_id(tdim, None,
                                             error, max_len=max_len) is not None
                # End try
                if not valid:
                    if error:
                        errmsg = "'{}' is an invalid dimension name"
                        raise CCPPError(errmsg.format(item))
                    else:
                        test_val = None
                    # End if
                    break
                # End if
            # End for
        # End for
    # End if
    return test_val

########################################################################

# CF_ID is a string representing the regular expression for CF Standard Names
CF_ID = r"(?i)[a-z][a-z0-9_]*"
__CFID_RE = re.compile(CF_ID+r"$")

def check_cf_standard_name(test_val, prop_dict, error):
    """Return <test_val> if a valid CF Standard Name, otherwise, None
    http://cfconventions.org/Data/cf-standard-names/docs/guidelines.html
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_cf_standard_name("hi_mom", None, False)
    'hi_mom'
    >>> check_cf_standard_name("hi mom", None, False)

    >>> check_cf_standard_name("hi mom", None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'hi_mom' is not a valid CF Standard Name
    >>> check_cf_standard_name("", None, False) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: CCPP Standard Name cannot be blank
    >>> check_cf_standard_name("_hi_mom", None, False)

    >>> check_cf_standard_name("2pac", None, False)

    >>> check_cf_standard_name("Agood4tranID", None, False)
    'agood4tranid'
    >>> check_cf_standard_name("agoodcfid", None, False)
    'agoodcfid'
    """
    if len(test_val) == 0:
        raise CCPPError("CCPP Standard Name cannot be blank")
    else:
        match = __CFID_RE.match(test_val)
    # End if
    if match is None:
        if error:
            errmsg = "'{}' is not a valid CCPP Standard Name"
            raise CCPPError(errmsg.format(test_val))
        else:
            test_val = None
        # End if
    else:
        test_val = test_val.lower()
    # End if
    return test_val

########################################################################

### Fortran-specific parsing helper variables and functions

########################################################################

# FORTRAN_ID is a string representing the regular expression for Fortran names
FORTRAN_ID = r"([A-Za-z][A-Za-z0-9_]*)"
__FID_RE = re.compile(FORTRAN_ID+r"$")
# Note that the scalar array reference expressions below are not really for
# scalar references because a colon can be a placeholder, unlike in Fortran code
__FORTRAN_AID = r"(?:[A-Za-z][A-Za-z0-9_]*)"
__FORT_DIM = r"(?:"+__FORTRAN_AID+r"|[:])"
__REPEAT_DIM = r"(?:,\s*"+__FORT_DIM+r"\s*)"
__FORTRAN_SCALAR_ARREF = r"[(]\s*("+__FORT_DIM+r"\s*"+__REPEAT_DIM+r"{0,6})[)]"
FORTRAN_SCALAR_REF = r"(?:"+FORTRAN_ID+r"\s*"+__FORTRAN_SCALAR_ARREF+r")"
FORTRAN_SCALAR_REF_RE = re.compile(FORTRAN_SCALAR_REF+r"$")
FORTRAN_INTRINSIC_TYPES = [ "integer", "real", "logical", "complex",
                            "double precision", "character" ]
FORTRAN_DP_RE = re.compile(r"(?i)double\s*precision")
FORTRAN_TYPE_RE = re.compile(r"(?i)type\s*\(\s*("+FORTRAN_ID+r")\s*\)")

_REGISTERED_FORTRAN_DDT_NAMES = list()

########################################################################

def check_fortran_id(test_val, prop_dict, error, max_len=0):
    """Return <test_val> if a valid Fortran identifier, otherwise, None
    If <max_len> > 0, <test_val> must not be longer than <max_len>.
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_fortran_id("hi_mom", None, False)
    'hi_mom'
    >>> check_fortran_id("hi_mom", None, False, max_len=5)

    >>> check_fortran_id("hi_mom", None, True, max_len=5) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'hi_mom' is too long (> 5 chars)
    >>> check_fortran_id("hi mom", None, False)

    >>> check_fortran_id("hi mom", None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'hi_mom' is not a valid Fortran identifier
    >>> check_fortran_id("", None, False)

    >>> check_fortran_id("_hi_mom", None, False)

    >>> check_fortran_id("2pac", None, False)

    >>> check_fortran_id("Agood4tranID", None, False)
    'Agood4tranID'
    """
    match = __FID_RE.match(test_val)
    if match is None:
        if error:
            raise CCPPError("'{}' is not a valid Fortran identifier".format(test_val))
        else:
            test_val = None
        # End if
    elif (max_len > 0) and (len(test_val) > max_len):
        if error:
            raise CCPPError("'{}' is too long (> {} chars)".format(test_val, max_len))
        else:
            test_val = None
        # End if
    # End if
    return test_val

########################################################################

def check_fortran_ref(test_val, prop_dict, error, max_len=0):
    """Return <test_val> if a valid simple Fortran variable reference,
    otherwise, None. A simple Fortran variable reference is defined as
    a scalar id or a scalar array reference.
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> FORTRAN_SCALAR_REF_RE.match("foo( bar, baz )").group(1)
    'foo'
    >>> FORTRAN_SCALAR_REF_RE.match("foo( bar, baz )").group(2)
    'bar, baz '
    >>> FORTRAN_SCALAR_REF_RE.match("foo( bar, baz )").group(2).split(',')[0].strip()
    'bar'
    >>> FORTRAN_SCALAR_REF_RE.match("foo( :, baz )").group(2).split(',')[0].strip()
    ':'
    >>> FORTRAN_SCALAR_REF_RE.match("foo( bar, baz )").group(2).split(',')[1].strip()
    'baz'
    >>> check_fortran_ref("hi_mom", None, False)
    'hi_mom'
    >>> check_fortran_ref("hi_mom", None, False, max_len=5)

    >>> check_fortran_ref("hi_mom", None, True, max_len=5) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'hi_mom' is too long (> 5 chars)
    >>> check_fortran_ref("hi mom", None, False)

    >>> check_fortran_ref("hi mom", None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'hi_mom' is not a valid Fortran identifier
    >>> check_fortran_ref("", None, False)

    >>> check_fortran_ref("_hi_mom", None, False)

    >>> check_fortran_ref("2pac", None, False)

    >>> check_fortran_ref("Agood4tranID", None, False)
    'Agood4tranID'
    >>> check_fortran_ref("foo(bar)", None, False)
    'foo(bar)'
    >>> check_fortran_ref("foo( bar, baz )", None, False)
    'foo( bar, baz )'
    >>> check_fortran_ref("foo( :, baz )", None, False)
    'foo( :, baz )'
    >>> check_fortran_ref("foo( bar, )", None, False)

    >>> check_fortran_ref("foo( bar, )", None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'foo( bar, )' is not a valid Fortran scalar reference
    >>> check_fortran_ref("foo()", None, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'foo()' is not a valid Fortran scalar reference
    >>> check_fortran_ref("foo(bar, bazz)", None, True, max_len=3) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'bazz' is too long (> 3 chars) in foo(bar, bazz)
    >>> check_fortran_ref("foo(barr, baz)", None, True, max_len=3) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'bazr' is too long (> 3 chars) in foo(barr, baz)
    >>> check_fortran_ref("fooo(bar, baz)", None, True, max_len=3) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'foo' is too long (> 3 chars) in fooo(bar, baz)
    """
    idval = check_fortran_id(test_val, prop_dict, False, max_len=max_len)
    if idval is None:
        match = FORTRAN_SCALAR_REF_RE.match(test_val)
        if match is None:
            if error:
                emsg = "'{}' is not a valid Fortran scalar reference"
                raise CCPPError(emsg.format(test_val))
            else:
                test_val = None
            # End if
        elif max_len > 0:
            tokens = test_val.strip().rstrip(')').split('(')
            tokens = [tokens[0].strip()] + [x.strip()
                                            for x in tokens[1].split(',')]
            for token in tokens:
                if len(token) > max_len:
                    if error:
                        emsg = "'{}' is too long (> {} chars) in {}"
                        raise CCPPError(emsg.format(token, max_len, test_val))
                    else:
                        test_val = None
                        break
                    # End if
                # End if
            # End for
        # End if
    # End if
    return test_val

########################################################################

def check_local_name(test_val, prop_dict, error, max_len=0):
    """Return <test_val> if a valid simple Fortran variable reference,
    or Fortran constant, otherwise, None.
    A simple Fortran variable reference is defined as a scalar id or a
    scalar array reference.
    A constant is only valid if <prop_dict> is not None, the 'protected'
    property is present and True, and the 'type' property matches the
    type of <test_val>.
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_local_name("hi_mom", None, error=False)
    'hi_mom'
    >>> check_local_name('122', {'protected':True,'type':'integer'}, error=False)
    '122'
    >>> check_local_name('122', None, error=False)

    >>> check_local_name('122', {}, error=False)

    >>> check_local_name('122', {'protected':False,'type':'integer'}, error=False)

    >>> check_local_name('122', {'protected':True,'type':'real'}, error=False)

    >>> check_local_name('-122.e4', {'protected':True,'type':'real'}, error=False)
    '-122.e4'
    >>> check_local_name('-122.', {'protected':True,'type':'real','kind':'kp'}, error=False)

    >>> check_local_name('-122._kp', {'protected':True,'type':'real','kind':'kp'}, error=False)
    '-122._kp'
    >>> check_local_name('q(:,:,index_of_water_vapor_specific_humidity)', {}, error=False)
    'q(:,:,index_of_water_vapor_specific_humidity)'
    """
    valid_val = None
    # First check for a constant
    if (prop_dict is not None) and ('protected' in prop_dict):
        protected = prop_dict['protected']
    else:
        protected = False
    # End if
    if (prop_dict is not None) and ('type' in prop_dict):
        vtype = prop_dict['type']
    else:
        vtype = ""
    # End if
    if (prop_dict is not None) and ('kind' in prop_dict):
        kind = prop_dict['kind']
    else:
        kind = ""
    # End if
    if protected and vtype and check_fortran_literal(test_val, vtype, kind):
        valid_val = test_val
    # End if
    if valid_val is None:
        valid_val = check_fortran_ref(test_val, prop_dict, error, max_len=max_len)
    # End if
    return valid_val


########################################################################

def check_fortran_intrinsic(typestr, error=False):
    """Return <test_val> if a valid Fortran intrinsic type, otherwise, None
    if <error> is True, raise an Exception if <test_val> is not valid.
    >>> check_fortran_intrinsic("real", error=False)
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
    CCPPError: 'char' is not a valid Fortran type
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
            raise CCPPError("'{}' is not a valid Fortran type".format(typestr))
        else:
            typestr = None
        # End if
    # End if
    return typestr

########################################################################

def check_fortran_type(typestr, prop_dict, error):
    """Return <typestr> if a valid Fortran type, otherwise, None
    if <error> is True, raise an Exception if <typestr> is not valid.
    >>> check_fortran_type("real", None, False)
    'real'
    >>> check_fortran_type("integer", None, False)
    'integer'
    >>> check_fortran_type("InteGer", None, False)
    'InteGer'
    >>> check_fortran_type("character", None, False)
    'character'
    >>> check_fortran_type("double precision", None, False)
    'double precision'
    >>> check_fortran_type("double   precision", None, False)
    'double   precision'
    >>> check_fortran_type("doubleprecision", None, False)
    'doubleprecision'
    >>> check_fortran_type("complex", None, False)
    'complex'
    >>> check_fortran_type("char", {}, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'char' is not a valid Fortran type
    >>> check_fortran_type("int", None, False)

    >>> check_fortran_type("char", {}, False)

    >>> check_fortran_type("type", None, False)

    >>> check_fortran_type("type", {}, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'type' is not a valid derived Fortran type
    >>> check_fortran_type("type(hi mom)", {}, True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'type(hi mom)' is not a valid derived Fortran type
    """
    dt = ""
    match = check_fortran_intrinsic(typestr, error=False)
    if match is None:
        match = registered_fortran_ddt_name(typestr)
        dt = " derived"
    # End if
    if match is None:
        if error:
            raise CCPPError("'{}' is not a valid{} Fortran type".format(typestr, dt))
        else:
            typestr = None
        # End if
    # End if
    return typestr

########################################################################

def check_fortran_literal(value, typestr, kind):
    """Return True iff <value> is a valid Fortran literal of type, <typestr>.
    Note: no attempt is made to handle the older D syntax for real literals.
    To promote clean coding, real values MUST have a decimal point, however,
       this check is not available for the complex type so we just require
       the two components to either both be integers or both be reals.
    If <kind> is not an empty string, it is required to be present (i.e., if
    <kind> == 'kind_phys', <value> should be of the form, 123.4_kind_phys)
    >>> check_fortran_literal("123", "integer", "")
    True
    >>> check_fortran_literal("123", "INTEGER", "")
    True
    >>> check_fortran_literal("-123", "integer", "")
    True
    >>> check_fortran_literal("+123", "integer", "")
    True
    >>> check_fortran_literal("+123", "integer", "kind_int")
    False
    >>> check_fortran_literal("+123_kind_int", "integer", "kind_int")
    True
    >>> check_fortran_literal("+123_int", "integer", "kind_int")
    False
    >>> check_fortran_literal("123", "real", "")
    False
    >>> check_fortran_literal("123.", "real", "")
    True
    >>> check_fortran_literal("123.45", "real", "kind_phys")
    False
    >>> check_fortran_literal("123.45_8", "real", "kind_phys")
    False
    >>> check_fortran_literal("123.45_kind_phys", "real", "kind_phys")
    True
    >>> check_fortran_literal("123", "double precision", "")
    False
    >>> check_fortran_literal("123.", "doubleprecision", "")
    True
    >>> check_fortran_literal("123.45", "double   precision", "kind_phys")
    False
    >>> check_fortran_literal("123.45_8", "doubleprecision", "kind_phys")
    False
    >>> check_fortran_literal("123.45_kp", "doubleprecision", "kp")
    True
    >>> check_fortran_literal("123", "logical", "")
    False
    >>> check_fortran_literal(".true.", "logical", "")
    True
    >>> check_fortran_literal(".false.", "logical", "")
    True
    >>> check_fortran_literal("T", "logical", "")
    False
    >>> check_fortran_literal("F", "logical", "")
    False
    >>> check_fortran_literal(".TRUE.", "logical", "kind_log")
    False
    >>> check_fortran_literal(".TRUE._kind_log", "logical", "kind_log")
    True
    >>> check_fortran_literal("(123.,456.)", "complex", "")
    True
    >>> check_fortran_literal("(123. , 456.)", "complex", "")
    True
    >>> check_fortran_literal("(123.,456", "complex", "")
    False
    >>> check_fortran_literal("(123. , 456.)", "complex", "kp")
    False
    >>> check_fortran_literal("(123._kp , 456)", "complex", "kp")
    False
    >>> check_fortran_literal("(123._kp , 456._kp)", "complex", "kp")
    True
    >>> check_fortran_literal("'hi mom'", "character", "")
    True
    >>> check_fortran_literal("'hi mom", "character", "")
    False
    >>> check_fortran_literal('"hi mom"', "character", "")
    True
    >>> check_fortran_literal('"hi""mom"', "character", "")
    True
    >>> check_fortran_literal('"hi" "mom"', "character", "")
    False
    >>> check_fortran_literal("'hi''there''mom'", "character", "")
    True
    >>> check_fortran_literal("'hi mom'", "character", "kc")
    False
    >>> check_fortran_literal("kc_'hi mom'", "character", "kc")
    True
    >>> check_fortran_literal("123._kp", "float", "kp") #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseInternalError: ERROR: 'float' is not a Fortran intrinsic type
    """
    valid = True
    if FORTRAN_DP_RE.match(typestr.strip()) is not None:
        vtype = 'real'
    else:
        vtype = typestr.lower()
    # End if
    # Check complex first
    if vtype == 'complex':
        cvals = value.strip().split(',')
        if len(cvals) == 2:
            tp = 'integer'
            if ('.' in cvals[0]) and ('.' in cvals[1]):
                tp = 'real'
            elif ('.' in cvals[0]) or ('.' in cvals[1]):
                valid = False
            # End if
            if (cvals[0][0] == '(') and (cvals[1][-1] == ')'):
                valid = valid and check_fortran_literal(cvals[0][1:], tp, kind)
                valid = valid and check_fortran_literal(cvals[1][:-1], tp, kind)
            else:
                valid = False
            # End if
        else:
            valid = False
    elif valid:
        vparts = value.strip().split('_')
        if vtype == 'character':
            if len(vparts) > 1:
                val = vparts[-1]
                vkind = '_'.join(vparts[0:-1])
            else:
                val = vparts[0]
                vkind = ''
            # End if
        else:
            val = vparts[0]
            if len(vparts) > 1:
                vkind = '_'.join(vparts[1:])
            else:
                vkind = ''
            # End if
        # End if
        if vkind != kind.lower():
            valid = False
        # End if, kind is okay, check value
        if valid and (vtype == 'integer'):
            try:
                vtest = int(val)
            except ValueError as ve:
                valid = False
            # End try
        elif valid and (vtype == 'real'):
            if '.' not in val:
                valid = False
            else:
                try:
                    vtest = float(val)
                except ValueError as ve:
                    valid = False
                # End try
            # End if
        elif valid and (vtype == 'logical'):
            valid = (val.upper() == '.TRUE.') or (val.upper() == '.FALSE.')
        elif valid and (vtype == 'character'):
            sep = val[0]
            cparts = val.split(sep)
            # We must have balanced delimiters
            if len(cparts)%2 == 0:
                valid = False
            else:
                for index in range(len(cparts)):
                    if (index%2 == 0) and (len(cparts[index]) > 0):
                        valid = False
                        break
                    # End if
                # End for
            # End if (else okay)
        elif valid:
            errmsg = "ERROR: '{}' is not a Fortran intrinsic type"
            raise ParseInternalError(errmsg.format(typestr))
        # End if (no else)
    # End if
    return valid


########################################################################

def check_balanced_paren(string, start=0, error=False):
    """Return <string> indices delineating a balance set of parentheses.
    Parentheses in character context do not count.
    Left parenthesis search begins at <start>.
    Return start and end indices if found
    If no parentheses are found, return (-1, -1).
    If a left parenthesis is found but no balancing right, return (begin, -1)
    where begin
    If error is True, raise a CCPPError.
    >>> check_balanced_paren("foo")
    (-1, -1)
    >>> check_balanced_paren("(foo, bar)")
    (0, 9)
    >>> check_balanced_paren("( (foo, bar) )", start=1)
    (2, 11)
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
        if (string[index] == '"') or (string[index] == "'"):
            if inchar == string[index]:
                inchar = None
            elif inchar is None:
                inchar = string[index]
            # else in character context, keep going
            # End if
        elif inchar is not None:
            # In character context, keep going
            pass
        elif string[index] == '(':
            if depth == 0:
                begin = index
            # End if
            depth = depth + 1
            if depth == 0:
                break
            # End if
        elif string[index] == ')':
            depth = depth - 1
            if depth == 0:
                end = index
                break
            # End if
        # else just keep going
        # End if
        index = index + 1
    # End while
    if (begin >= 0) and (end < 0) and error:
        raise CCPPError("ERROR: Unbalanced parenthesis in '{}'".format(string))
    # End if
    return begin, end

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
