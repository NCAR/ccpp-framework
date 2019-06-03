#!/usr/bin/env python

"""A pilot version to perform unit conversions. Each conversion must be representable
as a formula where {var} is substituted by the actual variable (scalar, array) to convert,
and {kind} by either _ followed by the kind of the variable, or an emptry string.
This allows having formulars such as func({var}) where func is defined as an elemental
function or as an interface to scalar and array-based functions that perform more
complex conversions than the ones listed here. It is also possible, but in some cases
less performant, to construct conversions for composed units by combining some of the
basic conversions listed here. For instance, one could create a speed conversion from
km h-1 to m s-1 by combining the formulas for km to m and h to min, which will be
slower than boiling it down to a single mathematical expression (see example below)."""

############
# Length   #
############

def mm__to__m():
    """Convert millimeter to meter"""
    return '1.0E-3{kind}*{var}'

def m__to__mm():
    """Convert meter to millimeter"""
    return '1.0E+3{kind}*{var}'

def m__to__km():
    """Convert meter to kilometer"""
    return '1.0E-3{kind}*{var}'

def km__to__m():
    """Convert kilometer to meter"""
    return '1.0E+3{kind}*{var}'

def mm__to__km():
    """Convert millimeter to kilometer"""
    return '1.0E-6{kind}*{var}'

def km__to__mm():
    """Convert kilometer to millimeter"""
    return '1.0E+6{kind}*{var}'

############
# Time     #
############

def s__to__min():
    """Convert second to minute"""
    return '{var}/6.0E+1{kind}'

def min__to__s():
    """Convert minute to second"""
    return '6.0E+1{kind}*{var}'

def s__to__h():
    """Convert second to hour"""
    return '{var}/3.6E+3{kind}'

def h__to__s():
    """Convert hour to second"""
    return '3.6E+3{kind}*{var}'

def h__to__d():
    """Convert hour to day"""
    return '{var}/2.4E+1{kind}'

def d__to__h():
    """Convert day to hour"""
    return '2.4E+1{kind}*{var}'

def s__to__d():
    """Convert second to day"""
    return '{var}/8.64E+4{kind}'

def d__to__s():
    """Convert day to second"""
    return '8.64E+4{kind}*{var}'

##################
# Composed units #
##################

def Pa__to__hPa():
    """Convert Pascal to Hectopascal"""
    return '1.0E-2{kind}*{var}'

def hPa__to__Pa():
    """Convert Hectopascal to Pascal"""
    return '1.0E+2{kind}*{var}'

def m_s_minus_1__to__km_h_minus_1():
    """Convert meter per second to kilometer per hour"""
    return '({0})/({1})'.format(m__to__km(),s__to__h()) + '*{var}'

def km_h_minus_1__to__m_s_minus_1():
    """Convert kilometer per hour to meter per second"""
    return '({0})/({1})'.format(km__to__m(),h__to__s()) + '*{var}'

def W_m_minus_2__to__erg_cm_minus_2_s_minus_1():
    """Convert Watt per square meter to erg per square centimeter and second"""
    return '1.0E+3{kind}*{var}'

def erg_cm_minus_2_s_minus_1__to__W_m_minus_2():
    """Convert erg per square centimeter and second to Watt per square meter"""
    return '1.0E-3{kind}*{var}'
