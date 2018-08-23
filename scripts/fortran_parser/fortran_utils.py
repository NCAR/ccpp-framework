#!/usr/bin/env python

import re

fname = re.compile(r"^[A-Za-z][A-Za-z0-9_]*$")

def is_variable_name(input):
    """Return True iff input is a valid Fortran variable name
    >>> is_variable_name('V_123')
    True
    >>> is_variable_name('v_v_a2')
    True
    >>> is_variable_name('i')
    True
    >>> is_variable_name('')
    False
    >>> is_variable_name('_hi_mom')
    False
    >>> is_variable_name('2i')
    False
    """
    return fname.match(input) is not None

########################################################################


if __name__ == "__main__":
    import doctest
    doctest.testmod()
