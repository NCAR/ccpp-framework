#!/usr/bin/env python3
#

"""Definitions to convert a conditional statement in the metadata, expressed in standard names,
into a Fortran conditional (used in an if statement), expressed in local names.
"""

import re

FORTRAN_CONDITIONAL_REGEX_WORDS = [' ', '(', ')', '==', '/=', '<=', '>=', '<', '>', '.eqv.', '.neqv.',
                                   '.true.', '.false.', '.lt.', '.le.', '.eq.', '.ge.', '.gt.', '.ne.',
                                   '.not.', '.and.', '.or.', '.xor.']
FORTRAN_CONDITIONAL_REGEX = re.compile(r"[\w']+|" + "|".join([word.replace('(','\(').replace(')', '\)') for word in FORTRAN_CONDITIONAL_REGEX_WORDS]))
