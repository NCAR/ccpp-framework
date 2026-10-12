#!/usr/bin/env python3
#

"""Definitions to convert a conditional statement in the metadata, expressed in standard names,
into a Fortran conditional (used in an if statement), expressed in local names.
"""

import re

# Every Fortran token in a conditional that is NOT an operand: whitespace,
# parentheses, the comparison operators in both symbolic (==, /=, <=, >=, <, >)
# and F77 dotted (.lt., .le., .eq., .ge., .gt., .ne.) spellings, the logical
# literals (.true., .false.) and the logical operators (.eqv., .neqv., .not.,
# .and., .or., .xor.).  These are the delimiters the tokenizer below splits on.
# Multi-character operators are listed before their single-character prefixes
# so, e.g., '<=' is matched whole rather than as '<' followed by '='.
FORTRAN_CONDITIONAL_REGEX_WORDS = [' ', '(', ')', '==', '/=', '<=', '>=', '<', '>', '.eqv.', '.neqv.',
                                   '.true.', '.false.', '.lt.', '.le.', '.eq.', '.ge.', '.gt.', '.ne.',
                                   '.not.', '.and.', '.or.', '.xor.']
# Tokenizer for a Fortran conditional: re.findall() returns an ordered list of
# every operand and operator in the expression.  Each match is EITHER
#   [\w']+ -- one operand: a run of identifier characters (a standard name or a
#             number), with ' included so a quoted literal like 'active' stays a
#             single token instead of being split -- OR
#   one of the operator/delimiter strings from the list above, each passed
#   through re.escape() -- so its regex metacharacters (the parentheses and the
#   dots in .and./.eq./...) match literally -- and joined with '|'.
# The caller then walks the tokens and swaps each operand's standard name for
# its local name, leaving the operators untouched.
# Known limitation: decimal literals are not supported.  An operand is a run of
# word characters, so '1.5' tokenizes as '1' and '5' -- the '.' matches no token
# and is dropped.  Metadata conditionals compare standard names against integers
# or .true./.false., so this has not mattered in practice.
FORTRAN_CONDITIONAL_REGEX = re.compile(r"[\w']+|" + "|".join([re.escape(word) for word in FORTRAN_CONDITIONAL_REGEX_WORDS]))
