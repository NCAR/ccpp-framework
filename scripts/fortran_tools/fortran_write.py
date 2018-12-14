#!/usr/bin/env python
#

"""Code to write Fortran code
"""

# Python library imports
from __future__ import print_function
# CCPP framework imports

###############################################################################
# Module (global) variables
###############################################################################

INDENT    =   3 # Spaces per indent level
CONTINUE  =   5 # Extra spaces on continuation line
LINE_FILL =  78 # Target line length
LINE_MAX  = 130 # Max line length

###############################################################################

def indent(level=0, continue_line=False):
    'Return an indent string for any level'
    if continue_line:
        return ((INDENT*level)+CONTINUE)*' '
    else:
        return (INDENT*level)*' '
    # End if

###############################################################################

def find_best_break(choices, last=LINE_FILL):
    'Find largest good break'
    possible = [x for x in choices if x < last]
    if len(possible) == 0:
        best = LINE_MAX + 1
    else:
        best = max(possible)
    # End if
    if (best > LINE_MAX) and (last < LINE_MAX):
        best = find_best_break(choices, last=LINE_MAX)
    # End if
    return best

###############################################################################

def write_fortran(outfile, statement, indent_level, continue_line=False):
    istr = indent(indent_level, continue_line)
    outstr = istr + statement.strip()
    line_len = len(outstr)
    if line_len > LINE_FILL:
        # Collect pretty break points
        spaces = list()
        commas = list()
        sptr = len(istr)
        in_single_char = False
        in_double_char = False
        while sptr < line_len:
            if in_single_char:
                if outstr[sptr] == "'":
                    in_single_char = False
                # End if (no else, just copy stuff in string)
            elif in_double_char:
                if outstr[sptr] == '"':
                    in_double_char = False
                # End if (no else, just copy stuff in string)
            elif outstr[sptr] == "'":
                in_single_char = True
            elif outstr[sptr] == '"':
                in_double_char = True
            elif outstr[sptr] == '!':
                # Commend in non-character context, suck in rest of line
                sptr = line_len - 1
            elif outstr[sptr] == ' ':
                # Non-quote spaces are where we can break
                spaces.append(sptr)
            elif outstr[sptr] == ',':
                # Non-quote commas are where we can break
                commas.append(sptr)
            # End if (no else, other characters will be ignored)
            sptr = sptr + 1
        # End while
        best = find_best_break(spaces)
        if best >= LINE_FILL:
            best = find_best_break(commas)
        # End if
        outfile.write("{}{}&\n".format(outstr[0:best+1], (LINE_FILL-best)*' '))
        statement = outstr[best+1:]
        write_fortran(outfile, statement, indent_level, continue_line=True)
    else:
        outfile.write("{}\n".format(outstr))
    # End if

###############################################################################
if __name__ == "__main__":
    from parse_tools import initLog, setLogToNull
    logger = initLog('ccpp_suite')
    setLogToNull(logger)
    # First, run doctest
    import doctest
    doctest.testmod()
    # Put some other tests here
# No else
