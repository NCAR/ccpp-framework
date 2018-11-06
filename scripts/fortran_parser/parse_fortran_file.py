#! /usr/bin/env python
"""
Tool to parse a Fortran file and return signature information
from metadata tables.
At the file level, we allow only PROGRAM blocks and MODULE blocks.
Subroutines, functions, or data are not supported outside a MODULE.
"""

import os.path
if __name__ == '__main__' and __package__ is None:
    import sys
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import re
import logging
from parse_tools import ParseContext
from parse_fortran import FNAME

comment_re = re.compile(r"!.*$")
program_re = re.compile(r"(?i)program\s+("+FNAME+r")")
endprogram_re = re.compile(r"(?i)program\s+("+FNAME+r")?")
continue_re = re.compile(r"(?i)&\s*(!.*)?$")
blank_re = re.compile(r"\s+")

def scan_line(line, in_continue, in_single_char, in_double_char):
    """Scan a line for continue indicators, continued quotes, and comments
    Return continue_in_col, continue_out_col, in_single_char, in_double_char,
           comment_col
    >>> scan_line("! Comment line", False, False, False)
    (-1, -1, False, False, 0)
    >>> scan_line("int :: index", False, False, False)
    (-1, -1, False, False, -1)
    >>> scan_line("int :: inde& ! oops", False, False, False)
    (-1, 11, False, False, 13)
    >>> scan_line("character(len=*), parameter :: foo = 'This line & not continued'", False, False, False)
    (-1, -1, False, False, -1)
    """

    continue_in_col = -1
    continue_out_col = -1
    comment_col = -1

    index = 0
    last_ind = len(line) - 1
    while index <= last_ind:
        blank = blank_re.match(line[index:])
        if blank is not None:
            index = index + len(blank.group(0)) - 1 # +1 at end of loop
        elif in_single_char:
            if line[index:min(index+1,last_ind)] == "''":
                # Embedded single quote
                index = index + 1 # +1 and end of loop
            elif line[index] == "'":
                in_single_char = False
            elif (line[index] == '&'):
                if index == last_ind:
                    continue_out_col == index
                else:
                    blank = blank_re.match(line[index:])
                    if (blank is not None) and (len(blank.group(0)) == (last_ind - index + 1)):
                        continue_out_col == index
                    # End if
                # End if
            # End if (just ignore any other character)
        elif in_double_char:
            if line[index:min(index+1,last_ind)] == '""':
                # Embedded double quote
                index = index + 1 # +1 and end of loop
            elif line[index] == '"':
                in_double_char = False
            elif (line[index] == '&'):
                if index == last_ind:
                    continue_out_col == index
                else:
                    blank = blank_re.match(line[index:])
                    if (blank is not None) and (len(blank.group(0)) == (last_ind - index + 1)):
                        continue_out_col == index
                    # End if
                # End if
            # End if (just ignore any other character)
        # End if
        index = index + 1
    # End while

    return continue_in_col, continue_out_col, in_single_char, in_double_char, comment_col

def read_file(filename, preproc_defs=None):
    """Read a file into an array of lines.
    Preprocess lines to consolidate continuation lines.
    Remove preprocessor directives and code eliminated by #if statements
    Remvoved code results in blank lines, not removed lines
    """
    if not os.path.exists(filename):
        raise IOError("read_file: file, '{}', does not exist".format(filename))
    else:
        # Read all lines of the file at once
        with (open(filename, 'r')) as file:
            file_lines = file.readlines()
            continue_col = -1
            in_single_char = False # Single quote character context
            in_double_char = False # Double quote character context
            for index in xrange(len(file_lines)):
                file_lines[index] = file_lines[index].rstrip('\n').strip()
                # Skip empty lines and comment-only lines
                if file_lines[index] == '' or (file_lines[index][0] == '!'):
                    continue
                # End if
                # Note if this line has a continuation and strip if true
                curr_continue = continue_re.search(file_lines[index])
                # If in a continuation context, move this line to previos
                if continue_col >= 0:
                    pass
                # Remove line continuations: concatenate with following lines
                if line.endswith('&'):
                    buffer += file_lines[i].rstrip('\n').replace('&', ' ')
                    continue
                # Write out line with buffer and reset buffer
                lines.append(buffer + file_lines[i].rstrip('\n').replace('&', ' '))
                buffer = ''
            # End for
        # End with
        return file_lines


def parse_program(lines, line_start, filename):
    pass

def parse_fortran_file(filename):
    pass

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
