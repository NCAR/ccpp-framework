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
from parse_tools import ParseContext, ParseInternalError, ParseSyntaxError
from parse_tools import ParseObject, FORTRAN_ID

comment_re = re.compile(r"!.*$")
program_re = re.compile(r"(?i)\s*program\s+("+FORTRAN_ID+r")")
endprogram_re = re.compile(r"(?i)\s*end\s*program\s+("+FORTRAN_ID+r")?")
module_re = re.compile(r"(?i)\s*module\s+("+FORTRAN_ID+r")")
endmodule_re = re.compile(r"(?i)\s*end\s*module\s+("+FORTRAN_ID+r")?")
continue_re = re.compile(r"(?i)&\s*(!.*)?$")
blank_re = re.compile(r"\s+")

########################################################################

def line_statements(line):
    """Break up line into a list of component Fortran statements
    Note, because this is a simple script, we can cheat on the
    interpretation of two consecutive quote marks.
    >>> line_statements('integer :: i, j')
    ['integer :: i, j']
    >>> line_statements('integer :: i; real :: j')
    ['integer :: i', ' real :: j']
    >>> line_statements('integer :: i ! Do not break; here')
    ['integer :: i ! Do not break; here']
    >>> line_statements("write(6, *) 'This is all one statement; y''all;'")
    ["write(6, *) 'This is all one statement; y''all;'"]
    >>> line_statements('write(6, *) "This is all one statement; y""all;"')
    ['write(6, *) "This is all one statement; y""all;"']
    """
    statements = list()
    ind_start = 0
    ind_end = 0
    line_len = len(line)
    in_single_char = False
    in_double_char = False
    while ind_end < line_len:
        if in_single_char:
            if line[ind_end] == "'":
                in_single_char = False
            # End if (no else, just copy stuff in string)
        elif in_double_char:
            if line[ind_end] == '"':
                in_double_char = False
            # End if (no else, just copy stuff in string)
        elif line[ind_end] == "'":
            in_single_char = True
        elif line[ind_end] == '"':
            in_double_char = True
        elif line[ind_end] == '!':
            # Commend in non-character context, suck in rest of line
            ind_end = line_len - 1
        elif line[ind_end] == ';':
            # The whole reason for this routine, the statement separator
            if ind_end > ind_start:
                statements.append(line[ind_start:ind_end])
            # End if
            ind_start = ind_end + 1
            ind_end = ind_start - 1
        # End if (no else, other characters will be copied)
        ind_end = ind_end + 1
    # End while
    # Cleanup
    if ind_end > ind_start:
        statements.append(line[ind_start:ind_end])
    # End if
    return statements


########################################################################

def scan_line(line, in_continue, in_single_char, in_double_char, context):
    """Scan a line for continue indicators, continued quotes, and comments
    Return continue_in_col, continue_out_col, in_single_char, in_double_char,
           comment_col
    >>> scan_line("! Comment line", False, False, False, ParseContext())
    (-1, -1, False, False, 0)
    >>> scan_line("int :: index", False, False, False, ParseContext())
    (-1, -1, False, False, -1)
    >>> scan_line("int :: inde& ! oops", False, False, False, ParseContext())
    (-1, 11, False, False, 13)
    >>> scan_line("int :: inde&", False, False, False, ParseContext())
    (-1, 11, False, False, -1)
    >>> scan_line("character(len=*), parameter :: foo = 'This line & not continued'", False, False, False, ParseContext())
    (-1, -1, False, False, -1)
    >>> scan_line("character(len=*), parameter :: foo = 'This is continue line& ", False, False, False, ParseContext())
    (-1, 59, True, False, -1)
    >>> scan_line('character(len=*), parameter :: foo = "This line & not continued"', False, False, False, ParseContext())
    (-1, -1, False, False, -1)
    >>> scan_line('character(len=*), parameter :: foo = "This is continue line& ', False, False, False, ParseContext())
    (-1, 59, False, True, -1)
    >>> scan_line('  & line continued"', True, False, True, ParseContext())
    (2, -1, False, False, -1)
    >>> scan_line('  & line continued"', True, True, False, ParseContext()) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Cannot end non-continued line in a character context, at <standard input>:1
    >>> scan_line("  & line continued'", True, False, True, ParseContext()) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Cannot end non-continued line in a character context, at <standard input>:1
    >>> scan_line("int :: inde&", False, True, False, ParseContext())
    Traceback (most recent call last):
    ParseSyntaxError: Cannot start line in character context if not a continued line, at <standard input>:1
    >>> scan_line("int :: inde&", True, True, True, ParseContext())
    Traceback (most recent call last):
    ParseSyntaxError: Cannot be both in an apostrophe character context and a quote character context, at <standard input>:1
    """

    # A few sanity checks
    if (in_single_char or in_double_char) and (not in_continue):
        raise ParseSyntaxError("Cannot start line in character context if not a continued line", context=context)
    # Endif
    if in_single_char and in_double_char:
        raise ParseSyntaxError("Cannot be both in an apostrophe character context and a quote character context", context=context)

    continue_in_col = -1
    continue_out_col = -1
    comment_col = -1

    index = 0
    last_ind = len(line.rstrip()) - 1
    # Is first non-blank character a continue character?
    if line.lstrip()[0] == '&':
        if not in_continue:
            raise ParseSyntaxError("Cannot begin line with continue character (&), not on continued line", context=context)
        else:
            continue_in_col = line.find('&')
            index = continue_in_col + 1
        # End if
    # Process rest of line
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
                    continue_out_col = index
                # End if
            # End if (just ignore any other character)
        elif in_double_char:
            if line[index:min(index+1,last_ind)] == '""':
                # Embedded double quote
                index = index + 1 # +1 and end of loop
            elif line[index] == '"':
                in_double_char = False
            elif line[index] == '&':
                if index == last_ind:
                    continue_out_col = index
                # End if
            # End if (just ignore any other character)
        elif line[index] == "'":
            # If we got here, we are not in a character context, start single
            in_single_char = True
        elif line[index] == '"':
            # If we got here, we are not in a character context, start double
            in_double_char = True
        elif line[index] == '!':
            # If we got here, we are not in a character context, done with line
            comment_col = index
            index = last_ind
        elif line[index] == '&':
            # If we got here, we are not in a character context, note continue
            # First make sure this is a valid continue
            match = continue_re.match(line[index:])
            if match is not None:
                continue_out_col = index
            else:
                raise ParseSyntaxError("Invalid continue, ampersand not followed by comment character", context=context)
            # End if
        index = index + 1
    # End while
    # A final check
    if (in_single_char or in_double_char) and (continue_out_col < 0):
        raise ParseSyntaxError("Cannot end non-continued line in a character context", context=context)

    return continue_in_col, continue_out_col, in_single_char, in_double_char, comment_col

########################################################################

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
        with open(filename, 'r') as file:
            file_lines = file.readlines()
            for index in xrange(len(file_lines)):
                file_lines[index] = file_lines[index].rstrip('\n')
            # End for
        # End with
        # create a parse object and context for this file
        pobj = ParseObject(filename, file_lines, syntax=None)
        continue_col = -1 # Active continue column
        in_schar = False # Single quote character context
        in_dchar = False # Double quote character context
        prev_line = None
        curr_line, curr_line_num = pobj.curr_line()
        while curr_line is not None:
            # Skip empty lines and comment-only lines
            if pobj.blank_line(curr_line) or (curr_line.lstrip()[0] == '!'):
                curr_line, curr_line_num = pobj.next_line()
                continue
            # End if
            # scan the line for properties
            res = scan_line(curr_line, (continue_col >= 0),
                            in_schar, in_dchar, pobj)
            cont_in_col, cont_out_col, in_schar, in_dchar, comment_col = res
            # If in a continuation context, move this line to previos
            if continue_col >= 0:
                if prev_line is None:
                    raise ParseInternalError("No prev_line to continue", pobj)
                # End if
                sindex = max(0, cont_in_col)
                if cont_out_col > 0:
                    eindex = cont_out_col
                else:
                    eindex = len(curr_line)
                # End if
                prev_line = prev_line + curr_line[sindex:eindex]
                # Rewrite the file's lines
                pobj.write_line(prev_line_num, prev_line)
                pobj.write_line(curr_line_num, "")
                if cont_out_col < 0:
                    # We are done with this line, reset prev_line
                    prev_line = None
                    prev_line_num = -1
                # End if
            # End if
            continue_col = cont_out_col
            if (continue_col >= 0) and (prev_line is None):
                # We need to set up prev_line as it is continued
                prev_line = curr_line[0:continue_col]
                if not (in_schar or in_dchar):
                    prev_line = prev_line.rstrip()
                # End if
                prev_line_num = curr_line_num
            # End if
            curr_line, curr_line_num = pobj.next_line()
        # End while
        return pobj

def parse_specification(pobj, statements):
    pass

def parse_program(pobj, statements):
    # The first statement should be a program statement, grab the name
    pmatch = program_re.match(statement[0])
    if pmatch is None:
        raise ParseSyntaxError('PROGRAM statement', statement[0])
    # End if
    prog_name = pmatch.group(1)
    pobj.enter_region('PROGRAM', region_name=prog_name, nested_ok=False)
    # After the program name is the specification part
    parse_specification(pobj, statements[1:])
    # Look for metadata tables


def parse_fortran_file(filename):
    pobj = read_file(filename, preproc_defs=None)
    pobj.reset_pos()
    curr_line, clo = pobj.curr_line()
    while curr_line is not None:
        statements = line_statements(curr_line)
        for index in xrange(len(statements)):
            statement = statements[index]
            if program_re.match(statement) is not None:
                parse_program(pobj, statements[index+1:]) # returns what?
            # What else? modules?
        curr_line, clo = pobj.next_line()

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
# XXgoldyXX: v debug only
#    pdir = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
#    parse_fortran_file(os.path.join(pdir, 'schemes', 'check', 'nan.f90'))
# XXgoldyXX: ^ debug only
