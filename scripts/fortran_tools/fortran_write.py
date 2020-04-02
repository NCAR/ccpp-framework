#!/usr/bin/env python
#

"""Code to write Fortran code
"""

# Python library imports
from __future__ import print_function
# CCPP framework imports

class FortranWriter(object):
    """Class to turn output into properly continued and indented Fortran code
    >>> FortranWriter("foo.F90", 'r') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Read mode not allowed in FortranWriter object
    >>> FortranWriter("foo.F90", 'wb') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Binary mode not allowed in FortranWriter object
    """

    ###########################################################################
    # Class variables
    ###########################################################################
    INDENT = 3          # Spaces per indent level
    CONTINUE_INDENT = 5 # Extra spaces on continuation line
    LINE_FILL = 97      # Target line length
    LINE_MAX = 130      # Max line length

    ###########################################################################

    def indent(self, level=0, continue_line=False):
        'Return an indent string for any level'
        indent = self._indent * level
        if continue_line:
            indent = indent + self._continue_indent
        # End if
        return indent*' '

    ###########################################################################

    def find_best_break(self, choices, last=None):
        """Find the best line break point given <choices>.
        If <last> is present, use it as a target line length."""
        if last is None:
            last = self._line_fill
        # End if
        # Find largest good break
        possible = [x for x in choices if x < last]
        if not possible:
            best = self._line_max + 1
        else:
            best = max(possible)
        # End if
        if (best > self._line_max) and (last < self._line_max):
            best = self.find_best_break(choices, last=self._line_max)
        # End if
        return best

    ###########################################################################

    def write(self, statement, indent_level, continue_line=False):
        if '\n' in statement:
            for stmt in statement.split('\n'):
                self.write(stmt, indent_level, continue_line)
            # End for
        else:
            istr = self.indent(indent_level, continue_line)
            outstr = istr + statement.strip()
            line_len = len(outstr)
            if line_len > self._line_fill:
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
                        # Comment in non-character context, suck in rest of line
                        spaces.append(sptr-1)
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
                best = self.find_best_break(spaces)
                if best >= self._line_fill:
                    best = self.find_best_break(commas)
                # End if
                if len(outstr) > best:
                    # If next line is just comment, do not use continue
                    # NB: Is this a Fortran issue or just a gfortran issue?
                    line_continue = outstr[best+1:].lstrip()[0] != '!'
                # End if
                if line_continue:
                    fill = "{}&".format((self._line_fill-best)*' ')
                else:
                    fill = ''
                # End if
                self._file.write("{}{}\n".format(outstr[0:best+1], fill))
                statement = outstr[best+1:]
                self.write(statement, indent_level, continue_line=line_continue)
            else:
                self._file.write("{}\n".format(outstr))
            # End if
        # End if

    ###########################################################################

    def __init__(self, filename, mode, indent=None,
                 continue_indent=None, line_fill=None, line_max=None):
        """Initialize thie FortranWriter object"""
        # We only handle writing situations (for now) and only text
        if 'r' in mode:
            raise ValueError('Read mode not allowed in FortranWriter object')
        elif 'b' in mode:
            raise ValueError('Binary mode not allowed in FortranWriter object')
        else:
            self._file = open(filename, mode)
        # End if
        if indent is None:
            self._indent = FortranWriter.INDENT
        else:
            self._indent = indent
        # End if
        if continue_indent is None:
            self._continue_indent = FortranWriter.CONTINUE_INDENT
        else:
            self._continue_indent = continue_indent
        # End if
        if line_fill is None:
            self._line_fill = FortranWriter.LINE_FILL
        else:
            self._line_fill = line_fill
        # End if
        if line_max is None:
            self._line_max = FortranWriter.LINE_MAX
        else:
            self._line_max = line_max
        # End if

    ###########################################################################

    def __enter__(self, *args):
        return self

    ###########################################################################

    def __exit__(self, *args):
        self._file.close()
        return False

###############################################################################
if __name__ == "__main__":
    # First, run doctest
    import doctest
    doctest.testmod()
    # Make sure we can write a file
    import sys
    import os
    import os.path
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    NAME = 'foo'
    while os.path.exists(NAME+'.F90'):
        NAME = NAME + 'xo'
    # End while
    NAME = NAME + '.F90'
    if os.access(os.getcwd(), os.W_OK):
        check = [('      subroutine foo(long_argument1, long_argument2, '
                  'long_argument3, long_argument4,              &'),
                 '           long_argument5)',
                 '      end subroutine foo']
        with FortranWriter(NAME, 'w') as foo:
            foo.write(("subroutine foo(long_argument1, long_argument2, "
                       "long_argument3, long_argument4, long_argument5)"), 2)
            foo.write("end subroutine foo", 2)
        # End with
        # Check file
        with open(NAME, 'r') as foo:
            statements = foo.readlines()
            if len(statements) != len(check):
                EMSG = "ERROR: File has {} statements, should have {}"
                print(EMSG.format(len(statements), len(check)))
            else:
                for line_num, statement in enumerate(statements):
                    if statement.rstrip() != check[line_num]:
                        EMSG = "ERROR: Line {} does not match"
                        print(EMSG.format(line_num+1))
                        print("{}".format(statement.rstrip()))
                        print("{}".format(check[line_num]))
                    # End if
                # End for
        # End with
        os.remove(NAME)
    else:
        print("WARNING: Unable to write test file, '{}'".format(NAME))
    # End if
# No else
