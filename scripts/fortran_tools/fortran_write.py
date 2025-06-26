#!/usr/bin/env python3
#

"""Code to write Fortran code
"""

import math

class FortranWriter:
    """Class to turn output into properly continued and indented Fortran code
    >>> FortranWriter("foo.F90", 'r', 'test', 'mod_name') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Read mode not allowed in FortranWriter object
    >>> FortranWriter("foo.F90", 'wb', 'test', 'mod_name') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Binary mode not allowed in FortranWriter object
    """

    ###########################################################################
    # Class variables
    ###########################################################################
    __INDENT = 3          # Spaces per indent level

    __CONTINUE_INDENT = 5 # Extra spaces on continuation line

    __LINE_FILL = 97      # Target line length

    __LINE_MAX = 130      # Max line length

    # CCPP copyright statement to be included in all generated Fortran files
    __COPYRIGHT = '''!
! This work (Common Community Physics Package Framework), identified by
! NOAA, NCAR, CU/CIRES, is free of known copyright restrictions and is
! placed in the public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'''

    __MOD_HEADER = '''
!>
!! @brief Auto-generated {file_desc}
!!
!
module {module}
'''

    __MOD_PREAMBLE = ["implicit none", "private"]

    __CONTAINS = '''
CONTAINS'''

    __MOD_FOOTER = '''
end module {module}'''

    ###########################################################################

    def indent(self, level=0, continue_line=False):
        'Return an indent string for any level'
        indent = self.indent_size * level
        if continue_line:
            indent = indent + self.__continue_indent
        # End if
        return indent*' '

    ###########################################################################

    def find_best_break(self, choices, last=None):
        """Find the best line break point given <choices>.
        If <last> is present, use it as a target line length."""
        if last is None:
            last = self.__line_fill
        # End if
        # Find largest good break
        possible = [x for x in choices if 0 < x < last]
        if not possible:
            best = self.__line_max + 1
        else:
            best = max(possible)
        # End if
        if (best > self.__line_max) and (last < self.__line_max):
            best = self.find_best_break(choices, last=self.__line_max)
        # End if
        return best

    ###########################################################################

    @staticmethod
    def _in_quote(test_str):
        """Return True if <test_str> ends in a character context.
        >>> FortranWriter._in_quote("hi'mom")
        True
        >>> FortranWriter._in_quote("hi mom")
        False
        >>> FortranWriter._in_quote("'hi mom'")
        False
        >>> FortranWriter._in_quote("'hi"" mom'")
        False
        """
        in_single_char = False
        in_double_char = False
        for char in test_str:
            if in_single_char:
                if char == "'":
                    in_single_char = False
                # end if
            elif in_double_char:
                if char == '"':
                    in_double_char = False
                # end if
            elif char == "'":
                in_single_char = True
            elif char == '"':
                in_double_char = True
            # end if
        # end for
        return in_single_char or in_double_char

    ###########################################################################

    def write(self, statement, indent_level, continue_line=False):
        """Write <statement> to the open file, indenting to <indent_level>
        (see self.indent).
        If <continue_line> is True, treat this line as a continuation of
        a previous statement."""
        if isinstance(statement, list):
            for stmt in statement:
                self.write(stmt, indent_level, continue_line)
            # End for
        elif '\n' in statement:
            for stmt in statement.split('\n'):
                self.write(stmt, indent_level, continue_line)
            # End for
        else:
            istr = self.indent(indent_level, continue_line)
            ostmt = statement.strip()
            is_comment_stmt = ostmt and (ostmt[0] == '!')
            in_comment = ""
            if ostmt and (ostmt[0] != '&'):
                # Skip indent for continue that is in the middle of a
                #    token or a quoted region
                outstr = istr + ostmt
            else:
                outstr = ostmt
            # end if
            line_len = len(outstr)
            if line_len > self.__line_fill:
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
                        # Comment in non-character context
                        spaces.append(sptr-1)
                        in_comment = "! " # No continue for comment
                        if ((not is_comment_stmt) and
                            (sptr >= self.__max_comment_start)):
                            # suck in rest of line
                            sptr = line_len - 1
                        # end if
                    elif outstr[sptr] == ' ':
                        # Non-quote spaces are where we can break
                        spaces.append(sptr)
                    elif outstr[sptr] == ',':
                        # Non-quote commas are where we can break
                        commas.append(sptr)
                    elif outstr[sptr:sptr+2] == '//':
                        # Non-quote commas are where we can break
                        commas.append(sptr + 1)
                    # End if (no else, other characters will be ignored)
                    sptr = sptr + 1
                # End while
                # Before looking for best space, reject any that are on a
                #    comment line but before any significant characters
                if outstr.lstrip().startswith('!'):
                    first_space = outstr.index('!') + 1
                    while ((outstr[first_space] == '!' or
                            outstr[first_space] == ' ') and
                           (first_space < line_len)):
                        first_space += 1
                    # end while
                    if min(spaces) < first_space:
                        spaces = [x for x in spaces if x >= first_space]
                    # end if
                best = self.find_best_break(spaces)
                if best >= self.__line_fill:
                    best = min(best, self.find_best_break(commas))
                # End if
                line_continue = False
                if best >= self.__line_max:
                    # This is probably a bad situation so we have to break
                    #   in an ugly spot
                    best = self.__line_max - 1
                    if len(outstr) > best:
                        line_continue = '&'
                    # end if
                # end if
                if len(outstr) > best:
                    if self._in_quote(outstr[0:best+1]):
                        line_continue = '&'
                    else:
                        # If next line is just comment, do not use continue
                        line_continue = outstr[best+1:].lstrip()[0] != '!'
                    # end if
                elif not line_continue:
                    line_continue = len(outstr) > best
                # End if
                if in_comment or is_comment_stmt:
                    line_continue = False
                # end if
                if line_continue:
                    fill = "{}&".format((self.__line_fill - best)*' ')
                else:
                    fill = ""
                # End if
                outline = f"{outstr[0:best+1]}{fill}".rstrip()
                self.__file.write(f"{outline}\n")
                if best <= 0:
                    imsg = "Internal ERROR: Unable to break line"
                    raise ValueError(f"{imsg}, '{statement}'")
                # end if
                statement = in_comment + outstr[best+1:]
                if isinstance(line_continue, str) and statement:
                    statement = line_continue + statement
                # end if
                self.write(statement, indent_level, continue_line=line_continue)
            else:
                self.__file.write("{}\n".format(outstr))
            # End if
        # End if

    ###########################################################################

    def __init__(self, filename, mode, file_description, module_name,
                 indent=None, continue_indent=None,
                 line_fill=None, line_max=None):
        """Initialize thie FortranWriter object.
        Some boilerplate is written automatically."""
        self.__file_desc = file_description.replace('\n', '\n!! ')
        self.__module = module_name
        # We only handle writing situations (for now) and only text
        if 'r' in mode:
            raise ValueError('Read mode not allowed in FortranWriter object')
        # end if
        if 'b' in mode:
            raise ValueError('Binary mode not allowed in FortranWriter object')
        # End if
        self.__file = open(filename, mode)
        if indent is None:
            self.__indent = FortranWriter.__INDENT
        else:
            self.__indent = indent
        # End if
        if continue_indent is None:
            self.__continue_indent = FortranWriter.__CONTINUE_INDENT
        else:
            self.__continue_indent = continue_indent
        # End if
        if line_fill is None:
            self.__line_fill = FortranWriter.__LINE_FILL
        else:
            self.__line_fill = line_fill
        # End if
        self.__max_comment_start = math.ceil(self.__line_fill * 3 / 4)
        if line_max is None:
            self.__line_max = FortranWriter.__LINE_MAX
        else:
            self.__line_max = line_max
        # End if

    ###########################################################################

    def write_preamble(self):
        """Write the module boilerplate that goes between use statements
        and module declarations."""
        self.write("", 0)
        for stmt in FortranWriter.__MOD_PREAMBLE:
            self.write(stmt, 1)
        # end for
        self.write("", 0)

    ###########################################################################

    def end_module_header(self):
        """Write the module contains statement."""
        self.write(FortranWriter.__CONTAINS, 0)

    ###########################################################################

    def __enter__(self, *args):
        self.write(FortranWriter.__COPYRIGHT, 0)
        self.write(self.module_header(), 0)
        return self

    ###########################################################################

    def __exit__(self, *args):
        self.write(FortranWriter.__MOD_FOOTER.format(module=self.__module), 0)
        self.__file.close()
        return False

    ###########################################################################

    def module_header(self):
        """Return the standard Fortran module header for <filename> and
        <module>"""
        return FortranWriter.__MOD_HEADER.format(file_desc=self.__file_desc,
                                                 module=self.__module)

    ###########################################################################

    def comment(self, comment, indent):
        """Write a Fortran comment with contents, <comment>"""
        mlcomment = comment.replace('\n', '\n! ') # No backslash in f string
        self.write(f"! {mlcomment}", indent)

    ###########################################################################

    def blank_line(self):
        """Write a blank line"""
        self.write("", 0)

    ###########################################################################

    def include(self, filename):
        """Insert the contents of <filename> verbatim."""
        with open(filename, 'r') as infile:
            for line in infile:
                self.__file.write(line)
            # end for
        # end with

    ###########################################################################

    @property
    def line_fill(self):
        """Return the target line length for this Fortran file"""
        return self.__line_fill

    ###########################################################################

    @property
    def indent_size(self):
        """Return the number of spaces for each indent level for this
              Fortran file
        """
        return self.__indent

    ###########################################################################

    @classmethod
    def copyright(cls):
        """Return the standard Fortran file copyright string"""
        return cls.__COPYRIGHT

###############################################################################
if __name__ == "__main__":
    # First, run doctest
    # pylint: disable=ungrouped-imports
    import doctest
    import os
    import sys
    # pylint: enable=ungrouped-imports
    fail, _ = doctest.testmod()
    # Make sure we can write a file
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    NAME = 'foo'
    while os.path.exists(NAME+'.F90'):
        NAME = NAME + 'xo'
    # end while
    NAME = NAME + '.F90'
    if os.access(os.getcwd(), os.W_OK):
        _CHECK = FortranWriter.copyright().split('\n')
        with FortranWriter(NAME, 'w', 'doctest', 'foo') as foo:
            foo.write_preamble()
            foo.end_module_header()
            foo.write(("subroutine foo(long_argument1, long_argument2, "
                       "long_argument3, long_argument4, long_argument5)"), 2)
            foo.write("end subroutine foo", 2)
            _CHECK.extend(foo.module_header().rstrip().split('\n'))
        # End with
        _CHECK.extend(["", "", "   implicit none", "   private",
                       "", "", "CONTAINS"])
        _CHECK.extend([('      subroutine foo(long_argument1, long_argument2, '
                        'long_argument3, long_argument4,              &'),
                       '           long_argument5)',
                       '      end subroutine foo', '',
                       'end module foo'])
        # Check file
        with open(NAME, 'r') as foo:
            _STATEMENTS = foo.readlines()
            if len(_STATEMENTS) != len(_CHECK):
                EMSG = "ERROR: File has {} statements, should have {}"
                print(EMSG.format(len(_STATEMENTS), len(_CHECK)))
            else:
                for _line_num, _statement in enumerate(_STATEMENTS):
                    if _statement.rstrip() != _CHECK[_line_num]:
                        EMSG = "ERROR: Line {} does not match"
                        print(EMSG.format(_line_num+1))
                        print("{}".format(_statement.rstrip()))
                        print("{}".format(_CHECK[_line_num]))
                    # end if
                # end for
        # end with
        os.remove(NAME)
    else:
        print("WARNING: Unable to write test file, '{}'".format(NAME))
    # end if
    sys.exit(fail)
# end if
