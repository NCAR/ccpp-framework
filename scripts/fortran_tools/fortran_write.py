#!/usr/bin/env python3
#

"""Code to write Fortran code
"""

class FortranWriter(object):
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
                    elif outstr[sptr:sptr+2] == '//':
                        # Non-quote commas are where we can break
                        commas.append(sptr + 1)
                    # End if (no else, other characters will be ignored)
                    sptr = sptr + 1
                # End while
                best = self.find_best_break(spaces)
                if best >= self._line_fill:
                    best = self.find_best_break(commas)
                # End if
                if best > self._line_max:
                    # This is probably a bad situation that might not
                    # compile, just write the line and hope for the best.
                    line_continue = False
                elif len(outstr) > best:
                    # If next line is just comment, do not use continue
                    # NB: Is this a Fortran issue or just a gfortran issue?
                    line_continue = outstr[best+1:].lstrip()[0] != '!'
                else:
                    line_continue = True
                # End if
                if line_continue:
                    fill = "{}&".format((self._line_fill - best)*' ')
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

    def __init__(self, filename, mode, file_description, module_name,
                 indent=None, continue_indent=None,
                 line_fill=None, line_max=None):
        """Initialize thie FortranWriter object.
        Some boilerplate is written automatically."""
        self.__file_desc = file_description
        self.__module = module_name
        # We only handle writing situations (for now) and only text
        if 'r' in mode:
            raise ValueError('Read mode not allowed in FortranWriter object')
        # end if
        if 'b' in mode:
            raise ValueError('Binary mode not allowed in FortranWriter object')
        # End if
        self._file = open(filename, mode)
        if indent is None:
            self._indent = FortranWriter.__INDENT
        else:
            self._indent = indent
        # End if
        if continue_indent is None:
            self._continue_indent = FortranWriter.__CONTINUE_INDENT
        else:
            self._continue_indent = continue_indent
        # End if
        if line_fill is None:
            self._line_fill = FortranWriter.__LINE_FILL
        else:
            self._line_fill = line_fill
        # End if
        if line_max is None:
            self._line_max = FortranWriter.__LINE_MAX
        else:
            self._line_max = line_max
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
        self._file.close()
        return False

    ###########################################################################

    def module_header(self):
        """Return the standard Fortran module header for <filename> and
        <module>"""
        return FortranWriter.__MOD_HEADER.format(file_desc=self.__file_desc,
                                                 module=self.__module)

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
