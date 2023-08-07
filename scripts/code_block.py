#!/usr/bin/env python3
#

"""Class and methods to create a code block which can then be written
to a file."""

# Python library imports
import re
# CCPP framework imports
from parse_tools   import ParseContext, ParseSource, context_string
from parse_tools   import ParseInternalError

class CodeBlock(object):
    """Class to store a block of code and a method to write it to a file
    >>> CodeBlock([]) #doctest: +ELLIPSIS
    <code_block.CodeBlock object at 0x...>
    >>> CodeBlock(['hi mom']) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseInternalError: Each element of <code_list> must contain exactly two items, a code string and a relative indent
    >>> CodeBlock([('hi mom')]) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseInternalError: Each element of <code_list> must contain exactly two items, a code string and a relative indent
    >>> CodeBlock([('hi mom', 'x')]) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseInternalError: Each element of <code_list> must contain exactly two items, a code string and a relative indent
    >>> CodeBlock([('hi mom', 1)]) #doctest: +ELLIPSIS
    <code_block.CodeBlock object at 0x...>
    >>> from fortran_tools import FortranWriter
    >>> outfile_name = "__code_block_temp.F90"
    >>> outfile = FortranWriter(outfile_name, 'w', 'test file', 'test_mod')
    >>> CodeBlock([('hi mom', 1)]).write(outfile, 1, {})

    >>> CodeBlock([('hi {greet} mom', 1)]).write(outfile, 1, {}) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseInternalError: 'greet' missing from <var_dict>
    >>> CodeBlock([('hi {{greet}} mom', 1)]).write(outfile, 1, {})
    >>> CodeBlock([('{greet} there mom', 1)]).write(outfile, 1, {'greet':'hi'})
    >>> outfile.__exit__()
    False
    >>> import os
    >>> os.remove(outfile_name)
    """

    __var_re = re.compile(r"[{][ ]*([A-Za-z][A-Za-z0-9_]*)[ ]*[}]")

    __fmt_msg = ('Each element of <code_list> must contain exactly two '
                 'items, a code string and a relative indent')

    def __init__(self, code_list):
        """Initialize object with a list of statements.
        Capture and store all variables required for output.
        Each statement is a tuple consisting of a string and an indent level.
        Non-negative indents will be added to a current indent at write time
        while negative indents are written with no indentation.
        """
        self.__code_block = code_list
        self.__write_vars = list()
        for line in self.__code_block:
            if len(line) != 2:
                raise ParseInternalError(CodeBlock.__fmt_msg.format(code_list))
            # end if
            stmt = line[0]
            if not isinstance(stmt, str):
                raise ParseInternalError(CodeBlock.__fmt_msg.format(code_list))
            # end if
            if not isinstance(line[1], int):
                raise ParseInternalError(CodeBlock.__fmt_msg.format(code_list))
            # end if
            beg = 0
            end = len(stmt)
            while beg < end:
                # Ignore double curly braces
                open_double_curly = stmt.find('{{', beg)
                close_double_curly = stmt.find('}}',
                                               max(open_double_curly, beg))
                if 0 <= open_double_curly < close_double_curly:
                    beg = close_double_curly + 2
                else:
                    match = CodeBlock.__var_re.search(stmt[beg:])
                    if match:
                        self.__write_vars.append(match.group(1))
                        beg = stmt.index('}', beg) + 1
                    else:
                        beg = end + 1
                    # end if
                # end if
            # end while
        # end for

    def write(self, outfile, indent_level, var_dict):
        """Write this object's code block to <outfile> using <indent_level>
        as a basic offset.
        Format each line using the variables from <var_dict>.
        It is an error for <var_dict> to not contain any variable
        indicated in the code block."""

        for line in self.__code_block:
            stmt = line[0]
            if indent_level >= 0:
                indent = indent_level + line[1]
            else:
                indent = 0
            # end if
            # Check that <var_dict> contains all required items
            errmsg = ''
            sep = ''
            for var in self.__write_vars:
                if var not in var_dict:
                    errmsg += "'{}' missing from <var_dict>".format(sep, var)
                    sep = '\n'
                # end if
            # end for
            if errmsg:
                raise ParseInternalError(errmsg)
            # end if
            outfile.write(stmt.format(**var_dict), indent)
        # end for

###############################################################################
