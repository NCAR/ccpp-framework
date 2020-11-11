#! /usr/bin/env python
"""
Tool to parse a Fortran file and return signature information
from metadata tables.
At the file level, we allow only PROGRAM blocks and MODULE blocks.
Subroutines, functions, or data are not supported outside a MODULE.
"""

# Python library imports
import os.path
if __name__ == '__main__' and __package__ is None:
    import sys
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
# end if
import re
from collections import OrderedDict
# CCPP framework imports
from parse_tools import CCPPError, ParseInternalError, ParseSyntaxError
from parse_tools import ParseContext, ParseObject, ParseSource, PreprocStack
from parse_tools import FORTRAN_ID, context_string
from metadata_table import MetadataTable
try:
    from parse_fortran import parse_fortran_var_decl, fortran_type_definition
except ModuleNotFoundError:
    from .parse_fortran import parse_fortran_var_decl, fortran_type_definition
# end try
from metavar import VarDictionary

_COMMENT_RE = re.compile(r"!.*$")
_FIXED_COMMENT_RE = re.compile(r"(?i)([C*]|(?:[ ]{0,4}!))")
_PROGRAM_RE = re.compile(r"(?i)\s*program\s+"+FORTRAN_ID)
_ENDPROGRAM_RE = re.compile(r"(?i)\s*end\s*program\s+"+FORTRAN_ID+r"?")
_MODULE_RE = re.compile(r"(?i)\s*module\s+"+FORTRAN_ID)
_ENDMODULE_RE = re.compile(r"(?i)\s*end\s*module\s+"+FORTRAN_ID+r"?")
_CONTAINS_RE = re.compile(r"(?i)\s*contains")
_CONTINUE_RE = re.compile(r"(?i)&\s*(!.*)?$")
_FIXED_CONTINUE_RE = re.compile(r"(?i)     [^0 ]")
_BLANK_RE = re.compile(r"\s+")
_ARG_TABLE_START_RE = re.compile(r"(?i)\s*![!>]\s*(?:\\section)?\s*arg_table_"+FORTRAN_ID)
_ARG_TABLE_END_RE = re.compile(r"(?i)![!>]\s*end_arg_table_"+FORTRAN_ID)
_PREFIX_SPECS = [r"(?:recursive)", r"(?:pure)", r"(?:elemental)"]
_PREFIX_SPEC_RE = r"(?:{})?\s*".format('|'.join(_PREFIX_SPECS))
_SUBNAME_RE = r"(?i)subroutine\s*"
_ARGLIST_RE = r"\s*(?:[(]\s*([^)]*)[)])?"
_SUBROUTINE_RE = re.compile(r"(?i)\s*"+_PREFIX_SPEC_RE+_SUBNAME_RE+FORTRAN_ID+_ARGLIST_RE)
_END_SUBROUTINE_RE = re.compile(r"(?i)\s*end\s*"+_SUBNAME_RE+FORTRAN_ID+r"?")
_USE_RE = re.compile(r"(?i)\s*use\s(?:,\s*intrinsic\s*::)?\s*only\s*:([^!]+)")
_END_TYPE_RE = re.compile(r"(?i)\s*end\s*type(?:\s+"+FORTRAN_ID+r")?")
_INTENT_STMT_RE = re.compile(r"(?i),\s*intent\s*[(]")

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
    >>> line_statements(" ! This is a comment statement; y'all;")
    [" ! This is a comment statement; y'all;"]
    >>> line_statements("!! ")
    ['!! ']
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

def read_statements(pobj, statements=None):
    """Retrieve the next line and break it into statements"""
    while (statements is None) or (sum([len(x) for x in statements]) == 0):
        nline, _ = pobj.next_line()
        if nline is None:
            statements = None
            break
        else:
            statements = line_statements(nline)
        # End if
    # End while
    return statements

########################################################################
def scan_fixed_line(line, in_single_char, in_double_char, context):
    """Scan a fixed-format FORTRAN line for continue indicators, continued
    quotes, and comments
    Return continue_in_col, in_single_char, in_double_char,
           comment_col
    >>> scan_fixed_line('     & line continued', False, False, ParseContext())
    (5, False, False, -1)
    >>> scan_fixed_line('     & line continued"', False, True, ParseContext())
    (5, False, False, -1)
    >>> scan_fixed_line('     * line continued', False, False, ParseContext())
    (5, False, False, -1)
    >>> scan_fixed_line('     1 line continued', False, False, ParseContext())
    (5, False, False, -1)
    >>> scan_fixed_line('C     comment line', False, False, ParseContext())
    (-1, False, False, 0)
    >>> scan_fixed_line('*     comment line', False, False, ParseContext())
    (-1, False, False, 0)
    >>> scan_fixed_line('!     comment line', False, False, ParseContext())
    (-1, False, False, 0)
    >>> scan_fixed_line(' !    comment line', False, False, ParseContext())
    (-1, False, False, 1)
    >>> scan_fixed_line('    ! comment line', False, False, ParseContext())
    (-1, False, False, 4)
    >>> scan_fixed_line('     ! not comment line', False, False, ParseContext())
    (5, False, False, -1)
    >>> scan_fixed_line('!...................................', False, False, ParseContext())
    (-1, False, False, 0)
    >>> scan_fixed_line('123   x = x + 1', False, False, ParseContext())
    (-1, False, False, -1)
    """

    # Check if comment or continue statement
    cmatch = _FIXED_COMMENT_RE.match(line)
    is_comment = cmatch is not None
    is_continue = _FIXED_CONTINUE_RE.match(line) is not None
    # A few sanity checks
    if (in_single_char or in_double_char) and (not is_continue):
        raise ParseSyntaxError("Cannot start line in character context if not a continued line", context=context)
    # Endif
    if in_single_char and in_double_char:
        raise ParseSyntaxError("Cannot be both in an apostrophe character context and a quote character context", context=context)

    if is_continue:
        continue_in_col = 5
        comment_col = -1
        index = 6
    elif is_comment:
        comment_col = len(cmatch.group(1)) - 1
        continue_in_col = -1
        index = len(line.rstrip())
    else:
        continue_in_col = -1
        comment_col = -1
        index = 0
    # End if

    last_ind = len(line.rstrip()) - 1
    # Process the line
    while index <= last_ind:
        blank = _BLANK_RE.match(line[index:])
        if blank is not None:
            index = index + len(blank.group(0)) - 1 # +1 at end of loop
        elif in_single_char:
            if line[index:min(index+1, last_ind)] == "''":
                # Embedded single quote
                index = index + 1 # +1 and end of loop
            elif line[index] == "'":
                in_single_char = False
                # End if
            # End if (just ignore any other character)
        elif in_double_char:
            if line[index:min(index+1, last_ind)] == '""':
                # Embedded double quote
                index = index + 1 # +1 and end of loop
            elif line[index] == '"':
                in_double_char = False
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
        # End if
        index = index + 1
    # End while

    return continue_in_col, in_single_char, in_double_char, comment_col

########################################################################

def scan_free_line(line, in_continue, in_single_char, in_double_char, context):
    """Scan a Fortran line for continue indicators, continued quotes, and
    comments
    Return continue_in_col, continue_out_col, in_single_char, in_double_char,
           comment_col
    >>> scan_free_line("! Comment line", False, False, False, ParseContext())
    (-1, -1, False, False, 0)
    >>> scan_free_line("!! ", False, False, False, ParseContext())
    (-1, -1, False, False, 0)
    >>> scan_free_line("int :: index", False, False, False, ParseContext())
    (-1, -1, False, False, -1)
    >>> scan_free_line("int :: inde& ! oops", False, False, False, ParseContext())
    (-1, 11, False, False, 13)
    >>> scan_free_line("int :: inde&", False, False, False, ParseContext())
    (-1, 11, False, False, -1)
    >>> scan_free_line("character(len=*), parameter :: foo = 'This line & not continued'", False, False, False, ParseContext())
    (-1, -1, False, False, -1)
    >>> scan_free_line("character(len=*), parameter :: foo = 'This is continue line& ", False, False, False, ParseContext())
    (-1, 59, True, False, -1)
    >>> scan_free_line('character(len=*), parameter :: foo = "This line & not continued"', False, False, False, ParseContext())
    (-1, -1, False, False, -1)
    >>> scan_free_line('character(len=*), parameter :: foo = "This is continue line& ', False, False, False, ParseContext())
    (-1, 59, False, True, -1)
    >>> scan_free_line('  & line continued"', True, False, True, ParseContext())
    (2, -1, False, False, -1)
    >>> scan_free_line('  & line continued"', True, True, False, ParseContext()) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Cannot end non-continued line in a character context, in <standard input>
    >>> scan_free_line("  & line continued'", True, False, True, ParseContext()) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Cannot end non-continued line in a character context, in <standard input>
    >>> scan_free_line("int :: inde&", False, True, False, ParseContext())
    Traceback (most recent call last):
    parse_source.ParseSyntaxError: Cannot start line in character context if not a continued line, in <standard input>
    >>> scan_free_line("int :: inde&", True, True, True, ParseContext())
    Traceback (most recent call last):
    parse_source.ParseSyntaxError: Cannot be both in an apostrophe character context and a quote character context, in <standard input>
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
        blank = _BLANK_RE.match(line[index:])
        if blank is not None:
            index = index + len(blank.group(0)) - 1 # +1 at end of loop
        elif in_single_char:
            if line[index:min(index+1, last_ind)] == "''":
                # Embedded single quote
                index = index + 1 # +1 and end of loop
            elif line[index] == "'":
                in_single_char = False
            elif line[index] == '&':
                if index == last_ind:
                    continue_out_col = index
                # End if
            # End if (just ignore any other character)
        elif in_double_char:
            if line[index:min(index+1, last_ind)] == '""':
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
            match = _CONTINUE_RE.match(line[index:])
            if match is not None:
                continue_out_col = index
            else:
                errmsg = ("Invalid continue, ampersand not followed by "
                          "comment character")
                raise ParseSyntaxError(errmsg, context=context)
            # End if
        # End if
        index = index + 1
    # End while
    # A final check
    if (in_single_char or in_double_char) and (continue_out_col < 0):
        errmsg = "Cannot end non-continued line in a character context"
        raise ParseSyntaxError(errmsg, context=context)

    return continue_in_col, continue_out_col, in_single_char, in_double_char, comment_col

########################################################################

def read_file(filename, preproc_defs=None, logger=None):
    """Read a file into an array of lines.
    Preprocess lines to consolidate continuation lines.
    Remove preprocessor directives and code eliminated by #if statements
    Remvoved code results in blank lines, not removed lines
    """
    preproc_status = PreprocStack()
    if not os.path.exists(filename):
        raise IOError("read_file: file, '{}', does not exist".format(filename))
    else:
        # We need special rules for fixed-form source
        fixed_form = filename[-2:].lower() == '.f'
        # Read all lines of the file at once
        with open(filename, 'r') as file:
            file_lines = file.readlines()
            for index, line in enumerate(file_lines):
                file_lines[index] = line.rstrip('\n').rstrip()
            # End for
        # End with
        # create a parse object and context for this file
        pobj = ParseObject(filename, file_lines)
        continue_col = -1 # Active continue column
        in_schar = False # Single quote character context
        in_dchar = False # Double quote character context
        prev_line = None
        prev_line_num = -1
        curr_line, curr_line_num = pobj.curr_line()
        while curr_line is not None:
            # Skip empty lines and comment-only lines
            skip_line = False
            if len(curr_line.strip()) == 0:
                skip_line = True
            elif (fixed_form and
                  (_FIXED_COMMENT_RE.match(curr_line) is not None)):
                skip_line = True
            elif curr_line.lstrip()[0] == '!':
                skip_line = True
            # End if
            if skip_line:
                curr_line, curr_line_num = pobj.next_line()
                continue
            # End if
            # Handle preproc issues
            if preproc_status.process_line(curr_line, preproc_defs, pobj, logger):
                pobj.write_line(curr_line_num, "")
                curr_line, curr_line_num = pobj.next_line()
                continue
            # End if
            if not preproc_status.in_true_region():
                # Special case to allow CCPP comment statements in False
                # regions to find DDT and module table code
                if (curr_line[0:2] != '!!') and (curr_line[0:2] != '!>'):
                    pobj.write_line(curr_line_num, "")
                    curr_line, curr_line_num = pobj.next_line()
                    continue
                # End if
            # End if
            # scan the line for properties
            if fixed_form:
                res = scan_fixed_line(curr_line, in_schar, in_dchar, pobj)
                cont_in_col, in_schar, in_dchar, comment_col = res
                continue_col = cont_in_col # No warning in fixed form
                cont_out_col = -1
                if (comment_col < 0) and (continue_col < 0):
                    # Real statement, grab the line # in case is continued
                    prev_line_num = curr_line_num
                    prev_line = None
                # End if
            else:
                res = scan_free_line(curr_line, (continue_col >= 0),
                                     in_schar, in_dchar, pobj)
                cont_in_col, cont_out_col, in_schar, in_dchar, comment_col = res
            # End if
            # If in a continuation context, move this line to previous
            if continue_col >= 0:
                if fixed_form and (prev_line is None):
                    prev_line = pobj.peek_line(prev_line_num)[0:72]
                # End if
                if prev_line is None:
                    raise ParseInternalError("No prev_line to continue", context=pobj)
                # End if
                sindex = max(cont_in_col+1, 0)
                if fixed_form:
                    sindex = 6
                    eindex = 72
                elif cont_out_col > 0:
                    eindex = cont_out_col
                else:
                    eindex = len(curr_line)
                # End if
                prev_line = prev_line + curr_line[sindex:eindex]
                if fixed_form:
                    prev_line = prev_line.rstrip()
                # End if
                # Rewrite the file's lines
                pobj.write_line(prev_line_num, prev_line)
                pobj.write_line(curr_line_num, "")
                if (not fixed_form) and (cont_out_col < 0):
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

########################################################################

def parse_use_statement(statement, logger):
    """Return True iff <statement> is a use statement"""
    umatch = _USE_RE.match(statement)
    if umatch is None:
        return False
    else:
        if logger:
            logger.debug("use = {}".format(umatch.group(1)))
        # end if
        return True
    # End if

########################################################################

def is_dummy_argument_statement(statement):
    """Return True iff <statement> is a dummy argument declaration"""
    return _INTENT_STMT_RE.search(statement) is not None

########################################################################

def is_comment_statement(statement):
    """Return True iff <statement> is a Fortran comment"""
    return statement.lstrip()[0] == '!'

########################################################################

def parse_type_def(statements, type_def, mod_name, pobj, logger):
    """Parse a type definition from <statements> and return the
    remaining statements along with a MetadataTable object representing
    the type's variables."""
    psrc = ParseSource(mod_name, 'ddt', pobj)
    seen_contains = False
    mheader = None
    var_dict = VarDictionary(type_def[0])
    inspec = True
    while inspec and (statements is not None):
        while len(statements) > 0:
            statement = statements.pop(0)
            # End program or module
            pmatch = _END_TYPE_RE.match(statement)
            if pmatch is not None:
                # We hit the end of the type, make a header
                mheader = MetadataTable(table_name_in=type_def[0],
                                        table_type_in='ddt',
                                        module=mod_name, var_dict=var_dict,
                                        logger=logger)
                inspec = False
            elif is_contains_statement(statement, inspec):
                seen_contains = True
            elif not seen_contains:
                # Comment of variable
                if ((not is_comment_statement(statement)) and
                    (not parse_use_statement(statement, logger))):
                    vars = parse_fortran_var_decl(statement, psrc, logger=logger)
                    for var in vars:
                        var_dict.add_variable(var)
                    # End for
                # End if
            else:
                # We are just skipping lines until the end type
                pass
            # End if
        # End while
        if inspec and (len(statements) == 0):
            statements = read_statements(pobj)
        # End if
    # End while
    return statements, mheader

########################################################################

def parse_preamble_data(statements, pobj, spec_name, endmatch, logger):
    """Parse module variables or DDT definitions from a module preamble
    or parse program variables from the beginning of a program.
    """
    inspec = True
    mheaders = list()
    var_dict = VarDictionary(spec_name)
    psrc = ParseSource(spec_name, 'MODULE', pobj)
    active_table = None
    if logger is not None:
        ctx = context_string(pobj, nodir=True)
        msg = "Parsing preamble variables of {}{}"
        logger.debug(msg.format(spec_name, ctx))
    # End if
    while inspec and (statements is not None):
        while len(statements) > 0:
            statement = statements.pop(0)
            # End program or module
            pmatch = endmatch.match(statement)
            asmatch = _ARG_TABLE_START_RE.match(statement)
            type_def = fortran_type_definition(statement)
            if asmatch is not None:
                active_table = asmatch.group(1)
            elif (pmatch is not None) or is_contains_statement(statement,
                                                               inspec):
                # We are done with the specification
                inspec = False
                # Put statement back so caller knows where we are
                statements.insert(0, statement)
                # Add the header (even if we found no variables)
                mheader = MetadataTable(table_name_in=spec_name,
                                        table_type_in='module',
                                        module=spec_name,
                                        var_dict=var_dict, logger=logger)
                mheaders.append(mheader)
                if logger is not None:
                    ctx = context_string(pobj, nodir=True)
                    msg = 'Adding header {}{}'
                    logger.debug(msg.format(mheader.table_name, ctx))
                break
            elif ((type_def is not None) and
                  (type_def[0].lower() == active_table.lower())):
                # Put statement back so caller knows where we are
                statements.insert(0, statement)
                statements, ddt = parse_type_def(statements, type_def,
                                                 spec_name, pobj, logger)
                if ddt is None:
                    ctx = context_string(pobj, nodir=True)
                    msg = "No DDT found at '{}'{}"
                    raise CCPPError(msg.format(statement, ctx))
                # End if
                mheaders.append(ddt)
                if logger is not None:
                    ctx = context_string(pobj, nodir=True)
                    msg = 'Adding DDT {}{}'
                    logger.debug(msg.format(ddt.table_name, ctx))
                # End if
                active_table = None
            elif active_table is not None:
                # We should have a variable definition to add
                if ((not is_comment_statement(statement)) and
                    (not parse_use_statement(statement, logger)) and
                    (active_table.lower() == spec_name.lower())):
                    vars = parse_fortran_var_decl(statement, psrc,
                                                  logger=logger)
                    for var in vars:
                        var_dict.add_variable(var)
                    # End for
                # End if
            # End if (else we are not in an active table so just skip)
        # End while
        if inspec and (len(statements) == 0):
            statements = read_statements(pobj)
        # End if
    # End while
    return statements, mheaders

########################################################################

def parse_scheme_metadata(statements, pobj, spec_name, table_name, logger):
    "Parse dummy argument information from a subroutine"
    psrc = None
    mheader = None
    var_dict = None
    scheme_name = None
    # Find the subroutine line, should be first executable statement
    inpreamble = False
    insub = True
    if logger is not None:
        ctx = context_string(pobj, nodir=True)
        msg = "Parsing specification of {}{}"
        logger.debug(msg.format(table_name, ctx))
    # End if
    ctx = context_string(pobj) # Save initial context with directory
    vdict = None # Initialized when we parse the subroutine arguments
    while insub and (statements is not None):
        while statements:
            statement = statements.pop(0)
            smatch = _SUBROUTINE_RE.match(statement)
            esmatch = _END_SUBROUTINE_RE.match(statement)
            pmatch = _ENDMODULE_RE.match(statement)
            asmatch = _ARG_TABLE_START_RE.match(statement)
            if asmatch is not None:
                # We have run off the end of something, hope that is okay
                # Put this statement back for the caller to deal with
                statements.insert(0, statement)
                insub = False
                break
            elif (pmatch is not None):
                # We have run off the end of the module, hope that is okay
                pobj.leave_region('MODULE', region_name=spec_name)
                insub = False
                break
            elif smatch is not None:
                scheme_name = smatch.group(1)
                inpreamble = scheme_name.lower() == table_name.lower()
                if inpreamble:
                    if smatch.group(2) is not None:
                        smstr = smatch.group(2).strip()
                        if len(smstr) > 0:
                            smlist = smstr.strip().split(',')
                        else:
                            smlist = list()
                        # End if
                        scheme_args = [x.strip().lower() for x in smlist]
                    else:
                        scheme_args = list()
                    # End if
                    # Create a dict template with all the scheme's arguments
                    # in the correct order
                    vdict = OrderedDict()
                    for arg in scheme_args:
                        if len(arg) == 0:
                            errmsg = 'Empty argument{}'
                            raise ParseInternalError(errmsg.format(pobj))
                        elif arg in vdict:
                            errmsg = 'Duplicate dummy argument, {}'
                            raise ParseSyntaxError(errmsg.format(arg),
                                                   context=pobj)
                        # End if
                        vdict[arg] = None
                    # End for
                    psrc = ParseSource(scheme_name, 'scheme', pobj)
                # End if
            elif inpreamble:
                # Process a preamble statement (use or argument declaration)
                if esmatch is not None:
                    inpreamble = False
                    insub = False
                elif ((not is_comment_statement(statement)) and
                      (not parse_use_statement(statement, logger)) and
                      is_dummy_argument_statement(statement)):
                    vars = parse_fortran_var_decl(statement, psrc,
                                                  logger=logger)
                    for var in vars:
                        lname = var.get_prop_value('local_name').lower()
                        if lname in vdict:
                            if vdict[lname] is not None:
                                emsg = "Error: duplicate dummy argument, {}"
                                raise ParseSyntaxError(emsg.format(lname),
                                                       context=pobj)
                            else:
                                vdict[lname] = var
                            # End if
                        else:
                            raise ParseSyntaxError('dummy argument',
                                                   token=lname, context=pobj)
                        # End if
                    # End for
                # End if
            # End if
        # End while
        if insub and (len(statements) == 0):
            statements = read_statements(pobj)
        # End if
    # End while
    # Check for missing declarations
    missing = list()
    if vdict is None:
        errmsg = 'Subroutine, {}, not found{}'
        raise CCPPError(errmsg.format(scheme_name, ctx))
    # End if
    for lname in vdict.keys():
        if vdict[lname] is None:
            missing.append(lname)
        # End if
    # End for
    if len(missing) > 0:
        errmsg = 'Missing local_variables, {} in {}'
        raise CCPPError(errmsg.format(missing, scheme_name))
    # End if
    var_dict = VarDictionary(scheme_name, variables=vdict)
    if (scheme_name is not None) and (var_dict is not None):
        mheader = MetadataTable(table_name_in=scheme_name,
                                table_type_in='scheme', module=spec_name,
                                var_dict=var_dict, logger=logger)
    # End if
    return statements, mheader

########################################################################

def is_contains_statement(statement, in_module):
    "Return True iff <statement> is an executable Fortran statement"
    # Fill this in when we need to parse programs or subroutines
    if in_module and (_CONTAINS_RE.match(statement.strip()) is not None):
        return True
    else:
        return False
    # End if

########################################################################

def duplicate_header(header, duplicate):
    ctx = duplicate.start_context()
    octx = header.start_context()
    errmsg = 'Duplicate header, {}{}'
    if len(octx) > 0:
        errmsg = errmsg + ', original{}'
    # End if
    return errmsg

########################################################################

def parse_specification(pobj, statements, mod_name=None, prog_name=None, logger=None):
    "Parse specification part of a module or (sub)program"
    if (mod_name is not None) and (prog_name is not None):
        raise ParseInternalError("<mod_name> and <prog_name> cannot both be used")
    elif mod_name is not None:
        spec_name = mod_name
        endmatch = _ENDMODULE_RE
        inmod = True
    elif prog_name is not None:
        spec_name = prog_name
        endmatch = _ENDPROGRAM_RE
        inmod = False
    else:
        raise ParseInternalError("One of <mod_name> or <prog_name> must be used")
    # End if
    if logger is not None:
        ctx = context_string(pobj, nodir=True)
        msg = "Parsing specification of {}{}"
        logger.debug(msg.format(spec_name, ctx))
    # End if

    inspec = True
    mtables = list()
    while inspec and (statements is not None):
        while len(statements) > 0:
            statement = statements.pop(0)
            # End program or module
            pmatch = endmatch.match(statement)
            asmatch = _ARG_TABLE_START_RE.match(statement)
            if pmatch is not None:
                # We never found a contains statement
                inspec = False
                break
            elif asmatch is not None:
                # Put table statement back to re-read
                statements.insert(0, statement)
                statements, new_tbls = parse_preamble_data(statements,
                                                           pobj, spec_name,
                                                           endmatch, logger)
                for tbl in new_tbls:
                    title = tbl.table_name
                    if title in mtables:
                        errmsg = duplicate_header(mtables[title], tbl)
                        raise CCPPError(errmsg)
                    elif logger is not None:
                        ctx = tbl.start_context()
                        mtype = tbl.table_type
                        msg = "Adding metadata from {}, {}{}"
                        logger.debug(msg.format(mtype, title, ctx))
                    # End if
                    mtables.append(tbl)
                # End if
                inspec = pobj.in_region('MODULE', region_name=mod_name)
                break
            elif is_contains_statement(statement, inmod):
                inspec = False
                break
            # End if
        # End while
        if inspec and (len(statements) == 0):
            statements = read_statements(pobj)
        # End if
    # End while
    return statements, mtables

########################################################################

def parse_program(pobj, statements, logger=None):
    # The first statement should be a program statement, grab the name
    pmatch = _PROGRAM_RE.match(statements[0])
    if pmatch is None:
        raise ParseSyntaxError('PROGRAM statement', statements[0])
    # End if
    prog_name = pmatch.group(1)
    pobj.enter_region('PROGRAM', region_name=prog_name, nested_ok=False)
    if logger is not None:
        ctx = context_string(pobj, nodir=True)
        msg = "Parsing Fortran program, {}{}"
        logger.debug(msg.format(prog_name, ctx))
    # End if
    # After the program name is the specification part
    statements, mtables = parse_specification(pobj, statements[1:],
                                              prog_name=prog_name,
                                              logger=logger)
    # We really cannot have tables inside a program's executable section
    # Just read until end
    statements = read_statements(pobj, statements)
    inprogram = True
    while inprogram and (statements is not None):
        while len(statements) > 0:
            statement = statements.pop(0)
            # End program
            pmatch = _ENDPROGRAM_RE.match(statement)
            if pmatch is not None:
                prog_name = pmatch.group(1)
                pobj.leave_region('PROGRAM', region_name=prog_name)
                inprogram = False
            # End if
        # End while
        if inprogram and (len(statements) == 0):
            statements = read_statements(pobj)
        # End if
    # End while
    return statements, mtables

########################################################################

def parse_module(pobj, statements, logger=None):
    # The first statement should be a module statement, grab the name
    pmatch = _MODULE_RE.match(statements[0])
    if pmatch is None:
        raise ParseSyntaxError('MODULE statement', statements[0])
    # End if
    mod_name = pmatch.group(1)
    pobj.enter_region('MODULE', region_name=mod_name, nested_ok=False)
    if logger is not None:
        ctx = context_string(pobj, nodir=True)
        msg = "Parsing Fortran module, {}{}"
        logger.debug(msg.format(mod_name, ctx))
    # End if
    # After the module name is the specification part
    statements, mtables = parse_specification(pobj, statements[1:], mod_name=mod_name, logger=logger)
    # Look for metadata tables
    statements = read_statements(pobj, statements)
    inmodule = pobj.in_region('MODULE', region_name=mod_name)
    active_table = None
    while inmodule and (statements is not None):
        while statements:
            statement = statements.pop(0)
            # End module
            pmatch = _ENDMODULE_RE.match(statement)
            asmatch = _ARG_TABLE_START_RE.match(statement)
            if asmatch is not None:
                active_table = asmatch.group(1)
            elif pmatch is not None:
                mod_name = pmatch.group(1)
                pobj.leave_region('MODULE', region_name=mod_name)
                inmodule = False
                break
            elif active_table is not None:
                statements, mheader = parse_scheme_metadata(statements, pobj,
                                                            mod_name,
                                                            active_table,
                                                            logger)
                if mheader is not None:
                    title = mheader.table_name
                    if title in mtables:
                        errmsg = duplicate_header(mtables[title], mheader)
                        raise CCPPError(errmsg)
                    elif logger is not None:
                        mtype = mheader.table_type
                        ctx = mheader.start_context()
                        msg = "Adding metadata from {}, {}{}"
                        logger.debug(msg.format(mtype, title, ctx))
                    # End if
                    mtables.append(mheader)
                # End if
                active_table = None
                inmodule = pobj.in_region('MODULE', region_name=mod_name)
                break
            # End if
        # End while
        if inmodule and (statements is not None) and (len(statements) == 0):
            statements = read_statements(pobj)
        # End if
    # End while
    return statements, mtables

########################################################################

def parse_fortran_file(filename, preproc_defs=None, logger=None):
    mtables = list()
    type_dict = {}
    pobj = read_file(filename, preproc_defs=preproc_defs, logger=logger)
    pobj.reset_pos()
    curr_line, _ = pobj.curr_line()
    statements = line_statements(curr_line)
    while statements is not None:
        if not statements:
            statements = read_statements(pobj)
        # End if
        statement = statements.pop(0)
        if _PROGRAM_RE.match(statement) is not None:
            # push statement back so parse_program can use it
            statements.insert(0, statement)
            statements, ptables = parse_program(pobj, statements, logger=logger)
            mtables.extend(ptables)
        elif _MODULE_RE.match(statement) is not None:
            # push statement back so parse_module can use it
            statements.insert(0, statement)
            statements, ptables = parse_module(pobj, statements, logger=logger)
            mtables.extend(ptables)
        # End if
        if (statements is not None) and (len(statements) == 0):
            statements = read_statements(pobj)
        # End if
    # End while
    return mtables

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
    from parse_tools import register_fortran_ddt_name
    fpath = '/Users/goldy/scratch/foo'
    fnames = ['GFS_PBL_generic.F90', 'GFS_rad_time_vary.fv3.F90',
              'GFS_typedefs.F90']
    register_fortran_ddt_name('GFS_control_type')
    register_fortran_ddt_name('GFS_data_type')
    for fname in fnames:
        fpathname = os.path.join(fpath, fname)
        if os.path.exists(fpathname):
            mh = parse_fortran_file(fpathname, preproc_defs={'CCPP':1})
            for h in mh:
                print('{}: {}'.format(fname, h))
            # End for
        # End if
    # End for
# End if
