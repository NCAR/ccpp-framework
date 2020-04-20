#! /usr/bin/env python
"""
Classes to parse C preprocessor lines and to maintain a stack to allow
inclusion and exclusion of lines based on preprocessor symbol definitions.
"""

# Python library imports
import re
import ast
# CCPP Framewor imports
from parse_source import ParseSyntaxError

__defined_re__ = re.compile(r"defined\s+([A-Za-z0-9_]+)")

###############################################################################

class PreprocError(ValueError):
    """Class to report preprocessor line errors"""
    def __init__(self, message):
        super(PreprocError, self).__init__(message)

########################################################################

def preproc_bool(value):
    """Turn a preprocessor value into a boolean"""
    if isinstance(value, bool):
        line_val = value
    else:
        try:
            ival = int(value)
            line_val = ival != 0
        except ValueError:
            line_val = value != "0"
        # end try
    # end if
    return line_val

########################################################################

def preproc_item_value(item, preproc_defs):
    """Find the value of a preproc <item> (part of a parsed
    preprocessor line)"""
    value = False
    if isinstance(item, ast.Expr):
        value = preproc_item_value(item.value, preproc_defs)
    elif isinstance(item, ast.Call):
        func = item.func.id
        # The only 'function' we know how to process is "defined"
        if func == "defined":
            args = item.args
            if len(args) != 1:
                raise PreprocError("Invalid defined statement, {}".format(ast.dump(item)))
            # end if
            symbol = args[0].id
            # defined is True as long as we know about the symbol
            value = symbol in preproc_defs
        elif func == "notdefined":
            args = item.args
            if len(args) != 1:
                raise PreprocError("Invalid defined statement, {}".format(ast.dump(item)))
            # end if
            symbol = args[0].id
            # notdefined is True as long as we do not know about the symbol
            value = symbol not in preproc_defs
        else:
            raise PreprocError("Cannot parse function {}".format(func))
        # end if
    elif isinstance(item, ast.BoolOp):
        left_val = preproc_item_value(item.values[0], preproc_defs)
        right_val = preproc_item_value(item.values[1], preproc_defs)
        oper = item.op
        if isinstance(oper, ast.And):
            value = preproc_bool(left_val) and preproc_bool(right_val)
        elif isinstance(oper, ast.Or):
            value = preproc_bool(left_val) or preproc_bool(right_val)
        else:
            raise PreprocError("Unknown binary operator, {}".format(oper))
        # end if
    elif isinstance(item, ast.UnaryOp):
        val = preproc_item_value(item.operand, preproc_defs)
        oper = item.op
        if isinstance(oper, ast.Not):
            value = not preproc_bool(val)
        else:
            raise PreprocError("Unknown unary operator, {}".format(oper))
        # end if
    elif isinstance(item, ast.Compare):
        left_val = preproc_item_value(item.left, preproc_defs)
        value = True
        for index in range(len(item.ops)):
            oper = item.ops[index]
            rcomp = item.comparators[index]
            right_val = preproc_item_value(rcomp, preproc_defs)
            if isinstance(oper, ast.Eq):
                value = value and (left_val == right_val)
            elif isinstance(oper, ast.NotEq):
                value = value and (left_val != right_val)
            else:
                # What remains are numerical comparisons, use integers
                try:
                    ilval = int(left_val)
                    irval = int(right_val)
                    if isinstance(oper, ast.Gt):
                        value = value and (ilval > irval)
                    elif isinstance(oper, ast.GtE):
                        value = value and (ilval >= irval)
                    elif isinstance(oper, ast.Lt):
                        value = value and (ilval < irval)
                    elif isinstance(oper, ast.LtE):
                        value = value and (ilval <= irval)
                    else:
                        emsg = "Unknown comparison operator, {}"
                        raise PreprocError(emsg.format(oper))
                    # end if
                except ValueError:
                    value = False
                # end try
            # end if
        # end for
    elif isinstance(item, ast.Name):
        id_key = item.id
        if id_key in preproc_defs:
            value = preproc_defs[id_key]
        else:
            value = id_key
        # end if
    elif isinstance(item, ast.Num):
        value = item.n
    else:
        raise PreprocError("Cannot parse {}".format(item))
    # end if
    return value

########################################################################

def parse_preproc_line(line, preproc_defs):
    """Parse a preprocessor line into a tree that can be evaluated"""
    # Scan line and translate to python syntax
    inchar = None # Character context
    line_len = len(line)
    pline = ""
    index = 0
    while index < line_len:
        if (line[index] == '"') or (line[index] == "'"):
            if inchar == line[index]:
                inchar = None
            elif inchar is None:
                inchar = line[index]
            # Else in character context, just copy
            # end if
            pline = pline + line[index]
        elif inchar is not None:
            # In character context, just copy current character
            pline = pline + line[index]
        elif line[index:index+2] == '&&':
            pline = pline + 'and'
            index = index + 1
        elif line[index:index+2] == '||':
            pline = pline + 'or'
            index = index + 1
        elif line[index] == '!':
            pline = pline + "not"
        else:
            match = __defined_re__.match(line[index:])
            if match is None:
                # Just copy current character
                pline = pline + line[index]
            else:
                mlen = len(match.group(0))
                pline = pline + "defined ({})".format(match.group(1))
                index = index + mlen - 1
            # end if
        # end if
        index = index + 1
    # end while
    try:
        ast_line = ast.parse(pline)
        # We should only have one 'statement'
        if len(ast_line.body) != 1:
            line_val = False
            success = False
        else:
            value = preproc_item_value(ast_line.body[0], preproc_defs)
            line_val = preproc_bool(value)
            success = True
        # end if
    except SyntaxError:
        line_val = False
        success = False
    # end try
    return line_val, success

########################################################################

class PreprocStack(object):
    """Class to handle preprocess regions"""

    ifdef_re = re.compile(r"#\s*ifdef\s+(.*)")
    ifndef_re = re.compile(r"#\s*ifndef\s+(.*)")
    if_re = re.compile(r"#\s*if([^dn].*)")
    elif_re = re.compile(r"#\s*elif\s(.*)")
    ifelif_re = re.compile(r"#\s*(?:el)?if\s(.*)")
    else_re = re.compile(r"#\s*else")
    end_re = re.compile(r"#\s*endif")
    define_re = re.compile(r"#\s*define\s+([A-Za-z0-9_]+)\s+([^\s]*)")
    undef_re = re.compile(r"#\s*undef\s+([A-Za-z0-9_]+)")

    def __init__(self):
        """Initialize our region stack"""
        self._region_stack = list()

    @staticmethod
    def process_if_line(line, preproc_defs):
        """Decide if (el)?if <line> represents a True or False condition.
        Return True iff the line evaluates to a True condition.
        <preproc_defs> is a dictionary where each key is a symbol which
        can be tested (e.g., 'FOO' in #ifdef FOO). The value is that
        symbol's preprocessor value, if provided (e.g., 3 for -DFOO=3),
        otherwise, it is None.
        Return second logical value of False if we are unable to process <line>
        >>> PreprocStack().process_if_line("#if 0", {'CCPP':1})
        (False, True)
        >>> PreprocStack().process_if_line("#if 1", {'CCPP':1})
        (True, True)
        >>> PreprocStack().process_if_line("#elif 0", {'CCPP':1})
        (False, True)
        >>> PreprocStack().process_if_line("#elif 1", {'CCPP':1})
        (True, True)
        >>> PreprocStack().process_if_line("#if ( WRF_CHEM == 1 )", {'CCPP':1})
        (False, True)
        >>> PreprocStack().process_if_line("#if ( WRF_CHEM == 1 )", {'WRF_CHEM':1})
        (True, True)
        >>> PreprocStack().process_if_line("#if ( WRF_CHEM == 1 )", {'WRF_CHEM':0})
        (False, True)
        >>> PreprocStack().process_if_line("#if (WRF_CHEM == 0)", {'CCPP':1})
        (False, True)
        >>> PreprocStack().process_if_line("#if (WRF_CHEM == 0)", {'WRF_CHEM':1})
        (False, True)
        >>> PreprocStack().process_if_line("#if (WRF_CHEM == 0)", {'WRF_CHEM':0})
        (True, True)
        >>> PreprocStack().process_if_line("#if defined(CCPP)", {'CCPP':1})
        (True, True)
        >>> PreprocStack().process_if_line("#if defined(CCPP)", {'CCPP':0})
        (True, True)
        >>> PreprocStack().process_if_line("#if defined(CCPP)", {})
        (False, True)
        >>> PreprocStack().process_if_line("#if ( defined WACCM_PHYS )", {'CCPP':1})
        (False, True)
        >>> PreprocStack().process_if_line("#if (defined WACCM_PHYS)", {'WACCM_PHYS':1})
        (True, True)
        >>> PreprocStack().process_if_line("#if (defined WACCM_PHYS)", {'WACCM_PHYS':0})
        (True, True)
        >>> PreprocStack().process_if_line("#if (defined(DM_PARALLEL) && (! defined(STUBMPI)))", {})
        (False, True)
        >>> PreprocStack().process_if_line("#if (defined(DM_PARALLEL) && (! defined(STUBMPI)))", {'DM_PARALLEL':1})
        (True, True)
        >>> PreprocStack().process_if_line("#if (defined(DM_PARALLEL) && (! defined(STUBMPI)))", {'DM_PARALLEL':1, 'STUBMPI':0})
        (False, True)
        >>> PreprocStack().process_if_line("#if (defined(DM_PARALLEL) && (! defined(STUBMPI)))", {'STUBMPI':0})
        (False, True)
        >>> PreprocStack().process_if_line("# if (defined(DM_PARALLEL) || (! defined(STUBMPI)))", {})
        (True, True)
        >>> PreprocStack().process_if_line("#if (defined(DM_PARALLEL) || (! defined(STUBMPI)))", {'DM_PARALLEL':1})
        (True, True)
        >>> PreprocStack().process_if_line("#if (defined(DM_PARALLEL) || (! defined(STUBMPI)))", {'DM_PARALLEL':1, 'STUBMPI':0})
        (True, True)
        >>> PreprocStack().process_if_line("#if (defined(DM_PARALLEL) || (! defined(STUBMPI)))", {'STUBMPI':0})
        (False, True)
        >>> PreprocStack().process_if_line("#elif ( WRF_CHEM == 1 )", {'CCPP':1})
        (False, True)
        >>> PreprocStack().process_if_line("#elif ( WRF_CHEM == 1 )", {'WRF_CHEM':1})
        (True, True)
        >>> PreprocStack().process_if_line("#elif ( WRF_CHEM == 1 )", {'WRF_CHEM':0})
        (False, True)
        >>> PreprocStack().process_if_line("#elif (WRF_CHEM == 0)", {'CCPP':1})
        (False, True)
        >>> PreprocStack().process_if_line("# elif (WRF_CHEM == 0)", {'WRF_CHEM':1})
        (False, True)
        >>> PreprocStack().process_if_line("#elif (WRF_CHEM == 0)", {'WRF_CHEM':0})
        (True, True)
        >>> PreprocStack().process_if_line("#if defined(CCPP) &&", {'CCPP':1})
        (False, False)
        """
        match = PreprocStack.ifelif_re.match(line)
        if match is None:
            return False, False # This is not a preproc line
        # end if
        value, okay = parse_preproc_line(match.group(1).strip(), preproc_defs)
        return value, okay

    def process_line(self, line, preproc_defs, pobj, logger):
        """Read <line> and return if it is a preprocessor line.
        In addition, if it is a preprocessor line enter an appropriate region
        if indicated by <preproc_defs>."""
        sline = line.strip()
        is_preproc_line = PreprocStack.is_preproc_line(line)
        if is_preproc_line and (preproc_defs is not None):
            match = PreprocStack.ifdef_re.match(sline)
            if match is not None:
                start_region = match.group(1) in preproc_defs
                if start_region and (logger is not None):
                    lmsg = "Preproc: Starting True region ({}) on line {}"
                    logger.debug(lmsg.format(match.group(1), pobj))
                # end if
                self.enter_region(start_region)
            # end if
            if match is None:
                match = PreprocStack.ifndef_re.match(sline)
                if match is not None:
                    start_region = match.group(1) not in preproc_defs
                    if (not start_region) and (logger is not None):
                        lmsg = "Preproc: Starting False region ({}) on line {}"
                        logger.debug(lmsg.format(match.group(1), pobj))
                    # end if
                    self.enter_region(start_region)
                # end if
            # end if
            if match is None:
                match = PreprocStack.if_re.match(sline)
                if match is not None:
                    line_val, success = self.process_if_line(sline,
                                                             preproc_defs)
                    self.enter_region(line_val)
                    if (not success) and (logger is not None):
                        lmsg = "WARNING: Preprocessor #if statement not handled, at {}"
                        logger.warning(lmsg.format(pobj))
                    # end if
                # end if
            # end if
            if match is None:
                match = PreprocStack.elif_re.match(sline)
                if match is not None:
                    line_val, success = self.process_if_line(sline,
                                                             preproc_defs)
                    self.modify_region(line_val)
                    if (not success) and (logger is not None):
                        lmsg = "WARNING: Preprocessor #elif statement not handled, at {}"
                        logger.warning(lmsg.format(pobj))
                    # end if
                # end if
            # end if
            if match is None:
                match = PreprocStack.else_re.match(sline)
                if match is not None:
                    # Always try to use True for else, modify_region will set
                    # correct value
                    self.modify_region(True)
                # end if
            # end if
            if match is None:
                match = PreprocStack.end_re.match(sline)
                if match is not None:
                    self.exit_region(pobj)
                # end if
            # end if
            if (match is None) and self.in_true_region():
                match = PreprocStack.define_re.match(sline)
                if match is not None:
                    # Add (or replace) a symbol to our defs
                    preproc_defs[match.group(1)] = match.group(2)
                # end if
            # end if
            if (match is None) and self.in_true_region():
                match = PreprocStack.undef_re.match(sline)
                if (match is not None) and (match.group(1) in preproc_defs):
                    # Remove a symbol from our defs
                    del preproc_defs[match.group(1)]
                # end if
            # end if
        # Ignore all other lines
        # end if
        return is_preproc_line

    def enter_region(self, valid):
        """Enter a new region (if, ifdef, ifndef) which may
        currently be valid"""
        self._region_stack.append([valid, valid])

    def exit_region(self, pobj):
        """Leave the current (innermost) region"""
        if not self._region_stack:
            emsg = "#endif found with no matching #if, #ifdef, or #ifndef"
            raise ParseSyntaxError(emsg, context=pobj)
        # end if
        self._region_stack.pop()

    def modify_region(self, valid):
        """Possibly modify the current (innermost) region.
        A region can be modified from False to True.
        Any attempt to modify a region which has been True results in False
        because after a region has been True, any #elif or #else must skipped.
        """
        curr_region = self._region_stack.pop()
        if curr_region[0]:
            self._region_stack.append([curr_region[0], False])
        else:
            self._region_stack.append([curr_region[0], valid])
        # end if

    def in_true_region(self):
        """Return True iff the current line should be processed"""
        true_region = True
        for region in self._region_stack:
            if not region[1]:
                true_region = False
                break
            # end if
        # end for
        return true_region

    @staticmethod
    def is_preproc_line(line):
        """Return True iff line appears to be a preprocessor line"""
        return line.lstrip()[0] == '#'

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
# end if
