#! /usr/bin/env python
"""
Classes to parse C preprocessor lines and to maintain a stack to allow
inclusion and exclusion of lines based on preprocessor symbol definitions.
"""

import re
import ast

__defined_re__ = re.compile(r"defined\s+([A-Za-z0-9_]+)")

###############################################################################

class PreprocError(ValueError):
    "Class to report preprocessor line errors"
    def __init__(self, message):
        super(PreprocError, self).__init__(message)

########################################################################

def preproc_bool(value):
    # Turn a preprocessor value into a boolean
    if isinstance(value, bool):
        line_val = value
    else:
        try:
            ival = int(value)
            line_val = ival != 0
        except ValueError as ve:
            line_val = value != "0"
        # End try
    # End if
    return line_val

########################################################################

def preproc_item_value(item, preproc_defs):
    "Find the value of a preproc <item> (part of a parsed preprocessor line)"
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
            # End if
            symbol = args[0].id
            # defined is True as long as we know about the symbol
            value = symbol in preproc_defs
        elif func == "notdefined":
            args = item.args
            if len(args) != 1:
                raise PreprocError("Invalid defined statement, {}".format(ast.dump(item)))
            # End if
            symbol = args[0].id
            # notdefined is True as long as we do not know about the symbol
            value = symbol not in preproc_defs
        else:
            raise PreprocError("Cannot parse function {}".format(func))
        # End if
    elif isinstance(item, ast.BoolOp):
        left_val = preproc_item_value(item.values[0], preproc_defs)
        right_val = preproc_item_value(item.values[1], preproc_defs)
        op = item.op
        if isinstance(op, ast.And):
            value = preproc_bool(left_val) and preproc_bool(right_val)
        elif isinstance(op, ast.Or):
            value = preproc_bool(left_val) or preproc_bool(right_val)
        else:
            raise PreprocError("Unknown binary operator, {}".format(op))
        # End if
    elif isinstance(item, ast.UnaryOp):
        val = preproc_item_value(item.operand, preproc_defs)
        op = item.op
        if isinstance(op, ast.Not):
            value = not preproc_bool(val)
        else:
            raise PreprocError("Unknown unary operator, {}".format(op))
        # End if
    elif isinstance(item, ast.Compare):
        left_val = preproc_item_value(item.left, preproc_defs)
        value = True
        for index in range(len(item.ops)):
            op = item.ops[index]
            rcomp = item.comparators[index]
            right_val = preproc_item_value(rcomp, preproc_defs)
            if isinstance(op, ast.Eq):
                value = value and (left_val == right_val)
            elif isinstance(op, ast.NotEq):
                value = value and (left_val != right_val)
            else:
                # What remains are numerical comparisons, use integers
                try:
                    ilval = int(left_val)
                    irval = int(right_val)
                    if isinstance(op, ast.Gt):
                        value = value and (ilval > irval)
                    elif isinstance(op, ast.GtE):
                        value = value and (ilval >= irval)
                    elif isinstance(op, ast.Lt):
                        value = value and (ilval < irval)
                    elif isinstance(op, ast.LtE):
                        value = value and (ilval <= irval)
                    else:
                        raise PreprocError("Unknown comparison operator, {}".format(op))
                    # End if
                except ValueError as ve:
                    value = False
                # End try
            # End if
        # End for
    elif isinstance(item, ast.Name):
        id = item.id
        if id in preproc_defs:
            value = preproc_defs[id]
        else:
            value = id
        # End if
    elif isinstance(item, ast.Num):
        value = item.n
    else:
        raise PreprocError("Cannot parse {}".format(item))
    # End if
    return value

########################################################################

def parse_preproc_line(line, preproc_defs):
    "Parse a preprocessor line into a tree that can be evaluated"
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
            # End if
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
            # End if
        # End if
        index = index + 1
    # End while
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
        # End if
    except SyntaxError as se:
        line_val = False
        success = False
    # End try
    return line_val, success

########################################################################

class PreprocStack(object):
    "Class to handle preprocess regions"

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
        self._region_stack = list()

    def process_if_line(self, line, preproc_defs):
        """Decide (el)?if <line> represents a True or False condition.
        Return True iff the line evaluates to a True condition.
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
        else:
            value, ok = parse_preproc_line(match.group(1).strip(), preproc_defs)
            return value, ok
        # End if

    def process_line(self, line, preproc_defs, pobj, logger):
        sline = line.strip()
        is_preproc_line = PreprocStack.is_preproc_line(line)
        if is_preproc_line and (preproc_defs is not None):
            match = PreprocStack.ifdef_re.match(sline)
            if match is not None:
                if match.group(1) in preproc_defs:
                    start_region = preproc_defs[match.group(1)] != 0
                else:
                    start_region = False
                # End if
                if start_region and (logger is not None):
                    logger.debug('Preproc: Starting True region ({}) on line {}'.format(match.group(1), pobj))
                # End if
                self.enter_region(start_region)
            # End if
            if match is None:
                match = PreprocStack.ifndef_re.match(sline)
                if match is not None:
                    if match.group(1) in preproc_defs:
                        start_region = preproc_defs[match.group(1)] == 0
                    else:
                        start_region = True
                    # End if
                    if (not start_region) and (logger is not None):
                        logger.debug('Preproc: Starting False region ({}) on line {}'.format(match.group(1), pobj))
                    # End if
                    self.enter_region(start_region)
                # End if
            # End if
            if match is None:
                match = PreprocStack.if_re.match(sline)
                if match is not None:
                    line_val, success = self.process_if_line(sline, preproc_defs)
                    self.enter_region(line_val)
                    if (not success) and (logger is not None):
                        logger.warning("WARNING: Preprocessor #if statement not handled, at {}".format(pobj))
                    # End if
                # End if
            # End if
            if match is None:
                match = PreprocStack.elif_re.match(sline)
                if match is not None:
                    line_val, success = self.process_if_line(sline, preproc_defs)
                    self.modify_region(line_val)
                    if (not success) and (logger is not None):
                        logger.warning("WARNING: Preprocessor #elif statement not handled, at {}".format(pobj))
                    # End if
                # End if
            # End if
            if match is None:
                match = PreprocStack.else_re.match(sline)
                if match is not None:
                    # Always try to use True for else, modify_region will set
                    # correct value
                    self.modify_region(True)
                # End if
            # End if
            if match is None:
                match = PreprocStack.end_re.match(sline)
                if match is not None:
                    self.exit_region()
                # End if
            # End if
            if (match is None) and self.in_true_region():
                match = PreprocStack.define_re.match(sline)
                if match is not None:
                    # Add (or replace) a symbol to our defs
                    preproc_defs[match.group(1)] = match.group(2)
                # End if
            # End if
            if (match is None) and self.in_true_region():
                match = PreprocStack.undef_re.match(sline)
                if (match is not None) and (match.group(1) in preproc_defs):
                    # Remove a symbol from our defs
                    del preproc_defs[match.group(1)]
                # End if
            # End if
        # Ignore all other lines
        # End if
        return is_preproc_line

    def enter_region(self, valid):
        "Enter a new region (if, ifdef, ifndef) which may currently be valid"
        self._region_stack.append([valid, valid])

    def exit_region(self):
        "Leave the current (innermost) region"
        if len(self._region_stack) == 0:
            raise ParseSyntaxError("#endif found with no matching #if, #ifdef, or #ifndef", context=pobj)
        # End if
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
        # End if

    def in_true_region(self):
        "Return True iff the current line should be processed"
        true_region = True
        for region in self._region_stack:
            if not region[1]:
                true_region = False
                break
            # End if
        # End for
        return true_region

    @classmethod
    def is_preproc_line(self, line):
        'Return True iff line appears to be a preprocessor line'
        return line.lstrip()[0] == '#'

########################################################################

if __name__ == "__main__":
    import doctest
    doctest.testmod()
# End if
