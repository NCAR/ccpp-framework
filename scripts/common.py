#!/usr/bin/env python

import keyword
import logging
import os
import re
import subprocess
import sys

CCPP_ERROR_FLAG_VARIABLE = 'ccpp_error_flag'
CCPP_ERROR_MSG_VARIABLE  = 'ccpp_error_message'
CCPP_LOOP_COUNTER        = 'ccpp_loop_counter'
CCPP_BLOCK_NUMBER        = 'ccpp_block_number'
CCPP_THREAD_NUMBER       = 'ccpp_thread_number'

CCPP_TYPE = 'ccpp_t'

# SCRIPTDIR is the directory where ccpp_prebuild.py and its Python modules are located
SCRIPTDIR = os.path.abspath(os.path.dirname(__file__))

# SRCDIR is the directory where the CCPP framework source code (C, Fortran) is located
SRCDIR = os.path.abspath(os.path.join(SCRIPTDIR, os.pardir, 'src'))

# Definition of variables (metadata tables) that are provided by CCPP
CCPP_INTERNAL_VARIABLE_DEFINITON_FILE = os.path.join(SRCDIR, 'ccpp_types.F90')

# List of internal variables provided by the CCPP
CCPP_INTERNAL_VARIABLES = {
    CCPP_ERROR_FLAG_VARIABLE : 'cdata%errflg',
    CCPP_ERROR_MSG_VARIABLE  : 'cdata%errmsg',
    CCPP_LOOP_COUNTER        : 'cdata%loop_cnt',
    CCPP_BLOCK_NUMBER        : 'cdata%blk_no',
    CCPP_THREAD_NUMBER       : 'cdata%thrd_no',
    }

STANDARD_VARIABLE_TYPES = [ 'character', 'integer', 'logical', 'real' ]
STANDARD_CHARACTER_TYPE = 'character'

# For static build
CCPP_STATIC_API_MODULE = 'ccpp_static_api'
CCPP_STATIC_SUBROUTINE_NAME = 'ccpp_physics_{stage}'

def execute(cmd, abort = True):
    """Runs a local command in a shell. Waits for completion and
    returns status, stdout and stderr. If abort = True, abort in
    case an error occurs during the execution of the command."""

    # Set debug to true if logging level is debug
    debug = logging.getLogger().getEffectiveLevel() == logging.DEBUG

    logging.debug('Executing "{0}"'.format(cmd))
    p = subprocess.Popen(cmd, stdout = subprocess.PIPE,
                         stderr = subprocess.PIPE, shell = True)
    (stdout, stderr) = p.communicate()
    status = p.returncode
    if debug:
        message = 'Execution of "{0}" returned with exit code {1}\n'.format(cmd, status)
        message += '    stdout: "{0}"\n'.format(stdout.rstrip('\n'))
        message += '    stderr: "{0}"'.format(stderr.rstrip('\n'))
        logging.debug(message)
    if not status == 0:
        message = 'Execution of command {0} failed, exit code {1}\n'.format(cmd, status)
        message += '    stdout: "{0}"\n'.format(stdout.rstrip('\n'))
        message += '    stderr: "{0}"'.format(stderr.rstrip('\n'))
        if abort:
            raise Exception(message)
        else:
            logging.error(message)
    return (status, stdout.rstrip('\n'), stderr.rstrip('\n'))

def indent(elem, level=0):
    """Subroutine for writing "pretty" XML; copied from
    http://effbot.org/zone/element-lib.htm#prettyprint"""
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
          elem.tail = i

def split_var_name_and_array_reference(var_name):
    """Split an expression like foo(:,a,1:ddt%ngas)
    into components foo and (:,a,1:ddt%ngas)."""
    actual_var_name = None
    array_reference = None
    # Search for first pair of parentheses from the end of the string
    parentheses = 0
    i = len(var_name)-1
    while i>=0:
        if var_name[i] == ')':
            parentheses += 1
        elif var_name[i] == '(':
            parentheses -= 1
        if parentheses == 0:
            actual_var_name = var_name[:i]
            array_reference = var_name[i:]
            break
        i -= 1
    return (actual_var_name, array_reference)

def encode_container(*args):
    """Encodes a container, i.e. the location of a metadata table for CCPP.
    Currently, there are three possibilities with different numbers of input
    arguments: module, module+typedef, module+scheme+subroutine."""
    if len(args)==3:
        container = 'MODULE_{0} SCHEME_{1} SUBROUTINE_{2}'.format(*args)
    elif len(args)==2:
        container = 'MODULE_{0} TYPE_{1}'.format(*args)
    elif len(args)==1:
        container = 'MODULE_{0}'.format(*args)
    else:
        raise Exception("encode_container not implemented for {0} arguments".format(len(args)))
    return container

def decode_container(container):
    """Decodes a container, i.e. the description of a location of a metadata table
    for CCPP. Currently, there are three possibilities with different numbers of
    input arguments: module, module+typedef, module+scheme+subroutine."""
    items = container.split(' ')
    if not len(items) in [1, 2, 3]:
        raise Exception("decode_container not implemented for {0} items".format(len(items)))
    for i in range(len(items)):
        items[i] = items[i][:items[i].find('_')] + ' ' + items[i][items[i].find('_')+1:]
    return ' '.join(items)

def decode_container_as_dict(container):
    """Decodes a container, i.e. the description of a location of a metadata table
    for CCPP. Currently, there are three possibilities with different numbers of
    input arguments: module, module+typedef, module+scheme+subroutine. Return
    a dictionary with possible keys MODULE, TYPE, SCHEME, SUBROUTINE."""
    items = container.split(' ')
    if not len(items) in [1, 2, 3]:
        raise Exception("decode_container not implemented for {0} items".format(len(items)))
    itemsdict = {}
    for i in range(len(items)):
        key, value = (items[i][:items[i].find('_')], items[i][items[i].find('_')+1:])
        itemsdict[key] = value
    return itemsdict

def escape_tex(text):
    """Substitutes characters for generating LaTeX sources files from Python."""
    return text.replace(
                '%', '\%').replace(
                '_', '\_')

def isstring(s):
    """Return true if a variable is a string"""
    # We use Python 3
    if (sys.version_info.major == 3):
        return isinstance(s, str)
    # We use Python 2
    elif (sys.version_info.major == 2):
        return isinstance(s, str)
    else:
        raise Exception('Unknown Python version')

def string_to_python_identifier(string):
    """Replaces forbidden characters in strings with standard substitutions
    so that the result is a valid Python object (variable, function) name.
    At this point, it only converts characters found in the units attributes.
    A check for allowed characters in Python v2 catches missing conversions."""
    # Replace whitespaces with underscores
    string = string.replace(" ","_")
    # Replace decimal points with _p_
    string = string.replace(".","_p_")
    # Replace dashes and minus sign with _minus_
    string = string.replace("-","_minus_")
    # Replace plus signs with _plus_
    string = string.replace("+","_plus_")
    # Test that the resulting string is a valid Python identifier
    if re.match("[_A-Za-z][_a-zA-Z0-9]*$", string) and not keyword.iskeyword(string):
        return string
    else:
        raise Exception("Resulting string '{0}' is not a valid Python identifier".format(string))
