#!/usr/bin/env python

"""
Parse a host-model registry XML file and return the captured variables.
"""

import sys
import os
import os.path
import logging
import six
import xml.etree.ElementTree as ET
try:
    from distutils.spawn import find_executable
    xmllint = find_executable('xmllint')
except ImportError as ie:
    xmllint = None

logger = logging.getLogger(__name__)
logger.addHandler(logging.StreamHandler())

###############################################################################
class HostRegAbort(ValueError):
    "Class so main can log user errors without backtrace"
    def __init__(self, message):
        super(HostRegAbort, self).__init__(message)

###############################################################################
def abort(message):
###############################################################################
    logger.error(message)
    raise HostRegAbort(message)

###############################################################################
def run_cmd(cmd, input_str=None, from_dir=None, verbose=None,
            arg_stdout=_hack, arg_stderr=_hack, env=None, combine_output=False):
###############################################################################
    """
    Wrapper around subprocess to make it much more convenient to run shell commands

    >>> run_cmd('ls file_i_hope_doesnt_exist')[0] != 0
    True
    """
    import subprocess # Not safe to do globally, module not available in older pythons

    # Real defaults for these value should be subprocess.PIPE
    if arg_stdout is _hack:
        arg_stdout = subprocess.PIPE
    elif isinstance(arg_stdout, six.string_types):
        arg_stdout = _convert_to_fd(arg_stdout, from_dir)

    if arg_stderr is _hack:
        arg_stderr = subprocess.STDOUT if combine_output else subprocess.PIPE
    elif isinstance(arg_stderr, six.string_types):
        arg_stderr = _convert_to_fd(arg_stdout, from_dir)

    if (verbose != False and (verbose or logger.isEnabledFor(logging.DEBUG))):
        logger.info("RUN: {}\nFROM: {}".format(cmd, os.getcwd() if from_dir is None else from_dir))

    if (input_str is not None):
        stdin = subprocess.PIPE
    else:
        stdin = None

    proc = subprocess.Popen(cmd,
                            shell=True,
                            stdout=arg_stdout,
                            stderr=arg_stderr,
                            stdin=stdin,
                            cwd=from_dir,
                            env=env)

    output, errput = proc.communicate(input_str)
    if output is not None:
        try:
            output = output.decode('utf-8', errors='ignore').strip()
        except AttributeError:
            pass
    if errput is not None:
        try:
            errput = errput.decode('utf-8', errors='ignore').strip()
        except AttributeError:
            pass

    stat = proc.wait()
    if six.PY2:
        if isinstance(arg_stdout, file): # pylint: disable=undefined-variable
            arg_stdout.close() # pylint: disable=no-member
        if isinstance(arg_stderr, file) and arg_stderr is not arg_stdout: # pylint: disable=undefined-variable
            arg_stderr.close() # pylint: disable=no-member
    else:
        if isinstance(arg_stdout, io.IOBase):
            arg_stdout.close() # pylint: disable=no-member
        if isinstance(arg_stderr, io.IOBase) and arg_stderr is not arg_stdout:
            arg_stderr.close() # pylint: disable=no-member


    if (verbose != False and (verbose or logger.isEnabledFor(logging.DEBUG))):
        if stat != 0:
            logger.info("  stat: {:d}\n".format(stat))
        if output:
            logger.info("  output: {}\n".format(output))
        if errput:
            logger.info("  errput: {}\n".format(errput))

    return stat, output, errput

###############################################################################
def validate_xml_file(filename, schema):
###############################################################################
    """
    validate an XML file against a provided schema file using pylint
    """
    if not os.path.isfile(filename):
        abort("read_xml_file: Filename, '{}', does not exist".format(filename))
    elif not os.access(filename, os.R_OK):
        abort("read_xml_file: Cannot open '{}'".format(filename))
    # End if
    if not os.path.isfile(schema):
        abort("read_xml_file: Schema, '{}', does not exist".format(schema))
    elif not os.access(schema, os.R_OK):
        abort("read_xml_file: Cannot open schema, '{}'".format(schema))
    # End if
    if xmllint is not None:
        logger.debug("Checking file {} against schema {}".format(filename, schema))
        run_cmd("{} --noout --schema {} {}".format(xmllint, schema, filename))
    else:
        logger.warning("xmllint not found, could not validate file {}".format(filename))

###############################################################################
def read_xml_file(filename):
###############################################################################
    if os.path.isfile(filename) and os.access(filename, os.R_OK):
        if six.PY3:
            file_open = (lambda x: open(x, 'r', encoding='utf-8'))
        else:
            file_open = (lambda x: open(x, 'r'))
        # End if
        with file_open(filename) as fd:
            tree = ET.parse(fd)
            root = tree.getroot()
    elif not os.access(filename, os.R_OK):
        abort("read_xml_file: Cannot open '{}'".format(filename))
    else:
        abort("read_xml_file: Filename, '{}', does not exist".format(filename))
    # End if
    return tree, root

###############################################################################
def _main_func():
###############################################################################

###############################################################################

if __name__ == "__main__":
    try:
        _main_func()
    except HostRegAbort as ca:
        pass # abort already logged the message
