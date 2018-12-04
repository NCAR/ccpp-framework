#!/usr/bin/env python

"""
Parse a host-model registry XML file and return the captured variables.
"""

from __future__ import print_function
import sys
import os
import os.path
import logging
import six
import subprocess
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
def check_output(commands, verbose=False):
###############################################################################
    """
    Try a command line and return the output on success (None on failure)
    """
    try:
        outstr = subprocess.check_output(commands, stderr=open("/dev/null", mode='w'))
    except OSError as e:
        logger.error("Execution of '{}' failed:".format(' '.join(commands)))
        abort("{}".format(e), file=sys.stderr)
    except ValueError as e:
        if (verbose):
            print("ValueError in '{}':".format(' '.join(commands)),
              file=sys.stderr)
            print("{}".format(e), file=sys.stderr)
            # End if
            outstr = None
    except subprocess.CalledProcessError as e:
        if (verbose):
            print("CalledProcessError in '{}':".format(' '.join(commands)),
              file=sys.stderr)
            print("{}".format(e), file=sys.stderr)
        # End if
        outstr = None
    # End of try
    return outstr

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
        outstr = check_output([xmllint, '--noout', '--schema', schema, filename])
        return outstr is not None
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
    pass

###############################################################################

if __name__ == "__main__":
    try:
        _main_func()
    except HostRegAbort as ca:
        pass # abort already logged the message
