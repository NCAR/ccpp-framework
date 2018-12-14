#!/usr/bin/env python

"""
Parse a host-model registry XML file and return the captured variables.
"""

# Python library imports
from __future__ import print_function
import os
import os.path
import subprocess
import xml.etree.ElementTree as ET
try:
    from distutils.spawn import find_executable
    xmllint = find_executable('xmllint')
except ImportError as ie:
    xmllint = None
# End try
# CCPP framework imports
import six
from parse_tools import CCPPError

###############################################################################
def call_command(commands, logger, silent=False):
###############################################################################
    """
    Try a command line and return the output on success (None on failure)
    >>> call_command(['ls', 'really__improbable_fffilename.foo'], logger) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Execution of 'ls really__improbable_fffilename.foo' failed:
    [Errno 2] No such file or directory
    >>> call_command(['ls', 'really__improbable_fffilename.foo'], logger, silent=True)
    False
    >>> call_command(['ls'], logger)
    True
    """
    result = False
    try:
        outstr = subprocess.check_output(commands, stderr=subprocess.STDOUT)
        result = True
    except (OSError, CCPPError, subprocess.CalledProcessError) as err:
        if silent:
            result = False
        else:
            cmd = ' '.join(commands)
            outstr = "Execution of '{}' failed:\n".format(cmd)
            outstr = outstr + "{}".format(err)
            raise CCPPError(outstr)
        # End if
    # End of try
    return result

###############################################################################
def find_schema_version(root, logger):
###############################################################################
    """
    Find the version of the host registry file represented by root
    >>> find_schema_version(ET.fromstring('<model name="CAM" version="1.0"></model>'), logger)
    [1, 0]
    >>> find_schema_version(ET.fromstring('<model name="CAM" version="1.a"></model>'), logger) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal version string, '1.a'
    Format must be <integer>.<integer>
    >>> find_schema_version(ET.fromstring('<model name="CAM" version="0.0"></model>'), logger) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal version string, '0.0'
    Major version must be at least 1
    >>> find_schema_version(ET.fromstring('<model name="CAM" version="0.-1"></model>'), logger) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal version string, '0.0'
    Minor version must be at least 0
    """
    verbits = None
    if 'version' not in root.attrib:
        raise CCPPError("version attribute required")
    else:
        version = root.attrib['version']
        versplit = version.split('.')
        try:
            if len(versplit) != 2:
                raise CCPPError('oops')
            # End if (no else needed)
            try:
                verbits = [int(x) for x in versplit]
            except ValueError as ve:
                raise CCPPError(ve)
            # End try
            if verbits[0] < 1:
                raise CCPPError('Major version must be at least 1')
            elif verbits[1] < 0:
                raise CCPPError('Minor version must be non-negative')
            # End if (no else needed)
        except CCPPError as ve:
            errstr = """Illegal version string, '{}'
Format must be <integer>.<integer>"""
            ve_str = str(ve)
            if len(ve_str) > 0:
                errstr = ve_str + '\n' + errstr
            # End if
            raise CCPPError(errstr.format(version))
        # End try
    # End if
    return verbits

###############################################################################
def validate_xml_file(filename, schema_root, version, logger):
###############################################################################
    """
    Find the appropriate schema and validate the XML file, <filename>,
    against it using xmllint
    """
    # Check the filename
    if not os.path.isfile(filename):
        raise CCPPError("read_xml_file: Filename, '{}', does not exist".format(filename))
    elif not os.access(filename, os.R_OK):
        raise CCPPError("read_xml_file: Cannot open '{}'".format(filename))
    # End if
    # Find the schema, based on the model version
    thispath = os.path.abspath(__file__)
    pdir = os.path.dirname(os.path.dirname(os.path.dirname(thispath)))
    schema_path = os.path.join(pdir, 'schema')
    verstring = '_'.join([str(x) for x in version])
    schema_file = os.path.join(schema_path,
                               "{}_v{}.xsd".format(schema_root, verstring))
    if not os.path.isfile(schema_file):
        verstring = '.'.join([str(x) for x in version])
        raise CCPPError("""read_xml_file: Cannot find schema for version {},
{} does not exist""".format(verstring, schema_file))
    elif not os.access(schema_file, os.R_OK):
        raise CCPPError("read_xml_file: Cannot open schema, '{}'".format(schema_file))
    # End if
    if xmllint is not None:
        logger.debug("Checking file {} against schema {}".format(filename, schema_file))
        cmd = [xmllint, '--noout', '--schema', schema_file, filename]
        result = call_command(cmd, logger)
        return result
    else:
        logger.warning("xmllint not found, could not validate file {}".format(filename))
        return True # We could not check but still need to proceed

###############################################################################
def read_xml_file(filename, logger):
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
        raise CCPPError("read_xml_file: Cannot open '{}'".format(filename))
    else:
        raise CCPPError("read_xml_file: Filename, '{}', does not exist".format(filename))
    # End if
    return tree, root

###############################################################################

if __name__ == "__main__":
    from parse_log import initLog, setLogToNull
    logger = initLog('xml_tools')
    setLogToNull(logger)
    try:
        # First, run doctest
        import doctest
        doctest.testmod()
        # We are in a subdir, find framework root and host root
        thispath = os.path.abspath(__file__)
        hdir = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(thispath))))
        cam7 = os.path.join(hdir, "cam_driver", "src", "cam7_registry.xml")
        if os.path.exists(cam7):
            tree, root = read_xml_file(cam7, logger)
            version = find_schema_version(root, logger)
            res = validate_xml_file(cam7, 'host_registry', version, logger)
            print("CAM7 registry {}".format("validates" if res else "does not validate"))
        else:
            print("Could not find CAM7 registry, {}".format(cam7))
    except CCPPError as ca:
        print("{}".format(ca))
# No else:
