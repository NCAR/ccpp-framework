#!/usr/bin/env python

"""
Parse a host-model registry XML file and return the captured variables.
"""

# Python library imports
from __future__ import print_function
import sys
import os
import os.path
import logging
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
from metavar import Var, VarDictionary
from parse_tools import ParseSource, ParseContext

logger = logging.getLogger(__name__)

###############################################################################
class HostRegAbort(ValueError):
    "Class so main can log user errors without backtrace"
    def __init__(self, message):
        super(HostRegAbort, self).__init__(message)

###############################################################################
class HostModel(object):
    "Class to hold the data from a host model"

    def __init__(self, ddt_defs, var_locations, variables):
        self._ddt_defs = ddt_defs
        self._var_locations = var_locations
        self._variables = variables

###############################################################################
def abort(message, log_error=True):
###############################################################################
    if log_error:
        logger.error(message)
    # End if (no else)
    raise HostRegAbort(message)

###############################################################################
def call_command(commands, silent=False):
###############################################################################
    """
    Try a command line and return the output on success (None on failure)
    >>> call_command(['ls', 'really__improbable_fffilename.foo']) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    HostRegAbort: Execution of 'ls really__improbable_fffilename.foo' failed:
    [Errno 2] No such file or directory
    >>> call_command(['ls', 'really__improbable_fffilename.foo'], silent=True)
    False
    >>> call_command(['ls'])
    True
    """
    result = False
    try:
        outstr = subprocess.check_output(commands, stderr=subprocess.STDOUT)
        result = True
    except (OSError, ValueError, subprocess.CalledProcessError) as err:
        if silent:
            outstr = ""
        else:
            cmd = ' '.join(commands)
            outstr = "Execution of '{}' failed:\n".format(cmd)
            outstr = outstr + "{}".format(err)
            abort(outstr)
        # End if
    # End of try
    return result

###############################################################################
def find_host_version(root):
###############################################################################
    """
    Find the version of the host registry file represented by root
    >>> find_host_version(ET.fromstring('<model name="CAM" version="1.0"></model>'))
    [1, 0]
    >>> find_host_version(ET.fromstring('<model name="CAM" version="1.a"></model>')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    HostRegAbort: Illegal version string, '1.a'
    Format must be <integer>.<integer>
    >>> find_host_version(ET.fromstring('<model name="CAM" version="0.0"></model>')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    HostRegAbort: Illegal version string, '0.0'
    Major version must be at least 1
    >>> find_host_version(ET.fromstring('<model name="CAM" version="0.-1"></model>')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    HostRegAbort: Illegal version string, '0.0'
    Minor version must be at least 0
    """
    verbits = None
    if 'version' not in root.attrib:
        abort("version attribute required")
    else:
        version = root.attrib['version']
        versplit = version.split('.')
        try:
            if len(versplit) != 2:
                raise ValueError('oops')
            verbits = [int(x) for x in versplit]
            if verbits[0] < 1:
                raise ValueError('Major version must be at least 1')
            elif verbits[1] < 0:
                raise ValueError('Minor version must be non-negative')
            # End if (no else needed)
        except ValueError as ve:
            errstr = """Illegal version string, '{}'
Format must be <integer>.<integer>"""
            ve_str = str(ve)
            if len(ve_str) > 0:
                errstr = ve_str + '\n' + errstr
            # End if
            abort(errstr.format(version))
        # End try
    # End if
    return verbits

###############################################################################
def validate_xml_file(filename, version):
###############################################################################
    """
    Find the appropriate schema and validate the XML file, <filename>,
    against it using xmllint
    """
    # Check the filename
    if not os.path.isfile(filename):
        abort("read_xml_file: Filename, '{}', does not exist".format(filename))
    elif not os.access(filename, os.R_OK):
        abort("read_xml_file: Cannot open '{}'".format(filename))
    # End if
    # Find the schema, based on the model version
    pdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    schema_path = os.path.join(pdir, 'src')
    verstring = '_'.join([str(x) for x in version])
    schema_file = os.path.join(schema_path,
                               "host_registry_v{}.xsd".format(verstring))
    if not os.path.isfile(schema_file):
        verstring = '.'.join([str(x) for x in version])
        abort("""read_xml_file: Cannot find schema for version {},
{} does not exist""".format(verstring, schema_file))
    elif not os.access(schema_file, os.R_OK):
        abort("read_xml_file: Cannot open schema, '{}'".format(schema_file))
    # End if
    if xmllint is not None:
        logger.debug("Checking file {} against schema {}".format(filename, schema_file))
        cmd = [xmllint, '--noout', '--schema', schema_file, filename]
        result = call_command(cmd)
        return result
    else:
        logger.warning("xmllint not found, could not validate file {}".format(filename))
        return True # We could not check but still need to proceed

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
def parse_host_registry(filename, verbosity):
###############################################################################
    variables = VarDictionary()
    host_variables = {}
    tree, root = read_xml_file(filename)
    # We do not have line number information for the XML file
    context = ParseContext(filename=filename)
    version = find_host_version(root) # Only one version so far
    res = validate_xml_file(filename, version)
    if not res:
        raise HostRegAbort("Invalid host registry file, '{}'".format(filename))
    # End if
    if verbosity > 0:
        logger.info("Reading host model registry for {}".format(root.attrib['model']))
    # End if
    for child in root:
        if (child.tag == 'dimension') or (child.tag == 'variable'):
            prop_dict = child.attrib
            vname = prop_dict['local_name']
            for var_prop in child:
                if var_prop.tag == 'module':
                    # We need to keep track of where host variables come from
                    host_variables[vname] = var_prop.text
                # End if
                if var_prop.tag == 'type':
                    prop_dict['type'] = var_prop.text
                    if 'kind' in var_prop.attrib:
                        prop_dict['kind'] = var_prop.attrib['kind']
                    # End if
                    if 'units' in var_prop.attrib:
                        prop_dict['units'] = var_prop.attrib['units']
                    # End if
                else:
                    # These elements should just have text, no attributes
                    if len(var_prop.attrib) > 0:
                        abort("Bad variable property, {} for variable, {}".format(var_prop.tag, vname))
                    else:
                        prop_dict[var_prop.tag] = var_prop.text
                    # End if
                # End if
            # End for
            ##XXgoldyXX: Try to make the XSD cough these up
            if (child.tag == 'dimension') and ('type' not in prop_dict):
                prop_dict['type'] = "integer"
            # End if
            if (child.tag == 'dimension') and ('units' not in prop_dict):
                prop_dict['units'] = "1"
            # End if
            if (child.tag == 'dimension') and ('kind' not in prop_dict):
                prop_dict['kind'] = ""
            # End if
            if (child.tag == 'dimension') and ('dimensions' not in prop_dict):
                prop_dict['dimensions'] = "()"
            # End if
            newvar = Var(prop_dict, ParseSource(vname, 'REGISTRY', context))
            variables.add_variable(newvar)
        # End if
    # End for
    return variables, host_variables

###############################################################################

if __name__ == "__main__":
    logger.addHandler(logging.NullHandler())
    try:
        # First, run doctest
        import doctest
        doctest.testmod()
        # We are in a subdir, find framework root and host root
        hdir = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        cam7 = os.path.join(hdir, "cam_driver", "src", "cam7_registry.xml")
        if os.path.exists(cam7):
            tree, root = read_xml_file(cam7)
            version = find_host_version(root)
            res = validate_xml_file(cam7, version)
            print("CAM7 registry {}".format("validates" if res else "does not validate"))
        else:
            print("Could not find CAM7 registry, {}".format(cam7))
    except HostRegAbort as ca:
        print("{}".format(ca))
else:
    logger.addHandler(logging.StreamHandler())
