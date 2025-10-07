#!/usr/bin/env python3

"""
Parse a host-model registry XML file and return the captured variables.
"""

# Python library imports
from __future__ import print_function
import os
import re
import shutil
import subprocess
import sys
import xml.etree.ElementTree as ET
import xml.dom.minidom
sys.path.insert(0, os.path.dirname(__file__))
# CCPP framework imports
from parse_source import CCPPError
from parse_log import init_log, set_log_to_null

# Global data
_INDENT_STR = "  "
_XMLLINT = shutil.which('xmllint') # Blank if not installed
beg_tag_re = re.compile(r"([<][^/][^<>]*[^/][>])")
end_tag_re = re.compile(r"([<][/][^<>/]+[>])")
simple_tag_re = re.compile(r"([<][^/][^<>/]+[/][>])")

# Find python version
PYSUBVER = sys.version_info[1]
_LOGGER = None

###############################################################################
class XMLToolsInternalError(ValueError):
###############################################################################
    """Error class for reporting internal errors"""
    def __init__(self, message):
        """Initialize this exception"""
        super().__init__(message)

###############################################################################
def call_command(commands, logger, silent=False):
###############################################################################
    """
    Try a command line and return the output on success (None on failure)
    >>> _LOGGER = init_log('xml_tools')
    >>> set_log_to_null(_LOGGER)
    >>> call_command(['ls', 'really__improbable_fffilename.foo'], _LOGGER) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Execution of 'ls really__improbable_fffilename.foo' failed:
    [Errno 2] No such file or directory
    >>> call_command(['ls', 'really__improbable_fffilename.foo'], _LOGGER, silent=True)
    False
    >>> call_command(['ls'], _LOGGER)
    True
    >>> try:
    ...    call_command(['ls','--invalid-option'], _LOGGER)
    ... except CCPPError as e:
    ...    print(str(e))
    Execution of 'ls --invalid-option' failed with code: 2
    Error output: ls: unrecognized option '--invalid-option'
    Try 'ls --help' for more information.
    >>> try:
    ...    os.chdir(os.path.dirname(__file__))
    ...    call_command(['ls', os.path.basename(__file__), 'foo.bar.baz'], _LOGGER)
    ... except CCPPError as e:
    ...    print(str(e))
    Execution of 'ls xml_tools.py foo.bar.baz' failed with code: 2
    xml_tools.py
    Error output: ls: cannot access 'foo.bar.baz': No such file or directory
    """
    result = False
    outstr = ''
    try:
        cproc = subprocess.run(commands, check=True,
                                capture_output=True)
        if not silent:
            logger.debug(cproc.stdout)
        # end if
        result = cproc.returncode == 0
    except (OSError, CCPPError, subprocess.CalledProcessError) as err:
        if silent:
            result = False
        else:
            cmd = ' '.join(commands)
            outstr = f"Execution of '{cmd}' failed with code: {err.returncode}\n"
            outstr += f"{err.output.decode('utf-8', errors='replace').strip()}"
            if hasattr(err, 'stderr') and err.stderr:
                stderr_str = err.stderr.decode('utf-8', errors='replace').strip()
                if stderr_str:
                    if err.output:
                        outstr += os.linesep
                    # end if
                    outstr += f"Error output: {stderr_str}"
                # end if
            # end if
            raise CCPPError(outstr) from err
        # end if
    # end of try
    return result

###############################################################################
def find_schema_version(root):
###############################################################################
    """
    Find the version of the host registry file represented by root
    >>> find_schema_version(ET.fromstring('<model name="CAM" version="1.0"></model>'))
    [1, 0]
    >>> find_schema_version(ET.fromstring('<entry_id version="2.0"></entry_id>'))
    [2, 0]
    >>> find_schema_version(ET.fromstring('<model name="CAM" version="1.a"></model>')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal version string, '1.a'
    Format must be <integer>.<integer>
    >>> find_schema_version(ET.fromstring('<model name="CAM" version="0.0"></model>')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal version string, '0.0'
    Major version must be at least 1
    >>> find_schema_version(ET.fromstring('<model name="CAM" version="0.-1"></model>')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal version string, '0.0'
    Minor version must be at least 0
    """
    verbits = None
    if 'version' not in root.attrib:
        raise CCPPError("version attribute required")
    # end if
    version = root.attrib['version']
    versplit = version.split('.')
    try:
        if len(versplit) != 2:
            raise CCPPError('oops')
        # end if (no else needed)
        try:
            verbits = [int(x) for x in versplit]
        except ValueError as verr:
            raise CCPPError(verr) from verr
        # end try
        if verbits[0] < 1:
            raise CCPPError('Major version must be at least 1')
        # end if
        if verbits[1] < 0:
            raise CCPPError('Minor version must be non-negative')
        # end if
    except CCPPError as verr:
        errstr = """Illegal version string, '{}'
        Format must be <integer>.<integer>"""
        ve_str = str(verr)
        if ve_str:
            errstr = ve_str + '\n' + errstr
        # end if
        raise CCPPError(errstr.format(version)) from verr
    # end try
    return verbits

###############################################################################
def find_schema_file(schema_root, version, schema_path=None):
###############################################################################
    """Find and return the schema file based on <schema_root> and <version>
    or return None.
    If <schema_path> is present, use that as the directory to find the
    appropriate schema file. Otherwise, just look in the current directory."""

    verstring = '_'.join([str(x) for x in version])
    schema_filename = "{}_v{}.xsd".format(schema_root, verstring)
    if schema_path:
        schema_file = os.path.join(schema_path, schema_filename)
    else:
        schema_file = schema_filename
    # end if
    if os.path.exists(schema_file):
        return schema_file
    # end if
    return None

###############################################################################
def validate_xml_file(filename, schema_root, version, logger,
                      schema_path=None, error_on_noxmllint=False):
###############################################################################
    """
    Find the appropriate schema and validate the XML file, <filename>,
    against it using xmllint
    """
    # Check the filename
    if not os.path.isfile(filename):
        raise CCPPError("validate_xml_file: Filename, '{}', does not exist".format(filename))
    # end if
    if not os.access(filename, os.R_OK):
        raise CCPPError("validate_xml_file: Cannot open '{}'".format(filename))
    # end if
    if os.path.isfile(schema_root):
        # We already have a file, just use it
        schema_file = schema_root
    else:
        if not schema_path:
            # Find the schema, based on the model version
            thispath = os.path.abspath(__file__)
            pdir = os.path.dirname(os.path.dirname(os.path.dirname(thispath)))
            schema_path = os.path.join(pdir, 'schema')
        # end if
        schema_file = find_schema_file(schema_root, version, schema_path)
        if not (schema_file and os.path.isfile(schema_file)):
            verstring = '.'.join([str(x) for x in version])
            emsg = f"""validate_xml_file: Cannot find schema for version {verstring},
            {schema_file} does not exist"""
            raise CCPPError(emsg)
        # end if
    # end if
    if not os.access(schema_file, os.R_OK):
        emsg = "validate_xml_file: Cannot open schema, '{}'"
        raise CCPPError(emsg.format(schema_file))
    # end if
    if _XMLLINT:
        logger.debug("Checking file {} against schema {}".format(filename,
                                                                 schema_file))
        cmd = [_XMLLINT, '--noout', '--schema', schema_file, filename]
        logger.debug(f"Executing command '{cmd}'")
        result = call_command(cmd, logger)
        return result
    # end if
    lmsg = "xmllint not found, could not validate file {}"
    if error_on_noxmllint:
        raise CCPPError("validate_xml_file: " + lmsg.format(filename))
    # end if
    logger.warning(lmsg.format(filename))
    return True # We could not check but still need to proceed

###############################################################################
def read_xml_file(filename, logger=None):
###############################################################################
    """Read the XML file, <filename>, and return its tree and root"""
    if os.path.isfile(filename) and os.access(filename, os.R_OK):
        file_open = (lambda x: open(x, 'r', encoding='utf-8'))
        with file_open(filename) as file_:
            try:
                tree = ET.parse(file_)
                root = tree.getroot()
            except ET.ParseError as perr:
                emsg = "read_xml_file: Cannot read {}, {}"
                raise CCPPError(emsg.format(filename, perr)) from perr
    elif not os.access(filename, os.R_OK):
        raise CCPPError("read_xml_file: Cannot open '{}'".format(filename))
    else:
        emsg = "read_xml_file: Filename, '{}', does not exist"
        raise CCPPError(emsg.format(filename))
    # end if
    if logger:
        logger.debug("Read XML file, '{}'".format(filename))
    # end if
    return tree, root

###############################################################################
def load_suite_by_name(suite_name, group_name, main_root, file=None, logger=None):
###############################################################################
    """Load a suite by its name, or a group of a suite by the suite
    and group names. If the optional file argument is provided, look
    for the object in that file, otherwise search the current main_root.""" 
    if file:
        _, root = read_xml_file(file, logger)
        schema_version = find_schema_version(root)
        if schema_version[0] < 2:
            raise CCPPError(f"XML schema version {schema_version} " + \
                            f"invalid for nested suite {suite_name}")
        res = validate_xml_file(file, 'suite', schema_version, logger)
        if not res:
            raise CCPPError(f"Invalid suite definition file, '{sdf}'")
    else:
        root = main_root
    for suite in root.findall("suite"):
        print("ABC: {suite.attrib.get('name')}")
        if suite.attrib.get("name") == suite_name:
            if group_name:
                for group in suite.findall("group"):
                    if group.attrib.get("name") == group_name:
                        return group
            else:
                return suite
    emsg = f"Nested suite {suite_name}" + (f", group {group_name}," if group_name else "") \
         + " not found" + (f" in file {file}" if file else "")
    raise CCPPError(emsg)

###############################################################################
def expand_nested_suites(root, logger=None):
###############################################################################
    """Iterate over the root element until all nested suites (single, double,
    triple, ...) are replaced with the actual content of the nested suite."""
    # Keep track of any nested suites defined under the same root
    # that need to be removed at the end of this function.
    # This happens all in memory, it does not alter files on disk.
    expanded_suites_to_remove = list()
    # Iteratively expand nested suites until they are all gone
    keep_expanding = True
    while keep_expanding:
        keep_expanding = False
        for suite in root.findall("suite"):
            # First, search all groups for nested_suite elements
            groups = suite.findall("group")
            for group in groups:
                nested_suites = group.findall("nested_suite")
                for nested in nested_suites:
                    suite_name = nested.attrib.get("name")
                    group_name = nested.attrib.get("group")
                    file = nested.attrib.get("file")
                    # This check is redundant, because the XML schema ensures
                    # that nested_suite elements inside a group have a group name
                    if not group_name:
                        CCPPError(f"Required attribute group not found for nested suite {suite_name}")
                    referenced_suite = load_suite_by_name(suite_name, group_name, root,
                                                          file=file, logger=logger)
                    # Deep copy to avoid modifying the original
                    imported_content = [ET.fromstring(ET.tostring(child)) for child in referenced_suite]
                    # Swap nested suite with imported content
                    for item in imported_content:
                        # If the imported content comes from a separate file and has
                        # nested suites that are within that separate file, then we
                        # need to inject the file attribute here.
                        if item.tag == "nested_suite":
                            if file and not item.attrib.get("file"):
                                item.set("file", file)
                        group.insert(list(group).index(nested), item)
                    group.remove(nested)
                    # Need another pass over the root element
                    keep_expanding = True
                    # If the nested suite resides in the same file, remove it
                    if not file:
                        expanded_suites_to_remove.append(suite_name)
                    if logger:
                        msg = f"Expanded nested suite '{suite_name}', group '{group_name}'"
                        if file:
                            msg += f", in file '{file}'"
                        logger.debug(msg)
            # Second, search all suites for nested_suite elements
            nested_suites = suite.findall("nested_suite")
            for nested in nested_suites:
                suite_name = nested.attrib.get("name")
                group_name = nested.attrib.get("group")
                # This check is redundant, because the XML schema ensures
                # that nested_suite elements at the suite level have no group name
                if group_name:
                    CCPPError("Nested suite {suite_name} cannot have attribute group")
                file = nested.attrib.get("file")
                referenced_suite = load_suite_by_name(suite_name, group_name, root,
                                                      file=file, logger=logger)
                # Deep copy to avoid modifying the original
                imported_content = [ET.fromstring(ET.tostring(child)) for child in referenced_suite]
                # Swap nested suite with imported content
                for item in imported_content:
                    # If the imported content comes from a separate file and has
                    # nested suites that are within that separate file, then we
                    # need to inject the file attribute here.
                    if item.tag == "nested_suite":
                        if file and not item.attrib.get("file"):
                            item.set("file", file)
                    suite.insert(list(suite).index(nested), item)
                suite.remove(nested)
                # Need another pass over the root element
                keep_expanding = True
                # If the nested suite resides in the same file, remove it
                if not file:
                    expanded_suites_to_remove.append(suite_name)
                if logger:
                    msg = f"Expanded nested suite '{suite_name}'"
                    if file:
                        msg += f" in file '{file}'"
                    logger.debug(msg)

    # Remove expanded suites
    for suite in root.findall("suite"):
        suite_name = suite.attrib["name"]
        if suite_name in expanded_suites_to_remove:
            root.remove(suite)
            if logger:
                msg = f"Removed nested suite '{suite_name}' from root element"
                logger.debug(msg)

###############################################################################
def write_xml_file(root, file_path, logger=None):
###############################################################################
    """Pretty-prints an ElementTree to an ASCII file using xml.dom.minidom"""

    def remove_whitespace_nodes(node):
        """Helper function to recursively remove all text nodes that contain
        only whitespace, which eliminates blank lines in the output."""
        for child in list(node.childNodes):
            if child.nodeType == child.TEXT_NODE and not child.data.strip():
                node.removeChild(child)
            elif child.hasChildNodes():
                remove_whitespace_nodes(child)

    # Convert ElementTree to a byte string
    byte_string = ET.tostring(root, 'us-ascii')
    
    # Parse string using minidom for pretty printing
    reparsed = xml.dom.minidom.parseString(byte_string)

    # Clean whitespace-only text nodes
    remove_whitespace_nodes(reparsed)

    # Generate pretty-printed XML string
    pretty_xml = reparsed.toprettyxml(indent="  ")

    # Write to file
    with open(file_path, 'w', errors='xmlcharrefreplace') as f:
        f.write(pretty_xml)

    # Tell everyone!
    if logger:
        logger.debug(f"Wrote {root} to {file_path}")

##############################################################################
