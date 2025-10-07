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
        logger.debug(f"Reading XML file {filename}")
    # end if
    return tree, root

###############################################################################
def load_suite_by_name(suite_name, group_name, main_root, file=None, logger=None):
###############################################################################
    """
    Load a suite by its name, or a group of a suite by the suite and group names.
    If the optional file argument is provided, look for the object in that file,
    otherwise search the current main_root.

    Parameters:
        suite_name (str): The name of the suite to find.
        group_name (str or None): The name of the group to find within the suite.
        main_root (xml.etree.ElementTree.Element): The XML root to search if no file is given.
        file (str, optional): The path to an XML file to read and search instead of main_root.
        logger (logging.Logger, optional): Logger for warnings/errors.

    Returns:
        xml.etree.ElementTree.Element: The matching suite or group element.

    Raises:
        CCPPError: If the suite or group is not found, or if the schema is invalid.

    Examples:
        >>> import xml.etree.ElementTree as ET
        >>> from types import SimpleNamespace
        >>> def read_xml_file(file, logger=None):
        ...     return None, ET.fromstring(file)
        >>> def find_schema_version(root):
        ...     return (2, 0)
        >>> def validate_xml_file(file, kind, schema_version, logger=None):
        ...     return True
        >>> xml_content = '''
        ... <ccpp>
        ...   <suite name="physics_suite">
        ...     <group name="dynamics"/>
        ...     <group name="physics"/>
        ...   </suite>
        ... </ccpp>
        ... '''
        >>> root = ET.fromstring(xml_content)
        >>> load_suite_by_name("physics_suite", None, root).tag
        'suite'
        >>> load_suite_by_name("physics_suite", "dynamics", root).attrib['name']
        'dynamics'
        >>> load_suite_by_name("physics_suite", "missing_group", root) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ...
        CCPPError: Nested suite physics_suite, group missing_group, not found
        >>> load_suite_by_name("missing_suite", None, root) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ...
        CCPPError: Nested suite missing_suite not found
    """
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
        if suite.attrib.get("name") == suite_name:
            if group_name:
                for group in suite.findall("group"):
                    if group.attrib.get("name") == group_name:
                        return group
            else:
                return suite
    emsg = f"Nested suite {suite_name}" \
         + (f", group {group_name}," if group_name else "") \
         + " not found" + (f" in file {file}" if file else "")
    raise CCPPError(emsg)

###############################################################################
def replace_nested_suite(element, nested_suite, root, logger):
###############################################################################
    """
    Replace a <nested_suite> tag with the actual suite or group it references.

    This function looks up a referenced suite or suite group from the main XML tree
    or an external file (if specified), deep copies its children, and replaces the
    <nested_suite> element in the parent `element` with the copied contents.

    If the nested suite being inserted contains its own <nested_suite> elements and 
    within the same  external file, the `file` attribute is propagated into those.

    Parameters:
        element (xml.etree.ElementTree.Element): The parent element containing the nested suite.
        nested_suite (xml.etree.ElementTree.Element): The <nested_suite> element to be replaced.
        root (xml.etree.ElementTree.Element): The root XML element (used when no file is specified).
        logger (logging.Logger or None): Logger to record debug information.

    Returns:
        str or None: The name of the suite if the nested suite came from the root XML element
                     (to delete it later), or None if it came from a separate file.

    Example:
        >>> import xml.etree.ElementTree as ET
        >>> from types import SimpleNamespace
        >>> logger = SimpleNamespace()
        >>> logger.debug = print
        >>> xml = '''
        ... <suites>
        ...   <suite name="my_suite">
        ...     <group name="my_group">
        ...       <scheme>my_scheme</scheme>
        ...     </group>
        ...   </suite>
        ...   <suite name="top">
        ...     <nested_suite name="my_suite"/>
        ...   </suite>
        ... </suites>
        ... '''
        >>> tree = ET.ElementTree(ET.fromstring(xml))
        >>> root = tree.getroot()
        >>> top_suite = root.find("suite[@name='top']")
        >>> nested = top_suite.find("nested_suite")
        >>> replace_nested_suite(top_suite, nested, root, logger)
        Expanded nested suite 'my_suite'
        'my_suite'
        >>> [child.tag for child in top_suite]
        ['group']
        >>> top_suite.find("group").find("scheme").text
        'my_scheme'
        >>> xml = '''
        ... <suites>
        ...   <suite name="my_suite">
        ...     <group name="my_group">
        ...       <scheme>my_scheme</scheme>
        ...     </group>
        ...   </suite>
        ...   <suite name="top">
        ...     <group name="top_group">
        ...       <nested_suite name="my_suite" group="my_group"/>
        ...     </group>
        ...   </suite>
        ... </suites>
        ... '''
        >>> tree = ET.ElementTree(ET.fromstring(xml))
        >>> root = tree.getroot()
        >>> top_suite = root.find("suite[@name='top']")
        >>> top_group = top_suite.find("group")
        >>> nested = top_group.find("nested_suite")
        >>> replace_nested_suite(top_group, nested, root, logger)
        Expanded nested suite 'my_suite', group 'my_group'
        'my_suite'
        >>> [child.tag for child in top_suite]
        ['group']
        >>> top_suite.find("group").find("scheme").text
        'my_scheme'
    """
    suite_name = nested_suite.attrib.get("name")
    group_name = nested_suite.attrib.get("group")
    file = nested_suite.attrib.get("file")
    referenced_suite = load_suite_by_name(suite_name, group_name, root,
                                          file=file, logger=logger)
    # Deep copy to avoid modifying the original
    imported_content = [ET.fromstring(ET.tostring(child)) 
                        for child in referenced_suite]
    # Swap nested suite with imported content
    for item in imported_content:
        # If the imported content comes from a separate file and has
        # nested suites that are within that separate file, then we
        # need to inject the file attribute here.
        if item.tag == "nested_suite":
            if file and not item.attrib.get("file"):
                item.set("file", file)
        element.insert(list(element).index(nested_suite), item)
    element.remove(nested_suite)
    if logger:
        msg = f"Expanded nested suite '{suite_name}'" \
            + (f", group '{group_name}'," if group_name else "") \
            + (f" in file '{file}'" if file else "")
        logger.debug(msg.rstrip(','))
    # If the nested suite resides in the same file as the root
    # element then we need to remove it
    return suite_name if not file else None

###############################################################################
def expand_nested_suites(root, logger=None):
###############################################################################
    """
    Recursively expand all <nested_suite> elements within the XML <suite> elements.

    This function finds <nested_suite> elements within <group> or <suite> elements,
    and replaces them with the corresponding content from another suite. The replacement
    is done in memory using the `replace_nested_suite` function (defined elsewhere).
    Nested suites from the same XML root are removed after expansion.

    This operation is recursive and will continue expanding until no <nested_suite>
    elements remain.

    Parameters:
        root (xml.etree.ElementTree.Element): The root <ccpp> element containing <suite> elements.
        logger (logging.Logger, optional): Logger for debug messages.

    Returns:
        None. The XML tree is modified in place.

    Example:
        >>> import xml.etree.ElementTree as ET
        >>> from types import SimpleNamespace
        >>> logger = SimpleNamespace()
        >>> logger.debug = print
        >>> xml = '''
        ... <suites>
        ...   <suite name="physics_suite">
        ...     <group name="main">
        ...       <nested_suite name="microphysics_suite" group="micro"/>
        ...     </group>
        ...     <nested_suite name="pbl_suite"/>
        ...   </suite>
        ...   <suite name="microphysics_suite">
        ...     <group name="micro">
        ...       <scheme>cloud_scheme</scheme>
        ...     </group>
        ...   </suite>
        ...   <suite name="pbl_suite">
        ...     <group name="pbl">
        ...       <scheme>pbl_scheme</scheme>
        ...     </group>
        ...   </suite>
        ... </suites>
        ... '''
        >>> root = ET.fromstring(xml)
        >>> expand_nested_suites(root, logger)
        Expanded nested suite 'microphysics_suite', group 'micro'
        Expanded nested suite 'pbl_suite'
        Removed nested suite 'microphysics_suite' from root element
        Removed nested suite 'pbl_suite' from root element
        >>> len(root.findall("suite"))  # Only one suite left
        1
        >>> suite = root.find("suite")
        >>> suite.attrib.get("name")
        'physics_suite'
        >>> group = suite.find("group")
        >>> group.attrib.get("name")
        'main'
        >>> group.find("scheme").text
        'cloud_scheme'
    """
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
                    suite_name = replace_nested_suite(group, nested, root, logger)
                    if suite_name:
                        expanded_suites_to_remove.append(suite_name)
                    # Trigger another pass over the root element
                    keep_expanding = True
            # Second, search all suites for nested_suite elements
            nested_suites = suite.findall("nested_suite")
            for nested in nested_suites:
                suite_name = replace_nested_suite(suite, nested, root, logger)
                if suite_name:
                    expanded_suites_to_remove.append(suite_name)
                # Trigger another pass over the root element
                keep_expanding = True
    # Remove expanded suites
    expanded_suites_to_remove = list(set(expanded_suites_to_remove))
    for suite in root.findall("suite"):
        suite_name = suite.attrib.get("name")
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
        logger.debug(f"Writing XML file {file_path}")

##############################################################################
