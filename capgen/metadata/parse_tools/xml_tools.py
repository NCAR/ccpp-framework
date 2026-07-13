#!/usr/bin/env python3

"""XML helpers: schema-version probing, schema validation, nested-suite
expansion, and pretty-printed XML writing.
"""

import os
import shutil
import subprocess
import xml.etree.ElementTree as ET
import xml.dom.minidom

from .parse_source import CCPPError
from .parse_log import init_log, set_log_to_null


def find_schema_version(root):
    """Return the schema version as ``[major, minor]`` from the *root*'s
    ``version`` attribute.

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
    """
    if 'version' not in root.attrib:
        raise CCPPError("Version attribute required")
    version = root.attrib['version']
    versplit = version.split('.')
    try:
        if len(versplit) != 2:
            raise CCPPError('Major and minor version required')
        try:
            verbits = [int(x) for x in versplit]
        except ValueError as verr:
            raise CCPPError(verr) from verr
        if verbits[0] < 1:
            raise CCPPError('Major version must be at least 1')
        if verbits[1] < 0:
            raise CCPPError('Minor version must be non-negative')
    except CCPPError as verr:
        errstr = """Illegal version string, '{}'
        Format must be <integer>.<integer>"""
        ve_str = str(verr)
        if ve_str:
            errstr = ve_str + '\n' + errstr
        raise CCPPError(errstr.format(version)) from verr
    return verbits


def find_schema_file(schema_root, version, schema_path=None):
    """Return ``<schema_root>_v<major>_<minor>.xsd`` under *schema_path*
    (or the current directory), or ``None`` if no such file exists."""
    verstring = '_'.join([str(x) for x in version])
    schema_filename = "{}_v{}.xsd".format(schema_root, verstring)
    if schema_path:
        schema_file = os.path.join(schema_path, schema_filename)
    else:
        schema_file = schema_filename
    if os.path.exists(schema_file):
        return schema_file
    return None


def validate_xml_file(filename, version, logger, schema_path=None):
    """Validate *filename* against the suite schema for *version* using xmllint."""
    if not os.path.isfile(filename):
        raise CCPPError("validate_xml_file: Filename, '{}', does not exist".format(filename))
    if not os.access(filename, os.R_OK):
        raise CCPPError("validate_xml_file: Cannot open '{}'".format(filename))
    if not schema_path:
        thispath = os.path.abspath(__file__)
        pdir = os.path.dirname(os.path.dirname(os.path.dirname(thispath)))
        schema_path = os.path.join(pdir, 'schema')
    schema_file = find_schema_file('suite', version, schema_path)
    if not (schema_file and os.path.isfile(schema_file)):
        verstring = '.'.join([str(x) for x in version])
        raise CCPPError(
            f"validate_xml_file: Cannot find suite schema for version {verstring},\n"
            f"            {schema_file} does not exist"
        )
    if not os.access(schema_file, os.R_OK):
        raise CCPPError(
            "validate_xml_file: Cannot open schema, '{}'".format(schema_file))

    xmllint = shutil.which('xmllint')
    if not xmllint:
        raise CCPPError(
            "validate_xml_file: xmllint not found, could not validate file {}".format(filename))

    logger.debug("Checking file {} against schema {}".format(filename, schema_file))
    cmd = [xmllint, '--noout', '--schema', schema_file, filename]
    cproc = subprocess.run(cmd, check=False, capture_output=True)
    if cproc.returncode == 0:
        # Some xmllint builds return 0 even when validation fails; double
        # check by looking for the literal 'validates' marker in output.
        result = b'validates' in cproc.stdout or b'validates' in cproc.stderr
    else:
        result = False
    if result:
        logger.debug(cproc.stdout)
        logger.debug(cproc.stderr)
        return result
    cmd_str = ' '.join(cmd)
    outstr = f"Execution of '{cmd_str}' failed with code: {cproc.returncode}\n"
    if cproc.stdout:
        outstr += f"{cproc.stdout.decode('utf-8', errors='replace').strip()}\n"
    if cproc.stderr:
        outstr += f"{cproc.stderr.decode('utf-8', errors='replace').strip()}\n"
    raise CCPPError(outstr)


def read_xml_file(filename, logger=None):
    """Read *filename* and return ``(tree, root)``.

    Raises ``CCPPError`` if the file is missing or unreadable.
    """
    if os.path.isfile(filename) and os.access(filename, os.R_OK):
        with open(filename, 'r', encoding='utf-8') as fh:
            try:
                tree = ET.parse(fh)
                root = tree.getroot()
            except ET.ParseError as perr:
                raise CCPPError(
                    "read_xml_file: Cannot read {}, {}".format(filename, perr)
                ) from perr
    elif not os.access(filename, os.R_OK):
        raise CCPPError("read_xml_file: Cannot open '{}'".format(filename))
    else:
        raise CCPPError(
            "read_xml_file: Filename, '{}', does not exist".format(filename))
    if logger:
        logger.debug(f"Reading XML file {filename}")
    return tree, root


def load_suite_by_name(suite_name, group_name, file, logger=None):
    """Load and return a suite or group element from a SDF file.

    Parameters
    ----------
    suite_name : str
        Name of the suite element to find.
    group_name : str or None
        Name of the group within the suite to find; ``None`` returns the
        whole suite.
    file : str
        Path to the XML file.
    logger : logging.Logger, optional

    Returns
    -------
    xml.etree.ElementTree.Element
        The matching suite or group element.
    """
    _, root = read_xml_file(file, logger)
    try:
        schema_version = find_schema_version(root)
    except CCPPError as verr:
        raise CCPPError(
            f"{verr} in nested suite XML file '{file}'"
        ) from verr
    if not validate_xml_file(file, schema_version, logger):
        raise CCPPError(f"Invalid suite definition file, '{file}'")
    if root.attrib.get("name") == suite_name:
        if group_name:
            for group in root.findall("group"):
                if group.attrib.get("name") == group_name:
                    return group
        else:
            return root
    emsg = f"Nested suite {suite_name}" \
         + (f", group {group_name}," if group_name else "") \
         + " not found" + (f" in file {file}" if file else "")
    raise CCPPError(emsg)


def replace_nested_suite(element, nested_suite, default_path, logger):
    """Replace a ``<nested_suite>`` element with the suite/group it references.

    Parameters
    ----------
    element : Element
        Parent of *nested_suite*.
    nested_suite : Element
        The ``<nested_suite>`` element to replace.
    default_path : str
        Directory to resolve a relative ``file=`` attribute against.
    logger : logging.Logger or None

    Returns
    -------
    str
        Name of the suite that was substituted in.
    """
    suite_name = nested_suite.attrib.get("name")
    group_name = nested_suite.attrib.get("group")
    file = nested_suite.attrib.get("file")
    if not os.path.isabs(file):
        file = os.path.join(default_path, file)
    referenced_suite = load_suite_by_name(suite_name, group_name, file,
                                          logger=logger)
    imported_content = [ET.fromstring(ET.tostring(child))
                        for child in referenced_suite]
    for item in imported_content:
        # When importing a single group at the suite level, wrap the item
        # in a fresh <group> so the parent's level isn't changed.
        if element.tag == 'suite' and group_name:
            item_to_insert = ET.Element("group", attrib={"name": group_name})
            item_to_insert.append(item)
        else:
            item_to_insert = item
        element.insert(list(element).index(nested_suite), item_to_insert)
    element.remove(nested_suite)
    if logger:
        msg = f"Expanded nested suite '{suite_name}'" \
            + (f", group '{group_name}'," if group_name else "") \
            + (f" in file '{file}'" if file else "")
        logger.debug(msg.rstrip(','))
    return suite_name


def expand_nested_suites(suite, default_path, logger=None):
    """Recursively expand every ``<nested_suite>`` element inside *suite*.

    Iterative bound caps the recursion at ``max_iterations`` passes to
    keep mutually-referential SDFs from looping forever.

    Examples
    --------
    Expand a single group-less ``<nested_suite>`` reference in place (error
    paths — missing suites/groups, cycle detection — are covered by
    ``test_suite_xml.py``):

    >>> import tempfile
    >>> import xml.etree.ElementTree as ET
    >>> logger = init_log('xml_tools')
    >>> set_log_to_null(logger)
    >>> tmpdir = tempfile.TemporaryDirectory()
    >>> ref = os.path.join(tmpdir.name, "pbl.xml")
    >>> with open(ref, "w") as f:
    ...     _ = f.write('<suite name="pbl_suite" version="2.0">'
    ...                 '<group name="pbl"><scheme>pbl_scheme</scheme></group></suite>')
    >>> suite = ET.fromstring(
    ...     f'<suite name="top" version="2.0">'
    ...     f'<nested_suite name="pbl_suite" file="{ref}"/></suite>')
    >>> expand_nested_suites(suite, tmpdir.name, logger)
    >>> [g.attrib["name"] for g in suite.findall("group")]
    ['pbl']
    >>> tmpdir.cleanup()
    """
    max_iterations = 10
    suite_names = []
    for _ in range(max_iterations):
        keep_expanding = False
        for group in suite.findall("group"):
            for nested in group.findall("nested_suite"):
                suite_names.append(
                    replace_nested_suite(group, nested, default_path, logger))
                keep_expanding = True
        for nested in suite.findall("nested_suite"):
            suite_names.append(
                replace_nested_suite(suite, nested, default_path, logger))
            keep_expanding = True
        if not keep_expanding:
            return
    raise CCPPError(
        "Exceeded number of iterations while expanding nested suites. "
        "Check for infinite recursion or adjust limit max_iterations. "
        f"Suites expanded so far: {suite_names}")


def write_xml_file(root, file_path, logger=None):
    """Pretty-print *root* to *file_path*, routed through write_if_changed.

    Unchanged regenerations preserve the on-disk mtime so downstream
    build tools don't rebuild.
    """

    def remove_whitespace_nodes(node):
        for child in list(node.childNodes):
            if child.nodeType == child.TEXT_NODE and not child.data.strip():
                node.removeChild(child)
            elif child.hasChildNodes():
                remove_whitespace_nodes(child)

    byte_string = ET.tostring(root, 'us-ascii')
    reparsed = xml.dom.minidom.parseString(byte_string)
    remove_whitespace_nodes(reparsed)
    pretty_xml = reparsed.toprettyxml(indent="  ")

    from .io_helpers import write_if_changed
    write_if_changed(file_path, pretty_xml, logger=logger)
