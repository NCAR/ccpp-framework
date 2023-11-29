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
            emsg = "Execution of '{}' failed with code:\n"
            outstr = emsg.format(cmd, err.returncode)
            outstr += "{}".format(err.output)
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
        logger.debug("Read XML file, '{}'".format(filename))
    # end if
    return tree, root

###############################################################################

class PrettyElementTree(ET.ElementTree):
    """An ElementTree subclass with nice formatting when writing to a file"""

    def __init__(self, element=None, file=None):
        """Initialize a PrettyElementTree object"""
        super().__init__(element, file)

    def _write(self, outfile, line, indent, eol=os.linesep):
        """Write <line> as an ASCII string to <outfile>"""
        outfile.write('{}{}{}'.format(_INDENT_STR*indent, line, eol))

    @staticmethod
    def _inc_pos(outstr, text, txt_beg):
        """Return a position increment based on the length of <outstr>
        or raise an exception if <outstr> is empty.
        <text> and <txt_beg> are used to provide some context for the error."""
        if outstr:
            return len(outstr)
        # end if
        txt_end = text[txt_beg].find(">") + txt_beg + 1
        if txt_end <= txt_beg:
            txt_end = txt_beg + 256
        # end if
        emsg = "No output at {} of {}\n{}".format(txt_beg, len(text),
                                                  text[txt_beg:txt_end])
        raise XMLToolsInternalError(emsg)

    def write(self, file, encoding="us-ascii", xml_declaration=None,
              default_namespace=None, method="xml",
              short_empty_elements=True):
        """Subclassed write method to format output."""
        et_str = ET.tostring(self.getroot(),
                             encoding=encoding, method=method,
                             xml_declaration=xml_declaration,
                             default_namespace=default_namespace,
                             short_empty_elements=short_empty_elements)
        # end if
        fmode = 'wt'
        root = str(et_str, encoding="utf-8")
        indent = 0
        last_write_text = False
        with open(file, fmode) as outfile:
            inline = root.strip()
            istart = 0 # Current start pos
            iend = len(inline)
            while istart < iend:
                bmatch = beg_tag_re.match(inline[istart:])
                ematch = end_tag_re.match(inline[istart:])
                smatch = simple_tag_re.match(inline[istart:])
                if bmatch is not None:
                    outstr = bmatch.group(1)
                    if inline[istart + len(bmatch.group(1))] != '<':
                        # Print text on same line
                        self._write(outfile, outstr, indent, eol='')
                    else:
                        self._write(outfile, outstr, indent)
                    # end if
                    indent += 1
                    istart += self._inc_pos(outstr, inline, istart)
                    last_write_text = False
                elif ematch is not None:
                    outstr = ematch.group(1)
                    indent -= 1
                    if last_write_text:
                        self._write(outfile, outstr, 0)
                        last_write_text = False
                    else:
                        self._write(outfile, outstr, indent)
                    # end if
                    istart += self._inc_pos(outstr, inline, istart)
                elif smatch is not None:
                    outstr = smatch.group(1)
                    self._write(outfile, outstr, indent)
                    istart += self._inc_pos(outstr, inline, istart)
                    last_write_text = False
                else:
                    # No tag, just output text
                    end_index = inline[istart:].find('<')
                    if end_index < 0:
                        end_index = iend
                    else:
                        end_index += istart
                    # end if
                    outstr = inline[istart:end_index]
                    self._write(outfile, outstr.strip(), 0, eol='')
                    last_write_text = True
                    istart += self._inc_pos(outstr, inline, istart)
                # end if
            # end while
        # end with

##############################################################################
