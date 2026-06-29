#!/usr/bin/env python3

"""Query CLI for the ``datatable.xml`` produced by ``ccpp_capgen.py``.

This is the read-side companion to :mod:`generator.datatable`.  The writer
half lives in the generator package; this module is a pure-Python,
dependency-free reader (only the stdlib ``xml.etree.ElementTree``) so it
can be invoked from CMake or any other build-time tooling.

The flag surface mirrors the original ``scripts/ccpp_datafile.py``:

  * Fortran-source enumeration:
    ``--host-files`` / ``--suite-files`` / ``--utility-files`` /
    ``--capgen-files``.  All four return *only* generated Fortran files
    (``.F90``).  ``--capgen-files`` is the union of the other three.
  * Non-Fortran enumeration:
    ``--inspection-files`` — generated ``.meta`` and expanded SDF XML.
  * ``--process-list`` / ``--module-list`` / ``--dependencies``
  * ``--suite-list``
  * ``--required-variables`` / ``--input-variables`` /
    ``--output-variables`` / ``--host-variables``
  * ``--show`` (pretty-print)
  * ``--separator``, ``--exclude-protected``, ``--line-wrap``, ``--indent``

Exactly one report action is required per invocation.

Notes specific to capgen
---------------------------
* ``--host-files`` returns ``<host>_ccpp_cap.F90`` (the per-host static API;
  filename and module name derived from ``--host-name`` at generation time).
* ``--capgen-files`` enumerates Fortran sources only.  Non-Fortran inspection
  artifacts (``ccpp_<suite>.meta``, ``ccpp_<suite>_expanded.xml``) are
  reported via ``--inspection-files``.
* ``--process-list`` is supported syntactically but will return an empty
  string: capgen does not currently record a ``process`` attribute on
  scheme entries.
"""

import argparse
import sys
import xml.etree.ElementTree as ET
from typing import List, Optional

_INDENT_STR = "  "

_VALID_REPORTS = [
    {"report": "host_files", "type": bool,
     "help": "Return a list of host CAP Fortran files created by capgen"},
    {"report": "suite_files", "type": bool,
     "help": "Return a list of suite CAP Fortran files created by capgen"},
    {"report": "scheme_files", "type": bool,
     "help": ("Return a list of scheme Fortran source files actually "
              "referenced by some loaded suite (group phases + "
              "suite-level <init>/<final> hooks).  These are the "
              "user-supplied scheme .F90 files, NOT capgen-generated "
              "caps")},
    {"report": "utility_files", "type": bool,
     "help": ("Return a list of utility Fortran files created by "
              "capgen (e.g., ccpp_kinds.F90)")},
    {"report": "capgen_files", "type": bool,
     "help": ("Return a list of all Fortran files created by capgen "
              "(union of --host-files, --suite-files, --utility-files); "
              "non-Fortran inspection artifacts are reported via "
              "--inspection-files")},
    {"report": "inspection_files", "type": bool,
     "help": ("Return a list of non-Fortran inspection files created by "
              "capgen (suite .meta files and expanded suite definition "
              "XML files)")},
    {"report": "process_list", "type": bool,
     "help": ("Return a list of process types and implementing "
              "scheme name")},
    {"report": "module_list", "type": bool,
     "help":
     "Return a list of module names used in this set of suites"},
    {"report": "dependencies", "type": bool,
     "help": ("Return a list of scheme and host "
              "dependency file paths (from the 'dependencies' "
              "attribute in metadata tables)")},
    {"report": "suite_list", "type": bool,
     "help": "Return a list of configured suite names"},
    {"report": "required_variables", "type": str,
     "help": ("Return a list of required variable "
              "standard names for suite, <SUITE_NAME>"),
     "metavar": "SUITE_NAME"},
    {"report": "input_variables", "type": str,
     "help": ("Return a list of required input variable "
              "standard names for suite, <SUITE_NAME>"),
     "metavar": "SUITE_NAME"},
    {"report": "output_variables", "type": str,
     "help": ("Return a list of required output variable "
              "standard names for suite, <SUITE_NAME>"),
     "metavar": "SUITE_NAME"},
    {"report": "host_variables", "type": bool,
     "help": ("Return a list of required host model variable "
              "standard names")},
    {"report": "show", "type": bool,
     "help":
     "Pretty print the database contents to the screen"},
]

###
### Utilities
###


class CCPPDatatableError(ValueError):
    """Error specific to errors found in the CCPP capgen datafile"""
    pass


class DatatableReport(object):
    """A class to hold a database report type and inquiry function"""

    __valid_actions = [x["report"] for x in _VALID_REPORTS]

    def __init__(self, action, value=True):
        """Initialize this report as report-type, <action>.
        # Test a valid action
        >>> DatatableReport('input_variables', False).action
        'input_variables'

        # Test an invalid action
        >>> DatatableReport('banana', True).value
        Traceback (most recent call last):
        ...
        ValueError: Invalid action, 'banana'

        """
        if action in DatatableReport.__valid_actions:
            self.__action = action
            self.__value = value
        else:
            raise ValueError("Invalid action, '{}'".format(action))

    def action_is(self, action):
        """If <action> matches this report type, return True.
        Otherwise, return False
        >>> DatatableReport('suite_files', False).action_is('suite_files')
        True

        >>> DatatableReport('suite_files', False).action_is('banana')
        False
        """
        return action == self.__action

    @property
    def action(self):
        """Return this action's action"""
        return self.__action

    @property
    def value(self):
        """Return this action's value"""
        return self.__value

    @classmethod
    def valid_actions(cls):
        """Return the list of valid actions for this class"""
        return cls.__valid_actions


###
### Interface for retrieving datatable information
###


def _command_line_parser():
    """Create and return an ArgumentParser for parsing the command line."""
    description = """
    Retrieve information about a ccpp_capgen run.
    The returned information is controlled by selecting an action from
    the list of optional arguments below.
    Note that exactly one action is required.
    """
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("datatable", type=str,
                        help="Path to a data table XML file created by capgen")
    # Only one action per call
    group = parser.add_mutually_exclusive_group(required=True)
    for report in _VALID_REPORTS:
        rep_type = "--{}".format(report["report"].replace("_", "-"))
        if report["type"] is bool:
            group.add_argument(rep_type, action='store_true', default=False,
                               help=report["help"])
        elif report["type"] is str:
            if "metavar" in report:
                group.add_argument(rep_type, required=False, type=str,
                                   metavar=report["metavar"], default='',
                                   help=report["help"])
            else:
                group.add_argument(rep_type, required=False, type=str,
                                   default='', help=report["help"])
        else:
            raise ValueError("Unknown report type, '{}'".format(report["type"]))
    defval = ","
    help_str = "String to separate items in a list (default: '{}')"
    parser.add_argument("--separator", type=str, required=False, default=defval,
                        metavar="SEP", dest="sep", help=help_str.format(defval))
    defval = False
    help_str = ("Exclude protected variables (only has an effect if the "
                "requested report is returning a list of variables)."
                " (default: {})")
    parser.add_argument("--exclude-protected", action='store_true',
                        required=False,
                        default=defval, help=help_str.format(defval))
    defval = -1
    help_str = ("Screen width for '--show' line wrapping. -1 means do not "
                "wrap. (default: {})")
    parser.add_argument("--line-wrap", type=int, required=False,
                        metavar="LINE_WIDTH", dest="line_wrap",
                        default=defval, help=help_str.format(defval))
    defval = 2
    help_str = "Indent depth for '--show' output (default: {})"
    parser.add_argument("--indent", type=int, required=False, default=2,
                        help=help_str.format(defval))
    return parser


def parse_command_line(args):
    """Create an ArgumentParser to parse and return command-line arguments."""
    parser = _command_line_parser()
    pargs = parser.parse_args(args)
    return pargs


###
### Accessor functions to retrieve information from a datatable file
###


def _read_datatable(datatable):
    """Read XML file *datatable* and return its root node."""
    tree = ET.parse(datatable)
    return tree.getroot()


def _find_table_section(table, elem_type):
    """Look for and return an element type, <elem_type>, in <table>.
    Raise an exception if the element is not found.
    # Test present section
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><capgen_files></capgen_files></ccpp_datatable>")
    >>> _find_table_section(table, "capgen_files").tag
    'capgen_files'

    # Test missing section
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><scheme></scheme></ccpp_datatable>")
    >>> _find_table_section(table, "capgen_files").tag
    Traceback (most recent call last):
    ...
    ccpp_datafile.CCPPDatatableError: Element type, 'capgen_files', not found in table
    """
    found = table.find(elem_type)
    if found is None:
        emsg = "Element type, '{}', not found in table"
        raise CCPPDatatableError(emsg.format(elem_type))
    return found


def _retrieve_capgen_files(table, file_type=None):
    """Find and retrieve a list of generated filenames from <table>.
    If <file_type> is not None, only return that file type.
    # Test valid ccpp files
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><capgen_files>"\
                "<utilities><file>/path/to/file1</file>"\
                "<file>/path/to/file2</file></utilities>"\
                "<host_files><file>/path/to/file3</file></host_files>"\
                "<suite_files><file>/path/to/file4</file></suite_files>"\
                "</capgen_files></ccpp_datatable>")
    >>> _retrieve_capgen_files(table)
    ['/path/to/file1', '/path/to/file2', '/path/to/file3', '/path/to/file4']

    # Test invalid file type
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><capgen_files>"\
                "<utilities><banana>/path/to/file1</banana></utilities>"\
                "</capgen_files></ccpp_datatable>")
    >>> _retrieve_capgen_files(table)
    Traceback (most recent call last):
    ...
    ccpp_datafile.CCPPDatatableError: Invalid file list entry type, 'banana'
    """
    capgen_files = list()
    for section in _find_table_section(table, "capgen_files"):
        if (not file_type) or (section.tag == file_type):
            for entry in section:
                if entry.tag == "file":
                    capgen_files.append(entry.text)
                else:
                    emsg = "Invalid file list entry type, '{}'"
                    raise CCPPDatatableError(emsg.format(entry.tag))
    return capgen_files


def _retrieve_scheme_files(table):
    """Find and return the list of used-scheme Fortran source paths from <table>.

    The ``<scheme_files>`` section lists the user-supplied scheme ``.F90``
    (or ``.F`` / ``.f90`` / ``.f``) sources for schemes that the loaded
    suites actually reference.  Build systems use this to compile exactly
    the scheme set the suites consume; unreferenced scheme metadata
    files passed on the capgen CLI for convenience are filtered out.

    # Test valid scheme files
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><scheme_files>"\
                "<file>/path/to/scheme1.F90</file>"\
                "<file>/path/to/scheme2.F90</file>"\
                "</scheme_files></ccpp_datatable>")
    >>> _retrieve_scheme_files(table)
    ['/path/to/scheme1.F90', '/path/to/scheme2.F90']

    # Test empty
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><scheme_files>"\
                "</scheme_files></ccpp_datatable>")
    >>> _retrieve_scheme_files(table)
    []

    # Test missing section
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'></ccpp_datatable>")
    >>> _retrieve_scheme_files(table)
    Traceback (most recent call last):
    ...
    ccpp_datafile.CCPPDatatableError: Element type, 'scheme_files', not found in table

    # Test invalid entry type
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><scheme_files>"\
                "<banana>/path/to/scheme1.F90</banana>"\
                "</scheme_files></ccpp_datatable>")
    >>> _retrieve_scheme_files(table)
    Traceback (most recent call last):
    ...
    ccpp_datafile.CCPPDatatableError: Invalid scheme file entry type, 'banana'
    """
    result = []
    section = _find_table_section(table, "scheme_files")
    for entry in section:
        if entry.tag == "file":
            if entry.text is not None:
                result.append(entry.text)
        else:
            raise CCPPDatatableError(
                "Invalid scheme file entry type, '{}'".format(entry.tag)
            )
    return result


def _retrieve_inspection_files(table, file_type=None):
    """Find and retrieve a list of inspection filenames from <table>.

    Inspection files are non-Fortran artifacts emitted by capgen for
    debugging and downstream tooling: suite ``.meta`` files and expanded
    suite-definition XML.  Each kind lives in its own subsection of
    ``<inspection_files>``.

    If <file_type> is not None, only return files in that subsection.

    # Test valid inspection files
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><inspection_files>"\
                "<suite_meta_files><file>/path/to/a.meta</file>"\
                "</suite_meta_files>"\
                "<expanded_sdf_files><file>/path/to/a_exp.xml</file>"\
                "</expanded_sdf_files>"\
                "</inspection_files></ccpp_datatable>")
    >>> _retrieve_inspection_files(table)
    ['/path/to/a.meta', '/path/to/a_exp.xml']

    # Test file_type filter
    >>> _retrieve_inspection_files(table, file_type='suite_meta_files')
    ['/path/to/a.meta']

    # Test invalid entry type
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><inspection_files>"\
                "<suite_meta_files><banana>/path/to/a.meta</banana>"\
                "</suite_meta_files></inspection_files></ccpp_datatable>")
    >>> _retrieve_inspection_files(table)
    Traceback (most recent call last):
    ...
    ccpp_datafile.CCPPDatatableError: Invalid file list entry type, 'banana'
    """
    inspection_files = list()
    for section in _find_table_section(table, "inspection_files"):
        if (not file_type) or (section.tag == file_type):
            for entry in section:
                if entry.tag == "file":
                    inspection_files.append(entry.text)
                else:
                    emsg = "Invalid file list entry type, '{}'"
                    raise CCPPDatatableError(emsg.format(entry.tag))
    return inspection_files


def _retrieve_process_list(table):
    """Find and return a list of all physics scheme processes in <table>.

    capgen does not currently record a ``process`` attribute on
    scheme entries, so this returns an empty list when no scheme carries
    one.  The flag is kept for CLI compatibility.

    # Test valid module
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><schemes>"\
                "<scheme name='scheme1' process='three'><initialize></initialize></scheme>"\
                "<scheme name='scheme2' process='four'><finalize></finalize></scheme>"\
                "</schemes></ccpp_datatable>")
    >>> _retrieve_process_list(table)
    ['four=scheme2', 'three=scheme1']

    # Test no schemes element
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'></ccpp_datatable>")
    >>> _retrieve_process_list(table)
    Traceback (most recent call last):
    ...
    ccpp_datafile.CCPPDatatableError: Could not find 'schemes' element
    """
    result = list()
    schemes = table.find("schemes")
    if schemes is None:
        raise CCPPDatatableError("Could not find 'schemes' element")
    for scheme in schemes:
        name = scheme.get("name")
        proc = scheme.get("process")
        if proc:
            result.append("{}={}".format(proc, name))
    return sorted(result)


def _retrieve_module_list(table):
    """Find and return a list of all scheme modules in <table>.
    # Test valid module
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><schemes>"\
                "<scheme><initialize module='partridge'></initialize></scheme>"\
                "<scheme><finalize module='turtle_dove'></finalize></scheme>"\
                "</schemes></ccpp_datatable>")
    >>> _retrieve_module_list(table)
    ['partridge', 'turtle_dove']

    # Test no schemes element
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'></ccpp_datatable>")
    >>> _retrieve_module_list(table)
    Traceback (most recent call last):
    ...
    ccpp_datafile.CCPPDatatableError: Could not find 'schemes' element

    """
    result = set()
    schemes = table.find("schemes")
    if schemes is None:
        raise CCPPDatatableError("Could not find 'schemes' element")
    for scheme in schemes:
        for phase in scheme:
            module = phase.get("module")
            if module is not None:
                result.add(module)
    return sorted(result)


def _retrieve_dependencies(table):
    """Find and return a sorted, dedup'd list of host and scheme
    dependency file paths (collected from the ``dependencies`` attribute
    in metadata tables).
    # Test valid dependencies
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><dependencies>" \
               "<dependency>banana</dependency><dependency>orange"           \
               "</dependency></dependencies></ccpp_datatable>")
    >>> _retrieve_dependencies(table)
    ['banana', 'orange']

    # Test no dependencies
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><dependencies>" \
               "</dependencies></ccpp_datatable>")
    >>> _retrieve_dependencies(table)
    []

    # Test missing dependencies tag
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'></ccpp_datatable>")
    >>> _retrieve_dependencies(table)
    Traceback (most recent call last):
    ...
    ccpp_datafile.CCPPDatatableError: Could not find 'dependencies' element
    """
    result = set()
    depends = table.find("dependencies")
    if depends is None:
        raise CCPPDatatableError("Could not find 'dependencies' element")
    for dependency in depends:
        dep_file = dependency.text
        if dep_file is not None:
            result.add(dep_file)
    return sorted(result)


def _find_var_dictionary(table, dict_name=None, dict_type=None):
    """Find and return a var_dictionary in <table>.
    If not found, return None.
    # Test valid table with dict_name provided
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><var_dictionaries>"\
                "<var_dictionary name='banana' type='host'><variables>"\
                "<var name='hi'></var></variables></var_dictionary>"\
                "<var_dictionary name='orange' type='api'></var_dictionary>"\
                "</var_dictionaries></ccpp_datatable>")
    >>> _find_var_dictionary(table, dict_name='orange').get("name")
    'orange'

    # Test valid table with dict_type provided
    >>> _find_var_dictionary(table, dict_type='host').get("name")
    'banana'

    # Test valid table with both dict_type and dict_name provided
    >>> _find_var_dictionary(table, dict_type='host', dict_name='banana').get("name")
    'banana'

    # Test no table found (expect None)
    >>> _find_var_dictionary(table, dict_name='apple') is None
    True

    # Test error handling
    >>> _find_var_dictionary(table)
    Traceback (most recent call last):
    ...
    ValueError: At least one of <dict_name> or <dict_type> must contain a string
    """
    var_dicts = table.find("var_dictionaries")
    target_dict = None
    if (dict_name is None) and (dict_type is None):
        raise ValueError(("At least one of <dict_name> or <dict_type> must "
                          "contain a string"))
    if var_dicts is None:
        return None
    for vdict in var_dicts:
        if (((dict_name is None) or (vdict.get("name") == dict_name)) and
            ((dict_type is None) or (vdict.get("type") == dict_type))):
            target_dict = vdict
            break
    return target_dict


def _retrieve_suite_list(table):
    """Find and return a list of all suites found in <table>.
    # Test suites are found
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><api><suites>"\
                "<suite name='umbrella'></suite><suite name='galoshes'></suite>"\
                "</suites></api></ccpp_datatable>")
    >>> _retrieve_suite_list(table)
    ['umbrella', 'galoshes']

    # Test suites not found
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'></ccpp_datatable>")
    >>> _retrieve_suite_list(table)
    []
    """
    result = list()
    api_elem = table.find("api")
    if api_elem is not None:
        suites_elem = api_elem.find("suites")
        if suites_elem is not None:
            for suite in suites_elem:
                result.append(suite.get("name"))
    return result


def _retrieve_suite_group_names(table, suite_name):
    """Find and return a list of the group names for this suite.
    # Test suites are found
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><api><suites>"\
                "<suite name='umbrella'><group name='florence'></group>"\
                "<group name='delores'></group><group name='edna'></group></suite>"\
                "<suite name='galoshes'><group name='clarence'></group></suite>"\
                "</suites></api></ccpp_datatable>")
    >>> _retrieve_suite_group_names(table, 'umbrella')
    ['florence', 'delores', 'edna']

    # Test non-present suite
    >>> _retrieve_suite_group_names(table, 'poncho')
    []
    """
    result = list()
    api_elem = table.find("api")
    if api_elem is not None:
        suites_elem = api_elem.find("suites")
        if suites_elem is not None:
            for suite in suites_elem:
                if suite.get("name") == suite_name:
                    for item in suite:
                        if item.tag == "group":
                            result.append(item.get("name"))
    return result


def _is_variable_protected(table, var_name, var_dict):
    """Determine whether variable, <var_name>, from <var_dict> is protected.
    Do this by checking the 'protected' attribute for <var_name> in
    <var_dict> or any of <var_dict>'s ancestors (parent dictionaries).
    # Test found variable
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><var_dictionaries>"\
                "<var_dictionary name='banana' type='host'><variables>"\
                "<var name='hi' protected='True'></var></variables></var_dictionary>"\
                "<var_dictionary name='orange' type='api'><variables><var name='hello'>"\
                "</var><var name='howdy'></var></variables></var_dictionary></var_dictionaries></ccpp_datatable>")
    >>> var_dict = _find_var_dictionary(table, dict_name="banana")
    >>> _is_variable_protected(table, "hi", var_dict)
    True

    >>> var_dict = _find_var_dictionary(table, dict_type="api")
    >>> _is_variable_protected(table, "hello", var_dict)
    False

    # Test non-present variable also returns False
    >>> _is_variable_protected(table, "hiya", var_dict)
    False
    """
    protected = False
    while (not protected) and (var_dict is not None):
        dvars = var_dict.find("variables")
        if dvars is not None:
            for var in dvars:
                if var.get("name") == var_name:
                    protected = var.get("protected", default="False") == "True"
                    break
        parent = var_dict.get("parent")
        if parent is not None:
            var_dict = _find_var_dictionary(table, dict_name=parent)
        else:
            var_dict = None
    return protected


def _retrieve_variable_list(table, suite_name,
                            intent_type=None, exclude_protected=True):
    """Find and return a list of all the required variables in <suite_name>.
    If suite, <suite_name>, is not found in <table>, return an empty list.
    If <intent_type> is present, return only that variable type (input or
    output).
    If <exclude_protected> is True, do not include protected variables.
    >>> table = ET.fromstring("<ccpp_datatable version='1.0'><api><suites><suite name='fruit'>"\
            "<group name='orange'></group></suite></suites></api><var_dictionaries>"\
            "<var_dictionary name='banana' type='host'><variables><var name='var1' intent='inout'>"\
            "</var><var name='var2' intent='out' protected='True'></var></variables></var_dictionary>"\
            "<var_dictionary name='orange_call_list' type='group_call_list'><variables><var name='var3' intent='in'>"\
            "</var><var name='var4' intent='out' protected='True'></var><var name='var5' intent='inout' protected='True'>"\
            "</var></variables></var_dictionary></var_dictionaries></ccpp_datatable>")

    # Test group variable retrieval
    >>> _retrieve_variable_list(table, 'fruit', exclude_protected=False)
    ['var3', 'var4', 'var5']

    >>> _retrieve_variable_list(table, 'fruit')
    ['var3']

    >>> _retrieve_variable_list(table, 'fruit', intent_type='input', exclude_protected=False)
    ['var3', 'var5']

    >>> _retrieve_variable_list(table, 'fruit', intent_type='output', exclude_protected=False)
    ['var4', 'var5']

    >>> _retrieve_variable_list(table, 'fruit', intent_type='input')
    ['var3']

    >>> _retrieve_variable_list(table, 'fruit', intent_type='output')
    []

    # Test host variable retrieval
    >>> _retrieve_variable_list(table, 'fruit', intent_type='host', exclude_protected=False)
    ['var1', 'var2']

    >>> _retrieve_variable_list(table, 'fruit', intent_type='host')
    ['var1']

    # Test invalid intent type
    >>> _retrieve_variable_list(table, 'fruit', intent_type='banana')
    Traceback (most recent call last):
    ...
    ccpp_datafile.CCPPDatatableError: Invalid intent_type, 'banana'

    """
    var_set = set()
    excl_vars = list()
    if intent_type == "host":
        allowed_intents = list()
    elif intent_type is None:
        allowed_intents = ['in', 'out', 'inout']
    elif intent_type == "input":
        allowed_intents = ['in', 'inout']
    elif intent_type == "output":
        allowed_intents = ['out', 'inout']
    else:
        emsg = "Invalid intent_type, '{}'"
        raise CCPPDatatableError(emsg.format(intent_type))
    if exclude_protected or (intent_type == "host"):
        host_dict = _find_var_dictionary(table, dict_type="host")
        if host_dict is not None:
            hvars = host_dict.find("variables")
            if hvars is not None:
                for var in hvars:
                    vname = var.get("name")
                    if exclude_protected:
                        exclude = _is_variable_protected(table, vname,
                                                         host_dict)
                    else:
                        exclude = False
                    if intent_type == "host":
                        if not exclude:
                            var_set.add(vname)
                    else:
                        if exclude:
                            excl_vars.append(vname)
    if intent_type != "host":
        group_names = _retrieve_suite_group_names(table, suite_name)
        for group in group_names:
            cl_name = group + "_call_list"
            group_dict = _find_var_dictionary(table, dict_name=cl_name,
                                              dict_type="group_call_list")
            if group_dict is not None:
                gvars = group_dict.find("variables")
                if gvars is not None:
                    for var in gvars:
                        vname = var.get("name")
                        vintent = var.get("intent")
                        if exclude_protected:
                            exclude = vname in excl_vars
                            if not exclude:
                                exclude = _is_variable_protected(table, vname,
                                                                 group_dict)
                        else:
                            exclude = False
                        if (vintent in allowed_intents) and (not exclude):
                            var_set.add(vname)
    return sorted(var_set)


def datatable_report(datatable, action, sep, exclude_protected=False):
    """Perform a lookup <action> on <datatable> and return the result."""
    if not action:
        emsg = "datatable_report: An action is required\n"
        emsg += _command_line_parser().format_usage()
        raise ValueError(emsg)
    if not sep:
        emsg = "datatable_report: A separator character (<sep>) is required\n"
        emsg += _command_line_parser().format_usage()
        raise ValueError(emsg)
    table = _read_datatable(datatable)
    if action.action_is("capgen_files"):
        result = _retrieve_capgen_files(table)
    elif action.action_is("host_files"):
        result = _retrieve_capgen_files(table, file_type="host_files")
    elif action.action_is("suite_files"):
        result = _retrieve_capgen_files(table, file_type="suite_files")
    elif action.action_is("scheme_files"):
        result = _retrieve_scheme_files(table)
    elif action.action_is("utility_files"):
        result = _retrieve_capgen_files(table, file_type="utilities")
    elif action.action_is("inspection_files"):
        result = _retrieve_inspection_files(table)
    elif action.action_is("process_list"):
        result = _retrieve_process_list(table)
    elif action.action_is("module_list"):
        result = _retrieve_module_list(table)
    elif action.action_is("dependencies"):
        result = _retrieve_dependencies(table)
    elif action.action_is("suite_list"):
        result = _retrieve_suite_list(table)
    elif action.action_is("required_variables"):
        result = _retrieve_variable_list(table, action.value,
                                         exclude_protected=exclude_protected)
    elif action.action_is("input_variables"):
        result = _retrieve_variable_list(table, action.value,
                                         intent_type="input",
                                         exclude_protected=exclude_protected)
    elif action.action_is("output_variables"):
        result = _retrieve_variable_list(table, action.value,
                                         intent_type="output",
                                         exclude_protected=exclude_protected)
    elif action.action_is("host_variables"):
        result = _retrieve_variable_list(table, "host",
                                         exclude_protected=exclude_protected,
                                         intent_type="host")
    else:
        result = ''
    if isinstance(result, list):
        result = sep.join(result)
    return result


def _indent_str(indent):
    """Return the line start string for indent level, <indent>."""
    return _INDENT_STR * indent


def _format_line(line_in, indent, line_wrap, increase_indent=True):
    """Format <line_in> into separate lines in an attempt to not have the
    length of any line greater than <line_wrap> characters including any
    indent (with indent level specified by <indent>).
    If <increase_indent> is True, increase the indent level for new lines
    created by the process.
    A value of <line_wrap> less one means do not wrap the line.
    >>> line = "This is a very long string that should be wrapped hopefully"
    >>> _format_line(line, 1, 50)
    '  This is a very long string that should be\\n      wrapped hopefully\\n'

    >>> _format_line(line, 1, 50, increase_indent=False)
    '  This is a very long string that should be\\n  wrapped hopefully\\n'

    >>> _format_line(line, 0, 50)
    'This is a very long string that should be wrapped\\n    hopefully\\n'

    >>> line = 'short line'
    >>> _format_line(line, 0, 2, increase_indent=False)
    'short\\nline\\n'
    """
    in_squote = False
    in_dquote = False
    outline = ''
    indent_str = _indent_str(indent)
    curr_indent = len(indent_str)
    wrap_points = list()
    line = line_in.strip()
    llen = len(line)
    if (line_wrap <= 0) or (llen + curr_indent <= line_wrap):
        index = llen + 1
    else:
        index = 0
    while index < llen:
        inchar = line[index]
        if in_squote:
            if inchar == "'":
                in_squote = False
        elif in_dquote:
            if inchar == '"':
                in_dquote = False
        elif inchar == ' ':
            wrap_points.append(index + curr_indent)
        index += 1
    if (line_wrap <= 0) or (llen + curr_indent <= line_wrap):
        this_line = indent_str + line
        next_line = ""
    else:
        good_points = [x for x in wrap_points if x <= line_wrap]
        if increase_indent:
            indent += 2
        if good_points:
            wrap = max(good_points) - curr_indent
            this_line = indent_str + line[0:wrap]
            next_line = _format_line(line[wrap+1:], indent, line_wrap,
                                     increase_indent=False)
        elif wrap_points:
            wrap = min(wrap_points) - curr_indent
            this_line = indent_str + line[0:wrap]
            next_line = _format_line(line[wrap+1:], indent, line_wrap,
                                     increase_indent=False)
        else:
            this_line = indent_str + line
            next_line = ""
    outline = this_line + '\n' + next_line
    return outline


def table_entry_pretty_print(entry, indent, line_wrap=-1):
    """Create and return a pretty print string of the contents of <entry>.
    >>> table = ET.fromstring("<ccpp_datatable><banana name='apple'></banana></ccpp_datatable>")
    >>> table_entry_pretty_print(table, 0)
    '<ccpp_datatable>\\n  <banana name=apple />\\n</ccpp_datatable>\\n'

    >>> table_entry_pretty_print(table, 1, line_wrap=20)
    '  <ccpp_datatable>\\n    <banana\\n        name=apple\\n        />\\n  </ccpp_datatable>\\n'
    """
    output = ""
    outline = "<{}".format(entry.tag)
    for name in entry.attrib:
        outline += " {}={}".format(name, entry.attrib[name])
    has_children = len(list(entry)) > 0
    has_text = entry.text
    if has_children or has_text:
        outline += ">"
        output += _format_line(outline, indent, line_wrap)
    else:
        outline += " />"
        output += _format_line(outline, indent, line_wrap)
    if has_children:
        for child in entry:
            output += table_entry_pretty_print(child, indent+1,
                                               line_wrap=line_wrap)
    if has_text:
        output += _format_line(entry.text, indent+1, line_wrap)
    if has_children or has_text:
        outline = "</{}>".format(entry.tag)
        output = output.rstrip() + '\n' + _format_line(outline,
                                                       indent, line_wrap)
    return output


def datatable_pretty_print(datatable, indent, line_wrap):
    """Create and return a pretty print string of the contents of <datatable>."""
    indent = 0
    table = _read_datatable(datatable)
    report = table_entry_pretty_print(table, indent, line_wrap=line_wrap)
    return report


###
### Main entry point
###


def main(argv: Optional[List[str]] = None) -> int:
    global _INDENT_STR
    if argv is None:
        argv = sys.argv[1:]
    pargs = parse_command_line(argv)
    if pargs.show:
        _INDENT_STR = " " * pargs.indent
        report = datatable_pretty_print(pargs.datatable, 0,
                                        line_wrap=pargs.line_wrap)
    else:
        arg_vars = vars(pargs)
        action = None
        errmsg = ''
        esep = ''
        for opt in arg_vars:
            if (opt in DatatableReport.valid_actions()) and arg_vars[opt]:
                if action:
                    errmsg += esep + "Duplicate action, '{}'".format(opt)
                    esep = '\n'
                else:
                    action = DatatableReport(opt, arg_vars[opt])
        if errmsg:
            raise ValueError(errmsg)
        report = datatable_report(pargs.datatable, action,
                                  pargs.sep, pargs.exclude_protected)
    print("{}".format(report.rstrip()))
    return 0


if __name__ == "__main__":
    sys.exit(main())
