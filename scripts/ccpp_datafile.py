#!/usr/bin/env python3

"""Code to generate and query the CCPP datafile returned by capgen.
The CCPP datafile is a database consisting of several tables:
- A list of all generated files, broken into groups for host cap,
  suite caps, and ccpp_kinds.
- A list of scheme entries, keyed by scheme name
- A list of CCPP metadata files actually used by capgen, broken into groups
  for host-model metadata and scheme metadata. These filenames may serve
  as keys
- A list of variable entries, keyed by standard name.
"""

## NB: A new report must be added in two places:
##     1) In the list of DatatableReport._valid_reports
##     2) As an option in datatable_report

# Python library imports
import argparse
import logging
import os
import sys
import xml.etree.ElementTree as ET
# CCPP framework imports
from metadata_table import UNKNOWN_PROCESS_TYPE
from metavar import Var
from parse_tools import read_xml_file, PrettyElementTree
from suite_objects import VerticalLoop, Subcycle

# Global data
_INDENT_STR = "  "

## datatable_report must have an action for each report type
_VALID_REPORTS = [{"report" : "host_files", "type" : bool,
                   "help" :
                   "Return a list of host CAP files created by capgen"},
                  {"report" : "suite_files", "type" : bool,
                   "help" :
                   "Return a list of suite CAP files created by capgen"},
                  {"report" : "utility_files", "type" : bool,
                   "help" : ("Return a list of utility files created by "
                             "capgen (e.g., ccpp_kinds.F90)")},
                  {"report" : "ccpp_files", "type" : bool,
                   "help" : "Return a list of all files created by capgen"},
                  {"report" : "process_list", "type" : bool,
                   "help" : ("Return a list of process types and implementing "
                             "scheme name")},
                  {"report" : "module_list", "type" : bool,
                   "help" :
                   "Return a list of module names used in this set of suites"},
                  {"report" : "dependencies", "type" : bool,
                   "help" : ("Return a list of scheme and host "
                             "dependency module names")},
                  {"report" : "dyn_const_routines", "type" : bool,
                   "help" : ("Return the constituent routines for a suite")},
                  {"report" : "suite_list", "type" : bool,
                   "help" : "Return a list of configured suite names"},
                  {"report" : "required_variables", "type" : str,
                   "help" : ("Return a list of required variable "
                             "standard names for suite, <SUITE_NAME>"),
                   "metavar" : "SUITE_NAME"},
                  {"report" : "input_variables", "type" : str,
                   "help" : ("Return a list of required input variable "
                             "standard names for suite, <SUITE_NAME>"),
                   "metavar" : "SUITE_NAME"},
                  {"report" : "output_variables", "type" : str,
                   "help" : ("Return a list of required output variable "
                             "standard names for suite, <SUITE_NAME>"),
                   "metavar" : "SUITE_NAME"},
                  {"report" : "host_variables", "type" : bool,
                   "help" : ("Return a list of required host model variable "
                             "standard names")},
                  {"report" : "show", "type" : bool,
                   "help" :
                   "Pretty print the database contents to the screen"}]

###
### Utilities
###

class CCPPDatatableError(ValueError):
    """Error specific to errors found in the CCPP capgen datafile"""
    pass

class DatatableInternalError(ValueError):
    """Error class for reporting internal errors"""
    def __init__(self, message):
        """Initialize this exception"""
        logging.shutdown()
        super(DatatableInternalError, self).__init__(message)

class DatatableReport(object):
    """A class to hold a database report type and inquiry function"""

    __valid_actions = [x["report"] for x in _VALID_REPORTS]

    def __init__(self, action, value=True):
        """Initialize this report as report-type, <action>"""
        if action in DatatableReport.__valid_actions:
            self.__action = action
            self.__value = value
        else:
            raise ValueError("Invalid action, '{}'".format(action))
        # end if

    def action_is(self, action):
        """If <action> matches this report type, return True.
        Otherwise, return False"""
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

###############################################################################
def _command_line_parser():
###############################################################################
    "Create and return an ArgumentParser for parsing the command line"
    description = """
    Retrieve information about a ccpp_capgen run.
    The returned information is controlled by selecting an action from
    the list of optional arguments below.
    Note that exactly one action is required.
    """
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("datatable", type=str,
                        help="Path to a data table XML file created by capgen")
    ### Only one action per call
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
            # end if
        else:
            raise ValueError("Unknown report type, '{}'".format(report["type"]))
        # end if
    # end for
    ###
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

###############################################################################
def parse_command_line(args):
###############################################################################
    """Create an ArgumentParser to parse and return command-line arguments"""
    parser = _command_line_parser()
    pargs = parser.parse_args(args)
    return pargs

###
### Accessor functions to retrieve information from a datatable file
###

###############################################################################
def _read_datatable(datatable):
###############################################################################
    """Read the XML file, <datatable> and return its root node"""
    _, datatable = read_xml_file(datatable, None) # No logger
    return datatable

###############################################################################
def _find_table_section(table, elem_type):
###############################################################################
    """Look for and return an element type, <elem_type>, in <table>.
    Raise an exception if the element is not found."""
    found = table.find(elem_type)
    if found is None:
        emsg = "Element type, '{}', not found in table"
        raise CCPPDatatableError(emsg.format(elem_type))
    # end if
    return found

###############################################################################
def _retrieve_ccpp_files(table, file_type=None):
###############################################################################
    """Find and retrieve a list of generated filenames from <table>.
    If <file_type> is not None, only return that file type."""
    ccpp_files = list()
    # Find the files section
    for section in _find_table_section(table, "ccpp_files"):
        if (not file_type) or (section.tag == file_type):
            for entry in section:
                if entry.tag == "file":
                    ccpp_files.append(entry.text)
                else:
                    emsg = "Invalid file list entry type, '{}'"
                    raise CCPPDatatableError(emsg.format(entry.tag))
                # end if
            # end for
        # end if
    # end if
    return ccpp_files

###############################################################################
def _retrieve_process_list(table):
###############################################################################
    """Find and return a list of all physics scheme processes in <table>."""
    result = list()
    schemes = table.find("schemes")
    if schemes is None:
        raise CCPPDatatableError("Could not find 'schemes' element")
    # end if
    for scheme in schemes:
        name = scheme.get("name")
        proc = scheme.get("process")
        if proc:
            result.append("{}={}".format(proc, name))
        # end if
    # end for
    return result

###############################################################################
def _retrieve_module_list(table):
###############################################################################
    """Find and return a list of all scheme modules in <table>."""
    result = set()
    schemes = table.find("schemes")
    if schemes is None:
        raise CCPPDatatableError("Could not find 'schemes' element")
    # end if
    for scheme in schemes:
        for phase in scheme:
            module = phase.get("module")
            if module is not None:
                result.add(module)
            # end if
        # end for
    # end for
    return sorted(result)

###############################################################################
def _retrieve_dependencies(table):
###############################################################################
    """Find and return a list of all host and scheme dependencies."""
    result = set()
    depends = table.find("dependencies")
    if depends is None:
        raise CCPPDatatableError("Could not find 'dependencies' element")
    # end if
    for dependency in depends:
        dep_file = dependency.text
        if dep_file is not None:
            result.add(dep_file)
        # end if
    # end for
    return sorted(result)

###############################################################################
def _retrieve_dyn_const_routines(table):
###############################################################################
    """Find and return a list of all scheme constituent routines."""
    result = set()
    routines = table.find("dyn_const_routines")
    if routines is None:
        raise CCPPDatatableError("Could not find 'dyn_const_routine' element")
    # end if
    for routine in routines:
        routine_name = routine.text
        if routine_name is not None:
            result.add(routine_name)
        # end if
    # end for
    return sorted(result)

###############################################################################
def _find_var_dictionary(table, dict_name=None, dict_type=None):
###############################################################################
    """Find and return a var_dictionary named, <dict_name> in <table>.
    If not found, return None"""
    var_dicts = table.find("var_dictionaries")
    target_dict = None
    if (dict_name is None) and (dict_type is None):
        raise ValueError(("At least one of <dict_name> or <dict_type> must "
                          "contain a string"))
    # end if
    for vdict in var_dicts:
        if (((dict_name is None) or (vdict.get("name") == dict_name)) and
            ((dict_type is None) or (vdict.get("type") == dict_type))):
            target_dict = vdict
            break
        # end if
    # end for
    return target_dict

###############################################################################
def _retrieve_suite_list(table):
###############################################################################
    """Find and return a list of all suites found in <table>."""
    result = list()
    # First, find the API variable dictionary
    api_elem = table.find("api")
    if api_elem is not None:
        suites_elem = api_elem.find("suites")
        if suites_elem is not None:
            for suite in suites_elem:
                result.append(suite.get("name"))
            # end for
        # end if
    # end if
    return result

###############################################################################
def _retrieve_suite_group_names(table, suite_name):
###############################################################################
    """Find and return a list of the group names for this suite."""
    result = list()
    # First, find the API variable dictionary
    api_elem = table.find("api")
    if api_elem is not None:
        suites_elem = api_elem.find("suites")
        if suites_elem is not None:
            for suite in suites_elem:
                if suite.get("name") == suite_name:
                    for item in suite:
                        if item.tag == "group":
                            result.append(item.get("name"))
                        # end if
                    # end for
                # end if
            # end for
        # end if
    # end if
    return result

###############################################################################
def _is_variable_protected(table, var_name, var_dict):
###############################################################################
    """Determine whether variable, <var_name>, from <var_dict> is protected.
    So this by checking for the 'protected' attribute for <var_name> in
    <var_dict> or any of <var_dict>'s ancestors (parent dictionaries).
    """
    protected = False
    while (not protected) and (var_dict is not None):
        dvars = var_dict.find("variables")
        if dvars is not None:
            for var in dvars:
                if var.get("name") == var_name:
                    protected = var.get("protected", default="False") == "True"
                    break
                # end if
            # end for
        # end if
        parent = var_dict.get("parent")
        if parent is not None:
            var_dict = _find_var_dictionary(table, dict_name=parent)
        else:
            var_dict = None
        # end if
    # end while
    return protected

###############################################################################
def _retrieve_variable_list(table, suite_name,
                            intent_type=None, excl_prot=True):
###############################################################################
    """Find and return a list of all the required variables in <suite_name>.
    If suite, <suite_name>, is not found in <table>, return an empty list.
    If <intent_type> is present, return only that variable type (input or
    output).
    If <excl_prot> is True, do not include protected variables"""
    # Note that suites do not have call lists so we have to collect
    # all the variables from the suite's groups.
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
    # end if
    if excl_prot or (intent_type == "host"):
        host_dict = _find_var_dictionary(table, dict_type="host")
        if host_dict is not None:
            hvars = host_dict.find("variables")
            if hvars is not None:
                for var in hvars:
                    vname = var.get("name")
                    if excl_prot:
                        exclude = _is_variable_protected(table, vname,
                                                         host_dict)
                    else:
                        exclude = False
                    # end if
                    if intent_type == "host":
                        if not exclude:
                            # Add to host variable set
                            var_set.add(vname)
                        # end if
                    else:
                        if exclude:
                            # Add to list of protected variables
                            excl_vars.append(vname)
                        # end if
                    # end if
                # end for
            # end if
        # end if
    # end if
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
                        if excl_prot:
                            exclude = vname in excl_vars
                            if not exclude:
                                exclude = _is_variable_protected(table, vname,
                                                                 group_dict)
                            # end if
                        else:
                            exclude = False
                        # end if
                        if (vintent in allowed_intents) and (not exclude):
                            var_set.add(vname)
                        # end if
                    # end for
                # end if
            # end if
        # end for
    # end if
    return sorted(var_set)

###############################################################################
def datatable_report(datatable, action, sep, excl_prot=False):
###############################################################################
    """Perform a lookup <action> on <datatable> and return the result.
    """
    if not action:
        emsg = "datatable_report: An action is required\n"
        emsg += _command_line_parser().format_usage()
        raise ValueError(emsg)
    # end if
    if not sep:
        emsg = "datatable_report: A separator character (<sep>) is required\n"
        emsg += _command_line_parser().format_usage()
        raise ValueError(emsg)
    # end if
    table = _read_datatable(datatable)
    if action.action_is("ccpp_files"):
        result = _retrieve_ccpp_files(table)
    elif action.action_is("host_files"):
        result = _retrieve_ccpp_files(table, file_type="host_files")
    elif action.action_is("suite_files"):
        result = _retrieve_ccpp_files(table, file_type="suite_files")
    elif action.action_is("utility_files"):
        result = _retrieve_ccpp_files(table, file_type="utilities")
    elif action.action_is("process_list"):
        result = _retrieve_process_list(table)
    elif action.action_is("module_list"):
        result = _retrieve_module_list(table)
    elif action.action_is("dependencies"):
        result = _retrieve_dependencies(table)
    elif action.action_is("dyn_const_routines"):
        result = _retrieve_dyn_const_routines(table)
    elif action.action_is("suite_list"):
        result = _retrieve_suite_list(table)
    elif action.action_is("required_variables"):
        result = _retrieve_variable_list(table, action.value,
                                         excl_prot=excl_prot)
    elif action.action_is("input_variables"):
        result = _retrieve_variable_list(table, action.value,
                                         intent_type="input",
                                         excl_prot=excl_prot)
    elif action.action_is("output_variables"):
        result = _retrieve_variable_list(table, action.value,
                                         intent_type="output",
                                         excl_prot=excl_prot)
    elif action.action_is("host_variables"):
        result = _retrieve_variable_list(table, "host", excl_prot=excl_prot,
                                         intent_type="host")
    else:
        result = ''
    # end if
    if isinstance(result, list):
        result = sep.join(result)
    # end if
    return result

###############################################################################
def _indent_str(indent):
###############################################################################
    """Return the line start string for indent level, <indent>."""
    return _INDENT_STR*indent

###############################################################################
def _format_line(line_in, indent, line_wrap, increase_indent=True):
###############################################################################
    """Format <line_in> into separate lines in an attempt to not have the
    length of any line greater than <line_wrap> characters including any
    indent (with indent level specified by <indent>).
    If <increase_indent> is True, increase the indent level for new lines
    created by the process.
    A value of <line_wrap> less one means do not wrap the line.
    """
    in_squote = False
    in_dquote = False
    outline = ''
    indent_str = _indent_str(indent)
    curr_indent = len(indent_str)
    wrap_points = list()
    line = line_in.strip()
    llen = len(line)
    # Do we need to wrap the line?
    if (line_wrap <= 0) or (llen + curr_indent <= line_wrap):
        index = llen + 1
    else:
        index = 0
    # end if
    # Collect possible wrap points
    while index < llen:
        inchar = line[index]
        if in_squote:
            if inchar == "'":
                in_squote = False
            # end if (else do nothing)
        elif in_dquote:
            if inchar == '"':
                in_dquote = False
            # end if (else do nothing)
        elif inchar == ' ':
            wrap_points.append(index + curr_indent)
        # end if (else it is not an interesting character)
        index += 1
    # end while
    if (line_wrap <= 0) or (llen + curr_indent <= line_wrap):
        this_line = indent_str + line
        next_line = ""
    else:
        # Find the best break point
        good_points = [x for x in wrap_points if x <= line_wrap]
        if increase_indent:
            indent += 2 # To indent past child tags
        # end if
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
        # end if
    # end if
    outline = this_line + '\n' + next_line
    return outline

###############################################################################
def table_entry_pretty_print(entry, indent, line_wrap=-1):
###############################################################################
    """Create and return a pretty print string of the contents of <entry>"""
    output = ""
    outline = "<{}".format(entry.tag)
    for name in entry.attrib:
        outline += " {}={}".format(name, entry.attrib[name])
    # end for
    has_children = len(list(entry)) > 0
    has_text = entry.text
    if has_children or has_text:
        # We have sub-structure, close and print this tag
        outline += ">"
        output += _format_line(outline, indent, line_wrap)
    else:
        # No sub-structure, we are done with this tag
        outline += " />"
        output += _format_line(outline, indent, line_wrap)
    # end if
    if has_children:
        for child in entry:
            output += table_entry_pretty_print(child, indent+1,
                                               line_wrap=line_wrap)
        # end for
    # end if
    if has_text:
        output += _format_line(entry.text, indent+1, line_wrap)
    # end if
    if has_children or has_text:
        # We had sub-structure, print the close tag
        outline = "</{}>".format(entry.tag)
        output = output.rstrip() + '\n' + _format_line(outline,
                                                       indent, line_wrap)
    # end if
    return output

###############################################################################
def datatable_pretty_print(datatable, indent, line_wrap):
###############################################################################
    """Create and return a pretty print string of the contents of <datatable>"""
    indent = 0
    table = _read_datatable(datatable)
    report = table_entry_pretty_print(table, indent, line_wrap=line_wrap)
    return report

###
### Functions to create the datatable file
###

###############################################################################
def _object_type(pobj):
###############################################################################
    """Return an XML-acceptable string for the type of <pobj>."""
    return pobj.__class__.__name__.lower()

###############################################################################
def _new_var_entry(parent, var, full_entry=True):
###############################################################################
    """Create a variable sub-element of <parent> with information from <var>.
    If <full_entry> is False, only include standard name and intent.
    """
    prop_list = ["intent", "local_name"]
    if full_entry:
        prop_list.extend(["allocatable", "active", "default_value",
                          "diagnostic_name", "diagnostic_name_fixed",
                          "kind", "persistence", "polymorphic", "protected",
                          "state_variable", "type", "units", "molar_mass",
                          "advected", "top_at_one"])
        prop_list.extend(Var.constituent_property_names())
    # end if
    ventry = ET.SubElement(parent, "var")
    ventry.set("name", var.get_prop_value("standard_name"))
    for prop in prop_list:
        value = var.get_prop_value(prop)
        if value:
            ventry.set(prop, str(value))
        # end if
    # end for
    if full_entry:
        dims = var.get_dimensions()
        if dims:
            v_entry = ET.SubElement(ventry, "dimensions")
            v_entry.text = " ".join(dims)
        # end if
        v_entry = ET.SubElement(ventry, "source_type")
        v_entry.text = var.source.ptype.lower()
        v_entry = ET.SubElement(ventry, "source_name")
        v_entry.text = var.source.name.lower()
    # end if

###############################################################################
def _new_scheme_entry(parent, scheme, group_name, scheme_headers):
###############################################################################
    """Create a new XML entry for <scheme> under <parent>"""
    sch_name = scheme.name
    sch_entry = parent.find(sch_name)
    process = None
    if not sch_entry:
        sch_entry = ET.SubElement(parent, "scheme")
        sch_entry.set("name", sch_name)
    # end if
    if scheme.run_phase():
        sch_tag = group_name
    else:
        sch_tag = scheme.phase()
    # end if
    if not sch_tag:
        emsg = "No phase info for scheme, '{}', group = '{}"
        raise CCPPDatatableError(emsg.format(sch_name, group_name))
    # end if
    phase_entry = sch_entry.find(sch_tag)
    if phase_entry:
        pname = phase_entry.get("name")
        if pname != sch_name:
            emsg = "Scheme entry already exists for {} but name is {}"
            raise CCPPDatatableError(emsg.format(sch_name, pname))
        # end if
    else:
        phase_entry = ET.SubElement(sch_entry, sch_tag)
        phase_entry.set("name", sch_name)
        title = scheme.subroutine_name
        phase_entry.set("subroutine_name", title)
        phase_entry.set("filename", scheme.context.filename)
        if title in scheme_headers:
            header = scheme_headers[title]
            proc = header.process_type
            if proc != UNKNOWN_PROCESS_TYPE:
                if process:
                    if process != proc:
                        emsg = 'Inconsistent process, {} != {}'
                        raise CCPPDatatableError(emsg.format(proc, process))
                    # end if (no else, things are okay)
                else:
                    process = proc
                # end if
            # end if
            module = header.module
            if module:
                phase_entry.set("module", module)
            # end if
        else:
            emsg = 'Could not find metadata header for {}'
            raise CCPPDatatableError(emsg.format(sch_name))
        # end if
        call_list = ET.SubElement(phase_entry, "call_list")
        vlist = scheme.call_list.variable_list()
        for var in vlist:
            _new_var_entry(call_list, var, full_entry=False)
        # end for
    # end if
    if process:
        sch_entry.set("process", proc)
    # end if

###############################################################################
def _new_variable_dictionary(dictionaries, var_dict, dict_type, parent=None):
###############################################################################
    """Create a new XML entry for <var_dict> under <dictionaries>."""
    dict_entry = ET.SubElement(dictionaries, "var_dictionary")
    dict_entry.set("name", var_dict.name)
    dict_entry.set("type", dict_type)
    if parent is not None:
        dict_entry.set("parent", parent.name)
    # end if
    sub_dicts = var_dict.sub_dictionaries()
    if sub_dicts:
        sd_entry = ET.SubElement(dict_entry, "sub_dictionaries")
        sd_entry.text = " ".join([x.name for x in sub_dicts])
    # end if
    vars_entry = ET.SubElement(dict_entry, "variables")
    for var in var_dict.variable_list():
        _new_var_entry(vars_entry, var, full_entry=True)
    # end for

###############################################################################
def _add_suite_object_dictionaries(dictionaries, suite_object):
###############################################################################
    """Create new XML entries for <suite_object> under <dictionaries>.
    Add <suite_object>'s dictionary and its call_list dictionary (if present).
    Recurse to this objects parts."""
    dict_type = _object_type(suite_object)
    _new_variable_dictionary(dictionaries, suite_object, dict_type,
                             parent=suite_object.parent)
    if suite_object.call_list:
        dict_type += "_call_list"
        _new_variable_dictionary(dictionaries, suite_object.call_list,
                                 dict_type, parent=suite_object.parent)
    # end if
    for part in suite_object.parts:
        _add_suite_object_dictionaries(dictionaries, part)
    # end for

###############################################################################
def _add_dependencies(parent, scheme_depends, host_depends):
###############################################################################
    """Add a section to <parent> that lists all the dependencies
    required by schemes or the host model.
    """
    file_entry = ET.SubElement(parent, "dependencies")
    for hfile in host_depends:
        entry = ET.SubElement(file_entry, "dependency")
        entry.text = hfile
    # end for
    for sfile in scheme_depends:
        entry = ET.SubElement(file_entry, "dependency")
        entry.text = sfile
    # end for

###############################################################################
def _add_dyn_const_routine(file_entry, routine, scheme):
###############################################################################
    """Add a section to <parent> that lists all the constituent routines
    for the suite"""
    entry = ET.SubElement(file_entry, "dyn_const_routine")
    entry.text = routine
    entry.set("parent", scheme)
    # end for

###############################################################################
def _add_generated_files(parent, host_files, suite_files, ccpp_kinds, src_dir):
###############################################################################
    """Add a section to <parent> that lists all the files generated
    by <api> in sections for host cap, suite caps, ccpp_kinds, and source files.
    Also add existing utility files which are always needed by the framework.
    """
    file_entry = ET.SubElement(parent, "ccpp_files")
    utilities = ET.SubElement(file_entry, "utilities")
    entry = ET.SubElement(utilities, "file")
    entry.text = ccpp_kinds
    entry = ET.SubElement(utilities, "file")
    entry.text = os.path.join(src_dir, "ccpp_constituent_prop_mod.F90")
    entry = ET.SubElement(utilities, "file")
    entry.text = os.path.join(src_dir, "ccpp_hashable.F90")
    entry = ET.SubElement(utilities, "file")
    entry.text = os.path.join(src_dir, "ccpp_hash_table.F90")
    host_elem = ET.SubElement(file_entry, "host_files")
    for hfile in host_files:
        entry = ET.SubElement(host_elem, "file")
        entry.text = hfile
    # end for
    suite_elem = ET.SubElement(file_entry, "suite_files")
    for sfile in suite_files:
        entry = ET.SubElement(suite_elem, "file")
        entry.text = sfile
    # end for

###############################################################################
def _add_suite_object(parent, suite_object):
###############################################################################
    """Add an entry for <suite_object> under <parent>. This operation is
    recursive to all the components inside of <suite_object>"""
    obj_elem = ET.SubElement(parent, _object_type(suite_object))
    obj_elem.set("name", suite_object.name)
    ptype = suite_object.phase_type
    if ptype:
        obj_elem.set("phase", ptype)
    # end if
    if isinstance(suite_object, VerticalLoop):
        obj_elem.set("dimension_name", suite_object.dimension_name)
    # end if
    if isinstance(suite_object, Subcycle):
        obj_elem.set("loop", suite_object.loop)
    # end if
    for obj_part in suite_object.parts:
        _add_suite_object(obj_elem, obj_part)
    # end for

###############################################################################
def generate_ccpp_datatable(run_env, host_model, api, scheme_headers,
                            scheme_tdict, host_files, suite_files,
                            ccpp_kinds, source_dir):
###############################################################################
    """Write a CCPP datatable for <api> to <filename>.
    The datatable includes the generated filenames for the host cap,
    the suite caps, the ccpp_kinds module, and source code files.
    """
    # Define new tree
    datatable = ET.Element("ccpp_datatable")
    datatable.set("version", "1.0")
    # Write out the generated files
    _add_generated_files(datatable, host_files, suite_files,
                         ccpp_kinds, source_dir)
    # Write out scheme info
    schemes = ET.SubElement(datatable, "schemes")
    # Create a dictionary of the scheme headers for easy lookup
    scheme_header_dict = {}
    for header in scheme_headers:
        if header.title in scheme_header_dict:
            emsg = 'Header {} already in dictionary'
            raise CCPPDatatableError(emsg.format(header.title))
        # end if
        scheme_header_dict[header.title] = header
    # end for
    # Dump all scheme info from the suites
    for suite in api.suites:
        for group in suite.groups:
            gname = group.name
            for scheme in group.schemes():
                _new_scheme_entry(schemes, scheme, gname, scheme_header_dict)
            # end for
        # end for
    # end for
    # Write the API
    api_elem = ET.SubElement(datatable, "api")
    suites_elem = ET.SubElement(api_elem, "suites")
    for suite in api.suites:
        suite_elem = ET.SubElement(suites_elem, "suite")
        suite_elem.set("name", suite.name)
        suite_elem.set("filename", suite.sdf_name)
        for group in suite.groups:
            # Skip empty groups
            if group.parts:
                _add_suite_object(suite_elem, group)
            # end if
        # end for
    # end for
    # Dump the variable dictionaries
    var_dicts = ET.SubElement(datatable, "var_dictionaries")
    # First, the top-level dictionaries
    _new_variable_dictionary(var_dicts, host_model, "host")
    _new_variable_dictionary(var_dicts, api, "api", parent=api.parent)
    # Now, the suite and group namelists, etc. (including call_lists)
    for suite in api.suites:
        _new_variable_dictionary(var_dicts, suite, "suite", parent=suite.parent)
        for group in suite.groups:
            _add_suite_object_dictionaries(var_dicts, group)
            # end for
        # end for
    # end for
    # Add in all dependencies
    scheme_depends = set()
    for table in scheme_tdict:
        for dep_file in scheme_tdict[table].dependencies:
            scheme_depends.add(dep_file)
        # end for
    # end for
    host_depends = set()
    host_tables = host_model.metadata_tables()
    for table in host_tables:
        for dep_file in host_tables[table].dependencies:
            host_depends.add(dep_file)
        # end for
    # end for
    _add_dependencies(datatable, scheme_depends, host_depends)
    # Add in all constituent routines
    first_const_routine = True
    for table in scheme_tdict:
        if scheme_tdict[table].dyn_const_routine is not None:
            if first_const_routine:
                file_entry = ET.SubElement(datatable, "dyn_const_routines")
                first_const_routine = False
            # end if
            _add_dyn_const_routine(file_entry, scheme_tdict[table].dyn_const_routine, table)
        # end if
    # end for
    # Write tree
    datatable_tree = PrettyElementTree(datatable)
    datatable_tree.write(run_env.datatable_file)

###############################################################################

if __name__ == "__main__":
    PARGS = parse_command_line(sys.argv[1:])
    if PARGS.show:
        _INDENT_STR = " "*PARGS.indent
        LINE_WRAP = PARGS.line_wrap
        REPORT = datatable_pretty_print(PARGS.datatable, 0, line_wrap=LINE_WRAP)
    else:
        ARG_VARS = vars(PARGS)
        _ACTION = None
        _ERRMSG = ''
        _ESEP = ''
        for opt in ARG_VARS:
            if (opt in DatatableReport.valid_actions()) and ARG_VARS[opt]:
                if _ACTION:
                    _ERRMSG += _ESEP + "Duplicate action, '{}'".format(opt)
                    _ESEP = '\n'
                else:
                    _ACTION = DatatableReport(opt, ARG_VARS[opt])
                # end if
            # end if
        # end for
        if _ERRMSG:
            raise ValueError(_ERRMSG)
        # end if
        REPORT = datatable_report(PARGS.datatable, _ACTION,
                                  PARGS.sep, PARGS.exclude_protected)
    # end if
    print("{}".format(REPORT.rstrip()))
    sys.exit(0)
