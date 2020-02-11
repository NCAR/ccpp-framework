#!/usr/bin/env python

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

# Python library imports
import xml.etree.ElementTree as ET
import argparse
import sys
# CCPP framework imports
from parse_tools import read_xml_file
from metadata_table import MetadataTable

###
### Utilities
###

class CCPPDatatableError(ValueError):
    """Error specific to errors found in the CCPP capgen datafile"""
    pass

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
    help_str = "Return a list of host CAP files created by capgen"
    group.add_argument("--host-files", action='store_true', default=False,
                       help=help_str)
    help_str = "Return a list of suite CAP files created by capgen"
    group.add_argument("--suite-files", action='store_true', default=False,
                       help=help_str)
    help_str = ("Return a list of utility files created by capgen "
            "(e.g., ccpp_kinds.F90)")
    group.add_argument("--utility-files", action='store_true', default=False,
                       help=help_str)
    help_str = "Return a list of all files created by capgen"
    group.add_argument("--ccpp-files", action='store_true', default=False,
                       help=help_str)
    help_str = "Return a list of process types and implementing scheme name"
    group.add_argument("--process-list", action='store_true', default=False,
                       help=help_str)
    help_str = "Return a list of module names used in this set of suites"
    group.add_argument("--module-list", action='store_true', default=False,
                       help=help_str)
    ###
    parser.add_argument("--separator", type=str, required=False, default=",",
                        metavar="SEP", dest="sep",
                        help="String to separate items in a list")
    return parser

###############################################################################
def parse_command_line(args):
###############################################################################
    "Create an ArgumentParser to parse and return command-line arguments"
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
    if not found:
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
    return list(result)

###############################################################################
def datatable_report(datatable, host_files, suite_files, utility_files,
                     ccpp_files, process_list, module_list, sep):
###############################################################################
    """Perform a lookup action on <datatable> and return the result.
    Each input except for <datatable> and <sep> specifies an action boolean.
    Note that only one action is allowed per call so if more than one
    action is specified, an exception is raised.
    """
    ## Check that exactly one action is specified.
    ## Note: This check is only needed for import usage of this function
    ##       but is does not hurt to repeat it.
    num_actions = sum([host_files, suite_files, utility_files, ccpp_files,
                       process_list, module_list])
    if num_actions != 1:
        if num_actions > 1:
            emsg = "datatable_report: Only one action is allowed\n"
        # end if
        if num_actions < 1:
            emsg = "datatable_report: An action is required\n"
        # end if
        emsg += _command_line_parser().format_usage()
        raise ValueError(emsg)
    # end if
    table = _read_datatable(datatable)
    if ccpp_files:
        result = _retrieve_ccpp_files(table)
    elif host_files:
        result = _retrieve_ccpp_files(table, file_type="host_files")
    elif suite_files:
        result = _retrieve_ccpp_files(table, file_type="suite_files")
    elif utility_files:
        result = _retrieve_ccpp_files(table, file_type="utilities")
    elif process_list:
        result = _retrieve_process_list(table)
    elif module_list:
        result = _retrieve_module_list(table)
    else:
        result = ''
    # end if
    if isinstance(result, list):
        result = sep.join(result)
    # end if
    return result

###
### Functions to create the datatable file
###

###############################################################################
def new_var_entry(parent, var, full_entry=True):
###############################################################################
    """Create a variable sub-element of <parent> with information from <var>.
    If <full_entry> is False, only include standard name and intent.
    """
    prop_list = ["intent"]
    if full_entry:
        prop_list.extend(["local_name", "type", "kind", "units"])
    # end if
    ventry = ET.SubElement(parent, "var")
    ventry.set("name", var.get_prop_value("standard_name"))
    for prop in prop_list:
        value = var.get_prop_value(prop)
        if prop:
            ventry.set(prop, str(value))
        # end if
    # end for
    if full_entry:
        dims = var.get_dimensions()
        if dims:
            dim_entry = ET.SubElement(ventry, "dimensions")
            dim_entry.text = " ".join(dims)
        # end if
    # end if

###############################################################################
def new_scheme_entry(parent, scheme, group_name, scheme_headers):
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
            if proc != MetadataTable.unknown_process_type:
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
        call_list = ET.SubElement(phase_entry, "calllist")
        vlist = scheme.call_list.variable_list()
        for var in vlist:
            new_var_entry(call_list, var, full_entry=False)
        # end for
    # end if
    if process:
        sch_entry.set("process", proc)
    # end if

###############################################################################
def add_generated_files(parent, host_files, suite_files, ccpp_kinds):
###############################################################################
    """Add a section to <parent> that lists all the files generated
    by <api> in sections for host cap, suite caps and ccpp_kinds.
    """
    file_entry = ET.SubElement(parent, "ccpp_files")
    utilities = ET.SubElement(file_entry, "utilities")
    entry = ET.SubElement(utilities, "file")
    entry.text = ccpp_kinds
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
def generate_ccpp_datatable(filename, api, scheme_headers,
                            host_files, suite_files, ccpp_kinds):
###############################################################################
    """Write a CCPP datatable for <api> to <filename>.
    The datatable includes the generated filenames for the host cap,
    the suite caps, and the ccpp_kinds module.
    """
    # Define new tree
    datatable = ET.Element("ccpp_datatable")
    datatable.set("version", "1.0")
    # Write out the generated files
    add_generated_files(datatable, host_files, suite_files, ccpp_kinds)
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
                new_scheme_entry(schemes, scheme, gname, scheme_header_dict)
            # end for
        # end for
    # end for
    # Write tree
    datatable_tree = ET.ElementTree(datatable)
    datatable_tree.write(filename)

###############################################################################

if __name__ == "__main__":
    PARGS = parse_command_line(sys.argv[1:])
    REPORT = datatable_report(PARGS.datatable, PARGS.host_files,
                              PARGS.suite_files, PARGS.utility_files,
                              PARGS.ccpp_files, PARGS.process_list,
                              PARGS.module_list, PARGS.sep)
    print("{}".format(REPORT))
    sys.exit(0)
