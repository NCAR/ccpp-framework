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
from parse_tools import read_xml_file

###
### Utilities
###

class CCPPDatatableError(ValueError):
    """Error specific to errors found in the CCPP capgen datafile"""
    def __init__(self, message):
        """Initialize this exception"""
        super(CCPPDatatableError, self).__init__(message)

###
### Interface for retrieving datatable information
###

###############################################################################
def _command_line_parser():
###############################################################################
    "Create and return an ArgumentParser for parsing the command line"
    format = argparse.RawTextHelpFormatter
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
    help = "Return a comma-separated list of host CAP files created by capgen"
    group.add_argument("--host-files", action='store_true', default=False,
                        help=help)
    help = "Return a comma-separated list of suite CAP files created by capgen"
    group.add_argument("--suite-files", action='store_true', default=False,
                        help=help)
    help = ("Return a comma-separated list of utility"
            "files created by capgen (e.g., ccpp_kinds.F90)")
    group.add_argument("--utility-files", action='store_true', default=False,
                        help=help)
    help = "Return a comma-separated list of all files created by capgen"
    group.add_argument("--ccpp-files", action='store_true', default=False,
                        help=help)
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
def datatable_report(datatable, host_files, suite_files, utility_files,
                     ccpp_files, sep):
###############################################################################
    """Perform a lookup action on <datatable> and return the result.
    Each input except for <datatable> and <sep> specifies an action boolean.
    Note that only one action is allowed per call so if more than one
    action is specified, an exception is raised.
    """
    ## Check that exactly one action is specified.
    ## Note: This check is only needed for import usage of this function
    ##       but is does not hurt to repeat it.
    num_actions = sum([host_files, suite_files, utility_files, ccpp_files])
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
def new_scheme_entry(parent, scheme):
###############################################################################
    """Return a new XML entry for <scheme> under <parent>"""
    entry = ET.SubElement(parent, "scheme")
    entry.set("name", "foo")

    return entry


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
def generate_ccpp_datatable(filename, api, host_files, suite_files, ccpp_kinds):
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

    # Write tree
    datatable_tree = ET.ElementTree(datatable)
    datatable_tree.write(filename)

###############################################################################

if __name__ == "__main__":
    args = parse_command_line(sys.argv[1:])
    result = datatable_report(args.datatable, args.host_files, args.suite_files,
                              args.utility_files, args.ccpp_files, args.sep)
    print("{}".format(result))
    sys.exit(0)
