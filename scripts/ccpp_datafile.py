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

###
### Interface for retrieving datatable information
###

###############################################################################
def parse_command_line(args):
###############################################################################
    "Create an ArgumentParser to parse and return command-line arguments"
    format = argparse.RawTextHelpFormatter
    description = """
    Retrieve information about a ccpp_capgen run.
    """
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=format)
    parser.add_argument("datatable", type=str, required=True,
                        help="Path to a data table XML file created by capgen")
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--host-files", action='store_true', default=False
                        help="""Return a comma-separated list of host CAP
                        files created by capgen""")
    group.add_argument("--suite-files", action='store_true', default=False
                        help="""Return a comma-separated list of suite CAP
                        files created by capgen""")
    group.add_argument("--utility-files", action='store_true', default=False
                        help="""Return a comma-separated list of utility
                        files created by capgen (e.g., ccpp_kinds.F90)""")
    group.add_argument("--ccpp-files", action='store_true', default=False
                        help="""Return a comma-separated list of all
                        files created by capgen""")
    pargs = parser.parse_args(args)
    return pargs


###
### Accessor functions to retrieve information from a datatable file
###

###############################################################################
def _main(datatable, host_files, suite_files, utility_files, ccpp_files):
###############################################################################
    result = ''
    if host_files:
        result += retrieve_host_files(datatable)

###
### Functions to create the datatable file
###

def new_scheme_entry(parent):
    """Return a new XML entry for <scheme> under <parent>"""
    entry = ET.SubElement(parent, "scheme")
    entry.set("name", "foo")

    return entry


def add_generated_files(parent, host_files, suite_files, ccpp_kinds):
    """Add a section to <parent> that lists all the files generated
    by <api> in sections for host cap, suite caps and ccpp_kinds.
    """
    file_entry = ET.SubElement(parent, "ccpp_files")
    utilities = ET.SubElement(parent, "utilities")
    entry = ET.SubElement(parent, "file")
    entry.text = ccpp_kinds
    host_files = ET.SubElement(parent, "host_files")
    for hfile in host_files:
        entry = ET.SubElement(parent, "file")
        entry.text = hfile
    # end for
    suite_files = ET.SubElement(parent, "suite_files")
    for sfile in suite_files:
        entry = ET.SubElement(parent, "file")
        entry.text = sfile

def generate_ccpp_datatable(filename, api, host_files, suite_files, ccpp_kinds):
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
    result = _main_func(args.datatable, args.host_files, args.suite_files,
                        args.utility_files, args.ccpp_files)
    print("{}".format(result))
    sys.exit(0)
