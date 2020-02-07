"""Code to generate and query the CCPP datafile returned by capgen.
The CCPP datafile is a database consisting of several tables:
- A list of all generated files, broken into groups for scheme files,
  host cap, suite caps, and ccpp_kinds.
- A list of scheme entries, keyed by scheme name
- A list of CCPP metadata files actually used by capgen, broken into groups
  for host-model metadata and scheme metadata. These filenames may serve
  as keys
- A list of variable entries, keyed by standard name.
"""

# Python library imports
import xml.etree.ElementTree as ET

def new_scheme_entry(parent):
    """Return a new XML entry for <scheme> under <parent>"""
    entry = ET.SubElement(parent, "scheme")
    entry.set("name", "foo")

    return entry


def generate_ccpp_datatable(filename):
    """Generate the CCPP datatable based on the inputs.
    """
    # Define new tree
    newdef = ET.Element("ccpp_datatable")
    _ = new_scheme_entry(newdef)

    # Write tree
    newdef_tree = ET.ElementTree(newdef)
    newdef_tree.write(filename)
