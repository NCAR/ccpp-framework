"""Code to generate and query the CCPP datafile returned by capgen.
The CCPP datafile is a database consisting of several tables:
- A list of all generated files, broken into groups for scheme files,
  host cap, and suite caps.
- A list of CCPP metadata files actually used by capgen, broken into groups
  for host-model metadata and scheme metadata. These filenames will serve
  as keys
"""

def generate_ccpp_datatable(filename):
    """Generate the CCPP datatable based on the inputs.
    """
    # Define new tree
    newdef = ET.Element("ccpp_datatable")

    # Write tree
    newdef_tree = ET.ElementTree(newdef)
    newdef_tree.write(filename)
