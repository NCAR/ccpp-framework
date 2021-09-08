import logging
import os
import sys

#sys.path.append(os.path.join(os.path.split(__file__)[0], '../scripts/parse_tools'))
from parse_checkers import registered_fortran_ddt_names
from metadata_table import MetadataTable, parse_metadata_file, Var
#from metadata_table import MetadataHeader

example_table = """
[ccpp-table-properties]
  name = <name>
  type = scheme
  relative_path = path
  dependencies = a.f,b.f

[ccpp-arg-table]
  name = <name>
  type = scheme
[ im ]
  standard_name = horizontal_loop_extent
  long_name = horizontal loop extent, start at 1
  units = index
  type = integer
  dimensions = ()
  intent = in
"""


def test_MetadataTable_parse_table(tmpdir):
    path = str(tmpdir.join("table.meta"))
    with open(path, "w") as f:
        f.write(example_table)

    metadata_headers = parse_metadata_file(path, known_ddts=registered_fortran_ddt_names(),
                                                        logger=logging.getLogger(__name__))

    # check metadata header
    assert len(metadata_headers) == 1
    metadata_header = metadata_headers[0]
    assert metadata_header.table_name == "<name>"
    assert metadata_header.table_type == "scheme"
    assert metadata_header.relative_path == "path"
    assert metadata_header.dependencies == ["a.f", "b.f"]

    # check metadata section
    assert len(metadata_header.sections()) == 1
    metadata_section = metadata_header.sections()[0]
    assert metadata_section.name == "<name>"
    assert metadata_section.type == "scheme"
    (im_data,) = metadata_section.variable_list()
    assert isinstance(im_data, Var)
    assert im_data.get_dimensions() == []
