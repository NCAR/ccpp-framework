from metadata_table import MetadataHeader, Var

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


def test_MetadataHeader_parse_table(tmpdir):
    path = str(tmpdir.join("table.meta"))
    with open(path, "w") as f:
        f.write(example_table)

    table1, table2 = MetadataHeader.parse_metadata_file(path)

    # check first table
    assert table1.name == "<name>"
    assert table1.type == "scheme"
    assert table1.dependencies == ["path/a.f", "path/b.f"]

    # check second table
    assert table2.name == "<name>"
    assert table2.type == "scheme"
    (im_data,) = table2.variable_list()
    assert isinstance(im_data, Var)
    assert im_data.get_dimensions() == []
