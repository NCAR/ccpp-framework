# How to run the python unit-tests

## Quick start:
To run all unit-tests:
```bash
$ export PYTHONPATH=<project_source_root_dir>/scripts:<project_source_root_dir>/scripts/parse_tools
$ pytest -v <project_source_root_dir>test/
```

To run a specific unit tests:
```bash
$ cd <root>/test/unit_tests
$ python  test_metadata_table.py
```
For more verbose output:
```bash
$ python  test_metadata_table.py -v 
```
If you have `coverage` installed, to get test coverage:
```bash
$ coverage run test_metadata_table.py
$ coverage report -m
```
To check source code quality with pylint:
```bash
$ cd <root>
$ env PYTHONPATH=scripts:${PYTHONPATH} pylint --rcfile ./test/.pylintrc ./test/unit_tests/test_metadata_table.py
$ env PYTHONPATH=scripts:${PYTHONPATH} pylint --rcfile ./test/.pylintrc ./test/unit_tests/test_metadata_scheme_file.py
```
