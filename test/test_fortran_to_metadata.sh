#! /bin/bash

## Relevant directories and file paths
test_dir="$(dirname ${0})"
script_dir="$(dirname ${test_dir})/scripts"
f2m_script="${script_dir}/ccpp_fortran_to_metadata.py"
filename="test_fortran_to_metadata"
test_input="${test_dir}/${filename}.F90"
tmp_dir="${test_dir}/unit_tests/tmp"
sample_files="${test_dir}/unit_tests/sample_files"
sample_meta="${sample_files}/check_fortran_to_metadata.meta"

# Run the script
opts="--ddt-names serling_t"
${f2m_script} --output-root "${tmp_dir}" ${opts} "${test_input}"
