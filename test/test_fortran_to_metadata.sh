#! /bin/bash

## Relevant directories and file paths
test_dir="$(cd $(dirname ${0}); pwd -P)"
script_dir="$(dirname ${test_dir})/scripts"
sample_files_dir="${test_dir}/unit_tests/sample_files"
f2m_script="${script_dir}/ccpp_fortran_to_metadata.py"
filename="test_fortran_to_metadata"
test_input="${sample_files_dir}/${filename}.F90"
tmp_dir="${test_dir}/unit_tests/tmp"
sample_meta="${sample_files_dir}/check_fortran_to_metadata.meta"

# Run the script
opts="--ddt-names serling_t"
${f2m_script} --output-root "${tmp_dir}" ${opts} "${test_input}"
res=$?

retval=0
if [ ${res} -ne 0 ]; then
    echo "FAIL: ccpp_fortran_to_metadata.py exited with error ${res}"
    retval=${res}
elif [ ! -f "${tmp_dir}/${filename}.meta" ]; then
    echo "FAIL: metadata file, '${tmp_dir}/${filename}.meta', not created"
    retval=1
else
    cmp --quiet "${sample_meta}" "${tmp_dir}/${filename}.meta"
    res=$?
    if [ ${res} -ne 0 ]; then
        echo "FAIL: Comparison with correct metadata file failed"
        retval=${res}
    else
        echo "PASS"
        # Cleanup
        rm "${tmp_dir}/${filename}.meta"
    fi
fi
exit ${retval}
