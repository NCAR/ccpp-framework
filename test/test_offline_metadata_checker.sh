#! /bin/bash

## Relevant directories and file paths
root_dir="$(cd $(dirname ${0}); pwd -P)"
script_dir="$(dirname ${root_dir})/scripts/fortran_tools"
test_dir="$(dirname ${root_dir})/test/advection_test"
offline_script="${script_dir}/offline_check_fortran_vs_metadata.py"
relative_path="capgen_test"

# Run the script
${offline_script} --directory ${test_dir}
res=$?

retval=0
if [ ${res} -ne 0 ]; then
    echo "FAIL: offline_check_fortran_vs_metadata.py exited with error ${res} while checking ${test_dir}"
    retval=${res}
    exit ${retval}
else
    echo "PASS"
fi

# Run the script again with a relative path
cd ${root_dir}
${offline_script} --directory ${relative_path}
res=$?

retval=0
if [ ${res} -ne 0 ]; then
    echo "FAIL: offline_check_fortran_vs_metadata.py exited with error ${res} while checking ${relative_path}"
    retval=${res}
else
    echo "PASS"
fi
exit ${retval}
