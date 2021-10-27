#! /bin/bash

root=$( dirname $( cd $( dirname ${0}); pwd -P ) )
test_dir=${root}/test

perr() {
  # Print error message ($2) on error ($1)
  if [ ${1} -ne 0 ]; then
    echo "ERROR: ${2}"
    if [ $# -gt 2 ]; then
      exit ${3}
    else
      exit 1
    fi
  fi
}


cd ${test_dir}
perr $? "Cannot cd to test directory, '${test_dir}'"

# Run capgen test
./capgen_test/run_test
perr $? "Failure running capgen test"

# Run advection test
./advection_test/run_test
perr $? "Failure running advection test"

# Run doctests
./run_doctest.sh
perr $? "Failure running doctests"

for test in `ls unit_tests/test_*.py`; do
  echo "Running unit test, ${test}"
  python3 ${test}
  perr $? "Failure running unit test, ${test}"
done

echo "All tests PASSed!"
