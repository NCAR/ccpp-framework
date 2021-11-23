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

errcnt=0

# Run capgen test
./capgen_test/run_test
res=$?
errcnt=$((errcnt + res))
if [ $res -ne 0 ]; then
  echo "Failure running capgen test"
fi

# Run advection test
./advection_test/run_test
res=$?
errcnt=$((errcnt + res))
if [ $res -ne 0 ]; then
  echo "Failure running advection test"
fi

# Run doctests
./run_doctest.sh
res=$?
errcnt=$((errcnt + res))
if [ $res -ne 0 ]; then
  echo "${errcnt} doctest failures"
fi

for test in `ls unit_tests/test_*.py`; do
  echo "Running unit test, ${test}"
  python3 ${test}
  res=$?
  errcnt=$((errcnt + res))
  if [ $res -ne 0 ]; then
    echo "Failure, '${res}', running unit test, ${test}"
  fi
done

if [ $errcnt -eq 0 ]; then
  echo "All tests PASSed!"
else
  echo "${errcnt} tests FAILed"
fi
