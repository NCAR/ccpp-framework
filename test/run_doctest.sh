#! /bin/bash

root=$( dirname $( cd $( dirname ${0}); pwd -P ) )
scripts=${root}/scripts

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

cd ${scripts}
perr $? "Cannot cd to scripts directory, '${scripts}'"

# Find all python scripts that have doctest
for pyfile in $(find . -name \*.py); do
  if [ -f "${pyfile}" ]; then
    if [ $(grep -c doctest ${pyfile}) -ne 0 ]; then
      python ${pyfile}
    fi
  fi
done
