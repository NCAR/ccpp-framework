#! /bin/bash

lname="caplocal.txt"
if [ -f "${lname}" ]; then
  rm ${lname}
fi
touch ${lname}
while read line || [[ -n "${line}" ]]; do
  echo "`basename ${line} .F90`.o" >> ${lname}
done < "${1}"
