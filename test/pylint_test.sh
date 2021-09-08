#! /bin/bash

# Script to run pylint tests on CCPP Framework python scripts

# Add CCPP Framework paths to PYTHONPATH so pylint can find them
SCRIPTDIR="$( cd $( dirname ${0} ); pwd -P )"
SPINROOT="$( dirname ${SCRIPTDIR} )"
CCPPDIR="${SPINROOT}/scripts"
export PYTHONPATH="${CCPPDIR}:$PYTHONPATH"

pylintcmd="pylint --rcfile=${SCRIPTDIR}/.pylintrc"

# Test top-level scripts
scripts="${CCPPDIR}/ccpp_capgen.py"
scripts="${scripts} ${CCPPDIR}/ccpp_suite.py"
scripts="${scripts} ${CCPPDIR}/ddt_library.py"
scripts="${scripts} ${CCPPDIR}/host_cap.py"
scripts="${scripts} ${CCPPDIR}/host_model.py"
scripts="${scripts} ${CCPPDIR}/metadata_table.py"
scripts="${scripts} ${CCPPDIR}/metavar.py"
scripts="${scripts} ${CCPPDIR}/state_machine.py"
${pylintcmd} ${scripts}
# Test the fortran_tools module
${pylintcmd} ${CCPPDIR}/fortran_tools
# Test the parse_tools module
${pylintcmd} ${CCPPDIR}/parse_tools
# Test the fortran to metadata converter tool
${pylintcmd} ${CCPPDIR}/ccpp_fortran_to_metadata.py
