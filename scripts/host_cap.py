#!/usr/bin/env python

"""
Parse a host-model registry XML file and return the captured variables.
"""

# Python library imports
from __future__ import print_function
import sys
import os
import os.path
import subprocess
import xml.etree.ElementTree as ET
# CCPP framework imports
from ccpp_suite    import COPYRIGHT
from fortran_tools import FortranWriter

###############################################################################
header = '''
!>
!! @brief Auto-generated cap for {host_model} calls to CCPP API
!!
!
module {module}
'''

preamble='''
   implicit none

   private
   public :: {host_model}_ccpp_physics
'''

subhead = '''
   subroutine {host_model}_ccpp_physics({api_vars})
'''

subfoot = '''
   end subroutine {host_model}_ccpp_physics
'''

footer = '''
end module {module}
'''

###############################################################################
def write_host_cap(host_model, api, output_dir, logger):
###############################################################################
    module_name = "{}_ccpp_cap".format(host_model.name)
    cap_filename = os.path.join(output_dir, '{}.F90'.format(module_name))
    with FortranWriter(cap_filename, 'w') as cap:
        cap.write(COPYRIGHT, 0)
        cap.write(header.format(host_model=host_model.name,
                                module=module_name), 0)
        modules = host_model.variable_locations()
        mlen = max([len(x[0]) for x in modules])
        for mod in modules:
            mspc = (mlen - len(mod[0]))*' '
            cap.write("use {}, {}only: {}".format(mod[0], mspc, mod[1]), 1)
        # End for
        cap.write(preamble.format(host_model=host_model.name), 1)
        cap.write('contains', 0)
        apivars = ", ".join(api.prop_list('local_name'))
        cap.write(subhead.format(api_vars=apivars, host_model=host_model.name), 1)
        api.declare_variables(cap, 2)
        cap.write('', 0)
        # Now, call the real API with the host model's variable list
        callstr = 'call ccpp_physics({}, {})'
        cap.write(callstr.format(apivars, host_model.argument_list()), 2)
        cap.write(subfoot.format(host_model=host_model.name), 1)
        cap.write(footer.format(module=module_name), 0)
    # End with
    return cap_filename

###############################################################################

if __name__ == "__main__":
    from parse_tools import initLog, setLogToNull
    logger = initLog('host_registry')
    setLogToNull(logger)
    # Run doctest
    import doctest
    doctest.testmod()
# No else:
