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
from metavar import Var, VarDictionary
from ccpp_suite import COPYRIGHT

###############################################################################
header = '''
!>
!! @brief Auto-generated cap for {host_model} calls to CCPP API
!!
!
module {module}

{module_use}

   implicit none

   private
   public :: {host_model}_ccpp_physics

contains
'''

subhead = '''
   subroutine {host_model}_ccpp_physics(suite_name, suite_part)
      character(len=*), intent(in) :: suite_name
      character(len=*), intent(in) :: suite_part
'''

subfoot = '''
   end subroutine {host_model}_ccpp_physics
'''

footer = '''
end module {module}
'''

###############################################################################
def write_host_cap(host_model, output_dir, logger):
###############################################################################
    module_name = "{}_ccpp_cap".format(host_model.name)
    cap_filename = os.path.join(output_dir, '{}.F90'.format(module_name))
    module_use = ''
    with open(cap_filename, 'w') as cap:
        cap.write(COPYRIGHT)
        cap.write(header.format(host_model=host_model.name,
                                module=module_name, module_use=module_use))
        cap.write(subhead.format(host_model=host_model.name))
        # Now, call the real API with the host model's variable list
        callstr = 'call ccpp_physics({})'
        cap.write(subfoot.format(host_model=host_model.name))
        cap.write(footer.format(module=module_name))
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
