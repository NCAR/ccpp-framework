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
from ccpp_suite    import COPYRIGHT, CCPP_STATE_MACH
from metavar       import VarDictionary
from fortran_tools import FortranWriter

###############################################################################
header = '''
!>
!! @brief Auto-generated cap for {host_model} calls to CCPP API
!!
!
module {module}

   use machine
'''

preamble='''
   implicit none
   private

'''

subhead = '''
   subroutine {host_model}_ccpp_physics_{stage}({api_vars})
'''

subfoot = '''
   end subroutine {host_model}_ccpp_physics_{stage}
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
        # CCPP_STATE_MACH.transitions represents the host CCPP interface
        for stage in CCPP_STATE_MACH.transitions():
            cap.write("public :: {host_model}_ccpp_physics_{stage}".format(host_model=host_model.name, stage=stage), 1)
        # End for
        cap.write('\ncontains\n', 0)
        for stage in CCPP_STATE_MACH.transitions():
            loop_vars = stage == 'run'
            # All interfaces need the suite name
            apivars = [api.suite_name_var]
            if loop_vars:
                # Only the run phase needs a suite part name
                apivars.append(api.suite_part_var)
            # End if
            hdvars = list() # Host interface dummy args
            hmvars = list() # Host model to API dummy args
            # Collect all the variables which do not have a <module> source.
            # Only the run phase needs loop variables
            for var in host_model.variable_list(loop_vars=loop_vars):
                hmvars.append(var)
                lname = var.get_prop_value('local_name')
                if host_model.host_variable_module(lname) is None:
                    hdvars.append(var)
                # End if
            # End for
            lnames = [x.get_prop_value('local_name') for x in apivars + hdvars]
            api_vlist = ", ".join(lnames)
            cap.write(subhead.format(api_vars=api_vlist,
                                     host_model=host_model.name,
                                     stage=stage), 1)
            for var in apivars:
                var.write_def(cap, 2)
            # End for
            for var in hdvars:
                var.write_def(cap, 2)
            # End for
            cap.write('', 0)
            # Now, call the real API with the host model's variable list
            callstr = 'call ccpp_physics_{}({})'
            lnames = [x.get_prop_value('local_name') for x in apivars + hmvars]
            api_vlist = ", ".join(lnames)
            cap.write(callstr.format(stage, api_vlist), 2)
            cap.write(subfoot.format(host_model=host_model.name,
                                     stage=stage), 1)
        # End for
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
