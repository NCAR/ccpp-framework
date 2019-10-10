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
from ccpp_suite    import COPYRIGHT, KINDS_MODULE, CCPP_STATE_MACH, API
from metavar       import Var, VarDictionary, CCPP_CONSTANT_VARS
from fortran_tools import FortranWriter
from parse_tools   import CCPPError, ParseSource, ParseContext

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

__api_src_name__ = "CCPP_API"

__api_source__ = ParseSource(__api_src_name__, "MODULE",
                             ParseContext(filename="host_cap.F90"))

__suite_name_var__ = Var({'local_name':'suite_name',
                          'standard_name':'suite_name',
                          'intent':'in', 'type':'character',
                          'kind':'len=*', 'units':'', 'protected':'True',
                          'dimensions':'()'}, __api_source__)

__suite_part_var__ = Var({'local_name':'suite_part',
                          'standard_name':'suite_part',
                          'intent':'in', 'type':'character',
                          'kind':'len=*', 'units':'', 'protected':'True',
                          'dimensions':'()'}, __api_source__)

# Used to prevent loop substitution lookups
__blank_dict__ = VarDictionary(__api_src_name__)

###############################################################################
def suite_part_list(suite, stage):
###############################################################################
    "Return a list of all the suite parts for this stage"
    run_stage = stage == 'run'
    if run_stage:
        spart_list = list()
        for spart in suite.groups:
            if suite.is_run_group(spart):
                spart_list.append(spart)
            # End if
        # End for
    else:
        spart_list = [suite.phase_group(stage)]
    # End if
    return spart_list

###############################################################################
def suite_part_call_list(host_model, suite_part, subst_loop_vars):
###############################################################################
    spart_args = suite_part.call_list.variable_list(loop_vars=subst_loop_vars)
    hmvars = list() # Host model to spart dummy args
    for sp_var in spart_args:
        stdname = sp_var.get_prop_value('standard_name')
        hvar = host_model.find_variable(stdname)
        if hvar is None:
            errmsg = 'No host model variable for {} in {}'
            raise CCPPError(errmsg.format(stdname, suite_part.name))
        # End if
        if stdname not in CCPP_CONSTANT_VARS:
            if subst_loop_vars:
                lname = host_model.var_call_string(hvar)
            else:
                lname = host_model.var_call_string(hvar, loop_vars=False)
            # End if
            hmvars.append(lname)
        # End if
    # End for
    return ', '.join(hmvars)

###############################################################################
def write_host_cap(host_model, api, output_dir, logger):
###############################################################################
    module_name = "{}_ccpp_cap".format(host_model.name)
    cap_filename = os.path.join(output_dir, '{}.F90'.format(module_name))
    if logger is not None:
        msg = 'Writing CCPP Host Model Cap for {} to {}'
        logger.info(msg.format(host_model.name, cap_filename))
    # End if
    with FortranWriter(cap_filename, 'w') as cap:
        cap.write(COPYRIGHT, 0)
        cap.write(header.format(host_model=host_model.name,
                                module=module_name), 0)
        cap.write('   use {kinds}'.format(kinds=KINDS_MODULE), 1)

        modules = host_model.variable_locations()
        if modules:
            mlen = max([len(x[0]) for x in modules])
        else:
            mlen = 0
        # End if
        max_suite_len = 0
        for suite in api.suites:
            max_suite_len = max(max_suite_len, len(suite.module))
        # End for
        for mod in modules:
            mspc = (mlen - len(mod[0]))*' '
            cap.write("use {}, {}only: {}".format(mod[0], mspc, mod[1]), 1)
        # End for
        cap.write(preamble.format(host_model=host_model.name), 1)
        # CCPP_STATE_MACH.transitions represents the host CCPP interface
        for stage in CCPP_STATE_MACH.transitions():
            stmt = "public :: {host_model}_ccpp_physics_{stage}"
            cap.write(stmt.format(host_model=host_model.name, stage=stage), 1)
        # End for
        API.declare_inspection_interfaces(cap)
        cap.write('\ncontains\n', 0)
        for stage in CCPP_STATE_MACH.transitions():
            # Create a dict of local variables for stage
            host_local_vars = VarDictionary("{}_{}".format(host_model.name,
                                                           stage))
            # Create part call lists
            # Look for any loop-variable mismatch
            spart_list = suite_part_list(suite, stage)
            for spart in spart_list:
                spart_args = spart.call_list.variable_list()
                hmvars = list() # Host model to spart dummy args
                for sp_var in spart_args:
                    stdname = sp_var.get_prop_value('standard_name')
                    hvar = host_model.find_variable(stdname)
                    if hvar is None:
                        errmsg = 'No host model variable for {} in {}'
                        raise CCPPError(errmsg.format(stdname, spart.name))
                    # End if
                # End for (loop over part variables)
            # End for (loop of suite parts
            run_stage = stage == 'run'
            # All interfaces need the suite name
            apivars = [__suite_name_var__]
            if run_stage:
                # Only the run phase needs a suite part name
                apivars.append(__suite_part_var__)
            # End if
            apinames = [x.get_prop_value('standard_name') for x in apivars]
            # Create a list of dummy arguments with correct intent settings
            callvars = host_model.call_list(stage) # Host interface dummy args
            hdvars = list()
            subst_dict = {}
            for hvar in callvars:
                protected = hvar.get_prop_value('protected')
                if protected:
                    subst_dict['intent'] = 'in'
                else:
                    subst_dict['intent'] = 'inout'
                # End if
                hdvars.append(hvar.clone(subst_dict,
                                         source_name=__api_src_name__))
            # End for
            lnames = [x.get_prop_value('local_name') for x in apivars + hdvars]
            api_vlist = ", ".join(lnames)
            cap.write(subhead.format(api_vars=api_vlist,
                                     host_model=host_model.name,
                                     stage=stage), 1)
            # Write out any suite part use statements
            for suite in api.suites:
                mspc = (max_suite_len - len(suite.module))*' '
                spart_list = suite_part_list(suite, stage)
                for spart in spart_list:
                    stmt = "use {}, {}only: {}"
                    cap.write(stmt.format(suite.module, mspc, spart.name), 2)
                # End for
            # End for
            # Write out any host model DDT input var use statements
            host_model._ddt_lib.write_ddt_use_statements(hdvars, cap, 2,
                                                         pad=max_suite_len)

            cap.write("", 1)
            # Write out dummy arguments
            for var in apivars:
                var.write_def(cap, 2, host_model)
            # End for
            for var in hdvars:
                var.write_def(cap, 2, host_model)
            # End for
            for var in host_local_vars.variable_list():
                var.write_def(cap, 2, host_model)
            # End for
            cap.write('', 0)
            # Write out the body clauses
            errmsg_name, errflg_name = api.get_errinfo_names()
            # Initialize err variables
            cap.write('{errflg} = 0'.format(errflg=errflg_name), 2)
            cap.write('{errmsg} = ""'.format(errmsg=errmsg_name), 2)
            else_str = ''
            for suite in api.suites:
                stmt = "{}if (trim(suite_name) == '{}') then"
                cap.write(stmt.format(else_str, suite.name), 2)
                if stage == 'run':
                    el2_str = ''
                    spart_list = suite_part_list(suite, stage)
                    for spart in spart_list:
                        pname = spart.name[len(suite.name)+1:]
                        stmt = "{}if (trim(suite_part) == '{}') then"
                        cap.write(stmt.format(el2_str, pname), 3)
                        call_str = suite_part_call_list(host_model, spart, True)
                        cap.write("call {}({})".format(spart.name, call_str), 4)
                        el2_str = 'else '
                    # End for
                    cap.write("else", 3)
                    emsg = "write({errmsg}, '(3a)')".format(errmsg=errmsg_name)
                    emsg += '"No suite part named ", '
                    emsg += 'trim(suite_part), '
                    emsg += '" found in suite {sname}"'.format(sname=suite.name)
                    cap.write(emsg, 4)
                    cap.write("{errflg} = 1".format(errflg=errflg_name), 4)
                    cap.write("end if", 3)
                else:
                    spart = suite.phase_group(stage)
                    call_str = suite_part_call_list(host_model, spart, False)
                    stmt = "call {}_{}({})"
                    cap.write(stmt.format(suite.name, stage, call_str), 3)
                # End if
                else_str = 'else '
            # End for
            cap.write("else", 2)
            emsg = "write({errmsg}, '(3a)')".format(errmsg=errmsg_name)
            emsg += '"No suite named ", '
            emsg += 'trim(suite_name), "found"'
            cap.write(emsg, 3)
            cap.write("{errflg} = 1".format(errflg=errflg_name), 3)
            cap.write("end if", 2)
            cap.write(subfoot.format(host_model=host_model.name,
                                     stage=stage), 1)
        # End for
        api.write_inspection_routines(cap)
        cap.write(footer.format(module=module_name), 0)
    # End with
    return cap_filename

###############################################################################

if __name__ == "__main__":
    from parse_tools import init_log, set_log_to_null
    logger = init_log('host_registry')
    set_log_to_null(logger)
    # Run doctest
    import doctest
    doctest.testmod()
# No else:
