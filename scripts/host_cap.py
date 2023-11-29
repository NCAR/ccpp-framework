#!/usr/bin/env python3

"""
Parse a host-model registry XML file and return the captured variables.
"""

# Python library imports
import logging
import os
# CCPP framework imports
from ccpp_suite import API, API_SOURCE_NAME
from ccpp_state_machine import CCPP_STATE_MACH
from constituents import ConstituentVarDict, CONST_DDT_NAME, CONST_DDT_MOD
from constituents import CONST_OBJ_STDNAME
from ddt_library import DDTLibrary
from file_utils import KINDS_MODULE
from framework_env import CCPPFrameworkEnv
from metadata_table import MetadataTable
from metavar import Var, VarDictionary, CCPP_CONSTANT_VARS
from metavar import CCPP_LOOP_VAR_STDNAMES
from fortran_tools import FortranWriter
from parse_tools import CCPPError
from parse_tools import ParseObject, ParseSource, ParseContext

###############################################################################
_HEADER = "cap for {host_model} calls to CCPP API"

_SUBHEAD = '''
   subroutine {host_model}_ccpp_physics_{stage}({api_vars})
'''

_SUBFOOT = '''
   end subroutine {host_model}_ccpp_physics_{stage}
'''

_API_SOURCE = ParseSource(API_SOURCE_NAME, "MODULE",
                          ParseContext(filename="host_cap.F90"))

_API_DUMMY_RUN_ENV = CCPPFrameworkEnv(None, ndict={'host_files':'',
                                                   'scheme_files':'',
                                                   'suites':''})

_SUITE_NAME_VAR = Var({'local_name':'suite_name',
                       'standard_name':'suite_name',
                       'intent':'in', 'type':'character',
                       'kind':'len=*', 'units':'', 'protected':'True',
                       'dimensions':'()'}, _API_SOURCE, _API_DUMMY_RUN_ENV)

_SUITE_PART_VAR = Var({'local_name':'suite_part',
                       'standard_name':'suite_part',
                       'intent':'in', 'type':'character',
                       'kind':'len=*', 'units':'', 'protected':'True',
                       'dimensions':'()'}, _API_SOURCE, _API_DUMMY_RUN_ENV)

###############################################################################
# Used for creating blank dictionary
_MVAR_DUMMY_RUN_ENV = CCPPFrameworkEnv(None, ndict={'host_files':'',
                                                    'scheme_files':'',
                                                    'suites':''})

# Used to prevent loop substitution lookups
_BLANK_DICT = VarDictionary(API_SOURCE_NAME, _MVAR_DUMMY_RUN_ENV)

###############################################################################
def suite_part_list(suite, stage):
###############################################################################
    """Return a list of all the suite parts for this stage"""
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
def constituent_num_suite_subname(host_model):
###############################################################################
    """Return the name of the number of suite constituents for this run
    Because this is a user interface API function, the name is fixed."""
    return f"{host_model.name}_ccpp_num_suite_constituents"

###############################################################################
def constituent_register_subname(host_model):
###############################################################################
    """Return the name of the subroutine used to register the constituent
    properties for this run.
    Because this is a user interface API function, the name is fixed."""
    return f"{host_model.name}_ccpp_register_constituents"

###############################################################################
def constituent_initialize_subname(host_model):
###############################################################################
    """Return the name of the subroutine used to initialize the
    constituents for this run.
    Because this is a user interface API function, the name is fixed."""
    return f"{host_model.name}_ccpp_initialize_constituents"

###############################################################################
def constituent_num_consts_funcname(host_model):
###############################################################################
    """Return the name of the function to return the number of
    constituents for this run.
    Because this is a user interface API function, the name is fixed."""
    return f"{host_model.name}_ccpp_number_constituents"

###############################################################################
def query_scheme_constituents_funcname(host_model):
###############################################################################
    """Return the name of the function to return True if the standard name
    passed in matches an existing constituent
    Because this is a user interface API function, the name is fixed."""
    return f"{host_model.name}_ccpp_is_scheme_constituent"

###############################################################################
def constituent_copyin_subname(host_model):
###############################################################################
    """Return the name of the subroutine to copy constituent fields to the
    host model.
    Because this is a user interface API function, the name is fixed."""
    return f"{host_model.name}_ccpp_gather_constituents"

###############################################################################
def constituent_copyout_subname(host_model):
###############################################################################
    """Return the name of the subroutine to update constituent fields from
    the host model.
    Because this is a user interface API function, the name is fixed."""
    return f"{host_model.name}_ccpp_update_constituents"

###############################################################################
def unique_local_name(loc_name, host_model):
###############################################################################
    """Create a unique local name based on the local_name property,
    <loc_name> for a variable with standard name, <std_name>.
    If <local_name> is an unique local name (not in <host_model>),
    simply return that. If not, create one and return that."""
    new_name = host_model.find_local_name(loc_name) is not None
    if new_name:
        new_lname = host_model.new_internal_variable_name(prefix=loc_name)
    else:
        new_lname = loc_name
    # end if
    return new_lname

###############################################################################
def constituent_model_object_name(host_model):
###############################################################################
    """Return the variable name of the object which holds the constituent
       metadata and field information."""
    hvar = host_model.find_variable(CONST_OBJ_STDNAME)
    if not hvar:
        raise CCPPError(f"Host model does not contain Var, {CONST_OBJ_STDNAME}")
    # end if
    return hvar.get_prop_value('local_name')

###############################################################################
def constituent_model_const_stdnames(host_model):
###############################################################################
    """Return the name of the array of constituent standard names"""
    hstr = f"{host_model.name}_model_const_stdnames"
    return unique_local_name(hstr, host_model)

###############################################################################
def constituent_model_const_indices(host_model):
###############################################################################
    """Return the name of the array of constituent field array indices"""
    hstr = f"{host_model.name}_model_const_indices"
    return unique_local_name(hstr, host_model)

###############################################################################
def constituent_model_consts(host_model):
###############################################################################
    """Return the name of the function that will return a pointer to the
       array of all constituents"""
    hstr = f"{host_model.name}_constituents_array"
    return unique_local_name(hstr, host_model)

###############################################################################
def constituent_model_advected_consts(host_model):
###############################################################################
    """Return the name of the function that will return a pointer to the
       array of advected constituents"""
    hstr = f"{host_model.name}_advected_constituents_array"
    return unique_local_name(hstr, host_model)

###############################################################################
def constituent_model_const_props(host_model):
###############################################################################
    """Return the name of the array of constituent property object pointers"""
    hstr = f"{host_model.name}_model_const_properties"
    return unique_local_name(hstr, host_model)

###############################################################################
def constituent_model_const_index(host_model):
###############################################################################
    """Return the name of the interface that returns the array index of
       a constituent array given its standard name"""
    hstr = f"{host_model.name}_const_get_index"
    return unique_local_name(hstr, host_model)

###############################################################################
def add_constituent_vars(cap, host_model, suite_list, run_env):
###############################################################################
    """Create a DDT library containing array reference variables
    for each constituent field for all suites in <suite_list>.
    Create and return a dictionary containing an index variable for each of the
    constituents as well as the variables from the DDT object.
    Also, write declarations for these variables to <cap>.
    Since the constituents are in a DDT (ccpp_constituent_properties_t),
    create a metadata table with the required information, then parse it
    to create the dictionary.
    """
    # First create a MetadataTable for the constituents DDT
    stdname_layer = "number_of_ccpp_constituents"
    horiz_dim = "horizontal_dimension"
    vert_layer_dim = "vertical_layer_dimension"
    vert_interface_dim = "vertical_interface_dimension"
    array_layer = "vars_layer"
    # Table preamble (leave off ccpp-table-properties header)
    ddt_mdata = [
        #"[ccpp-table-properties]",
        f" name = {CONST_DDT_NAME}", " type = ddt",
        "[ccpp-arg-table]",
        f" name = {CONST_DDT_NAME}", " type = ddt",
        "[ num_layer_vars ]",
        f" standard_name = {stdname_layer}",
        " units = count", " dimensions = ()", " type = integer",
        f"[ {array_layer} ]",
        " standard_name = ccpp_constituents",
        " units = none",
        f" dimensions = ({horiz_dim}, {vert_layer_dim}, {stdname_layer})",
        " type = real", " kind = kind_phys"]
    # Add entries for each constituent (once per standard name)
    const_stdnames = set()
    for suite in suite_list:
        if run_env.verbose:
            lmsg = "Adding constituents from {} to {}"
            run_env.logger.debug(lmsg.format(suite.name, host_model.name))
        # end if
        scdict = suite.constituent_dictionary()
        for cvar in scdict.variable_list():
            std_name = cvar.get_prop_value('standard_name')
            if std_name not in const_stdnames:
                # Add a metadata entry for this constituent
                # Check dimensions and figure vertical dimension
                # Currently, we only support variables with first dimension,
                #   horizontal_dimension, and second (optional) dimension,
                #   vertical_layer_dimension or vertical_interface_dimension
                dims = cvar.get_dimensions()
                if (len(dims) < 1) or (len(dims) > 2):
                    emsg = "Unsupported constituent dimensions, '{}'"
                    dimstr = "({})".format(", ".join(dims))
                    raise CCPPError(emsg.format(dimstr))
                # end if
                hdim = dims[0].split(':')[-1]
                if hdim != 'horizontal_dimension':
                    emsg = "Unsupported first constituent dimension, '{}', "
                    emsg += "must be 'horizontal_dimension'"
                    raise CCPPError(emsg.format(hdim))
                # end if
                if len(dims) > 1:
                    vdim = dims[1].split(':')[-1]
                    if vdim == vert_layer_dim:
                        cvar_array_name = array_layer
                    else:
                        emsg = "Unsupported vertical constituent dimension, "
                        emsg += "'{}', must be '{}' or '{}'"
                        raise CCPPError(emsg.format(vdim, vert_layer_dim,
                                                    vert_interface_dim))
                    # end if
                else:
                    emsg = f"Unsupported 2-D variable, '{std_name}'"
                    raise CCPPError(emsg)
                # end if
                # First, create an index variable for <cvar>
                ind_std_name = "index_of_{}".format(std_name)
                loc_name = f"{cvar_array_name}(:,:,{ind_std_name})"
                ddt_mdata.append(f"[ {loc_name} ]")
                ddt_mdata.append(f" standard_name = {std_name}")
                units = cvar.get_prop_value('units')
                ddt_mdata.append(f" units = {units}")
                dimstr = f"({', '.join(dims)})"
                ddt_mdata.append(f" dimensions = {dimstr}")
                vtype = cvar.get_prop_value('type')
                vkind = cvar.get_prop_value('kind')
                ddt_mdata.append(f" type = {vtype} | kind = {vkind}")
                const_stdnames.add(std_name)
            # end if
        # end for
    # end for
    # Parse this table using a fake filename
    parse_obj = ParseObject(f"{host_model.name}_constituent_mod.meta",
                            ddt_mdata)
    ddt_table = MetadataTable(run_env, parse_object=parse_obj)
    ddt_lib = DDTLibrary(f"{host_model.name}_constituent_ddtlib",
                         run_env, ddts=ddt_table.sections())
    # A bit of cleanup
    del parse_obj
    del ddt_mdata
    # Now, create the "host constituent module" dictionary
    const_dict = VarDictionary(f"{host_model.name}_constituents",
                               run_env, parent_dict=host_model)
    # Add the constituents object to const_dict and write its declaration
    const_var = host_model.find_variable(CONST_OBJ_STDNAME)
    if const_var:
        const_dict.add_variable(const_var, run_env)
        const_var.write_def(cap, 1, const_dict)
    else:
        raise CCPPError(f"Missing Var, {CONST_OBJ_STDNAME}, in host model")
    # end if
    ddt_lib.collect_ddt_fields(const_dict, const_var, run_env,
                               skip_duplicates=True)
    # Declare variable for the constituent standard names array
    max_csname = max([len(x) for x in const_stdnames]) if const_stdnames else 0
    num_const_fields = len(const_stdnames)
    cs_stdname = constituent_model_const_stdnames(host_model)
    const_list = sorted(const_stdnames)
    if const_list:
        const_strs = ['"{}{}"'.format(x, ' '*(max_csname - len(x)))
                      for x in const_list]
        cs_stdame_initstr = " = (/ " + ", ".join(const_strs) + " /)"
    else:
        cs_stdame_initstr = ""
    # end if
    cap.write("character(len={}) :: {}({}){}".format(max_csname, cs_stdname,
                                                     num_const_fields,
                                                     cs_stdame_initstr), 1)
    # Declare variable for the constituent standard names array
    array_name = constituent_model_const_indices(host_model)
    cap.write("integer :: {}({}) = -1".format(array_name, num_const_fields), 1)
    # Add individual variables for each index var to the const_dict
    for index, std_name in enumerate(const_list):
        ind_std_name = "index_of_{}".format(std_name)
        ind_loc_name = "{}({})".format(array_name, index + 1)
        prop_dict = {'standard_name' : ind_std_name,
                     'local_name' : ind_loc_name, 'dimensions' : '()',
                     'units' : 'index', 'protected' : "True",
                     'type' : 'integer', 'kind' : ''}
        ind_var = Var(prop_dict, _API_SOURCE, run_env)
        const_dict.add_variable(ind_var, run_env)
    # end for
    # Add vertical dimensions for DDT call strings
    pver = host_model.find_variable(standard_name=vert_layer_dim,
                                    any_scope=False)
    if pver is not None:
        prop_dict = {'standard_name' : vert_layer_dim,
                     'local_name' : pver.get_prop_value('local_name'),
                     'units' : 'count', 'type' : 'integer',
                     'protected' : 'True', 'dimensions' : '()'}
        if const_dict.find_variable(standard_name=vert_layer_dim,
                                    any_scope=False) is None:
            ind_var = Var(prop_dict, _API_SOURCE, _API_DUMMY_RUN_ENV)
            const_dict.add_variable(ind_var, run_env)
        # end if
    # end if
    pver = host_model.find_variable(standard_name=vert_interface_dim,
                                    any_scope=False)
    if pver is not None:
        prop_dict = {'standard_name' : vert_interface_dim,
                     'local_name' : pver.get_prop_value('local_name'),
                     'units' : 'count', 'type' : 'integer',
                     'protected' : 'True', 'dimensions' : '()'}
        if const_dict.find_variable(standard_name=vert_interface_dim,
                                    any_scope=False) is None:
            ind_var = Var(prop_dict, _API_SOURCE, run_env)
            const_dict.add_variable(ind_var, run_env)
        # end if
    # end if

    return const_dict

###############################################################################
def suite_part_call_list(host_model, const_dict, suite_part, subst_loop_vars):
###############################################################################
    """Return the <host_model> controlled call list for <suite_part>.
    <const_dict> is the constituent dictionary"""
    spart_args = suite_part.call_list.variable_list(loop_vars=subst_loop_vars)
    hmvars = list() # Host model to spart dummy args
    if subst_loop_vars:
        loop_vars = host_model.loop_vars
    else:
        loop_vars = None
    # end if
    for sp_var in spart_args:
        stdname = sp_var.get_prop_value('standard_name')
        sp_lname = sp_var.get_prop_value('local_name')
        var_dicts = [host_model, const_dict]
        # Figure out which dictionary has the variable
        for vdict in var_dicts:
            hvar = vdict.find_variable(standard_name=stdname, any_scope=False)
            if hvar is not None:
                var_dict = vdict
                break
            # end if
        # end for
        if hvar is None:
            errmsg = 'No host model variable for {} in {}'
            raise CCPPError(errmsg.format(stdname, suite_part.name))
        # End if
        if stdname not in CCPP_CONSTANT_VARS:
            lname = var_dict.var_call_string(hvar, loop_vars=loop_vars)
            hmvars.append("{}={}".format(sp_lname, lname))
        # End if
    # End for
    return ', '.join(hmvars)

###############################################################################
def write_host_cap(host_model, api, module_name, output_dir, run_env):
###############################################################################
    """Write an API to allow <host_model> to call any configured CCPP suite"""
    cap_filename = os.path.join(output_dir, '{}.F90'.format(module_name))
    if run_env.logger is not None:
        msg = 'Writing CCPP Host Model Cap for {} to {}'
        run_env.logger.info(msg.format(host_model.name, cap_filename))
    # End if
    header = _HEADER.format(host_model=host_model.name)
    with FortranWriter(cap_filename, 'w', header, module_name) as cap:
        # Write module use statements
        maxmod = len(KINDS_MODULE)
        cap.write('   use {kinds}'.format(kinds=KINDS_MODULE), 1)
        modules = host_model.variable_locations()
        if modules:
            mlen = max([len(x[0]) for x in modules])
            maxmod = max(maxmod, mlen)
        # End if
        mlen = max([len(x.module) for x in api.suites])
        maxmod = max(maxmod, mlen)
        maxmod = max(maxmod, len(CONST_DDT_MOD))
        for mod in modules:
            mspc = (maxmod - len(mod[0]))*' '
            cap.write("use {}, {}only: {}".format(mod[0], mspc, mod[1]), 1)
        # End for
        mspc = ' '*(maxmod - len(CONST_DDT_MOD))
        cap.write(f"use {CONST_DDT_MOD}, {mspc}only: {CONST_DDT_NAME}", 1)
        cap.write_preamble()
        max_suite_len = 0
        for suite in api.suites:
            max_suite_len = max(max_suite_len, len(suite.module))
        # End for
        cap.comment("Public Interfaces", 1)
        # CCPP_STATE_MACH.transitions represents the host CCPP interface
        for stage in CCPP_STATE_MACH.transitions():
            stmt = "public :: {host_model}_ccpp_physics_{stage}"
            cap.write(stmt.format(host_model=host_model.name, stage=stage), 1)
        # End for
        API.declare_inspection_interfaces(cap)
        # Write the host-model interfaces for constituents
        reg_name = constituent_register_subname(host_model)
        cap.write(f"public :: {reg_name}", 1)
        init_name = constituent_initialize_subname(host_model)
        cap.write(f"public :: {init_name}", 1)
        numconsts_name = constituent_num_consts_funcname(host_model)
        cap.write(f"public :: {numconsts_name}", 1)
        queryconsts_name = query_scheme_constituents_funcname(host_model)
        cap.write(f"public :: {queryconsts_name}", 1)
        copyin_name = constituent_copyin_subname(host_model)
        cap.write(f"public :: {copyin_name}", 1)
        copyout_name = constituent_copyout_subname(host_model)
        cap.write(f"public :: {copyout_name}", 1)
        const_array_func = constituent_model_consts(host_model)
        cap.write(f"public :: {const_array_func}", 1)
        advect_array_func = constituent_model_advected_consts(host_model)
        cap.write(f"public :: {advect_array_func}", 1)
        prop_array_func = constituent_model_const_props(host_model)
        cap.write(f"public :: {prop_array_func}", 1)
        const_index_func = constituent_model_const_index(host_model)
        cap.write(f"public :: {const_index_func}", 1)
        cap.write("", 0)
        cap.write("! Private module variables", 1)
        const_dict = add_constituent_vars(cap, host_model, api.suites, run_env)
        cap.end_module_header()
        for stage in CCPP_STATE_MACH.transitions():
            # Create a dict of local variables for stage
            host_local_vars = VarDictionary(f"{host_model.name}_{stage}",
                                            run_env)
            # Create part call lists
            # Look for any loop-variable mismatch
            for suite in api.suites:
                spart_list = suite_part_list(suite, stage)
                for spart in spart_list:
                    spart_args = spart.call_list.variable_list()
                    for sp_var in spart_args:
                        stdname = sp_var.get_prop_value('standard_name')
                        hvar = const_dict.find_variable(standard_name=stdname,
                                                        any_scope=True)
                        if hvar is None:
                            errmsg = 'No host model variable for {} in {}'
                            raise CCPPError(errmsg.format(stdname, spart.name))
                        # End if
                    # End for (loop over part variables)
                # End for (loop of suite parts)
            # End for (loop over suites)
            run_stage = stage == 'run'
            # All interfaces need the suite name
            apivars = [_SUITE_NAME_VAR]
            if run_stage:
                # Only the run phase needs a suite part name
                apivars.append(_SUITE_PART_VAR)
            # End if
            # Create a list of dummy arguments with correct intent settings
            callvars = host_model.call_list(stage) # Host interface dummy args
            hdvars = list()
            subst_dict = {}
            for hvar in callvars:
                protected = hvar.get_prop_value('protected')
                stdname = hvar.get_prop_value('standard_name')
                if stdname in CCPP_LOOP_VAR_STDNAMES:
                    protected = True # Cannot modify a loop variable
                # End if
                if protected:
                    subst_dict['intent'] = 'in'
                else:
                    subst_dict['intent'] = 'inout'
                # End if
                hdvars.append(hvar.clone(subst_dict,
                                         source_name=API_SOURCE_NAME))
            # End for
            lnames = [x.get_prop_value('local_name') for x in apivars + hdvars]
            api_vlist = ", ".join(lnames)
            cap.write(_SUBHEAD.format(api_vars=api_vlist,
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
            host_model.ddt_lib.write_ddt_use_statements(hdvars, cap, 2,
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
                        call_str = suite_part_call_list(host_model, const_dict,
                                                        spart, True)
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
                    call_str = suite_part_call_list(host_model, const_dict,
                                                    spart, False)
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
            cap.write(_SUBFOOT.format(host_model=host_model.name,
                                      stage=stage), 1)
        # End for
        # Write the API inspection routines (e.g., list of suites)
        api.write_inspection_routines(cap)
        # Write the constituent initialization interfaces
        err_vars = host_model.find_error_variables()
        const_obj_name = constituent_model_object_name(host_model)
        cap.write("", 0)
        const_names_name = constituent_model_const_stdnames(host_model)
        const_indices_name = constituent_model_const_indices(host_model)
        ConstituentVarDict.write_host_routines(cap, host_model, reg_name, init_name,
                                               numconsts_name, queryconsts_name,
                                               copyin_name, copyout_name, 
                                               const_obj_name,
                                               const_names_name,
                                               const_indices_name,
                                               const_array_func,
                                               advect_array_func,
                                               prop_array_func,
                                               const_index_func,
                                               api.suites, err_vars)
    # End with
    return cap_filename

###############################################################################

if __name__ == "__main__":
    from parse_tools import init_log, set_log_to_null
    _LOGGER = init_log('host_registry')
    set_log_to_null(_LOGGER)
    # Run doctest
    # pylint: disable=ungrouped-imports
    import doctest
    import sys
    # pylint: enable=ungrouped-imports
    fail, _ = doctest.testmod()
    sys.exit(fail)
# end if
