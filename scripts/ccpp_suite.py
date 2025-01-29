#!/usr/bin/env python3
#

"""Classes and methods to create a Fortran suite-implementation file
to implement calls to a set of suites for a given host model."""

# Python library imports
import os.path
import logging
import xml.etree.ElementTree as ET
# CCPP framework imports
from ccpp_state_machine import CCPP_STATE_MACH, RUN_PHASE_NAME
from code_block import CodeBlock
from constituents import ConstituentVarDict
from ddt_library import DDTLibrary
from file_utils import KINDS_MODULE
from fortran_tools import FortranWriter
from framework_env import CCPPFrameworkEnv
from metavar import Var, VarDictionary, ccpp_standard_var
from parse_tools import ParseContext, ParseSource
from parse_tools import ParseInternalError, CCPPError
from parse_tools import read_xml_file, validate_xml_file, find_schema_version
from parse_tools import init_log, set_log_to_null
from suite_objects import CallList, Group, Scheme
from metavar import CCPP_LOOP_VAR_STDNAMES
from var_props import is_horizontal_dimension

# pylint: disable=too-many-lines

###############################################################################
# Module (global) variables
###############################################################################

# Source for internally generated variables.
API_SOURCE_NAME = "CCPP_API"
# Use the constituent source type for consistency
_API_SUITE_VAR_NAME = ConstituentVarDict.constitutent_source_type()
_API_SCHEME_VAR_NAME = "scheme"
_API_CONTEXT = ParseContext(filename="ccpp_suite.py")
_API_SOURCE = ParseSource(API_SOURCE_NAME, _API_SCHEME_VAR_NAME, _API_CONTEXT)
_API_LOGGING = init_log('ccpp_suite')
set_log_to_null(_API_LOGGING)
_API_DUMMY_RUN_ENV = CCPPFrameworkEnv(_API_LOGGING,
                                      ndict={'host_files':'',
                                             'scheme_files':'',
                                             'suites':''})

# Required variables for inclusion in auto-generated schemes
CCPP_REQUIRED_VARS = [ccpp_standard_var('ccpp_error_code',
                                        _API_SCHEME_VAR_NAME,
                                        _API_DUMMY_RUN_ENV,
                                        context=_API_CONTEXT),
                      ccpp_standard_var('ccpp_error_message',
                                        _API_SCHEME_VAR_NAME,
                                        _API_DUMMY_RUN_ENV,
                                        context=_API_CONTEXT)]

###############################################################################

class Suite(VarDictionary):
    """Class to hold, process, and output a CAP for an entire CCPP suite.
    The Suite includes initialization and finalization Group objects as
    well as a Group for every suite part."""

    __state_machine_initial_state = 'uninitialized'
    __state_machine_var_name = 'ccpp_suite_state'

    __state_machine_init = '''
character(len=16) :: {css_var_name} = '{state}'
'''

    # Note that these group names need to match CCPP_STATE_MACH
    __register_group_name = 'register'

    __initial_group_name = 'initialize'

    __final_group_name = 'finalize'

    __timestep_initial_group_name = 'timestep_initial'

    __timestep_final_group_name = 'timestep_final'

    __scheme_template = '<scheme>{}</scheme>'

    def __init__(self, filename, api, run_env):
        """Initialize this Suite object from the SDF, <filename>.
        <api> serves as the Suite's parent."""
        self.__run_env = run_env
        self.__name = None
        self.__sdf_name = filename
        self.__groups = list()
        self.__suite_init_group = None
        self.__suite_final_group = None
        self.__timestep_init_group = None
        self.__timestep_final_group = None
        self.__context = None
        self.__host_arg_list_full = None
        self.__host_arg_list_noloop = None
        self.__module = None
        self.__ddt_library = None
        # Full phases/groups are special groups where the entire state is passed
        self.__full_groups = {}
        self._full_phases = {}
        self.__gvar_stdnames = {} # Standard names of group-created vars
        # Initialize our dictionary
        # Create a 'parent' to hold the constituent variables
        # The parent for the constituent dictionary is the API.
        temp_name = os.path.splitext(os.path.basename(filename))[0]
        const_dict = ConstituentVarDict(temp_name+'_constituents',
                                        api, run_env)
        super().__init__(self.sdf_name, run_env, parent_dict=const_dict)
        if not os.path.exists(self.__sdf_name):
            emsg = "Suite definition file {0} not found."
            raise CCPPError(emsg.format(self.__sdf_name))
        # end if
        # Parse the SDF
        self.parse(run_env)

    @property
    def name(self):
        """Get the name of the suite."""
        return self.__name

    @property
    def sdf_name(self):
        """Get the name of the suite definition file."""
        return self.__sdf_name

    @classmethod
    def check_suite_state(cls, stage):
        """Return a list of CCPP state check statements for <stage>"""
        check_stmts = list()
        if stage in CCPP_STATE_MACH.transitions():
            # We need to make sure we are an allowed previous state
            prev_state = CCPP_STATE_MACH.initial_state(stage)
            css = "trim({})".format(Suite.__state_machine_var_name)
            prev_str = "({} /= '{}')".format(css, prev_state)
            check_stmts.append(("if {} then".format(prev_str), 1))
            check_stmts.append(("{errcode} = 1", 2))
            errmsg_str = "write({errmsg}, '(3a)') "
            errmsg_str += "\"Invalid initial CCPP state, '\", " + css + ', '
            errmsg_str += "\"' in {funcname}\""
            check_stmts.append((errmsg_str, 2))
            check_stmts.append(("return", 2))
            check_stmts.append(("end if", 1))
        else:
            raise ParseInternalError("Unknown stage, '{}'".format(stage))
        # end if
        return CodeBlock(check_stmts)

    @classmethod
    def set_suite_state(cls, phase):
        """Return the code string to set the current suite state to <phase>.
        If the initial and final states of <phase> are identical, return blank.
        """
        initial = CCPP_STATE_MACH.initial_state(phase)
        final = CCPP_STATE_MACH.final_state(phase)
        if initial == final:
            stmt = '! Suite state does not change'
        else:
            stmt = "ccpp_suite_state = '{}'".format(final)
        # end if
        return CodeBlock([(stmt, 1)])

    def new_group(self, group_string, transition, run_env):
        """Create a new Group object from the a XML description"""
        if isinstance(group_string, str):
            gxml = ET.fromstring(group_string)
        else:
            gxml = group_string
        # end if
        group = Group(gxml, transition, self, self.__context, run_env)
        for svar in CCPP_REQUIRED_VARS:
            group.add_call_list_variable(svar)
        # end for
        if transition != RUN_PHASE_NAME:
            self.__full_groups[group.name] = group
            self._full_phases[group.phase()] = group
        # end if
        return group

    def new_group_from_name(self, group_name, run_env):
        '''Create an XML string for Group, <group_name>, and use it to
        create the corresponding group.
        Note: <group_name> must be the a transition string'''
        group_xml = '<group name="{}"></group>'.format(group_name)
        return self.new_group(group_xml, group_name, run_env)

    def parse(self, run_env):
        """Parse the suite definition file."""
        success = True

        _, suite_xml = read_xml_file(self.__sdf_name, run_env.logger)
        # We do not have line number information for the XML file
        self.__context = ParseContext(filename=self.__sdf_name)
        # Validate the XML file
        version = find_schema_version(suite_xml)
        res = validate_xml_file(self.__sdf_name, 'suite', version,
                                run_env.logger)
        if not res:
            emsg = "Invalid suite definition file, '{}'"
            raise CCPPError(emsg.format(self.__sdf_name))
        # end if
        self.__name = suite_xml.get('name')
        self.__module = 'ccpp_{}_cap'.format(self.name)
        lmsg = "Reading suite definition file for '{}'"
        if run_env.logger and run_env.logger.isEnabledFor(logging.INFO):
            run_env.logger.info(lmsg.format(self.name))
        # end if
        gname = Suite.__register_group_name
        self.__suite_reg_group = self.new_group_from_name(gname, run_env)
        gname = Suite.__initial_group_name
        self.__suite_init_group = self.new_group_from_name(gname, run_env)
        gname = Suite.__final_group_name
        self.__suite_final_group = self.new_group_from_name(gname, run_env)
        gname = Suite.__timestep_initial_group_name
        self.__timestep_init_group = self.new_group_from_name(gname, run_env)
        gname = Suite.__timestep_final_group_name
        self.__timestep_final_group = self.new_group_from_name(gname, run_env)
        # Set up some groupings for later efficiency
        self._beg_groups = [self.__suite_reg_group.name,
                            self.__suite_init_group.name,
                            self.__timestep_init_group.name]
        self._end_groups = [self.__suite_final_group.name,
                            self.__timestep_final_group.name]
        # Build hierarchical structure as in SDF
        self.__groups.append(self.__suite_reg_group)
        self.__groups.append(self.__suite_init_group)
        self.__groups.append(self.__timestep_init_group)
        for suite_item in suite_xml:
            item_type = suite_item.tag.lower()
            # Suite item is a group or a suite-wide init or final method
            if item_type == 'group':
                # Parse a group
                self.__groups.append(self.new_group(suite_item, RUN_PHASE_NAME,
                                                    run_env))
            else:
                match_trans = CCPP_STATE_MACH.function_match(item_type)
                if match_trans is None:
                    emsg = "Unknown CCPP suite component tag type, '{}'"
                    raise CCPPError(emsg.format(item_type))
                # end if
                if match_trans in self._full_phases:
                    # Parse a suite-wide initialization scheme
                    scheme = Scheme(suite_item, self.__context,
                                    self, run_env)
                    self._full_phases[match_trans].add_item(scheme)
                else:
                    emsg = "Unhandled CCPP suite component tag type, '{}'"
                    raise ParseInternalError(emsg.format(match_trans))
                # end if
        # end for
        self.__groups.append(self.__timestep_final_group)
        self.__groups.append(self.__suite_final_group)
        return success

    def suite_dicts(self):
        """Return a list of this Suite's dictionaries.
        A Suite's dictionaries are itself plus its constituent dictionary"""
        return [self, self.parent]

    @property
    def module(self):
        """Get the list of the module generated for this suite."""
        return self.__module

    @property
    def groups(self):
        """Get the list of groups in this suite."""
        return self.__groups

    def find_variable(self, standard_name=None, source_var=None,
                      any_scope=True, clone=None,
                      search_call_list=False, loop_subst=False):
        """Attempt to return the variable matching <standard_name>.
        if <standard_name> is None, the standard name from <source_var> is used.
        It is an error to pass both <standard_name> and <source_var> if
        the standard name of <source_var> is not the same as <standard_name>.
        If <any_scope> is True, search parent scopes if not in current scope.
        If the variable is not found this Suite's groups are searched for
        a matching output variable. If found that variable is promoted to be a
        Suite module variable and that variable is returned.
        If the variable is not found and <clone> is not None, add a clone of
        <clone> to this dictionary.
        If the variable is not found and <clone> is None, return None.
        """
        # First, see if the variable is already in our path
        srch_clist = search_call_list
        var = super().find_variable(standard_name=standard_name,
                                    source_var=source_var,
                                    any_scope=any_scope,
                                    clone=None,
                                    search_call_list=srch_clist,
                                    loop_subst=loop_subst)
        if var is None:
            # No dice? Check for a group variable which can be promoted
            # Don't promote loop standard names
            if (standard_name in self.__gvar_stdnames and standard_name
                not in CCPP_LOOP_VAR_STDNAMES):
                group = self.__gvar_stdnames[standard_name]
                var = group.find_variable(standard_name=standard_name,
                                          source_var=source_var,
                                          any_scope=False,
                                          search_call_list=srch_clist,
                                          loop_subst=loop_subst)

                if var is not None:
                    # Promote variable to suite level
                    # Remove this entry to avoid looping back here
                    del self.__gvar_stdnames[standard_name]
                    # Let everyone know this is now a Suite variable
                    var.source = ParseSource(API_SOURCE_NAME,
                                             _API_SUITE_VAR_NAME,
                                             var.context)
                    self.add_variable(var, self.__run_env)
                    # Remove the variable from the group
                    group.remove_variable(standard_name)
                    # Make sure the variable's dimensions are available
                    # at the init stage (for allocation)
                    for group in self.groups:
                        # only add dimension variables to init phase calling list
                        if group.name == self.__suite_init_group.name:
                            dims = var.get_dimensions()
                            # replace horizontal loop dimension if necessary
                            for idx, dim in enumerate(dims):
                                if is_horizontal_dimension(dim):
                                    if 'horizontal_loop' in dim:
                                        dims[idx] = 'ccpp_constant_one:horizontal_dimension'
                                    # end if
                                # end if
                            # end for
                            subst_dict = {'dimensions': dims}
                            prop_dict = var.copy_prop_dict(subst_dict=subst_dict)
                            temp_var = Var(prop_dict,
                                           ParseSource(var.get_prop_value('scheme'),
                                           var.get_prop_value('local_name'), var.context),
                                           self.__run_env)
                            # Add dimensions if they're not already there
                            group.add_variable_dimensions(temp_var, [],
                                                          adjust_intent=True,
                                                          to_dict=group.call_list)
                        # end if
                    # end for
                else:
                    emsg = ("Group, {}, claimed it had created {} "
                            "but variable was not found")
                    raise CCPPError(emsg.format(group.name, standard_name))
                # end if
            # end if
        # end if
        if (var is None) and (clone is not None):
            # Guess it is time to clone a different variable
            var = super().find_variable(standard_name=standard_name,
                                        source_var=source_var,
                                        any_scope=any_scope, clone=clone)
        # end if
        return var

    def analyze(self, host_model, scheme_library, ddt_library, run_env):
        """Collect all information needed to write a suite file
        >>> CCPP_STATE_MACH.transition_match('init')
        'initialize'
        >>> CCPP_STATE_MACH.transition_match('init', transition='finalize')

        >>> CCPP_STATE_MACH.transition_match('INIT')
        'initialize'
        >>> CCPP_STATE_MACH.transition_match('initial')
        'initialize'
        >>> CCPP_STATE_MACH.transition_match('timestep_initial')
        'timestep_initial'
        >>> CCPP_STATE_MACH.transition_match('timestep_initialize')
        'timestep_initial'
        >>> CCPP_STATE_MACH.transition_match('timestep_init')
        'timestep_initial'
        >>> CCPP_STATE_MACH.transition_match('initialize')
        'initialize'
        >>> CCPP_STATE_MACH.transition_match('initialize')[0:4]
        'init'
        >>> CCPP_STATE_MACH.transition_match('initize')

        >>> CCPP_STATE_MACH.transition_match('run')
        'run'
        >>> CCPP_STATE_MACH.transition_match('finalize')
        'finalize'
        >>> CCPP_STATE_MACH.transition_match('finalize')[0:5]
        'final'
        >>> CCPP_STATE_MACH.transition_match('final')
        'finalize'
        >>> CCPP_STATE_MACH.transition_match('finalize_bar')

        >>> CCPP_STATE_MACH.function_match('foo_init')
        ('foo', 'init', 'initialize')
        >>> CCPP_STATE_MACH.function_match('foo_init', transition='finalize')
        (None, None, None)
        >>> CCPP_STATE_MACH.function_match('FOO_INIT')
        ('FOO', 'INIT', 'initialize')
        >>> CCPP_STATE_MACH.function_match('foo_initial')
        ('foo', 'initial', 'initialize')
        >>> CCPP_STATE_MACH.function_match('foo_initialize')
        ('foo', 'initialize', 'initialize')
        >>> CCPP_STATE_MACH.function_match('foo_initialize')[1][0:4]
        'init'
        >>> CCPP_STATE_MACH.function_match('foo_initize')
        (None, None, None)
        >>> CCPP_STATE_MACH.function_match('foo_timestep_initial')
        ('foo', 'timestep_initial', 'timestep_initial')
        >>> CCPP_STATE_MACH.function_match('foo_timestep_init')
        ('foo', 'timestep_init', 'timestep_initial')
        >>> CCPP_STATE_MACH.function_match('foo_timestep_initialize')
        ('foo', 'timestep_initialize', 'timestep_initial')
        >>> CCPP_STATE_MACH.function_match('foo_run')
        ('foo', 'run', 'run')
        >>> CCPP_STATE_MACH.function_match('foo_finalize')
        ('foo', 'finalize', 'finalize')
        >>> CCPP_STATE_MACH.function_match('foo_finalize')[1][0:5]
        'final'
        >>> CCPP_STATE_MACH.function_match('foo_final')
        ('foo', 'final', 'finalize')
        >>> CCPP_STATE_MACH.function_match('foo_finalize_bar')
        (None, None, None)
        >>> CCPP_STATE_MACH.function_match('foo_timestep_final')
        ('foo', 'timestep_final', 'timestep_final')
        >>> CCPP_STATE_MACH.function_match('foo_timestep_finalize')
        ('foo', 'timestep_finalize', 'timestep_final')
        """
        self.__ddt_library = ddt_library
        # Collect all relevant schemes
        # For all groups, find associated init and final methods
        scheme_set = list()
        for group in self.groups:
            for scheme in group.schemes():
                scheme_set.append(scheme.name)
            # end for
        # end for
        no_scheme_entries = {} # Skip schemes that are not in this suite
        for module in scheme_set:
            if scheme_library[module]:
                scheme_entries = scheme_library[module]
            else:
                scheme_entries = no_scheme_entries
            # end if
            for phase in self._full_phases:
                if phase in scheme_entries:
                    header = scheme_entries[phase]
                    # Add this scheme's init or final routine
                    pgroup = self._full_phases[phase]
                    if not pgroup.has_item(header.title):
                        sstr = Suite.__scheme_template.format(module)
                        sxml = ET.fromstring(sstr)
                        scheme = Scheme(sxml, self.__context, pgroup, run_env)
                        pgroup.add_part(scheme)
                    # end if (no else, scheme is already in group)
                # end if (no else, phase not in scheme set)
            # end for
        # end for
        # Grab the host model argument list
        self.__host_arg_list_full = host_model.argument_list()
        self.__host_arg_list_noloop = host_model.argument_list(loop_vars=False)
        # First pass, create init, run, and finalize sequences
        for item in self.groups:
            if item.name in self.__full_groups:
                phase = self.__full_groups[item.name].phase()
            else:
                phase = RUN_PHASE_NAME
            # end if
            lmsg = "Group {}, schemes = {}"
            if run_env.verbose:
                run_env.logger.debug(lmsg.format(item.name,
                                                 [x.name
                                                  for x in item.schemes()]))
            item.analyze(phase, self, scheme_library, ddt_library,
                         self.check_suite_state(phase),
                         self.set_suite_state(phase))
            # Look for group variables that need to be promoted to the suite
            # We need to promote any variable used later to the suite, however,
            # we do not yet know if it will be used.
            # Add new group-created variables
            gvars = item.variable_list()
            for gvar in gvars:
                stdname = gvar.get_prop_value('standard_name')
                if not stdname in self.__gvar_stdnames:
                    self.__gvar_stdnames[stdname] = item
                # end if
            # end for
        # end for

    def is_run_group(self, group):
        """Method to separate out run-loop groups from special initial
        and final groups
        """
        return ((group.name not in self._beg_groups) and
                (group.name not in self._end_groups))

    def max_part_len(self):
        """What is the longest suite subroutine name?"""
        maxlen = 0
        for spart in self.groups:
            if self.is_run_group(spart):
                maxlen = max(maxlen, len(spart.name))
            # end if
        # end for
        return maxlen

    def part_list(self):
        """Return list of run phase parts (groups)"""
        parts = list()
        for spart in self.groups:
            if self.is_run_group(spart):
                parts.append(spart.name[len(self.name)+1:])
            # end if
        # end for
        return parts

    def phase_group(self, phase):
        """Return the (non-run) group specified by <phase>"""
        if phase in self._full_phases:
            return self._full_phases[phase]
        # end if
        raise ParseInternalError("Incorrect phase, '{}'".format(phase))

    def constituent_dictionary(self):
        """Return the constituent dictionary for this suite"""
        return self.parent

    def write(self, output_dir, run_env):
        """Create caps for all groups in the suite and for the entire suite
        (calling the group caps one after another)"""
        # Set name of module and filename of cap
        filename = '{module_name}.F90'.format(module_name=self.module)
        if run_env.verbose:
            run_env.logger.debug('Writing CCPP suite file, {}'.format(filename))
        # end if
        # Retrieve the name of the constituent module for Group use statements
        const_mod = self.parent.constituent_module_name()
        # Init
        output_file_name = os.path.join(output_dir, filename)
        with FortranWriter(output_file_name, 'w',
                           "CCPP Suite Cap for {}".format(self.name),
                           self.module) as outfile:
            # Write module 'use' statements here
            outfile.write('use {}'.format(KINDS_MODULE), 1)
            # Look for any DDT types
            self.__ddt_library.write_ddt_use_statements(self.values(),
                                                        outfile, 1)
            # Write out constituent module use statement(s)
            const_dict = self.constituent_dictionary()
            const_dict.write_suite_use(outfile, 1)
            outfile.write_preamble()
            outfile.write('! Suite interfaces', 1)
            line = Suite.__state_machine_init
            var_name = Suite.__state_machine_var_name
            var_state = Suite.__state_machine_initial_state
            outfile.write(line.format(css_var_name=var_name,
                                      state=var_state), 1)
            for group in self.__groups:
                outfile.write('public :: {}'.format(group.name), 1)
            # end for
            # Declare constituent public interfaces
            const_dict.declare_public_interfaces(outfile, 1)
            # Declare constituent private suite interfaces and data
            const_dict.declare_private_data(outfile, 1)
            outfile.write('\n! Private suite variables', 1)
            for svar in self.keys():
                self[svar].write_def(outfile, 1, self, allocatable=True)
            # end for
            outfile.end_module_header()
            for group in self.__groups:
                if group.name in self._beg_groups:
                    if group.name == self.__suite_reg_group.name:
                        group.write(outfile, self.__host_arg_list_noloop,
                                    1, const_mod, suite_vars=self)
                    else:
                        group.write(outfile, self.__host_arg_list_noloop,
                                    1, const_mod, suite_vars=self, allocate=True)
                    # end if
                elif group.name in self._end_groups:
                    group.write(outfile, self.__host_arg_list_noloop,
                                1, const_mod, suite_vars=self, deallocate=True)
                else:
                    group.write(outfile, self.__host_arg_list_full, 1,
                                const_mod)
                # end if
            # end for
            err_vars = self.find_error_variables(any_scope=True,
                                                 clone_as_out=True)
            # Write the constituent properties interface
            const_dict.write_constituent_routines(outfile, 1,
                                                  self.name, err_vars)
        # end with
        return output_file_name

###############################################################################

class API(VarDictionary):
    """Class representing the API for the CCPP framework.
    The API class organizes the suites for which CAPS will be generated"""

    __suite_fname = 'ccpp_physics_suite_list'
    __part_fname = 'ccpp_physics_suite_part_list'
    __vars_fname = 'ccpp_physics_suite_variables'
    __schemes_fname = 'ccpp_physics_suite_schemes'

    __file_desc = "API for {host_model} calls to CCPP suites"

    __preamble = '''
{module_use}
'''

    __sub_name_template = 'ccpp_physics'

    __subhead = 'subroutine {subname}({api_call_list})'

    __subfoot = 'end subroutine {subname}\n'

    # Note, we cannot add these vars to our dictionary as we do not want
    #    them showing up in group dummy arg lists
    __suite_name = Var({'local_name':'suite_name',
                        'standard_name':'suite_name',
                        'intent':'in', 'type':'character',
                        'kind':'len=*', 'units':'',
                        'dimensions':'()'}, _API_SOURCE, _API_DUMMY_RUN_ENV)

    __suite_part = Var({'local_name':'suite_part',
                        'standard_name':'suite_part',
                        'intent':'in', 'type':'character',
                        'kind':'len=*', 'units':'',
                        'dimensions':'()'}, _API_SOURCE, _API_DUMMY_RUN_ENV)

    def __init__(self, sdfs, host_model, scheme_headers, run_env):
        """Initialize this API.
        <sdfs> is the list of Suite Definition Files to be parsed for
            data needed by the CCPP cap.
        <host_model> is a HostModel object to reference for host model
            variables.
        <scheme_headers> is the list of parsed physics scheme metadata files.
            Every scheme referenced by an SDF in <sdfs> MUST be in this list,
            however, unused schemes are allowed.
        <run_env> is the CCPPFrameworkEnv object for this framework run.
        """
        self.__module = 'ccpp_physics_api'
        self.__host = host_model
        self.__suites = list()
        super().__init__(self.module, run_env, parent_dict=self.host_model)
        # Create a usable library out of scheme_headers
        # Structure is dictionary of dictionaries
        # Top-level dictionary is keyed by function name
        # Secondary level is by phase
        scheme_library = {}
        # First, process DDT headers
        self.__ddt_lib = DDTLibrary('{}_api'.format(self.host_model.name),
                                    run_env, ddts=[d for d in scheme_headers
                                                   if d.header_type == 'ddt'])
        for header in [d for d in scheme_headers if d.header_type != 'ddt']:
            if header.header_type != 'scheme':
                errmsg = "{} is an unknown CCPP API metadata header type, {}"
                raise CCPPError(errmsg.format(header.title, header.header_type))
            # end if
            func_id, _, match_trans =                                         \
                CCPP_STATE_MACH.function_match(header.title)
            if func_id not in scheme_library:
                scheme_library[func_id] = {}
            # end if
            func_entry = scheme_library[func_id]
            if match_trans not in func_entry:
                func_entry[match_trans] = header
            else:
                errmsg = "Duplicate scheme entry, {}"
                raise CCPPError(errmsg.format(header.title))
            # end if
        # end for
        # Turn the SDF files into Suites
        for sdf in sdfs:
            suite = Suite(sdf, self, run_env)
            suite.analyze(self.host_model, scheme_library,
                          self.__ddt_lib, run_env)
            self.__suites.append(suite)
        # end for
        # We will need the correct names for errmsg and errcode
        evar = self.host_model.find_variable(standard_name='ccpp_error_message')
        subst_dict = {'intent':'out'}
        if evar is not None:
            self._errmsg_var = evar.clone(subst_dict)
        else:
            raise CCPPError('Required variable, ccpp_error_message, not found')
        # end if
        evar = self.host_model.find_variable(standard_name='ccpp_error_code')
        if evar is not None:
            self._errcode_var = evar.clone(subst_dict)
        else:
            raise CCPPError('Required variable, ccpp_error_code, not found')
        # end if
        # We need a call list for every phase
        self.__call_lists = {}
        for phase in CCPP_STATE_MACH.transitions():
            self.__call_lists[phase] = CallList('API_' + phase, run_env)
            self.__call_lists[phase].add_variable(self.suite_name_var, run_env)
            if phase == RUN_PHASE_NAME:
                self.__call_lists[phase].add_variable(self.suite_part_var,
                                                      run_env)
            # end if
            for suite in self.__suites:
                for group in suite.groups:
                    if group.phase() == phase:
                        self.__call_lists[phase].add_vars(group.call_list,
                                                          run_env,
                                                          gen_unique=True)
                    # end if
                # end for
             # end for
        # end for

    @classmethod
    def interface_name(cls, phase):
        'Return the name of an API interface function'
        return "{}_{}".format(cls.__sub_name_template, phase)

    def call_list(self, phase):
        "Return the appropriate API call list variables"
        if phase in self.__call_lists:
            return self.__call_lists[phase]
        # end if
        raise ParseInternalError("Illegal phase, '{}'".format(phase))

    def write(self, output_dir, run_env):
        """Write CCPP API module"""
        if not self.suites:
            raise CCPPError("No suite specified for generating API")
        # end if
        api_filenames = list()
        # Write out the suite files
        for suite in self.suites:
            out_file_name = suite.write(output_dir, run_env)
            api_filenames.append(out_file_name)
        # end for
        return api_filenames

    @classmethod
    def declare_inspection_interfaces(cls, ofile):
        """Declare the API interfaces for the suite inquiry functions"""
        ofile.write("public :: {}".format(API.__suite_fname), 1)
        ofile.write("public :: {}".format(API.__part_fname), 1)
        ofile.write("public :: {}".format(API.__vars_fname), 1)
        ofile.write("public :: {}".format(API.__schemes_fname), 1)

    def get_errinfo_names(self):
        """Return a tuple of error output local names"""
        errmsg_name = self._errmsg_var.get_prop_value('local_name')
        errcode_name = self._errcode_var.get_prop_value('local_name')
        return (errmsg_name, errcode_name)

    @staticmethod
    def write_var_set_loop(ofile, varlist_name, var_list, indent,
                           add_allocate=True, start_index=1, start_var=None):
        """Write code to allocate (if <add_allocate> is True) and set
        <varlist_name> to <var_list>. Elements of <varlist_name> are set
        beginning at <start_index>.
        """
        if add_allocate:
            ofile.write("allocate({}({}))".format(varlist_name, len(var_list)),
                        indent)
        # end if
        for ind, var in enumerate(var_list):
            if start_var:
                ind_str = "{} + {}".format(start_var, ind + start_index)
            else:
                ind_str = "{}".format(ind + start_index)
            # end if
            ofile.write("{}({}) = '{}'".format(varlist_name, ind_str, var),
                        indent)
        # end for

    def write_suite_part_list_sub(self, ofile, errmsg_name, errcode_name):
        """Write the suite-part list subroutine"""
        inargs = f"suite_name, part_list, {errmsg_name}, {errcode_name}"
        ofile.write(f"subroutine {API.__part_fname}({inargs})", 1)
        oline = "character(len=*),              intent(in)  :: suite_name"
        ofile.write(oline, 2)
        oline = "character(len=*), allocatable, intent(out) :: part_list(:)"
        ofile.write(oline, 2)
        self._errmsg_var.write_def(ofile, 2, self)
        self._errcode_var.write_def(ofile, 2, self)
        else_str = ''
        ename = self._errcode_var.get_prop_value('local_name')
        ofile.write(f"{ename} = 0", 2)
        ename = self._errmsg_var.get_prop_value('local_name')
        ofile.write(f"{ename} = ''", 2)
        for suite in self.suites:
            oline = "{}if(trim(suite_name) == '{}') then"
            ofile.write(oline.format(else_str, suite.name), 2)
            API.write_var_set_loop(ofile, 'part_list', suite.part_list(), 3)
            else_str = 'else '
        # end for
        ofile.write("else", 2)
        emsg = f"write({errmsg_name}, '(3a)')"
        emsg += "'No suite named ', trim(suite_name), ' found'"
        ofile.write(emsg, 3)
        ofile.write(f"{errcode_name} = 1", 3)
        ofile.write("end if", 2)
        ofile.write(f"end subroutine {API.__part_fname}", 1)

    def write_req_vars_sub(self, ofile, errmsg_name, errcode_name):
        """Write the required variables subroutine"""
        oline = "suite_name, variable_list, {errmsg}, {errcode}"
        oline += ", input_vars, output_vars, struct_elements"
        inargs = oline.format(errmsg=errmsg_name, errcode=errcode_name)
        ofile.write("\nsubroutine {}({})".format(API.__vars_fname, inargs), 1)
        ofile.write("! Dummy arguments", 2)
        oline = "character(len=*),              intent(in)  :: suite_name"
        ofile.write(oline, 2)
        oline = "character(len=*), allocatable, intent(out) :: variable_list(:)"
        ofile.write(oline, 2)
        self._errmsg_var.write_def(ofile, 2, self, extra_space=22)
        self._errcode_var.write_def(ofile, 2, self, extra_space=22)
        oline = "logical, optional,             intent(in) :: input_vars"
        ofile.write(oline, 2)
        oline = "logical, optional,             intent(in) :: output_vars"
        ofile.write(oline, 2)
        oline = "logical, optional,             intent(in) :: struct_elements"
        ofile.write(oline, 2)
        ofile.write("! Local variables", 2)
        ofile.write("logical {}:: input_vars_use".format(' '*34), 2)
        ofile.write("logical {}:: output_vars_use".format(' '*34), 2)
        ofile.write("logical {}:: struct_elements_use".format(' '*34), 2)
        ofile.write("integer {}:: num_vars".format(' '*34), 2)
        ofile.write("", 0)
        ename = self._errcode_var.get_prop_value('local_name')
        ofile.write("{} = 0".format(ename), 2)
        ename = self._errmsg_var.get_prop_value('local_name')
        ofile.write("{} = ''".format(ename), 2)
        ofile.write("if (present(input_vars)) then", 2)
        ofile.write("input_vars_use = input_vars", 3)
        ofile.write("else", 2)
        ofile.write("input_vars_use = .true.", 3)
        ofile.write("end if", 2)
        ofile.write("if (present(output_vars)) then", 2)
        ofile.write("output_vars_use = output_vars", 3)
        ofile.write("else", 2)
        ofile.write("output_vars_use = .true.", 3)
        ofile.write("end if", 2)
        ofile.write("if (present(struct_elements)) then", 2)
        ofile.write("struct_elements_use = struct_elements", 3)
        ofile.write("else", 2)
        ofile.write("struct_elements_use = .true.", 3)
        ofile.write("end if", 2)
        else_str = ''
        for suite in self.suites:
            parent = suite.parent
            # Collect all the suite variables
            oline = "{}if(trim(suite_name) == '{}') then"
            input_vars = [set(), set(), set()] # leaves, arrays, leaf elements
            inout_vars = [set(), set(), set()] # leaves, arrays, leaf elements
            output_vars = [set(), set(), set()] # leaves, arrays, leaf elements
            const_initialized_in_physics = {}
            for part in suite.groups:
                for var in part.call_list.variable_list():
                    phase = part.phase()
                    stdname = var.get_prop_value("standard_name")
                    intent = var.get_prop_value("intent")
                    protected = var.get_prop_value("protected")
                    constituent = var.is_constituent()
                    if stdname not in const_initialized_in_physics:
                        const_initialized_in_physics[stdname] = False
                    # end if
                    if (parent is not None) and (not protected):
                        pvar = parent.find_variable(standard_name=stdname)
                        if pvar is not None:
                            protected = pvar.get_prop_value("protected")
                        # end if
                    # end if
                    elements = var.intrinsic_elements(check_dict=self.parent,
                                                      ddt_lib=self.__ddt_lib)
                    if (intent == 'in') and (not protected) and (not const_initialized_in_physics[stdname]):
                        if isinstance(elements, list):
                            input_vars[1].add(stdname)
                            input_vars[2].update(elements)
                        else:
                            input_vars[0].add(stdname)
                        # end if
                    elif intent == 'inout' and (not const_initialized_in_physics[stdname]):
                        if isinstance(elements, list):
                            inout_vars[1].add(stdname)
                            inout_vars[2].update(elements)
                        else:
                            inout_vars[0].add(stdname)
                        # end if
                    elif constituent and (intent == 'out' and phase != 'initialize' and not
                         const_initialized_in_physics[stdname]):
                        # constituents HAVE to be initialized in the init phase because the dycore needs to advect them
                        emsg = f"constituent variable '{stdname}' cannot be initialized in the '{phase}' phase"
                        raise CCPPError(emsg)
                    elif intent == 'out' and constituent and phase == 'initialize':
                        const_initialized_in_physics[stdname] = True
                    elif intent == 'out':
                        if isinstance(elements, list):
                            output_vars[1].add(stdname)
                            output_vars[2].update(elements)
                        else:
                            output_vars[0].add(stdname)
                        # end if
                    # end if
                # end for
            # end for
            # Figure out how many total variables to return and allocate
            #   variable_list to that size
            ofile.write(oline.format(else_str, suite.name), 2)
            ofile.write("if (input_vars_use .and. output_vars_use) then", 3)
            have_elems = input_vars[2] or inout_vars[2] or output_vars[2]
            if have_elems:
                ofile.write("if (struct_elements_use) then", 4)
                numvars = len(input_vars[0] | input_vars[2] | inout_vars[0] |
                              inout_vars[2] | output_vars[0] | output_vars[2])
                ofile.write("num_vars = {}".format(numvars), 5)
                ofile.write("else", 4)
            # end if
            numvars = len(input_vars[0] | input_vars[1] | inout_vars[0] |
                          inout_vars[1] | output_vars[0] | output_vars[1])
            ofile.write("num_vars = {}".format(numvars), 5 if have_elems else 4)
            if have_elems:
                ofile.write("end if", 4)
            # end if
            ofile.write("else if (input_vars_use) then", 3)
            have_elems = input_vars[2] or inout_vars[2]
            if have_elems:
                ofile.write("if (struct_elements_use) then", 4)
                numvars = len(input_vars[0] | input_vars[2] |
                              inout_vars[0] | inout_vars[2])
                ofile.write("num_vars = {}".format(numvars), 5)
                ofile.write("else", 4)
            # end if
            numvars = len(input_vars[0] | input_vars[1] |
                          inout_vars[0] | inout_vars[1])
            ofile.write("num_vars = {}".format(numvars), 5 if have_elems else 4)
            if have_elems:
                ofile.write("end if", 4)
            # end if
            ofile.write("else if (output_vars_use) then", 3)
            have_elems = inout_vars[2] or output_vars[2]
            if have_elems:
                ofile.write("if (struct_elements_use) then", 4)
                numvars = len(inout_vars[0] | inout_vars[2] |
                              output_vars[0] | output_vars[2])
                ofile.write("num_vars = {}".format(numvars), 5)
                ofile.write("else", 4)
            # end if
            numvars = len(inout_vars[0] | inout_vars[1] |
                          output_vars[0] | output_vars[1])
            ofile.write("num_vars = {}".format(numvars), 5 if have_elems else 4)
            if have_elems:
                ofile.write("end if", 4)
            # end if
            ofile.write("else", 3)
            ofile.write("num_vars = 0", 4)
            ofile.write("end if", 3)
            ofile.write("allocate(variable_list(num_vars))", 3)
            # Now, fill in the variable_list array
            # Start with inout variables
            elem_start = 1
            leaf_start = 1
            leaf_written_set = inout_vars[0].copy()
            elem_written_set = inout_vars[0].copy()
            leaf_list = sorted(inout_vars[0])
            if inout_vars[0] or inout_vars[1] or inout_vars[2]:
                ofile.write("if (input_vars_use .or. output_vars_use) then", 3)
                API.write_var_set_loop(ofile, 'variable_list', leaf_list, 4,
                                       add_allocate=False,
                                       start_index=leaf_start)
            # end if
            leaf_start += len(leaf_list)
            elem_start += len(leaf_list)
            # elements which have not been written out
            elem_list = sorted(inout_vars[2] - elem_written_set)
            elem_written_set = elem_written_set | inout_vars[2]
            leaf_list = sorted(inout_vars[1] - leaf_written_set)
            leaf_written_set = leaf_written_set | inout_vars[1]
            if elem_list or leaf_list:
                ofile.write("if (struct_elements_use) then", 4)
                API.write_var_set_loop(ofile, 'variable_list', elem_list, 5,
                                       add_allocate=False,
                                       start_index=elem_start)
                elem_start += len(elem_list)
                ofile.write("num_vars = {}".format(elem_start - 1), 5)
                ofile.write("else", 4)
                API.write_var_set_loop(ofile, 'variable_list', leaf_list, 5,
                                       add_allocate=False,
                                       start_index=leaf_start)
                leaf_start += len(leaf_list)
                ofile.write("num_vars = {}".format(leaf_start - 1), 5)
                ofile.write("end if", 4)
            else:
                ofile.write("num_vars = {}".format(len(leaf_written_set)),
                            4 if leaf_written_set else 3)
            # end if
            if inout_vars[0] or inout_vars[1] or inout_vars[2]:
                ofile.write("end if", 3)
            # end if
            # Write input variables
            leaf_list = sorted(input_vars[0] - leaf_written_set)
            # Are there any output variables which are also input variables
            #    (e.g., for a different part (group) of the suite)?
            # We need to collect them now in case <input_vars> is selected
            #    but not <output_vars>.
            leaf_cross_set = output_vars[0] & input_vars[0]
            simp_cross_set = (output_vars[1] & input_vars[1]) - leaf_cross_set
            elem_cross_set = (output_vars[2] & input_vars[2]) - leaf_cross_set
            # Subtract the variables which have already been written out
            leaf_cross_list = sorted(leaf_cross_set - leaf_written_set)
            simp_cross_list = sorted(simp_cross_set - leaf_written_set)
            elem_cross_list = sorted(elem_cross_set - elem_written_set)
            # Next move back to processing the input variables
            leaf_written_set = leaf_written_set | input_vars[0]
            elem_list = sorted(input_vars[2] - elem_written_set)
            elem_written_set = elem_written_set | input_vars[0] | input_vars[2]
            have_inputs = elem_list or leaf_list
            if have_inputs:
                ofile.write("if (input_vars_use) then", 3)
                # elements which have not been written out
            # end if
            API.write_var_set_loop(ofile, 'variable_list', leaf_list, 4,
                                   add_allocate=False, start_var="num_vars",
                                   start_index=1)
            if leaf_list:
                ofile.write("num_vars = num_vars + {}".format(len(leaf_list)),
                            4)
            # end if
            leaf_start += len(leaf_list)
            elem_start += len(leaf_list)
            leaf_list = input_vars[1].difference(leaf_written_set)
            leaf_written_set.union(input_vars[1])
            if elem_list or leaf_list:
                ofile.write("if (struct_elements_use) then", 4)
                API.write_var_set_loop(ofile, 'variable_list', elem_list, 5,
                                       add_allocate=False,
                                       start_index=elem_start)
                elem_start += len(elem_list) - 1
                ofile.write("num_vars = {}".format(elem_start), 5)
                ofile.write("else", 4)
                API.write_var_set_loop(ofile, 'variable_list', leaf_list, 5,
                                       add_allocate=False,
                                       start_index=leaf_start)
                leaf_start += len(leaf_list) - 1
                ofile.write("num_vars = {}".format(leaf_start), 5)
                ofile.write("end if", 4)
            # end if
            if have_inputs:
                ofile.write("end if", 3)
            # end if
            # Write output variables
            leaf_list = sorted(output_vars[0].difference(leaf_written_set))
            leaf_written_set = leaf_written_set.union(output_vars[0])
            elem_written_set = elem_written_set.union(output_vars[0])
            elem_list = sorted(output_vars[2].difference(elem_written_set))
            elem_written_set = elem_written_set.union(output_vars[2])
            have_outputs = elem_list or leaf_list
            if have_outputs:
                ofile.write("if (output_vars_use) then", 3)
            # end if
            leaf_start = 1
            API.write_var_set_loop(ofile, 'variable_list', leaf_list, 4,
                                   add_allocate=False, start_var="num_vars",
                                   start_index=leaf_start)
            leaf_start += len(leaf_list)
            elem_start = leaf_start
            leaf_list = output_vars[1].difference(leaf_written_set)
            leaf_written_set.union(output_vars[1])
            if elem_list or leaf_list:
                ofile.write("if (struct_elements_use) then", 4)
                API.write_var_set_loop(ofile, 'variable_list', elem_list, 5,
                                       add_allocate=False, start_var="num_vars",
                                       start_index=elem_start)
                elem_start += len(elem_list)
                ofile.write("else", 4)
                API.write_var_set_loop(ofile, 'variable_list', leaf_list, 5,
                                       add_allocate=False, start_var="num_vars",
                                       start_index=leaf_start)
                leaf_start += len(leaf_list)
                ofile.write("end if", 4)
            # end if
            if leaf_cross_list or elem_cross_list:
                ofile.write("if (.not. input_vars_use) then", 4)
                API.write_var_set_loop(ofile, 'variable_list', leaf_cross_list,
                                       5, add_allocate=False,
                                       start_var="num_vars",
                                       start_index=leaf_start)
                leaf_start += len(leaf_cross_list)
                elem_start += len(leaf_cross_list)
                if elem_cross_list or simp_cross_list:
                    ofile.write("if (struct_elements_use) then", 5)
                    API.write_var_set_loop(ofile, 'variable_list',
                                           elem_cross_list, 6,
                                           add_allocate=False,
                                           start_var="num_vars",
                                           start_index=elem_start)
                    elem_start += len(elem_list)
                    ofile.write("else", 5)
                    API.write_var_set_loop(ofile, 'variable_list',
                                           leaf_cross_list, 6,
                                           add_allocate=False,
                                           start_var="num_vars",
                                           start_index=leaf_start)
                    leaf_start += len(leaf_list)
                    ofile.write("end if", 5)
                # end if
                ofile.write("end if", 4)
            if have_outputs:
                ofile.write("end if", 3)
            # end if
            else_str = 'else '
        # end for
        ofile.write("else", 2)
        emsg = "write({errmsg}, '(3a)')".format(errmsg=errmsg_name)
        emsg += "'No suite named ', trim(suite_name), ' found'"
        ofile.write(emsg, 3)
        ofile.write("{errcode} = 1".format(errcode=errcode_name), 3)
        ofile.write("end if", 2)
        ofile.write("end subroutine {}".format(API.__vars_fname), 1)

    def write_suite_schemes_sub(self, ofile, errmsg_name, errcode_name):
        """Write the suite schemes list subroutine"""
        oline = "suite_name, scheme_list, {errmsg}, {errcode}"
        inargs = oline.format(errmsg=errmsg_name, errcode=errcode_name)
        ofile.write("\nsubroutine {}({})".format(API.__schemes_fname,
                                                 inargs), 1)
        oline = "character(len=*),              intent(in)  :: suite_name"
        ofile.write(oline, 2)
        oline = "character(len=*), allocatable, intent(out) :: scheme_list(:)"
        ofile.write(oline, 2)
        self._errmsg_var.write_def(ofile, 2, self)
        self._errcode_var.write_def(ofile, 2, self)
        else_str = ''
        ename = self._errcode_var.get_prop_value('local_name')
        ofile.write("{} = 0".format(ename), 2)
        ename = self._errmsg_var.get_prop_value('local_name')
        ofile.write("{} = ''".format(ename), 2)
        for suite in self.suites:
            oline = "{}if(trim(suite_name) == '{}') then"
            ofile.write(oline.format(else_str, suite.name), 2)
            # Collect the list of schemes in this suite
            schemes = set()
            for part in suite.groups:
                schemes.update([x.name for x in part.schemes()])
            # end for
            # Write out the list
            API.write_var_set_loop(ofile, 'scheme_list', schemes, 3)
            else_str = 'else '
        # end for
        ofile.write("else", 2)
        emsg = "write({errmsg}, '(3a)')".format(errmsg=errmsg_name)
        emsg += "'No suite named ', trim(suite_name), ' found'"
        ofile.write(emsg, 3)
        ofile.write("{errcode} = 1".format(errcode=errcode_name), 3)
        ofile.write("end if", 2)
        ofile.write("end subroutine {}".format(API.__schemes_fname), 1)

    def write_inspection_routines(self, ofile):
        """Write the list_suites and list_suite_parts subroutines"""
        errmsg_name, errcode_name = self.get_errinfo_names()
        ofile.write("subroutine {}(suites)".format(API.__suite_fname), 1)
        nsuites = len(self.suites)
        oline = "character(len=*), allocatable, intent(out) :: suites(:)"
        ofile.write(oline, 2)
        ofile.write("\nallocate(suites({}))".format(nsuites), 2)
        for ind, suite in enumerate(self.suites):
            ofile.write("suites({}) = '{}'".format(ind+1, suite.name), 2)
        # end for
        ofile.write("end subroutine {}".format(API.__suite_fname), 1)
        # Write out the suite part list subroutine
        self.write_suite_part_list_sub(ofile, errmsg_name, errcode_name)
        # Write out the suite required variable subroutine
        self.write_req_vars_sub(ofile, errmsg_name, errcode_name)
        # Write out the suite scheme list subroutine
        self.write_suite_schemes_sub(ofile, errmsg_name, errcode_name)

    @property
    def module(self):
        """Return the module name of the API."""
        return self.__module

    @property
    def host_model(self):
        """Return the host model which will use this API."""
        return self.__host

    @property
    def suite_name_var(self):
        "Return the name of the variable specifying the suite to run"
        return self.__suite_name

    @property
    def suite_part_var(self):
        "Return the name of the variable specifying the suite group to run"
        return self.__suite_part

    @property
    def suites(self):
        "Return the list of this API's suites"
        return self.__suites

###############################################################################
if __name__ == "__main__":
    try:
        # First, run doctest
        # pylint: disable=ungrouped-imports
        import doctest
        import sys
        # pylint: enable=ungrouped-imports
        fail, _ = doctest.testmod()
        # Goal: Replace this test with a suite from unit tests
        FRAME_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        TEMP_SUITE = os.path.join(FRAME_ROOT, 'test', 'capgen_test',
                                  'temp_suite.xml')
        if os.path.exists(TEMP_SUITE):
            _ = Suite(TEMP_SUITE, VarDictionary('temp_suite',
                                                _API_DUMMY_RUN_ENV),
                      _API_DUMMY_RUN_ENV)
        else:
            print("Cannot find test file, '{}', skipping test".format(TEMP_SUITE))
        # end if
        sys.exit(fail)
    except CCPPError as suite_error:
        print("{}".format(suite_error))
        sys.exit(fail)
    # end try
# end if
