#!/usr/bin/env python
#

"""Classes and methods to create a Fortran suite-implementation file
to implement calls to a set of suites for a given host model."""

# Python library imports
from __future__ import print_function
import copy
import os.path
import sys
import re
import xml.etree.ElementTree as ET
# CCPP framework imports
from parse_tools   import ParseContext, ParseSource, context_string
from parse_tools   import ParseInternalError, ParseSyntaxError, CCPPError
from parse_tools   import FORTRAN_ID
from parse_tools   import read_xml_file, validate_xml_file, find_schema_version
from metavar       import Var, VarDictionary, ddt_modules
from state_machine import StateMachine
from fortran_tools import FortranWriter

###############################################################################
# Module (global) variables
###############################################################################

__init_st__ = r"(?:(?i)init(?:ial(?:ize)?)?)"
__final_st__ = r"(?:(?i)final(?:ize)?)"
__run_st__ = r"(?:(?i)run)"
__ts_init_st__ = r"(?:(?i)timestep_init(?:ial(?:ize)?)?)"
__ts_final_st__ = r"(?:(?i)timestep_final(?:ize)?)"

dimension_re = re.compile(FORTRAN_ID+r"_((?i)dimension)$")

array_ref_re = re.compile(r"([^(]*)[(]([^)]*)[)]")

# Allowed CCPP transitions
CCPP_STATE_MACH = StateMachine((('initialize',       'uninitialized',
                                 'initialized',       __init_st__),
                                ('timestep_initial', 'initialized',
                                 'in_time_step',      __ts_init_st__),
                                ('run',              'in_time_step',
                                 'in_time_step',      __run_st__),
                                ('timestep_final',   'in_time_step',
                                 'initialized',       __ts_final_st__),
                                ('finalize',         'initialized',
                                 'uninitialized',     __final_st__)))

# Required variables for inclusion in auto-generated schemes
CCPP_REQUIRED_VARS = [Var({'local_name' : 'errflg',
                           'standard_name' : 'ccpp_error_flag', 'units' : '1',
                           'dimensions' : '()', 'type' : 'integer'},
                          ParseSource('ccpp_suite', 'REGISTRY',
                                      ParseContext())),
                      Var({'local_name' : 'errmsg',
                           'standard_name' : 'ccpp_error_message',
                           'units' : '1', 'dimensions' : '()',
                           'type' : 'integer'},
                          ParseSource('ccpp_suite', 'REGISTRY',
                                      ParseContext()))]

# CCPP copyright statement to be included in all generated Fortran files
COPYRIGHT = '''!
! This work (Common Community Physics Package Framework), identified by
! NOAA, NCAR, CU/CIRES, is free of known copyright restrictions and is
! placed in the public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'''

###############################################################################

class Scheme(object):
    "A single scheme in a suite (e.g., init method)"

    def __init__(self, scheme_xml, context):
        self._name = scheme_xml.text
        self._context = context
        self._version = scheme_xml.get('version', None)
        self._lib = scheme_xml.get('lib', None)

    @property
    def name(self):
        '''Return name of scheme'''
        return self._name

    def ddtspec_to_str(self, ddt_spec, host_model):
        "Properly convert a DDT field reference to a string"
        args = list()
        alen = len(ddt_spec)
        index = 0
        for var in ddt_spec:
            ddt = index < (alen - 1)
            argstr = self.host_arg_str(var, host_model, ddt)
            index = index + 1
            args.append(argstr)
        # End for
        return '%'.join(args)

    def find_host_model_var(self, hdim, host_model):
        "Create the correct array dimension reference for hdim"
        hsdims = list()
        for hsdim in hdim.split(':'):
            hsdim_var = host_model.find_variable(hsdim, loop_subst=True)
            if hsdim_var is None:
                raise CCPPError("No matching host variable for {} dimension, {}".format(self._subroutine_name, hsdim))
            elif isinstance(hsdim_var, tuple):
                # This is a dimension range (e.g., from a loop_subst)
                lnames = [x.get_prop_value('local_name') for x in hsdim_var]
                hsdims.extend(lnames)
            elif isinstance(hsdim_var, list):
                # This is a DDT reference
                hsdims.append(self.ddtspec_to_str(hsdim_var, host_model))
            else:
                hsdims.append(hsdim_var.get_prop_value('local_name'))
            # End if
        # End for
        loop_var = VarDictionary.loop_var_match(hdim)
        if (dimension_re.match(hdim) is not None) and (len(hsdims) == 1):
            # We need to specify the whole range
            hsdims = ['1'] + hsdims
        elif loop_var and (len(hsdims) == 1):
            # We may to specify the whole range
            lv_type = hdim.split('_')[-1]
            if lv_type == 'extent':
                hsdims = ['1'] + hsdims # This should print as '1:<name>_extent'
            elif lv_type == 'beg':
                hsdims.append('') # This should print as '<name>_beg:'
            elif lv_type == 'end':
                hsdims = [''] + hsdims # This should print as ':<name>_end'
            elif lv_type == 'number':
                pass # This should be a single value (not an array section)
            else:
                raise ParseInternalError("Unknown loop variable type, '{}' in '{}'".format(lv_type, hdim))
            # End if
        # End if
        return ':'.join(hsdims)

    def host_arg_str(self, hvar, host_model, ddt):
        '''Create the proper statement of a piece of a host-model variable.
        If ddt is True, we can only have a single element selected
        '''
        hstr = hvar.get_prop_value('local_name')
        hdims = hvar.get_dimensions()
        dimsep = ''
        # Does the local name have any extra indices?
        match = array_ref_re.match(hstr.strip())
        if match is not None:
            tokens = [x.strip() for x in match.group(2).strip().split(',')]
            # There should one ':' token for each entry in hdims
            if tokens.count(':') != len(hdims):
                raise CCPPError("Invalid DDT variable spec, {}, should have {} colons".format(hstr, len(hdims)))
            else:
                hstr = match.group(1)
                hdims_temp = hdims
                hdims = list()
                hdim_index = 0
                for token in tokens:
                    if token == ':':
                        hdims.append(hdims_temp[hdim_index])
                        hdim_index = hdim_index + 1
                    else:
                        hdims.append(token)
                    # End if
                # End for
            # End if
        # End if
        if len(hdims) > 0:
            dimstr = '('
        else:
            dimstr = ''
        # End if
        for hdim in hdims:
            # We can only have a single element of a DDT when selecting
            # a field. Is this a thread block?
            if ddt and (hdim == 'thread_block_begin:thread_block_end'):
                hdim = 'thread_block_number'
            # End if
            if ddt and (':' in hdim):
                raise CCPPError("Invalid DDT dimension spec {}{}".format(hstr, hdimval))
            else:
                # Find the host model variable for each dim
                hsdims = self.find_host_model_var(hdim, host_model)
                dimstr = dimstr + dimsep + hsdims
                dimsep = ', '
            # End if
        # End for
        if len(hdims) > 0:
            dimstr = dimstr + ')'
        # End if
        return hstr + dimstr

    def analyze(self, phase, parent, scheme_headers, suite_vars, logger):
        # Find the host model (do we need to do this?)
        host_model = parent
        while host_model.parent is not None:
            host_model = host_model.parent
        # End if
        my_header = None
        for module in scheme_headers:
            for header in module:
                func_id, trans_id, match_trans = CCPP_STATE_MACH.transition_match(header.title, transition=phase)
                if func_id == self.name:
                    my_header = header
                    self._subroutine_name = header.title
                    break
                # End if
            # End for
            if my_header is not None:
                break
            # End if
        # End for
        if my_header is None:
            estr = 'No {} header found for subroutine, {}'
            raise ParseInternalError(estr.format(phase, self.name),
                                     context=self._context)
        # End if
        if my_header.module is None:
            estr = 'No module found for subroutine, {}'
            raise ParseInternalError(estr.format(self._subroutine_name),
                                     context=self._context)
        # End if
        scheme_mods = set()
        scheme_use = 'use {}, only: {}'.format(my_header.module,
                                               self._subroutine_name)
        scheme_mods.add(scheme_use)
        if my_header is None:
            raise CCPPError('Could not find subroutine, {}'.format(subroutine_name))
        else:
            # We need to find the host model variable for each of our arguments
            my_args = my_header.variable_list()
            host_arglist = list()
            for svar in my_args:
                stdname = svar.get_prop_value('standard_name')
                intent = svar.get_prop_value('intent').lower()
                hvar = parent.find_variable(stdname, loop_subst=True)
                if (intent == 'in') or (intent == 'inout'):
                    if hvar is None:
                        raise CCPPError("No matching host or suite variable for {} input, {}".format(self._subroutine_name, stdname))
                elif hvar is None:
                    # Create suite variable for intent(out)
                    suite_vars.add_variable(svar, exists_ok=False)
                    hvar = suite_vars.find_variable(stdname)
                # End if (no else needed)
                if isinstance(hvar, list):
                    host_arglist.append(self.ddtspec_to_str(hvar, host_model))
                elif isinstance(hvar, tuple):
                    loop_subst = VarDictionary.loop_subst_match(stdname)
                    if (loop_subst is not None) and (len(loop_subst) == 2):
                        # Special case for loops
                        lnames = [x.get_prop_value('local_name') for x in hvar]
                        argstr = "{} - {} + 1".format(lnames[0], lnames[1])
                        host_arglist.append(argstr)
                    else:
                        for var in hvar:
                            argstr = self.host_arg_str(var, host_model, False)
                            host_arglist.append(argstr)
                        # End for
                    # End if
                else:
                    argstr = self.host_arg_str(hvar, host_model, False)
                    host_arglist.append(argstr)
                # End if
            # End for
            self._arglist = host_arglist
        # End if
        return scheme_mods, list() # No loop variables for scheme

    def write(self, outfile, indent):
        my_args = ', '.join(self._arglist)
        outfile.write('call {}({})'.format(self._subroutine_name, my_args), indent)

    def schemes(self):
        'Return self as a list for consistency with subcycle'
        return [self]

###############################################################################

class Subcycle(object):
    "Class to represent a subcycled group of schemes"

    __def_name_index__ = 0 # To create unique default loop index variables

    def __init__(self, sub_xml, context):
        self._name = sub_xml.get('name', None)
        if self._name is None:
            Subcycle.__def_name_index__ = Subcycle.__def_name_index__ + 1
            self._name = "subcycle_index{}".format(Subcycle.__def_name_index__)
        # End if
        self._loop = sub_xml.get('loop', "1")
        self._context = context
        self._schemes = list()
        for scheme in sub_xml:
            self._schemes.append(Scheme(scheme, context))
        # End forn

    def analyze(self, phase, parent, scheme_headers, suite_vars, logger):
        loopvars = set()
        loopvars.add('{}integer :: {}'.format(indent(2), self.name))
        scheme_mods = set()
        for scheme in self._schemes:
            smods, lvars = scheme.analyze(phase, parent, scheme_headers, suite_vars, logger)
            for smod in smods:
                scheme_mods.add(smod)
            # End for
            for lvar in lvars:
                loopvars.add(lvar)
            # End for
        # End for
        return scheme_mods, loopvars

    def write(self, outfile, indent):
        outfile.write('do {} = 1, {}'.format(self.name, self.loop), indent)
        # Note that 'scheme' may be a sybcycle or other construct
        for scheme in self._schemes:
            scheme.write(outfile, indent+1)
        # End for
        outfile.write('end do', 2)

    @property
    def name(self):
        '''name property to be consistent with other classes'''
        return self._name

    @property
    def loop(self):
        '''Get the loop value or variable standard_name'''
        return self._loop

    @property
    def schemes(self):
        '''Get the list of schemes'''
        return self._schemes

###############################################################################

class Group(VarDictionary):
    """Class to represent a grouping of schemes in a suite
    A Group object is implemented as a subroutine callable by the API.
    The main arguments to a group are the host model variables.
    Additional output arguments are generated from schemes with intent(out)
    arguments.
    Additional input or inout arguments are generated for inputs needed by
    schemes which are produced (intent(out)) by other groups.
    """

    __subhead__ = '''
   subroutine {subname}({args})
'''

    __subend__ = '''
   end subroutine {subname}
'''

    def __init__(self, group_xml, transition, parent, context):
        self._name = parent.name + '_' + group_xml.get('name')
        if transition not in CCPP_STATE_MACH.transitions():
            raise ParseInternalError("Bad transition argument to Group, '{}'".format(transition))
        # End if
        self._transition = transition
        self._parts = list()
        super(Group, self).__init__(self.name, parent_dict=parent)
        for item in group_xml:
            if item.tag == 'subcycle':
                self.add_item(Subcycle(item, self, context))
            else:
                self.add_item(Scheme(item, context))
            # End if
        # End for
        # Grab a frozen copy of the context
        self._context = ParseContext(context=context)
        self._loop_var_defs = set()
        self._local_schemes = set()
        self._host_vars = None
        self._host_ddts = None

    def has_item(self, item_name):
        'Check to see if an item is already in this group'
        has = False
        for item in self._parts:
            if item.name == item_name:
                has = True
                break
            # End if
        # End for
        return has

    def add_item(self, item):
        'Add an item (e.g., Suite, Subcycle) to this group'
        self._parts.append(item)

    def schemes(self):
        "Return a flattened list of schemes for this group"
        schemes = list()
        for item in self._parts:
            schemes.extend(item.schemes())
        # End for
        return schemes

    @property
    def phase(self):
        'Return the CCPP state transition for this group spec'
        return self._transition

    def phase_match(self, scheme_name):
        '''If scheme_name matches the group phase, return the group and
            function ID. Otherwise, return None
        '''
        fid, tid, mt = CCPP_STATE_MACH.transition_match(scheme_name,
                                                        transition=self.phase)
        if tid is not None:
            return self, fid
        else:
            return None, None
        # End if

    def analyze(self, phase, suite_vars, scheme_headers, logger):
        # We need a copy of the API and host model variables for dummy args
        self._host_vars = suite_vars.parent.variable_list(recursive=True,
                                                          loop_vars=(phase=='run'))
        self._phase = phase
        for item in self._parts:
            # Items can be schemes, subcycles or other objects
            # All have the same interface and return a set of module use
            # statements (lschemes) and a set of loop variables
            lschemes, lvars = item.analyze(phase, self, scheme_headers, suite_vars, logger)
            # Keep track of loop variables to define
            for lvar in lvars:
                self._loop_var_defs.add(lvar)
            # End for
            for lscheme in lschemes:
                self._local_schemes.add(lscheme)
            # End for
        # End for
        self._phase_check_stmts = Suite.check_suite_state(phase)
        self._set_state = Suite.set_suite_state(phase)
        # Find any host DDT variables to create use statements
        self._host_ddts = ddt_modules(self._host_vars)

    def write(self, outfile, host_arglist, indent,
              suite_vars=None, allocate=False, deallocate=False):
        local_subs = ''
        # group type for (de)allocation
        if 'timestep' in self._phase:
            group_type = 'timestep'
        else:
            group_type = 'run'
        # End if
        # First, write out the subroutine header
        subname = self.name
        outfile.write(Group.__subhead__.format(subname=subname, args=host_arglist), indent)
        # Write out any use statements
        mlen = max([len(x[0]) for x in self._host_ddts])
        for ddt in self._host_ddts:
            mspc = (mlen - len(ddt[0]))*' '
            outfile.write("use {}, {}only: {}".format(ddt[0], mspc, ddt[1]),
                          indent+1)
        # End for
        # Write out the scheme use statements
        for scheme in self._local_schemes:
            outfile.write(scheme, indent+1)
        # End for
        outfile.write('', 0)
        # Write out dummy arguments
        outfile.write('! Dummy arguments', indent+1)
        for var in self._host_vars:
            var.write_def(outfile, indent+1, self)
        # End for
        if len(self._loop_var_defs) > 0:
            outfile.write('\n! Local Variables', indent+1)
        # Write out local variables
        for var in self._loop_var_defs:
            outfile.write(var, indent+1)
        # End for
        outfile.write('', 0)
        # Check state machine
        verrflg = self.find_variable('ccpp_error_flag', any_scope=True)
        if verrflg is not None:
            errflg = verrflg.get_prop_value('local_name')
        else:
            raise CCPPError("No ccpp_error_flag variable for group, {}".format(self.name))
        # End if
        verrmsg = self.find_variable('ccpp_error_message', any_scope=True)
        if verrmsg is not None:
            errmsg = verrmsg.get_prop_value('local_name')
        else:
            raise CCPPError("No ccpp_error_message variable for group, {}".format(self.name))
        # End if
        for stmt in self._phase_check_stmts:
            text = stmt[0].format(errflg=errflg , errmsg=errmsg, funcname=self.name)
            outfile.write(text, indent + stmt[1])
        # End for
        # Allocate suite vars
        if allocate:
            for svar in suite_vars.variable_list():
                timestep_var = svar.get_prop_value('persistence')
                if group_type == timestep_var:
                    dims = svar.get_dimensions()
                    rdims = list()
                    for dim in dims:
                        dvar = self.find_dimension_subst(dim, context=self._context)
                        if dvar is None:
                            dvar = self.find_variable(dim, any_scope=True)
                        # End if
                        if dvar is None:
                            raise CCPPError("Dimension variable, {} not found{}".format(dim, context_string(self._context)))
                        else:
                            rdims.append(dvar)
                        # End if
                    # End for
                    alloc_str = ', '.join([x.get_prop_value('local_name') for x in rdims])
                    lname = svar.get_prop_value('local_name')
                    outfile.write("allocate({}({}))".format(lname, alloc_str), indent+1)
                # End if
            # End for
        # End if
        # Write the scheme and subcycle calls
        for item in self._parts:
            item.write(outfile, indent+1)
        # End for
        # Deallocate suite vars
        if deallocate:
            for svar in suite_vars.variable_list():
                timestep_var = svar.get_prop_value('persistence')
                if group_type == timestep_var:
                    lname = svar.get_prop_value('local_name')
                    outfile.write('deallocate({})'.format(lname), indent+1)
                # End if
            # End for
        # End if
        outfile.write(self._set_state[0], indent + self._set_state[1])
        outfile.write(Group.__subend__.format(subname=subname), indent)

    @property
    def name(self):
        '''Get the name of the group.'''
        return self._name

###############################################################################

class Suite(VarDictionary):

    ___state_machine_initial_state__ = 'uninitialized'
    __state_machine_var_name__     = 'ccpp_suite_state'

    __header__ ='''
!>
!! @brief Auto-generated cap module for the CCPP suite
!!
!
module {module}
'''

    __state_machine_init__ ='''
character(len=16) :: {css_var_name} = '{state}'
'''

    __footer__ = '''
end module {module}
'''

    # Note that these group names need to match CCPP_STATE_MACH
    __initial_group__ = '<group name="initialize"></group>'

    __final_group__ = '<group name="finalize"></group>'

    __timestep_initial_group__ = '<group name="timestep_initial"></group>'

    __timestep_final_group__ = '<group name="timestep_final"></group>'

    __scheme_template__ = '<scheme>{}</scheme>'

    def __init__(self, filename, api, logger):
        self._logger = logger
        self._name = None
        self._sdf_name = filename
        self._groups = list()
        self._suite_init_group = None
        self._suite_final_group = None
        self._timestep_init_group = None
        self._timestep_final_group = None
        self._context = None
        # Full phases/groups are special groups where the entire state is passed
        self._full_groups = {}
        self._full_phases = {}
        super(Suite, self).__init__(self.sdf_name, parent_dict=api, logger=logger)
        if not os.path.exists(self._sdf_name):
            raise CCPPError("Suite definition file {0} not found.".format(self._sdf_name))
        else:
            self.parse()
        # End if

    @property
    def name(self):
        '''Get the name of the suite.'''
        return self._name

    @property
    def sdf_name(self):
        '''Get the name of the suite definition file.'''
        return self._sdf_name

    @classmethod
    def check_suite_state(cls, stage):
        "Return a list of CCPP state check statements for <stage>"
        check_stmts = list()
        if stage in CCPP_STATE_MACH.transitions():
            # We need to make sure we are an allowed previous state
            prev_state = CCPP_STATE_MACH.initial_state(stage)
            css = "trim({})".format(Suite.__state_machine_var_name__)
            prev_str = "({} /= '{}')".format(css, prev_state)
            check_stmts.append(("if {} then".format(prev_str), 1))
            check_stmts.append(("{errflg} = 1", 2))
            errmsg_str = ("\"Invalid initial CCPP state, '\"//"+ css +
                          "//\"' in {funcname}\"")
            check_stmts.append(("{{errmsg}} = {}".format(errmsg_str), 2))
            check_stmts.append(("return", 2))
            check_stmts.append(("end if", 1))
        else:
            raise ParseInternalError("Unknown stage, '{}'".format(stage))
        # End if
        return check_stmts

    @classmethod
    def set_suite_state(cls, phase):
        final = CCPP_STATE_MACH.final_state(phase)
        return ("ccpp_suite_state = '{}'".format(final), 1)

    def new_group(self, group_string, transition):
        gxml = ET.fromstring(group_string)
        group = Group(gxml, transition, self, self._context)
        self._full_groups[group.name] = group
        self._full_phases[group.phase] = group
        return group

    def parse(self):
        '''Parse the suite definition file.'''
        success = True

        tree, suite_xml = read_xml_file(self._sdf_name, self._logger)
        # We do not have line number information for the XML file
        self._context = ParseContext(filename=self._sdf_name)
        # Validate the XML file
        version = find_schema_version(suite_xml, self._logger)
        res = validate_xml_file(self._sdf_name, 'suite', version, self._logger)
        if not res:
            raise CCPPError("Invalid suite definition file, '{}'".format(self._sdf_name))
        # End if
        self._name = suite_xml.get('name')
        self._logger.info("Reading suite definition file for '{}'".format(self._name))
        self._suite_init_group = self.new_group(Suite.__initial_group__,
                                                "initialize")
        self._suite_final_group = self.new_group(Suite.__final_group__,
                                                 "finalize")
        self._timestep_init_group = self.new_group(Suite.__timestep_initial_group__,
                                                   "timestep_initial")
        self._timestep_final_group = self.new_group(Suite.__timestep_final_group__,
                                                    "timestep_final")
        # Set up some groupings for later efficiency
        self._beg_groups = [x.name for x in [self._suite_init_group,
                                             self._timestep_init_group]]
        self._end_groups = [x.name for x in [self._suite_final_group,
                                             self._timestep_final_group]]
        # Build hierarchical structure as in SDF
        self._groups.append(self._suite_init_group)
        self._groups.append(self._timestep_init_group)
        for suite_item in suite_xml:
            item_type = suite_item.tag.lower()
            # Suite item is a group or a suite-wide init or final method
            if item_type == 'group':
                # Parse a group
                self._groups.append(Group(suite_item, 'run', self, self._context))
            else:
                match_trans = CCPP_STATE_MACH.group_match(item_type)
                if match_trans is None:
                    raise CCPPError("Unknown CCPP suite component tag type, '{}'".format(item_type))
                elif match_trans in self._full_phases:
                    # Parse a suite-wide initialization scheme
                    self._full_phases[match_trans].add_item(Scheme(suite_item, self._context))
                else:
                    raise ParseInternalError("Unhandled CCPP suite component tag type, '{}'".format(match_trans))
                # End if
        # End for
        self._groups.append(self._timestep_final_group)
        self._groups.append(self._suite_final_group)
        return success

    @property
    def module(self):
        '''Get the list of the module generated for this suite.'''
        return self._module

    @property
    def groups(self):
        '''Get the list of groups in this suite.'''
        return self._groups

    def analyze(self, host_model, scheme_headers, logger):
        '''Collect all information needed to write a suite file
        >>> CCPP_STATE_MACH.transition_match('foo_init')
        ('foo', 'init', 'initialize')
        >>> CCPP_STATE_MACH.transition_match('foo_init', transition='finalize')
        (None, None, None)
        >>> CCPP_STATE_MACH.transition_match('FOO_INIT')
        ('FOO', 'INIT', 'initialize')
        >>> CCPP_STATE_MACH.transition_match('foo_initial')
        ('foo', 'initial', 'initialize')
        >>> CCPP_STATE_MACH.transition_match('foo_initialize')
        ('foo', 'initialize', 'initialize')
        >>> CCPP_STATE_MACH.transition_match('foo_initialize')[1][0:4]
        'init'
        >>> CCPP_STATE_MACH.transition_match('foo_initize')
        (None, None, None)
        >>> CCPP_STATE_MACH.transition_match('foo_run')
        ('foo', 'run', 'run')
        >>> CCPP_STATE_MACH.transition_match('foo_finalize')
        ('foo', 'finalize', 'finalize')
        >>> CCPP_STATE_MACH.transition_match('foo_finalize')[1][0:5]
        'final'
        >>> CCPP_STATE_MACH.transition_match('foo_final')
        ('foo', 'final', 'finalize')
        >>> CCPP_STATE_MACH.transition_match('foo_finalize_bar')
        (None, None, None)
        '''
        # Collect all the available schemes
        for header_list in scheme_headers:
            for header in header_list:
                pgroup = None
                for gname in self._full_groups.keys():
                    mgroup = self._full_groups[gname]
                    pgroup, func_id = mgroup.phase_match(header.title)
                    if pgroup is not None:
                        break
                    # End if
                # End for
                if pgroup is not None:
                    if not pgroup.has_item(header.title):
                        sstr = Suite.__scheme_template__.format(func_id)
                        sxml = ET.fromstring(sstr)
                        scheme = Scheme(sxml, self._context)
                        pgroup.add_item(scheme)
                    # End if (no else needed)
                else:
                    if header.title.lower()[-4:] != '_run':
                        raise CCPPError("Unknown CCPP scheme run type, '{}'".format(header.title))
                    # End if
                # End if
            # End for
        # End for
        # Grab the host model argument list
        self._host_arg_list_full = host_model.argument_list()
        self._host_arg_list_noloop = host_model.argument_list(loop_vars=False)
        # First pass, create init, run, and finalize sequences
        for item in self.groups:
            if item.name in self._full_groups:
                phase = self._full_groups[item.name].phase
            else:
                phase = 'run'
            # End if
            logger.debug("Group {}, schemes = {}".format(item.name, [x.name for x in item.schemes()]))
            # Note that the group analyze can update this suite's vars
            item.analyze(phase, self, scheme_headers, logger)
        # End for

    def is_run_group(self, group):
        """Method to separate out run-loop groups from special initial
        and final groups
        """
        return (group.name not in self._beg_groups) and (group.name not in self._end_groups)

    def max_part_len(self):
        "What is the longest suite subroutine name?"
        maxlen = 0
        for spart in self.groups:
            if self.is_run_group(spart):
                maxlen = max(maxlen, len(spart.name))
            # End if
        # End for
        return maxlen

    def part_list(self):
        "Return list of run phase parts (groups)"
        parts = list()
        for spart in self.groups:
            if self.is_run_group(spart):
                parts.append(spart.name[len(self.name)+1:])
            # End if
        # End for
        return parts

    def write(self, output_dir):
        """Create caps for all groups in the suite and for the entire suite
        (calling the group caps one after another)"""
        # Set name of module and filename of cap
        self._module = 'ccpp_{}_cap'.format(self.name)
        filename = '{module_name}.F90'.format(module_name=self._module)
        # Init
        module_use = None
        output_file_name = os.path.join(output_dir, filename)
        with FortranWriter(output_file_name, 'w') as outfile:
            # Write suite header
            outfile.write(COPYRIGHT, 0)
            outfile.write(Suite.__header__.format(module=self._module), 0)
            # Write module 'use' statements here
            outfile.write('use machine', 1)
            outfile.write('implicit none\nprivate\n\n! Suite interfaces', 1)
            outfile.write(Suite.__state_machine_init__.format(css_var_name=Suite.__state_machine_var_name__, state=Suite.___state_machine_initial_state__), 1)
            for group in self._groups:
                outfile.write('public :: {}'.format(group.name), 1)
            # End for
            outfile.write('\n! Private suite variables', 1)
            for svar in self.keys():
                self[svar].write_def(outfile, 1, self, allocatable=True)
            # End for
            outfile.write('\ncontains', 0)
            for group in self._groups:
                if group.name in self._beg_groups:
                    group.write(outfile, self._host_arg_list_noloop, 1,
                                suite_vars=self, allocate=True)
                elif group.name in self._end_groups:
                    group.write(outfile, self._host_arg_list_noloop, 1,
                                suite_vars=self, deallocate=True)
                else:
                    group.write(outfile, self._host_arg_list_full, 1)
                # End if
            # End for
            # Finish off the module
            outfile.write(Suite.__footer__.format(module=self._module), 0)
            return output_file_name

###############################################################################

class API(VarDictionary):

    __suite_fname__ = 'ccpp_physics_suite_list'
    __part_fname__  = 'ccpp_physics_suite_part_list'

    __header__ = '''
!>
!! @brief Auto-generated API for {host_model} calls to CCPP suites
!!
!
module {module}
'''
    __preamble__ = '''
{module_use}

implicit none
private

'''

    __sub_name_template__ = 'ccpp_physics'

    __subhead__ = 'subroutine {subname}({host_call_list})'

    __subfoot__ = 'end subroutine {subname}\n'

    __footer__ = '''
end module {module}
'''

    __api_source__ = ParseSource("CCPP_API", "MODULE",
                                 ParseContext(filename="ccpp_suite.F90"))

    # Note, we cannot add these vars to our dictionary as we do not want
    #    them showing up in group dummy arg lists
    __suite_name__ = Var({'local_name':'suite_name',
                          'standard_name':'suite_name',
                          'intent':'in', 'type':'character',
                          'kind':'len=*', 'units':'',
                          'dimensions':'()'}, __api_source__)

    __suite_part__ = Var({'local_name':'suite_part',
                          'standard_name':'suite_part',
                          'intent':'in', 'type':'character',
                          'kind':'len=*', 'units':'',
                          'dimensions':'()'}, __api_source__)

    def __init__(self, sdfs, host_model, scheme_headers, logger):
        self._module        = 'ccpp_physics_api'
        self._host          = host_model
        self._schemes       = scheme_headers
        self._suites        = list()
        super(API, self).__init__(self.module, parent_dict=host_model, logger=logger)
        self._host_arg_list_full = host_model.argument_list()
        self._host_arg_list_noloop = host_model.argument_list(loop_vars=False)
        # Turn the SDF files into Suites
        for sdf in sdfs:
            suite = Suite(sdf, self, logger)
            suite.analyze(host_model, scheme_headers, logger)
            self._suites.append(suite)
        # End for
        # Find DDTs for use statements
        host_vars = host_model.variable_list()
        self._host_ddt_list_full = ddt_modules(host_vars)
        host_vars = host_model.variable_list(loop_vars=False)
        self._host_ddt_list_noloop = ddt_modules(host_vars)
        # We will need the correct names for errmsg and errflg
        self._errmsg_var = host_model.find_variable('ccpp_error_message')
        if self._errmsg_var is None:
            raise CCPPError('Required variable, ccpp_error_message, not found')
        # End if
        self._errflg_var = host_model.find_variable('ccpp_error_flag')
        if self._errflg_var is None:
            raise CCPPError('Required variable, ccpp_error_flag, not found')
        # End if

    @property
    def module(self):
        '''Get the module name of the API.'''
        return self._module

    @property
    def suite_name_var(self):
        return type(self).__suite_name__

    @property
    def suite_part_var(self):
        return type(self).__suite_part__

    @classmethod
    def interface_name(cls, phase):
        'Return the name of an API interface function'
        return "{}_{}".format(cls.__sub_name_template__, phase)

    def write(self, output_dir):
        """Write API for static build"""
        if len(self._suites) == 0:
            raise CCPPError("No suite specified for generating API")
        # End if
        filename = os.path.join(output_dir, self.module + '.F90')
        api_filenames = list()
        module_use = 'use machine'
        # Write out the suite files
        for suite in self._suites:
            out_file_name = suite.write(output_dir)
            api_filenames.append(out_file_name)
        # End for
        errmsg_name = self._errmsg_var.get_prop_value('local_name')
        errflg_name = self._errflg_var.get_prop_value('local_name')
        # Write out the API module
        with FortranWriter(filename, 'w') as api:
            api.write(COPYRIGHT, 0)
            api.write(API.__header__.format(host_model=self._host.name,
                                            module=self.module), 0)
            api.write(API.__preamble__.format(module_use=module_use), 1)
            for stage in CCPP_STATE_MACH.transitions():
                api.write("public :: ccpp_physics_{}".format(stage), 1)
            # End for
            api.write("public :: {}".format(API.__suite_fname__), 1)
            api.write("public :: {}".format(API.__part_fname__), 1)
            api.write("\ncontains\n", 0)
            # Write the module body
            max_suite_len = 0
            for suite in self._suites:
                max_suite_len = max(max_suite_len, len(suite.module))
            # End for
            for stage in CCPP_STATE_MACH.transitions():
                host_call_list = self.suite_name_var.get_prop_value('local_name')
                if stage == 'run':
                    host_call_list = host_call_list + ', ' + self.suite_part_var.get_prop_value('local_name')
                    hal = self._host_arg_list_full
                    hddt = self._host_ddt_list_full
                else:
                    hal = self._host_arg_list_noloop
                    hddt = self._host_ddt_list_noloop
                # End if
                host_call_list = host_call_list + ", " + hal
                host_call_list = host_call_list + ', '.join(self.prop_list('local_name'))
                subname = API.interface_name(stage)
                api.write(API.__subhead__.format(subname=subname, host_call_list=host_call_list), 1)
                # Write out any use statements
                mlen = max([len(x[0]) for x in hddt])
                mlen = max(mlen, max_suite_len)
                for suite in self._suites:
                    mspc = (mlen - len(suite.module))*' '
                    if stage == 'run':
                        for spart in suite.groups:
                            if suite.is_run_group(spart):
                                api.write("use {}, {}only: {}".format(suite.module, mspc, spart.name), 2)
                            # End if
                        # End for
                    else:
                        api.write("use {}, {}only: {}_{}".format(suite.module, mspc, suite.name, stage), 2)
                    # End if
                # End for
                for ddt in hddt:
                    mspc = (mlen - len(ddt[0]))*' '
                    api.write("use {}, {}only: {}".format(ddt[0], mspc, ddt[1]), 2)
                # End for
                # Declare dummy arguments
                self.suite_name_var.write_def(api, 2, self)
                if stage == 'run':
                    self.suite_part_var.write_def(api, 2, self)
                # End if
                for var in self._host.variable_list():
                    stdname = var.get_prop_value('standard_name')
                    if (stage=='run') or (not VarDictionary.loop_var_match(stdname)):
                        var.write_def(api, 2, self)
                    # End if
                # End for
                self.declare_variables(api, 2, loop_vars=(stage=='run'))
                # Now, add in cases for all suite parts
                else_str = '\n'
                for suite in self._suites:
                    api.write("{}if (trim(suite_name) == '{}') then".format(else_str, suite.name), 2)
                    if stage == 'run':
                        el2_str = ''
                        for spart in suite.groups:
                            if suite.is_run_group(spart):
                                pname = spart.name[len(suite.name)+1:]
                                api.write("{}if (trim(suite_part) == '{}') then".format(el2_str, pname), 3)
                                api.write("call {}({})".format(spart.name, self._host_arg_list_full), 4)
                                el2_str = 'else '
                            # End if
                        # End for
                        api.write("else", 3)
                        api.write("{errmsg} = 'No suite part named '//trim(suite_part)".format(errmsg=errmsg_name), 4)
                        api.write("{errmsg} = trim({errmsg})//' found in suite {sname}'".format(errmsg=errmsg_name, sname=suite.name), 4)
                        api.write("{errflg} = 1".format(errflg=errflg_name), 4)
                        api.write("end if", 3)
                    else:
                        api.write("call {}_{}({})".format(suite.name, stage, self._host_arg_list_noloop), 3)
                    # End if
                    else_str = 'else '
                # End for
                api.write("else", 2)
                api.write("{errmsg} = 'No suite named '//trim(suite_name)//' found'".format(errmsg=errmsg_name), 3)
                api.write("{errflg} = 1".format(errflg=errflg_name), 3)
                api.write("end if", 2)
                api.write(API.__subfoot__.format(subname=subname), 1)
            # End for
            # Write the list_suites subroutine
            api.write("subroutine {}(suites)".format(API.__suite_fname__), 1)
            nsuites = 0
            for suite in self._suites:
                nsuites = nsuites + 1
            # End for
            api.write("character(len=*), allocatable, intent(out) :: suites(:)", 2)
            api.write("\ninteger                                    :: sindex", 2)
            api.write("\nallocate(suites({}))".format(nsuites), 2)
            api.write("do sindex = 1, {}".format(nsuites), 2)
            for suite in self._suites:
                api.write("suites(sindex) = '{}'".format(suite.name), 3)
            # End for
            api.write("end do", 2)
            api.write("end subroutine {}".format(API.__suite_fname__), 1)
            # Write out the suite part list subroutine
            inargs = "suite_name, part_list, {errmsg}, {errflg}".format(errmsg=errmsg_name,
                                                                        errflg=errflg_name)
            api.write("\nsubroutine {}({})".format(API.__part_fname__, inargs), 1)
            api.write("character(len=*),              intent(in)  :: suite_name", 2)
            api.write("character(len=*), allocatable, intent(out) :: part_list(:)", 2)
            self._errmsg_var.write_def(api, 2, self)
            self._errflg_var.write_def(api, 2, self)
            api.write("\ninteger                                   :: pindex\n", 2)
            else_str = ''
            for suite in self._suites:
                api.write("{}if(trim(suite_name) == '{}') then".format(else_str, suite.name), 2)
                parts = suite.part_list()
                nparts = len(parts)
                api.write("allocate(part_list({}))\n".format(nparts), 3)
                api.write("do pindex = 1, {}".format(nparts), 3)
                for part in parts:
                    api.write("part_list(pindex) = '{}'".format((part)), 4)
                # End for
                api.write("end do", 3)
                else_str = 'else '
            # End for
            api.write("else", 2)
            api.write("{errmsg} = 'No suite named '//trim(suite_name)//' found'".format(errmsg=errmsg_name), 3)
            api.write("{errflg} = 1".format(errflg=errflg_name), 3)
            api.write("end if", 2)
            api.write("end subroutine {}".format(API.__part_fname__), 1)
            # Finish off the module
            api.write(API.__footer__.format(module=self.module), 0)
        # End with
        api_filenames.append(filename)
        return api_filenames

###############################################################################
if __name__ == "__main__":
    from parse_tools import initLog, setLogToNull
    logger = initLog('ccpp_suite')
    setLogToNull(logger)
    try:
        # First, run doctest
        import doctest
        doctest.testmod()
        frame_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        cpf = os.path.dirname(frame_root)
        kessler = os.path.join(cpf, 'cam_driver', 'suites',
                               'suite_cam_kessler_test_simple1.xml')
        if os.path.exists(kessler):
            foo = Suite(kessler, VarDictionary('foo'), logger)
        else:
            raise CCPPError("Cannot find test file, '{}'".format(kessler))
    except CCPPError as sa:
        print("{}".format(sa))
# No else
