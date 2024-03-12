#!/usr/bin/env python3
#

"""Classes and methods to create a Fortran suite-implementation file
to implement calls to a set of suites for a given host model."""

# Python library imports
import logging
import re
import xml.etree.ElementTree as ET
# CCPP framework imports
from ccpp_state_machine import CCPP_STATE_MACH, RUN_PHASE_NAME
from code_block import CodeBlock
from constituents import ConstituentVarDict
from framework_env import CCPPFrameworkEnv
from metavar import Var, VarDictionary, VarLoopSubst
from metavar import CCPP_CONSTANT_VARS, CCPP_LOOP_VAR_STDNAMES
from parse_tools import ParseContext, ParseSource, context_string
from parse_tools import ParseInternalError, CCPPError
from parse_tools import init_log, set_log_to_null
from var_props import is_horizontal_dimension, find_horizontal_dimension
from var_props import find_vertical_dimension
from var_props import VarCompatObj

# pylint: disable=too-many-lines

###############################################################################
# Module (global) variables
###############################################################################

_OBJ_LOC_RE = re.compile(r"(0x[0-9A-Fa-f]+)>")
_BLANK_DIMS_RE = re.compile(r"[(][:](,:)*[)]$")

# Source for internally generated variables.
_API_SOURCE_NAME = "CCPP_API"
# Use the constituent source type for consistency
_API_SUITE_VAR_NAME = ConstituentVarDict.constitutent_source_type()
_API_GROUP_VAR_NAME = "group"
_API_SCHEME_VAR_NAME = "scheme"
_API_LOCAL_VAR_NAME = "local"
_API_LOCAL_VAR_TYPES = [_API_LOCAL_VAR_NAME, _API_SUITE_VAR_NAME]
_API_CONTEXT = ParseContext(filename="ccpp_suite.py")
_API_SOURCE = ParseSource(_API_SOURCE_NAME, _API_SCHEME_VAR_NAME, _API_CONTEXT)
_API_LOCAL = ParseSource(_API_SOURCE_NAME, _API_LOCAL_VAR_NAME, _API_CONTEXT)
_API_TIMESPLIT_TAG = 'time_split'
_API_PROCESSSPLIT_TAG = 'process_split'
_API_LOGGING = init_log('ccpp_suite')
set_log_to_null(_API_LOGGING)
_API_DUMMY_RUN_ENV = CCPPFrameworkEnv(_API_LOGGING,
                                      ndict={'host_files':'',
                                             'scheme_files':'',
                                             'suites':''})

###############################################################################
def new_suite_object(item, context, parent, run_env):
###############################################################################
    "'Factory' method to create the appropriate suite object from XML"
    new_item = None
    if item.tag == 'subcycle':
        new_item = Subcycle(item, context, parent, run_env)
    elif item.tag == 'scheme':
        new_item = Scheme(item, context, parent, run_env)
    elif item.tag == _API_TIMESPLIT_TAG:
        new_item = TimeSplit(item, context, parent, run_env)
    else:
        emsg = "Unknown CCPP suite element type, '{}'"
        raise CCPPError(emsg.format(item.tag))
    # end if
    return new_item

###############################################################################

class CallList(VarDictionary):
    """A simple class to hold a routine's call list (dummy arguments)"""

    def __init__(self, name, run_env, routine=None):
        """Initialize this call list.
        <name> is the name of this dictionary.
        <routine> is a pointer to the routine for which this is a call list
        or None for a routine that is not a SuiteObject.
        """
        self.__routine = routine
        super().__init__(name, run_env)

    def add_vars(self, call_list, run_env, gen_unique=False):
        """Add new variables from another CallList (<call_list>)"""
        for var in call_list.variable_list():
            stdname = var.get_prop_value('standard_name')
            if stdname not in self:
                self.add_variable(var, run_env, gen_unique=gen_unique)
            # end if
        # end for

    def add_variable(self, newvar, run_env, exists_ok=False, gen_unique=False,
                     adjust_intent=False):
        """Add <newvar> as for VarDictionary but make sure that the variable
           has an intent with the default being intent(in).
        """
        # We really need an intent on a dummy argument
        if newvar.get_prop_value("intent") is None:
            subst_dict = {'intent' : 'in'}
            oldvar = newvar
            newvar = oldvar.clone(subst_dict, source_name=self.name,
                                  source_type=_API_GROUP_VAR_NAME,
                                  context=oldvar.context)
        # end if
        super().add_variable(newvar, run_env, exists_ok=exists_ok,
                             gen_unique=gen_unique, adjust_intent=adjust_intent)

    def call_string(self, cldicts=None, is_func_call=False, subname=None):
        """Return a dummy argument string for this call list.
        <cldict> may be a list of VarDictionary objects to search for
        local_names (default is to use self).
        <is_func_call> should be set to True to construct a call statement.
        If <is_func_call> is False, construct a subroutine dummy argument
        list.
        """
        arg_str = ""
        arg_sep = ""
        for var in self.variable_list():
            # Do not include constants
            stdname = var.get_prop_value('standard_name')
            if stdname not in CCPP_CONSTANT_VARS:
                # Find the dummy argument name
                dummy = var.get_prop_value('local_name')
                # Now, find the local variable name
                if cldicts is not None:
                    for cldict in cldicts:
                        dvar = cldict.find_variable(standard_name=stdname,
                                                    any_scope=False)
                        if dvar is not None:
                            break
                        # end if
                    # end for
                    if dvar is None:
                        if subname is not None:
                            errmsg = "{}: ".format(subname)
                        else:
                            errmsg = ""
                        # end if
                        errmsg += "'{}', not found in call list for '{}'"
                        clnames = [x.name for x in cldicts]
                        raise CCPPError(errmsg.format(stdname, clnames))
                    # end if
                    lname = dvar.get_prop_value('local_name')
                    # Optional variables in the caps are associated with 
                    # local pointers of <lname>_ptr
                    if dvar.get_prop_value('optional'):
                        lname = dummy+'_ptr'
                    # end if
                else:
                    cldict = None
                    aref = var.array_ref(local_name=dummy)
                    if aref is not None:
                        lname = aref.group(1)
                    else:
                        lname = dummy
                    # end if
                # end if
                if is_func_call:
                    if cldicts is not None:
                        use_dicts = cldicts
                    else:
                        use_dicts = [self]
                    # end if
                    run_phase = self.routine.run_phase()
                    # We only need dimensions for suite variables in run phase
                    need_dims = SuiteObject.is_suite_variable(dvar) and run_phase
                    vdims = var.call_dimstring(var_dicts=use_dicts,
                                               explicit_dims=need_dims,
                                               loop_subst=run_phase)
                    if _BLANK_DIMS_RE.match(vdims) is None:
                        lname = lname + vdims
                    # end if
                # end if
                if is_func_call:
                    arg_str += "{}{}={}".format(arg_sep, dummy, lname)
                else:
                    arg_str += "{}{}".format(arg_sep, lname)
                # end if
                arg_sep = ", "
            # end if
        # end for
        return arg_str

    @property
    def routine(self):
        """Return the routine for this call list (or None)"""
        return self.__routine

###############################################################################

class SuiteObject(VarDictionary):
    """Base class for all CCPP Suite objects (e.g., Scheme, Subcycle)
    SuiteObjects have an internal dictionary for variables created for
    execution of the SuiteObject. These variables will be allocated and
    managed at the Group level (unless cross-group usage or persistence
    requires handling at the Suite level).
    SuiteObjects also have a call list which is a list of variables which
    are passed to callable SuiteObjects (e.g., Scheme).
    """

    def __init__(self, name, context, parent, run_env,
                 active_call_list=False, variables=None, phase_type=None):
        # pylint: disable=too-many-arguments
        self.__name = name
        self.__context = context
        self.__parent = parent
        self.__run_env = run_env
        if active_call_list:
            self.__call_list = CallList(name + '_call_list', run_env,
                                        routine=self)
        else:
            self.__call_list = None
        # end if
        self.__parts = list()
        self.__needs_vertical = None
        self.__needs_horizontal = None
        self.__phase_type = phase_type
        # Initialize our dictionary
        super().__init__(self.name, run_env,
                         variables=variables, parent_dict=parent)

    def declarations(self):
        """Return a list of local variables to be declared in parent Group
        or Suite. By default, this list is the object's embedded VarDictionary.
        """
        return self.variable_list()

    def add_part(self, item, replace=False):
        """Add an object (e.g., Scheme, Subcycle) to this SuiteObject.
        If <item> needs to be in a VerticalLoop, look for an appropriate
        VerticalLoop object or create one.
        if <replace> is True, replace <item> in its current position in self.
        Note that if <item> is not to be inserted in a VerticalLoop,
        <replace> has no effect.
        """
        if replace:
            if item in self.__parts:
                index = self.__parts.index(item)
            else:
                emsg = 'Cannot replace {} in {}, not a member'
                raise ParseInternalError(emsg.format(item.name, self.name))
            # end if
        else:
            if item in self.__parts:
                emsg = 'Cannot add {} to {}, already a member'
                raise ParseInternalError(emsg.format(item.name, self.name))
            # end if
            index = len(self.__parts)
        # end if
        # Does this item need to be in a VerticalLoop?
        if item.needs_vertical is not None:
            iparent = item.parent
            if isinstance(self, VerticalLoop):
                # It is being added to a VerticalLoop, call it good
                pass
            elif isinstance(iparent, VerticalLoop):
                # Why are we doing this?
                emsg = ('Trying to add {} {} to {} {} but it is already '
                        'in VerticalLoop {}')
                raise ParseInternalError(emsg.format(item.__class__.__name__,
                                                     item.name,
                                                     self.__class__.__name__,
                                                     self.name, iparent.name))
            else:
                pitem = iparent.part(-1, error=False)
                added = False
                if isinstance(pitem, VerticalLoop):
                    # Can we attach item to this loop?
                    if pitem.dimension_name == item.needs_vertical:
                        pitem.add_part(item)
                        if replace:
                            self.remove_part(index)
                        # end if (no else, we already added it)
                        added = True
                    # end if
                # end if
                if not added:
                    # Need to add item to a new VerticalLoop
                    # We are in the process of providing the vertical coord
                    vert_index = item.needs_vertical
                    item.needs_vertical = None
                    new_vl = VerticalLoop(vert_index, self.__context,
                                          self, self.run_env, items=[item])
                    if replace:
                        self.remove_part(index)
                    # end if (no else, adding the loop below)
                    self.__parts.insert(index, new_vl)
                    item.reset_parent(new_vl)
                # end if
            # end if
        else:
            # Just add <item>
            self.__parts.insert(index, item)
            item.reset_parent(self)
        # end if

    def remove_part(self, index):
        """Remove the SuiteObject part at index"""
        plen = len(self.__parts)
        if (0 <= index < plen) or (abs(index) <= plen):
            del self.__parts[index]
        else:
            errmsg = "Invalid index for remove_part, {}, ".format(index)
            if plen > 0:
                errmsg += "SuiteObject only has {} parts".format(plen)
            else:
                errmsg += "SuiteObject only has no parts"
            # end if
            raise ParseInternalError(errmsg, context=self.__context)
        # end if

    def schemes(self):
        """Return a flattened list of schemes for this SuiteObject"""
        schemes = list()
        for item in self.__parts:
            schemes.extend(item.schemes())
        # end for
        return schemes

    def move_part(self, part, source_object, loc=-1):
        """Operator to move <part> from <source_object> to <self>.
        If <loc> is -1, <part> is appended to <self>,
        otherwise, <part> is inserted at <loc>.
        """
        if part in source_object.parts:
            # Sanitize loc
            try:
                iloc = int(loc)
            except ValueError:
                errmsg = "Invalid loc value for move_part, {}".format(loc)
                raise ParseInternalError(errmsg, context=self.__context)
            # end try
            if iloc == -1:
                self.__parts.append(part)
            else:
                self.__parts.insert(iloc, part)
            # end if
            index = source_object.index(part)
            source_object.remove_part(index)
            # <part> now has a new parent
            part.reset_parent(self)

    def reset_parent(self, new_parent):
        """Reset the parent of this SuiteObject (which has been moved)"""
        self.__parent = new_parent

    def phase(self):
        """Return the CCPP state phase_type for this SuiteObject"""
        trans = self.phase_type
        if trans is None:
            if self.parent is not None:
                trans = self.parent.phase()
            else:
                trans = False
            # end if
        # end if
        return trans

    def run_phase(self):
        """Return True iff this SuiteObject is in a run phase group"""
        return self.phase() == RUN_PHASE_NAME

    def timestep_phase(self):
        '''Return True iff this SuiteObject is in a timestep initial or
        timestep final phase group'''
        phase = self.phase()
        return (phase is not None) and ('timestep' in phase)

    def register_action(self, vaction):
        """Register (i.e., save information for processing during write stage)
        <vaction> and return True or pass <vaction> up to the parent of
        <self>. Return True if any level registers <vaction>, False otherwise.
        The base class will not register any action, it must be registered in
        an override of this method.
        """
        if self.parent is not None:
            return self.parent.register_action(vaction)
        # end if
        return False

    @classmethod
    def is_suite_variable(cls, var):
        """Return True iff <var> belongs to our Suite"""
        return var and (var.source.ptype == _API_SUITE_VAR_NAME)

    def is_local_variable(self, var):
        """Return the local variable matching <var> if one is found belonging
        to this object or any of its SuiteObject parents."""
        stdname = var.get_prop_value('standard_name')
        lvar = None
        obj = self
        while (not lvar) and (obj is not None) and isinstance(obj, SuiteObject):
            lvar = obj.find_variable(standard_name=stdname, any_scope=False,
                                     search_call_list=False)
            if not lvar:
                obj = obj.parent
            # end if
        # end while
        return lvar

    def add_call_list_variable(self, newvar, exists_ok=False,
                               gen_unique=False, subst_dict=None):
        """Add <newvar> to this SuiteObject's call_list. If this SuiteObject
           does not have a call list, recursively try the SuiteObject's parent
        If <subst_dict> is not None, create a clone using that as a dictionary
           of substitutions.
        Do not add <newvar> if it exists as a local variable.
        Do not add <newvar> if it is a suite variable"""
        stdname = newvar.get_prop_value('standard_name')
        if self.parent:
            pvar = self.parent.find_variable(standard_name=stdname,
                                             source_var=newvar,
                                             any_scope=False)
        else:
            pvar = None
        # end if
        if SuiteObject.is_suite_variable(pvar):
            pass # Do not add suite variable to a call list
        elif self.is_local_variable(newvar):
            pass # Do not add to call list, it is owned by a SuiteObject
        elif self.call_list is not None:
            if (stdname in CCPP_LOOP_VAR_STDNAMES) and (not self.run_phase()):
                errmsg = 'Attempting to use loop variable {} in {} phase'
                raise CCPPError(errmsg.format(stdname, self.phase()))
            # end if
            # Do we need a clone?
            if isinstance(self, Group):
                stype = _API_GROUP_VAR_NAME
            else:
                stype = None
            # end if
            if stype or subst_dict:
                oldvar = newvar
                if subst_dict is None:
                    subst_dict = {}
                # end if
                # Make sure that this variable has an intent
                if ((oldvar.get_prop_value("intent") is None) and
                    ("intent" not in subst_dict)):
                    subst_dict["intent"] = "in"
                # end if
                newvar = oldvar.clone(subst_dict, source_name=self.name,
                                      source_type=stype, context=self.context)
            # end if
            self.call_list.add_variable(newvar, self.run_env,
                                        exists_ok=exists_ok,
                                        gen_unique=gen_unique,
                                        adjust_intent=True)
            # We need to make sure that this variable's dimensions are available
            for vardim in newvar.get_dim_stdnames(include_constants=False):
                dvar = self.find_variable(standard_name=vardim,
                                          any_scope=True)
                if dvar is None:
                    emsg = "{}: Could not find dimension {} in {}"
                    raise ParseInternalError(emsg.format(self.name,
                                                         vardim, stdname))
                # end if
        elif self.parent is None:
            errmsg = 'No call_list found for {}'.format(newvar)
            raise ParseInternalError(errmsg)
        elif pvar:
            # Check for call list incompatibility
            if pvar is not None:
                compat, reason = pvar.compatible(newvar, self.run_env)
                if not compat:
                    emsg = 'Attempt to add incompatible variable to call list:'
                    emsg += '\n{} from {} is not compatible with {} from {}'
                    nlreason = newvar.get_prop_value(reason)
                    plreason = pvar.get_prop_value(reason)
                    emsg += '\nreason = {} ({} != {})'.format(reason,
                                                              nlreason,
                                                              plreason)
                    nlname = newvar.get_prop_value('local_name')
                    plname = pvar.get_prop_value('local_name')
                    raise CCPPError(emsg.format(nlname, newvar.source.name,
                                                plname, pvar.source.name))
                # end if
            # end if (no else, variable already in call list)
        else:
            self.parent.add_call_list_variable(newvar, exists_ok=exists_ok,
                                               gen_unique=gen_unique,
                                               subst_dict=subst_dict)
        # end if

    def add_variable_to_call_tree(self, var, vmatch=None, subst_dict=None):
        """Add <var> to <self>'s call_list (or a parent if <self> does not
              have an active call_list).
        If <vmatch> is not None, also add the loop substitution variables
           which must be present.
        If <subst_dict> is not None, create a clone using that as a dictionary
           of substitutions.
        """
        found_dims = False
        if var is not None:
            self.add_call_list_variable(var, exists_ok=True,
                                        gen_unique=True, subst_dict=subst_dict)
            found_dims = True
        # end if
        if vmatch is not None:
            svars = vmatch.has_subst(self, any_scope=True)
            if svars is None:
                found_dims = False
            else:
                found_dims = True
                for svar in svars:
                    self.add_call_list_variable(svar, exists_ok=True)
                # end for
                # Register the action (probably at Group level)
                self.register_action(vmatch)
            # end if
        # end if
        return found_dims

    def vert_dim_match(self, vloop_subst):
        """If self is or is a part of a VerticalLoop object for
        the substitute index for <vloop_subst>, return the substitute
        loop index standard name, otherwise, return None.
        """
        dim_match = None
        parent = self
        if len(vloop_subst.required_stdnames) != 1:
            errmsg = 'vert_dim_match can only handle one substitute index'
            raise ParseInternalError(errmsg)
        # end if
        index_dim = vloop_subst.required_stdnames[0]
        while parent is not None:
            if isinstance(parent, VerticalLoop) and (parent.name == index_dim):
                dim_match = index_dim
                break
            # end if
            parent = parent.parent
        # end for
        return dim_match

    def horiz_dim_match(self, ndim, hdim, nloop_subst):
        """Find a match between <ndim> and <hdim>, if they are both
        horizontal dimensions.
        If <ndim> == <hdim>, return <ndim>.
        If <nloop_subst> is not None and its required standard names exist
        in our extended dictionary, return them.
        Otherwise, return None.
        NB: Loop substitutions are only allowed during the run phase but in
            other phases, horizontal_dimension and horizontal_loop_extent
            are the same.
        """
        dim_match = None
        nis_hdim = is_horizontal_dimension(ndim)
        his_hdim = is_horizontal_dimension(hdim)
        if nis_hdim and his_hdim:
            if ndim == hdim:
                dim_match = ndim
            elif self.run_phase() and (nloop_subst is not None):
                svars = nloop_subst.has_subst(self, any_scope=True)
                match = svars is not None
                if match:
                    if isinstance(self, Scheme):
                        obj = self.parent
                    else:
                        obj = self
                    # end if
                    for svar in svars:
                        obj.add_call_list_variable(svar, exists_ok=True)
                    # end for
                    dim_match = ':'.join(nloop_subst.required_stdnames)
                # end if
            elif not self.run_phase():
                if ((hdim == 'ccpp_constant_one:horizontal_dimension') and
                    (ndim == 'ccpp_constant_one:horizontal_loop_extent')):
                    dim_match = hdim
                elif ((hdim == 'ccpp_constant_one:horizontal_dimension') and
                      (ndim == 'horizontal_loop_begin:horizontal_loop_end')):
                    dim_match = hdim
                # end if (no else, there is no non-run-phase match)
            # end if (no else, there is no match)
        # end if (no else, there is no match)
        return dim_match

    @staticmethod
    def dim_match(need_dim, have_dim):
        """Test whether <need_dim> matches <have_dim>.
        If they match, return the matching dimension (which may be
        modified by, e.g., a loop substitution).
        If they do not match, return None.
        """
        match = None
        # First, try for all the marbles
        if need_dim == have_dim:
            match = need_dim
        # end if
        # Is one side missing a one start?
        if not match:
            ndims = need_dim.split(':')
            hdims = have_dim.split(':')
            if len(ndims) > len(hdims):
                if ndims[0].lower == 'ccpp_constant_one':
                    ndims = ndims[1:]
                elif hdims[0].lower == 'ccpp_constant_one':
                    hdims = hdims[1:]
                # end if (no else)
                # Last try
                match = ndims == hdims
            # end if
        # end if

        return match

    def match_dimensions(self, need_dims, have_dims):
        """Compare dimensions between <need_dims> and <have_dims>.
        Return 6 items:
        1) Return True if all dims match.
           If <have_dims> has a vertical dimension and <need_dims> does not
           but all other dimensions match, return False but include the
           missing dimension index as the third return value.
        2) Return <need_dims> modified, if necessary to
           reflect the available limits.
        3) Return have_dims modified, if necessary to reflect
           any loop substitutions. If no substitutions, return None
           This is done so that the correct dimensions are used in the host cap.
        4) Return the name of the missing vertical index, or None
        5) Return a permutation array if the dimension ordering is
        different (or None if the ordering is the same). Each element of the
        permutation array is the index in <have_dims> for that dimension of
        <need_dims>.
        6) Finally, return a 'reason' string. If match (first return value) is
        False, this string will contain information about the reason for
        the match failure.
        >>> SuiteObject('foo', _API_CONTEXT, None, _API_DUMMY_RUN_ENV).match_dimensions(['horizontal_loop_extent'], ['horizontal_loop_extent'])
        (True, ['horizontal_loop_extent'], ['horizontal_loop_extent'], None, None, '')
        >>> SuiteObject('foo', _API_CONTEXT,None,_API_DUMMY_RUN_ENV,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL,_API_DUMMY_RUN_ENV),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV)],active_call_list=True,phase_type='initialize').match_dimensions(['ccpp_constant_one:horizontal_loop_extent'], ['ccpp_constant_one:horizontal_dimension'])
        (True, ['ccpp_constant_one:horizontal_dimension'], ['ccpp_constant_one:horizontal_dimension'], None, None, '')
        >>> SuiteObject('foo', _API_CONTEXT,None,_API_DUMMY_RUN_ENV,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV)],active_call_list=True,phase_type=RUN_PHASE_NAME).match_dimensions(['ccpp_constant_one:horizontal_loop_extent'], ['horizontal_loop_begin:horizontal_loop_end'])
        (True, ['horizontal_loop_begin:horizontal_loop_end'], ['horizontal_loop_begin:horizontal_loop_end'], None, None, '')
        >>> SuiteObject('foo', _API_CONTEXT,None,_API_DUMMY_RUN_ENV,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV),Var({'local_name':'lev','standard_name':'vertical_layer_dimension','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV)],active_call_list=True,phase_type=RUN_PHASE_NAME).match_dimensions(['ccpp_constant_one:horizontal_loop_extent'], ['horizontal_loop_begin:horizontal_loop_end','ccpp_constant_one:vertical_layer_dimension'])
        (False, ['horizontal_loop_begin:horizontal_loop_end', 'vertical_layer_index'], ['horizontal_loop_begin:horizontal_loop_end', 'ccpp_constant_one:vertical_layer_dimension'], 'vertical_layer_index', None, 'missing vertical dimension')
        >>> SuiteObject('foo', _API_CONTEXT,None,_API_DUMMY_RUN_ENV,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV),Var({'local_name':'lev','standard_name':'vertical_layer_dimension','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV)],active_call_list=True,phase_type=RUN_PHASE_NAME).match_dimensions(['ccpp_constant_one:horizontal_loop_extent','ccpp_constant_one:vertical_layer_dimension'], ['horizontal_loop_begin:horizontal_loop_end','ccpp_constant_one:vertical_layer_dimension'])
        (True, ['horizontal_loop_begin:horizontal_loop_end', 'ccpp_constant_one:vertical_layer_dimension'], ['horizontal_loop_begin:horizontal_loop_end', 'ccpp_constant_one:vertical_layer_dimension'], None, None, '')
        >>> SuiteObject('foo', _API_CONTEXT,None,_API_DUMMY_RUN_ENV,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV),Var({'local_name':'lev','standard_name':'vertical_layer_dimension','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL, _API_DUMMY_RUN_ENV)],active_call_list=True,phase_type=RUN_PHASE_NAME).match_dimensions(['ccpp_constant_one:horizontal_loop_extent','ccpp_constant_one:vertical_layer_dimension'], ['ccpp_constant_one:vertical_layer_dimension','horizontal_loop_begin:horizontal_loop_end'])
        (True, ['horizontal_loop_begin:horizontal_loop_end', 'ccpp_constant_one:vertical_layer_dimension'], ['ccpp_constant_one:vertical_layer_dimension', 'horizontal_loop_begin:horizontal_loop_end'], None, [1, 0], '')
        """
        new_need_dims = []
        new_have_dims = list(have_dims)
        perm = []
        match = True
        missing_vert_dim = None
        reason = ''
        nlen = len(need_dims)
        hlen = len(have_dims)
        _, nvdim_index = find_vertical_dimension(need_dims)
        _, hvdim_index = find_vertical_dimension(have_dims)
        _, nhdim_index = find_horizontal_dimension(need_dims)
        _, hhdim_index = find_horizontal_dimension(have_dims)
        if hhdim_index < 0 <= nhdim_index:
            match = False
            nlen = 0 # To skip logic below
            hlen = 0 # To skip logic below
            reason = '{hname}{hctx} is missing a horizontal dimension '
            reason += 'required by {nname}{nctx}'
        # end if
        for nindex in range(nlen):
            neddim = need_dims[nindex]
            if nindex == nhdim_index:
                # Look for a horizontal dimension match
                vmatch = VarDictionary.loop_var_match(neddim)
                hmatch = self.horiz_dim_match(neddim, have_dims[hhdim_index],
                                              vmatch)
                if hmatch:
                    perm.append(hhdim_index)
                    new_need_dims.append(hmatch)
                    new_have_dims[hhdim_index] = hmatch
                    found_ndim = True
                else:
                    found_ndim = False
                # end if
            else:
                # Find the first dimension in have_dims that matches neddim
                found_ndim = False
                if nvdim_index < 0 <= hvdim_index:
                    skip = hvdim_index
                else:
                    skip = -1
                # end if
                hdim_indices = [x for x in range(hlen)
                                if (x not in perm) and (x != skip)]
                for hindex in hdim_indices:
                    if (hindex != hvdim_index) or (nvdim_index >= 0):
                        hmatch = self.dim_match(neddim, have_dims[hindex])
                        if hmatch:
                            perm.append(hindex)
                            new_need_dims.append(hmatch)
                            new_have_dims[hindex] = hmatch
                            found_ndim = True
                            break
                        # end if
                    # end if
                # end if
            # end for
            if not found_ndim:
                match = False
                reason = 'Could not find dimension, ' + neddim + ', in '
                reason += '{hname}{hctx}. Needed by {nname}{nctx}'
                break
            # end if (no else, we are still okay)
        # end for
        # Find a missing vertical dimension index, if necessary
        if nvdim_index < 0 <= hvdim_index:
            # We need to make a substitution for the vertical
            # coordinate in have_dims
            vvmatch = VarDictionary.loop_var_match(have_dims[hvdim_index])
            if vvmatch:
                vmatch_dims = ':'.join(vvmatch.required_stdnames)
                # See if the missing vertical dimensions exist
                missing_vert_dim = None
                for mstdname in vvmatch.required_stdnames:
                    mvdim = self.find_variable(standard_name=mstdname,
                                               any_scope=True)
                    if not mvdim:
                        missing_vert_dim = vmatch_dims
                        match = False # Should trigger vertical loop action
                        reason = 'missing vertical dimension'
                        break
                    # end if
                # end for
                # While we have a missing vertical dimension which has been
                # created, do NOT enter the substitution into have_dims.
                # The supplied variable still has a vertical dimension.
                # On the other hand, we *do* need to add the new vertical
                # loop index to new_need_dims. Try to put it in the correct
                # place for easy calling from the existing variable.
                # Also update perm to match the array access
                if hvdim_index < len(new_need_dims):
                    # Insert the vertical loop dimension
                    if hvdim_index > 0:
                        before = new_need_dims[0:hvdim_index]
                        perm_before = perm[0:hvdim_index]
                    else:
                        before = []
                        perm_before = []
                    # end if
                    after = new_need_dims[hvdim_index:]
                    new_need_dims = before + [vmatch_dims] + after
                    perm = perm_before + [hvdim_index] + perm[hvdim_index:]
                else:
                    new_need_dims.append(vmatch_dims)
                    perm.append(hvdim_index)
                # end if
            else:
                emsg = "Unknown vertical dimension dimension, '{}'"
                raise CCPPError(emsg.format(have_dims[hvdim_index]))
            # end if
        else:
            missing_vert_dim = None
        # end if
        perm_test = list(range(hlen))
        # If no permutation is found, reset to None
        if perm == perm_test:
            perm = None
        elif (not match) and (missing_vert_dim is None):
            perm = None
        # end if (else, return perm as is)
        if new_have_dims == have_dims:
            have_dims = None # Do not make any substitutions
        # end if
        return match, new_need_dims, new_have_dims, missing_vert_dim, perm, reason

    def find_variable(self, standard_name=None, source_var=None,
                      any_scope=True, clone=None,
                      search_call_list=False, loop_subst=False):
        """Find a matching variable to <var>, create a local clone (if
        <clone> is True), or return None.
        First search the SuiteObject's internal dictionary, then its
        call list (unless <skip_call_list> is True, then any parent
        dictionary (if <any_scope> is True).
        <var> can be a Var object or a standard_name string.
        <loop_subst> is not used by this version of <find_variable>.
        """
        # First, search our local dictionary
        if standard_name is None:
            if source_var is None:
                emsg = "One of <standard_name> or <source_var> must be passed."
                raise ParseInternalError(emsg)
            # end if
            standard_name = source_var.get_prop_value('standard_name')
        elif source_var is not None:
            stest = source_var.get_prop_value('standard_name')
            if stest != standard_name:
                emsg = ("<standard_name> and <source_var> must match if " +
                        "both are passed.")
                raise ParseInternalError(emsg)
            # end if
        # end if
        scl = search_call_list
        stdname = standard_name
        # Don't clone yet, might find the variable further down
        found_var = super().find_variable(standard_name=stdname,
                                          source_var=source_var,
                                          any_scope=False, clone=None,
                                          search_call_list=scl,
                                          loop_subst=loop_subst)
        if (not found_var) and (self.call_list is not None) and scl:
            # Don't clone yet, might find the variable further down
            found_var = self.call_list.find_variable(standard_name=stdname,
                                                     source_var=source_var,
                                                     any_scope=False,
                                                     clone=None,
                                                     search_call_list=scl,
                                                     loop_subst=loop_subst)
        # end if
        loop_okay = VarDictionary.loop_var_okay(stdname, self.run_phase())
        if not loop_okay:
            loop_subst = False
        # end if
        if (found_var is None) and any_scope and (self.parent is not None):
            # We do not have the variable, look to parents.
            found_var = self.parent.find_variable(standard_name=stdname,
                                                  source_var=source_var,
                                                  any_scope=True,
                                                  clone=clone,
                                                  search_call_list=scl,
                                                  loop_subst=loop_subst)
        # end if
        return found_var

    def match_variable(self, var, run_env):
        """Try to find a source for <var> in this SuiteObject's dictionary
        tree. Several items are returned:
        found_var: True if a match was found
        vert_dim: The vertical dimension in <var>, or None
        call_dims: How this variable should be called (or None if no match)
        missing_vert: Vertical dim in parent but not in <var>
        perm: Permutation (XXgoldyXX: Not yet implemented)
        """
        vstdname = var.get_prop_value('standard_name')
        vdims    = var.get_dimensions()
        if (not vdims) and self.run_phase():
            vmatch = VarDictionary.loop_var_match(vstdname)
        else:
            vmatch = None
        # end if
        
        found_var = False
        missing_vert = None
        new_vdims = list()
        var_vdim = var.has_vertical_dimension(dims=vdims)
        compat_obj = None
        # Does this variable exist in the calling tree?
        dict_var = self.find_variable(source_var=var, any_scope=True)
        if dict_var is None:
            # No existing variable but add loop var match to call tree
            found_var = self.parent.add_variable_to_call_tree(dict_var,
                                                              vmatch=vmatch)
            new_vdims = vdims
        elif dict_var.source.ptype in _API_LOCAL_VAR_TYPES:
            # We cannot change the dimensions of locally-declared variables
            # Using a loop substitution is invalid because the loop variable
            # value has not yet been set.
            # Therefore, we have to use the declaration dimensions in the call.
            found_var = True
            new_vdims = dict_var.get_dimensions()
        else:
            # Check dimensions
            dict_dims = dict_var.get_dimensions()
            if vdims:
                args = self.parent.match_dimensions(vdims, dict_dims)
                match, new_vdims, new_dict_dims, missing_vert, perm, err = args
                if perm is not None:
                    errmsg = "Permuted indices are not yet supported"
                    lname = var.get_prop_value('local_name')
                    dstr = ', '.join(vdims)
                    ctx = context_string(var.context)
                    errmsg += ", var = {}({}){}".format(lname, dstr, ctx)
                    raise CCPPError(errmsg)
                # end if
            else:
                new_vdims = list()
                new_dict_dims = dict_dims
                match = True
            # end if
            # Create compatability object, containing any necessary forward/reverse 
            # transforms from <var> and <dict_var>
            compat_obj = var.compatible(dict_var, run_env)
            # If variable is defined as "inactive" by the host, ensure that
            # this variable is declared as "optional" by the scheme. If
            # not satisfied, return error.
            host_var_active     = dict_var.get_prop_value('active')
            scheme_var_optional = var.get_prop_value('optional')
            if (not scheme_var_optional and host_var_active.lower() != '.true.'):
                errmsg = "Non optional scheme arguments for conditionally allocatable variables"
                sname  = dict_var.get_prop_value('standard_name')
                errmsg += ", {}".format(sname)
                raise CCPPError(errmsg)
            # end if
            # Add the variable to the parent call tree
            if dict_dims == new_dict_dims:
                sdict = {}
            else:
                sdict = {'dimensions':new_dict_dims}
            # end if
            found_var = self.parent.add_variable_to_call_tree(var,
                                                              subst_dict=sdict)
            if not match:
                found_var = False
                if not missing_vert:
                    nctx = context_string(var.context)
                    nname = var.get_prop_value('local_name')
                    hctx = context_string(dict_var.context)
                    hname = dict_var.get_prop_value('local_name')
                    raise CCPPError(err.format(nname=nname, nctx=nctx,
                                               hname=hname, hctx=hctx))
                    # end if
                # end if
            # end if
        # end if
        return found_var, dict_var, var_vdim, new_vdims, missing_vert, compat_obj

    def in_process_split(self):
        """Find out if we are in a process-split region"""
        proc_split = False
        obj = self
        while obj is not None:
            if isinstance(obj, ProcessSplit):
                proc_split = True
                break
            # end if
            if isinstance(obj, TimeSplit):
                break
            # end if (other object types do not change status)
            obj = obj.parent
        # end while
        return proc_split

    def part(self, index, error=True):
        """Return one of this SuiteObject's parts raise an exception, or,
        if <error> is False, just return None"""
        plen = len(self.__parts)
        if (0 <= index < plen) or (abs(index) <= plen):
            return self.__parts[index]
        # end if
        if error:
            errmsg = 'No part {} in {} {}'.format(index,
                                                  self.__class__.__name__,
                                                  self.name)
            raise ParseInternalError(errmsg)
        # end if
        return None

    def has_item(self, item_name):
        """Return True iff item, <item_name>, is already in this SuiteObject"""
        has = False
        for item in self.__parts:
            if item.name == item_name:
                has = True
            else:
                has = item.has_item(item_name)
            # end if
            if has:
                break
            # end if
        # end for
        return has

    @property
    def name(self):
        """Return the name of the element"""
        return self.__name

    @name.setter
    def name(self, value):
        """Set the name of the element if it has not been set"""
        if self.__name is None:
            self.__name = value
        else:
            errmsg = 'Attempt to change name of {} to {}'
            raise ParseInternalError(errmsg.format(self, value))
        # end if

    @property
    def parent(self):
        """This SuiteObject's parent (or none)"""
        return self.__parent

    @property
    def call_list(self):
        """Return the SuiteObject's call_list"""
        return self.__call_list

    @property
    def phase_type(self):
        """Return the phase_type of this suite_object"""
        return self.__phase_type

    @property
    def parts(self):
        """Return a copy the component parts of this SuiteObject.
        Returning a copy allows for the part list to be changed during
        processing of the return value"""
        return self.__parts[:]

    @property
    def needs_vertical(self):
        """Return the vertical dimension this SuiteObject is missing or None"""
        return self.__needs_vertical

    @needs_vertical.setter
    def needs_vertical(self, value):
        """Reset the missing vertical dimension of this SuiteObject"""
        if value is None:
            self.__needs_vertical = value
        elif self.__needs_vertical is not None:
            if self.__needs_vertical != value:
                errmsg = ('Attempt to change missing vertical dimension '
                          'from {} to {}')
                raise ParseInternalError(errmsg.format(self.__needs_vertical,
                                                       value))
            # end if (no else, value is already correct)
        else:
            self.__needs_vertical = value
        # end if

    @property
    def context(self):
        """Return the context of this SuiteObject"""
        return self.__context

    @property
    def run_env(self):
        """Return the CCPPFrameworkEnv runtime object for this SuiteObject"""
        return self.__run_env

    def __repr__(self):
        """Create a unique readable string for this Object"""
        so_repr = super().__repr__()
        olmatch = _OBJ_LOC_RE.search(so_repr)
        if olmatch is not None:
            loc = ' at {}'.format(olmatch.group(1))
        else:
            loc = ""
        # end if
        return '<{} {}{}>'.format(self.__class__.__name__, self.name, loc)

    def __format__(self, spec):
        """Return a string representing the SuiteObject, including its children.
        <spec> is used between subitems.
        <ind_level> is the indent level for multi-line output.
        """
        if spec:
            sep = spec[0]
        else:
            sep = '\n'
        # end if
        try:
            ind_level = int(spec[1:])
        except (ValueError, IndexError):
            ind_level = 0
        # end try
        if sep == '\n':
            indent = "  "
        else:
            indent = ""
        # end if
        if self.name == self.__class__.__name__:
            # This object does not have separate name
            nstr = self.name
        else:
            nstr = "{}: {}".format(self.__class__.__name__, self.name)
        # end if
        output = "{}<{}>".format(indent*ind_level, nstr)
        subspec = "{}{}".format(sep, ind_level + 1)
        substr = "{o}{s}{p:" + subspec + "}"
        subout = ""
        for part in self.parts:
            subout = substr.format(o=subout, s=sep, p=part)
        # end for
        if subout:
            output = "{}{}{}{}</{}>".format(output, subout, sep,
                                            indent*ind_level,
                                            self.__class__.__name__)
        else:
            output = "{}</{}>".format(output, self.__class__.__name__)
        # end if
        return output

###############################################################################

class Scheme(SuiteObject):
    """A single scheme in a suite (e.g., init method)"""

    def __init__(self, scheme_xml, context, parent, run_env):
        """Initialize this physics Scheme"""
        name = scheme_xml.text
        self.__subroutine_name = None
        self.__context = context
        self.__version = scheme_xml.get('version', None)
        self.__lib = scheme_xml.get('lib', None)
        self.__has_vertical_dimension = False
        self.__group = None
        self.__var_debug_checks = list()
        self.__forward_transforms = list()
        self.__reverse_transforms = list()
        self._no_run_phase = False
        self.__optional_vars = list()
        super().__init__(name, context, parent, run_env, active_call_list=True)

    def update_group_call_list_variable(self, var):
        """If <var> is in our group's call list, update its intent.
        Add <var> to our group's call list unless:
        - <var> is in our group's call list
        - <var> is in our group's dictionary,
        - <var> is a suite variable"""
        stdname = var.get_prop_value('standard_name')
        my_group = self.__group
        gvar = my_group.call_list.find_variable(standard_name=stdname,
                                                any_scope=False)
        if gvar:
            gvar.adjust_intent(var)
        else:
            gvar = my_group.find_variable(standard_name=stdname,
                                          any_scope=False)
            if gvar is None:
                # Check for suite variable
                gvar = my_group.find_variable(standard_name=stdname,
                                              any_scope=True)
                if gvar and (not SuiteObject.is_suite_variable(gvar)):
                    gvar = None
                # end if
            if gvar is None:
                my_group.add_call_list_variable(var)
            # end if
        # end if

    def is_local_variable(self, var):
        """Return None as we never consider <var> to be in our local
        dictionary.
        This is an override of the SuiteObject version"""
        return None

    def analyze(self, phase, group, scheme_library, suite_vars, level):
        """Analyze the scheme's interface to prepare for writing"""
        self.__group = group
        my_header = None
        if self.name in scheme_library:
            func = scheme_library[self.name]
            if phase in func:
                my_header = func[phase]
                self.__subroutine_name = my_header.title
            else:
                self._no_run_phase = True
                return set()
            # end if
        else:
            estr = 'No schemes found for {}'
            raise ParseInternalError(estr.format(self.name),
                                     context=self.__context)
        # end if
        if my_header is None:
            estr = 'No {} header found for scheme, {}'
            raise ParseInternalError(estr.format(phase, self.name),
                                     context=self.__context)
        # end if
        if my_header.module is None:
            estr = 'No module found for subroutine, {}'
            raise ParseInternalError(estr.format(self.subroutine_name),
                                     context=self.__context)
        # end if
        scheme_mods = set()
        scheme_mods.add((my_header.module, self.subroutine_name))
        for var in my_header.variable_list():
            vstdname = var.get_prop_value('standard_name')
            def_val = var.get_prop_value('default_value')
            vdims = var.get_dimensions()
            vintent = var.get_prop_value('intent')
            args = self.match_variable(var, self.run_env)
            found, dict_var, vert_dim, new_dims, missing_vert, compat_obj = args
            if found:
                if self.__group.run_env.debug:
                    # Add variable allocation checks for group, suite and host variables
                    if dict_var:
                        self.add_var_debug_check(dict_var)
                # end if
                if not self.has_vertical_dim:
                    self.__has_vertical_dimension = vert_dim is not None
                # end if
                # We have a match, make sure var is in call list
                if new_dims == vdims:
                    self.add_call_list_variable(var, exists_ok=True)
                    self.update_group_call_list_variable(var)
                else:
                    subst_dict = {'dimensions':new_dims}
                    clone = var.clone(subst_dict)
                    self.add_call_list_variable(clone, exists_ok=True)
                    self.update_group_call_list_variable(clone)
                # end if
            else:
                if missing_vert is not None:
                    # This Scheme needs to be in a VerticalLoop
                    self.needs_vertical = missing_vert
                    break # Deal with this and come back
                # end if
                if vintent == 'out':
                    if self.__group is None:
                        errmsg = 'Group not defined for {}'.format(self.name)
                        raise ParseInternalError(errmsg)
                    # end if
                    # The Group will manage this variable
                    self.__group.manage_variable(var)
                    self.add_call_list_variable(var)
                elif def_val and (vintent != 'out'):
                    if self.__group is None:
                        errmsg = 'Group not defined for {}'.format(self.name)
                        raise ParseInternalError(errmsg)
                    # end if
                    # The Group will manage this variable
                    self.__group.manage_variable(var)
                    # We still need it in our call list (the group uses a clone)
                    self.add_call_list_variable(var)
                else:
                    errmsg = 'Input argument for {}, {}, not found.'
                    if self.find_variable(source_var=var) is not None:
                        # The variable exists, maybe it is dim mismatch
                        lname = var.get_prop_value('local_name')
                        emsg = '\nCheck for dimension mismatch in {}'
                        errmsg += emsg.format(lname)
                    # end if
                    if ((not self.run_phase()) and
                        (vstdname in CCPP_LOOP_VAR_STDNAMES)):
                        emsg = '\nLoop variables not allowed in {} phase.'
                        errmsg += emsg.format(self.phase())
                    # end if
                    raise CCPPError(errmsg.format(self.subroutine_name,
                                                  vstdname))
                # end if
            # end if
            # Are there any forward/reverse transforms for this variable?
            has_transform = False
            if compat_obj is not None and (compat_obj.has_vert_transforms or
                                           compat_obj.has_unit_transforms or
                                           compat_obj.has_kind_transforms):
                self.add_var_transform(var, compat_obj, vert_dim)
                has_transform = True
            # end if

            # Is this a conditionally allocated variable?
            # If so, declare localpointer varaible. This is needed to
            # pass inactive (not present) status through the caps.
            if var.get_prop_value('optional'):
                newvar_ptr = var.clone(var.get_prop_value('local_name')+'_ptr')
                self.__optional_vars.append([dict_var, var, newvar_ptr, has_transform])
            # end if

        # end for
        if self.needs_vertical is not None:
            self.parent.add_part(self, replace=True) # Should add a vloop
            if isinstance(self.parent, VerticalLoop):
                # Restart the loop analysis
                scheme_mods = self.parent.analyze(phase, group, scheme_library,
                                                  suite_vars, level)
            # end if
        # end if
        return scheme_mods

    def add_var_debug_check(self, var):
        """Add a debug check for a given variable var (host model variable,
        suite variable or group module variable) for this scheme.
        Return the variable and an associated dummy variable that is
        managed by the group subroutine that calls the scheme, and
        which is used to assign the scalar or the lower and upper bounds
        of the array to if the intent is 'inout' or 'out'.
        """
        # Get the basic attributes that decide whether we need
        # to check the variable when we write the group
        standard_name = var.get_prop_value('standard_name')
        dimensions = var.get_dimensions()
        active = var.get_prop_value('active')
        var_dicts = [ self.__group.call_list ] + self.__group.suite_dicts()

        # If the variable isn't active, skip it
        if active.lower() =='.false.':
            return
        # Also, if the variable is one of the CCPP error handling messages, skip it
        # since it is defined as intent(out) and we can't do meaningful checks on it
        elif standard_name == 'ccpp_error_code' or standard_name == 'ccpp_error_message':
            return
        # To perform allocation checks, we need to know all variables
        # that are part of the 'active' attribute conditional and add
        # it to the group's call list.
        else:
            (_, vars_needed) = var.conditional(var_dicts)
            for var_needed in vars_needed:
                self.update_group_call_list_variable(var_needed)

        # For scalars and arrays, need an internal_var variable (same kind and type)
        # that we can assign the scalar or the lbound/ubound of the array to.
        # We need to treat DDTs and variables with kind attributes slightly
        # differently, and make sure there are no duplicate variables. We
        # also need to assign a bogus standard name to these local variables.
        vtype = var.get_prop_value('type')
        if var.is_ddt():
            vkind = ''
            units = ''
        else:
            vkind = var.get_prop_value('kind')
            units = var.get_prop_value('units')
        if vkind:
            internal_var_lname = f'internal_var_{vtype.replace("=","_")}_{vkind.replace("=","_")}'
        else:
            internal_var_lname = f'internal_var_{vtype.replace("=","_")}'
        if var.is_ddt():
            internal_var = Var({'local_name':internal_var_lname, 'standard_name':f'{internal_var_lname}_local',
                         'ddt_type':vtype, 'kind':vkind, 'units':units, 'dimensions':'()'},
                         _API_LOCAL, self.run_env)
        else:
            internal_var = Var({'local_name':internal_var_lname, 'standard_name':f'{internal_var_lname}_local',
                         'type':vtype, 'kind':vkind, 'units':units, 'dimensions':'()'},
                         _API_LOCAL, self.run_env)
        found = self.__group.find_variable(source_var=internal_var, any_scope=False)
        if not found:
            self.__group.manage_variable(internal_var)

        # For arrays, we need to get information on the dimensions and add it to
        # the group's call list so that we can test for the correct size later on
        if dimensions:
            for dim in dimensions:
                if not ':' in dim:
                    dim_var = self.find_variable(standard_name=dim)
                    if not dim_var:
                        raise Exception(f"No dimension with standard name '{dim}'")
                    self.update_group_call_list_variable(dim_var)
                else:
                    (ldim, udim) = dim.split(":")
                    ldim_var = self.find_variable(standard_name=ldim)
                    if not ldim_var:
                        raise Exception(f"No dimension with standard name '{ldim}'")
                    self.update_group_call_list_variable(ldim_var)
                    udim_var = self.find_variable(standard_name=udim)
                    if not udim_var:
                        raise Exception(f"No dimension with standard name '{udim}'")
                    self.update_group_call_list_variable(udim_var)

        # Add the variable to the list of variables to check. Record which internal_var to use.
        self.__var_debug_checks.append([var, internal_var])

    def replace_horiz_dim_debug_check(self, dim, cldicts, var_in_call_list):
        """Determine the correct horizontal dimension to use for a given variable,
        depending on the CCPP phase and origin of the variable (from the host/suite
        or defined as a module variable for the parent group, or local to the group.
        Return the dimension length and other properties needed for debug checks."""
        if not is_horizontal_dimension(dim):
            raise Exception(f"Dimension {dim} is not a horizontal dimension")
        if self.run_phase():
            if var_in_call_list and \
                self.find_variable(standard_name="horizontal_loop_extent"):
                ldim = "ccpp_constant_one"
                udim = "horizontal_loop_extent"
            else:
                ldim = "horizontal_loop_begin"
                udim = "horizontal_loop_end"
        else:
            ldim = "ccpp_constant_one"
            udim = "horizontal_dimension"
        # Get dimension for lower bound
        for var_dict in cldicts:
            dvar = var_dict.find_variable(standard_name=ldim, any_scope=False)
            if dvar is not None:
                break
        if not dvar:
            raise Exception(f"No variable with standard name '{ldim}' in cldicts")
        ldim_lname = dvar.get_prop_value('local_name')
        # Get dimension for upper bound
        for var_dict in cldicts:
            dvar = var_dict.find_variable(standard_name=udim, any_scope=False)
            if dvar is not None:
                break
        if not dvar:
            raise Exception(f"No variable with standard name '{udim}' in cldicts")
        udim_lname = dvar.get_prop_value('local_name')
        # Assemble dimensions and bounds for size checking
        dim_length = f'{udim_lname}-{ldim_lname}+1'
        # If the variable that uses these dimensions is not in the group's call
        # list, then it is defined as a module variable for this group and the
        # dimensions run from ldim to udim, otherwise from 1:dim_length.
        if not var_in_call_list:
            dim_string = f"{ldim_lname}:{udim_lname}"
            lbound_string = ldim_lname
            ubound_string = udim_lname
        else:
            dim_string = ":"
            lbound_string = '1'
            ubound_string = f'{udim_lname}-{ldim_lname}+1'
        return (dim_length, dim_string, lbound_string, ubound_string)

    def write_var_debug_check(self, var, internal_var, cldicts, outfile, errcode, errmsg, indent):
        """Write the variable debug check for the given variable, as determined
        in a previous step (add_var_debug_check). Assign the scalar or lower and
        upper bounds of the array to the internal_var variable, and for arrays also check
        that the size of the array matches the dimensions from the metadata.
        """
        # Get the basic attributes for writing the check
        standard_name = var.get_prop_value('standard_name')
        dimensions = var.get_dimensions()
        active = var.get_prop_value('active')
        allocatable = var.get_prop_value('allocatable')

        # Need the local name from the group call list,
        # from the locally-defined variables of the group,
        # or from the suite, not how it is called in the scheme (var)
        # First, check if the variable is in the call list.
        dvar = self.__group.call_list.find_variable(standard_name=standard_name, any_scope=False)
        if dvar:
            var_in_call_list = True
        else:
            var_in_call_list = False
            # If it is not in the call list, try to find it
            # in the local variables of this group subroutine.
            dvar = self.__group.find_variable(standard_name=standard_name, any_scope=False)
            if not dvar:
                # This variable is handled by the group
                # and is declared as a module variable
                for var_dict in self.__group.suite_dicts():
                    dvar = var_dict.find_variable(standard_name=standard_name, any_scope=False)
                    if dvar:
                        break
        if not dvar:
            raise Exception(f"No variable with standard name '{standard_name}' in cldicts")
        local_name = dvar.get_prop_value('local_name')

        # If the variable is allocatable and the intent for the scheme is 'out',
        # then we can't test anything because the scheme is going to allocate 
        # the variable. We don't have this information earlier in
        # add_var_debug_check, therefore need to back out here,
        # using the information from the scheme variable (call list).
        svar = self.call_list.find_variable(standard_name=standard_name, any_scope=False)
        intent = svar.get_prop_value('intent')
        if intent == 'out' and allocatable:
            return

        # Get the condition on which the variable is active
        (conditional, _) = var.conditional(cldicts)

        # For scalars, assign to internal_var variable if the variable intent is in/inout
        if not dimensions:
            if not intent == 'out':
                internal_var_lname = internal_var.get_prop_value('local_name')
                tmp_indent = indent
                if conditional != '.true.':
                    tmp_indent = indent + 1
                    outfile.write(f"if {conditional} then", indent)
                # end if
                outfile.write(f"! Assign value of {local_name} to {internal_var_lname}", tmp_indent)
                outfile.write(f"{internal_var_lname} = {local_name}", tmp_indent)
                outfile.write('',tmp_indent)
                if conditional != '.true.':
                    outfile.write(f"end if", indent)
                # end if
        # For arrays, check size of array against dimensions in metadata, then assign
        # the lower and upper bounds to the internal_var variable if the intent is in/inout
        else:
            array_size = 1
            dim_strings = []
            lbound_strings = []
            ubound_strings = []
            for dim in dimensions:
                if not ':' in dim:
                    # In capgen, any true dimension (that is not a single index) does
                    # have a colon (:) in the dimension, therefore this is an index
                    for var_dict in cldicts:
                        dvar = var_dict.find_variable(standard_name=dim, any_scope=False)
                        if dvar is not None:
                            break
                    if not dvar:
                        raise Exception(f"No variable with standard name '{dim}' in cldicts")
                    dim_lname = dvar.get_prop_value('local_name')
                    dim_length = 1
                    dim_strings.append(dim_lname)
                    lbound_strings.append(dim_lname)
                    ubound_strings.append(dim_lname)
                else:
                    # Horizontal dimension needs to be dealt with separately, because it
                    # depends on the CCPP phase, whether the variable is a host/suite
                    # variable or locally defined on the group level.
                    if is_horizontal_dimension(dim):
                        (dim_length, dim_string, lbound_string, ubound_string) = \
                            self.replace_horiz_dim_debug_check(dim, cldicts, var_in_call_list)
                    else:
                        (ldim, udim) = dim.split(":")
                        # Get dimension for lower bound
                        for var_dict in cldicts:
                            dvar = var_dict.find_variable(standard_name=ldim, any_scope=False)
                            if dvar is not None:
                                break
                        if not dvar:
                            raise Exception(f"No variable with standard name '{ldim}' in cldicts")
                        ldim_lname = dvar.get_prop_value('local_name')
                        # Get dimension for upper bound
                        for var_dict in cldicts:
                            dvar = var_dict.find_variable(standard_name=udim, any_scope=False)
                            if dvar is not None:
                                break
                        if not dvar:
                            raise Exception(f"No variable with standard name '{udim}' in cldicts")
                        udim_lname = dvar.get_prop_value('local_name')
                        # Assemble dimensions and bounds for size checking
                        dim_length = f'{udim_lname}-{ldim_lname}+1'
                        dim_string = ":"
                        lbound_string = ldim_lname
                        ubound_string = udim_lname
                    # end if
                    dim_strings.append(dim_string)
                    lbound_strings.append(lbound_string)
                    ubound_strings.append(ubound_string)
                array_size = f'{array_size}*({dim_length})'

            # Various strings needed to get the right size
            # and lower/upper bound of the array
            dim_string = '(' + ','.join(dim_strings) + ')'
            lbound_string = '(' + ','.join(lbound_strings) + ')'
            ubound_string = '(' + ','.join(ubound_strings) + ')'

            # Write size check
            tmp_indent = indent
            if conditional != '.true.':
                tmp_indent = indent + 1
                outfile.write(f"if {conditional} then", indent)
            # end if
            outfile.write(f"! Check size of array {local_name}", tmp_indent)
            outfile.write(f"if (size({local_name}{dim_string}) /= {array_size}) then", tmp_indent)
            outfile.write(f"write({errmsg}, '(a)') 'In group {self.__group.name} before {self.__subroutine_name}:'", tmp_indent+1)
            outfile.write(f"write({errmsg}, '(2(a,i8))') 'for array {local_name}, expected size ', {array_size}, ' but got ', size({local_name})", tmp_indent+1)
            outfile.write(f"{errcode} = 1", tmp_indent+1)
            outfile.write(f"return", tmp_indent+1)
            outfile.write(f"end if", tmp_indent)
            if conditional != '.true.':
                outfile.write(f"end if", indent)
            # end if
            outfile.write('',indent)

            # Assign lower/upper bounds to internal_var (scalar) if intent is not out
            if not intent == 'out':
                internal_var_lname = internal_var.get_prop_value('local_name')
                tmp_indent = indent
                if conditional != '.true.':
                    tmp_indent = indent + 1
                    outfile.write(f"if {conditional} then", indent)
                # end if
                outfile.write(f"! Assign lower/upper bounds of {local_name} to {internal_var_lname}", tmp_indent)
                outfile.write(f"{internal_var_lname} = {local_name}{lbound_string}", tmp_indent)
                outfile.write(f"{internal_var_lname} = {local_name}{ubound_string}", tmp_indent)
                if conditional != '.true.':
                    outfile.write(f"end if", indent)
                # end if
                outfile.write('',indent)

    def associate_optional_var(self, dict_var, var, var_ptr, has_transform, cldicts, indent, outfile):
        """Write local pointer association for optional variables."""
        if (dict_var):
            (conditional, _) = dict_var.conditional(cldicts)
            if (has_transform):
                lname = var.get_prop_value('local_name')+'_local'
            else:
                lname = var.get_prop_value('local_name')
            # end if
            lname_ptr = var_ptr.get_prop_value('local_name')
            outfile.write(f"if {conditional} then", indent)
            outfile.write(f"{lname_ptr} => {lname}", indent+1)
            outfile.write(f"end if", indent)
        # end if

    def assign_pointer_to_var(self, dict_var, var, var_ptr, has_transform, cldicts, indent, outfile):
        """Assign local pointer to variable."""
        if (dict_var):
            intent = var.get_prop_value('intent')
            if (intent == 'out' or intent == 'inout'):
                (conditional, _) = dict_var.conditional(cldicts)
                if (has_transform):
                    lname = var.get_prop_value('local_name')+'_local'
                else:
                    lname = var.get_prop_value('local_name')
                # end if
                lname_ptr = var_ptr.get_prop_value('local_name')
                outfile.write(f"if {conditional} then", indent)
                outfile.write(f"{lname} = {lname_ptr}", indent+1)
                outfile.write(f"end if", indent)
            # end if
        # end if

    def add_var_transform(self, var, compat_obj, vert_dim):
        """Register any variable transformation needed by <var> for this Scheme.
        For any transformation identified in <compat_obj>, create dummy variable
        from <var> to perform the transformation. Determine the indices needed
        for the transform and save for use during write stage"""

        # Add dummy variable (<var>_local) needed for transformation.
        dummy = var.clone(var.get_prop_value('local_name')+'_local')
        self.__group.manage_variable(dummy)

        # Create indices (default) for transform.
        lindices   = [':']*var.get_rank()
        rindices   = [':']*var.get_rank()

        # If needed, modify vertical dimension for vertical orientation flipping
        _, vdim    = find_vertical_dimension(var.get_dimensions())
        if vdim >= 0:
           vdims  = vert_dim.split(':')
           vdim_name  = vdims[-1]
           group_vvar = self.__group.call_list.find_variable(vdim_name)
           if group_vvar is None:
               raise CCPPError(f"add_var_transform: Cannot find dimension variable, {vdim_name}")
           # end if
           vname = group_vvar.get_prop_value('local_name')
           if len(vdims) == 2:
               sdim_name = vdims[0]
               group_vvar = self.find_variable(sdim_name)
               if group_vvar is None:
                   raise CCPPError(f"add_var_transform: Cannot find dimension variable, {sdim_name}")
               # end if
               sname = group_vvar.get_prop_value('local_name')
           else:
               sname = '1'
           # end if
           lindices[vdim] = sname+':'+vname
           if compat_obj.has_vert_transforms:
               rindices[vdim] = vname+':'+sname+':-1'
           else:
               rindices[vdim] = sname+':'+vname
           # end if
        # end if

        # If needed, modify horizontal dimension for loop substitution.
        # NOT YET IMPLEMENTED
        #hdim = find_horizontal_dimension(var.get_dimensions())
        #if compat_obj.has_dim_transforms:

        #
        # Register any reverse (pre-Scheme) transforms.
        #
        if (var.get_prop_value('intent') != 'out'):
            self.__reverse_transforms.append([dummy.get_prop_value('local_name'),
                                              var.get_prop_value('local_name'),
                                              rindices, lindices, compat_obj])

        #
        # Register any forward (post-Scheme) transforms.
        #
        if (var.get_prop_value('intent') != 'in'):
            self.__forward_transforms.append([var.get_prop_value('local_name'),
                                              dummy.get_prop_value('local_name'),
                                              lindices, rindices, compat_obj])

    def write_var_transform(self, var, dummy, rindices, lindices, compat_obj,
                            outfile, indent, forward):
        """Write variable transformation needed to call this Scheme in <outfile>.
        <var> is the varaible that needs transformation before and after calling Scheme.
        <dummy> is the local variable needed for the transformation..
        <lindices> are the LHS indices of <dummy> for reverse transforms (before Scheme).
        <rindices> are the RHS indices of <var>   for reverse transforms (before Scheme).
        <lindices> are the LHS indices of <var>   for forward transforms (after  Scheme).
        <rindices> are the RHS indices of <dummy> for forward transforms (after  Scheme).
        """
        #
        # Write reverse (pre-Scheme) transform.
        #
        if not forward:
            # dummy(lindices) = var(rindices)
            stmt = compat_obj.reverse_transform(lvar_lname=dummy,
                                                rvar_lname=var,
                                                lvar_indices=lindices,
                                                rvar_indices=rindices)
        #
        # Write forward (post-Scheme) transform.
        #
        else:
            # var(lindices) = dummy(rindices)
            stmt = compat_obj.forward_transform(lvar_lname=var,
                                                rvar_lname=dummy,
                                                lvar_indices=rindices,
                                                rvar_indices=lindices)
        # end if
        outfile.write(stmt, indent)

    def write(self, outfile, errcode, errmsg, indent):
        # Unused arguments are for consistent write interface
        # pylint: disable=unused-argument
        """Write code to call this Scheme to <outfile>"""
        # Dictionaries to try are our group, the group's call list,
        #    or our module
        cldicts = [self.__group, self.__group.call_list]
        cldicts.extend(self.__group.suite_dicts())
        my_args = self.call_list.call_string(cldicts=cldicts,
                                             is_func_call=True,
                                             subname=self.subroutine_name)
        #
        outfile.write('', indent)
        outfile.write('if ({} == 0) then'.format(errcode), indent)
        #
        # Write debug checks (operating on variables
        # coming from the group's call list)
        #
        if self.__var_debug_checks:
            outfile.write('! ##################################################################', indent+1)
            outfile.comment('Begin debug tests', indent+1)
            outfile.write('! ##################################################################', indent+1)
            outfile.write('', indent+1)
        # end if
        for (var, internal_var) in self.__var_debug_checks:
            stmt = self.write_var_debug_check(var, internal_var, cldicts, outfile, errcode, errmsg, indent+1)
        # end for
        if self.__var_debug_checks:
            outfile.write('! ##################################################################', indent+1)
            outfile.comment('End debug tests', indent+1)
            outfile.write('! ##################################################################', indent+1)
            outfile.write('', indent+1)
        # end if
        #
        # Write any reverse (pre-Scheme) transforms.
        if len(self.__reverse_transforms) > 0:
            outfile.comment('Compute reverse (pre-scheme) transforms', indent+1)
        # end if
        for (dummy, var, rindices, lindices, compat_obj) in self.__reverse_transforms:
            tstmt = self.write_var_transform(var, dummy, rindices, lindices, compat_obj, outfile, indent+1, False)
        # end for
        outfile.write('',indent+1)
        #
        # Associate any conditionally allocated variables.
        #
        if self.__optional_vars:
            outfile.write('! Associate conditional variables', indent+1)
        # end if 
        for (dict_var, var, var_ptr, has_transform) in self.__optional_vars:
            tstmt = self.associate_optional_var(dict_var, var, var_ptr, has_transform, cldicts, indent+1, outfile)
        # end for
        # 
        # Write the scheme call.
        #
        if not self._no_run_phase:
#            outfile.comment('! No run phase', indent+1)
#        else:
            stmt = 'call {}({})'
            outfile.write('',indent+1)
            outfile.write('! Call scheme', indent+1)
            outfile.write(stmt.format(self.subroutine_name, my_args), indent+1)
            outfile.write('',indent+1)
        # end if
        #
        # Copy any local pointers.
        #
        first_ptr_declaration=True
        for (dict_var, var, var_ptr, has_transform) in self.__optional_vars:
            if first_ptr_declaration: 
                outfile.write('! Copy any local pointers to dummy/local variables', indent+1)
                first_ptr_declaration=False
            # end if
            tstmt = self.assign_pointer_to_var(dict_var, var, var_ptr, has_transform, cldicts, indent+1, outfile)
        # end for
        outfile.write('',indent+1)
        #
        # Write any forward (post-Scheme) transforms.
        #
        if len(self.__forward_transforms) > 0:
            outfile.comment('Compute forward (post-scheme) transforms', indent+1)
        # end if
        for (var, dummy, lindices, rindices, compat_obj) in self.__forward_transforms:
            tstmt = self.write_var_transform(var, dummy, rindices, lindices, compat_obj, outfile, indent+1, True)
        # end for
        outfile.write('', indent)
        outfile.write('end if', indent)

    def schemes(self):
        """Return self as a list for consistency with subcycle"""
        return [self]

    def variable_list(self, recursive=False,
                      std_vars=True, loop_vars=True, consts=True):
        """Return a list of all variables for this Scheme.
        Because Schemes do not have any variables, return a list
        of this object's CallList variables instead.
        Note that because of this, <recursive=True> is not allowed."""
        if recursive:
            raise ParseInternalError("recursive=True not allowed for Schemes")
        # end if
        return self.call_list.variable_list(recursive=recursive,
                                            std_vars=std_vars,
                                            loop_vars=loop_vars, consts=consts)

    @property
    def subroutine_name(self):
        """Return this scheme's actual subroutine name"""
        return self.__subroutine_name

    @property
    def has_vertical_dim(self):
        """Return True if at least one of this Scheme's variables has
        a vertical dimension (vertical_layer_dimension or
        vertical_interface_dimension)
        """
        return self.__has_vertical_dimension

    def __str__(self):
        """Create a readable string for this Scheme"""
        return '<Scheme {}: {}>'.format(self.name, self.subroutine_name)

###############################################################################

class VerticalLoop(SuiteObject):
    """Class to call a group of schemes or scheme collections in a
    loop over a vertical dimension."""

    def __init__(self, index_name, context, parent, run_env, items=None):
        """ <index_name> is the standard name of the variable holding the
        number of iterations (e.g., vertical_layer_dimension)."""
        # self._dim_name is the standard name for the number of iterations
        self._dim_name = VarDictionary.find_loop_dim_from_index(index_name)
        if self._dim_name is None:
            errmsg = 'No VerticalLoop dimension name for index = {}'
            raise ParseInternalError(errmsg.format(index_name))
        # end if
        if ':' in self._dim_name:
            dims = self._dim_name.split(':')
            if not dims[1]:
                errmsg = 'Invalid loop dimension, {}'
                raise ParseInternalError(errmsg.format(self._dim_name))
            # end if
            self._dim_name = dims[1]
        # end if
        # self._local_dim_name is the variable name for self._dim_name
        self._local_dim_name = None
        super().__init__(index_name, context, parent, run_env)
        if run_env.verbose:
            lmsg = "Adding VerticalLoop for '{}'"
            run_env.logger.debug(lmsg.format(index_name))
        # end if
        # Add any items
        if not isinstance(items, list):
            if items is None:
                items = list()
            else:
                items = [items]
            # end if
        # end if
        for item in items:
            self.add_part(item)
        # end for

    def analyze(self, phase, group, scheme_library, suite_vars, level):
        """Analyze the VerticalLoop's interface to prepare for writing"""
        # Handle all the suite objects inside of this subcycle
        scheme_mods = set()
        # Create a variable for the loop index
        newvar = Var({'local_name':self.name, 'standard_name':self.name,
                      'type':'integer', 'units':'count', 'dimensions':'()'},
                     _API_LOCAL, self.run_env)
        # The Group will manage this variable
        group.manage_variable(newvar)
        # Find the loop-extent variable
        dim_name = self._dim_name
        local_dim = group.find_variable(standard_name=dim_name, any_scope=False)
        if local_dim is None:
            local_dim = group.call_list.find_variable(standard_name=dim_name,
                                                      any_scope=False)
        # end if
        # If not found, check the suite level
        if local_dim is None:
            local_dim = group.suite.find_variable(standard_name=dim_name)
        # end if
        if local_dim is None:
            emsg = 'No variable found for vertical loop dimension {}'
            raise ParseInternalError(emsg.format(self._dim_name))
        # end if
        self._local_dim_name = local_dim.get_prop_value('local_name')
        emsg = "VerticalLoop local name for '{}'".format(self.name)
        emsg += " is '{}".format(self.dimension_name)
        if self.run_env.logger:
            self.run_env.logger.debug(emsg)
        # end if
        # Analyze our internal items
        for item in self.parts:
            smods = item.analyze(phase, group, scheme_library,
                                 suite_vars, level+1)
            for smod in smods:
                scheme_mods.add(smod)
            # end for
        # end for
        return scheme_mods

    def write(self, outfile, errcode, errmsg, indent):
        """Write code for the vertical loop, including contents, to <outfile>"""
        outfile.write('do {} = 1, {}'.format(self.name, self.dimension_name),
                      indent)
        # Note that 'scheme' may be a sybcycle or other construct
        for item in self.parts:
            item.write(outfile, errcode, errmsg, indent+1)
        # end for
        outfile.write('end do', 2)

    @property
    def dimension_name(self):
        """Return the vertical dimension over which this VerticalLoop loops"""
        return self._local_dim_name

###############################################################################

class Subcycle(SuiteObject):
    """Class to represent a subcycled group of schemes or scheme collections"""

    def __init__(self, sub_xml, context, parent, run_env):
        name = sub_xml.get('name', None) # Iteration count
        loop_extent = sub_xml.get('loop', "1") # Number of iterations
        # See if our loop variable is an interger or a variable
        try:
            loop_int = int(loop_extent) # pylint: disable=unused-variable
            self._loop = loop_extent
            self._loop_var_int = True
        except ValueError:
            self._loop_var_int = False
            lvar = parent.find_variable(standard_name=self.loop, any_scope=True)
            if lvar is None:
                emsg = "Subcycle, {}, specifies {} iterations but {} not found"
                raise CCPPError(emsg.format(name, self.loop, self.loop))
            # end if
            parent.add_call_list_variable(lvar)
        # end try
        super().__init__(name, context, parent, run_env)
        for item in sub_xml:
            new_item = new_suite_object(item, context, self, run_env)
            self.add_part(new_item)
        # end for

    def analyze(self, phase, group, scheme_library, suite_vars, level):
        """Analyze the Subcycle's interface to prepare for writing"""
        if self.name is None:
            self.name = "subcycle_index{}".format(level)
        # end if
        # Create a variable for the loop index
        self.add_variable(Var({'local_name':self.name,
                               'standard_name':'loop_variable',
                               'type':'integer', 'units':'count',
                               'dimensions':'()'}, _API_SOURCE, self.run_env),
                          self.run_env)
        # Handle all the suite objects inside of this subcycle
        scheme_mods = set()
        for item in self.parts:
            smods = item.analyze(phase, group, scheme_library,
                                 suite_vars, level+1)
            for smod in smods:
                scheme_mods.add(smod)
            # end for
        # end for
        return scheme_mods

    def write(self, outfile, errcode, errmsg, indent):
        """Write code for the subcycle loop, including contents, to <outfile>"""
        outfile.write('do {} = 1, {}'.format(self.name, self.loop), indent)
        # Note that 'scheme' may be a sybcycle or other construct
        for item in self.parts:
            item.write(outfile, errcode, errmsg, indent+1)
        # end for
        outfile.write('end do', 2)

    @property
    def loop(self):
        """Return the loop value or variable local_name"""
        lvar = self.find_variable(standard_name=self.loop, any_scope=True)
        if lvar is None:
            emsg = "Subcycle, {}, specifies {} iterations but {} not found"
            raise CCPPError(emsg.format(self.name, self.loop, self.loop))
        # end if
        lname = lvar.get_prop_value('local_name')
        return lname

###############################################################################

class TimeSplit(SuiteObject):
    """Class to represent a group of processes to be computed in a time-split
    manner -- each parameterization or other construct is called with an
    state which has been updated from the previous step.
    """

    def __init__(self, sub_xml, context, parent, run_env):
        super().__init__('TimeSplit', context, parent, run_env)
        for part in sub_xml:
            new_item = new_suite_object(part, context, self, run_env)
            self.add_part(new_item)
        # end for

    def analyze(self, phase, group, scheme_library, suite_vars, level):
        # Unused arguments are for consistent analyze interface
        # pylint: disable=unused-argument
        """Analyze the TimeSplit's interface to prepare for writing"""
        # Handle all the suite objects inside of this group
        scheme_mods = set()
        for item in self.parts:
            smods = item.analyze(phase, group, scheme_library,
                                 suite_vars, level+1)
            for smod in smods:
                scheme_mods.add(smod)
            # end for
        # end for
        return scheme_mods

    def write(self, outfile, errcode, errmsg, indent):
        """Write code for this TimeSplit section, including contents,
        to <outfile>"""
        for item in self.parts:
            item.write(outfile, errcode, errmsg, indent)
        # end for

###############################################################################

class ProcessSplit(SuiteObject):
    """Class to represent a group of processes to be computed in a
    process-split manner -- all parameterizations or other constructs are
    called with the same state.
    NOTE: Currently a stub
    """

    def __init__(self, sub_xml, context, parent, run_env):
        # Unused arguments are for consistent __init__ interface
        # pylint: disable=unused-argument
        super().__init__('ProcessSplit', context, parent, run_env)
        raise CCPPError('ProcessSplit not yet implemented')

    def analyze(self, phase, group, scheme_library, suite_vars, level):
        # Unused arguments are for consistent analyze interface
        # pylint: disable=unused-argument
        """Analyze the ProcessSplit's interface to prepare for writing"""
        # Handle all the suite objects inside of this group
        raise CCPPError('ProcessSplit not yet implemented')

    def write(self, outfile, errcode, errmsg, indent):
        """Write code for this ProcessSplit section, including contents,
        to <outfile>"""
        raise CCPPError('ProcessSplit not yet implemented')

###############################################################################

class Group(SuiteObject):
    """Class to represent a grouping of schemes in a suite
    A Group object is implemented as a subroutine callable by the API.
    The main arguments to a group are the host model variables.
    Additional output arguments are generated from schemes with intent(out)
    arguments.
    Additional input or inout arguments are generated for inputs needed by
    schemes which are produced (intent(out)) by other groups.
    """

    __subhead = '''
   subroutine {subname}({args})
'''

    __subend = '''
   end subroutine {subname}

! ========================================================================
'''

    __thread_check = CodeBlock([('#ifdef _OPENMP', -1),
                                ('if (omp_get_thread_num() > 1) then', 1),
                                ('{errcode} = 1', 2),
                                (('{errmsg} = "Cannot call {phase} routine '
                                  'from a threaded region"'), 2),
                                ('return', 2),
                                ('end if', 1),
                                ('#endif', -1)])

    __process_types = [_API_TIMESPLIT_TAG, _API_PROCESSSPLIT_TAG]

    __process_xml = {}
    for gptype in __process_types:
        __process_xml[gptype] = '<{ptype}></{ptype}>'.format(ptype=gptype)
    # end for

    def __init__(self, group_xml, transition, parent, context, run_env):
        """Initialize this Group object from <group_xml>.
        <transition> is the group's phase, <parent> is the group's suite.
        """
        name = parent.name + '_' + group_xml.get('name')
        if transition not in CCPP_STATE_MACH.transitions():
            errmsg = "Bad transition argument to Group, '{}'"
            raise ParseInternalError(errmsg.format(transition))
        # end if
        # Initialize the dictionary of variables internal to group
        super().__init__(name, context, parent, run_env,
                         active_call_list=True, phase_type=transition)
        # Add the items but first make sure we know the process type for
        # the group (e.g., TimeSplit or ProcessSplit).
        if (transition == RUN_PHASE_NAME) and ((not group_xml) or
                                               (group_xml[0].tag not in
                                                Group.__process_types)):
            # Default is TimeSplit
            tsxml = ET.fromstring(Group.__process_xml[_API_TIMESPLIT_TAG])
            time_split = new_suite_object(tsxml, context, self, run_env)
            add_to = time_split
            self.add_part(time_split)
        else:
            add_to = self
        # end if
        # Add the sub objects either directly to the Group or to the TimeSplit
        for item in group_xml:
            new_item = new_suite_object(item, context, add_to, run_env)
            add_to.add_part(new_item)
        # end for
        self._local_schemes = set()
        self._host_vars = None
        self._host_ddts = None
        self._loop_var_matches = list()
        self._phase_check_stmts = list()
        self._set_state = None
        self._ddt_library = None

    def phase_match(self, scheme_name):
        """If scheme_name matches the group phase, return the group and
            function ID. Otherwise, return None
        """
        fid, tid, _ = CCPP_STATE_MACH.transition_match(scheme_name,
                                                       transition=self.phase())
        if tid is not None:
            return self, fid
        # end if
        return None, None

    def move_to_call_list(self, standard_name):
        """Move a variable from the group internal dictionary to the call list.
        This is done when the variable, <standard_name>, will be allocated by
        the suite.
        """
        gvar = self.find_variable(standard_name=standard_name, any_scope=False)
        if gvar is None:
            errmsg = "Group {}, cannot move {}, variable not found"
            raise ParseInternalError(errmsg.format(self.name, standard_name))
        # end if
        self.add_call_list_variable(gvar, exists_ok=True)
        self.remove_variable(standard_name)

    def register_action(self, vaction):
        """Register any recognized <vaction> type for use during self.write.
        Return True iff <vaction> is handled.
        """
        if isinstance(vaction, VarLoopSubst):
            self._loop_var_matches = vaction.add_to_list(self._loop_var_matches)
            # Add the missing dim
            vaction.add_local(self, _API_LOCAL, self.run_env)
            return True
        # end if
        return False

    def manage_variable(self, newvar):
        """Add <newvar> to our local dictionary making necessary
        modifications to the variable properties so that it is
        allocated appropriately"""
        # Need new prop dict to eliminate unwanted properties (e.g., intent)
        vdims = newvar.get_dimensions()
        # Look for dimensions where we have a loop substitution and replace
        # with the correct size
        if self.run_phase():
            hdims = [x.missing_stdname for x in self._loop_var_matches]
        else:
            # Do not do loop substitutions in full phases
            hdims = list()
        # end if
        for index, dim in enumerate(vdims):
            newdim = None
            for subdim in dim.split(':'):
                if subdim in hdims:
                    # We have a loop substitution, find and replace
                    hindex = hdims.index(subdim)
                    names = self._loop_var_matches[hindex].required_stdnames
                    newdim = ':'.join(names)
                    break
                # end if
                if ('vertical' in subdim) and ('index' in subdim):
                    # We have a vertical index, replace with correct dimension
                    errmsg = "vertical index replace not implemented"
                    raise ParseInternalError(errmsg)
                # end if
            # end for
            if newdim is not None:
                vdims[index] = newdim
            # end if
        # end for
        if self.timestep_phase():
            persist = 'timestep'
        else:
            persist = 'run'
        # end if
        # Start with an official copy of <newvar>'s prop_dict with
        #      corrected dimensions
        subst_dict = {'dimensions':vdims}
        prop_dict = newvar.copy_prop_dict(subst_dict=subst_dict)
        # Add the allocatable items
        prop_dict['allocatable'] = len(vdims) > 0 # No need to allocate scalar
        prop_dict['persistence'] = persist
        # This is a local variable
        if 'intent' in prop_dict:
            del prop_dict['intent']
        # end if
        # Create a new variable, save the original context
        local_var = Var(prop_dict,
                        ParseSource(_API_SOURCE_NAME,
                                    _API_LOCAL_VAR_NAME, newvar.context),
                        self.run_env)
        self.add_variable(local_var, self.run_env, exists_ok=True)
        # Finally, make sure all dimensions are accounted for
        emsg = self.add_variable_dimensions(local_var, _API_LOCAL_VAR_TYPES,
                                            adjust_intent=True,
                                            to_dict=self.call_list)
        if emsg:
            raise CCPPError(emsg)
        # end if

    def analyze(self, phase, suite_vars, scheme_library, ddt_library,
                check_suite_state, set_suite_state):
        """Analyze the Group's interface to prepare for writing"""
        self._ddt_library = ddt_library
        # Sanity check for Group
        if phase != self.phase():
            errmsg = 'Group {} has phase {} but analyze is phase {}'
            raise ParseInternalError(errmsg.format(self.name,
                                                   self.phase(), phase))
        # end if
        for item in self.parts:
            # Items can be schemes, subcycles or other objects
            # All have the same interface and return a set of module use
            # statements (lschemes)
            lschemes = item.analyze(phase, self, scheme_library, suite_vars, 1)
            for lscheme in lschemes:
                self._local_schemes.add(lscheme)
            # end for
        # end for
        self._phase_check_stmts = check_suite_state
        self._set_state = set_suite_state
        if (self.run_env.logger and
            self.run_env.logger.isEnabledFor(logging.DEBUG)):
            self.run_env.logger.debug("{}".format(self))
        # end if

    def allocate_dim_str(self, dims, context):
        """Create the dimension string for an allocate statement"""
        rdims = list()
        for dim in dims:
            rdparts = list()
            dparts = dim.split(':')
            for dpart in dparts:
                dvar = self.find_variable(standard_name=dpart, any_scope=False)
                if dvar is None:
                    dvar = self.call_list.find_variable(standard_name=dpart,
                                                        any_scope=False)
                if dvar is None:
                    emsg = "Dimension variable, '{}', not found{}"
                    lvar = self.find_local_name(dpart, any_scope=True)
                    if lvar is not None:
                        emsg += "\nBe sure to use standard names!"
                    # end if
                    ctx = context_string(context)
                    raise CCPPError(emsg.format(dpart, ctx))
                # end if
                lname = dvar.get_prop_value('local_name')
                rdparts.append(lname)
            # end for
            rdims.append(':'.join(rdparts))
        # end for
        return ', '.join(rdims)

    def find_variable(self, standard_name=None, source_var=None,
                      any_scope=True, clone=None,
                      search_call_list=False, loop_subst=False):
        """Find a matching variable to <var>, create a local clone (if
        <clone> is True), or return None.
        This purpose of this special Group version is to record any constituent
        variable found for processing during the write phase.
        """
        fvar = super().find_variable(standard_name=standard_name,
                                     source_var=source_var,
                                     any_scope=any_scope, clone=clone,
                                     search_call_list=search_call_list,
                                     loop_subst=loop_subst)
        if fvar and fvar.is_constituent():
            if fvar.source.ptype == ConstituentVarDict.constitutent_source_type():
                # We found this variable in the constituent dictionary,
                #   add it to our call list
                self.add_call_list_variable(fvar, exists_ok=True)
            # end if
        # end if
        return fvar

    def write(self, outfile, host_arglist, indent, const_mod,
              suite_vars=None, allocate=False, deallocate=False):
        """Write code for this subroutine (Group), including contents,
        to <outfile>"""
        # Unused arguments are for consistent write interface
        # pylint: disable=unused-argument
        # group type for (de)allocation
        if self.timestep_phase():
            group_type = 'timestep' # Just allocate for the timestep
        else:
            group_type = 'run'      # Allocate for entire run
        # end if
        # Collect information on local variables
        subpart_allocate_vars = {}
        subpart_optional_vars = {}
        subpart_scalar_vars = {}
        allocatable_var_set = set()
        optional_var_set = set()
        pointer_var_set = list()
        inactive_var_set = set()
        for item in [self]:# + self.parts:
            for var in item.declarations():
                lname = var.get_prop_value('local_name')
                sname = var.get_prop_value('standard_name')
                if (lname in subpart_allocate_vars) or (lname in subpart_optional_vars) or (lname in subpart_scalar_vars):
                    if subpart_allocate_vars[lname][0].compatible(var, self.run_env):
                        pass # We already are going to declare this variable
                    else:
                        errmsg = "Duplicate Group variable, {}"
                        raise ParseInternalError(errmsg.format(lname))
                    # end if
                else:
                    opt_var = var.get_prop_value('optional')
                    dims = var.get_dimensions()
                    if (dims is not None) and dims:
                        if opt_var:
                            if (self.call_list.find_variable(standard_name=sname)):
                                subpart_optional_vars[lname] = (var, item, opt_var)
                                optional_var_set.add(lname)
                            else:
                                inactive_var_set.add(var)
                            # end if
                        else:
                            subpart_allocate_vars[lname] = (var, item, opt_var)
                            allocatable_var_set.add(lname)
                        # end if
                    else:
                        subpart_scalar_vars[lname] = (var, item, opt_var)
                    # end if
                # end if
            # end for
            # All optional dummy variables within group need to have 
            # an associated pointer array declared. 
            for cvar in self.call_list.variable_list():
                opt_var = cvar.get_prop_value('optional')
                if opt_var:
                    name = cvar.get_prop_value('local_name')+'_ptr'
                    kind = cvar.get_prop_value('kind')
                    dims = cvar.get_dimensions()
                    if cvar.is_ddt():
                        vtype = 'type'
                    else:
                        vtype = cvar.get_prop_value('type')
                    # end if
                    if dims:
                        dimstr = '(:' + ',:'*(len(dims) - 1) + ')'
                    else:
                        dimstr = ''
                    # end if
                    pointer_var_set.append([name,kind,dimstr,vtype])
                # end if
            # end for
            # Any optional arguments that are not requested by the host need to have
            # a local null pointer passed from the group to the scheme.
            for ivar in inactive_var_set:
                name = ivar.get_prop_value('local_name')+'_ptr'
                kind = ivar.get_prop_value('kind')
                dims = ivar.get_dimensions()
                if ivar.is_ddt():
                    vtype = 'type'
                else:
                    vtype = ivar.get_prop_value('type')
                # end if
                if dims:
                    dimstr = '(:' + ',:'*(len(dims) - 1) + ')'
                else:
                    dimstr = ''
                # end if
                pointer_var_set.append([name,kind,dimstr,vtype])
            # end for

        # end for
        # First, write out the subroutine header
        subname = self.name
        call_list = self.call_list.call_string()
        outfile.write(Group.__subhead.format(subname=subname, args=call_list),
                      indent)
        # Write out any use statements
        if self._local_schemes:
            modmax = max([len(s[0]) for s in self._local_schemes])
        else:
            modmax = 0
        # end if
        # Write out the scheme use statements
        scheme_use = 'use {},{} only: {}'
        for scheme in self._local_schemes:
            smod = scheme[0]
            sname = scheme[1]
            slen = ' '*(modmax - len(smod))
            outfile.write(scheme_use.format(smod, slen, sname), indent+1)
        # end for
        # Look for any DDT types
        call_vars = self.call_list.variable_list()
        self._ddt_library.write_ddt_use_statements(call_vars, outfile,
                                                   indent+1, pad=modmax)
        decl_vars = [x[0] for x in subpart_allocate_vars.values()]
        self._ddt_library.write_ddt_use_statements(decl_vars, outfile,
                                                   indent+1, pad=modmax)
        outfile.write('', 0)
        # Write out dummy arguments
        outfile.write('! Dummy arguments', indent+1)
        msg = 'Variables for {}: ({})'
        if (self.run_env.logger and
            self.run_env.logger.isEnabledFor(logging.DEBUG)):
            self.run_env.logger.debug(msg.format(self.name, call_vars))
        # end if
        self.call_list.declare_variables(outfile, indent+1, dummy=True)
        # DECLARE local variables
        if subpart_allocate_vars or subpart_scalar_vars or subpart_optional_vars:
            outfile.write('\n! Local Variables', indent+1)
        # end if
        # Scalars
        for key in subpart_scalar_vars:
            var = subpart_scalar_vars[key][0]
            spdict = subpart_scalar_vars[key][1]
            target = subpart_scalar_vars[key][2]
            var.write_def(outfile, indent+1, spdict,
                          allocatable=False, target=target)
        # end for
        # Allocatable arrays
        for key in subpart_allocate_vars:
            var = subpart_allocate_vars[key][0]
            spdict = subpart_allocate_vars[key][1]
            target = subpart_allocate_vars[key][2]
            var.write_def(outfile, indent+1, spdict,
                          allocatable=(key in allocatable_var_set),
                          target=target)
        # end for
        # Target arrays.
        for key in subpart_optional_vars:
            var = subpart_optional_vars[key][0]
            spdict = subpart_optional_vars[key][1]
            target = subpart_optional_vars[key][2]
            var.write_def(outfile, indent+1, spdict,
                          allocatable=(key in optional_var_set),
                          target=target)
        # end for
        # Pointer variables
        for (name, kind, dim, vtype) in pointer_var_set:
            var.write_ptr_def(outfile, indent+1, name,  kind, dim, vtype)
        # end for
        outfile.write('', 0)
        # Get error variable names
        if self.run_env.use_error_obj:
            raise ParseInternalError("Error object not supported")
        else:
            verrcode = self.call_list.find_variable(standard_name='ccpp_error_code')
            if verrcode is not None:
                errcode = verrcode.get_prop_value('local_name')
            else:
                errmsg = "No ccpp_error_code variable for group, {}"
                raise CCPPError(errmsg.format(self.name))
            # end if
            verrmsg = self.call_list.find_variable(standard_name='ccpp_error_message')
            if verrmsg is not None:
                errmsg = verrmsg.get_prop_value('local_name')
            else:
                errmsg = "No ccpp_error_message variable for group, {}"
                raise CCPPError(errmsg.format(self.name))
            # end if
            # Initialize error variables
            outfile.write("! Initialize ccpp error handling", 2)
            outfile.write("{} = 0".format(errcode), 2)
            outfile.write("{} = ''".format(errmsg), 2)
            outfile.write("",2)
        # end if
        # Output threaded region check (except for run phase)
        if not self.run_phase():
            outfile.write("! Output threaded region check ",indent+1)
            Group.__thread_check.write(outfile, indent,
                                       {'phase' : self.phase(),
                                        'errcode' : errcode,
                                        'errmsg' : errmsg})
        # Check state machine
        outfile.write("! Check state machine",indent+1)
        self._phase_check_stmts.write(outfile, indent,
                                      {'errcode' : errcode, 'errmsg' : errmsg,
                                       'funcname' : self.name})
        # Allocate local arrays
        outfile.write('\n! Allocate local arrays', indent+1)
        alloc_stmt = "allocate({}({}))"
        for lname in allocatable_var_set:
            var = subpart_allocate_vars[lname][0]
            dims = var.get_dimensions()
            alloc_str = self.allocate_dim_str(dims, var.context)
            outfile.write(alloc_stmt.format(lname, alloc_str), indent+1)
        # end for
        for lname in optional_var_set:
            var = subpart_optional_vars[lname][0]
            dims = var.get_dimensions()
            alloc_str = self.allocate_dim_str(dims, var.context)
            outfile.write(alloc_stmt.format(lname, alloc_str), indent+1)
        # end for
        # Allocate suite vars
        if allocate:
            outfile.write('\n! Allocate suite_vars', indent+1)
            for svar in suite_vars.variable_list():
                dims = svar.get_dimensions()
                if dims:
                    timestep_var = svar.get_prop_value('persistence')
                    if group_type == timestep_var:
                        alloc_str = self.allocate_dim_str(dims, svar.context)
                        lname = svar.get_prop_value('local_name')
                        outfile.write(alloc_stmt.format(lname, alloc_str),
                                      indent+1)
                    # end if (do not allocate in this phase)
                # end if dims (do not allocate scalars)
            # end for
        # end if
        # Write any loop match calculations
        for vmatch in self._loop_var_matches:
            action = vmatch.write_action(self, dict2=self.call_list)
            if action:
                outfile.write(action, indent+1)
            # end if
        # end for
        # Write the scheme and subcycle calls
        for item in self.parts:
            item.write(outfile, errcode, errmsg, indent + 1)
        # end for
        # Deallocate local arrays
        if allocatable_var_set:
            outfile.write('\n! Deallocate local arrays', indent+1)
        # end if
        for lname in allocatable_var_set:
            outfile.write('if (allocated({})) {} deallocate({})'.format(lname,' '*(20-len(lname)),lname), indent+1)
        # end for
        for lname in optional_var_set:
            outfile.write('if (allocated({})) {} deallocate({})'.format(lname,' '*(20-len(lname)),lname), indent+1)
        # end for
        # Nullify local pointers
        if pointer_var_set:
            outfile.write('\n! Nullify local pointers', indent+1)
        # end if
        for (name, kind, dim, vtype) in pointer_var_set:
            #cspace = ' '*(15-len(name))
            outfile.write('if (associated({})) {} nullify({})'.format(name,' '*(15-len(name)),name), indent+1)
        # end fo
        # Deallocate suite vars
        if deallocate:
            for svar in suite_vars.variable_list():
                dims = svar.get_dimensions()
                if dims:
                    timestep_var = svar.get_prop_value('persistence')
                    if group_type == timestep_var:
                        lname = svar.get_prop_value('local_name')
                        outfile.write('deallocate({})'.format(lname), indent+1)
                    # end if
                # end if (no else, do not deallocate scalars)
            # end for
        # end if
        self._set_state.write(outfile, indent, {})
        # end if
        outfile.write(Group.__subend.format(subname=subname), indent)

    @property
    def suite(self):
        """Return this Group's suite"""
        return self.parent

    def suite_dicts(self):
        """Return a list of this Group's Suite's dictionaries"""
        return self.suite.suite_dicts()

###############################################################################

if __name__ == "__main__":
    # First, run doctest
    # pylint: disable=ungrouped-imports
    import doctest
    import sys
    # pylint: enable=ungrouped-imports
    fail, _ = doctest.testmod()
    sys.exit(fail)
# end if
