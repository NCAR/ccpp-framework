#!/usr/bin/env python
#

"""Classes and methods to create a Fortran suite-implementation file
to implement calls to a set of suites for a given host model."""

# Python library imports
import os.path
import re
import xml.etree.ElementTree as ET
# CCPP framework imports
from parse_tools import ParseContext, ParseSource, context_string
from parse_tools import ParseInternalError, CCPPError
from parse_tools import read_xml_file, validate_xml_file, find_schema_version
from metavar import Var, VarDictionary, VarLoopSubst, ccpp_standard_var
from metavar import CCPP_CONSTANT_VARS, CCPP_LOOP_VAR_STDNAMES
from ddt_library import DDTLibrary
from state_machine import StateMachine
from fortran_tools import FortranWriter
from code_block import CodeBlock

# pylint: disable=too-many-lines

###############################################################################
# Module (global) variables
###############################################################################

_INIT_ST = r"(?:(?i)init(?:ial(?:ize)?)?)"
_FINAL_ST = r"(?:(?i)final(?:ize)?)"
_RUN_ST = r"(?:(?i)run)"
_TS_INIT_ST = r"(?:(?i)timestep_init(?:ial(?:ize)?)?)"
_TS_FINAL_ST = r"(?:(?i)timestep_final(?:ize)?)"

_OBJ_LOC_RE = re.compile(r"(0x[0-9A-Fa-f]+)>")
_BLANK_DIMS_RE = re.compile(r"[(][:](,:)*[)]$")

# Source for internally generated variables.
_API_SOURCE_NAME = "CCPP_API"
_API_SUITE_VAR_NAME = "suite"
_API_GROUP_VAR_NAME = "group"
_API_SCHEME_VAR_NAME = "scheme"
_API_LOCAL_VAR_NAME = "local"
_API_LOCAL_VAR_TYPES = [_API_LOCAL_VAR_NAME, _API_SUITE_VAR_NAME]
_API_CONTEXT = ParseContext(filename="ccpp_suite.py")
_API_SOURCE = ParseSource(_API_SOURCE_NAME, _API_SCHEME_VAR_NAME, _API_CONTEXT)
_API_LOCAL = ParseSource(_API_SOURCE_NAME, _API_LOCAL_VAR_NAME, _API_CONTEXT)
_API_GROUP = ParseSource(_API_SOURCE_NAME, _API_GROUP_VAR_NAME, _API_CONTEXT)
_API_TIMESPLIT_TAG = 'time_split'
_API_PROCESSSPLIT_TAG = 'process_split'

# Allowed CCPP transitions
# pylint: disable=bad-whitespace
_RUN_PHASE_NAME = 'run'
CCPP_STATE_MACH = StateMachine((('initialize', 'uninitialized',
                                 'initialized', _INIT_ST),
                                ('timestep_initial', 'initialized',
                                 'in_time_step', _TS_INIT_ST),
                                (_RUN_PHASE_NAME, 'in_time_step',
                                 'in_time_step', _RUN_ST),
                                ('timestep_final', 'in_time_step',
                                 'initialized', _TS_FINAL_ST),
                                ('finalize', 'initialized',
                                 'uninitialized', _FINAL_ST)))
# pylint: enable=bad-whitespace

# Required variables for inclusion in auto-generated schemes
CCPP_REQUIRED_VARS = [ccpp_standard_var('ccpp_error_flag',
                                        _API_SCHEME_VAR_NAME,
                                        context=_API_CONTEXT),
                      ccpp_standard_var('ccpp_error_message',
                                        _API_SCHEME_VAR_NAME,
                                        context=_API_CONTEXT)]

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

# Standardize name of generated kinds file and module
KINDS_MODULE = 'ccpp_kinds'
KINDS_FILENAME = '{}.F90'.format(KINDS_MODULE)

###############################################################################
def new_suite_object(item, context, parent, logger):
###############################################################################
    "'Factory' method to create the appropriate suite object from XML"
    new_item = None
    if item.tag == 'subcycle':
        new_item = Subcycle(item, context, parent, logger)
    elif item.tag == 'scheme':
        new_item = Scheme(item, context, parent, logger)
    elif item.tag == _API_TIMESPLIT_TAG:
        new_item = TimeSplit(item, context, parent, logger)
    else:
        raise CCPPError("Unknown CCPP suite element type, '{}'".format(item.tag))
    # end if
    return new_item

###############################################################################

class CallList(VarDictionary):
    """A simple class to hold a routine's call list (dummy arguments)"""

    def __init__(self, name, logger=None):
        super(CallList, self).__init__(name, logger=logger)

    def add_vars(self, call_list, gen_unique=False):
        """Add new variables from another CallList (<call_list>)"""
        for var in call_list.variable_list():
            stdname = var.get_prop_value('standard_name')
            if stdname not in self:
                self.add_variable(var, gen_unique=gen_unique)
            # end if
        # end for

    def call_string(self, cldict=None, include_dims=False):
        """Return a dummy argument string for this call list.
        <cldict> may be another VarDictionary object from which to retrieve
        local_names (default is to use self).
        """
        arg_str = ""
        arg_sep = ""
        for var in self.variable_list():
            # Do not include constants
            stdname = var.get_prop_value('standard_name')
            if stdname not in CCPP_CONSTANT_VARS:
                if cldict is not None:
                    dvar = cldict.find_variable(stdname)
                    if dvar is None:
                        errmsg = "Variable, '{}', not found in {}"
                        raise CCPPError(errmsg.format(stdname, cldict.name))
                    # end if
                    lname = dvar.get_prop_value('local_name')
                else:
                    lname = var.get_prop_value('local_name')
                    aref = var.array_ref()
                    if aref is not None:
                        lname = aref.group(1)
                    # end if
                # end if
                if include_dims:
                    if cldict is not None:
                        use_dict = cldict
                    else:
                        use_dict = self
                    # end if
                    vdims = var.call_dimstring(var_dict=use_dict)
                    if _BLANK_DIMS_RE.match(vdims) is None:
                        lname = lname + vdims
                    # end if
                # end if
                arg_str = arg_str + arg_sep + lname
                arg_sep = ", "
            # end if
        # end for
        return arg_str

    # pylint: disable=arguments-differ
    def add_variable(self, newvar, exists_ok=False, gen_unique=False,
                     objdict=None):
        """Add <newvar> to our dictionary (using the standard VarDictionary
        method).
        Then, attempt to add <newvar>'s dimensions as well. Note, that for
        this to succeed, the variable has to exist in <objdict>'s
        parent call tree.
        Raise an error if <objdict> is present but a dimension cannot
           be found"""
        # Check to see if we have a special call list intent case
        evar = super(CallList, self).find_variable(newvar, any_scope=False)
        nintent = newvar.get_prop_value('intent')
        if not nintent:
            # We must be trying to add a host variable, give it an intent
            nintent = 'in'
        # End if
        if evar:
            eintent = evar.get_prop_value('intent')
            if (eintent == 'out') and (nintent != 'out'):
                elname = evar.get_prop_value('local_name')
                nlname = newvar.get_prop_value('local_name')
                ectx = context_string(evar.context)
                nctx = context_string(newvar.context)
                emsg = "Call list intent mismatch, existing = {}{}"
                emsg += ", new = {}{}"
                raise CCPPError(emsg.format(elname, ectx, nlname, nctx))
            # end if
        # end if
        newvar.adjust_intent(evar or newvar)
        # Add variable
        super(CallList, self).add_variable(newvar, exists_ok=exists_ok,
                                           gen_unique=gen_unique,
                                           adjust_intent=True)
        # Attempt to add dimensions
        if objdict:
            emsg = objdict.add_variable_dimensions(newvar, _API_LOCAL_VAR_TYPES,
                                                   to_dict=self)
            if emsg:
                raise CCPPError(emsg)
            # end if
        # end if
        # pylint: enable=arguments-differ

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

    def __init__(self, name, context, parent, logger,
                 active_call_list=False, variables=None, phase_type=None):
        # pylint: disable=too-many-arguments
        self.__name = name
        self.__context = context
        self.__logger = logger
        self.__parent = parent
        if active_call_list:
            self.__call_list = CallList(name + '_call_list', logger)
        else:
            self.__call_list = None
        # end if
        self.__parts = list()
        self.__needs_vertical = None
        self.__needs_horizontal = None
        self.__phase_type = phase_type
        # Initialize our dictionary
        super(SuiteObject, self).__init__(self.name, variables=variables,
                                          parent_dict=parent, logger=logger)

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
                                          self, self.__logger, items=[item])
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
        return self.phase() == _RUN_PHASE_NAME

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
        return var.source.type == _API_SUITE_VAR_NAME

    def is_local_variable(self, var):
        """Return the local variable matching <var> if one is found belonging
        to this object or any of its SuiteObject parents."""
        stdname = var.get_prop_value('standard_name')
        lvar = None
        obj = self
        while (not lvar) and (obj is not None) and isinstance(obj, SuiteObject):
            lvar = obj.find_variable(stdname, any_scope=False,
                                     search_call_lst=False)
            if not lvar:
                obj = obj.parent
            # end if
        # end while
        return lvar

    def add_call_list_variable(self, newvar, exists_ok=False, gen_unique=False,
                               subst_dict=None):
        """Add <newvar> to this SuiteObject's call_list. If this SuiteObject
           does not have a call list, recursively try the SuiteObject's parent
        If <subst_dict> is not None, create a clone using that as a dictionary
           of substitutions.
        Do not add <newvar> if it exists as a local variable.
        Do not add <newvar> if it is a suite variable"""
        stdname = newvar.get_prop_value('standard_name')
        if self.parent:
            pvar = self.parent.find_variable(stdname, any_scope=False)
        else:
            pvar = None
        # end if
        if SuiteObject.is_suite_variable(newvar):
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
                newvar = oldvar.clone(subst_dict, source_name=self.name,
                                      source_type=stype, context=self.context)
            # end if
            self.call_list.add_variable(newvar, exists_ok=exists_ok,
                                        gen_unique=gen_unique, objdict=self)
        elif self.parent is None:
            errmsg = 'No call_list found for {}'.format(newvar)
            raise ParseInternalError(errmsg)
        elif pvar:
            # Check for call list incompatibility
            if pvar is not None:
                compat, reason = pvar.compatible(newvar)
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
            self.add_call_list_variable(var, exists_ok=True, gen_unique=True,
                                        subst_dict=subst_dict)
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
        nis_hdim = Var.is_horizontal_dimension(ndim)
        his_hdim = Var.is_horizontal_dimension(hdim)
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
        >>> SuiteObject('foo', _API_CONTEXT, None, None).match_dimensions(['horizontal_loop_extent'], ['horizontal_loop_extent'])
        (True, ['horizontal_loop_extent'], ['horizontal_loop_extent'], None, None, '')
        >>> SuiteObject('foo', _API_CONTEXT,None, None,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL)],active_call_list=True,phase_type='initialize').match_dimensions(['ccpp_constant_one:horizontal_loop_extent'], ['ccpp_constant_one:horizontal_dimension'])
        (True, ['ccpp_constant_one:horizontal_dimension'], ['ccpp_constant_one:horizontal_dimension'], None, None, '')
        >>> SuiteObject('foo', _API_CONTEXT,None,None,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL)],active_call_list=True,phase_type=_RUN_PHASE_NAME).match_dimensions(['ccpp_constant_one:horizontal_loop_extent'], ['horizontal_loop_begin:horizontal_loop_end'])
        (True, ['horizontal_loop_begin:horizontal_loop_end'], ['horizontal_loop_begin:horizontal_loop_end'], None, None, '')
        >>> SuiteObject('foo', _API_CONTEXT,None,None,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL),Var({'local_name':'lev','standard_name':'vertical_layer_dimension','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL)],active_call_list=True,phase_type=_RUN_PHASE_NAME).match_dimensions(['ccpp_constant_one:horizontal_loop_extent'], ['horizontal_loop_begin:horizontal_loop_end','ccpp_constant_one:vertical_layer_dimension'])
        (False, ['horizontal_loop_begin:horizontal_loop_end', 'vertical_layer_index'], ['horizontal_loop_begin:horizontal_loop_end', 'ccpp_constant_one:vertical_layer_dimension'], 'vertical_layer_index', None, 'missing vertical dimension')
        >>> SuiteObject('foo', _API_CONTEXT,None,None,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL),Var({'local_name':'lev','standard_name':'vertical_layer_dimension','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL)],active_call_list=True,phase_type=_RUN_PHASE_NAME).match_dimensions(['ccpp_constant_one:horizontal_loop_extent','ccpp_constant_one:vertical_layer_dimension'], ['horizontal_loop_begin:horizontal_loop_end','ccpp_constant_one:vertical_layer_dimension'])
        (True, ['horizontal_loop_begin:horizontal_loop_end', 'ccpp_constant_one:vertical_layer_dimension'], ['horizontal_loop_begin:horizontal_loop_end', 'ccpp_constant_one:vertical_layer_dimension'], None, None, '')
        >>> SuiteObject('foo', _API_CONTEXT,None,None,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL),Var({'local_name':'lev','standard_name':'vertical_layer_dimension','units':'count','dimensions':'()','type':'integer'}, _API_LOCAL)],active_call_list=True,phase_type=_RUN_PHASE_NAME).match_dimensions(['ccpp_constant_one:horizontal_loop_extent','ccpp_constant_one:vertical_layer_dimension'], ['ccpp_constant_one:vertical_layer_dimension','horizontal_loop_begin:horizontal_loop_end'])
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
        _, nvdim_index = Var.find_vertical_dimension(need_dims)
        _, hvdim_index = Var.find_vertical_dimension(have_dims)
        _, nhdim_index = Var.find_horizontal_dimension(need_dims)
        _, hhdim_index = Var.find_horizontal_dimension(have_dims)
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
                    mvdim = self.find_variable(mstdname, any_scope=True)
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

    # pylint: disable=arguments-differ
    def find_variable(self, standard_name, any_scope=True,
                      search_call_lst=True):
        """Find a matching variable to <var>, create a local clone (if
        <clone> is True), or return None.
        First search the SuiteObject's internal dictionary, then its
        call list (unless <skip_call_list> is True, then any parent
        dictionary (if <any_scope> is True).
        <var> can be a Var object or a standard_name string.
        """
        if isinstance(standard_name, Var):
            stdname = standard_name.get_prop_value('standard_name')
        else:
            stdname = standard_name
        # end if
        # First, search our local dictionary
        found_var = super(SuiteObject, self).find_variable(stdname,
                                                           any_scope=False)
        if (not found_var) and (self.call_list is not None) and search_call_lst:
            found_var = self.call_list.find_variable(stdname, any_scope=False)
        # end if
        loop_okay = VarDictionary.loop_var_okay(stdname, self.run_phase())
        if not loop_okay:
            any_scope = False
        # end if
        if (found_var is None) and any_scope and (self.parent is not None):
            # We do not have the variable, look to parents.
            if isinstance(self.parent, SuiteObject):
                scl = search_call_lst
                found_var = self.parent.find_variable(stdname, any_scope=True,
                                                      search_call_lst=scl)
            else:
                found_var = self.parent.find_variable(stdname, any_scope=True)
        # end if
        return found_var
    # pylint: enable=arguments-differ

    def match_variable(self, var, vstdname=None, vdims=None):
        """Try to find a source for <var> in this SuiteObject's dictionary
        tree. Several items are returned:
        found_var: True if a match was found
        vert_dim: The vertical dimension in <var>, or None
        call_dims: How this variable should be called (or None if no match)
        missing_vert: Vertical dim in parent but not in <var>
        perm: Permutation (XXgoldyXX: Not yet implemented)
        """
        if vstdname is None:
            vstdname = var.get_prop_value('standard_name')
        # end if
        if vdims is None:
            vdims = var.get_dimensions()
        # end if
        if (not vdims) and self.run_phase():
            vmatch = VarDictionary.loop_var_match(vstdname)
        else:
            vmatch = None
        # end if
        found_var = False
        missing_vert = None
        new_vdims = list()
        var_vdim = var.has_vertical_dimension(dims=vdims)
        # Does this variable exist in the calling tree?
        dict_var = self.find_variable(vstdname, any_scope=True)
        if dict_var is None:
            # No existing variable but add loop var match to call tree
            found_var = self.parent.add_variable_to_call_tree(dict_var,
                                                              vmatch=vmatch)
            new_vdims = vdims
        elif dict_var.source.type in _API_LOCAL_VAR_TYPES:
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
        return found_var, var_vdim, new_vdims, missing_vert

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
        """Element parent (or none)"""
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

    @property
    def context(self):
        """Return the context of this SuiteObject"""
        return self.__context

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

    def __repr__(self):
        """Create a unique readable string for this Object"""
        so_repr = super(SuiteObject, self).__repr__()
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

    def __init__(self, scheme_xml, context, parent, logger):
        """Initialize this physics Scheme"""
        name = scheme_xml.text
        self.__subroutine_name = None
        self.__context = context
        self.__version = scheme_xml.get('version', None)
        self.__lib = scheme_xml.get('lib', None)
        self.__has_vertical_dimension = False
        self.__group = None
        super(Scheme, self).__init__(name, context, parent,
                                     logger, active_call_list=True)

    def update_group_call_list_variable(self, var):
        """If <var> is in our group's call list, update its intent"""
        stdname = var.get_prop_value('standard_name')
        gvar = self.__group.call_list.find_variable(stdname, any_scope=False)
        if gvar:
            self.__group.add_call_list_variable(var, exists_ok=True)
        # end if

    def is_local_variable(self, var):
        """Return None as we never consider <var> to be in our local
        dictionary.
        This is an override of the SuiteObject version"""
        return None

    def analyze(self, phase, group, scheme_library, suite_vars, level, logger):
        # Unused arguments are for consistent analyze interface
        # pylint: disable=unused-argument
        """Analyze the scheme's interface to prepare for writing"""
        self.__group = group
        my_header = None
        if self.name in scheme_library:
            func = scheme_library[self.name]
            if phase in func:
                my_header = func[phase]
                self.__subroutine_name = my_header.title
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
            vdims = var.get_dimensions()
            args = self.match_variable(var, vstdname=vstdname, vdims=vdims)
            found, vert_dim, new_dims, missing_vert = args
            if found:
                if not self.has_vertical_dim:
                    self.__has_vertical_dimension = vert_dim is not None
                # end if
                # We have a match, make sure var is in call list
                if new_dims == vdims:
                    self.add_call_list_variable(var, exists_ok=True)
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
                if var.get_prop_value('intent') == 'out':
                    if self.__group is None:
                        errmsg = 'Group not defined for {}'.format(self.name)
                        raise ParseInternalError(errmsg)
                    # end if
                    # The Group will manage this variable
                    self.__group.manage_variable(var)
                    # We still need it in our call list but declared the
                    # same way our Group did
                    gvar = self.__group.find_variable(vstdname, any_scope=False)
                    if gvar is None:
                        errmsg = 'Group managed variable, {}, not found'
                        raise ParseInternalError(errmsg.format(vstdname))
                    # end if
                    self.add_call_list_variable(gvar)
                else:
                    errmsg = 'Input argument for {}, {}, not found.'
                    if self.find_variable(vstdname) is not None:
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
        # end for
        if self.needs_vertical is not None:
            self.parent.add_part(self, replace=True) # Should add a vloop
            if isinstance(self.parent, VerticalLoop):
                # Restart the loop analysis
                scheme_mods = self.parent.analyze(phase, group, scheme_library,
                                                  suite_vars, level, logger)
            # end if
        # end if
        return scheme_mods

    def write(self, outfile, logger, errflg, indent):
        # Unused arguments are for consistent write interface
        # pylint: disable=unused-argument
        """Write code to call this Scheme to <outfile>"""
        my_args = self.call_list.call_string(cldict=self.parent,
                                             include_dims=True)
        stmt = 'call {}({})'
        outfile.write('if ({} == 0) then'.format(errflg), indent)
        outfile.write(stmt.format(self.subroutine_name, my_args), indent+1)
        outfile.write('end if', indent)

    def schemes(self):
        """Return self as a list for consistency with subcycle"""
        return [self]

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

    def __init__(self, index_name, context, parent, logger, items=None):
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
        super(VerticalLoop, self).__init__(index_name, context, parent, logger)
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

    def analyze(self, phase, group, scheme_library, suite_vars, level, logger):
        """Analyze the VerticalLoop's interface to prepare for writing"""
        # Handle all the suite objects inside of this subcycle
        scheme_mods = set()
        # Create a variable for the loop index
        newvar = Var({'local_name':self.name, 'standard_name':self.name,
                      'type':'integer', 'units':'count', 'dimensions':'()'},
                     _API_LOCAL)
        # The Group will manage this variable
        group.manage_variable(newvar)
        # Find the loop-extent variable
        local_dim = self.find_variable(self._dim_name, any_scope=True)
        if local_dim is None:
            errmsg = 'No variable found for vertical loop dimension {}'
            raise ParseInternalError(errmsg.format(self._dim_name))
        # end if
        self._local_dim_name = local_dim.get_prop_value('local_name')
        # Analyze our internal items
        for item in self.parts:
            smods = item.analyze(phase, group, scheme_library,
                                 suite_vars, level+1, logger)
            for smod in smods:
                scheme_mods.add(smod)
            # end for
        # end for
        return scheme_mods

    def write(self, outfile, logger, errflg, indent):
        """Write code for the vertical loop, including contents, to <outfile>"""
        outfile.write('do {} = 1, {}'.format(self.name, self.dimension_name),
                      indent)
        # Note that 'scheme' may be a sybcycle or other construct
        for item in self.parts:
            item.write(outfile, logger, errflg, indent+1)
        # end for
        outfile.write('end do', 2)

    @property
    def dimension_name(self):
        """Return the vertical dimension over which this VerticalLoop loops"""
        return self._local_dim_name

###############################################################################

class Subcycle(SuiteObject):
    """Class to represent a subcycled group of schemes or scheme collections"""

    def __init__(self, sub_xml, context, parent, logger):
        name = sub_xml.get('name', None) # Iteration count
        loop_extent = sub_xml.get('loop', "1") # Number of iterations
        # See if our loop variable is an interger or a variable
        try:
            loop_int = int(loop_extent) # pylint: disable=unused-variable
            self._loop = loop_extent
            self._loop_var_int = True
        except ValueError:
            self._loop_var_int = False
            lvar = parent.find_variable(self.loop, any_scope=True)
            if lvar is None:
                emsg = "Subcycle, {}, specifies {} iterations but {} not found"
                raise CCPPError(emsg.format(name, self.loop, self.loop))
            # end if
            parent.add_call_list_variable(lvar)
        # end try
        super(Subcycle, self).__init__(name, context, parent, logger)
        for item in sub_xml:
            new_item = new_suite_object(item, context, self, logger)
            self.add_part(new_item)
        # end for

    def analyze(self, phase, group, scheme_library, suite_vars, level, logger):
        """Analyze the Subcycle's interface to prepare for writing"""
        if self.name is None:
            self.name = "subcycle_index{}".format(level)
        # end if
        # Create a variable for the loop index
        self.add_variable(Var({'local_name':self.name,
                               'standard_name':'loop_variable',
                               'type':'integer', 'units':'count',
                               'dimensions':'()'}, _API_SOURCE))
        # Handle all the suite objects inside of this subcycle
        scheme_mods = set()
        for item in self.parts:
            smods = item.analyze(phase, group, scheme_library,
                                 suite_vars, level+1, logger)
            for smod in smods:
                scheme_mods.add(smod)
            # end for
        # end for
        return scheme_mods

    def write(self, outfile, logger, errflg, indent):
        """Write code for the subcycle loop, including contents, to <outfile>"""
        outfile.write('do {} = 1, {}'.format(self.name, self.loop), indent)
        # Note that 'scheme' may be a sybcycle or other construct
        for item in self.parts:
            item.write(outfile, logger, errflg, indent+1)
        # end for
        outfile.write('end do', 2)

    @property
    def loop(self):
        """Return the loop value or variable local_name"""
        lvar = self.find_variable(self.loop, any_scope=True)
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

    def __init__(self, sub_xml, context, parent, logger):
        super(TimeSplit, self).__init__('TimeSplit', context, parent, logger)
        for part in sub_xml:
            new_item = new_suite_object(part, context, self, logger)
            self.add_part(new_item)
        # end for

    def analyze(self, phase, group, scheme_library, suite_vars, level, logger):
        # Unused arguments are for consistent analyze interface
        # pylint: disable=unused-argument
        """Analyze the TimeSplit's interface to prepare for writing"""
        # Handle all the suite objects inside of this group
        scheme_mods = set()
        for item in self.parts:
            smods = item.analyze(phase, group, scheme_library,
                                 suite_vars, level+1, logger)
            for smod in smods:
                scheme_mods.add(smod)
            # end for
        # end for
        return scheme_mods

    def write(self, outfile, logger, errflg, indent):
        """Write code for this TimeSplit section, including contents,
        to <outfile>"""
        for item in self.parts:
            item.write(outfile, logger, errflg, indent)
        # end for

###############################################################################

class ProcessSplit(SuiteObject):
    """Class to represent a group of processes to be computed in a
    process-split manner -- all parameterizations or other constructs are
    called with the same state.
    NOTE: Currently a stub
    """

    def __init__(self, sub_xml, context, parent, logger):
        # Unused arguments are for consistent __init__ interface
        # pylint: disable=unused-argument
        super(ProcessSplit, self).__init__('ProcessSplit', context,
                                           parent, logger)
        raise CCPPError('ProcessSplit not yet implemented')

    def analyze(self, phase, group, scheme_library, suite_vars, level, logger):
        # Unused arguments are for consistent analyze interface
        # pylint: disable=unused-argument
        """Analyze the ProcessSplit's interface to prepare for writing"""
        # Handle all the suite objects inside of this group
        raise CCPPError('ProcessSplit not yet implemented')

    def write(self, outfile, logger, errflg, indent):
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
'''

    __thread_check = CodeBlock([('#ifdef _OPENMP', -1),
                                ('if (omp_get_thread_num() > 1) then', 1),
                                ('{errflg} = 1', 2),
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

    def __init__(self, group_xml, transition, parent, context, logger):
        """Initialize this Group object from <group_xml>.
        <transition> is the group's phase, <parent> is the group's suite.
        """
        name = parent.name + '_' + group_xml.get('name')
        if transition not in CCPP_STATE_MACH.transitions():
            errmsg = "Bad transition argument to Group, '{}'"
            raise ParseInternalError(errmsg.format(transition))
        # end if
        # Initialize the dictionary of variables internal to group
        super(Group, self).__init__(name, context, parent,
                                    logger, active_call_list=True,
                                    phase_type=transition)
        # Add the items but first make sure we know the process tpye for
        # the group (e.g., TimeSplit or ProcessSplit).
        if (transition == _RUN_PHASE_NAME) and ((not len(group_xml)) or
                                                (group_xml[0].tag not in
                                                 Group.__process_types)):
            # Default is TimeSplit
            tsxml = ET.fromstring(Group.__process_xml[_API_TIMESPLIT_TAG])
            time_split = new_suite_object(tsxml, context, self, logger)
            add_to = time_split
            self.add_part(time_split)
        else:
            add_to = self
        # end if
        # Add the sub objects either directly to the Group or to the TimeSplit
        for item in group_xml:
            new_item = new_suite_object(item, context, add_to, logger)
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
        gvar = self.find_variable(standard_name, any_scope=False)
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
            vaction.add_local(self, _API_LOCAL)
            return True
        # end if
        return False

    def manage_variable(self, newvar):
        """Add <newvar> to our local dictionary making necessary
        modifications to the variable properties so that it is
        allocated appropriately"""
        # Need new dictionary to eliminate unwanted properties (e.g., intent)
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
        # Start with an official copy of <newvar>'s prop_dict
        subst_dict = {'dimensions':vdims}
        prop_dict = newvar.copy_prop_dict(subst_dict=subst_dict)
        # Add the allocatable items
        prop_dict['allocatable'] = True
        prop_dict['persistence'] = persist
        # This is a local variable
        if 'intent' in prop_dict:
            del prop_dict['intent']
        # end if
        # Create a new variable, save the original context
        local_var = Var(prop_dict, ParseSource(_API_SOURCE_NAME,
                                               _API_LOCAL_VAR_NAME,
                                               newvar.context))
        self.add_variable(local_var, exists_ok=True)
        # Finally, make sure all dimensions are accounted for
        emsg = self.add_variable_dimensions(local_var, _API_LOCAL_VAR_TYPES,
                                            to_dict=self.call_list)
        if emsg:
            raise CCPPError(emsg)
        # end if

    def analyze(self, phase, suite_vars, scheme_library, ddt_library, logger):
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
            lschemes = item.analyze(phase, self, scheme_library,
                                    suite_vars, 1, logger)
            for lscheme in lschemes:
                self._local_schemes.add(lscheme)
            # end for
        # end for
        self._phase_check_stmts = Suite.check_suite_state(phase)
        self._set_state = Suite.set_suite_state(phase)
        logger.debug("{}".format(self))

    def allocate_dim_str(self, dims, context):
        """Create the dimension string for an allocate statement"""
        rdims = list()
        for dim in dims:
            rdparts = list()
            dparts = dim.split(':')
            for dpart in dparts:
                dvar = self.find_variable(dpart, any_scope=True)
                if dvar is None:
                    emsg = "Dimension variable, {} not found{}"
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

    def write(self, outfile, logger, host_arglist, indent,
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
        subpart_var_set = {}
        allocatable_vars = set()
        for item in [self] + self.parts:
            for var in item.declarations():
                lname = var.get_prop_value('local_name')
                if lname in subpart_var_set:
                    if subpart_var_set[lname].compatible(var):
                        pass # We already are going to declare this variable
                    else:
                        errmsg = "Duplicate suite part variable, {}"
                        raise ParseInternalError(errmsg.format(lname))
                    # end if
                else:
                    subpart_var_set[lname] = (var, item)
                    dims = var.get_dimensions()
                    if (dims is not None) and dims:
                        allocatable_vars.add(lname)
                    # end if
                # end if
            # end for
        # end for
        # First, write out the subroutine header
        subname = self.name
        call_list = self.call_list.call_string()
        outfile.write(Group.__subhead.format(subname=subname, args=call_list),
                      indent)
        # Write out any use statements
        modmax = 0
        for scheme in self._local_schemes:
            if len(scheme[0]) > modmax:
                modmax = len(scheme[0])
            # end if
        # end for
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
        decl_vars = [x[0] for x in subpart_var_set.values()]
        self._ddt_library.write_ddt_use_statements(decl_vars, outfile,
                                                   indent+1, pad=modmax)
        outfile.write('', 0)
        # Write out dummy arguments
        outfile.write('! Dummy arguments', indent+1)
        msg = 'Variables for {}: ({})'
        logger.debug(msg.format(self.name, call_vars))
        self.call_list.declare_variables(outfile, indent+1, dummy=True)
        if subpart_var_set:
            outfile.write('\n! Local Variables', indent+1)
        # Write out local variables
        for key in subpart_var_set:
            var = subpart_var_set[key][0]
            spdict = subpart_var_set[key][1]
            var.write_def(outfile, indent+1, spdict,
                          allocatable=(key in allocatable_vars))
        # end for
        outfile.write('', 0)
        # Get error variable names
        verrflg = self.find_variable('ccpp_error_flag', any_scope=True)
        if verrflg is not None:
            errflg = verrflg.get_prop_value('local_name')
        else:
            errmsg = "No ccpp_error_flag variable for group, {}"
            raise CCPPError(errmsg.format(self.name))
        # end if
        verrmsg = self.find_variable('ccpp_error_message', any_scope=True)
        if verrmsg is not None:
            errmsg = verrmsg.get_prop_value('local_name')
        else:
            errmsg = "No ccpp_error_message variable for group, {}"
            raise CCPPError(errmsg.format(self.name))
        # end if
        # Output threaded region check (except for run phase)
        if not self.run_phase():
            Group.__thread_check.write(outfile, indent,
                                       {'phase' : self.phase(),
                                        'errflg' : errflg, 'errmsg' : errmsg})
        # Check state machine
        self._phase_check_stmts.write(outfile, indent,
                                      {'errflg' : errflg, 'errmsg' : errmsg,
                                       'funcname' : self.name})
        # Allocate local arrays
        alloc_stmt = "allocate({}({}))"
        for lname in allocatable_vars:
            var = subpart_var_set[lname][0]
            dims = var.get_dimensions()
            alloc_str = self.allocate_dim_str(dims, var.context)
            outfile.write(alloc_stmt.format(lname, alloc_str), indent+1)
        # end for
        # Allocate suite vars
        if allocate:
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
            item.write(outfile, logger, errflg, indent + 1)
        # end for
        # Deallocate local arrays
        for lname in allocatable_vars:
            outfile.write('deallocate({})'.format(lname), indent+1)
        # end for
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

###############################################################################

class Suite(VarDictionary):
    """Class to hold, process, and output a CAP for an entire CCPP suite.
    The Suite includes initialization and finalization Group objects as
    well as a Group for every suite part."""

    __state_machine_initial_state = 'uninitialized'
    __state_machine_var_name = 'ccpp_suite_state'

    __header = '''
!>
!! @brief Auto-generated cap module for the CCPP suite
!!
!
module {module}
'''

    __state_machine_init = '''
character(len=16) :: {css_var_name} = '{state}'
'''

    __footer = '''
end module {module}
'''

    # Note that these group names need to match CCPP_STATE_MACH
    __initial_group_name = 'initialize'

    __final_group_name = 'finalize'

    __timestep_initial_group_name = 'timestep_initial'

    __timestep_final_group_name = 'timestep_final'

    __scheme_template = '<scheme>{}</scheme>'

    def __init__(self, filename, api, logger):
        """Initialize this Suite object from the SDF, <filename>.
        <api> serves as the Suite's parent."""
        self.__logger = logger
        self._name = None
        self._sdf_name = filename
        self._groups = list()
        self._suite_init_group = None
        self._suite_final_group = None
        self._timestep_init_group = None
        self._timestep_final_group = None
        self.__context = None
        self._host_arg_list_full = None
        self._host_arg_list_noloop = None
        self._module = None
        self._ddt_library = None
        # Full phases/groups are special groups where the entire state is passed
        self._full_groups = {}
        self._full_phases = {}
        self._gvar_stdnames = {} # Standard names of group-created vars
        # Initialize our dictionary
        super(Suite, self).__init__(self.sdf_name, parent_dict=api,
                                    logger=logger)
        if not os.path.exists(self._sdf_name):
            emsg = "Suite definition file {0} not found."
            raise CCPPError(emsg.format(self._sdf_name))
        # end if
        # Parse the SDF
        self.parse()

    @property
    def name(self):
        """Get the name of the suite."""
        return self._name

    @property
    def sdf_name(self):
        """Get the name of the suite definition file."""
        return self._sdf_name

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
            check_stmts.append(("{errflg} = 1", 2))
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

    def new_group(self, group_string, transition):
        """Create a new Group object from the a XML description"""
        if isinstance(group_string, str):
            gxml = ET.fromstring(group_string)
        else:
            gxml = group_string
        # end if
        group = Group(gxml, transition, self, self.__context, self.__logger)
        for svar in CCPP_REQUIRED_VARS:
            group.add_call_list_variable(svar)
        # end for
        if transition != _RUN_PHASE_NAME:
            self._full_groups[group.name] = group
            self._full_phases[group.phase()] = group
        # end if
        return group

    def new_group_from_name(self, group_name):
        '''Create an XML string for Group, <group_name>, and use it to
        create the corresponding group.
        Note: <group_name> must be the a transition string'''
        group_xml = '<group name="{}"></group>'.format(group_name)
        return self.new_group(group_xml, group_name)

    def parse(self):
        """Parse the suite definition file."""
        success = True

        _, suite_xml = read_xml_file(self._sdf_name, self.__logger)
        # We do not have line number information for the XML file
        self.__context = ParseContext(filename=self._sdf_name)
        # Validate the XML file
        version = find_schema_version(suite_xml)
        res = validate_xml_file(self._sdf_name, 'suite', version, self.__logger)
        if not res:
            emsg = "Invalid suite definition file, '{}'"
            raise CCPPError(emsg.format(self._sdf_name))
        # end if
        self._name = suite_xml.get('name')
        self._module = 'ccpp_{}_cap'.format(self.name)
        lmsg = "Reading suite definition file for '{}'"
        self.__logger.info(lmsg.format(self._name))
        gname = Suite.__initial_group_name
        self._suite_init_group = self.new_group_from_name(gname)
        gname = Suite.__final_group_name
        self._suite_final_group = self.new_group_from_name("finalize")
        gname = Suite.__timestep_initial_group_name
        self._timestep_init_group = self.new_group_from_name("timestep_initial")
        gname = Suite.__timestep_final_group_name
        self._timestep_final_group = self.new_group_from_name("timestep_final")
        # Set up some groupings for later efficiency
        self._beg_groups = [self._suite_init_group.name,
                            self._timestep_init_group.name]
        self._end_groups = [self._suite_final_group.name,
                            self._timestep_final_group.name]
        # Build hierarchical structure as in SDF
        self._groups.append(self._suite_init_group)
        self._groups.append(self._timestep_init_group)
        for suite_item in suite_xml:
            item_type = suite_item.tag.lower()
            # Suite item is a group or a suite-wide init or final method
            if item_type == 'group':
                # Parse a group
                self._groups.append(self.new_group(suite_item, _RUN_PHASE_NAME))
            else:
                match_trans = CCPP_STATE_MACH.function_match(item_type)
                if match_trans is None:
                    emsg = "Unknown CCPP suite component tag type, '{}'"
                    raise CCPPError(emsg.format(item_type))
                # end if
                if match_trans in self._full_phases:
                    # Parse a suite-wide initialization scheme
                    scheme = Scheme(suite_item, self.__context,
                                    self, self.__logger)
                    self._full_phases[match_trans].add_item(scheme)
                else:
                    emsg = "Unhandled CCPP suite component tag type, '{}'"
                    raise ParseInternalError(emsg.format(match_trans))
                # end if
        # end for
        self._groups.append(self._timestep_final_group)
        self._groups.append(self._suite_final_group)
        return success

    @property
    def module(self):
        """Get the list of the module generated for this suite."""
        return self._module

    @property
    def groups(self):
        """Get the list of groups in this suite."""
        return self._groups

    def find_variable(self, standard_name, any_scope=True, clone=None):
        """Attempt to return the variable matching <standard_name>.
        If <any_scope> is True, search parent scopes if not in current scope.
        If the variable is not found and <clone> is not None, add a clone of
        <clone> to this dictionary.
        If the variable is not found and <clone> is None, return None.
        """
        # First, see if the variable is already in our path
        var = super(Suite, self).find_variable(standard_name,
                                               any_scope=any_scope)
        if var is None:
            # No dice? Check for a group variable which can be promoted
            if standard_name in self._gvar_stdnames:
                group = self._gvar_stdnames[standard_name]
                var = group.find_variable(standard_name, any_scope=False)
                if var is not None:
                    # Promote variable to suite level?
                    # Remove this entry to avoid looping back here
                    del self._gvar_stdnames[standard_name]
                    # Let everyone know this is now a Suite variable
                    var.source = ParseSource(_API_SOURCE_NAME,
                                             _API_SUITE_VAR_NAME,
                                             var.context)
                    self.add_variable(var)
                    # Move to group's call list and our group list
                    group.move_to_call_list(standard_name)
                else:
                    emsg = ("Group, {}, claimed it had created {} "
                            "but variable was not found")
                    raise CCPPError(emsg.format(group.name, standard_name))
                # end if
            # end if
        # end if
        if (var is None) and (clone is not None):
            # Guess it is time to clone a different variable
            var = super(Suite, self).find_variable(standard_name, any_scope=any_scope, clone=clone)
        # end if
        return var

    def analyze(self, host_model, scheme_library, ddt_library, logger):
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
        self._ddt_library = ddt_library
        # Collect all relevant schemes
        # For all groups, find associated init and final methods
        scheme_set = set()
        for group in self.groups:
            for scheme in group.schemes():
                scheme_set.add(scheme.name)
            # end for
        # end for
        no_scheme_entries = {} # Skip schemes that are not in this suite
        for module in scheme_library:
            if module in scheme_set:
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
                        scheme = Scheme(sxml, self.__context, pgroup,
                                        self.__logger)
                        pgroup.add_part(scheme)
                    # end if (no else, scheme is already in group)
                # end if (no else, phase not in scheme set)
            # end for
        # end for
        # Grab the host model argument list
        self._host_arg_list_full = host_model.argument_list()
        self._host_arg_list_noloop = host_model.argument_list(loop_vars=False)
        # First pass, create init, run, and finalize sequences
        for item in self.groups:
            if item.name in self._full_groups:
                phase = self._full_groups[item.name].phase()
            else:
                phase = _RUN_PHASE_NAME
            # end if
            lmsg = "Group {}, schemes = {}"
            self.__logger.debug(lmsg.format(item.name,
                                            [x.name for x in item.schemes()]))
            item.analyze(phase, self, scheme_library, ddt_library, logger)
            # Look for group variables that need to be promoted to the suite
            # We need to promote any variable used later to the suite, however,
            # we do not yet know if it will be used.
            # Add new group-created variables
            gvars = item.variable_list()
            for gvar in gvars:
                stdname = gvar.get_prop_value('standard_name')
                if not stdname in self._gvar_stdnames:
                    self._gvar_stdnames[stdname] = item
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

    def write(self, output_dir, logger):
        """Create caps for all groups in the suite and for the entire suite
        (calling the group caps one after another)"""
        # Set name of module and filename of cap
        filename = '{module_name}.F90'.format(module_name=self.module)
        logger.debug('Writing CCPP suite file, {}'.format(filename))
        # Init
        output_file_name = os.path.join(output_dir, filename)
        with FortranWriter(output_file_name, 'w') as outfile:
            # Write suite header
            outfile.write(COPYRIGHT, 0)
            outfile.write(Suite.__header.format(module=self.module), 0)
            # Write module 'use' statements here
            outfile.write('use {}'.format(KINDS_MODULE), 1)
            # Look for any DDT types
            self._ddt_library.write_ddt_use_statements(self.values(),
                                                       outfile, 1)
            outfile.write('implicit none\nprivate\n\n! Suite interfaces', 1)
            line = Suite.__state_machine_init
            var_name = Suite.__state_machine_var_name
            var_state = Suite.__state_machine_initial_state
            outfile.write(line.format(css_var_name=var_name,
                                      state=var_state), 1)
            for group in self._groups:
                outfile.write('public :: {}'.format(group.name), 1)
            # end for
            outfile.write('\n! Private suite variables', 1)
            for svar in self.keys():
                self[svar].write_def(outfile, 1, self, allocatable=True)
            # end for
            outfile.write('\ncontains', 0)
            for group in self._groups:
                if group.name in self._beg_groups:
                    group.write(outfile, logger, self._host_arg_list_noloop, 1,
                                suite_vars=self, allocate=True)
                elif group.name in self._end_groups:
                    group.write(outfile, logger, self._host_arg_list_noloop, 1,
                                suite_vars=self, deallocate=True)
                else:
                    group.write(outfile, logger, self._host_arg_list_full, 1)
                # end if
            # end for
            # Finish off the module
            outfile.write(Suite.__footer.format(module=self.module), 0)
            return output_file_name

###############################################################################

class API(VarDictionary):
    """Class representing the API for the CCPP framework.
    The API class organizes the suites for which CAPS will be generated"""

    __suite_fname = 'ccpp_physics_suite_list'
    __part_fname = 'ccpp_physics_suite_part_list'
    __vars_fname = 'ccpp_physics_suite_variables'

    __header = '''
!>
!! @brief Auto-generated API for {host_model} calls to CCPP suites
!!
!
module {module}
'''
    __preamble = '''
{module_use}

implicit none
private

'''

    __sub_name_template = 'ccpp_physics'

    __subhead = 'subroutine {subname}({api_call_list})'

    __subfoot = 'end subroutine {subname}\n'

    __footer = '''
end module {module}
'''

    # Note, we cannot add these vars to our dictionary as we do not want
    #    them showing up in group dummy arg lists
    __suite_name = Var({'local_name':'suite_name',
                        'standard_name':'suite_name',
                        'intent':'in', 'type':'character',
                        'kind':'len=*', 'units':'',
                        'dimensions':'()'}, _API_SOURCE)

    __suite_part = Var({'local_name':'suite_part',
                        'standard_name':'suite_part',
                        'intent':'in', 'type':'character',
                        'kind':'len=*', 'units':'',
                        'dimensions':'()'}, _API_SOURCE)

    def __init__(self, sdfs, host_model, scheme_headers, logger):
        self._module = 'ccpp_physics_api'
        self._host = host_model
        self._suites = list()
        super(API, self).__init__(self.module, parent_dict=host_model,
                                  logger=logger)
        # Create a usable library out of scheme_headers
        # Structure is dictionary of dictionaries
        # Top-level dictionary is keyed by function name
        # Secondary level is by phase
        scheme_library = {}
        # First, process DDT headers
        self._ddt_lib = DDTLibrary('{}_api'.format(host_model.name),
                                   ddts=[d for d in scheme_headers
                                         if d.header_type == 'ddt'],
                                   logger=logger)
        for header in [d for d in scheme_headers if d.header_type != 'ddt']:
            if header.header_type != 'scheme':
                errmsg = "{} is an unknown CCPP API metadata header type, {}"
                raise CCPPError(errmsg.format(header.title, header.header_type))
            # end if
            func_id, _, match_trans = CCPP_STATE_MACH.function_match(header.title)
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
            suite = Suite(sdf, self, logger)
            suite.analyze(host_model, scheme_library, self._ddt_lib, logger)
            self._suites.append(suite)
        # end for
        # We will need the correct names for errmsg and errflg
        evar = host_model.find_variable('ccpp_error_message')
        subst_dict = {'intent':'out'}
        if evar is not None:
            self._errmsg_var = evar.clone(subst_dict)
        else:
            raise CCPPError('Required variable, ccpp_error_message, not found')
        # end if
        evar = host_model.find_variable('ccpp_error_flag')
        if evar is not None:
            self._errflg_var = evar.clone(subst_dict)
        else:
            raise CCPPError('Required variable, ccpp_error_flag, not found')
        # end if
        # We need a call list for every phase
        self.__call_lists = {}
        for phase in CCPP_STATE_MACH.transitions():
            self.__call_lists[phase] = CallList('API_' + phase, logger)
            self.__call_lists[phase].add_variable(self.suite_name_var)
            if phase == _RUN_PHASE_NAME:
                self.__call_lists[phase].add_variable(self.suite_part_var)
            # end if
            for suite in self._suites:
                for group in suite.groups:
                    if group.phase() == phase:
                        self.__call_lists[phase].add_vars(group.call_list,
                                                          gen_unique=True)
                    # end if
                # end for
             # end for
        # end for

    @property
    def module(self):
        """Get the module name of the API."""
        return self._module

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
        return self._suites

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

    def write(self, output_dir, logger):
        """Write CCPP API module"""
        if not self.suites:
            raise CCPPError("No suite specified for generating API")
        # end if
        api_filenames = list()
        # Write out the suite files
        for suite in self.suites:
            out_file_name = suite.write(output_dir, logger)
            api_filenames.append(out_file_name)
        # end for
        return api_filenames

    @classmethod
    def declare_inspection_interfaces(cls, ofile):
        """Declare the API interfaces for the suite inquiry functions"""
        ofile.write("public :: {}".format(API.__suite_fname), 1)
        ofile.write("public :: {}".format(API.__part_fname), 1)
        ofile.write("public :: {}".format(API.__vars_fname), 1)

    def get_errinfo_names(self):
        """Return a tuple of error output local names"""
        errmsg_name = self._errmsg_var.get_prop_value('local_name')
        errflg_name = self._errflg_var.get_prop_value('local_name')
        return (errmsg_name, errflg_name)

    @staticmethod
    def write_var_set_loop(ofile, varlist_name, var_list, indent):
        """Write code to allocate and set <varlist_name> to <var_list>"""
        ofile.write("allocate({}({}))".format(varlist_name, len(var_list)),
                    indent)
        for ind, var in enumerate(var_list):
            ofile.write("{}({}) = '{}'".format(varlist_name, ind+1, var),
                        indent)
        # end for

    def write_inspection_routines(self, ofile):
        """Write the list_suites and list_suite_parts subroutines"""
        errmsg_name, errflg_name = self.get_errinfo_names()
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
        oline = "suite_name, part_list, {errmsg}, {errflg}"
        inargs = oline.format(errmsg=errmsg_name, errflg=errflg_name)
        ofile.write("\nsubroutine {}({})".format(API.__part_fname, inargs), 1)
        oline = "character(len=*),              intent(in)  :: suite_name"
        ofile.write(oline, 2)
        oline = "character(len=*), allocatable, intent(out) :: part_list(:)"
        ofile.write(oline, 2)
        self._errmsg_var.write_def(ofile, 2, self)
        self._errflg_var.write_def(ofile, 2, self)
        else_str = ''
        ename = self._errflg_var.get_prop_value('local_name')
        ofile.write("{} = 0".format(ename), 2)
        ename = self._errmsg_var.get_prop_value('local_name')
        ofile.write("{} = ''".format(ename), 2)
        for suite in self.suites:
            oline = "{}if(trim(suite_name) == '{}') then"
            ofile.write(oline.format(else_str, suite.name), 2)
            API.write_var_set_loop(ofile, 'part_list', suite.part_list(), 3)
            else_str = 'else '
        # end for
        ofile.write("else", 2)
        emsg = "write({errmsg}, '(3a)')".format(errmsg=errmsg_name)
        emsg += "'No suite named ', trim(suite_name), ' found'"
        ofile.write(emsg, 3)
        ofile.write("{errflg} = 1".format(errflg=errflg_name), 3)
        ofile.write("end if", 2)
        ofile.write("end subroutine {}".format(API.__part_fname), 1)
        # Write out the suite required variable subroutine
        oline = "suite_name, variable_list, {errmsg}, {errflg}"
        oline += ", input_vars_in, output_vars_in"
        inargs = oline.format(errmsg=errmsg_name, errflg=errflg_name)
        ofile.write("\nsubroutine {}({})".format(API.__vars_fname, inargs), 1)
        ofile.write("! Dummy arguments", 2)
        oline = "character(len=*),              intent(in)  :: suite_name"
        ofile.write(oline, 2)
        oline = "character(len=*), allocatable, intent(out) :: variable_list(:)"
        ofile.write(oline, 2)
        self._errmsg_var.write_def(ofile, 2, self)
        self._errflg_var.write_def(ofile, 2, self)
        oline = "logical, optional,             intent(in) :: input_vars_in"
        ofile.write(oline, 2)
        oline = "logical, optional,             intent(in) :: output_vars_in"
        ofile.write(oline, 2)
        ofile.write("! Local variables", 2)
        ofile.write("logical {}:: input_vars".format(' '*34), 2)
        ofile.write("logical {}:: output_vars".format(' '*34), 2)
        ofile.write("\n", 0)
        ename = self._errflg_var.get_prop_value('local_name')
        ofile.write("{} = 0".format(ename), 2)
        ename = self._errmsg_var.get_prop_value('local_name')
        ofile.write("{} = ''".format(ename), 2)
        ofile.write("if (present(input_vars_in)) then", 2)
        ofile.write("input_vars = input_vars_in", 3)
        ofile.write("else", 2)
        ofile.write("input_vars = .true.", 3)
        ofile.write("end if", 2)
        ofile.write("if (present(output_vars_in)) then", 2)
        ofile.write("output_vars = output_vars_in", 3)
        ofile.write("else", 2)
        ofile.write("output_vars = .true.", 3)
        ofile.write("end if", 2)
        else_str = ''
        for suite in self.suites:
            # Collect all the suite variables
            oline = "{}if(trim(suite_name) == '{}') then"
            suite_vars = [set(), set()] # input, output
            for part in suite.groups:
                for var in part.call_list.variable_list():
                    stdname = var.get_prop_value("standard_name")
                    intent = var.get_prop_value("intent")
                    if intent in ['in', 'inout']:
                        suite_vars[0].add(stdname)
                    # end if
                    if intent in ['inout', 'out']:
                        suite_vars[1].add(stdname)
                    # end if
                # end for
            # end for
            ofile.write(oline.format(else_str, suite.name), 2)
            ofile.write("if (input_vars .and. output_vars) then", 3)
            var_list = list(suite_vars[0].union(suite_vars[1]))
            API.write_var_set_loop(ofile, 'variable_list', var_list, 4)
            ofile.write("else if (input_vars) then", 3)
            var_list = list(suite_vars[0])
            API.write_var_set_loop(ofile, 'variable_list', var_list, 4)
            ofile.write("else if (output_vars) then", 3)
            var_list = list(suite_vars[1])
            API.write_var_set_loop(ofile, 'variable_list', var_list, 4)
            ofile.write("else", 3)
            ofile.write("allocate(variable_list(0))\n", 4)
            ofile.write("end if", 3)
            else_str = 'else '
        # end for
        ofile.write("else", 2)
        emsg = "write({errmsg}, '(3a)')".format(errmsg=errmsg_name)
        emsg += "'No suite named ', trim(suite_name), ' found'"
        ofile.write(emsg, 3)
        ofile.write("{errflg} = 1".format(errflg=errflg_name), 3)
        ofile.write("end if", 2)
        ofile.write("end subroutine {}".format(API.__vars_fname), 1)

###############################################################################
if __name__ == "__main__":
    # pylint: disable=ungrouped-imports
    from parse_tools import init_log, set_log_to_null
    LOGGING = init_log('ccpp_suite')
    set_log_to_null(LOGGING)
    try:
        # First, run doctest
        import doctest
        doctest.testmod()
        # Goal: Replace this test with a suite from unit tests
        FRAME_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        CAM = os.path.dirname(FRAME_ROOT)
        KESSLER = os.path.join(CAM, 'src', 'physics', 'ncar_ccpp',
                               'suite_kessler.xml')
        if os.path.exists(KESSLER):
            _ = Suite(KESSLER, VarDictionary('Kessler'), LOGGING)
        else:
            print("Cannot find test file, '{}', skipping test".format(KESSLER))
    except CCPPError as suite_error:
        print("{}".format(suite_error))
# end if (no else)
