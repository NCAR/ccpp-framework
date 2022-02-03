#!/usr/bin/env python3
#

import collections
import copy
import getopt
import filecmp
import logging
import os
import re
import sys
import types
import xml.etree.ElementTree as ET

from common import encode_container
from common import CCPP_STAGES
from common import CCPP_ERROR_FLAG_VARIABLE, CCPP_ERROR_MSG_VARIABLE, CCPP_LOOP_COUNTER, CCPP_LOOP_EXTENT
from common import CCPP_BLOCK_NUMBER, CCPP_BLOCK_COUNT, CCPP_BLOCK_SIZES, CCPP_THREAD_NUMBER, CCPP_INTERNAL_VARIABLES
from common import CCPP_CONSTANT_ONE, CCPP_HORIZONTAL_DIMENSION, CCPP_HORIZONTAL_LOOP_EXTENT
from common import FORTRAN_CONDITIONAL_REGEX_WORDS, FORTRAN_CONDITIONAL_REGEX
from common import CCPP_TYPE, STANDARD_VARIABLE_TYPES, STANDARD_CHARACTER_TYPE
from common import CCPP_STATIC_API_MODULE, CCPP_STATIC_SUBROUTINE_NAME
from mkcap import Var

###############################################################################

# Maximum number of dimensions of an array allowed by the Fortran 2008 standard
FORTRAN_ARRAY_MAX_DIMS = 15

###############################################################################

def extract_parents_and_indices_from_local_name(local_name):
    """Break apart local_name into the different components (members of DDTs)
    to determine all variables that are required; this must work for complex
    constructs such as Atm(mytile)%q(:,:,:,Atm2(mytile2)%graupel), with 
    result parent = 'Atm', indices = [mytile, Atm2, mytile2]"""
    # First, extract all variables/indices in parentheses (used for subsetting)
    indices = []
    while '(' in local_name:
        for i in range(len(local_name)):
            if local_name[i] == '(':
                last_open = i
            elif local_name[i] == ')':
                last_closed = i
                break
        index_set = local_name[last_open+1:last_closed].split(',')
        for index_group in index_set:
            for index in index_group.split(':'):
                if index:
                    if '%' in index:
                        indices.append(index[:index.find('%')])
                    else:
                        # Skip hard-coded integers that are not variables
                        try:
                            int(index)
                        except ValueError:
                            indices.append(index)
        # Remove this innermost index group (...) from local_name
        local_name = local_name.replace(local_name[last_open:last_closed+1], '')
    # Remove duplicates from indices
    indices = list(set(indices))
    # Derive parent of actual variable (now that all subsets have been processed)
    if '%' in local_name:
        parent = local_name[:local_name.find('%')]
    else:
        parent = local_name
    return (parent, indices)

def create_argument_list_wrapped(arguments):
    """Create a wrapped argument list, remove trailing ',' """
    argument_list = ''
    length = 0
    for argument in arguments:
        argument_list += argument + ','
        length += len(argument)+1
        # Split args so that lines don't exceed 260 characters (for PGI)
        if length > 70 and not argument == arguments[-1]:
            argument_list += ' &\n                  '
            length = 0
    if argument_list:
        argument_list = argument_list.rstrip(',')
    return argument_list

def create_argument_list_wrapped_explicit(arguments, additional_vars_following = False):
    """Create a wrapped argument list with explicit arguments x=y. If no additional
    variables are added (additional_vars_following == False), remove trailing ',' """
    argument_list = ''
    length = 0
    for argument in arguments:
        argument_list += argument + '=' + argument + ','
        length += 2*len(argument)+2
        # Split args so that lines don't exceed 260 characters (for PGI)
        if length > 70 and not argument == arguments[-1]:
            argument_list += ' &\n                  '
            length = 0
    if argument_list and not additional_vars_following:
        argument_list = argument_list.rstrip(',')
    return argument_list

def create_arguments_module_use_var_defs(variable_dictionary, metadata_define, tmpvars = None):
    """Given a dictionary of standard names and variables, and a metadata
    dictionary with the variable definitions by the host model, create a list
    of arguments (local names), module use statements (for derived data types
    and non-standard kinds), and the variable definition statements."""
    arguments = []
    module_use = []
    var_defs = []
    local_kind_and_type_vars = []

    for standard_name in variable_dictionary.keys():
        # Add variable local name and variable definitions
        arguments.append(variable_dictionary[standard_name].local_name)
        var_defs.append(variable_dictionary[standard_name].print_def_intent())
        # Add special kind variables and derived data type definitions to module use statements
        if variable_dictionary[standard_name].type in STANDARD_VARIABLE_TYPES and variable_dictionary[standard_name].kind \
                and not variable_dictionary[standard_name].type == STANDARD_CHARACTER_TYPE:
            kind_var_standard_name = variable_dictionary[standard_name].kind
            if not kind_var_standard_name in local_kind_and_type_vars:
                if not kind_var_standard_name in metadata_define.keys():
                    raise Exception("Kind {kind} not defined by host model".format(kind=kind_var_standard_name))
                kind_var = metadata_define[kind_var_standard_name][0]
                module_use.append(kind_var.print_module_use())
                local_kind_and_type_vars.append(kind_var_standard_name)
        elif not variable_dictionary[standard_name].type in STANDARD_VARIABLE_TYPES:
            type_var_standard_name = variable_dictionary[standard_name].type
            if not type_var_standard_name in local_kind_and_type_vars:
                if not type_var_standard_name in metadata_define.keys():
                    raise Exception("Type {type} not defined by host model".format(type=type_var_standard_name))
                type_var = metadata_define[type_var_standard_name][0]
                module_use.append(type_var.print_module_use())
                local_kind_and_type_vars.append(type_var_standard_name)

    # Add any local variables (required for unit conversions, array transformations, ...)
    if tmpvars:
        var_defs.append('')
        var_defs.append('! Local variables for unit conversions, array transformations, ...')
        for tmpvar in tmpvars:
            var_defs.append(tmpvar.print_def_local())
            # Add special kind variables
            if tmpvar.type in STANDARD_VARIABLE_TYPES and tmpvar.kind and not tmpvar.type == STANDARD_CHARACTER_TYPE:
                kind_var_standard_name = tmpvar.kind
                if not kind_var_standard_name in local_kind_and_type_vars:
                    if not kind_var_standard_name in metadata_define.keys():
                        raise Exception("Kind {kind} not defined by host model".format(kind=kind_var_standard_name))
                    kind_var = metadata_define[kind_var_standard_name][0]
                    module_use.append(kind_var.print_module_use())
                    local_kind_and_type_vars.append(kind_var_standard_name)

    return (arguments, module_use, var_defs)

class API(object):

    header='''
!
! This work (Common Community Physics Package), identified by NOAA, NCAR,
! CU/CIRES, is free of known copyright restrictions and is placed in the
! public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!

!>
!! @brief Auto-generated API for the CCPP static build
!!
!
module {module}

{module_use}
   implicit none

   private
   public :: {subroutines}

   contains
'''

    sub = '''
   subroutine {subroutine}({ccpp_var_name}, suite_name, group_name, ierr)

      use ccpp_types, only : ccpp_t

      implicit none

      type(ccpp_t),               intent(inout) :: {ccpp_var_name}
      character(len=*),           intent(in)    :: suite_name
      character(len=*), optional, intent(in)    :: group_name
      integer,                    intent(out)   :: ierr

      ierr = 0

{suite_switch}
      else

         write({ccpp_var_name}%errmsg,'(*(a))') 'Invalid suite ' // trim(suite_name)
         ierr = 1

      end if

      {ccpp_var_name}%errflg = ierr

   end subroutine {subroutine}
'''

    footer = '''
end module {module}
'''

    def __init__(self, **kwargs):
        self._filename    = CCPP_STATIC_API_MODULE + '.F90'
        self._module      = CCPP_STATIC_API_MODULE
        self._subroutines = None
        self._suites      = []
        self._directory   = '.'
        self._update_api  = True
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    @property
    def filename(self):
        '''Get the filename to write API to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

    @property
    def directory(self):
        '''Get the directory to write API to.'''
        return self._directory

    @directory.setter
    def directory(self, value):
        self._directory = value

    @property
    def update_api(self):
        '''Get the update_api flag.'''
        return self._update_api

    @update_api.setter
    def update_api(self, value):
        self._update_api = value

    @property
    def module(self):
        '''Get the module name of the API.'''
        return self._module

    @property
    def subroutines(self):
        '''Get the subroutines names of the API to.'''
        return self._subroutines

    def write(self):
        """Write API for static build"""
        if not self._suites:
            raise Exception("No suites specified for generating API")
        suites = self._suites

        # Module use statements for suite and group caps
        module_use = ''
        for suite in suites:
            for subroutine in suite.subroutines:
                module_use += '   use {module}, only: {subroutine}\n'.format(module=suite.module, subroutine=subroutine)
            for group in suite.groups:
                for subroutine in group.subroutines:
                    module_use += '   use {module}, only: {subroutine}\n'.format(module=group.module, subroutine=subroutine)

        # Add all variables required to module use statements. This is for the API only,
        # because the static API imports all variables from modules instead of receiving them
        # via the argument list. Special handling for a single variable of type CCPP_TYPE (ccpp_t),
        # which comes in as a scalar for any potential block/thread via the argument list.
        ccpp_var = None
        parent_standard_names = []
        for ccpp_stage in CCPP_STAGES.keys():
            for suite in suites:
                for parent_standard_name in suite.parents[ccpp_stage].keys():
                    if not parent_standard_name in parent_standard_names:
                        parent_var = suite.parents[ccpp_stage][parent_standard_name]
                        # Identify which variable is of type CCPP_TYPE (need local name)
                        if parent_var.type == CCPP_TYPE:
                            if ccpp_var and not ccpp_var.local_name==parent_var.local_name:
                                raise Exception('There can be only one variable of type {0}, found {1} and {2}'.format(
                                                CCPP_TYPE, ccpp_var.local_name, parent_var.local_name))
                            ccpp_var = parent_var
                            continue
                        module_use += '   {0}\n'.format(parent_var.print_module_use())
                        parent_standard_names.append(parent_standard_name)
        if not ccpp_var:
            raise Exception('No variable of type {0} found - need a scalar instance.'.format(CCPP_TYPE))
        elif not ccpp_var.rank == '':
            raise Exception('CCPP variable {0} of type {1} must be a scalar.'.format(ccpp_var.local_name, CCPP_TYPE))
        del parent_standard_names

        # Create a subroutine for each stage
        self._subroutines=[]
        subs = ''
        for ccpp_stage in CCPP_STAGES.keys():
            suite_switch = ''
            for suite in suites:
                # Calls to groups of schemes for this stage
                group_calls = ''
                for group in suite.groups:
                    # The <init></init> and <finalize></finalize> groups require special treatment,
                    # since they can only be run in the respective stage (init/finalize)
                    if (group.init and not ccpp_stage == 'init') or \
                        (group.finalize and not ccpp_stage == 'finalize'):
                        continue
                    if not group_calls:
                        clause = 'if'
                    else:
                        clause = 'else if'
                    argument_list_group = create_argument_list_wrapped_explicit(group.arguments[ccpp_stage])
                    group_calls += '''
            {clause} (trim(group_name)=="{group_name}") then
               ierr = {suite_name}_{group_name}_{stage}_cap({arguments})'''.format(clause=clause,
                                                                                   suite_name=group.suite,
                                                                                   group_name=group.name,
                                                                                   stage=CCPP_STAGES[ccpp_stage],
                                                                                   arguments=argument_list_group)
                group_calls += '''
            else
               write({ccpp_var_name}%errmsg, '(*(a))') 'Group ' // trim(group_name) // ' not found'
               ierr = 1
            end if
'''.format(ccpp_var_name=ccpp_var.local_name, group_name=group.name)

                # Call to entire suite for this stage

                # Create argument list for calling the full suite
                argument_list_suite = create_argument_list_wrapped_explicit(suite.arguments[ccpp_stage])
                suite_call = '''
           ierr = {suite_name}_{stage}_cap({arguments})
'''.format(suite_name=suite.name, stage=CCPP_STAGES[ccpp_stage], arguments=argument_list_suite)

                # Add call to all groups of this suite and to the entire suite
                if not suite_switch:
                    clause = 'if'
                else:
                    clause = 'else if'
                suite_switch += '''
      {clause} (trim(suite_name)=="{suite_name}") then

         if (present(group_name)) then
{group_calls}
         else
{suite_call}
         end if
'''.format(clause=clause, suite_name=suite.name, group_calls=group_calls, suite_call=suite_call)

            subroutine = CCPP_STATIC_SUBROUTINE_NAME.format(stage=ccpp_stage)
            self._subroutines.append(subroutine)
            subs += API.sub.format(subroutine=subroutine,
                                   ccpp_var_name=ccpp_var.local_name,
                                   suite_switch=suite_switch)

        # Write output to stdout or file
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            # If the file exists, write to temporary file first and compare them:
            # - if identical, delete the temporary file and keep the existing one
            #   and set the API update flag to false
            # - if different, replace existing file with temporary file and set
            #   the API update flag to true (default value)
            # - always replace the file if any of the suite caps has changed
            # If the file does not exist, write the API an set the flag to true
            if os.path.isfile(self.filename) and \
                    not any([suite.update_cap for suite in suites]):
                write_to_test_file = True
                test_filename = self.filename + '.test'
                f = open(test_filename, 'w')
            else:
                write_to_test_file = False
                f = open(self.filename, 'w')
        else:
            f = sys.stdout
        f.write(API.header.format(module=self._module,
                                  module_use=module_use,
                                  subroutines=','.join(self._subroutines)))
        f.write(subs)
        f.write(Suite.footer.format(module=self._module))
        if (f is not sys.stdout):
            f.close()
            # See comment above on updating the API or not
            if write_to_test_file:
                if filecmp.cmp(self.filename, test_filename):
                    # Files are equal, delete the test API and set update flag to False
                    os.remove(test_filename)
                    self.update_api = False
                else:
                    # Files are different, replace existing API with
                    # the test API and set update flag to True
                    # Python 3 only: os.replace(test_filename, self.filename)
                    os.remove(self.filename)
                    os.rename(test_filename, self.filename)
                    self.update_api = True
            else:
                self.update_api = True
        return

    def write_includefile(self, source_filename, type):
        success = True
        filepath = os.path.split(source_filename)[0]
        if filepath and not os.path.isdir(filepath):
            os.makedirs(filepath)
        # If the file exists, write to temporary file first and compare them:
        # - if identical, delete the temporary file and keep the existing one
        # - if different, replace existing file with temporary file
        # - however, always replace the file if the API update flag is true
        if os.path.isfile(source_filename) and not self.update_api:
            write_to_test_file = True
            test_filename = source_filename + '.test'
            f = open(test_filename, 'w')
        else:
            write_to_test_file = False
            f = open(source_filename, 'w')

        if type == 'shell':
            # Contents of shell/source file
            contents = """# The CCPP static API is defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
export CCPP_STATIC_API=\"{filename}\"
""".format(filename=os.path.abspath(os.path.join(self.directory,self.filename)))
        elif type == 'cmake':
            # Contents of cmake include file
            contents = """# The CCPP static API is defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
set(API \"{filename}\")
""".format(filename=os.path.abspath(os.path.join(self.directory,self.filename)))
        else:
            logging.error('Encountereed unknown type of file "{type}" when writing include file for static API'.format(type=type))
            success = False
            return

        f.write(contents)
        f.close()
        # See comment above on updating the API or not
        if write_to_test_file:
            if filecmp.cmp(source_filename, test_filename):
                # Files are equal, delete the test file
                os.remove(test_filename)
            else:
                # Files are different, replace existing file
                # Python 3 only: os.replace(test_filename, source_filename)
                os.remove(source_filename)
                os.rename(test_filename, source_filename)
        return success


class Suite(object):

    header='''
!
! This work (Common Community Physics Package), identified by NOAA, NCAR,
! CU/CIRES, is free of known copyright restrictions and is placed in the
! public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!

!>
!! @brief Auto-generated cap module for the CCPP suite
!!
!
module {module}

{module_use}

   implicit none

   private
   public :: {subroutines}

   contains
'''

    sub = '''
   function {subroutine}({arguments}) result(ierr)

      {module_use}

      implicit none

      integer :: ierr
      {var_defs}

      ierr = 0

{body}

   end function {subroutine}
'''

    footer = '''
end module {module}
'''

    def __init__(self, **kwargs):
        self._name = None
        self._filename = sys.stdout
        self._sdf_name = None
        self._all_schemes_called = None
        self._all_subroutines_called = None
        self._caps = None
        self._module = None
        self._subroutines = None
        self._parents = { ccpp_stage : collections.OrderedDict() for ccpp_stage in CCPP_STAGES.keys() }
        self._arguments = { ccpp_stage : [] for ccpp_stage in CCPP_STAGES.keys() }
        self._update_cap = True
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    @property
    def name(self):
        '''Get the name of the suite.'''
        return self._name

    @property
    def sdf_name(self):
        '''Get the name of the suite definition file.'''
        return self._sdf_name

    @sdf_name.setter
    def sdf_name(self, value):
        self._sdf_name = value

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

    @property
    def update_cap(self):
        '''Get the update_cap flag.'''
        return self._update_cap

    @update_cap.setter
    def update_cap(self, value):
        self._update_cap = value

    def parse(self):
        '''Parse the suite definition file.'''
        success = True

        if not os.path.exists(self._sdf_name):
            logging.critical("Suite definition file {0} not found.".format(self._sdf_name))
            success = False
            return success

        tree = ET.parse(self._sdf_name)
        suite_xml = tree.getroot()
        self._name = suite_xml.get('name')
        # Validate name of suite in XML tag against filename; could be moved to common.py
        if not (os.path.basename(self._sdf_name) == 'suite_{}.xml'.format(self._name)):
            logging.critical("Invalid suite name {0} in suite definition file {1}.".format(
                                                               self._name, self._sdf_name))
            success = False
            return success

        # Flattened lists of all schemes and subroutines in SDF
        self._all_schemes_called = []
        self._all_subroutines_called = []

        # Build hierarchical structure as in SDF
        self._groups = []
        for group_xml in suite_xml:
            subcycles = []

            # Add suite-wide init scheme to group 'init', similar for finalize
            if group_xml.tag.lower() == 'init' or group_xml.tag.lower() == 'finalize':
                self._all_schemes_called.append(group_xml.text)
                self._all_subroutines_called.append(group_xml.text + '_' + group_xml.tag.lower())
                schemes = [group_xml.text]
                subcycles.append(Subcycle(loop=1, schemes=schemes))
                if group_xml.tag.lower() == 'init':
                    self._groups.append(Group(name=group_xml.tag.lower(), subcycles=subcycles, suite=self._name, init=True))
                elif group_xml.tag.lower() == 'finalize':
                    self._groups.append(Group(name=group_xml.tag.lower(), subcycles=subcycles, suite=self._name, finalize=True))
                continue

            # Parse subcycles of all regular groups
            for subcycle_xml in group_xml:
                schemes = []
                for scheme_xml in subcycle_xml:
                    self._all_schemes_called.append(scheme_xml.text)
                    schemes.append(scheme_xml.text)
                    loop=int(subcycle_xml.get('loop'))
                    for ccpp_stage in CCPP_STAGES:
                        self._all_subroutines_called.append(scheme_xml.text + '_' + CCPP_STAGES[ccpp_stage])
                subcycles.append(Subcycle(loop=loop, schemes=schemes))

            self._groups.append(Group(name=group_xml.get('name'), subcycles=subcycles, suite=self._name))

        # Remove duplicates from list of all subroutines an schemes
        self._all_schemes_called = list(set(self._all_schemes_called))
        self._all_subroutines_called = list(set(self._all_subroutines_called))

        return success

    def print_debug(self):
        '''Basic debugging output about the suite.'''
        print("ALL SUBROUTINES:")
        print(self._all_subroutines_called)
        print("STRUCTURED:")
        print(self._groups)
        for group in self._groups:
            group.print_debug()

    @property
    def all_schemes_called(self):
        '''Get the list of all schemes.'''
        return self._all_schemes_called

    @property
    def all_subroutines_called(self):
        '''Get the list of all subroutines.'''
        return self._all_subroutines_called

    @property
    def module(self):
        '''Get the list of the module generated for this suite.'''
        return self._module

    @property
    def subroutines(self):
        '''Get the list of all subroutines generated for this suite.'''
        return self._subroutines

    @property
    def caps(self):
        '''Get the list of all caps.'''
        return self._caps

    @property
    def groups(self):
        '''Get the list of groups in this suite.'''
        return self._groups

    @property
    def parents(self):
        '''Get the parent variables for the suite.'''
        return self._parents

    @parents.setter
    def parents(self, value):
        self._parents = value

    @property
    def arguments(self):
        '''Get the argument list for the suite.'''
        return self._arguments

    @arguments.setter
    def arguments(self, value):
        self._arguments = value

    def write(self, metadata_request, metadata_define, arguments, debug):
        """Create caps for all groups in the suite and for the entire suite
        (calling the group caps one after another). Add additional code for
        debugging if debug flag is True."""
        # Set name of module and filename of cap
        self._module = 'ccpp_{suite_name}_cap'.format(suite_name=self._name)
        self.filename = '{module_name}.F90'.format(module_name=self._module)
        # Init
        self._subroutines = []
        # Write group caps and generate module use statements; combine the argument lists
        # and variable definitions for all groups into a suite argument list. This may
        # require adjusting the intent of the variables.
        module_use = ''
        for group in self._groups:
            group.write(metadata_request, metadata_define, arguments, debug)
            for subroutine in group.subroutines:
                module_use += '   use {m}, only: {s}\n'.format(m=group.module, s=subroutine)
            for ccpp_stage in CCPP_STAGES.keys():
                for parent_standard_name in group.parents[ccpp_stage].keys():
                    if parent_standard_name in self.parents[ccpp_stage]:
                        if self.parents[ccpp_stage][parent_standard_name].intent == 'in' and \
                            not group.parents[ccpp_stage][parent_standard_name].intent == 'in':
                            self.parents[ccpp_stage][parent_standard_name].intent = 'inout'
                        elif self.parents[ccpp_stage][parent_standard_name].intent == 'out' and \
                            not group.parents[ccpp_stage][parent_standard_name].intent == 'out':
                            self.parents[ccpp_stage][parent_standard_name].intent = 'inout'
                    else:
                        self.parents[ccpp_stage][parent_standard_name] = copy.deepcopy(group.parents[ccpp_stage][parent_standard_name])
        subs = ''
        for ccpp_stage in CCPP_STAGES.keys():
            # Create a wrapped argument list for calling the suite,
            # get module use statements and variable definitions
            (self.arguments[ccpp_stage], sub_module_use, sub_var_defs) = \
                create_arguments_module_use_var_defs(self.parents[ccpp_stage], metadata_define)
            argument_list_suite = create_argument_list_wrapped(self.arguments[ccpp_stage])
            body = ''
            for group in self._groups:
                # Groups 'init'/'finalize' are only run in stages 'init'/'finalize'
                if (group.init and not ccpp_stage == 'init') or \
                    (group.finalize and not ccpp_stage == 'finalize'):
                    continue
                # Create a wrapped argument list for calling the group
                (arguments_group, dummy, dummy) = create_arguments_module_use_var_defs(group.parents[ccpp_stage], metadata_define)
                argument_list_group = create_argument_list_wrapped_explicit(arguments_group)

                # Write to body that calls the groups for this stage
                body += '''
      ierr = {suite_name}_{group_name}_{stage}_cap({arguments})
      if (ierr/=0) return
'''.format(suite_name=self._name, group_name=group.name, stage=CCPP_STAGES[ccpp_stage], arguments=argument_list_group)
            # Add name of subroutine in the suite cap to list of subroutine names
            subroutine = '{name}_{stage}_cap'.format(name=self._name, stage=CCPP_STAGES[ccpp_stage])
            self._subroutines.append(subroutine)
            # Add subroutine to output
            subs += Suite.sub.format(subroutine=subroutine,
                                     arguments=argument_list_suite,
                                     module_use='\n      '.join(sub_module_use),
                                     var_defs='\n      '.join(sub_var_defs),
                                     body=body)

        # Write cap to stdout or file
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            # If the file exists, write to temporary file first and compare them:
            # - if identical, delete the temporary file and keep the existing one
            #   and set the suite cap update flag to false
            # - if different, replace existing file with temporary file and set
            #   the suite cap update flag to true (default value)
            # - however, if any of the group caps has changed, rewrite the suite
            #   cap as well and set the suite cap update flag to true
            # If the file does not exist, write the cap an set the flag to true
            if os.path.isfile(self.filename) and \
                    not any([group.update_cap for group in self._groups]):
                write_to_test_file = True
                test_filename = self.filename + '.test'
                f = open(test_filename, 'w')
            else:
                write_to_test_file = False
                f = open(self.filename, 'w')
        else:
            f = sys.stdout
        f.write(Suite.header.format(module=self._module,
                                    module_use=module_use,
                                    subroutines=', &\n             '.join(self._subroutines)))
        f.write(subs)
        f.write(Suite.footer.format(module=self._module))
        if (f is not sys.stdout):
            f.close()
            # See comment above on updating the suite cap or not
            if write_to_test_file:
                if filecmp.cmp(self.filename, test_filename):
                    # Files are equal, delete the test cap
                    # and set update flag to False
                    os.remove(test_filename)
                    self.update_cap = False
                else:
                    # Files are different, replace existing cap
                    # with test cap and set flag to True
                    # Python 3 only: os.replace(test_filename, self.filename)
                    os.remove(self.filename)
                    os.rename(test_filename, self.filename)
                    self.update_cap = True
            else:
                self.update_cap = True

        # Create list of all caps generated (for groups and suite)
        self._caps = [ self.filename ]
        for group in self._groups:
            self._caps.append(group.filename)


###############################################################################
class Group(object):

    header='''
!
! This work (Common Community Physics Package), identified by NOAA, NCAR,
! CU/CIRES, is free of known copyright restrictions and is placed in the
! public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!

!>
!! @brief Auto-generated cap module for the CCPP {group} group
!!
!
module {module}

{module_use}

   implicit none

   private
   public :: {subroutines}

   logical, save :: initialized = .false.

   contains
'''

    sub = '''
   function {subroutine}({argument_list}) result(ierr)

      {module_use}

      implicit none

      ! Error handling
      integer :: ierr

      {var_defs}

      ierr = 0

{initialized_test_block}

{body}

{initialized_set_block}

   end function {subroutine}
'''

    footer = '''
end module {module}
'''

    initialized_test_blocks = {
        'init' : '''
      if (initialized) return
''',
        'timestep_init' : '''
      if (.not.initialized) then
        write({target_name_msg},'(*(a))') '{name}_timestep_init called before {name}_init'
        {target_name_flag} = 1
        return
      end if
''',
        'run' : '''
      if (.not.initialized) then
        write({target_name_msg},'(*(a))') '{name}_run called before {name}_init'
        {target_name_flag} = 1
        return
      end if
''',
        'timestep_finalize' : '''
      if (.not.initialized) then
        write({target_name_msg},'(*(a))') '{name}_timestep_finalize called before {name}_init'
        {target_name_flag} = 1
        return
      end if
''',
        'finalize' : '''
      if (.not.initialized) return
''',
    }

    initialized_set_blocks = {
        'init' : '''
      initialized = .true.
''',
        'timestep_init' : '',
        'run' : '',
        'timestep_finalize' : '',
        'finalize' : '''
      initialized = .false.
''',
    }

    def __init__(self, **kwargs):
        self._name = ''
        self._suite = None
        self._filename = sys.stdout
        self._init = False
        self._finalize = False
        self._module = None
        self._subroutines = None
        self._pset = None
        self._parents = { ccpp_stage : collections.OrderedDict() for ccpp_stage in CCPP_STAGES }
        self._arguments = { ccpp_stage : [] for ccpp_stage in CCPP_STAGES }
        self._update_cap = True
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, metadata_request, metadata_define, arguments, debug):
        """Create caps for all stages of this group. Add additional code for
        debugging if debug flag is True."""

        # Create an inverse lookup table of local variable names defined (by the host model) and standard names
        standard_name_by_local_name_define = collections.OrderedDict()
        for standard_name in metadata_define.keys():
            standard_name_by_local_name_define[metadata_define[standard_name][0].local_name] = standard_name

        # First get target names of standard CCPP variables for subcycling and error handling
        if CCPP_LOOP_COUNTER in metadata_request.keys():
            ccpp_loop_counter_target_name = metadata_request[CCPP_LOOP_COUNTER][0].target
        else:
            ccpp_loop_counter_target_name = None
        if CCPP_LOOP_EXTENT in metadata_request.keys():
            ccpp_loop_extent_target_name = metadata_request[CCPP_LOOP_EXTENT][0].target
        else:
            ccpp_loop_extent_target_name = None
        ccpp_error_flag_target_name = metadata_request[CCPP_ERROR_FLAG_VARIABLE][0].target
        ccpp_error_msg_target_name = metadata_request[CCPP_ERROR_MSG_VARIABLE][0].target
        #
        module_use = ''
        self._module = 'ccpp_{suite}_{name}_cap'.format(name=self._name, suite=self._suite)
        self._filename = '{module_name}.F90'.format(module_name=self._module)
        self._subroutines = []
        local_subs = ''
        #
        for ccpp_stage in CCPP_STAGES.keys():
            # The special init and finalize routines are only run in that stage
            if self._init and not ccpp_stage == 'init':
                continue
            elif self._finalize and not ccpp_stage == 'finalize':
                continue
            # For mapping local variable names to standard names
            local_vars = collections.OrderedDict()
            # For mapping temporary variable names (for unit conversions, etc) to local variable names
            tmpvar_cnt = 0
            tmpvars    = collections.OrderedDict()
            #
            body = ''
            # Variable definitions automatically added for subroutines
            var_defs = ''
            # List of manual variable definitions, for example for handling blocked data structures
            var_defs_manual = []
            # Conditionals for variables (used or allocated only under certain conditions)
            conditionals = {}
            #
            for subcycle in self._subcycles:
                subcycle_body = ''
                # Call all schemes
                for scheme_name in subcycle.schemes:
                    # actions_before and actions_after capture operations such
                    # as unit conversions, transformations that have to happen
                    # before and/or after the call to the subroutine (scheme)
                    actions_before = ''
                    actions_after  = ''
                    #
                    module_name = scheme_name
                    subroutine_name = scheme_name + '_' + ccpp_stage
                    container = encode_container(module_name, scheme_name, subroutine_name)
                    # Skip entirely empty routines or non-existent routines
                    if not subroutine_name in arguments[scheme_name].keys() or not arguments[scheme_name][subroutine_name]:
                        continue
                    error_check = ''
                    args = ''
                    length = 0

                    # First identify all dimensions needed to handle the arguments
                    # and add them to the list of required variables for the cap
                    additional_variables_required = []
                    #
                    for var_standard_name in arguments[scheme_name][subroutine_name]:
                        if not var_standard_name in metadata_define.keys():
                            raise Exception('Variable {standard_name} not defined in host model metadata'.format(
                                                                                standard_name=var_standard_name))
                        var = metadata_define[var_standard_name][0]
                        # dim can be 'A', '1', '1:A', ...
                        for dim_expression in var.dimensions:
                            dims = dim_expression.split(':')
                            for dim in dims:
                                dim = dim.lower()
                                try:
                                    dim = int(dim)
                                except ValueError:
                                    if not dim in local_vars.keys() and \
                                            not dim in additional_variables_required + arguments[scheme_name][subroutine_name]:
                                        if not dim in metadata_define.keys():
                                            raise Exception('Dimension {}, required by variable {}, not defined in host model metadata'.format(
                                                                                                                       dim, var_standard_name))
                                        logging.debug("Adding dimension {} for variable {}".format(dim, var_standard_name))
                                        additional_variables_required.append(dim)

                        # If blocked data structures need to be converted, add necessary variables
                        if ccpp_stage in ['init', 'timestep_init', 'timestep_finalize', 'finalize'] and CCPP_INTERNAL_VARIABLES[CCPP_BLOCK_NUMBER] in var.local_name:
                            if not CCPP_BLOCK_COUNT in local_vars.keys() \
                                    and not CCPP_BLOCK_COUNT in additional_variables_required + arguments[scheme_name][subroutine_name]:
                                    logging.debug("Adding variable {} for handling blocked data structures".format(CCPP_BLOCK_COUNT))
                                    additional_variables_required.append(CCPP_BLOCK_COUNT)
                            if not CCPP_HORIZONTAL_LOOP_EXTENT in local_vars.keys() \
                                    and not CCPP_HORIZONTAL_LOOP_EXTENT in additional_variables_required + arguments[scheme_name][subroutine_name]:
                                    logging.debug("Adding variable {} for handling blocked data structures".format(CCPP_HORIZONTAL_LOOP_EXTENT))
                                    additional_variables_required.append(CCPP_HORIZONTAL_LOOP_EXTENT)
                            if not CCPP_HORIZONTAL_DIMENSION in local_vars.keys() \
                                    and not CCPP_HORIZONTAL_DIMENSION in additional_variables_required + arguments[scheme_name][subroutine_name]:
                                    logging.debug("Adding variable {} for handling blocked data structures".format(CCPP_HORIZONTAL_DIMENSION))
                                    additional_variables_required.append(CCPP_HORIZONTAL_DIMENSION)

                        # If the variable is only active/used under certain conditions, add necessary variables
                        # also record the conditional for later use in unit conversions / blocked data conversions.
                        if var.active == 'T':
                            conditional = '.true.'
                        elif var.active == 'F':
                            conditional = '.false.'
                        else:
                            # Convert conditional expression in standard_name format to local names known to the host model
                            conditional = ''
                            # Find all words in the conditional, for each of them look for a matching
                            # standard name in the list of known variables
                            items = FORTRAN_CONDITIONAL_REGEX.findall(var.active)
                            for item in items:
                                item = item.lower()
                                if item in FORTRAN_CONDITIONAL_REGEX_WORDS:
                                    conditional += item
                                else:
                                    # Detect integers, following Python's "easier to ask forgiveness than permission" mentality
                                    try:
                                        int(item)
                                        conditional += item
                                    except ValueError:
                                        if not item in metadata_define.keys():
                                            raise Exception("Variable {} used in conditional for {} not known to host model".format(
                                                                                                           item, var_standard_name))
                                        var2 = metadata_define[item][0]
                                        conditional += var2.local_name
                                        # Add to list of required variables for the cap
                                        if not item in local_vars.keys() \
                                                and not item in additional_variables_required + arguments[scheme_name][subroutine_name]:
                                            logging.debug("Adding variable {} for handling conditionals".format(item))
                                            additional_variables_required.append(item)
                        # Conditionals are identical per requirement, no need to test for consistency again
                        if not var_standard_name in conditionals.keys():
                            conditionals[var_standard_name] = conditional

                    # Extract all variables needed (including indices for components/slices of arrays)
                    for var_standard_name in additional_variables_required + arguments[scheme_name][subroutine_name]:
                        # Pick the correct variable for this module/scheme/subroutine
                        # from the list of requested variable, if it is in that list
                        if var_standard_name in arguments[scheme_name][subroutine_name]:
                            for var in metadata_request[var_standard_name]:
                                if container == var.container:
                                    break
                        # This is a dimension or required variable added automatically (e.g. for handling blocked data)
                        else:
                            # Create a copy of the variable in the metadata dictionary
                            # of host model variables and set necessary default values
                            var = copy.deepcopy(metadata_define[var_standard_name][0])
                            var.intent = 'in'

                        if not var_standard_name in local_vars.keys():
                            # The full name of the variable as known to the host model
                            var_local_name_define = metadata_define[var_standard_name][0].local_name

                            # Break apart var_local_name_define into the different components (members of DDTs)
                            # to determine all variables that are required
                            (parent_local_name_define, parent_local_names_define_indices) = \
                                extract_parents_and_indices_from_local_name(var_local_name_define)

                            parent_standard_name = None
                            parent_var = None
                            # Check for each of the derived parent local names as defined by the host model
                            # if they are registered (i.e. if there is a standard name for it). Note that
                            # the output of extract_parents_and_indices_from_local_name is stripped of any
                            # array subset information, i.e. a local name 'Atm(:)%...' will produce a
                            # parent local name 'Atm'. Since the rank of the parent variable is not known
                            # at this point and since the local name in the host model metadata table could
                            # contain '(:)', '(:,:)', ... (up to the rank of the array), we search for the
                            # maximum number of dimensions allowed by the Fortran standard.
                            for local_name_define in [parent_local_name_define] + parent_local_names_define_indices:
                                parent_standard_name = None
                                parent_var = None
                                for i in range(FORTRAN_ARRAY_MAX_DIMS+1):
                                    if i==0:
                                        dims_string = ''
                                    else:
                                        # (:) for i==1, (:,:) for i==2, ...
                                        dims_string = '(' + ','.join([':' for j in range(i)]) + ')'
                                    if local_name_define+dims_string in standard_name_by_local_name_define.keys():
                                        parent_standard_name = standard_name_by_local_name_define[local_name_define+dims_string]
                                        parent_var = metadata_define[parent_standard_name][0]
                                        break
                                if not parent_var:
                                    raise Exception('Parent variable {parent} of {child} with standard name '.format(
                                                               parent=local_name_define, child=var_local_name_define)+\
                                                    '{standard_name} not defined in host model metadata'.format(
                                                                               standard_name=var_standard_name))

                                # Reset local name for entire array to a notation without (:), (:,:), etc.;
                                # this is needed for the var.print_def_intent() routine to work correctly
                                parent_var.local_name = local_name_define

                                # Add variable to dictionary of parent variables, if not already there.
                                # Set or update intent, depending on whether the variable is an index
                                # in var_local_name_define or the actual parent of that variable.
                                if not parent_standard_name in self.parents[ccpp_stage].keys():
                                    self.parents[ccpp_stage][parent_standard_name] = copy.deepcopy(parent_var)
                                    # Copy the intent of the actual variable being processed
                                    if local_name_define == parent_local_name_define:
                                        self.parents[ccpp_stage][parent_standard_name].intent = var.intent
                                    # It's an index for the actual variable being processed --> intent(in)
                                    else:
                                        self.parents[ccpp_stage][parent_standard_name].intent = 'in'
                                elif self.parents[ccpp_stage][parent_standard_name].intent == 'in':
                                    # Adjust the intent if the actual variable is not intent(in)
                                    if local_name_define == parent_local_name_define and not var.intent == 'in':
                                        self.parents[ccpp_stage][parent_standard_name].intent = 'inout'
                                    # It's an index for the actual variable being processed, intent is ok
                                    #else:
                                    #   # nothing to do
                                elif self.parents[ccpp_stage][parent_standard_name].intent == 'out':
                                    # Adjust the intent if the actual variable is not intent(out)
                                    if local_name_define == parent_local_name_define and not var.intent == 'out':
                                        self.parents[ccpp_stage][parent_standard_name].intent = 'inout'
                                    # Adjust the intent, because the variable is also used as index variable
                                    else:
                                        self.parents[ccpp_stage][parent_standard_name].intent = 'inout'

                                # Record the parent information for this variable (with standard name var_standard_name)
                                if local_name_define == parent_local_name_define:
                                    local_vars[var_standard_name] = {
                                        'name' : metadata_define[var_standard_name][0].local_name,
                                        'kind' : metadata_define[var_standard_name][0].kind,
                                        'parent_standard_name' : parent_standard_name
                                        }

                            # Reset parent to actual parent of the variable with standard name var_standard_name
                            if local_vars[var_standard_name]['parent_standard_name']:
                                parent_standard_name = local_vars[var_standard_name]['parent_standard_name']
                                parent_var = metadata_define[parent_standard_name][0]

                        elif local_vars[var_standard_name]['parent_standard_name']:
                            parent_standard_name = local_vars[var_standard_name]['parent_standard_name']
                            parent_var = metadata_define[parent_standard_name][0]
                            # Update intent information if necessary
                            if self.parents[ccpp_stage][parent_standard_name].intent == 'in' and not var.intent == 'in':
                                self.parents[ccpp_stage][parent_standard_name].intent = 'inout'
                            elif self.parents[ccpp_stage][parent_standard_name].intent == 'out' and not var.intent == 'out':
                                self.parents[ccpp_stage][parent_standard_name].intent = 'inout'

                        # The remainder of this loop deals with adding variables to the argument list
                        # for this subroutine, not required for the additional dimensions and variables
                        if not var_standard_name in arguments[scheme_name][subroutine_name]:
                            continue

                        # To assist debugging efforts, check if arrays have the correct size (ignore scalars for now)
                        assign_test = ''
                        if debug:
                            if ccpp_stage in ['init', 'timestep_init', 'timestep_finalize', 'finalize'] and \
                                    CCPP_INTERNAL_VARIABLES[CCPP_BLOCK_NUMBER] in local_vars[var_standard_name]['name'] and \
                                    '{}:{}'.format(CCPP_CONSTANT_ONE,CCPP_HORIZONTAL_DIMENSION) in var.dimensions:
                                # We don't need extra tests for blocked arrays, because the de-blocking logic below
                                # will catch any out of bound reads with the appropriate compiler flags. It naturally
                                # deals with non-uniform block sizes etc.
                                pass
                            elif var.rank:
                                array_size = []
                                for dim in var.dimensions:
                                    # This is not supported/implemented: tmpvar would have one dimension less
                                    # than the original array, and the metadata requesting the variable would
                                    # not pass the initial test that host model variables and scheme variables
                                    # have the same rank.
                                    if dim == CCPP_BLOCK_NUMBER:
                                        raise Exception("{} cannot be part of the dimensions of variable {}".format(
                                                                              CCPP_BLOCK_NUMBER, var_standard_name))
                                    else:
                                        # Handle dimensions like "A:B", "A:3", "-1:Z"
                                        if ':' in dim:
                                            dims = [ x.lower() for x in dim.split(':')]
                                            try:
                                                dim0 = int(dims[0])
                                                dim0 = dims[0]
                                            except ValueError:
                                                if not dims[0].lower() in metadata_define.keys():
                                                    raise Exception('Dimension {}, required by variable {}, not defined in host model metadata'.format(
                                                                                                                   dims[0].lower(), var_standard_name))
                                                dim0 = metadata_define[dims[0].lower()][0].local_name
                                            try:
                                                dim1 = int(dims[1])
                                                dim1 = dims[1]
                                            except ValueError:
                                                if not dims[1].lower() in metadata_define.keys():
                                                    raise Exception('Dimension {}, required by variable {}, not defined in host model metadata'.format(
                                                                                                                   dims[1].lower(), var_standard_name))
                                                dim1 = metadata_define[dims[1].lower()][0].local_name
                                        # Single dimensions
                                        else:
                                            dim0 = 1
                                            try:
                                                dim1 = int(dim)
                                                dim1 = dim
                                            except ValueError:
                                                if not dim.lower() in metadata_define.keys():
                                                    raise Exception('Dimension {}, required by variable {}, not defined in host model metadata'.format(
                                                                                                                       dim.lower(), var_standard_name))
                                                dim1 = metadata_define[dim.lower()][0].local_name
                                    array_size.append('({last}-{first}+1)'.format(last=dim1, first=dim0))
                                var_size_expected  = '({})'.format('*'.join(array_size))
                                assign_test = '''        ! Check if variable {var_name} is associated/allocated and has the correct size
        if (size({var_name})/={var_size_expected}) then
          write({ccpp_errmsg}, '(2(a,i8))') 'Detected size mismatch for variable {var_name} in group {group_name} before {subroutine_name}, expected ', &
                                           {var_size_expected}, ' but got ', size({var_name})
          ierr = 1
          return
        end if
'''.format(var_name=local_vars[var_standard_name]['name'], var_size_expected=var_size_expected,
           ccpp_errmsg=CCPP_INTERNAL_VARIABLES[CCPP_ERROR_MSG_VARIABLE], group_name = self.name,
           subroutine_name=subroutine_name)
                        # end if debug

                        # kind_string is used for automated unit conversions, i.e. foo_kind_phys
                        kind_string = '_' + local_vars[var_standard_name]['kind'] if local_vars[var_standard_name]['kind'] else ''

                        # Convert blocked data in init and finalize steps - only required for variables with block number and horizontal_dimension
                        if ccpp_stage in ['init', 'timestep_init', 'timestep_finalize', 'finalize'] and \
                                CCPP_INTERNAL_VARIABLES[CCPP_BLOCK_NUMBER] in local_vars[var_standard_name]['name'] and \
                                '{}:{}'.format(CCPP_CONSTANT_ONE,CCPP_HORIZONTAL_DIMENSION) in var.dimensions:
                            # Reuse existing temporary variable, if possible
                            if local_vars[var_standard_name]['name'] in tmpvars.keys():
                                # If the variable already has a local variable (tmpvar), reuse it
                                tmpvar = tmpvars[local_vars[var_standard_name]['name']]
                                actions_in  = tmpvar.actions['in']
                                actions_out = tmpvar.actions['out']
                            else:
                                # Add a local variable (tmpvar) for this variable
                                tmpvar_cnt += 1
                                tmpvar = copy.deepcopy(var)
                                tmpvar.local_name = '{0}_local'.format(var.local_name)
                                #
                                # Create string for allocating the temporary array by converting the dimensions
                                # (in standard_name format) to local names as known to the host model
                                alloc_dimensions = []
                                for dim in tmpvar.dimensions:
                                    # This is not supported/implemented: tmpvar would have one dimension less
                                    # than the original array, and the metadata requesting the variable would
                                    # not pass the initial test that host model variables and scheme variables
                                    # have the same rank.
                                    if dim == CCPP_BLOCK_NUMBER:
                                        raise Exception("{} cannot be part of the dimensions of variable {}".format(
                                                                              CCPP_BLOCK_NUMBER, var_standard_name))
                                    else:
                                        # Handle dimensions like "A:B", "A:3", "-1:Z"
                                        if ':' in dim:
                                            dims = [ x.lower() for x in dim.split(':')]
                                            try:
                                                dim0 = int(dims[0])
                                            except ValueError:
                                                dim0 = metadata_define[dims[0]][0].local_name
                                            try:
                                                dim1 = int(dims[1])
                                            except ValueError:
                                                dim1 = metadata_define[dims[1]][0].local_name
                                        # Single dimensions
                                        else:
                                            dim0 = 1
                                            try:
                                                dim1 = int(dim)
                                            except ValueError:
                                                dim1 = metadata_define[dim][0].local_name
                                        alloc_dimensions.append('{}:{}'.format(dim0,dim1))

                                # Padding of additional dimensions - before and after the horizontal dimension
                                hdim_index = tmpvar.dimensions.index('{}:{}'.format(CCPP_CONSTANT_ONE,CCPP_HORIZONTAL_DIMENSION))
                                dimpad_before = '' + ':,'*(len(tmpvar.dimensions[:hdim_index]))
                                dimpad_after  = '' + ',:'*(len(tmpvar.dimensions[hdim_index+1:]))

                                # Add necessary local variables for looping over blocks
                                var_defs_manual.append('integer :: ib, nb')

                                # Define actions before. Always copy data in, independent of intent.
                                actions_in = '''        ! Allocate local variable to copy blocked data {var} into a contiguous array
        allocate({tmpvar}({dims}))
        ib = 1
        do nb=1,{block_count}
          {tmpvar}({dimpad_before}ib:ib+{block_size}-1{dimpad_after}) = {var}
          ib = ib+{block_size}
        end do
'''.format(tmpvar=tmpvar.local_name,
           block_count=metadata_define[CCPP_BLOCK_COUNT][0].local_name.replace(CCPP_INTERNAL_VARIABLES[CCPP_BLOCK_NUMBER],'nb'),
           block_size=metadata_define[CCPP_HORIZONTAL_LOOP_EXTENT][0].local_name.replace(CCPP_INTERNAL_VARIABLES[CCPP_BLOCK_NUMBER],'nb'),
           var=tmpvar.target.replace(CCPP_INTERNAL_VARIABLES[CCPP_BLOCK_NUMBER],'nb'),
           dims=','.join(alloc_dimensions),
           dimpad_before=dimpad_before,
           dimpad_after=dimpad_after,
           )
                                # Define actions after, depending on intent.
                                if var.intent in [ 'inout', 'out' ]:
                                    actions_out = '''        ib = 1
        do nb=1,{block_count}
          {var} = {tmpvar}({dimpad_before}ib:ib+{block_size}-1{dimpad_after})
          ib = ib+{block_size}
        end do
        deallocate({tmpvar})
'''.format(tmpvar=tmpvar.local_name,
           block_count=metadata_define[CCPP_BLOCK_COUNT][0].local_name.replace(CCPP_INTERNAL_VARIABLES[CCPP_BLOCK_NUMBER],'nb'),
           block_size=metadata_define[CCPP_HORIZONTAL_LOOP_EXTENT][0].local_name.replace(CCPP_INTERNAL_VARIABLES[CCPP_BLOCK_NUMBER],'nb'),
           var=tmpvar.target.replace(CCPP_INTERNAL_VARIABLES[CCPP_BLOCK_NUMBER],'nb'),
           dimpad_before=dimpad_before,
           dimpad_after=dimpad_after,
           )
                                else:
                                    actions_out = '''        deallocate({tmpvar})
'''.format(tmpvar=tmpvar.local_name)

                                # Set/update actions for this temporary variable
                                tmpvar.actions = {'in' : actions_in, 'out' : actions_out}
                                tmpvars[local_vars[var_standard_name]['name']] = tmpvar

                            # Add unit conversions, if necessary
                            if var.actions['in']:
                                # Add unit conversion before entering the subroutine, after allocating the temporary
                                # array holding the non-blocked data and copying the blocked data to it
                                actions_in = actions_in + \
                                                 '        {t} = {c}\n'.format(t=tmpvar.local_name,
                                                                            c=var.actions['in'].format(var=tmpvar.local_name,
                                                                                                    kind=kind_string))

                            if var.actions['out']:
                                # Add unit conversion after returning from the subroutine, before copying the non-blocked
                                # data back to the blocked data and deallocating the temporary array
                                actions_out  = '        {t} = {c}\n'.format(t=tmpvar.local_name,
                                                                            c=var.actions['out'].format(var=tmpvar.local_name,
                                                                                                        kind=kind_string)) + \
                                                 actions_out

                            # Add the conditionals for the "before" operations
                            actions_before += '''
      if ({conditional}) then
{actions_in}
      end if
'''.format(conditional=conditionals[var_standard_name],
           actions_in=actions_in.rstrip('\n'))
                            # Add the conditionals for the "after" operations
                            actions_after += '''
      if ({conditional}) then
{actions_out}
      end if
'''.format(conditional=conditionals[var_standard_name],
           actions_out=actions_out.rstrip('\n'))

                            # Add to argument list if required
                            if var_standard_name in arguments[scheme_name][subroutine_name]:
                                arg = '{local_name}={var_name},'.format(local_name=var.local_name, var_name=tmpvar.local_name)

                        # Variables stored in blocked data structures but without horizontal dimension not supported at this time (doesn't make sense anyway)
                        elif ccpp_stage in ['init', 'timestep_init', 'timestep_finalize', 'finalize'] and \
                                CCPP_INTERNAL_VARIABLES[CCPP_BLOCK_NUMBER] in local_vars[var_standard_name]['name']:
                            raise Exception("Variables stored in blocked data structures but without horizontal dimension not supported in phases ' + \
                                            'init, timestep_init, timestep_finalize, finalize at this time: {} in {}".format(var_standard_name, subroutine_name))

                        # Limitations for UFS: Variables stored in threaded data structures (i.e. only for one block at a time) in GFS_interstitial DDT
                        # are not supported at this time (doesn't make sense anyway)
                        elif ccpp_stage in ['init', 'timestep_init', 'timestep_finalize', 'finalize'] and \
                                CCPP_INTERNAL_VARIABLES[CCPP_THREAD_NUMBER] in local_vars[var_standard_name]['name']:
                            raise Exception("Variables stored in thread-specific data structures (GFS_interstitial DDT) are not supported in phases ' + \
                                            'init, timestep_init, timestep_finalize, finalize at this time: {} in {}".format(var_standard_name, subroutine_name))

                        # Unit conversions without converting blocked data structures
                        elif var.actions['in'] or var.actions['out']:
                            # If requested, check that arrays are allocated/associated and have the correct size
                            if debug:
                                actions_in = assign_test
                            else:
                                actions_in = ''
                            actions_out = ''
                            if local_vars[var_standard_name]['name'] in tmpvars.keys():
                                # If the variable already has a local variable (tmpvar), reuse it
                                tmpvar = tmpvars[local_vars[var_standard_name]['name']]
                            else:
                                # Add a local variable (tmpvar) for this variable
                                tmpvar_cnt += 1
                                tmpvar = copy.deepcopy(var)
                                tmpvar.local_name = 'tmpvar{0}'.format(tmpvar_cnt)
                                tmpvars[local_vars[var_standard_name]['name']] = tmpvar
                            if tmpvar.rank:
                                # Add allocate statement if the variable has a rank > 0
                                actions_in += '        allocate({t}, source={v})\n'.format(t=tmpvar.local_name,
                                                                                             v=tmpvar.target)
                            if var.actions['in']:
                                # Add unit conversion before entering the subroutine
                                actions_in += '        {t} = {c}\n'.format(t=tmpvar.local_name,
                                                                             c=var.actions['in'].format(var=tmpvar.target,
                                                                                                        kind=kind_string))
                            if var.actions['out']:
                                # Add unit conversion after returning from the subroutine
                                actions_out  += '        {v} = {c}\n'.format(v=tmpvar.target,
                                                                             c=var.actions['out'].format(var=tmpvar.local_name,
                                                                                                        kind=kind_string))
                            if tmpvar.rank:
                                # Add deallocate statement if the variable has a rank > 0
                                actions_out += '        deallocate({t})\n'.format(t=tmpvar.local_name)

                            # Add the conditionals for the "before" operations
                            actions_before += '''
      if ({conditional}) then
{actions_in}
      end if
'''.format(conditional=conditionals[var_standard_name],
           actions_in=actions_in.rstrip('\n'))
                            # Add the conditionals for the "after" operations
                            actions_after += '''
      if ({conditional}) then
{actions_out}
      end if
'''.format(conditional=conditionals[var_standard_name],
           actions_out=actions_out.rstrip('\n'))

                            # Add to argument list if required
                            if var_standard_name in arguments[scheme_name][subroutine_name]:
                                arg = '{local_name}={var_name},'.format(local_name=var.local_name, var_name=tmpvar.local_name)

                        # Ordinary variables, no blocked data or unit conversions
                        elif var_standard_name in arguments[scheme_name][subroutine_name]:
                            if debug and assign_test:
                                actions_in = assign_test
                                # Add the conditionals for the "before" operations
                                actions_before += '''
      if ({conditional}) then
{actions_in}
      end if
'''.format(conditional=conditionals[var_standard_name],
           actions_in=actions_in.rstrip('\n'))

                            # Add to argument list
                            arg = '{local_name}={var_name},'.format(local_name=var.local_name, 
                                                                    var_name=local_vars[var_standard_name]['name'])
                        else:
                            arg = ''
                        args += arg
                        length += len(arg)
                        # Split args so that lines don't get too long
                        if length > 70 and not var_standard_name == arguments[scheme_name][subroutine_name][-1]:
                            args += ' &\n                  '
                            length = 0
                    args = args.rstrip(',')
                    subroutine_call = '''
{actions_before}

      call {subroutine_name}({args})

{actions_after}
'''.format(subroutine_name=subroutine_name, args=args, actions_before=actions_before.rstrip('\n'), actions_after=actions_after.rstrip('\n'))
                    error_check = '''if ({target_name_flag}/=0) then
        {target_name_msg} = "An error occured in {subroutine_name}: " // trim({target_name_msg})
        ierr={target_name_flag}
        return
      end if
'''.format(target_name_flag=ccpp_error_flag_target_name, target_name_msg=ccpp_error_msg_target_name, subroutine_name=subroutine_name)
                    subcycle_body += '''
      {subroutine_call}
      {error_check}
    '''.format(subroutine_call=subroutine_call, error_check=error_check)

                    module_use += '   use {m}, only: {s}\n'.format(m=module_name, s=subroutine_name)

                # If this subcycle calls any schemes, i.e. has any variables registered
                # that need to be passed to the group for this stage, then handle the
                # subcycle loops by prepending/appending the necessary code to subcycle_body
                subcycle_body_prefix = '''
      ! Start of next subcycle
'''
                subcycle_body_suffix = ''
                if self.parents[ccpp_stage]:
                    # Set subcycle loop extent if requested by any scheme
                    if ccpp_loop_extent_target_name and ccpp_stage == 'run':
                        subcycle_body_prefix += '''
      ! Set loop extent variable for the following subcycle
      {loop_extent_var_name} = {loop_cnt_max}
'''.format(loop_extent_var_name=ccpp_loop_extent_target_name,
                                  loop_cnt_max=subcycle.loop)
                    elif ccpp_loop_extent_target_name:
                        subcycle_body_prefix += '''
      ! Set loop extent variable for the following subcycle
      {loop_extent_var_name} = 1
'''.format(loop_extent_var_name=ccpp_loop_extent_target_name)
                    # Create subcycle (Fortran do loop) if needed
                    if subcycle.loop > 1 and ccpp_stage == 'run':
                        subcycle_body_prefix += '''
      associate(cnt => {loop_var_name})
      do cnt=1,{loop_cnt_max}\n\n'''.format(loop_var_name=ccpp_loop_counter_target_name,
                                                        loop_cnt_max=subcycle.loop)
                        subcycle_body_suffix += '''
      end do
      end associate
'''
                    elif ccpp_loop_counter_target_name:
                        subcycle_body_prefix += '''
      {loop_var_name} = 1\n'''.format(loop_var_name=ccpp_loop_counter_target_name)

                # Add this subcycle's Fortran body to the group body
                if subcycle_body:
                    body += subcycle_body_prefix + subcycle_body + subcycle_body_suffix

            # Get list of arguments, module use statement and variable definitions for this subroutine (=stage for the group)
            (self.arguments[ccpp_stage], sub_module_use, sub_var_defs) = create_arguments_module_use_var_defs(
                                                           self.parents[ccpp_stage], metadata_define, tmpvars.values())
            sub_argument_list = create_argument_list_wrapped(self.arguments[ccpp_stage])

            # Remove duplicates from additional manual variable definitions
            var_defs_manual = list(set(var_defs_manual))

            # Write cap - shorten certain ccpp_stages to stay under the 63 character limit for Fortran function names
            subroutine = self._suite + '_' + self._name + '_' + CCPP_STAGES[ccpp_stage] + '_cap'
            self._subroutines.append(subroutine)
            # Test and set blocks for initialization status - check that at least
            # the mandatory CCPP error handling arguments are present (i.e. there is
            # at least one subroutine that gets called from this group), or skip.
            if self.arguments[ccpp_stage]:
                initialized_test_block = Group.initialized_test_blocks[ccpp_stage].format(
                                            target_name_flag=ccpp_error_flag_target_name,
                                            target_name_msg=ccpp_error_msg_target_name,
                                            name=self._name)
            else:
                initialized_test_block = ''
            initialized_set_block = Group.initialized_set_blocks[ccpp_stage].format(
                                        target_name_flag=ccpp_error_flag_target_name,
                                        target_name_msg=ccpp_error_msg_target_name,
                                        name=self._name)
            # Create subroutine
            local_subs += Group.sub.format(subroutine=subroutine,
                                           argument_list=sub_argument_list,
                                           module_use='\n      '.join(sub_module_use),
                                           initialized_test_block=initialized_test_block,
                                           initialized_set_block=initialized_set_block,
                                           var_defs='\n      '.join(sub_var_defs + var_defs_manual),
                                           body=body)

        # Write output to stdout or file
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            # If the file exists, write to temporary file first and compare them:
            # - if identical, delete the temporary file and keep the existing one
            #   and set the group cap update flag to false
            # - if different, replace existing file with temporary file and set
            #   the group cap update flag to true (default value)
            # If the file does not exist, write the cap an set the flag to true
            if os.path.isfile(self.filename):
                write_to_test_file = True
                test_filename = self.filename + '.test'
                f = open(test_filename, 'w')
            else:
                write_to_test_file = False
                f = open(self.filename, 'w')
        else:
            f = sys.stdout
        f.write(Group.header.format(group=self._name,
                                    module=self._module,
                                    module_use=module_use,
                                    subroutines=', &\n             '.join(self._subroutines)))
        f.write(local_subs)
        f.write(Group.footer.format(module=self._module))
        if (f is not sys.stdout):
            f.close()
            # See comment above on updating the group cap or not
            if write_to_test_file:
                if filecmp.cmp(self.filename, test_filename):
                    # Files are equal, delete the test cap
                    # and set update flag to False
                    os.remove(test_filename)
                    self.update_cap = False
                else:
                    # Files are different, replace existing cap
                    # with test cap and set flag to True
                    # Python 3 only: os.replace(test_filename, self.filename)
                    os.remove(self.filename)
                    os.rename(test_filename, self.filename)
                    self.update_cap = True
            else:
                self.update_cap = True
        return

    @property
    def name(self):
        '''Get the name of the group.'''
        return self._name

    @name.setter
    def name(self, value):
        self._name = value

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

    @property
    def update_cap(self):
        '''Get the update_cap flag.'''
        return self._update_cap

    @update_cap.setter
    def update_cap(self, value):
        self._update_cap = value

    @property
    def init(self):
        '''Get the init flag.'''
        return self._init

    @init.setter
    def init(self, value):
        if not type(value) == types.BooleanType:
            raise Exception("Invalid type {0} of argument value, boolean expected".format(type(value)))
        self._init = value

    @property
    def finalize(self):
        '''Get the finalize flag.'''
        return self._finalize

    @finalize.setter
    def finalize(self, value):
        if not type(value) == types.BooleanType:
            raise Exception("Invalid type {0} of argument value, boolean expected".format(type(value)))
        self._finalize = value

    @property
    def suite(self):
        '''Get the suite name.'''
        return self._suite

    @property
    def module(self):
        '''Get the module name.'''
        return self._module

    @property
    def subcycles(self):
        '''Get the subcycles.'''
        return self._subcycles

    @property
    def subroutines(self):
        '''Get the subroutine names.'''
        return self._subroutines

    def print_debug(self):
        '''Basic debugging output about the group.'''
        print(self._name)
        for subcycle in self._subcycles:
            subcycle.print_debug()

    @property
    def pset(self):
        '''Get the unique physics set of this group.'''
        return self._pset

    @pset.setter
    def pset(self, value):
        self._pset = value

    @property
    def parents(self):
        '''Get the parent variables for the group.'''
        return self._parents

    @parents.setter
    def parents(self, value):
        self._parents = value

    @property
    def arguments(self):
        '''Get the argument list of the group.'''
        return self._arguments

    @arguments.setter
    def arguments(self, value):
        self._arguments = value


class Subcycle(object):

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        self._schemes = None
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    @property
    def loop(self):
        '''Get the list of loop.'''
        return self._loop

    @loop.setter
    def loop(self, value):
        if not type(value) is int:
            raise Exception("Invalid type {0} of argument value, integer expected".format(type(value)))
        self._loop = value

    @property
    def schemes(self):
        '''Get the list of schemes.'''
        return self._schemes

    @schemes.setter
    def schemes(self, value):
        if not type(value) is list:
            raise Exception("Invalid type {0} of argument value, list expected".format(type(value)))
        self._schemes = value

    def print_debug(self):
        '''Basic debugging output about the subcycle.'''
        print(self._loop)
        for scheme in self._schemes:
            print(scheme)


###############################################################################
if __name__ == "__main__":
    main()
