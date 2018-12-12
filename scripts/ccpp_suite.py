#!/usr/bin/env python
#

"""Classes and methods to create a Fortran suite-implementation file
to implement calls to a set of suites for a given host model."""

# Python library imports
from __future__ import print_function
import copy
import logging
import os.path
import sys
import re
import xml.etree.ElementTree as ET
# CCPP framework imports
from parse_tools import ParseContext, ParseSyntaxError, FORTRAN_ID
from parse_tools import read_xml_file, validate_xml_file, find_schema_version
from parse_tools import initLog, logger, setLogToNull
from common import CCPP_ERROR_FLAG_VARIABLE, CCPP_ERROR_MSG_VARIABLE, CCPP_LOOP_COUNTER

init_re   = re.compile(FORTRAN_ID+r"_init$")
run_re    = re.compile(FORTRAN_ID+r"_run$")
final_re  = re.compile(FORTRAN_ID+r"_finalize$")

###############################################################################
# Module (global) variables
###############################################################################

CCPP_STAGES = [ 'init', 'run', 'finalize' ]

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

class SuiteAbort(ValueError):
    "Class so main can log user errors without backtrace"
    def __init__(self, message):
        super(SuiteAbort, self).__init__(message)

###############################################################################

class Scheme(object):
    "A single scheme in a suite (e.g., init method)"

    def __init__(self, scheme_xml, context):
        self._name = scheme_xml.text
        self._version = scheme_xml.get('version', None)
        self._lib = scheme_xml.get('lib', None)

    @property
    def name(self):
        '''Return name of scheme'''
        return self._name

    def schemes(self):
        'Return self as a list for consistency with subcycle'
        return [self]

###############################################################################

class Subcycle(object):
    "Class to represent a subcycled group of schemes"

    def __init__(self, sub_xml, context):
        self._name = sub_xml.get('name', "subcycle")
        self._loop = sub_xml.get('loop', "1")
        self._schemes = list()
        for scheme in sub_xml:
            self._schemes.append(Scheme(scheme, context))

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

    def print_debug(self):
        # DH * TODO: create pretty output and return as string to calling function
        print("{}".format(self._loop))
        for scheme in self._schemes:
            print("{}".format(scheme))

###############################################################################

class Group(object):
    "Class to represent a grouping of schemes in a suite"

    header='''
!>
!! @brief Auto-generated cap module for the CCPP {group} group
!!
!
module {module}

   use, intrinsic :: iso_c_binding,                                   &
                     only: c_f_pointer, c_ptr, c_int32_t
   use            :: ccpp_types,                                      &
                     only: ccpp_t, CCPP_GENERIC_KIND
   use            :: ccpp_fields,                                     &
                     only: ccpp_field_get
#ifdef DEBUG
   use            :: ccpp_errors,                                     &
                     only: ccpp_error, ccpp_debug
#endif

   ! Other modules required, e.g. type definitions
   {module_use}

   implicit none

   private
   public :: {subroutines}

   logical, save :: initialized = .false.

contains
'''

    sub = '''
   function {subroutine}(cdata) result(ierr)

      integer                     :: ierr
      type(ccpp_t), intent(inout) :: cdata
      type(c_ptr)                 :: cptr
      integer, allocatable        :: cdims(:)
      integer                     :: ckind

{var_defs}

      ierr = 0

{initialized_test_block}

{var_gets}

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
        'run' : '''
      if (.not.initialized) then
         write({target_name_msg},'(*(a))') '{name}_run called before {name}_init'
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
        'run' : '',
        'finalize' : '''
      initialized = .false.
''',
    }

    def __init__(self, group_xml, context):
        self._name = group_xml.get('name')
        self._parts = list()
        for item in group_xml:
            if item.tag == 'subcycle':
                self._parts.append(Subcycle(item, context))
            else:
                self._parts.append(Scheme(item, context))
            # End if
        # End for

    def schemes(self):
        "Return a flattened list of schemes for this group"
        schemes = list()
        for item in self._parts:
            schemes.extend(item.schemes())
        # End for
        return schemes

    def write(self, metadata_request, metadata_define, arguments, ccpp_field_maps, module_use_cap):
        # First get target names of standard CCPP variables for subcycling and error handling
        ccpp_loop_counter_target_name = metadata_request[CCPP_LOOP_COUNTER][0].target
        ccpp_error_flag_target_name = metadata_request[CCPP_ERROR_FLAG_VARIABLE][0].target
        ccpp_error_msg_target_name = metadata_request[CCPP_ERROR_MSG_VARIABLE][0].target
        #
        module_use = module_use_cap
        self._module = 'ccpp_group_{name}_cap'.format(name=self._name)
        self._filename = '{module_name}.F90'.format(module_name=self._module)
        self._subroutines = []
        local_subs = ''
        #
        for ccpp_stage in CCPP_STAGES:
            if self._init and not ccpp_stage == 'init':
                continue
            elif self._finalize and not ccpp_stage == 'finalize':
                continue
            # For creating and mappig local variable names to standard names
            local_vars = {}
            local_var_cnt = 0
            local_var_cnt_max = 9999
            local_var_template = 'v{x:04d}'
            # Add the predefined variables for subcycling and error handling (special, no need to retrieve from cdata via ccpp_field_get)
            local_vars[CCPP_LOOP_COUNTER] = ccpp_loop_counter_target_name
            local_vars[CCPP_ERROR_FLAG_VARIABLE] = ccpp_error_flag_target_name
            local_vars[CCPP_ERROR_MSG_VARIABLE] = ccpp_error_msg_target_name
            #
            body = ''
            var_defs = ''
            var_gets = ''
            for subcycle in self._subcycles:
                if subcycle.loop > 1 and ccpp_stage == 'run':
                    body += '''
      associate(cnt => {loop_var_name})
      do cnt=1,{loop_cnt}\n\n'''.format(loop_var_name=ccpp_loop_counter_target_name,loop_cnt=subcycle.loop)
                for scheme_name in subcycle.schemes:
                    module_name = scheme_name
                    subroutine_name = scheme_name + '_' + ccpp_stage
                    container = encode_container(module_name, scheme_name, subroutine_name)
                    # Skip entirely empty routines
                    if not arguments[module_name][scheme_name][subroutine_name]:
                        continue
                    error_check = ''
                    args = ''
                    length = 0
                    for var_name in arguments[module_name][scheme_name][subroutine_name]:
                        for var in metadata_request[var_name]:
                            if container == var.container:
                                break
                        if not var_name in local_vars:
                            local_var_cnt += 1
                            if local_var_cnt > local_var_cnt_max:
                                raise Exception("local_var_cnt exceeding local_var_cnt_max, increase limit!")
                            tmpvar = copy.deepcopy(var)
                            tmpvar.local_name = local_var_template.format(x=local_var_cnt) #var_name.replace('-','_')
                            var_defs += '      ' + tmpvar.print_def() + '\n'
                            # Use the index lookup from ccpp_field_maps for the group's
                            # physics set and given variable name (standard name)
                            var_gets += tmpvar.print_get(index=ccpp_field_maps[self._pset][var_name]) + '\n'
                            # Add to list of local variables with the auto-generated local variable name
                            local_vars[var_name] = tmpvar.local_name
                            del tmpvar

                        # Add to argument list
                        arg = '{local_name}={var_name},'.format(local_name=var.local_name, var_name=local_vars[var_name])#var_name.replace('-','_'))
                        args += arg
                        length += len(arg)
                        # Split args so that lines don't exceed 260 characters (for PGI)
                        if length > 70 and not var_name == arguments[module_name][scheme_name][subroutine_name][-1]:
                            args += ' &\n                  '
                            length = 0

                    args = args.rstrip(',')
                    subroutine_call = 'call {subroutine_name}({args})'.format(subroutine_name=subroutine_name, args=args)
                    error_check = '''if ({target_name_flag}/=0) then
             write({target_name_msg},'(a)') "An error occured in {subroutine_name}"
             ierr={target_name_flag}
             return
          end if
'''.format(target_name_flag=ccpp_error_flag_target_name, target_name_msg=ccpp_error_msg_target_name, subroutine_name=subroutine_name)
                    body += '''
        {subroutine_call}
        {error_check}
    '''.format(subroutine_call=subroutine_call, error_check=error_check)

                    module_use += '   use {m}, only: {s}\n'.format(m=module_name, s=subroutine_name)

                if subcycle.loop > 1 and ccpp_stage == 'run':
                    body += '''
      end do
      end associate
'''

            subroutine = self._name + '_' + ccpp_stage + '_cap'
            self._subroutines.append(subroutine)
            # Test and set blocks for initialization status
            initialized_test_block = Group.initialized_test_blocks[ccpp_stage].format(
                                        target_name_flag=ccpp_error_flag_target_name,
                                        target_name_msg=ccpp_error_msg_target_name,
                                        name=self._name)
            initialized_set_block = Group.initialized_set_blocks[ccpp_stage].format(
                                        target_name_flag=ccpp_error_flag_target_name,
                                        target_name_msg=ccpp_error_msg_target_name,
                                        name=self._name)
            # Create subroutine
            local_subs += Group.sub.format(subroutine=subroutine,
                                           initialized_test_block=initialized_test_block,
                                           initialized_set_block=initialized_set_block,
                                           var_defs=var_defs,
                                           var_gets=var_gets,
                                           body=body)

        # Write output to stdout or file
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout
        f.write(Group.header.format(group=self._name,
                                    module=self._module,
                                    module_use=module_use,
                                    subroutines=','.join(self._subroutines)))
        f.write(local_subs)
        f.write(Group.footer.format(module=self._module))
        if (f is not sys.stdout):
            f.close()

        return

    @property
    def name(self):
        '''Get the name of the group.'''
        return self._name

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @property
    def init(self):
        '''Get the init flag.'''
        return self._init

    @property
    def finalize(self):
        '''Get the finalize flag.'''
        return self._finalize

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
        # DH * TODO: create pretty output and return as string to calling function
        print("{}".format(self._name))
        for subcycle in self._subcycles:
            subcycle.print_debug()

    @property
    def pset(self):
        '''Get the unique physics set of this group.'''
        return self._pset

    @pset.setter
    def pset(self, value):
        self._pset = value


###############################################################################

class Suite(object):

    header='''
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
   function {subroutine}(cdata) result(ierr)

      integer :: ierr
      type(ccpp_t), intent(inout) :: cdata

      ierr = 0

{body}

   end function {subroutine}
'''

    footer = '''
end module {module}
'''

    def __init__(self, filename):
        self._name = None
        self._sdf_name = filename
        self._groups = list()
        self._init_scheme = None
        self._final_scheme = None
        if not os.path.exists(self._sdf_name):
            raise SuiteAbort("Suite definition file {0} not found.".format(self._sdf_name))
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

    def parse(self):
        '''Parse the suite definition file.'''
        success = True

        tree, suite_xml = read_xml_file(self._sdf_name)
        # We do not have line number information for the XML file
        context = ParseContext(filename=self._sdf_name)
        # Validate the XML file
        version = find_schema_version(suite_xml) # Only one version so far
        res = validate_xml_file(self._sdf_name, 'suite', version)
        if not res:
            raise SuiteAbort("Invalid suite definition file, '{}'".format(self._sdf_name))
        # End if
        self._name = suite_xml.get('name')
        logger.info("Reading suite definition file for {}".format(self._name))

        # Flattened lists of all schemes and subroutines in SDF
        self._all_schemes_called = list()
        self._all_subroutines_called = list()

        # Build hierarchical structure as in SDF
        for suite_item in suite_xml:
            schemes = list()
            subcycles = list()
            item_type = suite_item.tag.lower()
            # Suite item is a group or a suite-wide init or final method
            if item_type in ['init', 'initialize']:
                # Parse a suite-wide initialization scheme
                self._init_scheme = Scheme(suite_item, context)
            elif item_type in ['final', 'finalize']:
                # Parse a suite-wide finalization scheme
                self._final_scheme = Scheme(suite_item, context)
            else:
                # Parse a group
                self._groups.append(Group(suite_item, context))
            # End if
        # End for
        return success

    def print_debug(self):
        # DH * TODO: create pretty output and return as string to calling function
        print("ALL SUBROUTINES:")
        print("{}".format(self._all_subroutines_called))
        print("STRUCTURED:")
        print("{}".format(self._groups))
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
    def groups(self):
        '''Get the list of groups in this suite.'''
        return self._groups

    def analyze(self, host_model, scheme_headers, verbosity=0):
        'Collect all information needed to write a suite file'
        # Collect all the available schemes
        init_schemes = list()
        run_schemes = list()
        final_schemes = list()
        for header_list in scheme_headers:
            for header in header_list:
                if init_re.match(header.title) is not None:
                    init_schemes.append(header)
                elif run_re.match(header.title) is not None:
                    run_schemes.append(header)
                elif final_re.match(header.title) is not None:
                    final_schemes.append(header)
                else:
                    raise SuiteAbort('Unknown scheme metadata type, "{}"'.format(header.title))
                # End if
            # End for
        # End for
        # First pass, create init, run, and finalize sequences
        init_seq = list()
        run_seq = list()
        final_seq = list()
        for item in self.groups:
            if (item.name == 'init') or (item.name == 'initialize'):
                # This is an initial scheme
                if not isinstance(item, Scheme):
                    raise SuiteAbort("Bad Suite initial method, {}".format(type(item)))
                else:
                    init_seq.append(item)
                # End if
            elif (item.name == 'final') or (item.name == 'finalize'):
                # This is an final scheme
                if not isinstance(item, Scheme):
                    raise SuiteAbort("Bad Suite final method, {}".format(type(item)))
                else:
                    final_seq.append(item)
                # End if
            else:
                # Find out what sort of schemes are available
                group_schemes = item.schemes()
                print("Group {}, schemes = {}".format(item.name, [x.name for x in group_schemes]))
#                run_seq.append(item)

    def write(self, metadata_request, metadata_define, arguments, ccpp_field_maps, module_use_cap):
        """Create caps for all groups in the suite and for the entire suite
        (calling the group caps one after another)"""
        # Set name of module and filename of cap
        self._module = 'ccpp_suite_cap'
        self._filename = '{module_name}.F90'.format(module_name=self._module)
        # Init
        self._subroutines = []
        module_use = module_use_cap
        # Write group caps and generate module use statements
        for group in self._groups:
            group.write(metadata_request, metadata_define, arguments, ccpp_field_maps, module_use_cap)
            module_use += '   use {m}, only: {s}\n'.format(m=group.module, s=','.join(group.subroutines))
        subs = ''
        for ccpp_stage in CCPP_STAGES:
            body = ''
            for group in self._groups:
                # Groups 'init'/'finalize' are only run in stages 'init'/'finalize'
                if (group.init and not ccpp_stage == 'init') or \
                    (group.finalize and not ccpp_stage == 'finalize'):
                    continue
                # Write to body that calls the groups for this stage
                body += '''
      ierr = {group_name}_{stage}_cap(cdata)
      if (ierr/=0) return
'''.format(group_name=group.name, stage=ccpp_stage)
            # Add name of subroutine in the suite cap to list of subroutine names
            subroutine = 'suite_{stage}_cap'.format(stage=ccpp_stage)
            self._subroutines.append(subroutine)
            # Add subroutine to output
            subs += Suite.sub.format(subroutine=subroutine, body=body)

        # Write cap to stdout or file
        if (self._filename is not sys.stdout):
            f = open(self._filename, 'w')
        else:
            f = sys.stdout
        f.write(Suite.header.format(module=self._module,
                                    module_use=module_use,
                                    subroutines=','.join(self._subroutines)))
        f.write(subs)
        f.write(Suite.footer.format(module=self._module))
        if (f is not sys.stdout):
            f.close()

        # Create list of all caps generated (for groups and suite)
        self._caps = [ self._filename ]
        for group in self._groups:
            self._caps.append(group.filename)


    def create_sdf_name_include_file(self, incfilename):
        # Create include file for framework that contains the name of the sdf used for static build
        f = open(incfilename, "w")
        f.write('character(len=*), parameter :: ccpp_suite_static_name = "{suite_name}"\n'.format(suite_name = self._name))
        f.close()

###############################################################################

class API(object):

    header='''
!>
!! @brief Auto-generated API for {host_model} calls to CCPP suites
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
   subroutine {subroutine}(cdata, group_name, ierr)

      type(ccpp_t),               intent(inout) :: cdata
      character(len=*), optional, intent(in)    :: group_name
      integer,                    intent(out)   :: ierr

      ierr = 0

      if (present(group_name)) then
{group_calls}
      else
{suite_call}
      end if

   end subroutine {subroutine}
'''

    footer = '''
end module {module}
'''

    def __init__(self, suites):
        self._module      = 'ccpp_physics_api'
        self._filename    = self._module + '.F90'
        self._suites      = suites

    @property
    def filename(self):
        '''Get the filename to write API to.'''
        return self._filename

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
        if not self._suite:
            raise Exception("No suite specified for generating API")
        suite = self._suite
        # Module use statements
        module_use = '   use {module}, only: {subroutines}\n'.format(module=suite.module,
                                                  subroutines=','.join(suite.subroutines))
        for group in suite.groups:
            module_use += '   use {module}, only: {subroutines}\n'.format(module=group.module,
                                                      subroutines=','.join(group.subroutines))

        # Create a subroutine for each stage
        self._subroutines=[]
        subs = ''
        for ccpp_stage in CCPP_STAGES:
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
                group_calls += '''
         {clause} (trim(group_name)=="{group_name}") then
            ierr = {group_name}_{stage}_cap(cdata)'''.format(clause=clause, group_name=group.name, stage=ccpp_stage)
            group_calls += '''
         else
            call ccpp_error("Group " // trim(group_name) // " not found")
            ierr = 1
        end if
'''.format(group_name=group.name)
            suite_call = '''
        ierr = suite_{stage}_cap(cdata)
'''.format(stage=ccpp_stage)
            subroutine = CCPP_STATIC_SUBROUTINE_NAME.format(stage=ccpp_stage)
            self._subroutines.append(subroutine)
            subs += API.sub.format(subroutine=subroutine,
                                   group_calls=group_calls,
                                   suite_call=suite_call)

        # Write output to stdout or file
        if (self._filename is not sys.stdout):
            f = open(self._filename, 'w')
        else:
            f = sys.stdout
        f.write(API.header.format(module=self._module,
                                  module_use=module_use,
                                  subroutines=','.join(self._subroutines)))
        f.write(subs)
        f.write(Suite.footer.format(module=self._module))
        if (f is not sys.stdout):
            f.close()
        return

###############################################################################
if __name__ == "__main__":
    logger = initLog('ccpp_suite')
    setLogToNull()
    try:
        # First, run doctest
        import doctest
        doctest.testmod()
        frame_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        cpf = os.path.dirname(frame_root)
        kessler = os.path.join(cpf, 'cam_driver', 'suites',
                               'suite_cam_kessler_test_simple1.xml')
        if os.path.exists(kessler):
            foo = Suite(kessler)
        else:
            raise SuiteAbort("Cannot find test file, '{}'".format(kessler))
    except SuiteAbort as sa:
        print("{}".format(sa))
# No else
