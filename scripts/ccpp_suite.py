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
from metavar       import Var, VarDictionary
from parse_tools   import ParseContext, ParseSource, ParseSyntaxError, CCPPError
from parse_tools   import FORTRAN_ID
from parse_tools   import read_xml_file, validate_xml_file, find_schema_version
from fortran_tools import FortranWriter

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

    def header_has_var(std_name, header):
        '''Find the requested variable in the header or return the alternatives
        This routine is only meant for loop dimension variables
        '''
        hvar = header.get_var(standard_name=std_name)
        if hvar is None:
            ##XXgoldyXX: Need to look for alternatives here
            raise CCPPError("Scheme {} does not have input, {}, required by host model".format(self.name, std_name))
        # End if (else we are okay)
        return hvar

    def host_arg_str(hvar_ind, host_model, ddt):
        '''Create the proper statement of a piece of a host-model variable.
        If ddt is True, we can only have a single element selected
        '''
        hstr = hvar.get_prop_value('local_name')
        hdimval = hvar.get_prop_value('dimensions')
        # Turn the dimensions string into a proper list and take the correct one
        hdims = Var.get_prop('dimensions').valid_value(hdimval)
        dimsep = ''
        if len(hdims) > 0:
            dimstr = '('
        else:
            dimstr = ''
        # End if
        for hdim in hdims:
            if ddt and (':' in hdim):
                raise CCPPError("Invalid DDT dimension spec {}({})".format(hstr, hdimval))
            else:
                hdim_vars = hdim.split(':')

                dimstr = dimstr + dimsep + hdimstr
                dimsep = ', '
            # End if
        # End for
        if len(hdims) > 0:
            dimstr = dimstr + ')'
        # End if

    def analyze(self, phase, host_model, scheme_headers, logger):
        # Find our header
        self._subroutine_name = self.name + '_' + phase
        my_header = None
        for module in scheme_headers:
            for header in module:
                if header.title == self._subroutine_name:
                    my_header = header
                    break
                # End if
            # End for
            if my_header is not None:
                break
            # End if
        # End for
        if my_header.module is None:
            raise ParseSyntaxError('No module found for subroutine',
                                   token=self._subroutine_name, context=self._context)
        # End if
        scheme_use = 'use {}, only: {}'.format(my_header.module,
                                               self._subroutine_name)
        if my_header is None:
            raise CCPPError('Could not find subroutine, {}'.format(subroutine_name))
        else:
            # We need to find the host model variable for each of our arguments
            my_arglist = my_header.prop_list('standard_name')
            host_arglist = list()
            for arg in my_arglist:
                hvar = host_model.find_variable(arg)
                if hvar is None:
                    raise CCPPError("No matching host variable for {} input, {}".format(self._subroutine_name, arg))
                elif isinstance(hvar, list):
                    args = list()
                    alen = len(args)
                    index = 0
                    for var in hvar:
                        new_arg = hvar.get_prop_name('local_name')
                        ddt = index < (alen - 1)
                        argstr = host_arg_str(new_arg, host_model, my_header, ddt)
                        index = index + 1
                        args.append(argstr)
                    # End for
                    host_arglist.append('%'.join(args))
                else:
                    new_arg = hvar.get_prop_name('local_name')
                    argstr = host_arg_str(new_arg, host_model, my_header, False)
                    host_arglist.append(argstr)
                # End if
            # End for
            self._arglist = host_arglist
        # End if
        return scheme_use

    def write(self, outfile, phase, level=2):
        outfile.write('call {}({})'.format(self._subroutine_name, self._arglist), level)

    def schemes(self):
        'Return self as a list for consistency with subcycle'
        return [self]

###############################################################################

class Subcycle(object):
    "Class to represent a subcycled group of schemes"

    def __init__(self, sub_xml, context):
        self._name = sub_xml.get('name', "subcycle")
        self._loop = sub_xml.get('loop', "1")
        self._context = context
        self._schemes = list()
        for scheme in sub_xml:
            self._schemes.append(Scheme(scheme, context))
        # End forn

    def analyze(self, phase, host_model, scheme_headers, logger):
        loopvar = '{}integer :: {}'.format(indent(2), self.name)
        scheme_mods = list()
        for scheme in self._schemes:
            scheme_mods.append(scheme.analyze(phase, host_model, scheme_headers, logger))
        # End for
        return loopvar, scheme_mods

    def write(self, outfile):
        outfile.write('do {} = 1, {}'.format(self.name, self.loop), 2)
        for scheme in self._schemes:
            scheme.write(outfile, level=3)
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

    def print_debug(self):
        # DH * TODO: create pretty output and return as string to calling function
        print("{}".format(self._loop))
        for scheme in self._schemes:
            print("{}".format(scheme))

###############################################################################

class Group(object):
    "Class to represent a grouping of schemes in a suite"

    subhead = '''
   subroutine {subname}({args})
'''

    subend = '''
   end subroutine {subname}
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

    def analyze(self, phase, host_model, scheme_headers, logger):
        self._local_vars = set()
        self._local_schemes = set()
        for item in self._parts:
            if isinstance(item, Subcycle):
                lvar, lschemes = item.analyze(phase, host_model, scheme_headers, logger)
                self._local_vars.add(lvar)
                for lscheme in lschemes:
                    self._local_schemes.add(lscheme)
                # End for
            elif isinstance(item, Scheme):
                self._local_schemes.add(item.analyze(phase, host_model, scheme_headers, logger))
            else:
                raise CCPPError('Illegal group type, {} in group {}'.format(type(item), self.name))
            # End if
        # End for

    def write(self, outfile, host_arglist):
        self._subroutines = list()
        local_subs = ''
        # First, write out the subroutine header
        subname = self.name + '_run'
        outfile.write(Group.subhead.format(subname=subname, args=host_arglist), 1)
        # Write out the scheme use statements
        for scheme in self._local_schemes:
            outfile.write(scheme, 1)
        # End for
        # Write out local variables
        for var in self._local_vars:
            outfile.write(var, 1)
        # End for
        # Write the scheme calls
        for item in self._parts:
            item.write(outfile)
        # End for
        outfile.write(Group.subend.format(subname=subname), 1)
            # # Test and set blocks for initialization status
            # initialized_test_block = Group.initialized_test_blocks[ccpp_stage].format(
            #                             target_name_flag=ccpp_error_flag_target_name,
            #                             target_name_msg=ccpp_error_msg_target_name,
            #                             name=self._name)
            # initialized_set_block = Group.initialized_set_blocks[ccpp_stage].format(
            #                             target_name_flag=ccpp_error_flag_target_name,
            #                             target_name_msg=ccpp_error_msg_target_name,
            #                             name=self._name)
            # Create subroutine

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
'''

    preamble = '''
   {module_use}

   implicit none

   private
   {subroutines}
'''

    footer = '''
end module {module}
'''

    def __init__(self, filename, logger):
        self._logger = logger
        self._name = None
        self._sdf_name = filename
        self._groups = list()
        self._init_scheme = None
        self._final_scheme = None
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

    def parse(self):
        '''Parse the suite definition file.'''
        success = True

        tree, suite_xml = read_xml_file(self._sdf_name, self._logger)
        # We do not have line number information for the XML file
        context = ParseContext(filename=self._sdf_name)
        # Validate the XML file
        version = find_schema_version(suite_xml, self._logger)
        res = validate_xml_file(self._sdf_name, 'suite', version, self._logger)
        if not res:
            raise CCPPError("Invalid suite definition file, '{}'".format(self._sdf_name))
        # End if
        self._name = suite_xml.get('name')
        self._logger.info("Reading suite definition file for {}".format(self._name))

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

    def analyze(self, host_model, scheme_headers, logger):
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
                    raise CCPPError('Unknown scheme metadata type, "{}"'.format(header.title))
                # End if
            # End for
        # End for
        # Grab the host model argument list
        self._host_arg_list = host_model.argument_list()
        # First pass, create init, run, and finalize sequences
        init_seq = list()
        run_seq = list()
        final_seq = list()

        for item in self.groups:
            if (item.name == 'init') or (item.name == 'initialize'):
                # This is an initial scheme
                if not isinstance(item, Scheme):
                    raise CCPPError("Bad Suite initial method, {}".format(type(item)))
                else:
                    init_seq.append(item)
                # End if
            elif (item.name == 'final') or (item.name == 'finalize'):
                # This is an final scheme
                if not isinstance(item, Scheme):
                    raise CCPPError("Bad Suite final method, {}".format(type(item)))
                else:
                    final_seq.append(item)
                # End if
            else:
                # Find out what sort of schemes are available
                group_schemes = item.schemes()
                print("Group {}, schemes = {}".format(item.name, [x.name for x in group_schemes]))
                item.analyze('run', host_model, scheme_headers, logger)

    def write(self, output_dir):
        """Create caps for all groups in the suite and for the entire suite
        (calling the group caps one after another)"""
        # Set name of module and filename of cap
        self._module = 'ccpp_{}_cap'.format(self.name)
        filename = '{module_name}.F90'.format(module_name=self._module)
        # Init
        self._subroutines = []
        module_use = None
        output_file_name = os.path.join(output_dir, filename)
        with FortranWriter(output_file_name, 'w') as outfile:
            # Write suite header
            gsub_list = ""
            for group in self._groups:
                gsub_list = gsub_list + ('   public :: {}_run\n'.format(group.name))
            # End for
            outfile.write(COPYRIGHT, 0)
            outfile.write(Suite.header.format(module=self._module), 0)
            outfile.write(Suite.preamble.format(module_use='',
                                                subroutines=gsub_list), 1)
            outfile.write('contains', 0)
            for group in self._groups:
                group.write(outfile, self._host_arg_list)
            outfile.write(Suite.footer.format(module=self._module), 0)
            return output_file_name

###############################################################################

class API(object):

    header = '''
!>
!! @brief Auto-generated API for {host_model} calls to CCPP suites
!!
!
module {module}

{module_use}

   implicit none

   private
   public :: ccpp_physics

contains
'''

    subhead = '''
   subroutine ccpp_physics({host_call_list})
'''
    subfoot = '''
   end subroutine ccpp_physics
'''

    footer = '''
end module {module}
'''

    api_source = ParseSource("CCPP_API", "MODULE",
                             ParseContext(filename="ccpp_suite.F90"))

    required_vars = VarDictionary([Var({'local_name':'suite_name',
                                        'standard_name':'suite_name',
                                        'intent':'in', 'type':'character',
                                        'kind':'len=*', 'units':'',
                                        'dimensions':'()'}, api_source),
                                   Var({'local_name':'suite_part',
                                        'standard_name':'suite_part',
                                        'intent':'in', 'type':'character',
                                        'kind':'len=*', 'units':'',
                                        'dimensions':'()'}, api_source)])

    def __init__(self, sdfs, host_model, scheme_headers, logger):
        self._module        = 'ccpp_physics_api'
        self._host          = host_model
        self._schemes       = scheme_headers
        self._suites        = list()
        self._host_arg_list = host_model.argument_list()
        # Turn the SDF files into Suites
        for sdf in sdfs:
            suite = Suite(sdf, logger)
# XXgoldyXX: v debug only
            suite.analyze(host_model, scheme_headers, logger)
            self._suites.append(suite)
# XXgoldyXX: ^ debug only
        # End for
    # End if

    @property
    def module(self):
        '''Get the module name of the API.'''
        return self._module

    @property
    def subroutines(self):
        '''Get the subroutines names of the API to.'''
        return self._subroutines

    def write(self, output_dir):
        """Write API for static build"""
        if len(self._suites) == 0:
            raise CCPPError("No suite specified for generating API")
        # End if
        filename = os.path.join(output_dir, self.module + '.F90')
        api_filenames = list()
        host_call_list = ', '.join(API.required_vars.prop_list('local_name'))
        host_call_list = host_call_list + self._host_arg_list
        module_use = ''
        # Write out the suite files
        for suite in self._suites:
            out_file_name = suite.write(output_dir)
            api_filenames.append(out_file_name)
        # End for
        # Write out the API module
        with FortranWriter(filename, 'w') as api:
            api.write(COPYRIGHT, 0)
            api.write(API.header.format(host_model=self._host.name,
                                        module=self.module,
                                        module_use=module_use), 0)
            api.write(API.subhead.format(host_call_list=host_call_list), 1)
            # Declare dummy arguments
            API.required_vars.declare_variables(api, 2)
#            for arg in self._host_arg_list:
#                pass

            # Now, add in cases for all suite parts
            callstr = 'call ccpp_physics({})'
            api.write(API.subfoot, 1)
            api.write(API.footer.format(module=self.module), 0)
        # End with
        api_filenames.append(filename)
        return api_filenames


#         suite = self._suite
#         # Module use statements
#         module_use = '   use {module}, only: {subroutines}\n'.format(module=suite.module,
#                                                   subroutines=','.join(suite.subroutines))
#         for group in suite.groups:
#             module_use += '   use {module}, only: {subroutines}\n'.format(module=group.module,
#                                                       subroutines=','.join(group.subroutines))

#         # Create a subroutine for each stage
#         self._subroutines=[]
#         subs = ''
#         for ccpp_stage in CCPP_STAGES:
#             # Calls to groups of schemes for this stage
#             group_calls = ''
#             for group in suite.groups:
#                 # The <init></init> and <finalize></finalize> groups require special treatment,
#                 # since they can only be run in the respective stage (init/finalize)
#                 if (group.init and not ccpp_stage == 'init') or \
#                     (group.finalize and not ccpp_stage == 'finalize'):
#                     continue
#                 if not group_calls:
#                     clause = 'if'
#                 else:
#                     clause = 'else if'
#                 group_calls += '''
#          {clause} (trim(group_name)=="{group_name}") then
#             ierr = {group_name}_{stage}_cap(cdata)'''.format(clause=clause, group_name=group.name, stage=ccpp_stage)
#             group_calls += '''
#          else
#             call ccpp_error("Group " // trim(group_name) // " not found")
#             ierr = 1
#         end if
# '''.format(group_name=group.name)
#             suite_call = '''
#         ierr = suite_{stage}_cap(cdata)
# '''.format(stage=ccpp_stage)
#             subroutine = CCPP_STATIC_SUBROUTINE_NAME.format(stage=ccpp_stage)
#             self._subroutines.append(subroutine)
#             subs += API.sub.format(subroutine=subroutine,
#                                    group_calls=group_calls,
#                                    suite_call=suite_call)

#         # Write output to stdout or file
#         if (self._filename is not sys.stdout):
#             f = open(self._filename, 'w')
#         else:
#             f = sys.stdout
#         f.write(API.header.format(module=self._module,
#                                   module_use=module_use,
#                                   subroutines=','.join(self._subroutines)))
#         f.write(subs)
#         f.write(Suite.footer.format(module=self._module))
#         if (f is not sys.stdout):
#             f.close()
#         return

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
            foo = Suite(kessler, logger)
        else:
            raise CCPPError("Cannot find test file, '{}'".format(kessler))
    except CCPPError as sa:
        print("{}".format(sa))
# No else
