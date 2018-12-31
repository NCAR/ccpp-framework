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
from parse_tools   import ParseContext, ParseSource
from parse_tools   import ParseInternalError, ParseSyntaxError, CCPPError
from parse_tools   import FORTRAN_ID
from parse_tools   import read_xml_file, validate_xml_file, find_schema_version
from fortran_tools import FortranWriter

phase_re  = re.compile(FORTRAN_ID+r"_((?i)(?:init(?:ial(?:ize)?)?)|(?:run)|(?:final(?:ize)?))$")
loop_re  = re.compile(FORTRAN_ID+r"_((?i)(?:extent)|(?:begin)|(?:end))$")
dimension_re = re.compile(FORTRAN_ID+r"_((?i)dimension)$")

array_ref_re = re.compile(r"([^(]*)[(]([^)]*)[)]")

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

    def find_host_model_var(self, hdim, host_model):
        hsdims = list()
        for hsdim in hdim.split(':'):
            hsdim_var = host_model.find_variable(hsdim, loop_subst=True)
            if hsdim_var is None:
                raise CCPPError("No matching host variable for {} dimension, {}".format(self._subroutine_name, hsdim))
            elif isinstance(hsdim_var, tuple):
                hsdims.append(hsdim_var[0].get_prop_value('local_name'))
                hsdims.append(hsdim_var[1].get_prop_value('local_name'))
            else:
                hsdims.append(hsdim_var.get_prop_value('local_name'))
            # End if
        # End for
        loop_var = loop_re.match(hdim)
        if (dimension_re.match(hdim) is not None) and (len(hsdims) == 1):
            # We need to specify the whole range
            hsdims = ['1'] + hsdims
        elif (loop_var is not None) and (len(hsdims) == 1):
            # We may to specify the whole range
            if loop_var.group(2).lower() == 'extent':
                hsdims = ['1'] + hsdims
            # End if
        # End if
        return ':'.join(hsdims)

    def host_arg_str(self, hvar, host_model, header, ddt):
        '''Create the proper statement of a piece of a host-model variable.
        If ddt is True, we can only have a single element selected
        '''
        hstr = hvar.get_prop_value('local_name')
        hdimval = hvar.get_prop_value('dimensions')
        # Turn the dimensions string into a proper list and take the correct one
        hdims = Var.get_prop('dimensions').valid_value(hdimval)
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
            if ddt and (':' in hdim):
                raise CCPPError("Invalid DDT dimension spec {}({})".format(hstr, hdimval))
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
        # Find our header
        test_name1 = self.name + '_' + phase
        if phase == 'init':
            test_name2 = self.name + '_' + 'initialize'
        elif phase == 'finalize':
            test_name2 = self.name + '_' + 'final'
        else:
            test_name2 = None
        # End if
        my_header = None
        for module in scheme_headers:
            for header in module:
                if header.title == test_name1:
                    my_header = header
                    self._subroutine_name = test_name1
                    break
                elif (test_name2 is not None) and (header.title == test_name2):
                    my_header = header
                    self._subroutine_name = test_name2
                    break
                # End if
            # End for
            if my_header is not None:
                break
            # End if
        # End for
        if my_header is None:
            estr = 'No header found for subroutine, {}'
            raise ParseInternalError(estr.format(test_name1),
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
                if (intent == 'in') or (intent == 'inout'):
                    hvar = parent.find_variable(stdname, loop_subst=True)
                    if hvar is None:
                        # No host variable, see if we have a suite variable
                        if stdname in suite_vars.keys():
                            hvar = suite_vars[stdname]
                        # End if
                    # End if
                    if hvar is None:
                        raise CCPPError("No matching host or suite variable for {} input, {}".format(self._subroutine_name, stdname))
                    elif isinstance(hvar, list):
                        args = list()
                        alen = len(hvar)
                        index = 0
                        for var in hvar:
                            ddt = index < (alen - 1)
                            argstr = self.host_arg_str(var, host_model, my_header, ddt)
                            index = index + 1
                            args.append(argstr)
                        # End for
                        host_arglist.append('%'.join(args))
                    elif isinstance(hvar, tuple):
                        for var in hvar:
                            argstr = self.host_arg_str(var, host_model, my_header, False)
                            host_arglist.append(argstr)
                        # End for
                    else:
                        argstr = self.host_arg_str(hvar, host_model, my_header, False)
                        host_arglist.append(argstr)
                    # End if
                else:
                    suite_vars.add_variable(svar, exists_ok=True)
                # End if
            # End for
            self._arglist = host_arglist
        # End if
        return scheme_mods, list() # No loop variables for scheme

    def write(self, outfile, phase, indent):
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

    def write(self, outfile, phase, indent):
        outfile.write('do {} = 1, {}'.format(self.name, self.loop), indent)
        # Note that 'scheme' may be a sybcycle or other construct
        for scheme in self._schemes:
            scheme.write(outfile, phase, indent+1)
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

class Group(VarDictionary):
    """Class to represent a grouping of schemes in a suite
    A Group object is implemented as a subroutine callable by the API.
    The main arguments to a group are the host model variables.
    Additional output arguments are generated from schemes with intent(out)
    arguments.
    Additional input or inout arguments are generated for inputs needed by
    schemes which are produced (intent(out)) by other groups.
    """

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

    def __init__(self, group_xml, parent, context):
        self._name = group_xml.get('name')
        self._parts = list()
        super(Group, self).__init__(self.name, parent_dict=parent)
        for item in group_xml:
            if item.tag == 'subcycle':
                self.add_item(Subcycle(item, self, context))
            else:
                self.add_item(Scheme(item, context))
            # End if
        # End for
        self._loop_var_defs = set()
        self._local_schemes = set()

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

    def analyze(self, phase, suite_vars, scheme_headers, logger):
        # We need a copy of the host model variables for dummy args
        self._host_vars = suite_vars.variable_list(recursive=True)
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

    def write(self, outfile, host_arglist, indent):
        self._subroutines = list()
        local_subs = ''
        # First, write out the subroutine header
        subname = self.name
        outfile.write(Group.subhead.format(subname=subname, args=host_arglist), indent)
        # Write out the scheme use statements
        for scheme in self._local_schemes:
            outfile.write(scheme, indent+1)
        # End for
        outfile.write('', 0)
        # Write out dummy arguments
        outfile.write('! Dummy arguments', indent+1)
        for var in self._host_vars:
            var.write_def(outfile, indent+1)
        # End for
        outfile.write('', 0)
        outfile.write('! Local Variables', indent+1)
        # Write out local variables
        for var in self._loop_var_defs:
            outfile.write(var, indent)
        # End for
        outfile.write('', 0)
        # Write the scheme and subcycle calls
        for item in self._parts:
            item.write(outfile, 'run', indent+1)
        # End for
        outfile.write(Group.subend.format(subname=subname), indent)
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

class Suite(VarDictionary):

    _header_='''
!>
!! @brief Auto-generated cap module for the CCPP suite
!!
!
module {module}
'''

    _preamble_ = '''
   {module_use}

   implicit none

   private
   {subroutines}
'''

    _footer_ = '''
end module {module}
'''

    _initial_group_ = '<group name="{}_suite_initialize"></group>'

    _final_group_ = '<group name="{}_suite_finalize"></group>'

    _scheme_template = '<scheme>{}</scheme>'

    def __init__(self, filename, api, logger):
        self._logger = logger
        self._name = None
        self._sdf_name = filename
        self._groups = list()
        self._init_group = None
        self._final_group = None
        self._context = None
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
        self._logger.info("Reading suite definition file for {}".format(self._name))
        gxml = ET.fromstring(Suite._initial_group_.format(self.name))
        self._init_group = Group(gxml, self, self._context)
        self._groups.append(self._init_group)
        gxml = ET.fromstring(Suite._final_group_.format(self.name))
        self._final_group = Group(gxml, self, self._context)
        # Build hierarchical structure as in SDF
        for suite_item in suite_xml:
            item_type = suite_item.tag.lower()
            # Suite item is a group or a suite-wide init or final method
            if item_type in ['init', 'initialize']:
                # Parse a suite-wide initialization scheme
                self._init_group.add_item(Scheme(suite_item, self._context))
            elif item_type in ['final', 'finalize']:
                # Parse a suite-wide finalization scheme
                self._final_group.add_item(Scheme(suite_item, self._context))
            else:
                # Parse a group
                self._groups.append(Group(suite_item, self, self._context))
            # End if
        # End for
        self._groups.append(self._final_group)
        return success

    def print_debug(self):
        # DH * TODO: create pretty output and return as string to calling function
        print("STRUCTURED:")
        print("{}".format(self._groups))
        for group in self._groups:
            group.print_debug()

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
        '''Collect all information needed to write a suite file
        >>> phase_re.match('foo_init').group(2)
        'init'
        >>> phase_re.match('FOO_INIT').group(2)
        'INIT'
        >>> phase_re.match('foo_initial').group(2)
        'initial'
        >>> phase_re.match('foo_initialize').group(2)
        'initialize'
        >>> phase_re.match('foo_initialize').group(2)[0:4]
        'init'
        >>> phase_re.match('foo_initize') is None
        True
        >>> phase_re.match('foo_run').group(2)
        'run'
        >>> phase_re.match('foo_finalize').group(2)
        'finalize'
        >>> phase_re.match('foo_finalize').group(2)[0:5]
        'final'
        >>> phase_re.match('foo_final').group(2)
        'final'
        >>> phase_re.match('foo_finalize_bar') is None
        True
        '''
        # Collect all the available schemes
        for header_list in scheme_headers:
            for header in header_list:
                match = phase_re.match(header.title)
                if match is not None:
                    pmatch = match.group(2).lower()
                else:
                    raise CCPPError('Unknown scheme metadata type, "{}"'.format(header.title))
                # End if
                if pmatch[0:4] == 'init':
                    sstr = Suite._scheme_template.format(match.group(1))
                    sxml = ET.fromstring(sstr)
                    scheme = Scheme(sxml, self._context)
                    self._init_group.add_item(scheme)
                elif pmatch == 'run':
                    pass # We do not need this list for anything
                elif pmatch[0:5] == 'final':
                    sstr = Suite._scheme_template.format(match.group(1))
                    sxml = ET.fromstring(sstr)
                    scheme = Scheme(sxml, self._context)
                    self._final_group.add_item(scheme)
                # End if (else already raised error as above)
            # End for
        # End for
        # Grab the host model argument list
        self._host_arg_list = host_model.argument_list()
        # First pass, create init, run, and finalize sequences
        for item in self.groups:
            if item is self._init_group:
                phase = 'init'
            elif item is self._final_group:
                phase = 'finalize'
            else:
                phase = 'run'
            # End if
            logger.debug("Group {}, schemes = {}".format(item.name, [x.name for x in item.schemes()]))
            # Note that the group analyze can update this suite's vars
            item.analyze(phase, self, scheme_headers, logger)
        # End for

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
            outfile.write(Suite._header_.format(module=self._module), 0)
            outfile.write(Suite._preamble_.format(module_use='',
                                                  subroutines=gsub_list), 1)
            outfile.write('contains', 0)
            for group in self._groups:
                group.write(outfile, self._host_arg_list, 1)
            outfile.write(Suite._footer_.format(module=self._module), 0)
            return output_file_name

###############################################################################

class API(VarDictionary):

    _header_ = '''
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

    _footer_ = '''
end module {module}
'''

    api_source = ParseSource("CCPP_API", "MODULE",
                             ParseContext(filename="ccpp_suite.F90"))

    required_vars = [Var({'local_name':'suite_name',
                          'standard_name':'suite_name',
                          'intent':'in', 'type':'character',
                          'kind':'len=*', 'units':'',
                          'dimensions':'()'}, api_source),
                     Var({'local_name':'suite_part',
                          'standard_name':'suite_part',
                          'intent':'in', 'type':'character',
                          'kind':'len=*', 'units':'',
                          'dimensions':'()'}, api_source)]

    def __init__(self, sdfs, host_model, scheme_headers, logger):
        self._module        = 'ccpp_physics_api'
        self._host          = host_model
        self._schemes       = scheme_headers
        self._suites        = list()
        super(API, self).__init__(self.module, variables=API.required_vars,
                                  parent_dict=host_model, logger=logger)
        self._host_arg_list = host_model.argument_list()
        # Turn the SDF files into Suites
        for sdf in sdfs:
            suite = Suite(sdf, self, logger)
            suite.analyze(host_model, scheme_headers, logger)
            self._suites.append(suite)
        # End for
    # End if

    @property
    def module(self):
        '''Get the module name of the API.'''
        return self._module

    @property
    def subroutines(self):
        '''Get the subroutines names of the API'''
        return self._subroutines

    def write(self, output_dir):
        """Write API for static build"""
        if len(self._suites) == 0:
            raise CCPPError("No suite specified for generating API")
        # End if
        filename = os.path.join(output_dir, self.module + '.F90')
        api_filenames = list()
        host_call_list = ', '.join(self.prop_list('local_name'))
        host_call_list = host_call_list + ", " + self._host_arg_list
        module_use = ''
        # Write out the suite files
        for suite in self._suites:
            out_file_name = suite.write(output_dir)
            api_filenames.append(out_file_name)
        # End for
        # Write out the API module
        with FortranWriter(filename, 'w') as api:
            api.write(COPYRIGHT, 0)
            api.write(API._header_.format(host_model=self._host.name,
                                          module=self.module,
                                          module_use=module_use), 0)
            api.write(API.subhead.format(host_call_list=host_call_list), 1)
            # Declare dummy arguments
            self.declare_variables(api, 2)
            for var in self._host.variable_list():
                var.write_def(api, 2)

            # Now, add in cases for all suite parts
            callstr = 'call ccpp_physics({})'
            api.write(API.subfoot, 1)
            api.write(API._footer_.format(module=self.module), 0)
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
            foo = Suite(kessler, VarDictionary('foo'), logger)
        else:
            raise CCPPError("Cannot find test file, '{}'".format(kessler))
    except CCPPError as sa:
        print("{}".format(sa))
# No else
