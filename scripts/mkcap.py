#!/usr/bin/env python
#
# Script to generate a cap module and subroutines
# from a scheme xml file.
#

from __future__ import print_function
import copy
import os
import sys
import getopt
import xml.etree.ElementTree as ET

from common import CCPP_ERROR_FLAG_VARIABLE
from common import CCPP_INTERNAL_VARIABLES
from common import STANDARD_VARIABLE_TYPES, STANDARD_CHARACTER_TYPE
from common import isstring, string_to_python_identifier
from conversion_tools import unit_conversion

###############################################################################

class Var(object):

    def __init__(self, **kwargs):
        self._standard_name = None
        self._long_name     = None
        self._units         = None
        self._local_name    = None
        self._type          = None
        self._rank          = None
        self._container     = None
        self._kind          = None
        self._intent        = None
        self._optional      = None
        self._target        = None
        self._type_kind_var = False
        self._actions       = { 'in' : None, 'out' : None }
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)
        # Consistency test for special kind/type definition variables
        self.type_kind_test()

    @property
    def standard_name(self):
        '''Get the name of the variable.'''
        return self._standard_name

    @standard_name.setter
    def standard_name(self, value):
        self._standard_name = value

    @property
    def long_name(self):
        '''Get the name of the variable.'''
        return self._long_name

    @long_name.setter
    def long_name(self, value):
        self._long_name = value

    @property
    def units(self):
        '''Get the units of the variable.'''
        return self._units

    @units.setter
    def units(self, value):
        self._units = value

    @property
    def local_name(self):
        '''Get the local variable name of the variable.'''
        return self._local_name

    @local_name.setter
    def local_name(self, value):
        self._local_name = value

    @property
    def type(self):
        '''Get the type of the variable.'''
        return self._type

    @type.setter
    def type(self, value):
        self._type = value

    @property
    def rank(self):
        '''Get the rank of the variable.'''
        return self._rank

    @rank.setter
    def rank(self, value):
        if not isinstance(value, int):
            raise TypeError('Invalid type for variable property rank, must be integer')
        elif self.type == 'character' and value > 0:
            raise Exception('Arrays of Fortran strings not implemented in CCPP')
        if (value == 0):
            self._rank = ''
        else:
            self._rank = '('+ ','.join([':'] * value) +')'

    @property
    def kind(self):
        '''Get the kind of the variable.'''
        return self._kind

    @kind.setter
    def kind(self, value):
        self._kind = value

    @property
    def intent(self):
        '''Get the intent of the variable.'''
        return self._intent

    @intent.setter
    def intent(self, value):
        if not value in ['none', 'in', 'out', 'inout']:
            raise ValueError('Invalid value {0} for variable property intent'.format(value))
        self._intent = value

    @property
    def optional(self):
        '''Get the optional of the variable.'''
        return self._optional

    @optional.setter
    def optional(self, value):
        if not value in ['T', 'F']:
            raise ValueError('Invalid value {0} for variable property optional'.format(value))
        self._optional = value

    @property
    def target(self):
        '''Get the target of the variable.'''
        return self._target

    @target.setter
    def target(self, value):
        self._target = value

    @property
    def container(self):
        '''Get the container of the variable.'''
        return self._container

    @container.setter
    def container(self, value):
        self._container = value

    @property
    def type_kind_var(self):
        '''Get the type_kind_var flag of the variable.'''
        return self._type_kind_var

    @type_kind_var.setter
    def type_kind_var(self, value):
        if type(value)==bool:
            self._type_kind_var = value
        else:
            raise Exception('Invalid value for variable attribute type_kind_var.')

    @property
    def actions(self):
        '''Get the action strings for the variable.'''
        return self._actions

    @actions.setter
    def actions(self, values):
        if type(value)==dict:
            for key in values.keys():
                if key in ['in', 'out'] and isstring(values[key]):
                    self._actions[key] = values[key]
                else:
                    raise Exception('Invalid values for variable attribute actions.')
        else:
            raise Exception('Invalid values for variable attribute actions.')

    def type_kind_test(self):
        """Type and kind definitions are special variables.
        Type definitions can be identified by the type being the same
        as the local name and the standard name of the variable.
        Kind definitions are identified by the localname being the same
        as the standard name, and the type being integer."""
        if not self.type:
            return
        elif self.type == self.local_name and self.type == self.standard_name:
            self.type_kind_var = True
        elif self.local_name == self.standard_name and self.type == 'integer':
            self.type_kind_var = True
        elif self.type == self.local_name or self.type == self.standard_name:
            raise Exception('Type or kind definitions must have matching local_name, standard_name and type.')

    def compatible(self, other):
        """Test if the variable is compatible another variable. This requires
        that certain variable attributes are identical. Others, for example
        len=... for character variables have less strict requirements: accept
        character(len=*) as compatible with character(len=INTEGER_VALUE).
        We defer testing units here and catch incompatible units later when
        unit-conversion code is autogenerated."""
        if self.type == 'character':
            if (self.kind == 'len=*' and other.kind.startswith('len=')) or \
                   (self.kind.startswith('len=') and other.kind == 'len=*'):
                return self.standard_name == other.standard_name \
                    and self.type == other.type \
                    and self.rank == other.rank
        return self.standard_name == other.standard_name \
            and self.type == other.type \
            and self.kind == other.kind \
            and self.rank == other.rank

    def convert_to(self, units):
        """Generate action to convert data in the variable's units to other units"""
        function_name = '{0}__to__{1}'.format(string_to_python_identifier(self.units), string_to_python_identifier(units))
        try:
            function = getattr(unit_conversion, function_name)
        except AttributeError:
            raise Exception('Error, automatic unit conversion from {0} to {1} not implemented'.format(self.units, units))
        conversion = function()
        self._actions['out'] = function()

    def convert_from(self, units):
        """Generate action to convert data in other units to the variable's units"""
        function_name = '{1}__to__{0}'.format(string_to_python_identifier(self.units), string_to_python_identifier(units))
        try:
            function = getattr(unit_conversion, function_name)
        except AttributeError:
            raise Exception('Error, automatic unit conversion from {1} to {0} not implemented'.format(self.units, units))
        conversion = function()
        self._actions['in'] = function()

    def print_module_use(self):
        '''Print the module use line for the variable.'''
        if not self.type_kind_var:
            raise Exception('Variable function print_module_use is only implemented for kind/type definitions')
        for item in self.container.split(' '):
            if item.startswith('MODULE_'):
                module = item.replace('MODULE_', '')
                break
        str = 'use {module}, only: {varname}'.format(module=module,varname=self.local_name)
        return str

    def print_def_pointer(self):
        '''Print the definition line for the variable, using pointers'''
        if self.type in STANDARD_VARIABLE_TYPES:
            if self.kind:
                str = "{s.type}({s._kind}), pointer :: {s.local_name}{s.rank}"
            else:
                str = "{s.type}, pointer :: {s.local_name}{s.rank}"
        else:
            if self.kind:
                error_message = "Generating variable definition statements for derived types with" + \
                                " kind attributes not implemented; variable: {0}".format(self.standard_name)
                raise Exception(error_message)
            else:
                str = "type({s.type}), pointer     :: {s.local_name}{s.rank}"
        return str.format(s=self)

    def print_def_intent(self):
        '''Print the definition line for the variable, using intent.'''
        if self.type in STANDARD_VARIABLE_TYPES:
            if self.kind:
                str = "{s.type}({s._kind}), intent({s.intent}) :: {s.local_name}{s.rank}"
            else:
                str = "{s.type}, intent({s.intent}) :: {s.local_name}{s.rank}"
        else:
            if self.kind:
                error_message = "Generating variable definition statements for derived types with" + \
                                " kind attributes not implemented; variable: {0}".format(self.standard_name)
                raise Exception(error_message)
            else:
                str = "type({s.type}), intent({s.intent}) :: {s.local_name}{s.rank}"
        return str.format(s=self)

    def print_def_local(self):
        '''Print the definition line for the variable, assuming it is a local variable.'''
        if self.type in STANDARD_VARIABLE_TYPES:
            if self.kind:
                if self.rank:
                    str = "{s.type}({s._kind}), dimension{s.rank}, allocatable :: {s.local_name}"
                else:
                    str = "{s.type}({s._kind}) :: {s.local_name}"
            else:
                if self.rank:
                    str = "{s.type}, dimension{s.rank}, allocatable :: {s.local_name}"
                else:
                    str = "{s.type} :: {s.local_name}"
        else:
            if self.kind:
                error_message = "Generating variable definition statements for derived types with" + \
                                " kind attributes not implemented; variable: {0}".format(self.standard_name)
                raise Exception(error_message)
            else:
                if self.rank:
                    str = "type({s.type}), dimension{s.rank}, allocatable :: {s.local_name}"
                else:
                    str = "type({s.type}) :: {s.local_name}"
        return str.format(s=self)

    def print_get(self, index=0):
        '''Print the data retrieval line for the variable. Depends on the type and of variable.
        If index (= location of variable in cdata structure) is supplied, pass to Fortran call.'''
        if index==0:
            index_string = ''
        else:
            index_string = ', index={index}'.format(index=index)
        if self.type in STANDARD_VARIABLE_TYPES and self.rank == '':
            str='''
        call ccpp_field_get(cdata, '{s.standard_name}', {s.local_name}, ierr=ierr, kind=ckind{index_string})
#ifdef DEBUG
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve {s.standard_name} from CCPP data structure')
            return
        end if
        if (kind({s.local_name}).ne.ckind) then
            call ccpp_error('Kind mismatch for variable {s.standard_name}')
            ierr = 1
            return
        end if
#endif
        '''
        elif self.type in STANDARD_VARIABLE_TYPES:
            str='''
        call ccpp_field_get(cdata, '{s.standard_name}', {s.local_name}, ierr=ierr, dims=cdims, kind=ckind{index_string})
#ifdef DEBUG
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve {s.standard_name} from CCPP data structure')
            return
        end if
        if (kind({s.local_name}).ne.ckind) then
            call ccpp_error('Kind mismatch for variable {s.standard_name}')
            ierr = 1
            return
        end if
#endif
        deallocate(cdims)
        '''
        # Derived-type variables, scalar
        elif self.rank == '':
            str='''
        call ccpp_field_get(cdata, '{s.standard_name}', cptr, ierr=ierr, kind=ckind{index_string})
#ifdef DEBUG
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve {s.standard_name} from CCPP data structure')
            return
        end if
        if (ckind.ne.CCPP_GENERIC_KIND) then
            call ccpp_error('Kind mismatch for variable {s.standard_name}')
            ierr = 1
            return
        end if
#endif
        call c_f_pointer(cptr, {s.local_name})'''
        # Derived-type variables, array
        else:
            str='''
        call ccpp_field_get(cdata, '{s.standard_name}', cptr, ierr=ierr, dims=cdims, kind=ckind{index_string})
#ifdef DEBUG
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve {s.standard_name} from CCPP data structure')
            return
        end if
        if (ckind.ne.CCPP_GENERIC_KIND) then
            call ccpp_error('Kind mismatch for variable {s.standard_name}')
            ierr = 1
            return
        end if
#endif
        call c_f_pointer(cptr, {s.local_name}, cdims)
        deallocate(cdims)
        '''
        return str.format(s=self, index_string=index_string)

    def print_add(self, ccpp_data_structure, index=0):
        '''Print the data addition line for the variable. Depends on the type of variable.
        Since the name of the ccpp data structure is not known, this needs to be filled later.
        In case of errors a message is printed to screen; using 'return' statements as above
        for ccpp_field_get is not possible, since the ccpp_field_add statements may be placed
        inside OpenMP parallel regions.
        If index (= location of variable in cdata structure) is supplied, pass to Fortran call.'''
        # Index string to test that index generated by CCPP prebuild matches
        # the actual index in the cdata lookup table
        if index==0:
            index_string = ''
        else:
            index_string = ', index={index}'.format(index=index)
        # Standard-type variables, scalar and array
        if self.type in STANDARD_VARIABLE_TYPES:
            str='''
            call ccpp_field_add({ccpp_data_structure}, '{s.standard_name}', {s.target}, ierr=ierr, units='{s.units}'{index_string})
            if (ierr /= 0) then
                call ccpp_error('Unable to add field "{s.standard_name}" to CCPP data structure')
            end if'''
        # Derived-type variables, scalar
        elif self.rank == '':
            str='''
            call ccpp_field_add({ccpp_data_structure}, '{s.standard_name}', '', c_loc({s.target}), ierr=ierr{index_string})
            if (ierr /= 0) then
                call ccpp_error('Unable to add field "{s.standard_name}" to CCPP data structure')
            end if'''
        # Derived-type variables, array
        else:
            str='''
            call ccpp_field_add({ccpp_data_structure}, '{s.standard_name}', '', c_loc({s.target}), rank=size(shape({s.target})), dims=shape({s.target}), ierr=ierr{index_string})
            if (ierr /= 0) then
                call ccpp_error('Unable to add field "{s.standard_name}" to CCPP data structure')
            end if'''
        return str.format(ccpp_data_structure=ccpp_data_structure, s=self, index_string=index_string)

    def print_debug(self):
        '''Print the data retrieval line for the variable.'''
        str='''Contents of {s} (* = mandatory for compatibility):
        standard_name = {s.standard_name} *
        long_name     = {s.long_name}
        units         = {s.units} *
        local_name    = {s.local_name}
        type          = {s.type} *
        rank          = {s.rank} *
        kind          = {s.kind} *
        intent        = {s.intent}
        optional      = {s.optional}
        target        = {s.target}
        container     = {s.container}
        type_kind_var = {s.type_kind_var}
        actions       = {s.actions}'''
        return str.format(s=self)

    @classmethod
    def from_table(cls, columns, data):
        var = cls()
        var.standard_name = data[columns.index('standard_name')]
        var.long_name     = data[columns.index('long_name')]
        var.units         = data[columns.index('units')]
        var.local_name    = data[columns.index('local_name')]
        var.rank          = int(data[columns.index('rank')])
        var.type          = data[columns.index('type')]
        var.kind          = data[columns.index('kind')]
        var.intent        = data[columns.index('intent')]
        var.optional      = data[columns.index('optional')]
        var.type_kind_test()
        return var

    def to_xml(self, element):
        element.set('name', self._standard_name)
        sub_element = ET.SubElement(element, 'standard_name')
        sub_element.text = self._standard_name
        sub_element = ET.SubElement(element, 'long_name')
        sub_element.text = self._long_name
        sub_element = ET.SubElement(element, 'units')
        sub_element.text = self._units
        sub_element = ET.SubElement(element, 'local_name')
        sub_element.text = self._local_name
        sub_element = ET.SubElement(element, 'type')
        sub_element.text = self._type
        sub_element = ET.SubElement(element, 'rank')
        sub_element.text = self._rank
        sub_element = ET.SubElement(element, 'intent')
        sub_element.text = self._intent
        sub_element = ET.SubElement(element, 'optional')
        sub_element.text = self._optional
        sub_element = ET.SubElement(element, 'container')
        sub_element.text = self._container
        return element

###############################################################################
class Cap(object):

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
!! @brief Auto-generated cap module for the {module} scheme
!!
!
module {module}_cap

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_f_pointer, c_ptr, c_int32_t
    use            :: ccpp_types,                                      &
                      only: ccpp_t, CCPP_GENERIC_KIND
    use            :: ccpp_fields,                                     &
                      only: ccpp_field_get
    use            :: ccpp_errors,                                     &
                      only: ccpp_error, ccpp_debug
    use            :: {module}, &
                      only: {subroutines}
    ! Other modules required, e.g. type definitions
    {module_use}

    implicit none

    private
    public :: {subroutine_caps}

    contains

'''

    sub='''
    function {subroutine}_cap(ptr) bind(c) result(ierr)

        integer(c_int32_t)         :: ierr
        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer           :: cdata
        type(c_ptr)                     :: cptr
        integer, allocatable            :: cdims(:)
        integer                         :: ckind
{var_defs}

        ierr = 0

        call c_f_pointer(ptr, cdata)

{var_gets}

{actions_before}

        call {subroutine}({args})
        {ierr_assign}

{actions_after}

    end function {subroutine}_cap
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, module, data, ccpp_field_map, metadata_define):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        subs = ','.join(["{0}".format(s) for s in data.keys()])
        sub_caps = ','.join(["{0}_cap".format(s) for s in data.keys()])

        # Import variable type definitions for all subroutines (init, run, finalize)
        module_use = []
        local_kind_and_type_vars = []
        for sub in data.keys():
            for var in data[sub]:
                if var.type in STANDARD_VARIABLE_TYPES and var.kind and not var.type == STANDARD_CHARACTER_TYPE:
                    kind_var_standard_name = var.kind
                    if not kind_var_standard_name in local_kind_and_type_vars:
                        if not kind_var_standard_name in metadata_define.keys():
                            raise Exception("Kind {kind} not defined by host model".format(kind=kind_var_standard_name))
                        kind_var = metadata_define[kind_var_standard_name][0]
                        module_use.append(kind_var.print_module_use())
                        local_kind_and_type_vars.append(kind_var_standard_name)
                elif not var.type in STANDARD_VARIABLE_TYPES:
                    type_var_standard_name = var.type
                    if not type_var_standard_name in local_kind_and_type_vars:
                        if not type_var_standard_name in metadata_define.keys():
                            raise Exception("Type {type} not defined by host model".format(type=type_var_standard_name))
                        type_var = metadata_define[type_var_standard_name][0]
                        module_use.append(type_var.print_module_use())
                        local_kind_and_type_vars.append(type_var_standard_name)
        del local_kind_and_type_vars

        f.write(Cap.header.format(module = module,
                                  module_use = '\n    '.join(module_use),
                                  subroutines = subs,
                                  subroutine_caps = sub_caps))

        for sub in data.keys():
            # Treat CCPP internal variables differently: do not retrieve
            # via ccpp_field_get, use them directly via cdata%...
            # (configured in common.py, needs to match what is is ccpp_types.F90)
            var_defs = "\n".join([" "*8 + x.print_def_pointer() for x in data[sub] if x.standard_name not in CCPP_INTERNAL_VARIABLES.keys()])
            # Use lookup index in cdata from build time for faster retrieval
            var_gets = "\n".join([x.print_get(ccpp_field_map[x.standard_name]) for x in data[sub]if x.standard_name not in CCPP_INTERNAL_VARIABLES.keys()])
            # Generate unit conversion statements on input and output. Special handling for
            # unit conversions for intent(in) variables, these don't require defining a
            # temporary variable, instead just pass the conversion function as argument
            actions_before = ''
            actions_after  = ''
            tmpvar_cnt     = 0
            tmpvars        = {}
            for x in data[sub]:
                if x.actions['out']:
                    tmpvar_cnt += 1
                    tmpvar = copy.deepcopy(x)
                    tmpvar.local_name = 'tmpvar{0}'.format(tmpvar_cnt)
                    var_defs += '\n' + " "*8 + tmpvar.print_def_local()
                    if x.rank:
                        actions_before += '        allocate({t}, source={x})\n'.format(t=tmpvar.local_name, x=x.local_name)
                    if x.actions['in']:
                        actions_before += '        {t} = {c}\n'.format(t=tmpvar.local_name,
                                                                       c=x.actions['in'].format(var=x.local_name,
                                                                                                kind='_' + x.kind if x.kind else ''))
                    actions_after  += '        {x} = {c}\n'.format(x=x.local_name,
                                                                   c=x.actions['out'].format(var=tmpvar.local_name,
                                                                                             kind='_' + x.kind if x.kind else ''))
                    if x.rank:
                        actions_after += '        deallocate({t})\n'.format(t=tmpvar.local_name)
                    tmpvars[x.local_name] = tmpvar.local_name
            # Split args so that lines don't exceed 260 characters (for PGI)
            args = ''
            length = 0
            for x in data[sub]:
                if x.standard_name in CCPP_INTERNAL_VARIABLES.keys():
                    arg = "{0}={1},".format(x.local_name, CCPP_INTERNAL_VARIABLES[x.standard_name])
                elif x.local_name in tmpvars.keys():
                    arg = "{0}={1},".format(x.local_name, tmpvars[x.local_name])
                elif x.actions['in'] and not x.actions['out']:
                    action = x.actions['in'].format(var=x.local_name, kind='_' + x.kind if x.kind else '')
                    arg = '{0}={1},'.format(x.local_name, action)
                else:
                    arg = "{0}={0},".format(x.local_name)
                args += arg
                length += len(arg)
                if length > 70 and not x == data[sub][-1]:
                    args += ' &\n                  '
                    length = 0
            args = args.rstrip(',')
            # If CCPP_ERROR_FLAG_VARIABLE is present, assign to ierr
            ierr_assign = ''
            for x in data[sub]:
                if x.standard_name == CCPP_ERROR_FLAG_VARIABLE:
                    ierr_assign = 'ierr={0}'.format(CCPP_INTERNAL_VARIABLES[CCPP_ERROR_FLAG_VARIABLE])
                    break
            # Write to scheme cap
            f.write(Cap.sub.format(subroutine=sub,
                                   var_defs=var_defs,
                                   var_gets=var_gets,
                                   actions_before=actions_before.rstrip('\n'),
                                   args=args,
                                   ierr_assign=ierr_assign,
                                   actions_after=actions_after.rstrip('\n')))
        f.write("end module {module}_cap\n".format(module = module))

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class CapsMakefile(object):

    header='''
# All CCPP caps are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
CAPS_F90 ='''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, caps):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for cap in caps:
            contents += ' \\\n\t   {0}'.format(cap)
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class CapsCMakefile(object):

    header='''
# All CCPP caps are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
set(CAPS
'''
    footer=''')
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, schemes):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for scheme in schemes:
            contents += '      {0}\n'.format(scheme)
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class SchemesMakefile(object):

    header='''
# All CCPP schemes are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
SCHEMES_F =

SCHEMES_F90 =

SCHEMES_f =

SCHEMES_f90 ='''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, schemes):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        schemes_F = 'SCHEMES_F ='
        schemes_F90 = 'SCHEMES_F90 ='
        schemes_f = 'SCHEMES_f ='
        schemes_f90 = 'SCHEMES_f90 ='
        for scheme in schemes:
            if scheme.endswith('.F'):
                schemes_F += ' \\\n\t   {0}'.format(scheme)
            elif scheme.endswith('.F90'):
                schemes_F90 += ' \\\n\t   {0}'.format(scheme)
            elif scheme.endswith('.f'):
                schemes_f += ' \\\n\t   {0}'.format(scheme)
            elif scheme.endswith('.f90'):
                schemes_f90 += ' \\\n\t   {0}'.format(scheme)
        contents = contents.replace('SCHEMES_F =', schemes_F)
        contents = contents.replace('SCHEMES_F90 =', schemes_F90)
        contents = contents.replace('SCHEMES_f =', schemes_f)
        contents = contents.replace('SCHEMES_f90 =', schemes_f90)
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class SchemesCMakefile(object):

    header='''
# All CCPP schemes are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
set(SCHEMES
'''
    footer=''')
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, schemes):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for scheme in schemes:
            contents += '      {0}\n'.format(scheme)
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

###############################################################################
if __name__ == "__main__":
    main()
