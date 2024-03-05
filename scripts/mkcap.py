#!/usr/bin/env python3
#
# Script to generate a cap module and subroutines
# from a scheme xml file.
#

from __future__ import print_function
import copy
import logging
import os
import sys
import getopt
import xml.etree.ElementTree as ET

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
        self._dimensions    = []
        self._container     = None
        self._kind          = None
        self._intent        = None
        self._active        = None
        self._target        = None
        self._actions       = { 'in' : None, 'out' : None }
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

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
    def dimensions(self):
        '''Get the dimensions of the variable.'''
        return self._dimensions

    @dimensions.setter
    def dimensions(self, value):
        if not isinstance(value, list):
            raise TypeError('Invalid type for variable property dimensions, must be a list')
        self._dimensions = value

    @property
    def rank(self):
        '''Get the rank of the variable. Originally, this was an integer indicating
        the number of dimensions (therefore the name), now it is a list of colons to use
        for assumed-size array definitions in Fortran.'''
        if len(self._dimensions) == 0:
            return ''
        else:
            return '('+ ','.join([':'] * len(self._dimensions)) +')'

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
    def active(self):
        '''Get the active attribute of the variable.'''
        return self._active

    @active.setter
    def active(self, value):
        if not isinstance(value, str):
            raise ValueError('Invalid value {0} for variable property active, must be a string'.format(value))
        self._active = value

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
    def actions(self):
        '''Get the action strings for the variable.'''
        return self._actions

    @actions.setter
    def actions(self, values):
        if isinstance(values, dict):
            for key in values.keys():
                if key in ['in', 'out', 'in_inactive', 'out_inactive'] and isstring(values[key]):
                    self._actions[key] = values[key]
                else:
                    raise Exception('Invalid values for variable attribute actions.')
        else:
            raise Exception('Invalid values for variable attribute actions.')

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
            logging.info('Automatic unit conversion from {0} to {1} for {2} after returning from {3}'.format(self.units, units, self.standard_name, self.container))
        except AttributeError:
            raise Exception('Error, automatic unit conversion from {0} to {1} for {2} in {3} not implemented'.format(self.units, units, self.standard_name, self.container))
        conversion = function()
        self._actions['out'] = function()

    def convert_from(self, units):
        """Generate action to convert data in other units to the variable's units"""
        function_name = '{1}__to__{0}'.format(string_to_python_identifier(self.units), string_to_python_identifier(units))
        try:
            function = getattr(unit_conversion, function_name)
            logging.info('Automatic unit conversion from {0} to {1} for {2} before entering {3}'.format(self.units, units, self.standard_name, self.container))
        except AttributeError:
            raise Exception('Error, automatic unit conversion from {1} to {0} for {2} in {3} not implemented'.format(self.units, units, self.standard_name, self.container))
        conversion = function()
        self._actions['in'] = function()

    def dimstring_local_names(self, metadata, assume_shape = False):
        '''Create the dimension string for assumed shape or explicit arrays
        in Fortran. Requires a metadata dictionary to resolve the dimensions,
        which are in CCPP standard names, to local variable names. If the 
        optional argument assume_shape is True, return an assumed shape
        dimension string with the upper bound being left undefined.'''
        # Simplest case: scalars
        if len(self.dimensions) == 0:
            return ''
        dimstring = []
        # Arrays
        for dim in self.dimensions:
            # Handle dimensions like "A:B", "A:3", "-1:Z"
            if ':' in dim:
                dims = [ x.lower() for x in dim.split(':')]
                try:
                    dim0 = int(dims[0])
                    dim0 = dims[0]
                except ValueError:
                    if not dims[0].lower() in metadata.keys():
                        raise Exception('Dimension {}, required by variable {}, not defined in metadata'.format(
                                                                           dims[0].lower(), self.standard_name))
                    dim0 = metadata[dims[0].lower()][0].local_name
                try:
                    dim1 = int(dims[1])
                    dim1 = dims[1]
                except ValueError:
                    if not dims[1].lower() in metadata.keys():
                        raise Exception('Dimension {}, required by variable {}, not defined in metadata'.format(
                                                                           dims[1].lower(), self.standard_name))
                    dim1 = metadata[dims[1].lower()][0].local_name
            # Single dimensions
            else:
                dim0 = 1
                try:
                    dim1 = int(dim)
                    dim1 = dim
                except ValueError:
                    if not dim.lower() in metadata.keys():
                        raise Exception('Dimension {}, required by variable {}, not defined in metadata'.format(
                                                                               dim.lower(), self.standard_name))
                    dim1 = metadata[dim.lower()][0].local_name
            if assume_shape:
                dimstring.append('{}:'.format(dim0))
            else:
                dimstring.append('{}:{}'.format(dim0, dim1))
        return '({})'.format(','.join(dimstring))

    def print_module_use(self):
        '''Print the module use line for the variable.'''
        for item in self.container.split(' '):
            if item.startswith('MODULE_'):
                module = item.replace('MODULE_', '')
                break
        str = 'use {module}, only: {varname}'.format(module=module,varname=self.local_name)
        return str

    def print_def_intent(self, metadata):
        '''Print the definition line for the variable, using intent. Use the metadata
        dictionary to resolve lower bounds for array dimensions.'''
        # Resolve dimensisons to local names using undefined upper bounds (assumed shape)
        dimstring = self.dimstring_local_names(metadata, assume_shape = True)
        #
        if self.type in STANDARD_VARIABLE_TYPES:
            if self.kind:
                str = "{s.type}({s._kind}), intent({s.intent}) :: {s.local_name}{dimstring}"
            else:
                str = "{s.type}, intent({s.intent}) :: {s.local_name}{dimstring}"
        else:
            if self.kind:
                error_message = "Generating variable definition statements for derived types with" + \
                                " kind attributes not implemented; variable: {0}".format(self.standard_name)
                raise Exception(error_message)
            else:
                str = "type({s.type}), intent({s.intent}) :: {s.local_name}{dimstring}"
        return str.format(s=self, dimstring=dimstring)

    def print_def_local(self, metadata):
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

    def print_debug(self):
        '''Print the data retrieval line for the variable.'''
        str='''Contents of {s} (* = mandatory for compatibility):
        standard_name = {s.standard_name} *
        long_name     = {s.long_name}
        units         = {s.units} *
        local_name    = {s.local_name}
        type          = {s.type} *
        dimensions    = {s.dimensions}
        rank          = {s.rank} *
        kind          = {s.kind} *
        intent        = {s.intent}
        active        = {s.active}
        target        = {s.target}
        container     = {s.container}
        actions       = {s.actions}'''
        return str.format(s=self)

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

    def write(self, caps):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for cap in caps:
            contents += '      {0}\n'.format(cap)
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

class CapsSourcefile(object):

    header='''
# All CCPP caps are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
export CCPP_CAPS="'''
    footer='''"
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, caps):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for cap in caps:
            contents += '{0};'.format(cap)
        contents = contents.rstrip(';')
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
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
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
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
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

class SchemesSourcefile(object):

    header='''
# All CCPP schemes are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
export CCPP_SCHEMES="'''
    footer='''"
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, schemes):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for scheme in schemes:
            contents += '{0};'.format(scheme)
        contents = contents.rstrip(';')
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

class TypedefsMakefile(object):

    header='''
# All CCPP types are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
TYPEDEFS ='''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, typedefs):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for typedef in typedefs:
            contents += ' \\\n\t   {0}'.format(typedef)
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

class TypedefsCMakefile(object):

    header='''
# All CCPP types are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
set(TYPEDEFS
'''
    footer=''')
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, typedefs):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for typedef in typedefs:
            contents += '      {0}\n'.format(typedef)
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

class TypedefsSourcefile(object):

    header='''
# All CCPP types are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
export CCPP_TYPEDEFS="'''
    footer='''"
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, typedefs):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for typedef in typedefs:
            contents += '{0};'.format(typedef)
        contents = contents.rstrip(';')
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
