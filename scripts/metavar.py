#!/usr/bin/env python
#
# Class to hold all information on a CCPP metadata variable
#

from __future__ import print_function
import logging
import ast
import xml.etree.ElementTree as ET
from parse_tools import check_fortran_id, check_fortran_type
from parse_tools import check_dimensions, check_cf_standard_name

logger = logging.getLogger(__name__)

########################################################################

class VariableProperty(object):
    """Class to represent a single property of a metadata header entry
    >>> VariableProperty('local_name', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('standard_name', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('description', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('units', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('dimensions', list) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('type', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('kind', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('state_variable', str, valid_values_in=['True',   'False', '.true.', '.false.' ], optional_in=True, default_in=False) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('intent', str, valid_values_in=['in', 'out', 'inout']) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('optional', str, valid_values_in=['True',   'False', '.true.', '.false.' ], optional_in=True, default_in=False) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('local_name', str).name
    'local_name'
    >>> VariableProperty('standard_name', str).type
    <type 'str'>
    >>> VariableProperty('units', str).is_match('units')
    True
    >>> VariableProperty('units', str).is_match('UNITS')
    True
    >>> VariableProperty('units', str).is_match('type')
    False
    """

    def __init__(self, name_in, type_in, valid_values_in=None, optional_in=False, default_in=None, check_fn_in=None):
        _llevel = logger.getEffectiveLevel()
        logger.setLevel(logging.ERROR)
        self._name = name_in
        self._type = type_in
        if self._type not in [ bool, int, list, str ]:
            raise ValueError("{} has illegal VariableProperty type, '{}'".format(name_in, type_in))
        # End if
        self._valid_values = valid_values_in
        self._optional = optional_in
        if self.optional:
            if default_in is None:
                raise ValueError('default is a required property for {} because it is optional'.format(name_in))
            self._default = default_in
        elif default_in is not None:
            raise ValueError('default is not a valid property for {} because it is not optional'.format(name_in))
        self._check_fn = check_fn_in
        logger.setLevel(logging.WARNING if _llevel == logging.NOTSET else _llevel)

    @property
    def name(self):
        'Return the name of the property'
        return self._name

    @name.setter
    def name(self, value):
        'Do not allow the name to be set'
        logger.warning('Cannot set value of name')

    @property
    def type(self):
        'Return the type of the property'
        return self._type

    @type.setter
    def type(self, value):
        'Do not allow the type to be set'
        logger.warning('Cannot set value of type')

    @property
    def default(self):
        return self._default

    @default.setter
    def default(self, value):
        'Do not allow default to be reset'
        logger.warning('Cannot set value of default')

    @property
    def optional(self):
        return self._optional

    @optional.setter
    def optional(self, value):
        'Do not allow optional to be reset'
        logger.warning('Cannot set value of optional')

    def is_match(self, test_name):
        "Return True iff <test_name> is the name of this property"
        return self.name.lower() == test_name.lower()

    def valid_value(self, test_value):
        'Return True iff test_value is valid'
        valid_val = None
        if self.type is int:
            try:
                tv = int(test_value)
                if self._valid_values is not None:
                    if tv in self._valid_values:
                        valid_val = tv
                    else:
                        valid_val = None # i.e. pass
                else:
                    valid_val = tv
            except ValueError:
                valid_val = None # Redundant but more expressive than pass
        elif self.type is list:
            if type(test_value) is str:
                try:
                    tvv = ast.literal_eval(test_value)
                    if type(tvv) is str:
                        # There was only one dimension, convert to list
                        tv = list()
                        tv.append(tvv)
                    else:
                        tv = tvv
                except SyntaxError as se:
                    tv = None
                except ValueError as ve:
                    tv = None
            else:
                tv = test_value
            # End if
            if type(tv) is list:
                valid_val = tv
            elif type(tv) is tuple:
                valid_val = list(tv)
            else:
                valid_val = None
            # End if
            if (valid_val is not None) and (self._valid_values is not None):
                # Special case for lists, _valid_values applies to elements
                for item in valid_val:
                    if item not in self._valid_values:
                        valid_val = None
                        break
                    # End if
                # End for
            else:
                pass
        elif self.type is bool:
            if type(test_value) is str:
                valid_val = (test_value in ['True', 'False']) or (test_value.lower() in ['t', 'f', '.true.', '.false.'])
            else:
                valid_val = not not test_value
        elif self.type is str:
            if type(test_value) is str:
                if self._valid_values is not None:
                    if test_value in self._valid_values:
                        valid_val = test_value
                    else:
                        valid_val = None # i.e., pass
                else:
                    valid_val = test_value
                # End if
            # End if
        # End if
        # Call a check function?
        if valid_val and (self._check_fn is not None):
            valid_val = self._check_fn(valid_val)
        # End if
        return valid_val

########################################################################

class VariableProps(object):
    """VariableProps is used to validate property inputs
    """

###############################################################################

class Var(object):
    """ A class to hold a metadata variable
    >>> Var.get_prop('standard_name') #doctest: +ELLIPSIS
    <__main__.VariableProperty object at 0x...>
    >>> Var.get_prop('standard')

    >>> Var.get_prop('type').is_match('type')
    True
    >>> Var.get_prop('type').is_match('description')
    False
    >>> Var.get_prop('type').valid_value('character')
    'character'
    >>> Var.get_prop('type').valid_value('char')

    >>> Var.get_prop('description').valid_value('hi mom')
    'hi mom'
    >>> Var.get_prop('dimensions').valid_value('hi mom')

    >>> Var.get_prop('dimensions').valid_value(['Bob', 'Ray'])
    ['Bob', 'Ray']
    """

    __var_props = [ VariableProperty('local_name', str,
                                     check_fn_in=check_fortran_id),
                    VariableProperty('standard_name', str,
                                     check_fn_in=check_cf_standard_name),
                    VariableProperty('description', str),
                    VariableProperty('units', str),
                    VariableProperty('dimensions', list,
                                     check_fn_in=check_dimensions),
                    VariableProperty('type', str,
                                     check_fn_in=check_fortran_type),
                    VariableProperty('kind', str,
                                     optional_in=True, default_in='kind_phys'),
                    VariableProperty('state_variable', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('intent', str,
                                     valid_values_in=['in', 'out', 'inout']),
                    VariableProperty('optional', bool,
                                     optional_in=True, default_in=False) ]

    __var_propdict = {}
    __required_props = list()
    for p in __var_props:
        __var_propdict[p.name] = p
        if not p.optional:
            __required_props.append(p.name)
        # End if
    # End for

    def __init__(self, prop_dict):
        # First, check the input
        for key in prop_dict:
            if Var.get_prop(key) is None:
                raise ValueError("Invalid metadata variable property, '{}'".format(key))
            # End if
        # End for
        # Make sure required properties are present
        for propname in Var.__required_props:
            if propname not in prop_dict:
                raise ValueError("Required property, '{}', missing".format(propname))
            # End if
        # End for
        self._prop_dict = prop_dict # Stealing dict from caller
        # Fill in default values for missing properties
        for propname in Var.__var_propdict:
            if (propname not in prop_dict) and Var.__var_propdict[propname].optional:
                self._prop_dict[propname] = Var.__var_propdict[propname].default
            # End if
        # End for

    def compatible(self, other):
        # We accept character(len=*) as compatible with character(len=INTEGER_VALUE)
        if self.type == 'character':
            if (self.kind == 'len=*' and other.kind.startswith('len=')) or \
                   (self.kind.startswith('len=') and other.kind == 'len=*'):
                return self.standard_name == other.standard_name \
                    and self.units == other.units \
                    and self.type == other.type \
                    and self.rank == other.rank
        return self.standard_name == other.standard_name \
            and self.units == other.units \
            and self.type == other.type \
            and self.kind == other.kind \
            and self.rank == other.rank

    @classmethod
    def get_prop(cls, name):
        if name in Var.__var_propdict:
            return Var.__var_propdict[name]
        else:
            return None

    def get_prop_value(self, name):
        if name in self._prop_dict:
            return self._prop_dict[name]
        else:
            return None

    def print_def(self):
        '''Print the definition line for the variable.'''
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

    def print_get(self):
        '''Print the data retrieval line for the variable. Depends on the type and of variable'''
        if self.type in STANDARD_VARIABLE_TYPES and self.rank == '':
            str='''
        call ccpp_field_get(cdata, '{s.standard_name}', {s.local_name}, ierr=ierr, kind=ckind)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve {s.standard_name} from CCPP data structure')
            return
        end if
        if (kind({s.local_name}).ne.ckind) then
            call ccpp_error('Kind mismatch for variable {s.standard_name}')
            ierr = 1
            return
        end if
        '''
        elif self.type in STANDARD_VARIABLE_TYPES:
            str='''
        call ccpp_field_get(cdata, '{s.standard_name}', {s.local_name}, ierr=ierr, dims=cdims, kind=ckind)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve {s.standard_name} from CCPP data structure')
            return
        end if
        if (kind({s.local_name}).ne.ckind) then
            call ccpp_error('Kind mismatch for variable {s.standard_name}')
            ierr = 1
            return
        end if
        '''
        # Derived-type variables, scalar
        elif self.rank == '':
            str='''
        call ccpp_field_get(cdata, '{s.standard_name}', cptr, ierr=ierr, kind=ckind)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve {s.standard_name} from CCPP data structure')
            return
        end if
        if (ckind.ne.CCPP_GENERIC_KIND) then
            call ccpp_error('Kind mismatch for variable {s.standard_name}')
            ierr = 1
            return
        end if
        call c_f_pointer(cptr, {s.local_name})'''
        # Derived-type variables, array
        else:
            str='''
        call ccpp_field_get(cdata, '{s.standard_name}', cptr, ierr=ierr, dims=cdims, kind=ckind)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve {s.standard_name} from CCPP data structure')
            return
        end if
        if (ckind.ne.CCPP_GENERIC_KIND) then
            call ccpp_error('Kind mismatch for variable {s.standard_name}')
            ierr = 1
            return
        end if
        call c_f_pointer(cptr, {s.local_name}, cdims)
        deallocate(cdims)
        '''
        return str.format(s=self)

    def print_add(self, ccpp_data_structure):
        '''Print the data addition line for the variable. Depends on the type of variable.
        Since the name of the ccpp data structure is not known, this needs to be filled later.
        In case of errors a message is printed to screen; using 'return' statements as above
        for ccpp_field_get is not possible, since the ccpp_field_add statements may be placed
        inside OpenMP parallel regions.'''
        # Standard-type variables, scalar and array
        if self.type in STANDARD_VARIABLE_TYPES:
            str='''
            call ccpp_field_add({ccpp_data_structure}, '{s.standard_name}', {s.target}, ierr, '{s.units}')
            if (ierr /= 0) then
                call ccpp_error('Unable to add field "{s.standard_name}" to CCPP data structure')
            end if'''
        # Derived-type variables, scalar
        elif self.rank == '':
            str='''
            call ccpp_field_add({ccpp_data_structure}, '{s.standard_name}', '', c_loc({s.target}), ierr)
            if (ierr /= 0) then
                call ccpp_error('Unable to add field "{s.standard_name}" to CCPP data structure')
            end if'''
        # Derived-type variables, array
        else:
            str='''
            call ccpp_field_add({ccpp_data_structure}, '{s.standard_name}', '', c_loc({s.target}), rank=size(shape({s.target})), dims=shape({s.target}), ierr=ierr)
            if (ierr /= 0) then
                call ccpp_error('Unable to add field "{s.standard_name}" to CCPP data structure')
            end if'''
        return str.format(ccpp_data_structure=ccpp_data_structure, s=self)

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
        container     = {s.container}'''
        return str.format(s=self)

###############################################################################

CCPP_ERROR_STATUS_VARIABLE = 'error_status'

###############################################################################
if __name__ == "__main__":
    import doctest
    doctest.testmod()
