#!/usr/bin/env python
#
# Class to hold all information on a CCPP metadata variable
#

from __future__ import print_function
import logging
import re
import xml.etree.ElementTree as ET
from parse_tools import check_fortran_id, check_fortran_type
from parse_tools import check_dimensions, check_cf_standard_name
from parse_tools import ParseContext, ParseSource, ParseSyntaxError

logger = logging.getLogger(__name__)
logger.addHandler(logging.StreamHandler())

real_subst_re = re.compile(r"(.*\d)p(\d.*)")
list_re = re.compile(r"[(]([^)]*)[)]\s*$")

########################################################################

def standard_name_to_long_name(standard_name):
    """Translate a standard_name to its default long_name
    >>> standard_name_to_long_name('cloud_optical_depth_layers_from_0p55mu_to_0p99mu')
    'Cloud optical depth layers from 0.55mu to 0.99mu'
    """
    # We assume that standar_name has been checked for validity
    # Make the first char uppercase and replace each underscore with a space
    long_name = standard_name[0].upper() + re.sub("_", " ", standard_name[1:])
    # Next, substitute a decimal point for the p in [:digit]p[:digit]
    match = real_subst_re.match(long_name)
    while match is not None:
        long_name = match.group(1) + '.' + match.group(2)
        match = real_subst_re.match(long_name)
    # End while
    return long_name

########################################################################

class VariableProperty(object):
    """Class to represent a single property of a metadata header entry
    >>> VariableProperty('local_name', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('standard_name', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('long_name', str) #doctest: +ELLIPSIS
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

    def __init__(self, name_in, type_in, valid_values_in=None, optional_in=False, default_in=None, default_fn_in=None, check_fn_in=None):
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
            if (default_in is None) and (default_fn_in is None):
                raise ValueError('default_in or default_fn_in is a required property for {} because it is optional'.format(name_in))
            if (default_in is not None) and (default_fn_in is not None):
                raise ValueError('default_in and default_fn_in cannot both be provided')
            self._default = default_in
            self._default_fn = default_fn_in
        elif default_in is not None:
            raise ValueError('default_in is not a valid property for {} because it is not optional'.format(name_in))
        elif default_in is not None:
            raise ValueError('default_fn_in is not a valid property for {} because it is not optional'.format(name_in))
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
    def default(self, arg=None):
        if self._default is not None:
            return self._default
        else:
            if arg is not None:
                return self._default_fn(arg)
            else:
                return self._default_fn
            # End if
        # End if

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
                match = list_re.match(test_value)
                if match is None:
                    tv = None
                else:
                    tv = [x.strip() for x in match.group(1).split(',')]
                    if (len(tv) == 1) and (len(tv[0]) == 0):
                        # Scalar
                        tv = list()
                    # End if
                # End if
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

###############################################################################

class Var(object):
    """ A class to hold a metadata variable
    >>> Var.get_prop('standard_name') #doctest: +ELLIPSIS
    <__main__.VariableProperty object at 0x...>
    >>> Var.get_prop('standard')

    >>> Var.get_prop('type').is_match('type')
    True
    >>> Var.get_prop('type').is_match('long_name')
    False
    >>> Var.get_prop('type').valid_value('character')
    'character'
    >>> Var.get_prop('type').valid_value('char')

    >>> Var.get_prop('long_name').valid_value('hi mom')
    'hi mom'
    >>> Var.get_prop('dimensions').valid_value('hi mom')

    >>> Var.get_prop('dimensions').valid_value(['Bob', 'Ray'])
    ['Bob', 'Ray']
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'vtype', ParseContext())).get_prop_value('long_name')
    'Hi mom'
    """

    # __spec_props are for variables defined in a specification
    __spec_props = [VariableProperty('local_name', str,
                                     check_fn_in=check_fortran_id),
                    VariableProperty('standard_name', str,
                                     check_fn_in=check_cf_standard_name),
                    VariableProperty('long_name', str, optional_in=True,
                                     default_fn_in=standard_name_to_long_name),
                    VariableProperty('units', str),
                    VariableProperty('dimensions', list,
                                     check_fn_in=check_dimensions),
                    VariableProperty('type', str,
                                     check_fn_in=check_fortran_type),
                    VariableProperty('kind', str,
                                     optional_in=True, default_in='kind_phys'),
                    VariableProperty('state_variable', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('optional', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('module', str,
                                     optional_in=True, default_in='')]

    # __var_props contains properties which are not in __spec_props
    __var_props = [VariableProperty('intent', str,
                                    valid_values_in=['in', 'out', 'inout'])]


    __spec_propdict = {}
    __var_propdict = {}
    __required_spec_props = list()
    __required_var_props = list()
    for p in __spec_props:
        __spec_propdict[p.name] = p
        __var_propdict[p.name] = p
        if not p.optional:
            __required_spec_props.append(p.name)
            __required_var_props.append(p.name)
        # End if
    # End for
    for p in __var_props:
        __var_propdict[p.name] = p
        if not p.optional:
            __required_var_props.append(p.name)
        # End if
    # End for

    def __init__(self, prop_dict, source):
        _llevel = logger.getEffectiveLevel()
        logger.setLevel(logging.ERROR)
        if source.type is 'SCHEME':
            required_props = Var.__required_var_props
            master_propdict = Var.__var_propdict
        else:
            required_props = Var.__required_spec_props
            master_propdict = Var.__spec_propdict
        # End if
        self._source = source
        # Grab a frozen copy of the context
        self._context = ParseContext(context=source.context)
        # First, check the input
        if 'ddt_type' in prop_dict:
            # Special case to bypass normal type rules
            if 'type' not in prop_dict:
                prop_dict['type'] = 'type'
            # End if
            if 'units' not in prop_dict:
                prop_dict['units'] = ""
            # End if
            prop_dict['kind'] = prop_dict['ddt_type']
            del prop_dict['ddt_type']
        # End if
        for key in prop_dict:
            if Var.get_prop(key) is None:
                raise ValueError("Invalid metadata variable property, '{}'".format(key))
            # End if
        # End for
        # Make sure required properties are present
        for propname in required_props:
            if propname not in prop_dict:
                raise ValueError("Required property, '{}', missing".format(propname))
            # End if
        # End for
        self._prop_dict = prop_dict # Stealing dict from caller
        # Fill in default values for missing properties
        for propname in master_propdict:
            if (propname not in prop_dict) and master_propdict[propname].optional:
                default_val = master_propdict[propname].default
                if (propname == 'long_name') and callable(default_val):
                    self._prop_dict[propname] = default_val(prop_dict['standard_name'])
                else:
                    self._prop_dict[propname] = default_val
                # End if
            # End if
        # End for
        logger.setLevel(logging.WARNING if _llevel == logging.NOTSET else _llevel)

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
    def get_prop(cls, name, spec_type=None):
        if (spec_type is None) and (name in Var.__var_propdict):
            return Var.__var_propdict[name]
        elif (spec_type is not None) and (name in Var.__spec_propdict):
            return Var.__spec_propdict[name]
        else:
            return None

    def get_prop_value(self, name):
        if name in self._prop_dict:
            return self._prop_dict[name]
        else:
            return None

    @property
    def context(self):
        return self._context

    @context.setter
    def context(self, value):
        'Do not allow context to be reset'
        logger.warning('Cannot set value of context')

    @property
    def source(self):
        return self._source

    @source.setter
    def source(self, value):
        'Do not allow source to be reset'
        logger.warning('Cannot set value of source')

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
        str='''Contents of {local_name} (* = mandatory for compatibility):
        standard_name = {standard_name} *
        long_name     = {long_name}
        units         = {units} *
        local_name    = {local_name}
        type          = {type} *
        dimensions    = {dimensions} *
        kind          = {kind} *
        intent        = {intent}
        optional      = {optional}
        '''
        if self._context is not None:
            str = str + '\n        context       = {}'.format(self._context)
        # End if
        return str.format(**self._prop_dict)

###############################################################################

class VarDictionary(dict):
    """
    A class to store and cross-check variables from one or more metadata
    headers.
    The dictionary is organized by standard_name with each entry a list
    containing all the known variables sharing that standard_name.
    >>> VarDictionary()
    {}
    >>> VarDictionary({})
    {}
    >>> VarDictionary(Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'vtype', ParseContext()))) #doctest: +ELLIPSIS
    {'hi_mom': [<__main__.Var object at 0x...>]}
    >>> VarDictionary([Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'vtype', ParseContext()))]) #doctest: +ELLIPSIS
    {'hi_mom': [<__main__.Var object at 0x...>]}
    >>> VarDictionary().add_variable(Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'vtype', ParseContext())))

    """

    def __init__(self, variables=None):
        "Unlike dict, VarDictionary only takes a Var or Var list"
        super(VarDictionary, self).__init__()
        if type(variables) is Var:
            self.add_variable(variables)
        elif variables is not None:
            for var in variables:
                self.add_variable(var)
            # End for
        # End if

    def variable_list(self, standard_name=None):
        "Retrieve variables matching <standard_name> (default all variables)"
        if standard_name is None:
            # Return a list of all variables in the system
            vlist = list()
            for sn in self.keys():
                vlist.extend(self[sn])
            # End for
        elif standard_name in self:
            # Return a list of all variables matching <standard_name>
            vlist = self[standard_name]
        else:
            vlist = None
        # End if
        return vlist

    def add_variable(self, newvar):
        """Add a variable if it does not conflict with existing entries"""
        standard_name = newvar.get_prop_value('standard_name')
        if standard_name in self:
            # We have a matching variable, is it legal?
            currvars = self.variable_list(standard_name)
            for cvar in currvars:
                # Are we in the same context? Compare region info
                if newvar.source == cvar.source:
                    logger.error("Attempt to add duplicate variable, {} from {}".format(standard_name, newvar.source.name))
                    raise ParseSyntaxError("Duplicate standard name",
                                           token=standard_name,
                                           context=cvar._context)
                elif not cvar.compatible(newvar):
                    errstr = "Standard name incompatible with {}"
                    raise ParseSyntaxError(errstr.format(cvar._context),
                                           token=standard_name,
                                           context=newvar.source.context)
                # End if
            # End for
        else:
            self[standard_name] = list()
        # End if
        # If we make it to here without an exception, add the variable
        self[standard_name].append(newvar)

    def has_variable(self, other_var):
        "Test wither <other_var> is already in our dictionary"
        in_dict = False
        standard_name = other_var.get_prop_value('standard_name')
        var_list = self.variable_list(standard_name)
        if var_list is not None:
            for cvar in var_list:
                if cvar.source == other_var.source:
                    in_dict = True
                    break
                # End if
            # End for
        # End if
        return in_dict

    def merge(self, other_dict):
        "Add new entries from <other_dict>"
        for ovar in other_dict.variable_list():
            if not self.has_variable(ovar):
                self.add_variable(ovar)
            # End if
        # End for

###############################################################################

CCPP_ERROR_STATUS_VARIABLE = 'error_status'

###############################################################################
if __name__ == "__main__":
    import doctest
    doctest.testmod()
