#!/usr/bin/env python
#
# Class to hold all information on a CCPP metadata variable
#

# Python library imports
from __future__ import print_function
import re
import xml.etree.ElementTree as ET
from collections import OrderedDict
# CCPP framework imports
from parse_tools import check_fortran_id, check_fortran_type
from parse_tools import check_dimensions, check_cf_standard_name
from parse_tools import ParseContext, ParseSource, ParseSyntaxError, CCPPError

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
    >>> VariableProperty('value', int, valid_values_in=[1, 2 ]).valid_value('2')
    2
    >>> VariableProperty('value', int, valid_values_in=[1, 2 ]).valid_value('3')

    >>> VariableProperty('value', int, valid_values_in=[1, 2 ]).valid_value('3', error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal value variable property, '3'
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('()')
    []
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x)')
    ['x']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('x')

    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x:y)')
    ['x:y']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(w:x,y:z)')
    ['w:x', 'y:z']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(w:x,x:y:z)', error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'x:y:z' is an invalid dimension range
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x:3y)', error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: '3y' is not a valid Fortran identifier
    """

    def __init__(self, name_in, type_in, valid_values_in=None, optional_in=False, default_in=None, default_fn_in=None, check_fn_in=None):
        self._name = name_in
        self._type = type_in
        if self._type not in [ bool, int, list, str ]:
            raise CCPPError("{} has illegal VariableProperty type, '{}'".format(name_in, type_in))
        # End if
        self._valid_values = valid_values_in
        self._optional = optional_in
        if self.optional:
            if (default_in is None) and (default_fn_in is None):
                raise CCPPError('default_in or default_fn_in is a required property for {} because it is optional'.format(name_in))
            if (default_in is not None) and (default_fn_in is not None):
                raise CCPPError('default_in and default_fn_in cannot both be provided')
            self._default = default_in
            self._default_fn = default_fn_in
        elif default_in is not None:
            raise CCPPError('default_in is not a valid property for {} because it is not optional'.format(name_in))
        elif default_in is not None:
            raise CCPPError('default_fn_in is not a valid property for {} because it is not optional'.format(name_in))
        self._check_fn = check_fn_in

    @property
    def name(self):
        'Return the name of the property'
        return self._name

    @property
    def type(self):
        'Return the type of the property'
        return self._type

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

    @property
    def optional(self):
        return self._optional

    def is_match(self, test_name):
        "Return True iff <test_name> is the name of this property"
        return self.name.lower() == test_name.lower()

    def valid_value(self, test_value, error=False):
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
            except CCPPError:
                valid_val = None # Redundant but more expressive than pass
        elif self.type is list:
            if isinstance(test_value, str):
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
            if isinstance(tv, list):
                valid_val = tv
            elif isinstance(tv, tuple):
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
            if isinstance(test_value, str):
                valid_val = (test_value in ['True', 'False']) or (test_value.lower() in ['t', 'f', '.true.', '.false.'])
            else:
                valid_val = not not test_value
        elif self.type is str:
            if isinstance(test_value, str):
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
            valid_val = self._check_fn(valid_val, error=error)
        elif (valid_val is None) and error:
            raise CCPPError("Illegal {} variable property, '{}'".format(self.name, test_value))
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
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext())).get_prop_value('long_name')
    'Hi mom'
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext())).get_prop_value('intent')
    'in'
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'ttype' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))
    Traceback (most recent call last):
    ParseSyntaxError: Invalid metadata variable property, 'ttype', at <standard input>:1
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))
    Traceback (most recent call last):
    ParseSyntaxError: Required property, 'units', missing, at <standard input>:1
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'inout', 'constant' : '.true.'}, ParseSource('vname', 'SCHEME', ParseContext())) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: foo is marked constant but is intent inout, at <standard input>:1
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'ino'}, ParseSource('vname', 'SCHEME', ParseContext())) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Illegal intent variable property, 'ino', at <standard input>:1
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
                                     optional_in=True, default_in=''),
                    VariableProperty('constant', bool,
                                     optional_in=True, default_in=False)]

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
                raise ParseSyntaxError("Invalid metadata variable property, '{}'".format(key), context=self.context)
            # End if
        # End for
        # Make sure required properties are present
        for propname in required_props:
            if propname not in prop_dict:
                raise ParseSyntaxError("Required property, '{}', missing".format(propname), context=self.context)
            # End if
        # End for
        # Check for any mismatch
        if ('constant' in prop_dict) and ('intent' in prop_dict):
            if prop_dict['intent'].lower() != 'in':
                raise ParseSyntaxError("{} is marked constant but is intent {}".format(prop_dict['local_name'], prop_dict['intent']), context=self.context)
            # End if
        # End if
        # Steal dict from caller
        self._prop_dict = prop_dict
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
        # Make sure all the variable values are valid
        try:
            for prop in self._prop_dict.keys():
                check = Var.get_prop(prop).valid_value(self._prop_dict[prop],
                                                       error=True)
            # End for
        except CCPPError as cp:
            raise ParseSyntaxError(cp, context=self.context)
        # End try

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

    @property
    def source(self):
        return self._source

    def write_def(self, outfile, indent):
        '''Write the definition line for the variable.'''
        vtype = self.get_prop_value('type')
        kind = self.get_prop_value('kind')
        name = self.get_prop_value('local_name')
        dimval = self.get_prop_value('dimensions')
        dims = Var.get_prop('dimensions').valid_value(dimval)
        if (dims is not None) and (len(dims) > 0):
            dimstr = '({})'.format(', '.join(dims))
        else:
            dimstr = ''
        # End if
        constant = self.get_prop_value('constant')
        intent = self.get_prop_value('intent')
        if constant is not None:
            const_str = ', indent(in)'.format(intent)
        elif intent is not None:
            intent_str = ', intent({})'.format(intent)
        else:
            intent_str = ''
        # End if
        if vtype == 'type':
            str = "type({kind}){intent}     :: {name}{dims}"
        else:
            if (kind is not None) and (len(kind) > 0):
                str = "{type}({kind}){param}{intent} :: {name}{dims}"
            else:
                str = "{type}{param}{intent} :: {name}{dims}"
            # End if
        # End if
        outfile.write(str.format(type=vtype, kind=kind, intent=intent_str,
                                 name=name, dims=dimstr), indent)

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

class VarDictionary(OrderedDict):
    """
    A class to store and cross-check variables from one or more metadata
    headers.
    The dictionary is organized by standard_name with each entry a list
    containing all the known variables sharing that standard_name.
    >>> VarDictionary()
    VarDictionary()
    >>> VarDictionary({})
    VarDictionary()
    >>> VarDictionary(Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))) #doctest: +ELLIPSIS
    VarDictionary([('hi_mom', [<__main__.Var object at 0x...>])])
    >>> VarDictionary([Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))]) #doctest: +ELLIPSIS
    VarDictionary([('hi_mom', [<__main__.Var object at 0x...>])])
    >>> VarDictionary().add_variable(Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext())))

    >>> VarDictionary([Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))]).prop_list('local_name')
    ['foo']
    """

    def __init__(self, variables=None, logger=None):
        "Unlike dict, VarDictionary only takes a Var or Var list"
        super(VarDictionary, self).__init__()
        self._logger = logger
        if isinstance(variables, Var):
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
                    if self._logger is not None:
                        self._logger.error("Attempt to add duplicate variable, {} from {}".format(standard_name, newvar.source.name))
                    # End if
                    raise ParseSyntaxError("Duplicate standard name",
                                           token=standard_name,
                                           context=cvar._context)
                elif not cvar.compatible(newvar):
                    errstr = "Standard name incompatible with {}"
                    raise ParseSyntaxError(errstr.format(cvar.context),
                                           token=standard_name,
                                           context=newvar.source.context)
                # End if
            # End for
        else:
            self[standard_name] = list()
        # End if
        # If we make it to here without an exception, add the variable
        self[standard_name].append(newvar)

    def find_variable(self, standard_name, list_ok=False):
        "Return the host model variable matching <standard_name> or None"
        vlist = self.variable_list(standard_name)
        if vlist is not None:
            if (not isinstance(vlist, list)) or (len(vlist) < 1):
                raise CCPPError("Illegal VarDictionary entry, '{}'".format(vlist))
            elif (len(vlist) > 1) and (not list_ok):
                raise CCPPError("Duplicate variable, '{}'".format(standard_name))
            # End if
            if list_ok:
                var = vlist
            else:
                var = vlist[0]
            # End if
        else:
            var = None
        # End if
        return var

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

    def prop_list(self, prop_name):
        '''Return a list of the <prop_name> property for each
        variable. This method only allows one variable per standard name'''
        plist = list()
        for standard_name in self.keys():
            vlist = self.variable_list(standard_name)
            if (not isinstance(vlist, list)) or (len(vlist) < 1):
                raise CCPPError("Illegal VarDictionary entry, '{}'".format(vlist))
            elif len(vlist) > 1:
                raise CCPPError("Duplicate variable, '{}'".format(standard_name))
            else:
                pval = vlist[0].get_prop_value(prop_name)
                if pval is not None:
                    plist.append(pval)
                # End if (no else, just ignore variables without <prop_name>)
            # End if
        # End for
        return plist

    def declare_variables(self, outfile, indent):
        "Write out the declarations for this dictionary's variables"
        for standard_name in self.keys():
            vlist = self.variable_list(standard_name)
            if (not isinstance(vlist, list)) or (len(vlist) < 1):
                raise CCPPError("Illegal VarDictionary entry, '{}'".format(vlist))
            elif len(vlist) > 1:
                raise CCPPError("Duplicate variable, '{}'".format(standard_name))
            else:
                vlist[0].write_def(outfile, indent)
            # End if
        # End for

    def merge(self, other_dict):
        "Add new entries from <other_dict>"
        for ovar in other_dict.variable_list():
            if not self.has_variable(ovar):
                self.add_variable(ovar)
            # End if
        # End for

###############################################################################
if __name__ == "__main__":
    import doctest
    doctest.testmod()
