#!/usr/bin/env python

"""
Classes and supporting code to hold all information on CCPP metadata variables
VariableProperty: Class which describes a single variable property
Var: Class which holds all information on a single CCPP metadata variable
VarSpec: Class to hold a standard_name description which can include dimensions
VarAction: Base class for describing actions on variables
VarLoopSubst: Class for describing a loop substitution
VarDictionary: Class to hold all CCPP variables of a CCPP unit (e.g., suite,
               scheme, host)
"""

# Python library imports
from __future__ import print_function
import re
from collections import OrderedDict
# CCPP framework imports
from parse_tools import check_local_name, check_fortran_type, context_string
from parse_tools import FORTRAN_DP_RE, FORTRAN_SCALAR_REF_RE
from parse_tools import check_dimensions, check_cf_standard_name
from parse_tools import ParseContext, ParseSource
from parse_tools import ParseInternalError, ParseSyntaxError, CCPPError

###############################################################################
_REAL_SUBST_RE = re.compile(r"(.*\d)p(\d.*)")
_LIST_RE = re.compile(r"[(]([^)]*)[)]\s*$")

# Dictionary of standard CCPP variables
CCPP_STANDARD_VARS = {
    # Variable representing the constant integer, 1
    'ccpp_constant_one' :
    {'local_name' : '1', 'protected' : 'True',
     'standard_name' : 'ccpp_constant_one',
     'units' : '1', 'dimensions' : '()', 'type' : 'integer'},
    'ccpp_error_flag' :
    {'local_name' : 'errflg', 'standard_name' : 'ccpp_error_flag',
     'units' : 'flag', 'dimensions' : '()', 'type' : 'integer'},
    'ccpp_error_message' :
    {'local_name' : 'errmsg', 'standard_name' : 'ccpp_error_message',
     'units' : '1', 'dimensions' : '()', 'type' : 'character',
     'kind' : 'len=512'},
    'horizontal_dimension' :
    {'local_name' : 'total_columns',
     'standard_name' : 'horizontal_dimension', 'units' : 'count',
     'dimensions' : '()', 'type' : 'integer'},
    'horizontal_loop_extent' :
    {'local_name' : 'horz_loop_ext',
     'standard_name' : 'horizontal_loop_extent', 'units' : 'count',
     'dimensions' : '()', 'type' : 'integer'},
    'horizontal_loop_begin' :
    {'local_name' : 'horz_col_beg',
     'standard_name' : 'horizontal_loop_begin', 'units' : 'count',
     'dimensions' : '()', 'type' : 'integer'},
    'horizontal_loop_end' :
    {'local_name' : 'horz_col_end',
     'standard_name' : 'horizontal_loop_end', 'units' : 'count',
     'dimensions' : '()', 'type' : 'integer'},
    'vertical_layer_dimension' :
    {'local_name' : 'num_model_layers',
     'standard_name' : 'vertical_layer_dimension', 'units' : 'count',
     'dimensions' : '()', 'type' : 'integer'},
    'vertical_interface_dimension' :
    {'local_name' : 'num_model_interfaces',
     'standard_name' : 'vertical_interface_dimension', 'units' : 'count',
     'dimensions' : '()', 'type' : 'integer'},
    'vertical_interface_index' :
    {'local_name' : 'layer_index',
     'standard_name' : 'vertical_interface_index', 'units' : 'count',
     'dimensions' : '()', 'type' : 'integer'}
}

# Pythonic version of a forward reference (CCPP_CONSTANT_VARS defined below)
CCPP_CONSTANT_VARS = {}
# Pythonic version of a forward reference (CCPP_VAR_LOOP_SUBST defined below)
CCPP_VAR_LOOP_SUBSTS = {}
# Loop variables only allowed during run phases
CCPP_LOOP_VAR_STDNAMES = ['horizontal_loop_extent',
                          'horizontal_loop_begin', 'horizontal_loop_end',
                          'vertical_layer_index', 'vertical_interface_index']

###############################################################################
# Supported horizontal dimensions (should be defined in CCPP_STANDARD_VARS)
CCPP_HORIZONTAL_DIMENSIONS = ['ccpp_constant_one:horizontal_dimension',
                              'ccpp_constant_one:horizontal_loop_extent',
                              'horizontal_loop_begin:horizontal_loop_end',
                              'horizontal_loop_extent']

###############################################################################
# Supported vertical dimensions (should be defined in CCPP_STANDARD_VARS)
CCPP_VERTICAL_DIMENSIONS = ['ccpp_constant_one:vertical_layer_dimension',
                            'ccpp_constant_one:vertical_interface_dimension',
                            'vertical_layer_index', 'vertical_interface_index']

###############################################################################
# Substituions for run time dimension control
CCPP_LOOP_DIM_SUBSTS = {'ccpp_constant_one:horizontal_dimension' :
                        'horizontal_loop_begin:horizontal_loop_end',
                        'ccpp_constant_one:vertical_layer_dimension' :
                        'vertical_layer_index',
                        'ccpp_constant_one:vertical_interface_dimension' :
                        'vertical_interface_index'}

########################################################################
def standard_name_to_long_name(prop_dict, context=None):
########################################################################
    """Translate a standard_name to its default long_name
    >>> standard_name_to_long_name({'standard_name':'cloud_optical_depth_layers_from_0p55mu_to_0p99mu'})
    'Cloud optical depth layers from 0.55mu to 0.99mu'
    >>> standard_name_to_long_name({'local_name':'foo'}) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No standard name to convert foo to long name
    >>> standard_name_to_long_name({}) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No standard name to convert to long name
    >>> standard_name_to_long_name({'local_name':'foo'}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No standard name to convert foo to long name at foo.F90:3
    >>> standard_name_to_long_name({}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No standard name to convert to long name at foo.F90:3
    """
    # We assume that standar_name has been checked for validity
    # Make the first char uppercase and replace each underscore with a space
    if 'standard_name' in prop_dict:
        standard_name = prop_dict['standard_name']
        if standard_name:
            long_name = standard_name[0].upper() + re.sub("_", " ",
                                                          standard_name[1:])
        else:
            long_name = ''
        # end if
        # Next, substitute a decimal point for the p in [:digit]p[:digit]
        match = _REAL_SUBST_RE.match(long_name)
        while match is not None:
            long_name = match.group(1) + '.' + match.group(2)
            match = _REAL_SUBST_RE.match(long_name)
        # end while
    else:
        long_name = ''
        if 'local_name' in prop_dict:
            lname = ' {}'.format(prop_dict['local_name'])
        else:
            lname = ''
        # end if
        ctxt = context_string(context)
        emsg = 'No standard name to convert{} to long name{}'
        raise CCPPError(emsg.format(lname, ctxt))
    # end if
    return long_name

########################################################################
def default_kind_val(prop_dict, context=None):
########################################################################
    """Choose a default kind based on a variable's type
    >>> default_kind_val({'type':'REAL'})
    'kind_phys'
    >>> default_kind_val({'type':'complex'})
    'kind_phys'
    >>> default_kind_val({'type':'double precision'})
    'kind_phys'
    >>> default_kind_val({'type':'integer'})
    ''
    >>> default_kind_val({'type':'character'})
    ''
    >>> default_kind_val({'type':'logical'})
    ''
    >>> default_kind_val({'local_name':'foo'}) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No type to find default kind for foo
    >>> default_kind_val({}) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No type to find default kind
    >>> default_kind_val({'local_name':'foo'}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No type to find default kind for foo at foo.F90:3
    >>> default_kind_val({}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No type to find default kind at foo.F90:3
    """
    if 'type' in prop_dict:
        vtype = prop_dict['type'].lower()
        if vtype == 'real':
            kind = 'kind_phys'
        elif vtype == 'complex':
            kind = 'kind_phys'
        elif FORTRAN_DP_RE.match(vtype) is not None:
            kind = 'kind_phys'
        else:
            kind = ''
        # end if
    else:
        kind = ''
        if 'local_name' in prop_dict:
            lname = ' {}'.format(prop_dict['local_name'])
            errmsg = 'No type to find default kind for {ln}{ct}'
        else:
            lname = ''
            errmsg = 'No type to find default kind{ct}'
        # end if
        ctxt = context_string(context)
        raise CCPPError(errmsg.format(ln=lname, ct=ctxt))
    # end if
    return kind

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
    >>> VariableProperty('standard_name', str).type == str
    True
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
    CCPPError: Invalid value variable property, '3'
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('()')
    []
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x)')
    ['x']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('x')

    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x:y)')
    ['x:y']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(w:x,y:z)')
    ['w:x', 'y:z']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('size(foo)')
    ['size(foo)']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(w:x,x:y:z:q)', error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'x:y:z:q' is an invalid dimension range
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x:3y)', error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: '3y' is not a valid Fortran identifier
    >>> VariableProperty('local_name', str, check_fn_in=check_local_name).valid_value('foo')
    'foo'
    >>> VariableProperty('local_name', str, check_fn_in=check_local_name).valid_value('foo(bar)')
    'foo(bar)'
    >>> VariableProperty('local_name', str, check_fn_in=check_local_name).valid_value('q(:,:,index_of_water_vapor_specific_humidity)')
    'q(:,:,index_of_water_vapor_specific_humidity)'
    """

    def __init__(self, name_in, type_in, valid_values_in=None, optional_in=False, default_in=None, default_fn_in=None, check_fn_in=None):
        """Conduct sanity checks and initialize this variable property."""
        self._name = name_in
        self._type = type_in
        if self._type not in [bool, int, list, str]:
            emsg = "{} has invalid VariableProperty type, '{}'"
            raise CCPPError(emsg.format(name_in, type_in))
        # end if
        self._valid_values = valid_values_in
        self._optional = optional_in
        if self.optional:
            if (default_in is None) and (default_fn_in is None):
                emsg = 'default_in or default_fn_in is a required property for {} because it is optional'
                raise CCPPError(emsg.format(name_in))
            if (default_in is not None) and (default_fn_in is not None):
                emsg = 'default_in and default_fn_in cannot both be provided'
                raise CCPPError(emsg)
            self._default = default_in
            self._default_fn = default_fn_in
        elif default_in is not None:
            emsg = 'default_in is not a valid property for {} because it is not optional'
            raise CCPPError(emsg.format(name_in))
        elif default_in is not None:
            emsg = 'default_fn_in is not a valid property for {} because it is not optional'
            raise CCPPError(emsg.format(name_in))
        self._check_fn = check_fn_in

    @property
    def name(self):
        """Return the name of the property"""
        return self._name

    @property
    def type(self):
        """Return the type of the property"""
        return self._type

    def get_default_val(self, prop_dict, context=None):
        """Return this variable property's default value or raise an
        exception if there is no default value or default value function."""
        if self._default_fn is not None:
            return self._default_fn(prop_dict, context)
        # end if
        if self._default is not None:
            return self._default
        # end if
        ctxt = context_string(context)
        emsg = 'No default for variable property {}{}'
        raise CCPPError(emsg.format(self.name, ctxt))


    @property
    def optional(self):
        """Return True iff this variable property is optional"""
        return self._optional

    def is_match(self, test_name):
        """Return True iff <test_name> is the name of this property"""
        return self.name.lower() == test_name.lower()

    def valid_value(self, test_value, prop_dict=None, error=False):
        """Return a valid version of <test_value> if it is valid.
        If <test_value> is not valid, return None or raise an exception,
        depending on the value of <error>.
        If <prop_dict> is not None, it may be used in value validation.
        """
        valid_val = None
        if self.type is int:
            try:
                tval = int(test_value)
                if self._valid_values is not None:
                    if tval in self._valid_values:
                        valid_val = tval
                    else:
                        valid_val = None # i.e. pass
                else:
                    valid_val = tval
            except CCPPError:
                valid_val = None # Redundant but more expressive than pass
        elif self.type is list:
            if isinstance(test_value, str):
                match = _LIST_RE.match(test_value)
                if match is None:
                    tval = None
                else:
                    tval = [x.strip() for x in match.group(1).split(',')]
                    if (len(tval) == 1) and (not tval[0]):
                        # Scalar
                        tval = list()
                    # end if
                # end if
            else:
                tval = test_value
            # end if
            if isinstance(tval, list):
                valid_val = tval
            elif isinstance(tval, tuple):
                valid_val = list(tval)
            else:
                valid_val = None
            # end if
            if (valid_val is not None) and (self._valid_values is not None):
                # Special case for lists, _valid_values applies to elements
                for item in valid_val:
                    if item not in self._valid_values:
                        valid_val = None
                        break
                    # end if
                # end for
            else:
                pass
        elif self.type is bool:
            if isinstance(test_value, str):
                valid_val = ((test_value in ['True', 'False']) or
                             (test_value.lower() in
                              ['t', 'f', '.true.', '.false.']))
            else:
                valid_val = not not test_value # pylint: disable=unneeded-not
        elif self.type is str:
            if isinstance(test_value, str):
                if self._valid_values is not None:
                    if test_value in self._valid_values:
                        valid_val = test_value
                    else:
                        valid_val = None # i.e., pass
                else:
                    valid_val = test_value
                # end if
            # end if
        # end if
        # Call a check function?
        if valid_val and (self._check_fn is not None):
            valid_val = self._check_fn(valid_val, prop_dict, error)
        elif (valid_val is None) and error:
            emsg = "Invalid {} variable property, '{}'"
            raise CCPPError(emsg.format(self.name, test_value))
        # end if
        return valid_val

###############################################################################

class Var(object):
    """ A class to hold a metadata or code variable.
    Var objects should be treated as immutable.
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
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'ttype' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext())) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid metadata variable property, 'ttype', in <standard input>
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext())) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Required property, 'units', missing, in <standard input>
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'inout', 'protected' : '.true.'}, ParseSource('vname', 'SCHEME', ParseContext())) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: foo is marked protected but is intent inout, at <standard input>:1
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'ino'}, ParseSource('vname', 'SCHEME', ParseContext())) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid intent variable property, 'ino', at <standard input>:1
    """

    # __spec_props are for variables defined in a specification
    __spec_props = [VariableProperty('local_name', str,
                                     check_fn_in=check_local_name),
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
                                     optional_in=True,
                                     default_fn_in=default_kind_val),
                    VariableProperty('state_variable', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('optional', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('protected', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('allocatable', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('persistence', str, optional_in=True,
                                     valid_values_in=['timestep', 'run'],
                                     default_in='timestep')]

    # __var_props contains properties which are not in __spec_props
    __var_props = [VariableProperty('intent', str,
                                    valid_values_in=['in', 'out', 'inout'])]

    # __no_metadata_props__ contains properties to omit from metadata
    __no_metadata_props__ = ['local_name']

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
        # end if
    # end for
    for p in __var_props:
        __var_propdict[p.name] = p
        if not p.optional:
            __required_var_props.append(p.name)
        # end if
    # end for

    def __init__(self, prop_dict, source, context=None,
                 invalid_ok=False, logger=None):
        """NB: invalid_ok=True is dangerous because it allows creation
        of a Var object with invalid properties.
        In order to prevent silent failures, invalid_ok requires a logger
        in order to take effect.
        if <prop_dict> is really a Var object, use that object's prop_dict."""
        self._parent_var = None # for array references
        if isinstance(prop_dict, Var):
            prop_dict = prop_dict.copy_prop_dict()
        # end if
        if source.type == 'scheme':
            required_props = Var.__required_var_props
            mstr_propdict = Var.__var_propdict
        else:
            required_props = Var.__required_spec_props
            mstr_propdict = Var.__spec_propdict
        # end if
        self._source = source
        # Grab a frozen copy of the context
        if context is None:
            self._context = ParseContext(context=source.context)
        else:
            self._context = context
        # end if
        # First, check the input
        if 'ddt_type' in prop_dict:
            # Special case to bypass normal type rules
            if 'type' not in prop_dict:
                prop_dict['type'] = prop_dict['ddt_type']
            # end if
            if 'units' not in prop_dict:
                prop_dict['units'] = ""
            # end if
            prop_dict['kind'] = prop_dict['ddt_type']
            del prop_dict['ddt_type']
            self._intrinsic = False
        else:
            self._intrinsic = True
        # end if
        for key in prop_dict:
            if Var.get_prop(key) is None:
                raise ParseSyntaxError("Invalid metadata variable property, '{}'".format(key), context=self.context)
            # end if
        # end for
        # Make sure required properties are present
        for propname in required_props:
            if propname not in prop_dict:
                if invalid_ok and (logger is not None):
                    ctx = context_string(self.context)
                    logger.warning("Required property, '{}', missing{}".format(propname, ctx))
                else:
                    raise ParseSyntaxError("Required property, '{}', missing".format(propname), context=self.context)
                # end if
            # end if
        # end for
        # Check for any mismatch
        if ('protected' in prop_dict) and ('intent' in prop_dict):
            if (prop_dict['intent'].lower() != 'in') and prop_dict['protected']:
                if invalid_ok and (logger is not None):
                    ctx = context_string(self.context)
                    wmsg = "{} is marked protected but is intent {}{}"
                    logger.warning(wmsg.format(prop_dict['local_name'],
                                               prop_dict['intent'], ctx))
                else:
                    emsg = "{} is marked protected but is intent {}"
                    raise ParseSyntaxError(emsg.format(prop_dict['local_name'],
                                                       prop_dict['intent']),
                                           context=self.context)
                # end if
            # end if
        # end if
        # Steal dict from caller
        self._prop_dict = prop_dict
        # Fill in default values for missing properties
        for propname in mstr_propdict:
            if (propname not in prop_dict) and mstr_propdict[propname].optional:
                mval = mstr_propdict[propname]
                def_val = mval.get_default_val(self._prop_dict,
                                               context=self.context)
                self._prop_dict[propname] = def_val
            # end if
        # end for
        # Make sure all the variable values are valid
        try:
            for prop_name in self._prop_dict.keys():
                prop = Var.get_prop(prop_name)
                _ = prop.valid_value(self._prop_dict[prop_name],
                                     prop_dict=self._prop_dict, error=True)
            # end for
        except CCPPError as cperr:
            if invalid_ok and (logger is not None):
                ctx = context_string(self.context)
                wmsg = "{}: {}{}"
                logger.warning(wmsg.format(self._prop_dict['local_name'],
                                           cperr, ctx))
            else:
                emsg = "{}: {}"
                lname = self._prop_dict['local_name']
                raise ParseSyntaxError(emsg.format(lname, cperr),
                                       context=self.context)
            # end if
        # end try

    def compatible(self, other, logger=None):
        """Return True, None iff <other> is compatible with self.
        If not compatible, return False,<reason> where <reason> is
        a string describing the incompatibility.
        """
        # We accept character(len=*) as compatible with
        # character(len=INTEGER_VALUE)
        compat = False
        reason = None
        stype = self.get_prop_value('type')
        skind = self.get_prop_value('kind')
        sunits = self.get_prop_value('units')
        sstd_name = self.get_prop_value('standard_name')
        otype = other.get_prop_value('type')
        okind = other.get_prop_value('kind')
        ounits = other.get_prop_value('units')
        ostd_name = other.get_prop_value('standard_name')
        if stype == 'character':
            kind_eq = ((skind == okind) or
                       (skind == 'len=*' and okind.startswith('len=')) or
                       (skind.startswith('len=') and okind == 'len=*'))
        else:
            kind_eq = skind == okind
        # end if
        if ((sstd_name == ostd_name) and kind_eq and
            (sunits == ounits) and (stype == otype)):
            compat = True
        else:
            logger_str = None
            error_str = None
            if sstd_name != ostd_name:
                logger_str = "standard_name: '{}' != '{}'".format(sstd_name,
                                                                  ostd_name)
                reason = 'standard_name'
            elif not kind_eq:
                logger_str = "kind: '{}' != '{}'".format(skind, okind)
                reason = 'kind'
            elif sunits != ounits:
                logger_str = "units: '{}' != '{}'".format(sunits, ounits)
                reason = 'units'
            elif stype != otype:
                logger_str = "type: '{}' != '{}'".format(stype, otype)
                reason = 'type'
            else:
                error_str = 'Why are these variables not compatible?'
                reason = 'UNKNOWN'
            # end if
            if logger is not None:
                if error_str is not None:
                    logger.error('{}'.format(error_str))
                elif logger_str is not None:
                    logger.info('{}'.format(logger_str))
                # end if (no else)
            # end if
        # end if
        return compat, reason

    def adjust_intent(self, intent, src_var=None):
        """Add an intent to this Var or adjust its existing intent.
        Note: An existing intent can only be adjusted to 'inout'
        """
        if 'intent' in self._prop_dict:
            my_intent = self.get_prop_value('intent')
        else:
            my_intent = None
        # end if
        if not my_intent:
            self._prop_dict['intent'] = intent
        elif not intent:
            self._prop_dict['intent'] = 'in'
        elif intent == 'inout':
            self._prop_dict['intent'] = intent
        elif intent != my_intent:
            lname = self.get_prop_value('local_name')
            lctx = context_string(self.context)
            emsg = "Attempt to set intent of {}{} to {}, only 'inout' allowed."
            if src_var:
                slname = src_var.get_prop_value('local_name')
                sctx = context_string(src_var.context)
                emsg += "\nintent source: {}{}".format(slname, sctx)
            # end if
            raise ParseInternalError(emsg.format(lname, lctx, intent))
        # no else, intent does not need to change
        # end if

    @classmethod
    def get_prop(cls, name, spec_type=None):
        """Return VariableProperty object for <name> or None"""
        prop = None
        if (spec_type is None) and (name in Var.__var_propdict):
            prop = Var.__var_propdict[name]
        elif (spec_type is not None) and (name in Var.__spec_propdict):
            prop = Var.__spec_propdict[name]
        # end if (else prop = None)
        return prop

    @classmethod
    def is_horizontal_dimension(cls, dim_name):
        """Return True if it is a recognized horizontal
        dimension or index, otherwise, return False
        >>> Var.is_horizontal_dimension('horizontal_loop_extent')
        True
        >>> Var.is_horizontal_dimension('ccpp_constant_one:horizontal_loop_extent')
        True
        >>> Var.is_horizontal_dimension('ccpp_constant_one:horizontal_dimension')
        True
        >>> Var.is_horizontal_dimension('horizontal_loop_begin:horizontal_loop_end')
        True
        >>> Var.is_horizontal_dimension('horizontal_loop_begin:horizontal_loop_extent')
        False
        >>> Var.is_horizontal_dimension('ccpp_constant_one')
        False
        """
        return dim_name in CCPP_HORIZONTAL_DIMENSIONS

    @classmethod
    def is_vertical_dimension(cls, dim_name):
        """Return True if it is a recognized vertical
        dimension or index, otherwise, return False
        >>> Var.is_vertical_dimension('ccpp_constant_one:vertical_layer_dimension')
        True
        >>> Var.is_vertical_dimension('ccpp_constant_one:vertical_interface_dimension')
        True
        >>> Var.is_vertical_dimension('vertical_layer_index')
        True
        >>> Var.is_vertical_dimension('vertical_interface_index')
        True
        >>> Var.is_vertical_dimension('ccpp_constant_one:vertical_layer_index')
        False
        >>> Var.is_vertical_dimension('ccpp_constant_one:vertical_interface_index')
        False
        >>> Var.is_vertical_dimension('horizontal_loop_extent')
        False
        """
        return dim_name in CCPP_VERTICAL_DIMENSIONS

    @classmethod
    def find_horizontal_dimension(cls, dims):
        """Return the horizontal dimension string and location in <dims>
        or (None, -1).
        Return form is (horizontal_dimension, index) where index is the
        location of horizontal_dimension in <dims>"""
        var_hdim = None
        hindex = -1
        for index, dimname in enumerate(dims):
            if Var.is_horizontal_dimension(dimname):
                var_hdim = dimname
                hindex = index
                break
            # end if
        # end for
        return (var_hdim, hindex)

    @classmethod
    def find_vertical_dimension(cls, dims):
        """Return the vertical dimension string and location in <dims>
        or (None, -1).
        Return form is (vertical_dimension, index) where index is the
        location of vertical_dimension in <dims>"""
        var_vdim = None
        vindex = -1
        for index, dimname in enumerate(dims):
            if Var.is_vertical_dimension(dimname):
                var_vdim = dimname
                vindex = index
                break
            # end if
        # end for
        return (var_vdim, vindex)

    def copy_prop_dict(self, subst_dict=None):
        """Create a copy of our prop_dict, possibly substituting properties
        from <subst_dict>."""
        cprop_dict = {}
        # Start with a straight copy of this variable's prop_dict
        for prop in self._prop_dict.keys():
            cprop_dict[prop] = self._prop_dict[prop]
        # end for
        # Now add or substitute properties from <subst_dict>
        if subst_dict:
            for prop in subst_dict.keys():
                cprop_dict[prop] = subst_dict[prop]
            # end for
        # end if
        # Special key for creating a copy of a DDT (see Var.__init__)
        if self.is_ddt():
            cprop_dict['ddt_type'] = cprop_dict['type']
        # end if
        return cprop_dict

    def clone(self, subst_dict, source_name=None, source_type=None,
              context=None):
        """Create a clone of this Var object with properties from <subst_dict>
        overriding this variable's properties. <subst_dict> may also be
        a string in which case only the local_name property is changed
        (to the value of the <subst_dict> string).
        The optional <source_name>, <source_type>, and <context> inputs
        allow the clone to appear to be coming from a designated source,
        by default, the source and type are the same as this Var (self).
        """
        if isinstance(subst_dict, str):
            subst_dict = {'local_name':subst_dict}
        # end if
        cprop_dict = self.copy_prop_dict(subst_dict=subst_dict)
        if source_name is None:
            source_name = self.source.name
        # end if
        if source_type is None:
            source_type = self.source.type
        # end if
        if context is None:
            context = self._context
        # end if
        psource = ParseSource(source_name, source_type, context)
        return Var(cprop_dict, psource)

    def get_prop_value(self, name):
        """Return the value of key, <name> if <name> is in this variable's
        property dictionary, otherwise, return None
        """
        if name in self._prop_dict:
            pvalue = self._prop_dict[name]
        else:
            pvalue = None
        # end if
        return pvalue

    def handle_array_ref(self):
        """If this Var's local_name is an array ref, add in the array
        reference indices to the Var's dimensions.
        Return the (stripped) local_name and the full dimensions.
        >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext())).handle_array_ref()
        ('foo', [])
        >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '(ccpp_constant_one:dim1)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext())).handle_array_ref()
        ('foo', ['ccpp_constant_one:dim1'])
        >>> Var({'local_name' : 'foo(:,:,bar)', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '(ccpp_constant_one:dim1,ccpp_constant_one:dim2)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext())).handle_array_ref()
        ('foo', ['ccpp_constant_one:dim1', 'ccpp_constant_one:dim2', 'bar'])
        >>> Var({'local_name' : 'foo(bar,:)', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '(ccpp_constant_one:dim1)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext())).handle_array_ref()
        ('foo', ['bar', 'ccpp_constant_one:dim1'])
        >>> Var({'local_name' : 'foo(bar)', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '(ccpp_constant_one:dim1)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext())).handle_array_ref() #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: Call dims mismatch for foo(bar), not enough colons
        >>> Var({'local_name' : 'foo(:,bar,:)', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '(ccpp_constant_one:dim1)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext())).handle_array_ref() #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: Call dims mismatch for foo(:,bar,:), not enough dims
        >>> Var({'local_name' : 'foo(:,:,bar)', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '(ccpp_constant_one:dim1)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext())).handle_array_ref() #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: Call dims mismatch for foo(:,:,bar), not enough dims
        >>> Var({'local_name' : 'foo(:,bar)', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '(ccpp_constant_one:dim1,ccpp_constant_one:dim2)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext())).handle_array_ref() #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: Call dims mismatch for foo(:,bar), too many dims
        """
        dimlist = self.get_dimensions()
        aref = self.array_ref()
        if aref is not None:
            lname = aref.group(1)
            # Substitute dimensions for colons in array reference
            sdimlist = dimlist
            num_dims = len(sdimlist)
            dimlist = [x.strip() for x in aref.group(2).split(',')]
            num_colons = sum(dim == ':' for dim in dimlist)
            cind = 0
            if num_dims > num_colons:
                emsg = 'Call dims mismatch for {}, not enough colons'
                lname = self.get_prop_value('local_name')
                raise CCPPError(emsg.format(lname))
            # end if
            for dind, dim in enumerate(dimlist):
                if dim == ':':
                    if cind >= num_dims:
                        emsg = 'Call dims mismatch for {}, not enough dims'
                        lname = self.get_prop_value('local_name')
                        raise CCPPError(emsg.format(lname))
                    # end if
                    dimlist[dind] = sdimlist[cind]
                    cind += 1
                # end if
            # end for
            if cind < num_colons:
                emsg = 'Call dims mismatch for {}, too many dims'
                lname = self.get_prop_value('local_name')
                raise CCPPError(emsg.format(lname))
            # end if
        else:
            lname = self.get_prop_value('local_name')
        # end if
        return lname, dimlist

    def call_dimstring(self, var_dict=None, explicit_dims=False):
        """Return the dimensions string for a variable call.
        If <var_dict> is present, find and substitute a local_name for
        each standard_name in this variable's dimensions.
        If <var_dict> is not present, return a colon for each dimension.
        """
        emsg = ''
        _, dims = self.handle_array_ref()
        if var_dict is not None:
            dimlist = []
            sepstr = ''
            for dim in dims:
                dstdnames = dim.split(':')
                # Decide whether to list all dimensions or to replace
                # a range with a colon.
                add_dims = explicit_dims or (len(dstdnames) == 1)
                dnames = []
                for stdname in dstdnames:
                    if add_dims:
                        dvar = var_dict.find_variable(stdname)
                        if dvar:
                            dnames.append(dvar.get_prop_value('local_name'))
                        else:
                            emsg += sepstr + "No variable found for dimension '"
                            emsg += stdname + "' in {vlnam}"
                            sepstr = '\n'
                        # end if
                    else:
                        dnames.append('')
                    # end if
                # end for
                dimlist.append(':'.join(dnames))
            # end for
        else:
            dimlist = [':']*len(dims)
        # end if
        if dimlist:
            dimstr = '(' + ','.join(dimlist) + ')'
        else:
            dimstr = '' # It ends up being a scalar reference
        # end if
        if emsg:
            ctx = context_string(self.context)
            emsg += "{ctx}"
            lname = self.get_prop_value('local_name')
            raise CCPPError(emsg.format(vlnam=lname, ctx=ctx))
        # end if
        return dimstr

    def call_string(self, var_dict, loop_vars=None):
        """Construct the actual argument string for this Var by translating
        standard names to local names.
        String includes array bounds unless loop_vars is None.
        if <loop_vars> is not None, look there first for array bounds,
        even if usage requires a loop substitution.
        """
        if loop_vars is None:
            call_str = self.get_prop_value('local_name')
            # Look for dims in case this is an array selection variable
            dind = call_str.find('(')
            if dind > 0:
                dimstr = call_str[dind+1:].rstrip()[:-1]
                dims = [x.strip() for x in dimstr.split(',')]
                call_str = call_str[:dind].strip()
            else:
                dims = None
            # end if
        else:
            call_str, dims = self.handle_array_ref()
        # end if
        if dims:
            call_str = call_str + '('
            dsep = ''
            for dim in dims:
                if loop_vars:
                    lname = loop_vars.find_loop_dim_match(dim)
                else:
                    lname = None
                # end if
                if lname is None:
                    isep = ''
                    lname = ""
                    for item in dim.split(':'):
                        if item:
                            dvar = var_dict.find_variable(item, any_scope=False)
                            if dvar is None:
                                iname = None
                            else:
                                iname = dvar.get_prop_value('local_name')
                            # end if
                        else:
                            iname = ''
                        # end if
                        if iname is not None:
                            lname = lname + isep + iname
                            isep = ':'
                        else:
                            errmsg = 'No local variable {} in {}{}'
                            ctx = context_string(self.context)
                            dname = var_dict.name
                            raise CCPPError(errmsg.format(item, dname, ctx))
                        # end if
                    # end for
                # end if
                if lname is not None:
                    call_str = call_str + dsep + lname
                    dsep = ', '
                else:
                    errmsg = 'Unable to convert {} to local variables in {}{}'
                    ctx = context_string(self.context)
                    raise CCPPError(errmsg.format(dim, var_dict.name, ctx))
                # end if
            # end for
            call_str = call_str + ')'
        # end if
        return call_str

    def valid_value(self, prop_name, test_value=None, error=False):
        """Return a valid version of <test_value> if it is a valid value
        for the property, <prop_name>.
        If <test_value> is not valid, return None or raise an exception,
        depending on the value of <error>.
        If <test_value> is None, use the current value of <prop_name>.
        """
        vprop = Var.get_prop(prop_name)
        if vprop is not None:
            if test_value is None:
                test_val = self.get_prop_value(prop_name)
            # end if
            valid = vprop.valid_value(test_val,
                                      prop_dict=self._prop_dict, error=error)
        else:
            valid = None
            errmsg = 'Invalid variable property, {}'
            raise ParseInternalError(errmsg.format(prop_name))
        # end if
        return valid

    def array_ref(self):
        """If this Var's local_name is an array reference, return a
        Fortran array reference regexp match.
        Otherwise, return None"""
        match = FORTRAN_SCALAR_REF_RE.match(self.get_prop_value('local_name'))
        return match

    @property
    def parent(self):
        """Return this variable's parent variable (or None)"""
        return self._parent_var

    @parent.setter
    def parent(self, parent_var):
        """Set this variable's parent if not already set"""
        if self._parent_var is not None:
            emsg = 'Attempting to set parent for {} but parent already set'
            lname = self.get_prop_value('local_name')
            raise ParseInternalError(emsg.format(lname))
        # end if
        if isinstance(parent_var, Var):
            self._parent_var = parent_var
        else:
            emsg = 'Attempting to set parent for {}, bad parent type, {}'
            lname = self.get_prop_value('local_name')
            raise ParseInternalError(emsg.format(lname, type(parent_var)))
        # end if

    @property
    def context(self):
        """Return this variable's parsed context"""
        return self._context

    @property
    def source(self):
        """Return the source object for this variable"""
        return self._source

    @source.setter
    def source(self, new_source):
        """Reset this Var's source if <new_source> seems legit"""
        if isinstance(new_source, ParseSource):
            self._source = new_source
        else:
            errmsg = 'Attemping to set source of {} ({}) to "{}"'
            stdname = self.get_prop_value('standard_name')
            lname = self.get_prop_value('local_name')
            raise ParseInternalError(errmsg.format(stdname, lname, new_source))
        # end if

    @property
    def host_interface_var(self):
        """True iff self is included in the host model interface calls"""
        return self.source.type == 'host'

    def get_dimensions(self):
        """Return a list with the variable's dimension strings"""
        dims = self.valid_value('dimensions')
        return dims

    def get_dim_stdnames(self, include_constants=True):
        """Return a set of all the dimension standard names for this Var"""
        dimset = set()
        for dim in self.get_dimensions():
            for name in dim.split(':'):
                # Weed out the integers
                try:
                    _ = int(name)
                except ValueError:
                    # Not an integer, maybe add it
                    if include_constants or (not name in CCPP_CONSTANT_VARS):
                        dimset.add(name)
                    # end if
                # end try
            # end for
        # end for
        return dimset

    def get_rank(self):
        """Return the variable's rank (zero for scalar)"""
        dims = self.get_dimensions()
        return len(dims)

    def has_horizontal_dimension(self, dims=None):
        """Return horizontal dimension standard name string for
        <self> or <dims> (if present) if a horizontal dimension is
        present in the list"""
        if dims is None:
            vdims = self.get_dimensions()
        else:
            vdims = dims
        # end if
        return Var.find_horizontal_dimension(vdims)[0]

    def has_vertical_dimension(self, dims=None):
        """Return vertical dimension standard name string for
        <self> or <dims> (if present) if a vertical dimension is
        present in the list"""
        if dims is None:
            vdims = self.get_dimensions()
        else:
            vdims = dims
        # end if
        return Var.find_vertical_dimension(vdims)[0]

    def write_def(self, outfile, indent, wdict, allocatable=False, dummy=False):
        """Write the definition line for the variable."""
        stdname = self.get_prop_value('standard_name')
        if stdname in CCPP_CONSTANT_VARS:
            # There is no declaration line for a constant
            return
        # end if
        if self.is_ddt():
            vtype = 'type'
        else:
            vtype = self.get_prop_value('type')
        # end if
        kind = self.get_prop_value('kind')
        name = self.get_prop_value('local_name')
        aref = self.array_ref()
        if aref is not None:
            name = aref.group(1)
        # end if
        dims = self.get_dimensions()
        if dims:
            if allocatable or dummy:
                dimstr = '(:' + ',:'*(len(dims) - 1) + ')'
            else:
                dimstr = self.call_dimstring(var_dict=wdict)
        else:
            dimstr = ''
        # end if
        protected = self.get_prop_value('protected')
        intent = self.get_prop_value('intent')
        if protected and allocatable:
            errmsg = 'Cannot create allocatable variable from protected, {}'
            raise CCPPError(errmsg.format(name))
        # end if
        if protected:
            intent_str = 'intent(in)   '
        elif allocatable:
            if dimstr:
                intent_str = 'allocatable  '
            else:
                intent_str = ' '*13
            # end if
        elif intent is not None:
            alloval = self.get_prop_value('allocatable')
            if (intent.lower()[-3:] == 'out') and alloval:
                intent_str = 'allocatable, intent({})'.format(intent)
            else:
                intent_str = 'intent({}){}'.format(intent,
                                                   ' '*(5 - len(intent)))
            # end if
        else:
            intent_str = ' '*13
        # end if
        if intent_str.strip():
            comma = ','
        else:
            comma = ' '
        # end if
        if self.is_ddt():
            dstr = "type({kind}){cspc}{intent} :: {name}{dims} ! {sname}"
            cspc = comma + ' '*(13 - len(kind))
        else:
            if kind:
                dstr = "{type}({kind}){cspc}{intent} :: {name}{dims} ! {sname}"
                cspc = comma + ' '*(17 - len(vtype) - len(kind))
            else:
                dstr = "{type}{cspc}{intent} :: {name}{dims} ! {sname}"
                cspc = comma + ' '*(19 - len(vtype))
            # end if
        # end if
        outfile.write(dstr.format(type=vtype, kind=kind, intent=intent_str,
                                  name=name, dims=dimstr, cspc=cspc,
                                  sname=stdname), indent)

    def is_ddt(self):
        """Return True iff <self> is a DDT type."""
        return not self._intrinsic

    def __str__(self):
        """Print representation or string for Var objects"""
        return "<Var {standard_name}: {local_name}>".format(**self._prop_dict)

    def __repr__(self):
        """Object representation for Var objects"""
        base = super(Var, self).__repr__()
        pind = base.find(' object ')
        if pind >= 0:
            pre = base[0:pind]
        else:
            pre = '<Var'
        # end if
        bind = base.find('at 0x')
        if bind >= 0:
            post = base[bind:]
        else:
            post = '>'
        # end if
        return '{} {}: {} {}'.format(pre, self._prop_dict['standard_name'],
                                     self._prop_dict['local_name'], post)

###############################################################################

class VarSpec(object):
    """A class to hold a standard_name description of a variable.
    A scalar variable is just a standard name while an array also
    contains a comma-separated list of dimension standard names in parentheses.
    """

    def __init__(self, var):
        """Initialize the common properties of this VarSpec-based object"""
        self._name = var.get_prop_value('standard_name')
        self._dims = var.get_dimensions()
        if not self._dims:
            self._dims = None
        # end if

    @property
    def name(self):
        """Return the name of this VarSpec-based object"""
        return self._name

    def get_dimensions(self):
        """Return the dimensions of this VarSpec-based object."""
        rdims = self._dims
        return rdims

    def __repr__(self):
        """Return a representation of this object"""
        if self._dims is not None:
            repr_str = "{}({})".format(self._name, ', '.join(self._dims))
        else:
            repr_str = self._name
        # end if
        return repr_str

###############################################################################

__CCPP_PARSE_CONTEXT = ParseContext(filename='metavar.py')

###############################################################################

def ccpp_standard_var(std_name, source_type, context=None, intent='out'):
    """If <std_name> is a CCPP standard variable name, return a variable
    with that name.
    Otherwise return None.
    """
    if std_name in CCPP_STANDARD_VARS:
        # Copy the dictionary because Var can change it
        vdict = dict(CCPP_STANDARD_VARS[std_name])
        if context is None:
            psource = ParseSource('ccpp_standard_vars', source_type,
                                  __CCPP_PARSE_CONTEXT)
        else:
            psource = ParseSource('ccpp_standard_vars', source_type, context)
        # end if
        if source_type.lower() == 'scheme':
            vdict['intent'] = intent
        # end if
        newvar = Var(vdict, psource)
    else:
        newvar = None
    # end if
    return newvar

###############################################################################

class VarAction(object):
    """A base class for variable actions such as loop substitutions or
    temporary variable handling."""

    def __init__(self):
        """Initialize this action (nothing to do)"""
        # pass # Nothing general here yet

    def add_local(self, vadict, source):
        """Add any variables needed by this action to <dict>.
        Variable(s) will appear to originate from <source>."""
        raise ParseInternalError('VarAction add_local method must be overriden')

    def write_action(self, vadict, dict2=None, any_scope=False):
        """Return a string setting implementing the action of <self>.
        Variables must be in <dict> or <dict2>"""
        errmsg = 'VarAction write_action method must be overriden'
        raise ParseInternalError(errmsg)

    def equiv(self, vmatch):
        """Return True iff <vmatch> is equivalent to <self>.
        Equivalence at this level is tested by comparing the type
        of the objects.
        equiv should be overridden with a method that first calls this
        method and then tests class-specific object data."""
        return vmatch.__class__ == self.__class__

    def add_to_list(self, vlist):
        """Add <self> to <vlist> unless <self> or its equivalent is
        already in <vlist>. This method should not need to be overriden.
        Return the (possibly modified) list"""
        ok_to_add = True
        for vlist_action in vlist:
            if vlist_action.equiv(self):
                ok_to_add = False
                break
            # end if
        # end for
        if ok_to_add:
            vlist.append(self)
        # end if
        return vlist

###############################################################################

class VarLoopSubst(VarAction):
    """A class to handle required loop substitutions where the host model
    (or a suite part) does not provide a loop-like variable used by a
    suite part or scheme or where a host model passes a subset of a
    dimension at run time."""

    def __init__(self, missing_stdname, required_stdnames,
                 local_name, set_action):
        """Initialize this variable loop substitution"""
        self._missing_stdname = missing_stdname
        self._local_name = local_name
        if isinstance(required_stdnames, Var):
            self._required_stdnames = (required_stdnames,)
        else:
            # Make sure required_stdnames is iterable
            try:
                _ = (v for v in required_stdnames)
                self._required_stdnames = required_stdnames
            except TypeError:
                emsg = "required_stdnames must be a tuple or a list"
                raise ParseInternalError(emsg)
            # end try
        # end if
        self._set_action = set_action
        super(VarLoopSubst, self).__init__()

    def has_subst(self, vadict, any_scope=False):
        """Determine if variables for the required standard names of this
        VarLoopSubst object are present in <vadict> (or in the parents of
        <vadict>) if <any_scope> is True.
        Return a list of the required variables on success, None on failure.
        """
        # A template for 'missing' should be in the standard variable list
        subst_list = list()
        for name in self.required_stdnames:
            svar = vadict.find_variable(name, any_scope=any_scope)
            if svar is None:
                subst_list = None
                break
            else:
                subst_list.append(svar)
            # end if
        # end for
        return subst_list

    def add_local(self, vadict, source):
        """Add a Var created from the missing name to <vadict>"""
        if self.missing_stdname not in vadict:
            lname = self._local_name
            local_name = vadict.new_internal_variable_name(prefix=lname)
            prop_dict = {'standard_name':self.missing_stdname,
                         'local_name':local_name,
                         'type':'integer', 'units':'count', 'dimensions':'()'}
            var = Var(prop_dict, source)
            vadict.add_variable(var, exists_ok=True, gen_unique=True)
        # end if

    def equiv(self, vmatch):
        """Return True iff <vmatch> is equivalent to <self>.
        Equivalence is determined by matching the missing standard name
        and the required standard names"""
        is_equiv = super(VarLoopSubst, self).equiv(vmatch)
        if is_equiv:
            is_equiv = vmatch.missing_stdname == self.missing_stdname
        # end if
        if is_equiv:
            for dim1, dim2 in zip(vmatch.required_stdnames,
                                  self.required_stdnames):
                if dim1 != dim2:
                    is_equiv = False
                    break
                # end if
            # end for
        # end if
        return is_equiv

    def write_action(self, vadict, dict2=None, any_scope=False):
        """Return a string setting the correct values for our
        replacement variable. Variables must be in <vadict> or <dict2>"""
        action_dict = {}
        if self._set_action:
            for stdname in self.required_stdnames:
                var = vadict.find_variable(stdname, any_scope=any_scope)
                if (var is None) and (dict2 is not None):
                    var = dict2.find_variable(stdname, any_scope=any_scope)
                # end if
                if var is None:
                    errmsg = "Required variable, {}, not found"
                    raise CCPPError(errmsg.format(stdname))
                # end if
                action_dict[stdname] = var.get_prop_value('local_name')
            # end for
            var = vadict.find_variable(self.missing_stdname)
            if var is None:
                errmsg = "Required variable, {}, not found"
                raise CCPPError(errmsg.format(self.missing_stdname))
            # end if
            action_dict[self.missing_stdname] = var.get_prop_value('local_name')
        # end if
        return self._set_action.format(**action_dict)

    def write_metadata(self, mfile):
        """Write our properties as metadata to <mfile>"""
        pass # Currently no properties to write

    @property
    def required_stdnames(self):
        """Return the _required_stdnames for this object"""
        return self._required_stdnames

    @property
    def missing_stdname(self):
        """Return the _missing_stdname for this object"""
        return self._missing_stdname

    def __repr__(self):
        """Return string representing this VarLoopSubst object"""
        action_dict = {}
        repr_str = ''
        if self._set_action:
            for stdname in self.required_stdnames:
                action_dict[stdname] = stdname
            # end for
            action_dict[self.missing_stdname] = self.missing_stdname
            repr_str = self._set_action.format(**action_dict)
        else:
            repr_str = "{} => {}".format(self.missing_stdname,
                                         ':'.join(self.required_stdnames))
        # end if
        return repr_str

    def __str__(self):
        """Return print string for this VarLoopSubst object"""
        return "<{}>".format(self.__repr__())

# Substitutions where a new variable must be created
CCPP_VAR_LOOP_SUBSTS = {
    'horizontal_loop_extent' :
    VarLoopSubst('horizontal_loop_extent',
                 ('horizontal_loop_begin', 'horizontal_loop_end'), 'ncol',
                 '{} = {} - {} + 1'.format('{horizontal_loop_extent}',
                                           '{horizontal_loop_end}',
                                           '{horizontal_loop_begin}')),
    'horizontal_loop_begin' :
    VarLoopSubst('horizontal_loop_begin',
                 ('ccpp_constant_one',), 'one', '{horizontal_loop_begin} = 1'),
    'horizontal_loop_end' :
    VarLoopSubst('horizontal_loop_end',
                 ('horizontal_loop_extent',), 'ncol',
                 '{} = {}'.format('{horizontal_loop_end}',
                                  '{horizontal_loop_extent}')),
    'vertical_layer_dimension' :
    VarLoopSubst('vertical_layer_dimension',
                 ('vertical_layer_index',), 'layer_index', ''),
    'vertical_interface_dimension' :
    VarLoopSubst('vertical_interface_dimension',
                 ('vertical_interface_index',), 'level_index', '')
}

###############################################################################

class VarDictionary(OrderedDict):
    """
    A class to store and cross-check variables from one or more metadata
    headers. The class also serves as a scoping construct so that a variable
    can be found in an innermost available scope.
    The dictionary is organized by standard_name. It is an error to try
    to add a variable if its standard name is already in the dictionary.
    Scoping is a tree of VarDictionary objects.
    >>> VarDictionary('foo')
    VarDictionary(foo)
    >>> VarDictionary('bar', variables={})
    VarDictionary(bar)
    >>> VarDictionary('baz', Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))) #doctest: +ELLIPSIS
    VarDictionary(baz, [('hi_mom', <__main__.Var hi_mom: foo at 0x...>)])
    >>> print("{}".format(VarDictionary('baz', Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext())))))
    VarDictionary(baz, ['hi_mom'])
    >>> VarDictionary('qux', [Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))]) #doctest: +ELLIPSIS
    VarDictionary(qux, [('hi_mom', <__main__.Var hi_mom: foo at 0x...>)])
    >>> VarDictionary('boo').add_variable(Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext())))

    >>> VarDictionary('who', variables=[Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))]).prop_list('local_name')
    ['foo']
    >>> VarDictionary('who', variables=[Var({'local_name' : 'who_var1', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext())),Var({'local_name' : 'who_var', 'standard_name' : 'bye_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))]).new_internal_variable_name()
    'who_var2'
    >>> VarDictionary('who', variables=[Var({'local_name' : 'who_var1', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))]).new_internal_variable_name(prefix='bar')
    'bar'
    >>> VarDictionary('glitch', Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))).add_variable(Var({'local_name' : 'bar', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname2', 'DDT', ParseContext()))) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid Duplicate standard name, 'hi_mom', at <standard input>:
    """

    def __init__(self, name, variables=None, parent_dict=None, logger=None):
        """Unlike dict, VarDictionary only takes a Var or Var list"""
        super(VarDictionary, self).__init__()
        self._name = name
        self._logger = logger
        self._parent_dict = parent_dict
        if parent_dict is not None:
            parent_dict.add_sub_scope(self)
        # end if
        self._sub_dicts = list()
        self._local_names = {} # local names in use
        if isinstance(variables, Var):
            self.add_variable(variables)
        elif isinstance(variables, list):
            for var in variables:
                self.add_variable(var)
            # end for
        elif isinstance(variables, VarDictionary):
            for stdname in variables.keys():
                self[stdname] = variables[stdname]
            # end for
        elif isinstance(variables, dict):
            # variables may not be in 'order', but we accept them anyway
            for key in variables.keys():
                var = variables[key]
                stdname = var.get_prop_value('standard_name')
                self[stdname] = variables[key]
            # end for
        elif variables is not None:
            raise ParseInternalError('Illegal type for variables, {} in {}'.format(type(variables), self.name))
        # end if

    @property
    def name(self):
        """Return this dictionary's name"""
        return self._name

    @property
    def parent(self):
        """Return the parent dictionary of this dictionary"""
        return self._parent_dict

    @staticmethod
    def include_var_in_list(var, std_vars, loop_vars, consts):
        """Return True iff <var> is of a type allowed by the logicals,
        <std_vars> (not constants or loop_vars),
        <loop_vars> a variable ending in '_extent', '_begin', '_end', or
        <consts> a variable with the 'protected' property.
        """
        standard_name = var.get_prop_value('standard_name')
        const_var = standard_name in CCPP_CONSTANT_VARS
        loop_var = standard_name in CCPP_LOOP_VAR_STDNAMES
        include_var = (consts and const_var) or (loop_var and loop_vars)
        if not include_var:
            std_var = not (loop_var or const_var)
            include_var = std_vars and std_var
        # end if
        return include_var

    def variable_list(self, recursive=False,
                      std_vars=True, loop_vars=True, consts=True):
        """Return a list of all variables"""
        if recursive and (self._parent_dict is not None):
            vlist = self._parent_dict.variable_list(recursive=recursive,
                                                    std_vars=std_vars,
                                                    loop_vars=loop_vars,
                                                    consts=consts)
        else:
            vlist = list()
        # end if
        for stdnam in self:
            var = self[stdnam]
            if self.include_var_in_list(var, std_vars=std_vars,
                                        loop_vars=loop_vars, consts=consts):
                vlist.append(var)
            # end if
        # end for
        return vlist

    def add_variable(self, newvar, exists_ok=False, gen_unique=False,
                     adjust_intent=False):
        """Add <newvar> if it does not conflict with existing entries
        If <exists_ok> is True, attempting to add an identical copy is okay.
        If <gen_unique> is True, a new local_name will be created if a
        local_name collision is detected.
        if <adjust_intent> is True, adjust conflicting intents to inout."""
        standard_name = newvar.get_prop_value('standard_name')
        if (standard_name in self) and (not exists_ok):
            # We already have a matching variable, error!
            if self._logger is not None:
                emsg = "Attempt to add duplicate variable, {} from {}"
                self._logger.error(emsg.format(standard_name,
                                               newvar.source.name))
            # end if
            emsg = "(duplicate) standard name in {}"
            ovar = self.find_variable(standard_name, any_scope=False)
            if ovar is not None:
                emsg += ", defined at {}".format(ovar.context)
            # end if
            raise ParseSyntaxError(emsg.format(self.name),
                                   token=standard_name, context=newvar.context)
        # end if
        cvar = self.find_variable(standard_name, any_scope=False)
        if cvar is not None:
            compat, reason = cvar.compatible(newvar, logger=self._logger)
            if compat:
                # Check for intent mismatch
                vintent = cvar.get_prop_value('intent')
                dintent = newvar.get_prop_value('intent')
                if vintent != dintent:
                    if adjust_intent:
                        if (vintent == 'in') and (dintent in ['inout', 'out']):
                            cvar.adjust_intent('inout')
                        elif ((vintent == 'out') and
                              (dintent in ['inout', 'in'])):
                            cvar.adjust_intent('inout')
                        # No else, variables are compatible
                    else:
                        emsg = "Attempt to add incompatible variable to {}"
                        emsg += "\nintent mismatch: {} ({}){} != {} ({}){}"
                        nlname = newvar.get_prop_value('local_name')
                        clname = cvar.get_prop_value('local_name')
                        nctx = context_string(newvar.context)
                        cctx = context_string(cvar.context)
                        raise CCPPError(emsg.format(self.name,
                                                    clname, vintent, cctx,
                                                    nlname, dintent, nctx))
                    # end if
                # end if
            else:
                if self._logger is not None:
                    emsg = "Attempt to add incompatible variable, {} from {}"
                    emsg += "\n{}".format(reason)
                    self._logger.error(emsg.format(standard_name,
                                                   newvar.source.name))
                # end if
                nlname = newvar.get_prop_value('local_name')
                clname = cvar.get_prop_value('local_name')
                cstr = context_string(cvar.context, with_comma=True)
                errstr = "new variable, {}, incompatible {} between {}{} and"
                raise ParseSyntaxError(errstr.format(nlname, reason,
                                                     clname, cstr),
                                       token=standard_name,
                                       context=newvar.context)
            # end if
        # end if
        lname = newvar.get_prop_value('local_name')
        lvar = self.find_local_name(lname)
        if lvar is not None:
            if gen_unique:
                new_lname = self.new_internal_variable_name(prefix=lname)
                newvar = newvar.clone(new_lname)
            elif not exists_ok:
                errstr = 'Invalid local_name: {} already registered{}'
                cstr = context_string(lvar.source.context, with_comma=True)
                raise ParseSyntaxError(errstr.format(lname, cstr),
                                       context=newvar.source.context)
            # end if (no else, things are okay)
        # end if (no else, things are okay)
        # Check if this variable has a parent (i.e., it is an array reference)
        aref = newvar.array_ref()
        if aref is not None:
            pname = aref.group(1).strip()
            pvar = self.find_local_name(pname)
            if pvar is not None:
                newvar.parent = pvar
            # end if
        # end if
        # If we make it to here without an exception, add the variable
        if standard_name not in self:
            self[standard_name] = newvar
        # end if
        lname = lname.lower()
        if lname not in self._local_names:
            self._local_names[lname] = standard_name
        # end if

    def remove_variable(self, standard_name):
        """Remove <standard_name> from the dictionary.
        Ignore if <standard_name> is not in dict
        """
        if standard_name in self:
            del self[standard_name]
        # end if

    def add_variable_dimensions(self, var, ignore_sources, to_dict=None):
        """Attempt to find a source for each dimension in <var> and add that
        Variable to this dictionary.
        Dimension variables which are found but whose Source is in
        <ignore_sources> are not added to this dictionary.
        If <to_dict> is present, add the dimension variables to it instead.
        Return an error string on failure."""

        err_ret = ''
        ctx = ''
        vdims = var.get_dim_stdnames(include_constants=False)
        for dimname in vdims:
            present = self.find_variable(dimname, any_scope=False)
            if not present:
                dvar = self.find_variable(dimname, any_scope=True)
                if dvar and (dvar.source.type not in ignore_sources):
                    if to_dict:
                        to_dict.add_variable(dvar)
                    else:
                        self.add_variable(dvar)
                    # end if
                else:
                    if err_ret:
                        err_ret += '\n'
                    else:
                        ctx = context_string(var.context)
                    # end if
                    err_ret += "{}: ".format(self.name)
                    err_ret += "Cannot find variable for dimension, {} of {}{}"
                    vstdname = var.get_prop_value('standard_name')
                    err_ret = err_ret.format(dimname, vstdname, ctx)
                # end if
            # end if
        # end for
        return err_ret

    def find_variable(self, standard_name, any_scope=True, clone=None):
        """Attempt to return the variable matching <standard_name>.
        If <any_scope> is True, search parent scopes if not in current scope.
        If the variable is not found and <clone> is not None, add a clone of
        <clone> to this dictionary.
        If the variable is not found and <clone> is None, return None.
        """
        if standard_name in CCPP_CONSTANT_VARS:
            var = CCPP_CONSTANT_VARS[standard_name]
        elif standard_name in self:
            var = self[standard_name]
        elif any_scope and (self._parent_dict is not None):
            var = self._parent_dict.find_variable(standard_name, any_scope)
        else:
            var = None
        # end if
        if (var is None) and (clone is not None):
            lname = clone.get_prop_value['local_name']
            new_name = self.new_internal_variable_name(prefix=lname)
            var = clone.clone(new_name)
        # end if
        return var

    def find_local_name(self, local_name, any_scope=False):
        """Return a variable in this dictionary with local_name = <local_name>
        or return None if no such variable is currently in the dictionary"""
        pvar = None
        lname = local_name.lower() # Case is insensitive for local names
        if lname in self._local_names:
            stdname = self._local_names[lname]
            pvar = self.find_variable(stdname, any_scope=False)
            if not pvar:
                emsg = 'VarDictionary {} should have standard_name, {}, '
                emsg += 'based on local_name {}'
                raise ParseInternalError(emsg.format(self.name,
                                                     stdname, local_name))
            # end if (no else, pvar is fine)
        elif any_scope and (self._parent_dict is not None):
            pvar = self._parent_dict.find_local_name(local_name,
                                                     any_scope=any_scope)
        # end if
        return pvar

    def add_sub_scope(self, sub_dict):
        """Add a child dictionary to enable traversal"""
        self._sub_dicts.append(sub_dict)

    def sub_dictionaries(self):
        """Return a list of this dictionary's sub-dictionaries"""
        return list(self._sub_dicts)

    def prop_list(self, prop_name, std_vars=True, loop_vars=True, consts=True):
        """Return a list of the <prop_name> property for each variable.
        std_vars are variables which are neither constants nor loop variables.
        """
        plist = list()
        for var in self.values():
            if self.include_var_in_list(var, std_vars=std_vars,
                                        loop_vars=loop_vars, consts=consts):
                plist.append(var.get_prop_value(prop_name))
            # end if
        # end for
        return plist

    def declare_variables(self, outfile, indent, dummy=False,
                          std_vars=True, loop_vars=True, consts=True):
        """Write out the declarations for this dictionary's variables"""
        for standard_name in self.keys():
            var = self.find_variable(standard_name, any_scope=False)
            if self.include_var_in_list(var, std_vars=std_vars,
                                        loop_vars=loop_vars, consts=consts):
                self[standard_name].write_def(outfile, indent, self,
                                              dummy=dummy)
            # end if
        # end for

    def merge(self, other_dict):
        """Add new entries from <other_dict>"""
        for ovar in other_dict.variable_list():
            self.add_variable(ovar)
        # end for

    @staticmethod
    def loop_var_okay(standard_name, is_run_phase):
        """If <standard_name> is a loop variable, return True only if it
        is appropriate for the phase (e.g., horizontal_loop_extent is okay
        during a run phase only while horizontal_dimension is not allowed
        during a run phase).
        If <standard_name> is not a loop variable, return True"""
        if (standard_name in CCPP_LOOP_VAR_STDNAMES) and (not is_run_phase):
            # Prohibit looking for loop variables except in run phases
            retval = False
        elif (standard_name == "horizontal_dimension") and is_run_phase:
            # horizontal_dimension should not be used in run phase
            retval = False
        else:
            retval = True
        # end if
        return retval

    def __str__(self):
        """Return a string that represents this dictionary object"""
        return "VarDictionary({}, {})".format(self.name, list(self.keys()))

    def __repr__(self):
        """Return an unique representation for this object"""
        srepr = super(VarDictionary, self).__repr__()
        vstart = len("VarDictionary") + 1
        if len(srepr) > vstart + 1:
            comma = ", "
        else:
            comma = ""
        # end if
        return "VarDictionary({}{}{}".format(self.name, comma, srepr[vstart:])

    def __del__(self):
        """Attempt to delete all of the variables in this dictionary"""
        self.clear()

    def __eq__(self, other):
        """Override == to restore object equality, not dictionary
        list equality"""
        return self is other

    @classmethod
    def loop_var_match(cls, standard_name):
        """Return a VarLoopSubst if <standard_name> is a loop variable,
        otherwise, return None"""
        # Strip off 'ccpp_constant_one:', if present
        if standard_name[0:18] == 'ccpp_constant_one:':
            beg = 18
        else:
            beg = 0
        # end if
        if standard_name[beg:] in CCPP_VAR_LOOP_SUBSTS:
            vmatch = CCPP_VAR_LOOP_SUBSTS[standard_name[beg:]]
        else:
            vmatch = None
        # end if
        return vmatch

    def find_loop_dim_match(self, dim_string):
        """Find a match in local dict for <dim_string>. That is, if
        <dim_string> has a loop dim substitution, and each standard name
        in that substitution is in self, return the equivalent local
        name string."""
        ldim_string = None
        if dim_string in CCPP_LOOP_DIM_SUBSTS:
            lnames = list()
            std_subst = CCPP_LOOP_DIM_SUBSTS[dim_string].split(':')
            for ssubst in std_subst:
                svar = self.find_variable(ssubst, any_scope=False)
                if svar is not None:
                    lnames.append(svar.get_prop_value('local_name'))
                else:
                    break
                # end if
            # end for
            if len(lnames) == len(std_subst):
                ldim_string = ':'.join(lnames)
            # end if
        # end if
        return ldim_string

    @classmethod
    def find_loop_dim_from_index(cls, index_string):
        """Given a loop index standard name, find the related loop dimension.
        """
        loop_dim_string = None
        for dim_string in CCPP_LOOP_DIM_SUBSTS:
            if index_string == CCPP_LOOP_DIM_SUBSTS[dim_string]:
                loop_dim_string = dim_string
                break
            # end if
        # end for
        return loop_dim_string

    def find_loop_subst(self, standard_name, any_scope=True, context=None):
        """If <standard_name> is of the form <standard_name>_extent and that
        variable is not in the dictionary, substitute a tuple of variables,
        (<standard_name>_begin, <standard_name>_end), if those variables are
        in the dictionary.
        If <standard_name>_extent *is* present, return that variable as a
        range, ('ccpp_constant_one', <standard_name>_extent)
        In other cases, return None
        """
        loop_var = VarDictionary.loop_var_match(standard_name)
        logger_str = None
        if loop_var is not None:
            # Let us see if we can fix a loop variable
            dict_var = self.find_variable(standard_name, any_scope=any_scope)
            if dict_var is not None:
                var_one = CCPP_CONSTANT_VARS['ccpp_constant_one']
                my_var = (var_one, dict_var)
                if self._logger is not None:
                    lstr = "loop_subst: found {}{}"
                    logger_str = lstr.format(standard_name,
                                             context_string(context))
                # end if
            else:
                my_vars = [self.find_variable(x) for x in loop_var]
                if None not in my_vars:
                    my_var = tuple(my_vars)
                    if self._logger is not None:
                        names = [x.get_prop_value('local_name')
                                 for x in my_vars]
                        lstr = "loop_subst: {} ==> ({}){}"
                        logger_str = lstr.format(standard_name,
                                                 ', '.join(names),
                                                 context_string(context))
                    # end if
                else:
                    if self._logger is not None:
                        lstr = "loop_subst: {} ==> (??) FAILED{}"
                        logger_str = lstr.format(standard_name,
                                                 context_string(context))
                    # end if
                    my_var = None
                # end if
            # end if
        else:
            if self._logger is not None:
                lstr = "loop_subst: {} is not a loop variable{}"
                logger_str = lstr.format(standard_name,
                                         context_string(context))
            # end if
            my_var = None
        # end if
        if logger_str is not None:
            self._logger.debug(logger_str)
        # end if
        return my_var

    def var_call_string(self, var, loop_vars=None):
        """Construct the actual argument string for <var> by translating
        standard names to local names. String includes array bounds.
        if <loop_vars> is present, look there first for array bounds,
        even if usage requires a loop substitution.
        """
        return var.call_string(self, loop_vars=loop_vars)

    def new_internal_variable_name(self, prefix=None, max_len=63):
        """Find a new local variable name for this dictionary.
        The new name begins with <prefix>_<self.name> or with <self.name>
        (where <self.name> is this VarDictionary's name) if <prefix> is None.
        The new variable name is kept to a maximum length of <max_len>.
        """
        index = 0
        if prefix is None:
            var_prefix = '{}_var'.format(self.name)
        else:
            var_prefix = '{}'.format(prefix)
        # end if
        varlist = [x for x in self._local_names.keys() if var_prefix in x]
        newvar = None
        while newvar is None:
            if index == 0:
                newvar = var_prefix
            else:
                newvar = '{}{}'.format(var_prefix, index)
            # end if
            index = index + 1
            if len(newvar) > max_len:
                var_prefix = var_prefix[:-1]
                newvar = None
            elif newvar in varlist:
                newvar = None
            # end if
        # end while
        return newvar

###############################################################################

# List of constant variables which are universally available
CCPP_CONSTANT_VARS = VarDictionary('CCPP_CONSTANT_VARS',
                                   [ccpp_standard_var('ccpp_constant_one',
                                                      'module')])

###############################################################################
if __name__ == "__main__":
    import doctest
    doctest.testmod()
