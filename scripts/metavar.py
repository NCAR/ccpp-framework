#!/usr/bin/env python3

"""
Classes and supporting code to hold all information on CCPP metadata variables
Var: Class which holds all information on a single CCPP metadata variable
VarSpec: Class to hold a standard_name description which can include dimensions
VarAction: Base class for describing actions on variables
VarLoopSubst: Class for describing a loop substitution
VarDictionary: Class to hold all CCPP variables of a CCPP unit (e.g., suite,
               scheme, host)
"""

# Python library imports
import re
from collections import OrderedDict
# CCPP framework imports
from framework_env import CCPPFrameworkEnv
from parse_tools import check_local_name, check_fortran_type, context_string
from parse_tools import FORTRAN_SCALAR_REF_RE
from parse_tools import check_units, check_dimensions, check_cf_standard_name
from parse_tools import check_diagnostic_id, check_diagnostic_fixed
from parse_tools import check_default_value, check_valid_values
from parse_tools import ParseContext, ParseSource
from parse_tools import ParseInternalError, ParseSyntaxError, CCPPError
from var_props import CCPP_LOOP_DIM_SUBSTS, VariableProperty, VarCompatObj
from var_props import find_horizontal_dimension, find_vertical_dimension
from var_props import standard_name_to_long_name, default_kind_val
from conversion_tools import unit_conversion

##############################################################################

# Dictionary of standard CCPP variables
CCPP_STANDARD_VARS = {
    # Variable representing the constant integer, 1
    'ccpp_constant_one' :
    {'local_name' : '1', 'protected' : 'True',
     'standard_name' : 'ccpp_constant_one',
     'long_name' : "CCPP constant one",
     'units' : '1', 'dimensions' : '()', 'type' : 'integer'},
    'ccpp_error_code' :
    {'local_name' : 'errflg', 'standard_name' : 'ccpp_error_code',
     'long_name' : "CCPP error flag",
     'units' : '1', 'dimensions' : '()', 'type' : 'integer'},
    'ccpp_error_message' :
    {'local_name' : 'errmsg', 'standard_name' : 'ccpp_error_message',
     'long_name' : "CCPP error message",
     'units' : 'none', 'dimensions' : '()', 'type' : 'character',
     'kind' : 'len=512'},
    'horizontal_dimension' :
    {'local_name' : 'total_columns',
     'standard_name' : 'horizontal_dimension', 'units' : 'count',
     'long_name' : "total number of columns",
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
# Used for creating template variables
_MVAR_DUMMY_RUN_ENV = CCPPFrameworkEnv(None, ndict={'host_files':'',
                                                    'scheme_files':'',
                                                    'suites':''})

##############################################################################

class Var:
    """ A class to hold a metadata or code variable.
    Var objects should be treated as immutable.
    >>> Var.get_prop('standard_name') #doctest: +ELLIPSIS
    <var_props.VariableProperty object at 0x...>
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
    >>> Var.get_prop('active') #doctest: +ELLIPSIS
    <var_props.VariableProperty object at 0x...>
    >>> Var.get_prop('active').valid_value('flag_for_aerosol_physics')
    'flag_for_aerosol_physics'
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV).get_prop_value('long_name')
    'Hi mom'
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV).get_prop_value('intent')
    'in'
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV).get_prop_value('units')
    'm s-1'
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV).get_prop_value('units') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Required property, 'units', missing, in <standard input>
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : ' ', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV).get_prop_value('units') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: foo: ' ' is not a valid unit, in <standard input>
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'ttype' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid metadata variable property, 'ttype', in <standard input>
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Required property, 'units', missing, in <standard input>
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'inout', 'protected' : '.true.'}, ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: foo is marked protected but is intent inout, at <standard input>:1
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'ino'}, ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid intent variable property, 'ino', at <standard input>:1
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in', 'optional' : 'false'}, ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid variable property name, 'optional', at <standard input>:1
    # Check that two variables that differ in their units - m vs km - are compatible
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm',     \
             'dimensions' : '()', 'type' : 'real', 'intent' : 'in'},              \
            ParseSource('vname', 'SCHEME', ParseContext()),                       \
            _MVAR_DUMMY_RUN_ENV).compatible(Var({'local_name' : 'bar',            \
             'standard_name' : 'hi_mom', 'units' : 'km',                          \
             'dimensions' : '()', 'type' : 'real', 'intent' : 'in'},              \
            ParseSource('vname', 'SCHEME', ParseContext()), _MVAR_DUMMY_RUN_ENV), \
            _MVAR_DUMMY_RUN_ENV) #doctest: +ELLIPSIS
    <var_props.VarCompatObj object at ...>
    """

    ## Prop lists below define all the allowed CCPP Metadata attributes

    # __spec_props are for variables defined in a specification
    __spec_props = [VariableProperty('local_name', str,
                                     check_fn_in=check_local_name),
                    VariableProperty('standard_name', str,
                                     check_fn_in=check_cf_standard_name),
                    VariableProperty('long_name', str, optional_in=True,
                                     default_fn_in=standard_name_to_long_name),
                    VariableProperty('units', str,
                                     check_fn_in=check_units),
                    VariableProperty('dimensions', list,
                                     check_fn_in=check_dimensions),
                    VariableProperty('type', str,
                                     check_fn_in=check_fortran_type),
                    VariableProperty('kind', str,
                                     optional_in=True,
                                     default_fn_in=default_kind_val),
                    VariableProperty('state_variable', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('protected', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('allocatable', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('diagnostic_name', str,
                                     optional_in=True, default_in='',
                                     check_fn_in=check_diagnostic_id),
                    VariableProperty('diagnostic_name_fixed', str,
                                     optional_in=True, default_in='',
                                     check_fn_in=check_diagnostic_fixed),
                    VariableProperty('default_value', str,
                                     optional_in=True, default_in='',
                                     check_fn_in=check_default_value),
                    VariableProperty('persistence', str, optional_in=True,
                                     valid_values_in=['timestep', 'run'],
                                     default_in='timestep'),
                    VariableProperty('active', str, optional_in=True,
                                     default_in='.true.'),
                    VariableProperty('polymorphic', bool, optional_in=True,
                                     default_in='.false.')]

# XXgoldyXX: v debug only
    __to_add = VariableProperty('valid_values', str,
                                optional_in=True, default_in='',
                                check_fn_in=check_valid_values)
# XXgoldyXX: ^ debug only

    # __var_props contains properties which are not in __spec_props
    __var_props = [VariableProperty('intent', str,
                                    valid_values_in=['in', 'out', 'inout'])]

    # __constituent_props contains properties associated only with constituents
    # Note that all constituent properties must be optional and contain either
    #   a default value or default function.
    __constituent_props = [VariableProperty('advected', bool,
                                            optional_in=True, default_in=False)]

    __constituent_prop_dict = {x.name : x for x in __constituent_props}

    # __no_metadata_props__ contains properties to omit from metadata
    __no_metadata_props__ = ['local_name']

    __spec_propdict = {p.name : p for p in __spec_props}
    __var_propdict = {p.name : p for p in __spec_props + __var_props}
    __required_spec_props = list()
    __required_var_props = list()
    for p in __spec_props:
        __var_propdict[p.name] = p
        if not p.optional:
            __required_spec_props.append(p.name)
            __required_var_props.append(p.name)
        # end if
    # end for
    for p in __var_props:
# XXgoldyXX: v why?
#        __spec_propdict[p.name] = p
# XXgoldyXX: ^ why?
#        __var_propdict[p.name] = p
        if not p.optional:
            __required_var_props.append(p.name)
        # end if
    # end for
    __var_propdict.update({p.name : p for p in __constituent_props})
    # All constituent props are optional so no check

    def __init__(self, prop_dict, source, run_env, context=None,
                 clone_source=None):
        """Initialize a new Var object.
        If <prop_dict> is really a Var object, use that object's prop_dict.
        If this Var object is a clone, record the original Var object
            for reference
        <source> is a ParseSource object describing the source of this Var.
        <run_env> is the CCPPFrameworkEnv object for this framework run.
        <context> is a ParseContext object
        <clone_source> is a Var object. If provided, it is used as the original
            source of a cloned variable.
        """
        self.__parent_var = None # for array references
        self.__children = list() # This Var's array references
        self.__clone_source = clone_source
        self.__run_env = run_env
        if isinstance(prop_dict, Var):
            prop_dict = prop_dict.copy_prop_dict()
        # end if
        if source.type == 'scheme':
            self.__required_props = Var.__required_var_props
# XXgoldyXX: v don't fill in default properties?
#            mstr_propdict = Var.__var_propdict
# XXgoldyXX: ^ don't fill in default properties?
        else:
            self.__required_props = Var.__required_spec_props
# XXgoldyXX: v don't fill in default properties?
            mstr_propdict = Var.__spec_propdict
# XXgoldyXX: ^ don't fill in default properties?
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
            self.__intrinsic = False
        else:
            self.__intrinsic = True
        # end if
        for key in prop_dict:
            if Var.get_prop(key) is None:
                raise ParseSyntaxError("Invalid metadata variable property, '{}'".format(key), context=self.context)
            # end if
        # end for
        # Make sure required properties are present
        for propname in self.__required_props:
            if propname not in prop_dict:
                emsg = "Required property, '{}', missing"
                raise ParseSyntaxError(emsg.format(propname),
                                       context=self.context)
            # end if
        # end for
        # Check for any mismatch
        if ('protected' in prop_dict) and ('intent' in prop_dict):
            if (prop_dict['intent'].lower() != 'in') and prop_dict['protected']:
                emsg = "{} is marked protected but is intent {}"
                raise ParseSyntaxError(emsg.format(prop_dict['local_name'],
                                                   prop_dict['intent']),
                                       context=self.context)
            # end if
        # end if
        # Look for any constituent properties
        self.__is_constituent = False
        for name, prop in Var.__constituent_prop_dict.items():
            if (name in prop_dict) and                                         \
               (prop_dict[name] != prop.get_default_val(prop_dict,
                                                        context=self.context)):
                self.__is_constituent = True
                break
            # end if
        # end for
        # Steal dict from caller
        self._prop_dict = prop_dict
# XXgoldyXX: v don't fill in default properties?
#        # Fill in default values for missing properties
#        for propname in mstr_propdict:
#            if (propname not in prop_dict) and mstr_propdict[propname].optional:
#                mval = mstr_propdict[propname]
#                def_val = mval.get_default_val(self._prop_dict,
#                                               context=self.context)
#                self._prop_dict[propname] = def_val
#            # end if
#        # end for
# XXgoldyXX: ^ don't fill in default properties?
        # Make sure all the variable values are valid
        try:
            for prop_name, prop_val in self.var_properties():
                prop = Var.get_prop(prop_name)
                _ = prop.valid_value(prop_val,
                                     prop_dict=self._prop_dict, error=True)
            # end for
        except CCPPError as cperr:
            lname = self._prop_dict['local_name']
            emsg = "{}: {}"
            raise ParseSyntaxError(emsg.format(lname, cperr),
                                   context=self.context) from cperr
        # end try

    def compatible(self, other, run_env):
        """Return a VarCompatObj object which describes the equivalence,
        compatibility, or incompatibility between <self> and <other>.
        """
        # We accept character(len=*) as compatible with
        # character(len=INTEGER_VALUE)
        stype = self.get_prop_value('type')
        skind = self.get_prop_value('kind')
        sunits = self.get_prop_value('units')
        sstd_name = self.get_prop_value('standard_name')
        sloc_name = self.get_prop_value('local_name')
        sdims = self.get_dimensions()
        otype = other.get_prop_value('type')
        okind = other.get_prop_value('kind')
        ounits = other.get_prop_value('units')
        ostd_name = other.get_prop_value('standard_name')
        oloc_name = other.get_prop_value('local_name')
        odims = other.get_dimensions()
        compat = VarCompatObj(sstd_name, stype, skind, sunits, sdims, sloc_name,
                              ostd_name, otype, okind, ounits, odims, oloc_name,
                              run_env,
                              v1_context=self.context, v2_context=other.context)
        if (not compat) and (run_env.logger is not None):
            incompat_str = compat.incompat_reason
            if incompat_str is not None:
                run_env.logger.info('{}'.format(incompat_str))
            # end if (no else)
        # end if
        return compat

    def adjust_intent(self, src_var):
        """Add an intent to this Var or adjust its existing intent.
        Note: An existing intent can only be adjusted to 'inout'
        """
        if 'intent' in self._prop_dict:
            my_intent = self.get_prop_value('intent')
        else:
            my_intent = None
        # end if
        sv_intent = src_var.get_prop_value('intent')
        if not sv_intent:
            sv_intent = 'in'
        # end if
        if sv_intent in ['inout', 'out'] and self.get_prop_value('protected'):
            lname = self.get_prop_value('local_name')
            lctx = context_string(self.context)
            emsg = "Attempt to set intent of {}{} to {}, only 'in' allowed "
            emsg += "for 'protected' variable."
            if src_var:
                slname = src_var.get_prop_value('local_name')
                sctx = context_string(src_var.context)
                emsg += "\nintent source: {}{}".format(slname, sctx)
            # end if
            raise CCPPError(emsg.format(lname, lctx, sv_intent))
        # end if (else, no error)
        if my_intent:
            if my_intent != sv_intent:
                self._prop_dict['intent'] = 'inout'
            # end if  (no else, intent is okay)
        else:
            self._prop_dict['intent'] = sv_intent
        # end if

    @staticmethod
    def get_prop(name, spec_type=None):
        """Return VariableProperty object for <name> or None"""
        prop = None
        if (spec_type is None) and (name in Var.__var_propdict):
            prop = Var.__var_propdict[name]
        elif (spec_type is not None) and (name in Var.__spec_propdict):
            prop = Var.__spec_propdict[name]
        # end if (else prop = None)
        return prop

    def var_properties(self):
        """Return an iterator for this Var's property dictionary"""
        return self._prop_dict.items()

    def copy_prop_dict(self, subst_dict=None):
        """Create a copy of our prop_dict, possibly substituting properties
        from <subst_dict>."""
        cprop_dict = {}
        # Start with a straight copy of this variable's prop_dict
        for prop, val in self.var_properties():
            cprop_dict[prop] = val
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

    def clone(self, subst_dict=None, remove_intent=False,
              source_name=None, source_type=None, context=None):
        """Create a clone of this Var object with properties from <subst_dict>
        overriding this variable's properties. <subst_dict> may also be
        a string in which case only the local_name property is changed
        (to the value of the <subst_dict> string).
        If <remove_intent> is True, remove the 'intent' property, if present.
           This can be used to promote a variable to module level.
        The optional <source_name>, <source_type>, and <context> inputs
        allow the clone to appear to be coming from a designated source,
        by default, the source and type are the same as this Var (self).
        """
        if isinstance(subst_dict, str):
            subst_dict = {'local_name':subst_dict}
        elif subst_dict is None:
            subst_dict = {}
        # end if
        cprop_dict = self.copy_prop_dict(subst_dict=subst_dict)
        if remove_intent and ('intent' in cprop_dict):
            del cprop_dict['intent']
        # end if
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

        return Var(cprop_dict, psource, self.run_env, clone_source=self)

    def get_prop_value(self, name):
        """Return the value of key, <name> if <name> is in this variable's
        property dictionary.
        If <name> is not in the prop dict but does have a <default_fn_in>
           property, return the value specified by calling that function.
        Otherwise, return None
        """
        if name in self._prop_dict:
            pvalue = self._prop_dict[name]
        elif name in Var.__var_propdict:
            vprop = Var.__var_propdict[name]
            if vprop.has_default_func:
                pvalue = vprop.get_default_val(self._prop_dict,
                                               context=self.context)
            else:
                pvalue = None
            # end if
        else:
            pvalue = None
        # end if
        return pvalue

    def handle_array_ref(self):
        """If this Var's local_name is an array ref, add in the array
        reference indices to the Var's dimensions.
        Return the (stripped) local_name and the full dimensions.
        >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext()), _MVAR_DUMMY_RUN_ENV).handle_array_ref()
        ('foo', [])
        >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '(ccpp_constant_one:dim1)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext()), _MVAR_DUMMY_RUN_ENV).handle_array_ref()
        ('foo', ['ccpp_constant_one:dim1'])
        >>> Var({'local_name' : 'foo(:,:,bar)', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '(ccpp_constant_one:dim1,ccpp_constant_one:dim2)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext()), _MVAR_DUMMY_RUN_ENV).handle_array_ref()
        ('foo', ['ccpp_constant_one:dim1', 'ccpp_constant_one:dim2', 'bar'])
        >>> Var({'local_name' : 'foo(bar,:)', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '(ccpp_constant_one:dim1)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext()), _MVAR_DUMMY_RUN_ENV).handle_array_ref()
        ('foo', ['bar', 'ccpp_constant_one:dim1'])
        >>> Var({'local_name' : 'foo(bar)', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '(ccpp_constant_one:dim1)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext()), _MVAR_DUMMY_RUN_ENV).handle_array_ref() #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: Call dims mismatch for foo(bar), not enough colons
        >>> Var({'local_name' : 'foo(:,bar,:)', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '(ccpp_constant_one:dim1)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext()), _MVAR_DUMMY_RUN_ENV).handle_array_ref() #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: Call dims mismatch for foo(:,bar,:), not enough dims
        >>> Var({'local_name' : 'foo(:,:,bar)', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '(ccpp_constant_one:dim1)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext()), _MVAR_DUMMY_RUN_ENV).handle_array_ref() #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: Call dims mismatch for foo(:,:,bar), not enough dims
        >>> Var({'local_name' : 'foo(:,bar)', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '(ccpp_constant_one:dim1,ccpp_constant_one:dim2)', 'type' : 'real',}, ParseSource('vname', 'HOST', ParseContext()), _MVAR_DUMMY_RUN_ENV).handle_array_ref() #doctest: +IGNORE_EXCEPTION_DETAIL
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

    def call_dimstring(self, var_dicts=None,
                       explicit_dims=False, loop_subst=False):
        """Return the dimensions string for a variable call.
        If <var_dict> is present, find and substitute a local_name for
        each standard_name in this variable's dimensions.
        If <var_dict> is not present, return a colon for each dimension.
        If <explicit_dims> is True, include the variable's dimensions.
        If <loop_subst> is True, apply a loop substitution, if found for any
           missing dimension.
        """
        emsg = ''
        _, dims = self.handle_array_ref()
        if var_dicts is not None:
            dimlist = []
            sepstr = ''
            for dim in dims:
                # Decide whether to list all dimensions or to replace
                # a range with a colon.
                dstdnames = dim.split(':')
                add_dims = explicit_dims or (len(dstdnames) == 1)
                dvar = None
                if add_dims and loop_subst:
                    for vdict in var_dicts:
                        dvar = vdict.find_loop_dim_match(dim)
                        if dvar is not None:
                            break
                        # end if
                    # end for
                    if dvar:
                        dimlist.append(dvar)
                    # end if
                if (not dvar) and add_dims:
                    dnames = []
                    for stdname in dstdnames:
                        for vdict in var_dicts:
                            dvar = vdict.find_variable(standard_name=stdname,
                                                       any_scope=False)
                            if dvar is not None:
                                break
                            # end if
                        # end for
                        if dvar:
                            dnames.append(dvar.get_prop_value('local_name'))
                        # end if
                        if not dvar:
                            emsg += sepstr + "No variable found in "
                            vnames = [x.name for x in var_dicts]
                            if len(vnames) > 2:
                                vstr = ', '.join(vnames[:-1])
                                vstr += ', or {}'.format(vnames[-1])
                            elif len(vnames) > 1:
                                vstr = ' or '.join(vnames)
                            else:
                                vstr = vnames[0]
                            # end if
                            emsg += "{} for dimension '".format(vstr)
                            emsg += stdname + "' in {vlnam}"
                            sepstr = '\n'
                        # end if
                    # end for
                    dimlist.append(':'.join(dnames))
                elif not add_dims:
                    dimlist.append(':')
                # end if (no else needed, we must have found loop substitution)
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
                            dvar = var_dict.find_variable(standard_name=item,
                                                          any_scope=False)
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

    def array_ref(self, local_name=None):
        """If this Var's local_name is an array reference, return a
        Fortran array reference regexp match.
        Otherwise, return None"""
        if local_name is None:
            local_name = self.get_prop_value('local_name')
        # end if
        match = FORTRAN_SCALAR_REF_RE.match(local_name)
        return match

    def intrinsic_elements(self, check_dict=None):
        """Return a list of the standard names of this Var object's 'leaf'
        intrinsic elements or this Var object's standard name if it is an
        intrinsic 'leaf' variable.
        If this Var object cannot be reduced to one or more intrinsic 'leaf'
        variables (e.g., a DDT Var with no named elements), return None.
        A 'leaf' intrinsic Var is a Var of intrinsic Fortran type which has
        no children. If a Var has children, those children will be searched
        to find leaves. If a Var is a DDT, its named elements are searched.
        If <check_dict> is not None, it is checked for children if none are
        found in this variable (via finding a variable in <check_dict> with
        the same standard name).
        Currently, an array of DDTs is not processed (return None) since
        Fortran does not support a way to reference those elements.
        """
        if self.is_ddt():
            element_names = None
            raise ValueError("shouldn't happen?")
            # To Do, find and process named elements of DDT
        # end if
        children = self.children()
        if (not children) and check_dict:
            stdname = self.get_prop_value("standard_name")
            pvar = check_dict.find_variable(standard_name=stdname,
                                            any_scope=True)
            if pvar:
                children = pvar.children()
            # end if
        # end if
        if children:
            element_names = list()
            for child in children:
                child_elements = child.intrinsic_elements()
                if isinstance(child_elements, str):
                    child_elements = [child_elements]
                # end if
                if child_elements:
                    for elem in child_elements:
                        if elem:
                            element_names.append(elem)
                        # end if
                    # end for
                # end if
            # end for
        else:
            element_names = self.get_prop_value('standard_name')
        # end if
        return element_names

    @classmethod
    def constituent_property_names(cls):
        """Return a list of the names of constituent properties"""
        return Var.__constituent_prop_dict.keys()

    @property
    def parent(self):
        """Return this variable's parent variable (or None)"""
        return self.__parent_var

    @parent.setter
    def parent(self, parent_var):
        """Set this variable's parent if not already set"""
        if self.__parent_var is not None:
            emsg = 'Attempting to set parent for {} but parent already set'
            lname = self.get_prop_value('local_name')
            raise ParseInternalError(emsg.format(lname))
        # end if
        if isinstance(parent_var, Var):
            self.__parent_var = parent_var
            parent_var.add_child(self)
        else:
            emsg = 'Attempting to set parent for {}, bad parent type, {}'
            lname = self.get_prop_value('local_name')
            raise ParseInternalError(emsg.format(lname, type(parent_var)))
        # end if

    def add_child(self, cvar):
        """Add <cvar> as a child of this Var object"""
        if cvar not in self.__children:
            self.__children.append(cvar)
        # end if

    def children(self):
        """Return an iterator over this object's children or None if the
        object has no children."""
        children = self.__children
        if not children:
            pvar = self
            while (not children) and pvar.clone_source:
                pvar = pvar.clone_source
                children = pvar.children()
            # end while
        # end if
        return iter(children) if children else None

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
    def clone_source(self):
        """Return this Var object's clone source (or None)"""
        return self.__clone_source

    @property
    def host_interface_var(self):
        """True iff self is included in the host model interface calls"""
        return self.source.type == 'host'

    @property
    def run_env(self):
        """Return the CCPPFrameworkEnv object used to create this Var object."""
        return self.__run_env

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
        return find_horizontal_dimension(vdims)[0]

    def has_vertical_dimension(self, dims=None):
        """Return vertical dimension standard name string for
        <self> or <dims> (if present) if a vertical dimension is
        present in the list"""
        if dims is None:
            vdims = self.get_dimensions()
        else:
            vdims = dims
        # end if
        return find_vertical_dimension(vdims)[0]

    def write_def(self, outfile, indent, wdict, allocatable=False,
                  dummy=False, add_intent=None, extra_space=0):
        """Write the definition line for the variable to <outfile>.
        If <dummy> is True, include the variable's intent.
        If <dummy> is True but the variable has no intent, add the
        intent indicated by <add_intent>. This is intended for host model
        variables and it is an error to not pass <add_intent> if <dummy>
        is True and the variable has no intent property."""
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
        aref = self.array_ref(local_name=name)
        if aref is not None:
            name = aref.group(1)
        # end if
        dims = self.get_dimensions()
        if dims:
            if allocatable or dummy:
                dimstr = '(:' + ',:'*(len(dims) - 1) + ')'
            else:
                dimstr = self.call_dimstring(var_dicts=[wdict])
        else:
            dimstr = ''
        # end if
        protected = self.get_prop_value('protected')
        polymorphic = self.get_prop_value('polymorphic')
        if dummy:
            intent = self.get_prop_value('intent')
        else:
            intent = None
        # end if
        if protected and allocatable:
            errmsg = 'Cannot create allocatable variable from protected, {}'
            raise CCPPError(errmsg.format(name))
        # end if
        if dummy and (intent is None):
            if add_intent is not None:
                intent = add_intent
            else:
                errmsg = "<add_intent> is missing for dummy argument, {}"
                raise CCPPError(errmsg.format(name))
            # end if
        # end if
        if protected and dummy:
            intent_str = 'intent(in)   '
        elif allocatable:
            if dimstr or polymorphic:
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
        elif not dummy:
            intent_str = ''
        else:
            intent_str = ' '*13
        # end if
        if intent_str.strip():
            comma = ','
        else:
            comma = ' '
        # end if
        if self.is_ddt():
            if polymorphic:
                dstr = "class({kind}){cspc}{intent} :: {name}{dims} ! {sname}"
                cspc = comma + ' '*(extra_space + 12 - len(kind))
            else:
                dstr = "type({kind}){cspc}{intent} :: {name}{dims} ! {sname}"
                cspc = comma + ' '*(extra_space + 13 - len(kind))
            # end if
        else:
            if kind:
                dstr = "{type}({kind}){cspc}{intent} :: {name}{dims} ! {sname}"
                cspc = comma + ' '*(extra_space + 17 - len(vtype) - len(kind))
            else:
                dstr = "{type}{cspc}{intent} :: {name}{dims} ! {sname}"
                cspc = comma + ' '*(extra_space + 19 - len(vtype))
            # end if
        # end if
        outfile.write(dstr.format(type=vtype, kind=kind, intent=intent_str,
                                  name=name, dims=dimstr, cspc=cspc,
                                  sname=stdname), indent)

    def is_ddt(self):
        """Return True iff <self> is a DDT type."""
        return not self.__intrinsic

    def is_constituent(self):
        """Return True iff <self> is a constituent variable."""
        return self.__is_constituent

    def __str__(self):
        """Print representation or string for Var objects"""
        return "<Var {standard_name}: {local_name}>".format(**self._prop_dict)

    def __repr__(self):
        """Object representation for Var objects"""
        base = super().__repr__()
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

class FortranVar(Var):
    """A class to hold the metadata for a Fortran variable which can
    contain properties not used in CCPP metadata.
    """

    __fortran_props = [VariableProperty('optional', bool,
                                        optional_in=True, default_in=False)]

    def __init__(self, prop_dict, source, run_env, context=None,
                 clone_source=None):
        """Initialize a FortranVar object.
        """

        # Remove and save any Fortran-only properties
        save_dict = {}
        for prop in self.__fortran_props:
            if prop.name in prop_dict:
                save_dict[prop.name] = prop_dict[prop.name]
                del prop_dict[prop.name]
            # end if
        # end for
        # Initialize Var
        super().__init__(prop_dict, source, run_env, context=context,
                         clone_source=clone_source)
        # Now, restore the saved properties
        for prop in save_dict:
            self._prop_dict[prop] = save_dict[prop]
        # end for


###############################################################################

class VarSpec:
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

def ccpp_standard_var(std_name, source_type, run_env,
                      context=None, intent='out'):
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
        newvar = Var(vdict, psource, run_env)
    else:
        newvar = None
    # end if
    return newvar

###############################################################################

class VarAction:
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
        return vmatch.__class__ != self.__class__

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

class VarUnitConv(VarAction):
    """A class to perform unit conversions"""

    def __init__(self, set_action, local_name, kind, **kwargs):
        """Initialize unit conversion"""
        self._local_name      = local_name
        self._local_name_temp = local_name+'_local'
        self._kind            = kind
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)
        self._set_action = set_action

    def add_local(self, var):
        """Add a local variable for unit conversion"""
        tmp_var = var.clone(self._local_name_temp)
        self.__group.manage_variable(tmp_var)

    def write_action(self, vadict):
        """Return unit conversion transformation"""
        if self._set_action:
            if self._key == 'from':
                lhs = var=self._local_name_temp
                rhs = self._set_action.format(var=self._local_name, kind='_'+self._kind)
            if self._key == 'to':
                lhs = var=self._local_name
                rhs = self._set_action.format(var=self._local_name_temp, kind='_'+self._kind)

        return f"{lhs} = {rhs}"

    @property
    def required_stdnames(self):
        """Return the _required_stdnames for this object"""
        return self._required_stdnames

    @property
    def missing_stdname(self):
        """Return the _missing_stdname for this object"""
        return self._missing_stdname

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
        super().__init__()

    def has_subst(self, vadict, any_scope=False):
        """Determine if variables for the required standard names of this
        VarLoopSubst object are present in <vadict> (or in the parents of
        <vadict>) if <any_scope> is True.
        Return a list of the required variables on success, None on failure.
        """
        # A template for 'missing' should be in the standard variable list
        subst_list = list()
        for name in self.required_stdnames:
            svar = vadict.find_variable(standard_name=name, any_scope=any_scope)
            if svar is None:
                subst_list = None
                break
            # end i
            subst_list.append(svar)
        # end for
        return subst_list

    def add_local(self, vadict, source, run_env):
        """Add a Var created from the missing name to <vadict>"""
        if self.missing_stdname not in vadict:
            lname = self._local_name
            local_name = vadict.new_internal_variable_name(prefix=lname)
            prop_dict = {'standard_name':self.missing_stdname,
                         'local_name':local_name,
                         'type':'integer', 'units':'count', 'dimensions':'()'}
            var = Var(prop_dict, source, run_env)
            print('    add_local():')
            vadict.add_variable(var, run_env, exists_ok=True, gen_unique=True)
        # end if

    def equiv(self, vmatch):
        """Return True iff <vmatch> is equivalent to <self>.
        Equivalence is determined by matching the missing standard name
        and the required standard names"""
        is_equiv = super().equiv(vmatch)
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
                var = vadict.find_variable(standard_name=stdname,
                                           any_scope=any_scope)
                if (var is None) and (dict2 is not None):
                    var = dict2.find_variable(standard_name=stdname,
                                              any_scope=any_scope)
                # end if
                if var is None:
                    errmsg = "Required variable, {}, not found"
                    raise CCPPError(errmsg.format(stdname))
                # end if
                action_dict[stdname] = var.get_prop_value('local_name')
            # end for
            var = vadict.find_variable(standard_name=self.missing_stdname)
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
    >>> VarDictionary('foo', _MVAR_DUMMY_RUN_ENV)
    VarDictionary(foo)
    >>> VarDictionary('bar', _MVAR_DUMMY_RUN_ENV, variables={})
    VarDictionary(bar)
    >>> VarDictionary('baz', _MVAR_DUMMY_RUN_ENV, variables=Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()), _MVAR_DUMMY_RUN_ENV)) #doctest: +ELLIPSIS
    VarDictionary(baz, [('hi_mom', <__main__.Var hi_mom: foo at 0x...>)])
    >>> print("{}".format(VarDictionary('baz', _MVAR_DUMMY_RUN_ENV, variables=Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()), _MVAR_DUMMY_RUN_ENV))))
    VarDictionary(baz, ['hi_mom'])
    >>> VarDictionary('qux', _MVAR_DUMMY_RUN_ENV, variables=[Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()), _MVAR_DUMMY_RUN_ENV)]) #doctest: +ELLIPSIS
    VarDictionary(qux, [('hi_mom', <__main__.Var hi_mom: foo at 0x...>)])
    >>> VarDictionary('boo', _MVAR_DUMMY_RUN_ENV).add_variable(Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()), _MVAR_DUMMY_RUN_ENV), _MVAR_DUMMY_RUN_ENV)

    >>> VarDictionary('who', _MVAR_DUMMY_RUN_ENV, variables=[Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()), _MVAR_DUMMY_RUN_ENV)]).prop_list('local_name')
    ['foo']
    >>> VarDictionary('who', _MVAR_DUMMY_RUN_ENV, variables=[Var({'local_name' : 'who_var1', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()), _MVAR_DUMMY_RUN_ENV),Var({'local_name' : 'who_var', 'standard_name' : 'bye_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()), _MVAR_DUMMY_RUN_ENV)]).new_internal_variable_name()
    'who_var2'
    >>> VarDictionary('who', _MVAR_DUMMY_RUN_ENV, variables=[Var({'local_name' : 'who_var1', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()), _MVAR_DUMMY_RUN_ENV)]).new_internal_variable_name(prefix='bar')
    'bar'
    >>> VarDictionary('glitch', _MVAR_DUMMY_RUN_ENV, variables=Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()), _MVAR_DUMMY_RUN_ENV)).add_variable(Var({'local_name' : 'bar', 'standard_name' : 'hi_mom', 'units' : 'm s-1', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname2', 'DDT', ParseContext()), _MVAR_DUMMY_RUN_ENV), _MVAR_DUMMY_RUN_ENV) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid Duplicate standard name, 'hi_mom', at <standard input>:
    """

    def __init__(self, name, run_env, variables=None,
                 parent_dict=None):
        """Unlike dict, VarDictionary only takes a Var or Var list"""
        super().__init__()
        self.__name = name
        self.__run_env = run_env
        self.__parent_dict = parent_dict
        if parent_dict is not None:
            parent_dict.add_sub_scope(self)
        # end if
        self.__sub_dicts = list()
        self.__local_names = {} # local names in use
        if isinstance(variables, Var):
            self.add_variable(variables, run_env)
        elif isinstance(variables, list):
            for var in variables:
                self.add_variable(var, run_env)
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
        return self.__name

    @property
    def parent(self):
        """Return the parent dictionary of this dictionary"""
        return self.__parent_dict

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
        if recursive and (self.__parent_dict is not None):
            vlist = self.__parent_dict.variable_list(recursive=recursive,
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

    def add_variable(self, newvar, run_env, exists_ok=False, gen_unique=False,
                     adjust_intent=False):
        """Add <newvar> if it does not conflict with existing entries
        If <exists_ok> is True, attempting to add an identical copy is okay.
        If <gen_unique> is True, a new local_name will be created if a
        local_name collision is detected.
        if <adjust_intent> is True, adjust conflicting intents to inout."""
        standard_name = newvar.get_prop_value('standard_name')
        cvar = self.find_variable(standard_name=standard_name, any_scope=False)
        if (standard_name in self) and (not exists_ok):
            # We already have a matching variable, error!
            if self.__run_env.logger is not None:
                emsg = "Attempt to add duplicate variable, {} from {}"
                self.__run_env.logger.error(emsg.format(standard_name,
                                                        newvar.source.name))
            # end if
            emsg = "(duplicate) standard name in {}"
            if cvar is not None:
                emsg += ", defined at {}".format(cvar.context)
            # end if
            raise ParseSyntaxError(emsg.format(self.name),
                                   token=standard_name, context=newvar.context)
        # end if
        if cvar is not None:
            compat = cvar.compatible(newvar, run_env)
            if compat:
                # Check for intent mismatch
                vintent = cvar.get_prop_value('intent')
                dintent = newvar.get_prop_value('intent')
                # XXgoldyXX: Add special case for host variables here?
                if vintent != dintent:
                    if adjust_intent:
                        if (vintent == 'in') and (dintent in ['inout', 'out']):
                            cvar.adjust_intent(newvar)
                        elif ((vintent == 'out') and
                              (dintent in ['inout', 'in'])):
                            cvar.adjust_intent(newvar)
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
                if self.__run_env.logger is not None:
                    emsg = "Attempt to add incompatible variable, {} from {}"
                    emsg += "\n{}".format(compat.incompat_reason)
                    self.__run_env.logger.error(emsg.format(standard_name,
                                                            newvar.source.name))
                # end if
                nlname = newvar.get_prop_value('local_name')
                clname = cvar.get_prop_value('local_name')
                cstr = context_string(cvar.context, with_comma=True)
                errstr = "new variable, {}, incompatible {} between {}{} and"
                raise ParseSyntaxError(errstr.format(nlname,
                                                     compat.incompat_reason,
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
        aref = newvar.array_ref(local_name=lname)
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
        if lname not in self.__local_names:
            self.__local_names[lname] = standard_name
        # end if

    def remove_variable(self, standard_name):
        """Remove <standard_name> from the dictionary.
        Ignore if <standard_name> is not in dict
        """
        if standard_name in self:
            del self[standard_name]
        # end if

    def add_variable_dimensions(self, var, ignore_sources, to_dict=None,
                                adjust_intent=False):
        """Attempt to find a source for each dimension in <var> and add that
        Variable to this dictionary or to <to_dict>, if passed.
        Dimension variables which are found but whose Source is in
        <ignore_sources> are not added to this dictionary.
        Return an error string on failure."""

        err_ret = ''
        ctx = ''
        vdims = var.get_dim_stdnames(include_constants=False)
        for dimname in vdims:
            if to_dict:
                present = to_dict.find_variable(standard_name=dimname,
                                                any_scope=False)
            else:
                present = None
            # end if
            if not present:
                present = self.find_variable(standard_name=dimname,
                                             any_scope=False)
            # end if
            if not present:
                dvar = self.find_variable(standard_name=dimname, any_scope=True)
                if dvar and (dvar.source.type not in ignore_sources):
                    if to_dict:
                        print('   add_variable_dimensions():')
                        to_dict.add_variable(dvar, self.__run_env,
                                             exists_ok=True,
                                             adjust_intent=adjust_intent)
                    else:
                        self.add_variable(dvar, self.__run_env, exists_ok=True,
                                          adjust_intent=adjust_intent)
                    # end if
                else:
                    if err_ret:
                        err_ret += '\n'
                    else:
                        ctx = context_string(var.context)
                    # end if
                    err_ret += "{}: ".format(self.name)
                    err_ret += "Cannot find variable for dimension, {}, of {}{}"
                    vstdname = var.get_prop_value('standard_name')
                    err_ret = err_ret.format(dimname, vstdname, ctx)
                    if dvar:
                        err_ret += "\nFound {} from excluded source, '{}'{}"
                        lname = dvar.get_prop_value('local_name')
                        dctx = context_string(dvar.context)
                        err_ret = err_ret.format(lname, dvar.source.type, dctx)
                    # end if
                # end if
            # end if
        # end for
        return err_ret

    def find_variable(self, standard_name=None, source_var=None,
                      any_scope=True, clone=None,
                      search_call_list=False, loop_subst=False):
        """Attempt to return the variable matching <standard_name>.
        if <standard_name> is None, the standard name from <source_var> is used.
        It is an error to pass both <standard_name> and <source_var> if
        the standard name of <source_var> is not the same as <standard_name>.
        If <any_scope> is True, search parent scopes if not in current scope.
        If the variable is not found and <clone> is not None, add a clone of
        <clone> to this dictionary.
        If the variable is not found and <clone> is None, return None.
        <search_call_list> and <loop_subst> are not used in this base class
        but are included to provide a consistent interface.
        """
        if standard_name is None:
            if source_var is None:
                emsg = "One of <standard_name> or <source_var> must be passed."
                raise ParseInternalError(emsg)
            # end if
            standard_name = source_var.get_prop_value('standard_name')
        elif source_var is not None:
            stest = source_var.get_prop_value('standard_name')
            if stest != standard_name:
                emsg = ("<standard_name> and <source_var> must match " +
                        "if both are passed.")
                raise ParseInternalError(emsg)
            # end if
        # end if
        if standard_name in CCPP_CONSTANT_VARS:
            var = CCPP_CONSTANT_VARS[standard_name]
        elif standard_name in self:
            var = self[standard_name]
        elif any_scope and (self.__parent_dict is not None):
            src_clist = search_call_list
            var = self.__parent_dict.find_variable(standard_name=standard_name,
                                                   source_var=source_var,
                                                   any_scope=any_scope,
                                                   clone=clone,
                                                   search_call_list=src_clist,
                                                   loop_subst=loop_subst)
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
        if lname in self.__local_names:
            stdname = self.__local_names[lname]
            pvar = self.find_variable(standard_name=stdname, any_scope=False)
            if not pvar:
                emsg = 'VarDictionary {} should have standard_name, {}, '
                emsg += 'based on local_name {}'
                raise ParseInternalError(emsg.format(self.name,
                                                     stdname, local_name))
            # end if (no else, pvar is fine)
        elif any_scope and (self.__parent_dict is not None):
            pvar = self.__parent_dict.find_local_name(local_name,
                                                      any_scope=any_scope)
        # end if
        return pvar

    def find_error_variables(self, any_scope=False, clone_as_out=False):
        """Find and return a consistent set of error variables in this
        dictionary.
        First, attempt to find the set of errflg and errmsg.
        Currently, there is no alternative but it will be inserted here.
        If a consistent set is not found, return an empty list.
        """
        err_vars = list()
        # Look for the combo of errflg and errmsg
        errflg = self.find_variable(standard_name="ccpp_error_code",
                                    any_scope=any_scope)
        errmsg = self.find_variable(standard_name="ccpp_error_message",
                                    any_scope=any_scope)
        if (errflg is not None) and (errmsg is not None):
            if clone_as_out:
                eout = errmsg.get_prop_value('intent')
                if eout != 'out':
                    subst_dict = {'intent':'out'}
                    errmsg = errmsg.clone(subst_dict)
                # end if
            # end if
            err_vars.append(errmsg)
            if clone_as_out:
                eout = errflg.get_prop_value('intent')
                if eout != 'out':
                    subst_dict = {'intent':'out'}
                    errflg = errflg.clone(subst_dict)
                # end if
            # end if
            err_vars.append(errflg)
        # end if
        return err_vars

    def add_sub_scope(self, sub_dict):
        """Add a child dictionary to enable traversal"""
        self.__sub_dicts.append(sub_dict)

    def sub_dictionaries(self):
        """Return a list of this dictionary's sub-dictionaries"""
        return list(self.__sub_dicts)

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
            var = self.find_variable(standard_name=standard_name,
                                     any_scope=False)
            if self.include_var_in_list(var, std_vars=std_vars,
                                        loop_vars=loop_vars, consts=consts):
                self[standard_name].write_def(outfile, indent, self,
                                              dummy=dummy)
            # end if
        # end for

    def merge(self, other_dict, run_env):
        """Add new entries from <other_dict>"""
        for ovar in other_dict.variable_list():
            print('   merge():')
            self.add_variable(ovar, run_env)
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
        srepr = super().__repr__()
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
    def unit_cnv_match(cls, hunits, var):
        """Return VarUnitConv if <hunits> and <var> require unit
        conversion, otherwise, return None"""
        ucmatch = {"from": '' ,"to": ''}
        function_name_from = '{0}__to__{1}'.format(hunits, var.get_prop_value('units'))
        function_name_to   = '{0}__to__{1}'.format(var.get_prop_value('units'), hunits)

        try:
            function_from   = getattr(unit_conversion, function_name_from)
            ucmatch["from"] = VarUnitConv(function_from(),key="from",
                                          local_name=var.get_prop_value('local_name'),
                                          kind=var.get_prop_value('kind'))
        except:
            ucmatch["from"] = None
        try:
            if (var.get_prop_value('intent') != 'in'):
                function_to   = getattr(unit_conversion, function_name_to)
                ucmatch["to"] = VarUnitConv(function_to(),key="to",
                                            local_name=var.get_prop_value('local_name'),
                                            kind=var.get_prop_value('kind'))
        except:
            ucmatch["to"]   = None

        return ucmatch

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
                svar = self.find_variable(standard_name=ssubst, any_scope=False)
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
            dict_var = self.find_variable(standard_name=standard_name,
                                          any_scope=any_scope)
            if dict_var is not None:
                var_one = CCPP_CONSTANT_VARS['ccpp_constant_one']
                my_var = (var_one, dict_var)
                if self.__run_env.logger is not None:
                    lstr = "loop_subst: found {}{}"
                    logger_str = lstr.format(standard_name,
                                             context_string(context))
                # end if
            else:
                my_vars = [self.find_variable(standard_name=x,
                                              any_scope=any_scope)
                           for x in loop_var]
                if None not in my_vars:
                    my_var = tuple(my_vars)
                    if self.__run_env.logger is not None:
                        names = [x.get_prop_value('local_name')
                                 for x in my_vars]
                        lstr = "loop_subst: {} ==> ({}){}"
                        logger_str = lstr.format(standard_name,
                                                 ', '.join(names),
                                                 context_string(context))
                    # end if
                else:
                    if self.__run_env.logger is not None:
                        lstr = "loop_subst: {} ==> (??) FAILED{}"
                        logger_str = lstr.format(standard_name,
                                                 context_string(context))
                    # end if
                    my_var = None
                # end if
            # end if
        else:
            if self.__run_env.logger is not None:
                lstr = "loop_subst: {} is not a loop variable{}"
                logger_str = lstr.format(standard_name,
                                         context_string(context))
            # end if
            my_var = None
        # end if
        if logger_str is not None:
            self.__run_env.logger.debug(logger_str)
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
        varlist = [x for x in self.__local_names.keys() if var_prefix in x]
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
CCPP_CONSTANT_VARS =                                                         \
    VarDictionary('CCPP_CONSTANT_VARS', _MVAR_DUMMY_RUN_ENV,
                  variables=[ccpp_standard_var('ccpp_constant_one', 'module',
                                               _MVAR_DUMMY_RUN_ENV)])

###############################################################################
if __name__ == "__main__":
    # pylint: disable=ungrouped-imports
    import doctest
    import sys
    # pylint: enable=ungrouped-imports
    fail, _ = doctest.testmod()
    sys.exit(fail)
# end if
