#!/usr/bin/env python3

"""
Classes and supporting code to hold all information on the compatibility of
two CCPP metadata variables.
VariableProperty: Class which describes a single variable property
VarCompatObj
"""

# Python library imports
import keyword
import re
# CCPP framework imports
from conversion_tools import unit_conversion
from framework_env import CCPPFrameworkEnv
from parse_tools import check_local_name, check_fortran_type, context_string
from parse_tools import check_molar_mass
from parse_tools import FORTRAN_DP_RE, FORTRAN_SCALAR_REF_RE, fortran_list_match
from parse_tools import check_units, check_dimensions, check_cf_standard_name
from parse_tools import check_diagnostic_id, check_diagnostic_fixed
from parse_tools import check_default_value, check_valid_values
from parse_tools import ParseContext, ParseSource
from parse_tools import ParseInternalError, ParseSyntaxError, CCPPError

###############################################################################
_REAL_SUBST_RE = re.compile(r"(.*\d)p(\d.*)")
_HDIM_TEMPNAME = '_CCPP_HORIZ_DIM'

###############################################################################
# Supported horizontal dimensions (should be defined in CCPP_STANDARD_VARS)
CCPP_HORIZONTAL_DIMENSIONS = ['ccpp_constant_one:horizontal_dimension',
                              'ccpp_constant_one:horizontal_loop_extent',
                              'horizontal_loop_begin:horizontal_loop_end',
                              'horizontal_dimension', 'horizontal_loop_extent']

###############################################################################
# Supported vertical dimensions (should be defined in CCPP_STANDARD_VARS)
CCPP_VERTICAL_DIMENSIONS = ['ccpp_constant_one:vertical_layer_dimension',
                            'ccpp_constant_one:vertical_interface_dimension',
                            'vertical_layer_dimension',
                            'vertical_interface_dimension',
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
def is_horizontal_dimension(dim_name):
########################################################################
    """Return True if it is a recognized horizontal
    dimension or index, otherwise, return False
    >>> is_horizontal_dimension('horizontal_loop_extent')
    True
    >>> is_horizontal_dimension('ccpp_constant_one:horizontal_loop_extent')
    True
    >>> is_horizontal_dimension('ccpp_constant_one:horizontal_dimension')
    True
    >>> is_horizontal_dimension('horizontal_loop_begin:horizontal_loop_end')
    True
    >>> is_horizontal_dimension('horizontal_loop_begin:horizontal_loop_extent')
    False
    >>> is_horizontal_dimension('ccpp_constant_one')
    False
    """
    return dim_name in CCPP_HORIZONTAL_DIMENSIONS

########################################################################
def is_vertical_dimension(dim_name):
########################################################################
    """Return True if it is a recognized vertical
    dimension or index, otherwise, return False
    >>> is_vertical_dimension('ccpp_constant_one:vertical_layer_dimension')
    True
    >>> is_vertical_dimension('ccpp_constant_one:vertical_interface_dimension')
    True
    >>> is_vertical_dimension('vertical_layer_index')
    True
    >>> is_vertical_dimension('vertical_interface_index')
    True
    >>> is_vertical_dimension('ccpp_constant_one:vertical_layer_index')
    False
    >>> is_vertical_dimension('ccpp_constant_one:vertical_interface_index')
    False
    >>> is_vertical_dimension('horizontal_loop_extent')
    False
    """
    return dim_name in CCPP_VERTICAL_DIMENSIONS

########################################################################
def find_horizontal_dimension(dims):
########################################################################
    """Return the horizontal dimension string and location in <dims>
    or (None, -1).
    Return form is (horizontal_dimension, index) where index is the
    location of horizontal_dimension in <dims>"""
    var_hdim = None
    hindex = -1
    for index, dimname in enumerate(dims):
        if is_horizontal_dimension(dimname):
            var_hdim = dimname
            hindex = index
            break
        # end if
    # end for
    return (var_hdim, hindex)

########################################################################
def find_vertical_dimension(dims):
########################################################################
    """Return the vertical dimension string and location in <dims>
    or (None, -1).
    Return form is (vertical_dimension, index) where index is the
    location of vertical_dimension in <dims>"""
    var_vdim = None
    vindex = -1
    for index, dimname in enumerate(dims):
        if is_vertical_dimension(dimname):
            var_vdim = dimname
            vindex = index
            break
        # end if
    # end for
    return (var_vdim, vindex)

########################################################################
def standard_name_to_long_name(prop_dict, context=None):
########################################################################
    """Translate a standard_name to its default long_name
    >>> standard_name_to_long_name({'standard_name':'cloud_optical_depth_layers_from_0p55mu_to_0p99mu'})
    'Cloud optical depth layers from 0.55mu to 0.99mu'
    >>> standard_name_to_long_name({'local_name':'foo'}) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: No standard name to convert foo to long name
    >>> standard_name_to_long_name({}) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: No standard name to convert to long name
    >>> standard_name_to_long_name({'local_name':'foo'}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: No standard name to convert foo to long name, at foo.F90:4
    >>> standard_name_to_long_name({}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: No standard name to convert to long name, at foo.F90:4
    """
    # We assume that standard_name has been checked for validity
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
    >>> default_kind_val({'local_name':'foo'}) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: No type to find default kind for  foo
    >>> default_kind_val({}) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: No type to find default kind
    >>> default_kind_val({'local_name':'foo'}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: No type to find default kind for  foo, at foo.F90:4
    >>> default_kind_val({}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: No type to find default kind, at foo.F90:4
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

class DimTransform:
    """Class to represent a transformation between two variables with
       compatible dimensions.
    Compatible differences include permutations, sub-selection of the
       horizontal dimension, and the ordering of the vertical dimension.

    The "forward" transformation transforms "var1" into "var2"
       (i.e., var2 = forward_transform(var1)).
    The "reverse" transformation transforms "var2" into "var1"
       (i.e., var1 = reverse_transform(var2)).
    """

    def __init__(self, forward_permutation, reverse_permutation,
                 forward_hdim, forward_hdim_index, forward_vdim_index,
                 reverse_hdim, reverse_hdim_index, reverse_vdim_index):
        """Initialize a dimension transform object.
        <forward_permutation>: A tuple of integers with the location of the
           "var1" index for each "var2" index. That is, the first index
           for "var2" on the LHS of the forward transform is
           <forward_permutation>[0].
        <reverse_permutation>: A tuple of integers with the location of the
           "var2" index for each "var1" index. That is, the first index
           for "var1" on the LHS of the forward transform is
           <reverse_permutation>[0].
        <forward_hdim>: The name of the horizontal dimension for "var1".
           This is used to determine if an offset needs to be added to
           the forward and reverse transforms.
        <forward_hdim_index>: This is the position of the horizontal dimension
           for "var1". For instance, zero means that the horizontal axis is
           the fastest varying.
        <forward_vdim_index>: This is the position of the vertical dimension
           for "var1". For instance, zero means that the vertical axis is
           the fastest varying.
        <reverse_hdim>: The name of the horizontal dimension for "var2".
           This is used to determine if an offset needs to be added to
           the forward and reverse transforms.
        <reverse_hdim_index>: This is the position of the horizontal dimension
           for "var2". For instance, zero means that the horizontal axis is
           the fastest varying.
        <reverse_vdim_index>: This is the position of the vertical dimension
           for "var2". For instance, zero means that the vertical axis is
           the fastest varying.

        # Test that bad inputs are trapped:
        >>> DimTransform((0, 1, 2), (2, 1), 'horizontal_dimension', 0, 1,    \
                         'horizontal_dimension',                             \
                         1, 0) #doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        parse_source.ParseInternalError: Permutation mismatch, '(0, 1, 2)' and '(2, 1)'
        >>> DimTransform((2, 0, 1), (1, 2, 0), 'horizontal_dimension', 3, 2, \
                         'horizontal_dimension',                             \
                         4, 3) #doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        parse_source.ParseInternalError: forward_hdim_index (3) out of range [0, 2]
        >>> DimTransform((2, 0, 1), (1, 2, 0), 'horizontal_dimension', 0, 4, \
                         'horizontal_dimension',                             \
                         4, 3) #doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        parse_source.ParseInternalError: forward_vdim_index (4) out of range [0, 2]
        >>> DimTransform((2, 0, 1), (1, 2, 0), 'horizontal_dimension', 0, 2, \
                         'horizontal_dimension',                             \
                         4, 3) #doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        parse_source.ParseInternalError: reverse_hdim_index (4) out of range [0, 2]
        >>> DimTransform((2, 0, 1), (1, 2, 0), 'horizontal_dimension', 1, 2, \
                         'horizontal_dimension',                             \
                         0, 3) #doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        parse_source.ParseInternalError: reverse_vdim_index (3) out of range [0, 2]
        """
        # Store inputs
        if len(forward_permutation) != len(reverse_permutation):
            emsg = "Permutation mismatch, '{}' and '{}'"
            raise ParseInternalError(emsg.format(forward_permutation,
                                                 reverse_permutation))
        # end if
        self.__forward_perm = forward_permutation
        self.__reverse_perm = reverse_permutation
        if ((forward_hdim_index < 0) or
            (forward_hdim_index >= len(forward_permutation))):
            emsg = "forward_hdim_index ({}) out of range [0, {}]"
            raise ParseInternalError(emsg.format(forward_hdim_index,
                                                 len(forward_permutation)-1))
        # end if
        self.__forward_hdim_index = forward_hdim_index
        # We cannot test for negative forward_vdim_index because there may
        #     not be a vertical dimension
        if forward_vdim_index >= len(forward_permutation):
            emsg = "forward_vdim_index ({}) out of range [0, {}]"
            raise ParseInternalError(emsg.format(forward_vdim_index,
                                                 len(forward_permutation)-1))
        # end if
        self.__forward_vdim_index = forward_vdim_index
        if ((reverse_hdim_index < 0) or
            (reverse_hdim_index >= len(reverse_permutation))):
            emsg = "reverse_hdim_index ({}) out of range [0, {}]"
            raise ParseInternalError(emsg.format(reverse_hdim_index,
                                                 len(reverse_permutation)-1))
        # end if
        self.__reverse_hdim_index = reverse_hdim_index
        # We cannot test for negative reverse_vdim_index because there may
        #     not be a vertical dimension
        if reverse_vdim_index >= len(reverse_permutation):
            emsg = "reverse_vdim_index ({}) out of range [0, {}]"
            raise ParseInternalError(emsg.format(reverse_vdim_index,
                                                 len(reverse_permutation)-1))
        # end if
        self.__reverse_vdim_index = reverse_vdim_index
        # Categorize horizontal dimensions
        # v<x>_hloop is True if "var<x>" has extent "horizontal_loop_extent".
        #    The loop for these variables begins at one while variables with
        #    extent, "horizontal_dimension" begin at "horizontal_loop_begin"
        #    during the run phase.
        self.__v1_hloop = self.__is_horizontal_loop_dimension(forward_hdim)
        if ((not self.__v1_hloop) and
            (not ("horizontal_dimension" in forward_hdim))):
            emsg = "Uncategorized forward horizontal dimension, '{}'"
            raise ParseInternalError(emsg.format(forward_hdim))
        # end if
        self.__v2_hloop = self.__is_horizontal_loop_dimension(reverse_hdim)
        if ((not self.__v2_hloop) and
            (not ("horizontal_dimension" in reverse_hdim))):
            emsg = "Uncategorized reverse horizontal dimension, '{}'"
            raise ParseInternalError(emsg.format(reverse_hdim))
        # end if

    def forward_transform(self, var2_lname, indices,
                          adjust_hdim=None, flip_vdim=None):
        """Compute and return the LHS of the forward transform from "var1" to
           "var2".
        <var2_lname> is the local name of "var2".
        <indices> is a tuple of the loop indices for "var1" (i.e., "var1"
           will show up in the RHS of the transform as "var1(indices)".
        If <adjust_hdim> is not None, it should be a string containing the
           local name of the "horizontal_loop_begin" variable. This is used to
           compute the offset in the horizontal axis index between one and
           "horizontal_loop_begin" (if any). This occurs when one of the
           variables has extent "horizontal_loop_extent" and the other has
           extent "horizontal_dimension".
        If flip_vdim is not None, it should be a string containing the local
           name of the vertical extent of the vertical axis for "var1" and
           "var2" (i.e., "vertical_layer_dimension" or
           "vertical_interface_dimension").

        # Test forward transform with just horizontal adjustment
        >>> DimTransform((0, 1), (0, 1), 'horizontal_dimension', 0, 1,        \
                         'horizontal_loop_extent',                            \
                         0, 1).forward_transform("foo_lhs", ("hind", "vind"), \
                                                 adjust_hdim="col_start")
        'foo_lhs(hind-col_start+1,vind)'
        >>> DimTransform((0, 1), (0, 1), 'horizontal_loop_extent', 0, 1,      \
                         'horizontal_dimension',                              \
                         0, 1).forward_transform("foo_lhs", ("hind", "vind"), \
                         adjust_hdim="col_start")
        'foo_lhs(hind+col_start-1,vind)'

        # Test flipping vertical dimension
        >>> DimTransform((0, 1), (0, 1), 'horizontal_dimension', 0, 1,        \
                         'horizontal_dimension',                              \
                         0, 1).forward_transform("foo_lhs", ("hind", "vind"), \
                         flip_vdim="pver")
        'foo_lhs(hind,pver-vind+1)'

        # Test simple permutations
        >>> DimTransform((1, 0), (1, 0), 'horizontal_dimension', 0, 1,        \
                         'horizontal_dimension',                              \
                         1, 0).forward_transform("foo_lhs", ("hind", "vind"))
        'foo_lhs(vind,hind)'
        >>> DimTransform((2, 0, 1), (1, 2, 0), 'horizontal_dimension', 0, 2,  \
                         'horizontal_dimension',                              \
                         0, 1).forward_transform("foo_lhs",                   \
                         ("hind", "xdim", "vind"))
        'foo_lhs(vind,hind,xdim)'
        """
        v2_indices = [indices[x] for x in self.__forward_perm]
        if adjust_hdim is not None:
            if self.__v1_hloop and (not self.__v2_hloop):
                hdim = v2_indices[self.__forward_hdim_index]
                adj_str = f"{hdim}+{adjust_hdim}-1"
                v2_indices[self.__forward_hdim_index] = adj_str
            elif self.__v2_hloop and (not self.__v1_hloop):
                hdim = v2_indices[self.__forward_hdim_index]
                adj_str = f"{hdim}-{adjust_hdim}+1"
                v2_indices[self.__forward_hdim_index] = adj_str
            # end if
        # end if
        if flip_vdim is not None:
            vdim = v2_indices[self.__forward_vdim_index]
            adj_str = f"{flip_vdim}-{vdim}+1"
            v2_indices[self.__forward_vdim_index] = adj_str
        # end if
        return f"{var2_lname}({','.join(v2_indices)})"

    def reverse_transform(self, var1_lname, indices,
                          adjust_hdim=None, flip_vdim=None):
        """Compute and return the LHS of the forward transform from "var2" to
           "var1".
        <var1_lname> is the local name of "var1".
        <indices> is a tuple of the loop indices for "var2" (i.e., "var2"
           will show up in the RHS of the transform as "var2(indices)".
        If <adjust_hdim> is not None, it should be a string containing the
           local name of the "horizontal_loop_begin" variable. This is used to
           compute the offset in the horizontal axis index between one and
           "horizontal_loop_begin" (if any). This occurs when one of the
           variables has extent "horizontal_loop_extent" and the other has
           extent "horizontal_dimension".
        If flip_vdim is not None, it should be a string containing the local
           name of the vertical extent of the vertical axis for "var2" and
           "var1" (i.e., "vertical_layer_dimension" or
           "vertical_interface_dimension").

        # Test reverse transform with just horizontal adjustment
        >>> DimTransform((0, 1), (0, 1), 'horizontal_dimension', 0, 1,        \
                         'horizontal_loop_extent',                            \
                         0, 1).reverse_transform("bar_lhs", ("hind", "vind"), \
                                                 adjust_hdim="col_start")
        'bar_lhs(hind+col_start-1,vind)'
        >>> DimTransform((0, 1), (0, 1), 'horizontal_loop_extent', 0, 1,      \
                         'horizontal_dimension',                              \
                         0, 1).reverse_transform("bar_lhs", ("hind", "vind"), \
                         adjust_hdim="col_start")
        'bar_lhs(hind-col_start+1,vind)'

        # Test flipping vertical dimension
        >>> DimTransform((0, 1), (0, 1), 'horizontal_dimension', 0, 1,        \
                         'horizontal_dimension',                              \
                         0, 1).reverse_transform("bar_lhs", ("hind", "vind"), \
                         flip_vdim="pver")
        'bar_lhs(hind,pver-vind+1)'

        # Test simple permutations
        >>> DimTransform((1, 0), (1, 0), 'horizontal_dimension', 0, 1,        \
                         'horizontal_dimension',                              \
                         1, 0).reverse_transform("bar_lhs", ("hind", "vind"))
        'bar_lhs(vind,hind)'
        >>> DimTransform((2, 0, 1), (1, 2, 0), 'horizontal_dimension', 0, 2,  \
                         'horizontal_dimension',                              \
                         0, 1).reverse_transform("bar_lhs",                   \
                         ("vind", "hind", "xdim"))
        'bar_lhs(hind,xdim,vind)'
        """
        v1_indices = [indices[x] for x in self.__reverse_perm]
        if adjust_hdim is not None:
            if self.__v1_hloop and (not self.__v2_hloop):
                hdim = v1_indices[self.__reverse_hdim_index]
                adj_str = f"{hdim}-{adjust_hdim}+1"
                v1_indices[self.__reverse_hdim_index] = adj_str
            elif self.__v2_hloop and (not self.__v1_hloop):
                hdim = v1_indices[self.__reverse_hdim_index]
                adj_str = f"{hdim}+{adjust_hdim}-1"
                v1_indices[self.__reverse_hdim_index] = adj_str
            # end if
        # end if
        if flip_vdim is not None:
            vdim = v1_indices[self.__reverse_vdim_index]
            adj_str = f"{flip_vdim}-{vdim}+1"
            v1_indices[self.__reverse_vdim_index] = adj_str
        # end if
        return f"{var1_lname}({','.join(v1_indices)})"

    @staticmethod
    def __is_horizontal_loop_dimension(hdim):
        """Return True if <hdim> is a run-phase horizontal dimension"""
        return (is_horizontal_dimension(hdim) and
                ("horizontal_dimension" not in hdim))

########################################################################

class VariableProperty:
    """Class to represent a single property of a metadata header entry
    >>> VariableProperty('local_name', str) #doctest: +ELLIPSIS
    <var_props.VariableProperty object at ...>
    >>> VariableProperty('standard_name', str) #doctest: +ELLIPSIS
    <var_props.VariableProperty object at ...>
    >>> VariableProperty('long_name', str) #doctest: +ELLIPSIS
    <var_props.VariableProperty object at ...>
    >>> VariableProperty('units', str) #doctest: +ELLIPSIS
    <var_props.VariableProperty object at ...>
    >>> VariableProperty('dimensions', list) #doctest: +ELLIPSIS
    <var_props.VariableProperty object at ...>
    >>> VariableProperty('type', str) #doctest: +ELLIPSIS
    <var_props.VariableProperty object at ...>
    >>> VariableProperty('kind', str) #doctest: +ELLIPSIS
    <var_props.VariableProperty object at ...>
    >>> VariableProperty('state_variable', str, valid_values_in=['True',   'False', '.true.', '.false.' ], optional_in=True, default_in=False) #doctest: +ELLIPSIS
    <var_props.VariableProperty object at ...>
    >>> VariableProperty('intent', str, valid_values_in=['in', 'out', 'inout']) #doctest: +ELLIPSIS
    <var_props.VariableProperty object at ...>
    >>> VariableProperty('optional', str, valid_values_in=['True',   'False', '.true.', '.false.' ], optional_in=True, default_in=False) #doctest: +ELLIPSIS
    <var_props.VariableProperty object at ...>
    >>> VariableProperty('local_name', str).name
    'local_name'
    >>> VariableProperty('standard_name', str).ptype == str
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

    >>> VariableProperty('value', int, valid_values_in=[1, 2 ]).valid_value('3', error=True) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: Invalid value variable property, '3'
    >>> VariableProperty('units', str, check_fn_in=check_units).valid_value('m s-1')
    'm s-1'
    >>> VariableProperty('units', str, check_fn_in=check_units).valid_value(' ')

    >>> VariableProperty('units', str, check_fn_in=check_units).valid_value(' ', error=True) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: ' ' is not a valid unit
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('()')
    []
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x)')
    ['x']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('x')

    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x:y)')
    ['x:y']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(w:x,y:z)')
    ['w:x', 'y:z']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value(['size(foo)'])
    ['size(foo)']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(w:x,x:y:z:q)', error=True) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: 'x:y:z:q' is an invalid dimension range
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x:3y)', error=True) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: '3y' is not a valid Fortran identifier
    >>> VariableProperty('local_name', str, check_fn_in=check_local_name).valid_value('foo')
    'foo'
    >>> VariableProperty('local_name', str, check_fn_in=check_local_name).valid_value('foo(bar)')
    'foo(bar)'
    >>> VariableProperty('local_name', str, check_fn_in=check_local_name).valid_value('q(:,:,index_of_water_vapor_specific_humidity)')
    'q(:,:,index_of_water_vapor_specific_humidity)'
    >>> VariableProperty('molar_mass', float, check_fn_in=check_molar_mass).valid_value('12.1')
    12.1
    """

    __true_vals = ['t', 'true', '.true.']
    __false_vals = ['f', 'false', '.false.']

    def __init__(self, name_in, type_in, valid_values_in=None,
                 optional_in=False, default_in=None, default_fn_in=None,
                 check_fn_in=None, mult_entry_ok=False):
        """Conduct sanity checks and initialize this variable property."""
        self._name = name_in
        self._type = type_in
        if self._type not in [bool, int, list, str, float]:
            emsg = "{} has invalid VariableProperty type, '{}'"
            raise CCPPError(emsg.format(name_in, type_in))
        # end if
        self._valid_values = valid_values_in
        self._optional = optional_in
        self._default = None
        self._default_fn = None
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
        self._add_multiple_ok = mult_entry_ok

    @property
    def name(self):
        """Return the name of the property"""
        return self._name

    @property
    def ptype(self):
        """Return the type of the property"""
        return self._type

    @property
    def has_default_func(self):
        """Return True iff this variable property has a default function"""
        return self._default_fn is not None

    def get_default_val(self, prop_dict, context=None):
        """Return this variable property's default value or raise an
        exception if there is no default value or default value function."""
        if self.has_default_func:
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

    @property
    def add_multiple(self):
        """Return True iff multiple entries of this property should be
        accumulated. If False, it should either be an error or new
        instances should replace the old, however, this functionality
        must be implemented by the calling routine (e.g., Var)"""
        return self._add_multiple_ok

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
        if self.ptype is int:
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
        elif self.ptype is float:
            try:
                tval = float(test_value)
                if self._valid_values is not None:
                    if tval in self._valid_values:
                        valid_val = tval
                    else:
                        valid_val = None # i.e. pass
                    # end if
                else:
                    valid_val = tval
                # end if
            except CCPPError:
                valid_val = None
            # end try
        elif self.ptype is list:
            if isinstance(test_value, str):
                tval = fortran_list_match(test_value)
                if tval and (len(tval) == 1) and (not tval[0]):
                    # Scalar
                    tval = list()
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
        elif self.ptype is bool:
            if isinstance(test_value, str):
                if test_value.lower() in VariableProperty.__true_vals + VariableProperty.__false_vals:
                    valid_val = test_value.lower() in VariableProperty.__true_vals
                else:
                    valid_val = None # i.e., pass
                # end if
            else:
                valid_val = not not test_value # pylint: disable=unneeded-not
        elif self.ptype is str:
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

##############################################################################

class VarCompatObj:
    """Class to compare two Var objects and then answer questions about
    the compatibility of the two variables.
    There are three levels of compatibility.
    * Compatible is when two variables match in all properties so that one
        can be passed to another with no transformation.
    * Comformable is when two variables have the same information but may
        need some transformation between them. Examples are differences in
        dimension ordering, units, or kind.
    * Not Compatible is when information from one variable cannot be passed
        to the other.

     Note that character(len=*) is considered equivalent to
        character(len=<INTEGER_VALUE>)

    # Test that we can create a standard VarCompatObj object
    >>> from parse_tools import init_log, set_log_to_null
    >>> _DOCTEST_LOGGING = init_log('var_props')
    >>> set_log_to_null(_DOCTEST_LOGGING)
    >>> _DOCTEST_RUNENV = CCPPFrameworkEnv(_DOCTEST_LOGGING, \
                                       ndict={'host_files':'', \
                                              'scheme_files':'', \
                                              'suites':''}, \
                                       kind_types=["kind_phys=REAL64", \
                                                   "kind_dyn=REAL32", \
                                                   "kind_host=REAL64"])
    >>> VarCompatObj("var_stdname", "real", "kind_phys", "m", [], "var1_lname", False,\
                     "var_stdname", "real", "kind_phys", "m", [], "var2_lname", False,\
                     _DOCTEST_RUNENV) #doctest: +ELLIPSIS
    <var_props.VarCompatObj object at 0x...>

    # Test that a 2-D var with no horizontal transform works
    >>> VarCompatObj("var_stdname", "real", "kind_phys", "m", ['horizontal_dimension'], "var1_lname", False, \
                     "var_stdname", "real", "kind_phys", "m", ['horizontal_dimension'], "var2_lname", False, \
                     _DOCTEST_RUNENV) #doctest: +ELLIPSIS
    <var_props.VarCompatObj object at 0x...>

    # Test that a 2-D var with a horizontal transform works
    >>> VarCompatObj("var_stdname", "real", "kind_phys", "m", ['horizontal_dimension'],   "var1_lname", False, \
                     "var_stdname", "real", "kind_phys", "m", ['horizontal_loop_extent'], "var2_lname", False, \
                     _DOCTEST_RUNENV) #doctest: +ELLIPSIS
    <var_props.VarCompatObj object at 0x...>

    # Test that a 1-D var with no vertical transform works
    >>> VarCompatObj("var_stdname", "real", "kind_phys", "m", ['vertical_layer_dimension'], "var1_lname", False, \
                     "var_stdname", "real", "kind_phys", "m", ['vertical_layer_dimension'], "var2_lname", False, \
                     _DOCTEST_RUNENV) #doctest: +ELLIPSIS
    <var_props.VarCompatObj object at 0x...>

    # Test that a 1-D var with vertical flipping works and that it
    # produces the correct reverse transformation
    >>> VarCompatObj("var_stdname", "real", "kind_phys", "m", ['vertical_layer_dimension'], "var1_lname", False,\
                     "var_stdname", "real", "kind_phys", "m", ['vertical_layer_dimension'], "var2_lname", True, \
                     _DOCTEST_RUNENV).reverse_transform("var1_lname", "var2_lname", ('k',), ('nk-k+1',))
    'var1_lname(nk-k+1) = var2_lname(k)'

    # Test that unit conversions with a scalar var works
    >>> VarCompatObj("var_stdname", "real", "kind_phys", "Pa", [], "var1_lname", False, \
                      "var_stdname", "real", "kind_phys", "hPa", [], "var2_lname", False, \
                      _DOCTEST_RUNENV).forward_transform("var1_lname", "var2_lname", [], []) #doctest: +ELLIPSIS
    'var1_lname = 1.0E-2_kind_phys*var2_lname'

    # Test that unit conversions with a scalar var works
    >>> VarCompatObj("var_stdname", "real", "kind_phys", "Pa", [], "var1_lname", False, \
                      "var_stdname", "real", "kind_phys", "hPa", [], "var2_lname", False, \
                      _DOCTEST_RUNENV).reverse_transform("var1_lname", "var2_lname", [], []) #doctest: +ELLIPSIS
    'var1_lname = 1.0E+2_kind_phys*var2_lname'

    # Test that a 2-D var with unit conversion m->km works
    >>> VarCompatObj("var_stdname", "real", "kind_phys", "m",  ['horizontal_dimension'], "var1_lname", False, \
                     "var_stdname", "real", "kind_phys", "km", ['horizontal_dimension'], "var2_lname", False, \
                     _DOCTEST_RUNENV) #doctest: +ELLIPSIS
    <var_props.VarCompatObj object at 0x...>

    # Test that a 2-D var with unit conversion m->km works and that it
    # produces the correct forward transformation
    >>> VarCompatObj("var_stdname", "real", "kind_phys", "m",  ['horizontal_dimension'], "var1_lname", False, \
                     "var_stdname", "real", "kind_phys", "km", ['horizontal_dimension'], "var2_lname", False, \
                     _DOCTEST_RUNENV).forward_transform("var1_lname", "var2_lname", 'i', 'i')
    'var1_lname(i) = 1.0E-3_kind_phys*var2_lname(i)'

    # Test that a 3-D var with unit conversion m->km and vertical flipping
    # works and that it produces the correct reverse transformation
    >>> VarCompatObj("var_stdname", "real", "kind_phys", "m", ['horizontal_dimension', 'vertical_layer_dimension'], "var1_lname", False,\
                     "var_stdname", "real", "kind_phys", "km",['horizontal_dimension', 'vertical_layer_dimension'], "var2_lname", True, \
                     _DOCTEST_RUNENV).reverse_transform("var1_lname", "var2_lname", ('i','k'), ('i','nk-k+1'))
    'var1_lname(i,nk-k+1) = 1.0E+3_kind_phys*var2_lname(i,k)'
    """

    def __init__(self, var1_stdname, var1_type, var1_kind, var1_units,
                 var1_dims, var1_lname, var1_top, var2_stdname, var2_type, var2_kind,
                 var2_units, var2_dims, var2_lname, var2_top, run_env, v1_context=None,
                 v2_context=None, is_tend=False):
        """Initialize this object with information on the equivalence and/or
           conformability of two variables.
        variable 1 is described by <var1_stdname>, <var1_type>, <var1_kind>,
           <var1_units>, <var1_dims>, <var1_lname>, <var1_top>, and <v1_context>.
        variable 2 is described by <var2_stdname>, <var2_type>, <var2_kind>,
           <var2_units>, <var2_dims>, <var2_lname>, <var2_top>, and <v2_context>.
        <run_env> is the CCPPFrameworkEnv object used here to verify kind
           equivalence or to produce kind transformations.
        <is_tend> is a flag where, if true, we are validating a tendency variable (var1)
           against it's equivalent state variable (var2)
        """
        self.__equiv = True   # No transformation required
        self.__compat = True  # Callable with transformation
        self.__stdname = var1_stdname
        self.__v1_context = v1_context
        self.__v2_context = v2_context
        self.__v1_kind = var1_kind
        self.__v2_kind = var2_kind
        # Default (null) transform information
        self.__dim_transforms = None
        self.__kind_transforms = None
        self.__unit_transforms = None
        self.has_vert_transforms = False
        incompat_reason = list()
        # First, check for fatal incompatibilities
        # If it's a tendency variable, it's assumed the standard name is of the
        #  form "tendency_of_var2_stdname"
        if not is_tend and  var1_stdname != var2_stdname:
            self.__equiv = False
            self.__compat = False
            incompat_reason.append("standard names")
        # end if
        if var1_type != var2_type:
            self.__equiv = False
            self.__compat = False
            incompat_reason.append("types")
        # end if
        # Check kind argument
        if self.__compat:
            if var1_type == 'character':
                # First, make sure we have supported character 'kind' args:
                v1_kind = self.char_kind_check(var1_kind)
                if not v1_kind:
                    ctx = context_string(v1_context)
                    emsg = "Unsupported character kind/len argument, '{}', "
                    emsg += "in {}{}"
                    incompat_reason.append(emsg.format(var1_kind,
                                                       var1_lname, ctx))
                # end if
                self.__v1_kind = None
                v2_kind = self.char_kind_check(var2_kind)
                if not v2_kind:
                    ctx = context_string(v2_context)
                    emsg = "Unsupported character kind/len argument, '{}', "
                    emsg += "in {}{}"
                    incompat_reason.append(emsg.format(var2_kind,
                                                       var2_lname, ctx))
                # end if
                self.__v2_kind = None
                # Character types have to 'match' or the variables are
                #    incompatible
                kind_eq = ((v1_kind and v2_kind) and
                           ((v1_kind == v2_kind) or
                            (((v1_kind == 'len=*') and
                              (v2_kind.startswith('len='))) or
                             (v1_kind.startswith('len=') and
                              (v2_kind == 'len=*')))))
                if not kind_eq:
                    self.__equiv = False
                    self.__compat = False
                    incompat_reason.append("character len arguments")
                # end if
            else:
                if var1_kind != var2_kind:
                    self.__kind_transforms = self._get_kind_convstrs(var1_kind,
                                                                     var2_kind,
                                                                     run_env)
                    self.__equiv = self.__kind_transforms is None
                # end if
            # end if
        # end if
        if self.__compat:
            # Only "none" units are case-insensitive
            if var1_units.lower() == 'none':
                var1_units = 'none'
            # end if
            if var2_units.lower() == 'none':
                var2_units = 'none'
            # end if
            # Check units argument
            if is_tend:
                # A tendency variable's units should be "<var2_units> s-1"
                tendency_split_units = var1_units.split('s-1')[0].strip()
                if tendency_split_units != var2_units:
                    # We don't currently support unit conversions for tendency variables
                    emsg = f"\nMismatch tendency variable units '{var1_units}'"
                    emsg += f" for variable '{var1_stdname}'."
                    emsg += " No variable transforms supported for tendencies."
                    emsg += f" Tendency units should be '{var2_units} s-1' to match state variable."
                    self.__equiv = False
                    self.__compat = False
                    incompat_reason.append(emsg)
                # end if
            elif var1_units != var2_units:
                self.__equiv = False
                # Try to find a set of unit conversions
                self.__unit_transforms = self._get_unit_convstrs(var1_units,
                                                                 var2_units)
            # end if
        # end if
        if self.__compat:
            # Check for vertical array flipping (do later)
            if var1_top != var2_top:
                self.__compat            = True
                self.has_vert_transforms = True
            # end if
        # end if
        if self.__compat:
            # Check dimensions
            if var1_dims or var2_dims:
                _, vdim_ind = find_vertical_dimension(var1_dims)
                if (var1_dims != var2_dims):
                    self.__dim_transforms = self._get_dim_transforms(var1_dims,
                                                                     var2_dims)
                    self.__compat = self.__dim_transforms is not None
                # end if
            # end if
            if not self.__compat:
                incompat_reason.append('dimensions')
            # end if
        # end if
        self.__incompat_reason = " and ".join([x for x in incompat_reason if x])

    def forward_transform(self, lvar_lname, rvar_lname, rvar_indices, lvar_indices,
                          adjust_hdim=None, flip_vdim=None):
        """Compute and return the the forward transform from "var1" to "var2".
        <lvar_lname> is the local name of "var2".
        <rvar_lname> is the local name of "var1".
        <rvar_indices> is a tuple of the loop indices for "var1" (i.e., "var1"
           will show up in the RHS of the transform as "var1(rvar_indices)".
        <lvar_indices> is a tuple of the loop indices for "var2" (i.e., "var2"
           will show up in the LHS of the transform as "var2(lvar_indices)".
        If <adjust_hdim> is not None, it should be a string containing the
           local name of the "horizontal_loop_begin" variable. This is used to
           compute the offset in the horizontal axis index between one and
           "horizontal_loop_begin" (if any). This occurs when one of the
           variables has extent "horizontal_loop_extent" and the other has
           extent "horizontal_dimension".
        If flip_vdim is not None, it should be a string containing the local
           name of the vertical extent of the vertical axis for "var1" and
           "var2" (i.e., "vertical_layer_dimension" or
           "vertical_interface_dimension").
        """
        # Dimension transform (Indices handled externally)
        if len(rvar_indices) == 0:
            rhs_term = f"{rvar_lname}"
            lhs_term = f"{lvar_lname}"
        else:
            rhs_term = f"{rvar_lname}({','.join(rvar_indices)})"
            lhs_term = f"{lvar_lname}({','.join(lvar_indices)})"
        # end if

        if self.has_kind_transforms:
            kind = self.__kind_transforms[1]
            rhs_term = f"real({rhs_term}, {kind})"
        else:
            kind = ''
        # end if
        if self.has_unit_transforms:
            if kind:
                kind = "_" + kind
            elif self.__v2_kind:
                kind = "_" + self.__v2_kind
            # end if
            rhs_term = self.__unit_transforms[0].format(var=rhs_term, kind=kind)
        # end if
        return f"{lhs_term} = {rhs_term}"

    def reverse_transform(self, lvar_lname, rvar_lname, rvar_indices, lvar_indices,
                          adjust_hdim=None, flip_vdim=None):
        """Compute and return the the reverse transform from "var2" to "var1".
        <lvar_lname> is the local name of "var1".
        <rvar_lname> is the local name of "var2".
        <rvar_indices> is a tuple of the loop indices for "var1" (i.e., "var1"
           will show up in the RHS of the transform as "var1(rvar_indices)".
        <lvar_indices> is a tuple of the loop indices for "var2" (i.e., "var2"
           will show up in the LHS of the transform as "var2(lvar_indices)".
        If <adjust_hdim> is not None, it should be a string containing the
           local name of the "horizontal_loop_begin" variable. This is used to
           compute the offset in the horizontal axis index between one and
           "horizontal_loop_begin" (if any). This occurs when one of the
           variables has extent "horizontal_loop_extent" and the other has
           extent "horizontal_dimension".
        If flip_vdim is not None, it should be a string containing the local
           name of the vertical extent of the vertical axis for "var1" and
           "var2" (i.e., "vertical_layer_dimension" or
           "vertical_interface_dimension").
        """
        # Dimension transforms (Indices handled externally)
        if len(rvar_indices) == 0:
            rhs_term = f"{rvar_lname}"
            lhs_term = f"{lvar_lname}"
        else:
            lhs_term = f"{lvar_lname}({','.join(lvar_indices)})"
            rhs_term = f"{rvar_lname}({','.join(rvar_indices)})"
        # end if

        if self.has_kind_transforms:
            kind = self.__kind_transforms[0]
            rhs_term = f"real({rhs_term}, {kind})"
        else:
            kind = ''
        # end if
        if self.has_unit_transforms:
            if kind:
                kind = "_" + kind
            elif self.__v1_kind:
                kind = "_" + self.__v1_kind
            # end if
            rhs_term = self.__unit_transforms[1].format(var=rhs_term, kind=kind)
        # end if
        return f"{lhs_term} = {rhs_term}"

    def _get_kind_convstrs(self, var1_kind, var2_kind, run_env):
        """Attempt to determine if no transformation is required (i.e., if
           <var1_kind> and <var2_kind> will be the same at runtime. If so,
           return None.
        If a conversion is required, return a tuple with the two kinds,
           i.e., (var1_kind, var2_kind).

        # Initial setup
        >>> from parse_tools import init_log, set_log_to_null
        >>> _DOCTEST_LOGGING = init_log('var_props')
        >>> set_log_to_null(_DOCTEST_LOGGING)
        >>> _DOCTEST_RUNENV = CCPPFrameworkEnv(_DOCTEST_LOGGING, \
                                               ndict={'host_files':'', \
                                                      'scheme_files':'', \
                                                      'suites':''}, \
                                               kind_types=["kind_phys=REAL64", \
                                                           "kind_dyn=REAL32", \
                                                           "kind_host=REAL64"])
        >>> _DOCTEST_CONTEXT1 = ParseContext(linenum=3, filename='foo.F90')
        >>> _DOCTEST_CONTEXT2 = ParseContext(linenum=5, filename='bar.F90')
        >>> _DOCTEST_VCOMPAT = VarCompatObj("var_stdname", "real", "kind_phys", \
                                            "m", [], "var1_lname", False, "var_stdname", \
                                            "real", "kind_phys", "m", [], \
                                            "var2_lname", False, _DOCTEST_RUNENV, \
                                            v1_context=_DOCTEST_CONTEXT1, \
                                            v2_context=_DOCTEST_CONTEXT2)

        # Try some kind conversions
        >>> _DOCTEST_VCOMPAT._get_kind_convstrs('kind_phys', 'kind_dyn',   \
                                                _DOCTEST_RUNENV)
        ('kind_phys', 'kind_dyn')
        >>> _DOCTEST_VCOMPAT._get_kind_convstrs('kind_phys', 'REAL32',     \
                                                _DOCTEST_RUNENV)
        ('kind_phys', 'REAL32')

        # Try some non-conversions
        >>> _DOCTEST_VCOMPAT._get_kind_convstrs('kind_phys', 'kind_host',  \
                                                _DOCTEST_RUNENV)

        >>> _DOCTEST_VCOMPAT._get_kind_convstrs('REAL64', 'kind_host',     \
                                                _DOCTEST_RUNENV)

        """
        kind1 = run_env.kind_spec(var1_kind)
        if kind1 is None:
            kind1 = var1_kind
        # end if
        kind2 = run_env.kind_spec(var2_kind)
        if kind2 is None:
            kind2 = var2_kind
        # end if
        if kind1 != kind2:
            return (var1_kind, var2_kind)
        # end if
        return None

    def _get_unit_convstrs(self, var1_units, var2_units):
        """Attempt to retrieve the forward and reverse unit transformations
        for transforming a variable in <var1_units> to / from a variable in
        <var2_units>.

        # Initial setup
        >>> from parse_tools import init_log, set_log_to_null
        >>> _DOCTEST_LOGGING = init_log('var_props')
        >>> set_log_to_null(_DOCTEST_LOGGING)
        >>> _DOCTEST_RUNENV = CCPPFrameworkEnv(_DOCTEST_LOGGING, \
                                               ndict={'host_files':'', \
                                                      'scheme_files':'', \
                                                      'suites':''}, \
                                               kind_types=["kind_phys=REAL64", \
                                                           "kind_dyn=REAL32", \
                                                           "kind_host=REAL64"])
        >>> _DOCTEST_CONTEXT1 = ParseContext(linenum=3, filename='foo.F90')
        >>> _DOCTEST_CONTEXT2 = ParseContext(linenum=5, filename='bar.F90')
        >>> _DOCTEST_VCOMPAT = VarCompatObj("var_stdname", "real", "kind_phys", \
                                            "m", [], "var1_lname", False, "var_stdname", \
                                            "real", "kind_phys", "m", [], \
                                            "var2_lname", False, _DOCTEST_RUNENV, \
                                            v1_context=_DOCTEST_CONTEXT1, \
                                            v2_context=_DOCTEST_CONTEXT2)

        # Try some working unit transforms
        >>> _DOCTEST_VCOMPAT._get_unit_convstrs('m', 'mm')
        ('1.0E+3{kind}*{var}', '1.0E-3{kind}*{var}')
        >>> _DOCTEST_VCOMPAT._get_unit_convstrs('kg kg-1', 'g kg-1')
        ('1.0E+3{kind}*{var}', '1.0E-3{kind}*{var}')
        >>> _DOCTEST_VCOMPAT._get_unit_convstrs('C', 'K')
        ('{var}+273.15{kind}', '{var}-273.15{kind}')

        # Try an invalid conversion
        >>> _DOCTEST_VCOMPAT._get_unit_convstrs('1', 'none') #doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        parse_source.ParseSyntaxError: Unsupported unit conversion, '1' to 'none' for 'var_stdname'

        # Try an unsupported conversion
        >>> _DOCTEST_VCOMPAT._get_unit_convstrs('C', 'm') #doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        parse_source.ParseSyntaxError: Unsupported unit conversion, 'C' to 'm' for 'var_stdname'
        """
        u1_str = self.units_to_string(var1_units, self.__v1_context)
        u2_str = self.units_to_string(var2_units, self.__v2_context)
        unit_conv_str = "{0}__to__{1}".format(u1_str, u2_str)
        try:
            forward_transform = getattr(unit_conversion, unit_conv_str)()
        except AttributeError:
            emsg = "Unsupported unit conversion, '{}' to '{}' for '{}'"
            raise ParseSyntaxError(emsg.format(var1_units, var2_units,
                                               self.__stdname,
                                               context=self.__v2_context))
        # end if
        unit_conv_str = "{0}__to__{1}".format(u2_str, u1_str)
        try:
            reverse_transform = getattr(unit_conversion, unit_conv_str)()
        except AttributeError:
            emsg = "Unsupported unit conversion, '{}' to '{}' for '{}'"
            raise ParseSyntaxError(emsg.format(var2_units, var1_units,
                                               self.__stdname,
                                               context=self.__v1_context))
        # end if
        return (forward_transform, reverse_transform)

    def _get_dim_transforms(self, var1_dims, var2_dims):
        """Attempt to find forward and reverse permutations for transforming a
           variable with shape, <v1_dims>, to / from a variable with shape,
           <v2_dims>.
        Return the permutations, or None.
        The forward dimension transformation is a permutation of the indices of
           the first variable to the second.
        The reverse dimension transformation is a permutation of the indices of
           the second variable to the first.

        # Initial setup
        >>> from parse_tools import init_log, set_log_to_null
        >>> _DOCTEST_LOGGING = init_log('var_props')
        >>> set_log_to_null(_DOCTEST_LOGGING)
        >>> _DOCTEST_RUNENV = CCPPFrameworkEnv(_DOCTEST_LOGGING, \
                                       ndict={'host_files':'', \
                                              'scheme_files':'', \
                                              'suites':''}, \
                                       kind_types=["kind_phys=REAL64", \
                                                   "kind_dyn=REAL32", \
                                                   "kind_host=REAL64"])
        >>> _DOCTEST_CONTEXT1 = ParseContext(linenum=3, filename='foo.F90')
        >>> _DOCTEST_CONTEXT2 = ParseContext(linenum=5, filename='bar.F90')
        >>> _DOCTEST_VCOMPAT = VarCompatObj("var_stdname", "real", "kind_phys", \
                                    "m", [], "var1_lname", False, "var_stdname", \
                                    "real", "kind_phys", "m", [], \
                                    "var2_lname", False, _DOCTEST_RUNENV, \
                                    v1_context=_DOCTEST_CONTEXT1, \
                                    v2_context=_DOCTEST_CONTEXT2)

        # Test simple permutations
        >>> _DOCTEST_VCOMPAT._get_dim_transforms(['horizontal_dimension',      \
                                                  'vertical_layer_dimension'], \
                                                 ['vertical_layer_dimension',  \
                                                  'horizontal_dimension'])     \
        #doctest: +ELLIPSIS
        <var_props.DimTransform object at 0x...>
        >>> _DOCTEST_VCOMPAT._get_dim_transforms(['horizontal_dimension',      \
                                                  'vertical_layer_dimension',  \
                                                  'xdim'],                     \
                                                 ['vertical_layer_dimension',  \
                                                  'horizontal_dimension',      \
                                                  'xdim']) #doctest: +ELLIPSIS
        <var_props.DimTransform object at 0x...>
        >>> _DOCTEST_VCOMPAT._get_dim_transforms(['horizontal_dimension',      \
                                                  'vertical_layer_dimension',  \
                                                  'xdim'],                     \
                                                 ['xdim',                      \
                                                  'horizontal_dimension',      \
                                                  'vertical_layer_dimension']) \
        #doctest: +ELLIPSIS
        <var_props.DimTransform object at 0x...>

        # Test some mismatch sets
        >>> _DOCTEST_VCOMPAT._get_dim_transforms(['horizontal_dimension',      \
                                                  'vertical_layer_dimension',  \
                                                  'xdim'],                     \
                                                 ['horizontal_dimension',      \
                                                  'vertical_layer_dimension']) \

        >>> _DOCTEST_VCOMPAT._get_dim_transforms(['horizontal_dimension',      \
                                                  'vertical_layer_dimension',  \
                                                  'xdim'],                     \
                                                 ['horizontal_dimension',      \
                                                  'vertical_layer_dimension',  \
                                                  'ydim'])

        """
        transforms = None
        v1_dims = self.__regularize_dimensions(var1_dims)
        v2_dims = self.__regularize_dimensions(var2_dims)
        if v1_dims != v2_dims:
            self.__equiv = False
        # end if
        # Is v2 a permutation of v1?
        if len(v1_dims) == len(v2_dims):
            v1_set = sorted(v1_dims)
            v2_set = sorted(v2_dims)
            if v1_set == v2_set:
                forward_permutation = list()
                reverse_permutation = [None] * len(v1_dims)
                forward_hdim = ''
                forward_hdim_index = -1
                forward_vdim_index = -1
                reverse_hdim = ''
                reverse_hdim_index = -1
                reverse_vdim_index = -1
                for v2index, v2dim in enumerate(v2_dims):
                    for v1index, v1dim in enumerate(v1_dims):
                        if v1dim == v2dim:
                            # Add check for repeated indices
                            if v1index not in forward_permutation:
                                forward_permutation.append(v1index)
                                reverse_permutation[v1index] = v2index
                                if is_horizontal_dimension(var1_dims[v1index]):
                                    forward_hdim = var1_dims[v1index]
                                    forward_hdim_index = v1index
                                    reverse_hdim = var2_dims[v2index]
                                    reverse_hdim_index = v2index
                                elif is_vertical_dimension(var1_dims[v1index]):
                                    forward_vdim_index = v1index
                                    reverse_vdim_index = v2index
                                # end if
                                break
                            # end if
                        # end if (hope there is a repeated dimension)
                    # end for
                # end for
                if len(forward_permutation) != len(v1_dims):
                    emsg = "Bad dimension handling, '{}' and '{}'"
                    raise ParseInternalError(emsg.format(var1_dims, var2_dims))
                # end if
                transforms = DimTransform(forward_permutation,
                                          reverse_permutation,
                                          forward_hdim, forward_hdim_index,
                                          forward_vdim_index,
                                          reverse_hdim, reverse_hdim_index,
                                          reverse_vdim_index)
            # end if
        # end if
        return transforms

    @staticmethod
    def char_kind_check(kind_str):
        """If <kind_str> is a supported character 'kind' argument, return its
        standardized form, otherwise return False.
        """
        kind_ok = False
        if isinstance(kind_str, str):
            # Character allows both len and kind but we only support len
            kentries = [x.strip() for x in kind_str.split(',') if x.strip()]
            if len(kentries) == 1:
                if kentries[0][0:4].lower() == 'len=':
                    kind_ok = True
                # end if (no else, kind_ok already False)
            # end if (no else, kind_ok already False)
        # end if (no else, kind_ok already False)
        return kind_ok

    def units_to_string(self, units, context=None):
        """Replace variable unit description with string that is a legal
        Python identifier.
        If the resulting string is a Python keyword, raise an exception."""
        # Replace each whitespace with an underscore
        string = units.replace(" ","_")
        # Replace each minus sign with '_minus_'
        string = string.replace("-","_minus_")
        # Replace each plus sign with '_plus_'
        string = string.replace("+","_plus_")
        # "1" is a valid unit
        if string == "1":
            string = "one"
        # end if
        # Test that the resulting string is a valid Python identifier
        if not string.isidentifier():
            emsg = "Unsupported units entry for {}, '{}'{}"
            ctx = context_string(context)
            raise ParseSyntaxError(emsg.format(self.__stdname, units ,ctx))
        # end if
        # Test that the resulting string is NOT a Python keyword
        if keyword.iskeyword(string):
            emsg = "Invalid units entry, '{}', Python identifier"
            raise ParseSyntaxError(emsg.format(units),
                                   context=context)
        # end if
        return string

    @staticmethod
    def __regularize_dimensions(dims):
        """Regularize <dims> by substituting a standin for any horizontal
        dimension description (e.g., 'ccpp_constant_one:horizontal_loop_extent',
        'horizontal_loop_begin:horizontal_loop_end'). Also, regularize all
        other dimensions by adding 'ccpp_constant_one' to any singleton
        dimension.
        Return the regularized dimensions.
        """
        new_dims = list()
        for dim in dims:
            if is_horizontal_dimension(dim):
                new_dims.append(_HDIM_TEMPNAME)
            elif ':' not in dim:
                new_dims.append('ccpp_constant_one:' + dim)
            else:
                new_dims.append(dim)
            # end if
        # end for
        return new_dims

    @property
    def incompat_reason(self):
        """Return the reason(s) the two variables are incompatible (or an
           empty string)"""
        return self.__incompat_reason

    @property
    def equiv(self):
        """Return True if this object describes two Var objects which are
        equivalent (i.e., no transformation required to pass one to the other).
        """
        return self.__equiv

    @property
    def compat(self):
        """Return True if this object describes two Var objects which are
        compatible (i.e., the values from one can be transferred to the other
        via the transformation(s) described in the object).
        """
        return self.__compat

    @property
    def has_dim_transforms(self):
        """Return True if this object has dimension transformations.
        The dimension transformations is a tuple for forward and reverse
        transformation.
        The forward dimension transformation is a permutation of the indices of
           the first variable to the second.
        The reverse dimension transformation is a permutation of the indices of
           the second variable to the first.
        """
        return self.__dim_transforms is not None

    @property
    def has_kind_transforms(self):
        """Return True if this object has the kind transformation.
        The kind transformation is a tuple containing the forward and reverse
           kind transformations.
        The forward kind transformation is a string representation of the
           kind of the second variable.
        The reverse kind transformation is a string representation of the
           kind of the first variable.
        """
        return self.__kind_transforms is not None

    @property
    def has_unit_transforms(self):
        """Return True if this object has the unit transformations.
        The unit transformations is a tuple with forward and reverse unit
           transformations.
        The forward unit transformation is a string representation of the
           equation to transform the first variable into the units of the second
        The reverse unit transformation is a string representation of the
           equation to transform the second variable into the units of the first
        Each unit transform is a string which can be formatted with <kind>
           and <var> arguments to produce code to transform one variable into
           the correct units of the other.
        """
        return self.__unit_transforms is not None

    def __bool__(self):
        """Return True if this object describes two Var objects which are
        equivalent (i.e., no transformation required to pass one to the other).
        """
        return self.equiv

###############################################################################
