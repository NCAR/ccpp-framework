#!/usr/bin/env python

"""
Class and supporting code to hold all information on CCPP constituent
variables. A constituent variable is defined and maintained by the CCPP
Framework instead of the host model.
The ConstituentVarDict class contains methods to generate the necessary code
to implement this support.
"""

# Python library imports
from __future__ import print_function
# CCPP framework imports
from metavar import Var, VarDictionary


########################################################################

class ConstituentVarDict(VarDictionary):
    """A class to hold all the constituent variables for a CCPP Suite.
    Also contains methods to generate the necessary code for runtime
    allocation and support for these variables.
    """

    __const_prop_names = Var.constituent_property_names()

    def __init__(self, name, parent_dict, variables=None, logger=None):
        """Create a specialized VarDictionary for constituents.
        The main difference is functionality to allocate and support
        these variables with special functions for the host model.
        The main reason for a separate dictionary is that these are not
        proper Suite variables but will belong to the host model at run time.
        The <parent_dict> feature of the VarDictionary class is required
        because this dictionary must be connected to a host model.
        """
        super(ConstituentVarDict, self).__init__(name, variables=variables,
                                                 parent_dict=parent_dict,
                                                 logger=logger)

    def find_variable(self, standard_name=None, source_var=None,
                      any_scope=True, clone=None,
                      search_call_list=False, loop_subst=False):
        """Attempt to return the variable matching <standard_name>.
        if <standard_name> is None, the standard name from <source_var> is used.
        It is an error to pass both <standard_name> and <source_var> if
        the standard name of <source_var> is not the same as <standard_name>.
        If <any_scope> is True, search parent scopes if not in current scope.
        Note: Unlike the <VarDictionary> version of this method, the case for
              CCPP_CONSTANT_VARS is not handled -- it should have been handled
              by a lower level.
        If the variable is not found but is a constituent variable type,
           create the variable in this dictionary
        If the variable is not found and <clone> is not None, add a clone of
           <clone> to this dictionary.
        If the variable is not found and <clone> is None, return None.
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
                emsg = ("Only one of <standard_name> or <source_var> may " +
                        "be passed.")
                raise ParseInternalError(emsg)
            # end if
        # end if
        if standard_name in self:
            var = self[standard_name]
        elif any_scope and (self._parent_dict is not None):
            src_clist = search_call_list
            var = self._parent_dict.find_variable(standard_name=standard_name,
                                                  source_var=source_var,
                                                  any_scope=any_scope,
                                                  clone=clone,
                                                  search_call_list=src_clist,
                                                  loop_subst=loop_subst)
        elif ConstituentVarDict.is_constituent(source_var):
            # If we did not find the variable and it is a constituent type,
            # add a clone of <source_var> to our dictionary.
            # First, maybe do a loop substitution
            dims = source_var.get_dimensions()
            newdims = list()
            for dim in dims:
                dstdnames = dim.split(':')
                new_dnames = list()
                for dstdname in dstdnames:
                    if dstdname == 'horizontal_loop_extent':
                        new_dnames.append('horizontal_dimension')
                    elif dstdname == 'horizontal_loop_end':
                        new_dnames.append('horizontal_dimension')
                    elif dstdname == 'horizontal_loop_begin':
                        new_dnames.append('ccpp_constant_one')
                    else:
                        new_dnames.append(dstdname)
                    # end if
                # end for
                newdims.append(':'.join(new_dnames))
            # end for
            var = source_var.clone({'dimensions' : newdims}, remove_intent=True)
            self.add_variable(var)
        else:
            var = None
        # end if
        if (var is None) and isinstance(clone, Var):
            lname = clone.get_prop_value['local_name']
            new_name = self.new_internal_variable_name(prefix=lname)
            var = clone.clone(new_name)
        # end if
        return var

    def is_constituent(var):
        """Return True iff <var> is a constituent variable type.
        A constituent variable is a variable that has one or more
        constituent properties.
        """
        const_var = False
        if isinstance(var, Var):
            for prop, _ in var.var_properties():
                if prop in self.__const_prop_names:
                    const_var = True
                    break
                # end if
            # end for
        # end if
        return const_var
