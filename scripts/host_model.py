#!/usr/bin/env python

"""
Parse a host-model registry XML file and return the captured variables.
"""

# Python library imports
from __future__ import print_function
import sys
import os
import os.path
import re
import subprocess
import xml.etree.ElementTree as ET
# CCPP framework imports
from metavar import Var, VarDictionary
from ddt_library import VarDDT, DDTLibrary
from parse_tools import ParseSource, ParseContext, CCPPError, ParseInternalError
from parse_tools import read_xml_file, validate_xml_file, find_schema_version
from parse_tools import context_string, check_fortran_intrinsic, FORTRAN_ID

###############################################################################
class HostModel(VarDictionary):
    "Class to hold the data from a host model"

    def __init__(self, meta_headers, name_in, logger):
        self._name = name_in
        self._var_locations = {} # Local name to module map
        self._loop_vars = None   # Loop control vars in interface calls
        # First, process DDT headers
        self._ddt_lib = DDTLibrary('{}_ddts'.format(self.name),
                                   ddts=[d for d in meta_headers
                                         if d.header_type == 'ddt'],
                                   logger=logger)
        self._ddt_dict = VarDictionary("{}_ddt_vars".format(self.name),
                                       logger=logger)
        # Now, process the code headers by type
        varlist = list()
        for header in [h for h in meta_headers if h.header_type != 'ddt']:
            title = header.title
            if logger is not None:
                msg = 'Adding {} {} to host model'
                logger.debug(msg.format(header.header_type, title))
            # End if
            if header.header_type == 'module':
                varlist.extend(header.variable_list())
                # Set the variable modules
                modname = header.title
                for var in header.variable_list():
                    lname = var.get_prop_value('local_name')
                    self._var_locations[lname] = modname
                    self._ddt_lib.check_ddt_type(var, header, lname=lname)
                    if var.is_ddt():
                        self._ddt_lib.collect_ddt_fields(self._ddt_dict,
                                                         var, logger=None)
                    # End if
                # End for
            elif header.header_type == 'host':
                if self._name is None:
                    # Grab the first host name we see
                    self._name = header.name
                # End if
                varlist.extend(header.variable_list())
                for var in header.variable_list():
                    self._ddt_lib.check_ddt_type(var, header)
                    if var.is_ddt():
                        self._ddt_lib.collect_ddt_fields(self._ddt_dict,
                                                         var, logger=None)
                    # End if
                # End for
                loop_vars = header.variable_list(std_vars=False,
                                                 loop_vars=True, consts=False)
                if len(loop_vars) > 0:
                    # loop_vars are part of the host-model interface call
                    # at run time. As such, they override the host-model
                    # array dimensions.
                    self._loop_vars = VarDictionary(self.name)
                # End if
                for hvar in loop_vars:
                    std_name = hvar.get_prop_value('standard_name')
                    if std_name in self._loop_vars:
                        ovar = self._loop_vars[std_name]
                        ctx1 = context_string(ovar.context)
                        ctx2 = context_string(hvar.context)
                        lname1 = ovar.get_prop_value('local_name')
                        lname2 = hvar.get_prop_value('local_name')
                        errmsg = ("Duplicate host loop var for {n}:\n"
                                  "  Dup:  {l1}{c1}\n  Orig: {l2}{c2}")
                        raise CCPPError(errmsg.format(n=self.name,
                                                      l1=lname1,c1=ctx1,
                                                      l2=lname2,c2=ctx2))
                    else:
                        self._loop_vars.add_variable(hvar)
                    # End if
                # End for
            else:
                errmsg = "Invalid host model metadata header, {} ({}){}"
                ctx = context_string(header.context)
                raise CCPPError(errmsg.format(header.title,
                                              header.header_type, ctx))
            # End if
        # End while
        if self.name is None:
            errmsg = 'No name found for host model, add a host metadata entry'
            raise CCPPError(errmsg)
        # Initialize variable dictionary
        super(HostModel, self).__init__(self.name, variables=varlist,
                                        logger=logger)

    @property
    def name(self):
        'Return the host model name'
        return self._name

    @property
    def loop_vars(self):
        return self._loop_vars

    def argument_list(self, loop_vars=True):
        'Return a string representing the host model variable arg list'
        args = [v.call_string(self)
                for v in self.variable_list(loop_vars=loop_vars, consts=False)]
        return ', '.join(args)

    def host_variable_module(self, local_name):
        "Return the module name for a host variable"
        if local_name in self._var_locations:
            return self._var_locations[local_name]
        else:
            return None
        # End if

    def variable_locations(self):
        """Return a set of module-variable and module-type pairs.
        These represent the locations of all host model data with a listed
        source location (variables with no <module> source are omitted)."""
        varset = set()
        lnames = self.prop_list('local_name')
        for name in lnames:
            module = self.host_variable_module(name)
            if (module is not None) and (len(module) > 0):
                varset.add((module, name))
            # No else, either no module or a zero-length module name
            # End if
        # End for
        return varset

    def find_variable(self, standard_name, any_scope=False, loop_subst=False):
        """Return the host model variable matching <standard_name> or None
        If loop_subst is True, substitute a begin:end range for an extent.
        """
        my_var = super(HostModel, self).find_variable(standard_name,
                                                      any_scope=any_scope)
        if my_var is None:
            # Check our DDT library
            my_var = self._ddt_dict.find_variable(standard_name)
        # End if
        if loop_subst:
            if my_var is None:
                my_var = self.find_loop_subst(standard_name)
            # End if
            if my_var is not None:
                # If we get here, the host does not have the requested
                # variable but does have a replacement set. Create a new
                # variable to use to send to suites.
                new_name = self.new_internal_variable_name(prefix=self.name)
                new_var = my_var.clone(new_name, source_name=self.name,
                                       source_type="HOST",
                                       context=ParseContext(filename='host_model.py'))
                self.add_variable(new_var)
                my_var = new_var
            # End if
        # End if
        return my_var

    def add_host_variable_module(self, local_name, module, logger=None):
        "Add a module name location for a host variable"
        if local_name in self._var_locations:
            raise CCPPError("Host variable, {}, already located in module".format(self._var_locations[local_name]))
        else:
            if logger is not None:
                logger.debug('Adding variable, {}, from module, {}'.format(local_name, module))
            # End if
            self._var_locations[local_name] = module
        # End if

    def var_call_string(self, var, loop_vars=None):
        """Construct the actual argument string for <var> by translating
        standard names to local names. String includes array bounds.
        """
        if loop_vars is None:
            loop_vars = self.loop_vars
        elif loop_vars is False:
            loop_vars = None
        # End if
        return super(HostModel, self).var_call_string(var, loop_vars=loop_vars)

    def call_list(self, phase):
        "Return the list of variables passed by the host model to the host cap"
        hdvars = list()
        loop_vars = phase == 'run'
        for hvar in self.variable_list(loop_vars=loop_vars, consts=False):
            lname = hvar.get_prop_value('local_name')
            if self.host_variable_module(lname) is None:
                hdvars.append(hvar)
            # End if
        # End for
        return hdvars

###############################################################################

if __name__ == "__main__":
    from parse_tools import init_log, set_log_to_null
    logger = init_log('host_registry')
    set_log_to_null(logger)
    # First, run doctest
    import doctest
    doctest.testmod()
# No else:
