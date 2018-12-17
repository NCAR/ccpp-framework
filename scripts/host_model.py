#!/usr/bin/env python

"""
Parse a host-model registry XML file and return the captured variables.
"""

# Python library imports
from __future__ import print_function
import sys
import os
import os.path
import subprocess
import xml.etree.ElementTree as ET
# CCPP framework imports
from metavar import Var, VarDictionary
from parse_tools import ParseSource, ParseContext, CCPPError
from parse_tools import read_xml_file, validate_xml_file, find_schema_version

###############################################################################
class HostModel(object):
    "Class to hold the data from a host model"

    def __init__(self, name, ddt_defs, var_locations, variables):
        self._name = name
        self._ddt_defs = ddt_defs
        self._var_locations = var_locations
        self._variables = variables

    @property
    def name(self):
        'Return the host model name'
        return self._name

    def argument_list(self):
        'Return a string representing the host model variable arg list'
        args = self._variables.prop_list('local_name')
        return ', '.join(args)

    def variable_list(self):
        "Return an ordered list of the host model's variables"
        return self._variables.variable_list()

    def add_ddt_defs(new_ddt_defs):
        "Add new DDT metadata definitions to model"
        if new_ddt_defs is not None:
            self._ddt_defs.extend(new_ddt_defs)
        # End if

    def add_variables(new_variables):
        "Add new variables definitions to model"
        if new_variables is not None:
            self._variables.merge(new_variables)
        # End if

    def variable_locations(self):
        """Return a set of module-variable and module-type pairs.
        These represent the locations of all host model data."""
        varset = set()
        mods = self._variables.prop_list('module')
        names = self._variables.prop_list('local_name')
        for item in zip(mods, names):
            varset.add(item)
        # End for
        return varset

    def host_variable_module(self, local_name):
        "Return the module name for a host variable"
        if local_name in self._var_locations:
            return self._var_locations[local_name]
        else:
            return None
        # End if

    def add_host_variable_module(self, local_name, module):
        "Add a module name location for a host variable"
        if local_name in self._var_locations:
            raise CCPPError("Host variable, {}, already located in module".format(self._var_locations[local_name]))
        else:
            self._var_locations[local_name] = module
        # End if

    ###########################################################################
    @classmethod
    def parse_host_registry(cls, filename, logger, ddt_defs, host_model=None):
        variables = VarDictionary(logger=logger)
        host_variables = {}
        tree, root = read_xml_file(filename, logger=logger)
        # We do not have line number information for the XML file
        context = ParseContext(filename=filename)
        version = find_schema_version(root, logger=logger)
        res = validate_xml_file(filename, 'host_registry', version, logger=logger)
        if not res:
            abort("Invalid host registry file, '{}'".format(filename), logger)
        # End if
        host_name = root.get('name')
        logger.info("Reading host model registry for {}".format(host_name))
        for child in root:
            if (child.tag == 'dimension') or (child.tag == 'variable'):
                prop_dict = child.attrib
                vname = prop_dict['local_name']
                for var_prop in child:
                    if var_prop.tag == 'module':
                        # We need to keep track of where host variables come from
                        host_variables[vname] = var_prop.text
                        logger.debug("{} defined in {}".format(vname, host_variables[vname]))
                    # End if
                    if var_prop.tag == 'type':
                        prop_dict['type'] = var_prop.text
                        if 'kind' in var_prop.attrib:
                            prop_dict['kind'] = var_prop.get('kind')
                        # End if
                        if 'units' in var_prop.attrib:
                            prop_dict['units'] = var_prop.get('units')
                        # End if
                    else:
                        # These elements should just have text, no attributes
                        if len(var_prop.attrib) > 0:
                            abort("Bad variable property, {} for variable, {}".format(var_prop.tag, vname), logger)
                        else:
                            prop_dict[var_prop.tag] = var_prop.text
                        # End if
                    # End if
                # End for
                ##XXgoldyXX: Try to make the XSD cough these up
                if (child.tag == 'dimension') and ('type' not in prop_dict):
                    prop_dict['type'] = "integer"
                # End if
                if (child.tag == 'dimension') and ('units' not in prop_dict):
                    prop_dict['units'] = "1"
                # End if
                if (child.tag == 'dimension') and ('kind' not in prop_dict):
                    prop_dict['kind'] = ""
                # End if
                if (child.tag == 'dimension') and ('dimensions' not in prop_dict):
                    prop_dict['dimensions'] = "()"
                # End if
                newvar = Var(prop_dict, ParseSource(vname, 'REGISTRY', context))
                variables.add_variable(newvar)
            # End if
        # End for
        # Now that we have all the info from this XML, create a HostModel
        # object or add to the one that is there:
        if host_model is None:
            host_model = HostModel(host_name, ddt_defs, host_variables, variables)
        elif host_name != host_model.name:
            raise CCPPError('Inconsistent host model names, {} and {}'.format(host_name, host_model.name))
        else:
            host_model.add_variables(variables)
            host_model.add_ddt_defs(ddt_defs)
            for var in host_variables.keys():
                host_model.add_host_variable_module(var, host_variables[var])
            # End for
        # End if
        return host_model

###############################################################################

if __name__ == "__main__":
    from parse_tools import initLog, setLogToNull
    logger = initLog('host_registry')
    setLogToNull(logger)
    # First, run doctest
    import doctest
    doctest.testmod()
# No else:
