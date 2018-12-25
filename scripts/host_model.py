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
from parse_tools import ParseSource, ParseContext, CCPPError
from parse_tools import read_xml_file, validate_xml_file, find_schema_version
from parse_tools import check_fortran_intrinsic, FORTRAN_ID

###############################################################################
class HostModel(VarDictionary):
    "Class to hold the data from a host model"

    loop_re  = re.compile(FORTRAN_ID+r"_((?i)(?:extent)|(?:begin)|(?:end))$")

    def __init__(self, name, ddt_defs, var_locations, variables, logger=None):
        self._name = name
        self._ddt_defs = ddt_defs
        self._var_locations = var_locations
        super(HostModel, self).__init__(self.name, variables=variables, logger=logger)
        self._ddt_vars = {}
        self._ddt_fields = {}
        # Make sure we have a DDT definition for every DDT variable
        self.check_ddt_vars(logger)
        self.collect_ddt_fields(logger)

    @property
    def name(self):
        'Return the host model name'
        return self._name

    def argument_list(self):
        'Return a string representing the host model variable arg list'
        args = self.prop_list('local_name')
        return ', '.join(args)

    def add_ddt_defs(new_ddt_defs, logger=None):
        "Add new DDT metadata definitions to model"
        if new_ddt_defs is not None:
            for header in new_ddt_defs:
                if header.title in self._ddt_defs:
                    raise CCPPError("Duplicate DDT, {}, passed to add_ddt_defs".format(header.title))
                else:
                    if logger is not None:
                        logger.debug('Adding ddt, {}'.format(header.title))
                    # End if
                    self._ddt_defs[header.title] = header
                # End if
            # End for
            # Make sure we have a DDT definition for every DDT variable
            self.check_ddt_vars()
            self.collect_ddt_fields()
        # End if

    def add_variables(new_variables, logger=None):
        "Add new variables definitions to model"
        if new_variables is not None:
            self.merge(new_variables)
        # End if

    def check_ddt_vars(self, logger=None):
        "Check that we have a DDT definition for every DDT variable"
        for vkey in self.keys():
            var = self[vkey]
            vtype = var.get_prop_value('type')
            if not check_fortran_intrinsic(vtype):
                vkind = var.get_prop_value('kind')
                stdname = var.get_prop_value('standard_name')
                if stdname in self._ddt_vars:
                    # Make sure this is not a duplicate
                    if self._ddt_vars[stdname] != self._ddt_defs[vkind]:
                        raise CCPPError("Duplicate DDT definition for {}".format(vkind))
                    # End if
                else:
                    self._ddt_vars[stdname] = self._ddt_defs[vkind]
                # End if
            # End if (no else, intrinsic types)
        # End for

    def collect_fields_from_ddt(self, ddt, source, logger=None):
        "Collect all the reachable fields from one DDT definition"
        for var in ddt.variable_list():
            vtype = var.get_prop_value('type')
            stdname = var.get_prop_value('standard_name')
            if stdname in self._ddt_fields:
                src = [x.get_prop_name('kind') for x in source]
                raise CCPPError("Duplicate DDT standard name, {}, from {}".format(stdname), src)
            elif vtype == 'type':
                # Process this DDT field if it defined
                vkind = var.get_prop_value('kind')
                if vkind in self._ddt_defs[vkind]:
                    self.collect_fields(var, source + [var])
                # End if (just ignore undefined subfields)
            else:
                # Record this intrinsic variable
                self._ddt_fields[stdname] = source + [var]
                if logger is not None:
                    logger.debug('Adding DDT field, {}, {}'.format(stdname, [x.get_prop_value('local_name') for x in self._ddt_fields[stdname]]))
                # End if
            # End if
        # End for

    def collect_ddt_fields(self, logger=None):
        "Make sure we know the standard names of all reachable fields"
        for stdname in self._ddt_vars.keys():
            # The source for the fields in this DDT is the variable
            ddt = self._ddt_vars[stdname]
            svar = super(HostModel, self).find_variable(stdname)
            self.collect_fields_from_ddt(ddt, [svar], logger)
        # End for

    def variable_locations(self):
        """Return a set of module-variable and module-type pairs.
        These represent the locations of all host model data."""
        varset = set()
        mods = self.prop_list('module')
        names = self.prop_list('local_name')
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

    def find_variable(self, standard_name, loop_subst=False):
        "Return the host model variable matching <standard_name> or None"
        my_var = super(HostModel, self).find_variable(standard_name)
        if (my_var is None) and (standard_name in self._ddt_fields):
            # Found variable in a DDT element
            my_var = self._ddt_fields[standard_name]
        # End if
        if (my_var is None) and loop_subst:
            loop_var = HostModel.loop_re.match(standard_name)
            if loop_var is not None:
                # Let us see if we can fix a loop variable
                # First up, we have an extent but host has begin and end
                if loop_var.group(2).lower() == 'extent':
                    beg_name = loop_var.group(1) + '_begin'
                    end_name = loop_var.group(1) + '_end'
                    beg_var = self.find_variable(beg_name)
                    end_var = self.find_variable(end_name)
                    if (beg_var is not None) and (end_var is not None):
                        my_var = (beg_var, end_var)
                    # End if
                # End if
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

    ###########################################################################
    @classmethod
    def parse_host_registry(cls, filename, logger, ddt_defs, host_model=None):
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
        variables = VarDictionary(host_name, logger=logger)
        logger.info("Reading host model registry for {}".format(host_name))
        for child in root:
            if (child.tag == 'variable') or (child.tag == 'constant'):
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
                        elif var_prop.tag == 'dimensions':
                            # We need to rebuild this text for compatibility
                            dims = [x.strip() for x in var_prop.text.split(',')]
                            prop_dict[var_prop.tag] = '({})'.format(', '.join(dims))
                        else:
                            prop_dict[var_prop.tag] = var_prop.text
                        # End if
                    # End if
                # End for
                if child.tag == 'constant':
                    prop_dict['constant'] = '.true.'
                # End if
                newvar = Var(prop_dict, ParseSource(vname, 'REGISTRY', context))
                variables.add_variable(newvar)
            # Else need to read ddt_defs? <== XXgoldyXX?
            # End if
        # End for
        # Now that we have all the info from this XML, create a HostModel
        # object or add to the one that is there:
        if host_model is None:
            host_model = HostModel(host_name, ddt_defs, host_variables, variables, logger=logger)
        elif host_name != host_model.name:
            raise CCPPError('Inconsistent host model names, {} and {}'.format(host_name, host_model.name))
        else:
            host_model.add_variables(variables, logger=logger)
            host_model.add_ddt_defs(ddt_defs, logger=logger)
            for var in host_variables.keys():
                host_model.add_host_variable_module(var, host_variables[var], logger=logger)
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
