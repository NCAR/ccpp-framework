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
from parse_tools import ParseSource, ParseContext
from parse_tools import read_xml_file, validate_xml_file, find_schema_version

###############################################################################
class HostRegAbort(ValueError):
    "Class so main can log user errors without backtrace"
    def __init__(self, message):
        super(HostRegAbort, self).__init__(message)

###############################################################################
class HostModel(object):
    "Class to hold the data from a host model"

    def __init__(self, ddt_defs, var_locations, variables):
        self._ddt_defs = ddt_defs
        self._var_locations = var_locations
        self._variables = variables

###############################################################################
def abort(message, logger=None):
###############################################################################
    if logger is not None:
        logger.error(message)
    # End if (no else)
    raise HostRegAbort(message)

###############################################################################
def parse_host_registry(filename, logger):
###############################################################################
    variables = VarDictionary(logger=logger)
    host_variables = {}
    tree, root = read_xml_file(filename, logger=logger)
    # We do not have line number information for the XML file
    context = ParseContext(filename=filename)
    version = find_schema_version(root, logger=logger) # Only one version so far
    res = validate_xml_file(filename, 'host_registry', version, logger=logger)
    if not res:
        abort("Invalid host registry file, '{}'".format(filename), logger)
    # End if
    logger.info("Reading host model registry for {}".format(root.get('model')))
    # End if
    for child in root:
        if (child.tag == 'dimension') or (child.tag == 'variable'):
            prop_dict = child.attrib
            vname = prop_dict['local_name']
            for var_prop in child:
                if var_prop.tag == 'module':
                    # We need to keep track of where host variables come from
                    host_variables[vname] = var_prop.text
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
    return variables, host_variables

###############################################################################

if __name__ == "__main__":
    from parse_tools import initLog, setLogToNull
    logger = initLog('host_registry')
    setLogToNull(logger)
    # First, run doctest
    import doctest
    doctest.testmod()
# No else:
