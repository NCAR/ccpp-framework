#!/usr/bin/env python

"""
Parse a host-model registry XML file and return the captured variables.
"""

# Python library imports
from __future__ import print_function
# CCPP framework imports
from metavar import VarDictionary
from ddt_library import VarDDT, DDTLibrary
from parse_tools import ParseContext, CCPPError
from parse_tools import context_string
from parse_tools import FORTRAN_SCALAR_REF_RE

###############################################################################
class HostModel(VarDictionary):
    """Class to hold the data from a host model"""

    def __init__(self, meta_headers, name_in, logger):
        self.__name = name_in
        self.__var_locations = {} # Local name to module map
        self.__loop_vars = None   # Loop control vars in interface calls
        self.__used_variables = None # Local names which have been requested
        self.__deferred_finds = None # Used variables that were missed at first
        # First, process DDT headers
        self.__ddt_lib = DDTLibrary('{}_ddts'.format(self.name),
                                    ddts=[d for d in meta_headers
                                          if d.header_type == 'ddt'],
                                    logger=logger)
        self.__ddt_dict = VarDictionary("{}_ddt_vars".format(self.name),
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
                    self.__var_locations[lname] = modname
                    self.__ddt_lib.check_ddt_type(var, header, lname=lname)
                    if var.is_ddt():
                        self.__ddt_lib.collect_ddt_fields(self.__ddt_dict, var)
                    # End if
                # End for
            elif header.header_type == 'host':
                if self.__name is None:
                    # Grab the first host name we see
                    self.__name = header.name
                # End if
                varlist.extend(header.variable_list())
                for var in header.variable_list():
                    self.__ddt_lib.check_ddt_type(var, header)
                    if var.is_ddt():
                        self.__ddt_lib.collect_ddt_fields(self.__ddt_dict, var)
                    # End if
                # End for
                loop_vars = header.variable_list(std_vars=False,
                                                 loop_vars=True, consts=False)
                if loop_vars:
                    # loop_vars are part of the host-model interface call
                    # at run time. As such, they override the host-model
                    # array dimensions.
                    self.__loop_vars = VarDictionary(self.name)
                # End if
                for hvar in loop_vars:
                    std_name = hvar.get_prop_value('standard_name')
                    if std_name not in self.__loop_vars:
                        self.__loop_vars.add_variable(hvar)
                    else:
                        ovar = self.__loop_vars[std_name]
                        ctx1 = context_string(ovar.context)
                        ctx2 = context_string(hvar.context)
                        lname1 = ovar.get_prop_value('local_name')
                        lname2 = hvar.get_prop_value('local_name')
                        errmsg = ("Duplicate host loop var for {n}:\n"
                                  "  Dup:  {l1}{c1}\n  Orig: {l2}{c2}")
                        raise CCPPError(errmsg.format(n=self.name,
                                                      l1=lname1, c1=ctx1,
                                                      l2=lname2, c2=ctx2))
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
        # End if
        # Initialize variable dictionary
        super(HostModel, self).__init__(self.name, variables=varlist,
                                        logger=logger)
        # Finally, turn on the use meter so we know which module variables
        #    to 'use' in a host cap.
        self.__used_variables = set() # Local names which have been requested
        self.__deferred_finds = set() # Used variables that were missed at first

    @property
    def name(self):
        """Return the host model name"""
        return self.__name

    @property
    def loop_vars(self):
        """Return this host model's loop variables"""
        return self.__loop_vars

    @property
    def ddt_lib(self):
        """Return this host model's DDT library"""
        return self.__ddt_lib

    def argument_list(self, loop_vars=True):
        """Return a string representing the host model variable arg list"""
        args = [v.call_string(self)
                for v in self.variable_list(loop_vars=loop_vars, consts=False)]
        return ', '.join(args)

    def host_variable_module(self, local_name):
        """Return the module name for a host variable"""
        if local_name in self.__var_locations:
            return self.__var_locations[local_name]
        # End if
        return None

    def variable_locations(self):
        """Return a set of module-variable and module-type pairs.
        These represent the locations of all host model data with a listed
        source location (variables with no <module> source are omitted)."""
        varset = set()
        lnames = self.prop_list('local_name')
        # Attempt to realize deferred lookups
        if self.__deferred_finds is not None:
            for std_name in list(self.__deferred_finds):
                var = self.find_variable(std_name)
                if var is not None:
                    self.__deferred_finds.remove(std_name)
                # End if
            # End for
        # End if
        # Now, find all the used module variables
        for name in lnames:
            module = self.host_variable_module(name)
            used = self.__used_variables and (name in self.__used_variables)
            if module and used:
                varset.add((module, name))
            # No else, either no module or a zero-length module name
            # End if
        # End for
        return varset

    # pylint: disable=arguments-differ
    def find_variable(self, standard_name, any_scope=False, loop_subst=False):
        """Return the host model variable matching <standard_name> or None
        If loop_subst is True, substitute a begin:end range for an extent.
        """
        my_var = super(HostModel, self).find_variable(standard_name,
                                                      any_scope=any_scope)
        if my_var is None:
            # Check our DDT library
            my_var = self.__ddt_dict.find_variable(standard_name)
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
                ctx = ParseContext(filename='host_model.py')
                new_var = my_var.clone(new_name, source_name=self.name,
                                       source_type="HOST",
                                       context=ctx)
                self.add_variable(new_var)
                my_var = new_var
            # End if
        # End if
        if my_var is None:
            if self.__deferred_finds is not None:
                self.__deferred_finds.add(standard_name)
            # End if
        elif self.__used_variables is not None:
            lname = my_var.get_prop_value('local_name')
            # Try to add any index references (should be method?)
            imatch = FORTRAN_SCALAR_REF_RE.match(lname)
            if imatch is not None:
                vdims = [x.strip() for x in imatch.group(2).split(',')
                         if ':' not in x]
                for vname in vdims:
                    _ = self.find_variable(vname)
                # End for
            # End if
            if isinstance(my_var, VarDDT):
                lname = my_var.get_parent_prop('local_name')
            # End if
            self.__used_variables.add(lname)
        # End if
        return my_var
        # pylint: enable=arguments-differ

    def add_host_variable_module(self, local_name, module, logger=None):
        """Add a module name location for a host variable"""
        if local_name not in self.__var_locations:
            if logger is not None:
                emsg = 'Adding variable, {}, from module, {}'
                logger.debug(emsg.format(local_name, module))
            # End if
            self.__var_locations[local_name] = module
        else:
            emsg = "Host variable, {}, already located in module"
            raise CCPPError(emsg.format(self.__var_locations[local_name]))
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
    _LOGGER = init_log('host_registry')
    set_log_to_null(_LOGGER)
    # First, run doctest
    import doctest
    doctest.testmod()
# No else:
