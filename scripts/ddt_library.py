#!/usr/bin/env python3
#
# Class
#

"""Module to implement DDT support in the CCPP Framework.
VarDDT is a class to hold all information on a CCPP DDT metadata variable
"""

# Python library imports
import logging
# CCPP framework imports
from parse_tools    import ParseInternalError, CCPPError, context_string
from metavar        import Var
from metadata_table import MetadataSection

###############################################################################

class VarDDT(Var):
    """A class to store a variable that is a component of a DDT (at any
    DDT nesting level).
    """

    def __init__(self, new_field, var_ref, run_env, recur=False):
        """Initialize a new VarDDT object.
        <new_field> is the DDT component.
        <var_ref> is a Var or VarDDT whose root originates in a model
        dictionary.
        <run_env> is the CCPPFrameworkEnv object for this framework run.
        The structure of the VarDDT object is:
            The super class Var object is a copy of the model root Var.
            The <var_ref object is a VarDDT containing the top-level
                field that leads to this component.
        Thus, <new_field> (a Var) ends up at the end of a VarDDT chain.
        """
        self.__field = None
        # Grab the info from the root of <var_ref>
        source = var_ref.source
        super().__init__(var_ref, source, run_env, context=source.context)
        # Find the correct place for <new_field>
        if isinstance(var_ref, Var):
            # We are at a top level DDT var, set our field
            self.__field = new_field
        else:
            # Recurse to find correct (tail) location for <new_field>
            self.__field = VarDDT(new_field, var_ref.field, run_env, recur=True)
        # end if
        if ((not recur) and
            run_env.debug_on()):
            run_env.logger.debug('Adding DDT field, {}'.format(self))
        # end if

    def is_ddt(self):
        """Return True iff <self> is a DDT type."""
        return True

    def get_parent_prop(self, name):
        """Return the Var property value for the parent Var object.
        """
        return super().get_prop_value(name)

    def get_prop_value(self, name):
        """Return the Var property value for the leaf Var object.
        """
        if self.field is None:
            pvalue = super().get_prop_value(name)
        else:
            pvalue = self.field.get_prop_value(name)
        # end if
        return pvalue

    def intrinsic_elements(self, check_dict=None):
        """Return the Var intrinsic elements for the leaf Var object.
        See Var.intrinsic_elements for details
        """
        if self.field is None:
            pvalue = super().intrinsic_elements(check_dict=check_dict)
        else:
            pvalue = self.field.intrinsic_elements(check_dict=check_dict)
        # end if
        return pvalue

    def clone(self, subst_dict, source_name=None, source_type=None,
              context=None):
        """Create a clone of this VarDDT object's leaf Var with properties
        from <subst_dict> overriding this variable's properties.
        <subst_dict> may also be a string in which case only the local_name
        property is changed (to the value of the <subst_dict> string).
        The optional <source_name>, <source_type>, and <context> inputs
        allow the clone to appear to be coming from a designated source,
        by default, the source and type are the same as this Var (self).
        """
        if self.field is None:
            clone_var = super().clone(subst_dict, source_name=source_name,
                                      source_type=source_type, context=context)
        else:
            clone_var = self.field.clone(subst_dict,
                                         source_name=source_name,
                                         source_type=source_type,
                                         context=context)
        # end if
        return clone_var

    def call_string(self, var_dict, loop_vars=None):
        """Return a legal call string of this VarDDT's local name sequence.
        """
        # XXgoldyXX: Need to add dimensions to this
        call_str = super().get_prop_value('local_name')
        if self.field is not None:
            call_str += '%' + self.field.call_string(var_dict,
                                                     loop_vars=loop_vars)
        # end if
        return call_str

    def write_def(self, outfile, indent, ddict, allocatable=False, dummy=False):
        """Write the definition line for this DDT.
        The type of this declaration is the type of the Var at the
        end of the chain of references."""
        if self.field is None:
            super().write_def(outfile, indent, ddict,
                              allocatable=allocatable, dummy=dummy)
        else:
            self.field.write_def(outfile, indent, ddict,
                                 allocatable=allocatable, dummy=dummy)
        # end if

    @staticmethod
    def __var_rep(var, prefix=""):
        """Internal helper function for creating VarDDT representations
        Create a call string from the local_name and dimensions of <var>.
        Optionally, prepend <prefix>%.
        """
        lname = var.get_prop_value('local_name')
        ldims = var.get_prop_value('dimensions')
        if ldims:
            if prefix:
                lstr = '{}%{}({})'.format(prefix, lname, ', '.join(ldims))
            else:
                lstr = '{}({})'.format(lname, ', '.join(ldims))
            # end if
        else:
            if prefix:
                lstr = '{}%{}'.format(prefix, lname)
            else:
                lstr = '{}'.format(lname)
            # end if
        # end if
        return lstr

    def __repr__(self):
        """Print representation for VarDDT objects"""
        # Note, recursion would be messy because of formatting issues
        lstr = ""
        sep = ""
        field = self
        while field is not None:
            if isinstance(field, VarDDT):
                lstr += sep + self.__var_rep(field.var)
                field = field.field
            elif isinstance(field, Var):
                lstr = self.__var_rep(field, prefix=lstr)
                field = None
            # end if
            sep = '%'
        # end while
        return "<VarDDT {}>".format(lstr)

    def __str__(self):
        """Print string for VarDDT objects"""
        return self.__repr__()

    @property
    def var(self):
        "Return this VarDDT's Var object"
        return super()

    @property
    def field(self):
        "Return this objects field object, or None"
        return self.__field

###############################################################################
class DDTLibrary(dict):
    """DDTLibrary is a collection of DDT definitions, broken down into
    individual fields with metadata. It provides efficient ways to find
    the field corresponding to any standard-named field contained in
    any of the (potentially nested) included DDT definitions.
    The dictionary holds known standard names.
    """

    def __init__(self, name, run_env, ddts=None):
        "Our dict is DDT definition headers, key is type"
        self.__name = '{}_ddt_lib'.format(name)
# XXgoldyXX: v remove?
#        self.__ddt_fields = {}    # DDT field to DDT access map
# XXgoldyXX: ^ remove?
        self.__max_mod_name_len = 0
        self.__run_env = run_env
        super().__init__()
        if ddts is None:
            ddts = list()
        elif not isinstance(ddts, list):
            ddts = [ddts]
        # end if
        # Add all the DDT headers, then process
        for ddt in ddts:
            if not isinstance(ddt, MetadataSection):
                errmsg = 'Invalid DDT metadata type, {}'
                raise ParseInternalError(errmsg.format(type(ddt).__name__))
            # end if
            if not ddt.header_type == 'ddt':
                errmsg = 'Metadata table header is for a {}, should be DDT'
                raise ParseInternalError(errmsg.format(ddt.header_type))
            # end if
            if ddt.title in self:
                errmsg = "Duplicate DDT, {}, found{}, original{}"
                ctx = context_string(ddt.source.context)
                octx = context_string(self[ddt.title].source.context)
                raise CCPPError(errmsg.format(ddt.title, ctx, octx))
            # end if
            if run_env.debug_on():
                lmsg = f"Adding DDT {ddt.title} to {self.name}"
                run_env.logger.debug(lmsg)
            # end if
            self[ddt.title] = ddt
            dlen = len(ddt.module)
            if dlen > self.__max_mod_name_len:
                self.__max_mod_name_len = dlen
            # end if
        # end for

    def check_ddt_type(self, var, header, lname=None):
        """If <var> is a DDT, check to make sure it is in this DDT library.
        If not, raise an exception.
        """
        if var.is_ddt():
            # Make sure we know this DDT type
            vtype = var.get_prop_value('type')
            if vtype not in self:
                if lname is None:
                    lname = var.get_prop_value('local_name')
                # end if
                errmsg = 'Variable {} is of unknown type ({}) in {}'
                ctx = context_string(var.context)
                raise CCPPError(errmsg.format(lname, vtype, header.title, ctx))
            # end if
        # end if (no else needed)

    def collect_ddt_fields(self, var_dict, var, run_env,
                           ddt=None, skip_duplicates=False):
        """Add all the reachable fields from DDT variable <var> of type,
           <ddt> to <var_dict>. Each field is added as a VarDDT.
           Note: By default, it is an error to try to add a duplicate
                 field to <var_dict> (i.e., the field already exists in
                 <var_dict> or one of its parents). To simply skip duplicate
                 fields, set <skip_duplicates> to True.
        """
        if ddt is None:
            vtype = var.get_prop_value('type')
            if vtype in self:
                ddt = self[vtype]
            else:
                lname = var.get_prop_value('local_name')
                ctx = context_string(var.context)
                errmsg = "Variable, {}, is not a known DDT{}"
                raise ParseInternalError(errmsg.format(lname, ctx))
            # end if
        # end if
        for dvar in ddt.variable_list():
            subvar = VarDDT(dvar, var, self.run_env)
            dvtype = dvar.get_prop_value('type')
            if (dvar.is_ddt()) and (dvtype in self):
                # If DDT in our library, we need to add sub-fields recursively.
                subddt = self[dvtype]
                self.collect_ddt_fields(var_dict, subvar, run_env, ddt=subddt)
            # end if
            # add_variable only checks the current dictionary. By default,
            # for a DDT, the variable also cannot be in our parent
            # dictionaries.
            stdname = dvar.get_prop_value('standard_name')
            pvar = var_dict.find_variable(standard_name=stdname, any_scope=True)
            if pvar and (not skip_duplicates):
                ntx = context_string(dvar.context)
                ctx = context_string(pvar.context)
                emsg = f"Attempt to add duplicate DDT sub-variable, {stdname}{ntx}."
                emsg += f"\nVariable originally defined{ctx}"
                raise CCPPError(emsg.format(stdname, ntx, ctx))
            # end if
            # Add this intrinsic to <var_dict>
            if not pvar:
                var_dict.add_variable(subvar, run_env)
            # end if
        # end for

    def ddt_modules(self, variable_list, ddt_mods=None):
        """Collect information for module use statements.
        Add module use information (module name, DDT name) for any variable
        in <variable_list> which is a DDT in this library.
        """
        if ddt_mods is None:
            ddt_mods = set() # Need a new set for every call
        # end if
        for var in variable_list:
            vtype = var.get_prop_value('type')
            if vtype in self:
                module = self[vtype].module
                ddt_mods.add((module, vtype))
            # end if
        # end for
        return ddt_mods

    def write_ddt_use_statements(self, variable_list, outfile, indent, pad=0):
        """Write the use statements for all ddt modules needed by
        <variable_list>"""
        pad = max(pad, self.__max_mod_name_len)
        ddt_mods = self.ddt_modules(variable_list)
        for ddt_mod in ddt_mods:
            dmod = ddt_mod[0]
            dtype = ddt_mod[1]
            slen = ' '*(pad - len(dmod))
            ustring = 'use {},{} only: {}'
            outfile.write(ustring.format(dmod, slen, dtype), indent)
        # end for

    @property
    def name(self):
        "Return the name of this DDT library"
        return self.__name

    @property
    def run_env(self):
        """Return the CCPPFrameworkEnv object for this DDT library"""
        return self.__run_env

###############################################################################
if __name__ == "__main__":
    # pylint: disable=ungrouped-imports
    import doctest
    import sys
    # pylint: enable=ungrouped-imports
    fail, _ = doctest.testmod()
    sys.exit(fail)
# end if
