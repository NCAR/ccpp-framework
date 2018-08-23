#!/usr/bin/env python
#
# Class to hold all information on a CCPP DDT metadata variable
#

# Python library imports
from __future__     import print_function
# CCPP framework imports
from parse_tools    import check_local_name, check_fortran_type, context_string
from parse_tools    import FORTRAN_DP_RE, FORTRAN_ID
from parse_tools    import registered_fortran_ddt_name
from parse_tools    import ParseInternalError, CCPPError
from metavar        import Var
from metadata_table import MetadataTable

###############################################################################

class VarDDT(Var):
    """A class to store a variable that is a component of a DDT (at any
    DDT nesting level).
    <new_field> is the DDT component.
    <var_ref> is a Var or VarDDT whose root originates in a model
    dictionary.
    The structure of the VarDDT object is:
       The super class Var object is a copy of the model root Var.
       The self._var_ref object is a VarDDT containing the top-level
          field that leads to this component.
    Thus, <new_field> (a Var) ends up at the end of a VarDDT chain.
    """

    def __init__(self, new_field, var_ref, logger=None, recur=False):
        self._field = None
        # Grab the info from the root of <var_ref>
        source = var_ref.source
        super(VarDDT, self).__init__(var_ref, source, context=source.context,
                                     logger=logger)
        # Find the correct place for <new_field>
        if isinstance(var_ref, Var):
            self._field = new_field
        else:
            self._field = VarDDT(new_field, var_ref.field,
                                 logger=logger, recur=True)
        # End if
        if (not recur) and (logger is not None):
            logger.debug('Adding DDT field, {}'.format(self))
        # End if

    def is_ddt(self):
        '''Return True iff <self> is a DDT type.'''
        return True

    def get_prop_value(self, name):
        """Return the Var property value for the leaf Var object.
        """
        if self.field is None:
            pvalue = super(VarDDT, self).get_prop_value(name)
        else:
            pvalue = self.field.get_prop_value(name)
        # End if
        return pvalue

    def clone(self, subst_dict, source_name=None, source_type=None,
              context=None, loop_match=False):
        """Create a clone of this VarDDT object's leaf Var with properties
        from <subst_dict> overriding this variable's properties.
        <subst_dict> may also be a string in which case only the local_name
        property is changed (to the value of the <subst_dict> string).
        The optional <source_name>, <source_type>, and <context> inputs
        allow the clone to appear to be coming from a designated source,
        by default, the source and type are the same as this Var (self).
        """
        if self.field is None:
            clone_var = super(VarDDT, self).clone(subst_dict,
                                                  source_name=source_name,
                                                  source_type=source_type,
                                                  context=context,
                                                  loop_match=loop_match)
        else:
            clone_var = self.field.clone(subst_dict,
                                         source_name=source_name,
                                         source_type=source_type,
                                         context=context, loop_match=loop_match)
        # End if
        return clone_var

    def call_string(self, var_dict, loop_vars=None):
        """Return a legal call string of this VarDDT's local name sequence.
        """
        # XXgoldyXX: Need to add dimensions to this
        call_str = super(VarDDT, self).get_prop_value('local_name')
        if self.field is not None:
            call_str += '%' + self.field.call_string(var_dict,
                                                     loop_vars=loop_vars)
        # End if
        return call_str

    def write_def(self, outfile, indent, dict, allocatable=False, dummy=False):
        '''Write the definition line for this DDT.
        The type of this declaration is the type of the Var at the
        end of the chain of references.'''
        if self.field is None:
            super(VarDDT, self).write_def(outfile, indent, dict,
                                          allocatable=allocatable, dummy=dummy)
        else:
            self.field.write_def(outfile, indent, dict,
                                 allocatable=allocatable, dummy=dummy)
        # End if

    def __var_rep__(self, var, prefix=""):
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
            # End if
        else:
            if prefix:
                lstr = '{}%{}'.format(prefix, lname)
            else:
                lstr = '{}'.format(lname)
            # End if
        # End if
        return lstr

    def __repr__(self):
        '''Print representation for VarDDT objects'''
        # Note, recursion would be messy because of formatting issues
        lstr = ""
        sep = ""
        field = self
        while field is not None:
            if isinstance(field, VarDDT):
                lstr += sep + self.__var_rep__(field.var)
                field = field.field
            elif isinstance(field, Var):
                lstr = self.__var_rep__(field, prefix=lstr)
                field = None
            # End if
            sep = '%'
        # End while
        return "<VarDDT {}>".format(lstr)

    def __str__(self):
        '''Print string for VarDDT objects'''
        return self.__repr__()

    @property
    def var(self):
        "Return this VarDDT's Var object"
        return super(VarDDT, self)

    @property
    def field(self):
        "Return this objects field object, or None"
        return self._field

###############################################################################
class DDTLibrary(dict):
    """DDTLibrary is a collection of DDT definitions, broken down into
    individual fields with metadata. It provides efficient ways to find
    the field corresponding to any standard-named field contained in
    any of the (potentially nested) included DDT definitions.
    The dictionary holds known standard names.
    """

    def __init__(self, name, ddts=None, logger=None):
        "Our dict is DDT definition headers, key is type"
        self._name = '{}_ddt_lib'.format(name)
        self._ddt_fields = {}    # DDT field to DDT access map
        super(DDTLibrary, self).__init__()
        if ddts is None:
            ddts = list()
        elif not isinstance(ddts, list):
            ddts = [ddts]
        # End if
        # Add all the DDT headers, then process
        for ddt in ddts:
            if not isinstance(ddt, MetadataTable):
                errmsg = 'Invalid DDT metadata table type, {}'
                raise ParseInternalError(errmsg.format(type(ddt)))
            elif not ddt.header_type == 'ddt':
                errmsg = 'Metadata table header is for a {}, should be DDT'
                raise ParseInternalError(errmsg.format(ddt.header_type))
            elif ddt.title in self:
                errmsg = "Duplicate DDT, {}, found{}, original{}"
                ctx = context_string(ddt.source.context)
                octx = context_string(self[ddt.title].source.context)
                raise CCPPError(errmsg.format(ddt.title, ctx, octx))
            else:
                if logger is not None:
                    lmsg = 'Adding DDT {} to {}'
                    logger.debug(lmsg.format(ddt.title, self.name))
                self[ddt.title] = ddt
            # End if
        # End for

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
                # End if
                errmsg = 'Variable {} is of unknown type ({}) in {}'
                ctx = context_string(var.context)
                raise CCPPError(errmsg.format(lname, vtype, header.title, ctx))
            # End if
        # End if (no else needed)

    def collect_fields_from_ddt(self, source_dict, dest_dict, logger=None):
        """Add all the reachable fields from each DDT var in <source_dict> to
        <dest_dict>. Each field is added as a VarDDT."""
        for var in source_dict.variable_list():
            vtype = var.get_prop_value('type')
            if vtype in self:
                ddt = self[vtype]
                self.collect_ddt_fields(dest_dict, var, ddt, logger=logger)
            # End if
        # End for

    def collect_ddt_fields(self, var_dict, var, logger=None):
        """Add all the reachable fields from DDT variable <var> of type,
        <ddt> to <var_dict>. Each field is added as a VarDDT.
        """
        vtype = var.get_prop_value('type')
        if vtype in self:
            ddt = self[vtype]
        else:
            lname = var.get_prop_value('local_name')
            ctx = context_string(var.context)
            errmsg = "Variable, {}, is not a known DDT{}"
            raise ParseInternalError(errmsg.format(lname, ctx))
        # End if
        stdname = var.get_prop_value('standard_name')
        for dvar in ddt.variable_list():
            subvar = VarDDT(dvar, var)
            dvtype = dvar.get_prop_value('type')
            if (dvar.is_ddt()) and (dvtype in self):
                # If DDT in our library, we need to add sub-fields recursively.
                subddt = self[dvtype]
                self.collect_ddt_fields(var_dict, subvar, subddt)
            else:
                # Add this intrinsic to <var_dict>
                var_dict.add_variable(subvar)
        # End for

    def ddt_modules(self, variable_list, ddt_mods=None):
        """Collect information for module use statements.
        Add module use information (module name, DDT name) for any variable
        in <variable_list> which is a DDT in this library.
        """
        if ddt_mods is None:
            ddt_mods = set() # Need a new set for every call
        # End if
        for var in variable_list:
            vtype = var.get_prop_value('type')
            if vtype in self:
                module = self[vtype].module
                ddt_mods.add((module, vtype))
            # End if
        # End for
        return ddt_mods

    def write_ddt_use_statements(self, variable_list, outfile, indent):
        ddt_mods = self.ddt_modules(variable_list)
        dmax = 0
        for ddt_mod in ddt_mods:
            dmax = max(dmax, len(ddt_mod[0]))
        # End for
        for ddt_mod in ddt_mods:
            dmod = ddt_mod[0]
            dtype = ddt_mod[1]
            slen = ' '*(dmax - len(dmod))
            ustring = 'use {},{} only: {}'
            outfile.write(ustring.format(dmod, slen, dtype), indent)
        # End for

    @property
    def name(self):
        "Return the name of this DDT library"
        return self._name

###############################################################################
if __name__ == "__main__":
    import doctest
    doctest.testmod()
