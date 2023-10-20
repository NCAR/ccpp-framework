#!/usr/bin/env python3

"""
Class and supporting code to hold all information on CCPP constituent
variables. A constituent variable is defined and maintained by the CCPP
Framework instead of the host model.
The ConstituentVarDict class contains methods to generate the necessary code
to implement this support.
"""

# Python library imports
from __future__ import print_function
import os
# CCPP framework imports
from file_utils import KINDS_MODULE
from fortran_tools import FortranWriter
from parse_tools import ParseInternalError
from metavar import Var, VarDictionary

########################################################################

CONST_DDT_NAME = "ccpp_model_constituents_t"
CONST_DDT_MOD = "ccpp_constituent_prop_mod"
CONST_PROP_TYPE = "ccpp_constituent_properties_t"

########################################################################

class ConstituentVarDict(VarDictionary):
    """A class to hold all the constituent variables for a CCPP Suite.
    Also contains methods to generate the necessary code for runtime
    allocation and support for these variables.
    """

    __const_prop_array_name  = "ccpp_constituent_array"
    __const_prop_init_name  = "ccpp_constituents_initialized"
    __const_prop_init_consts = "ccpp_create_constituent_array"
    __const_prop_type_name = "ccpp_constituent_properties_t"
    __constituent_type = "suite"

    def __init__(self, name, parent_dict, run_env, variables=None):
        """Create a specialized VarDictionary for constituents.
        The main difference is functionality to allocate and support
        these variables with special functions for the host model.
        The main reason for a separate dictionary is that these are not
        proper Suite variables but will belong to the host model at run time.
        The <parent_dict> feature of the VarDictionary class is required
        because this dictionary must be connected to a host model.
        """
        self.__run_env = run_env
        super(ConstituentVarDict, self).__init__(name, run_env,
                                                 variables=variables,
                                                 parent_dict=parent_dict)

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
        Note that although the <clone> argument is accepted for consistency,
           cloning is not handled at this level.
        If the variable is not found and <source_var> is not a constituent
           variable, return None.
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
        elif any_scope and (self.parent is not None):
            srch_clist = search_call_list
            var = self.parent.find_variable(standard_name=standard_name,
                                            source_var=source_var,
                                            any_scope=any_scope, clone=None,
                                            search_call_list=srch_clist,
                                            loop_subst=loop_subst)
        else:
            var = None
        # end if
        if (var is None) and source_var and source_var.is_constituent():
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
            var = source_var.clone({'dimensions' : newdims}, remove_intent=True,
                                   source_type=self.__constituent_type)
            self.add_variable(var, self.__run_env)
        return var

    @staticmethod
    def __init_err_var(evar, outfile, indent):
        """If <evar> is a known error variable, generate the code to
        initialize it as an output variable.
        If unknown, simply ignore.
        """
        stdname = evar.get_prop_value('standard_name')
        if stdname == 'ccpp_error_message':
            lname = evar.get_prop_value('local_name')
            outfile.write("{} = ''".format(lname), indent)
        elif stdname == 'ccpp_error_code':
            lname = evar.get_prop_value('local_name')
            outfile.write("{} = 0".format(lname), indent)
        # end if (no else, just ignore)

    def declare_public_interfaces(self, outfile, indent):
        """Declare the public constituent interfaces.
        Declarations are written to <outfile> at indent, <indent>."""
        outfile.write("! Public interfaces for handling constituents", indent)
        outfile.write("! Return the number of constituents for this suite",
                      indent)
        outfile.write("public :: {}".format(self.num_consts_funcname()), indent)
        outfile.write("! Return the name of a constituent", indent)
        outfile.write("public :: {}".format(self.const_name_subname()), indent)
        outfile.write("! Copy the data for a constituent", indent)
        outfile.write("public :: {}".format(self.copy_const_subname()), indent)

    def declare_private_data(self, outfile, indent):
        """Declare private suite module variables and interfaces
        to <outfile> with indent, <indent>."""
        outfile.write("! Private constituent module data", indent)
        if self:
            stmt = "type({}), private, allocatable :: {}(:)"
            outfile.write(stmt.format(self.constituent_prop_type_name(),
                                      self.constituent_prop_array_name()),
                          indent)
        # end if
        stmt = "logical, private :: {} = .false."
        outfile.write(stmt.format(self.constituent_prop_init_name()), indent)
        outfile.write("! Private interface for constituents", indent)
        stmt = "private :: {}"
        outfile.write(stmt.format(self.constituent_prop_init_consts()), indent)

    @classmethod
    def __errcode_names(cls, err_vars):
        """Return the (<errcode> <errmsg>) where <errcode> is the local name
        for ccpp_error_code in <err_vars> and <errmsg> is the local name for
        ccpp_error_message in <err_vars>.
        if either variable is not found in <err_vars>, return None."""
        errcode = None
        errmsg = None
        for evar in err_vars:
            stdname = evar.get_prop_value('standard_name')
            if stdname  == 'ccpp_error_code':
                errcode = evar.get_prop_value('local_name')
            elif stdname == 'ccpp_error_message':
                errmsg = evar.get_prop_value('local_name')
            else:
                emsg = "Bad errcode variable, '{}'"
                raise ParseInternalError(emsg.format(stdname))
            # end if
        # end for
        if (not errcode) or (not errmsg):
            raise ParseInternalError("Unsupported error scheme")
        # end if
        return errcode, errmsg

    @staticmethod
    def __errcode_callstr(errcode_name, errmsg_name, suite):
        """Create and return the error code calling string for <suite>.
        <errcode_name> is the calling routine's ccpp_error_code variable name.
        <errmsg_name> is the calling routine's ccpp_error_message variable name.
        """
        err_vars = suite.find_error_variables(any_scope=True, clone_as_out=True)
        errcode, errmsg = ConstituentVarDict.__errcode_names(err_vars)
        errvar_str = "{}={}, {}={}".format(errcode, errcode_name,
                                           errmsg, errmsg_name)
        return errvar_str

    def _write_init_check(self, outfile, indent, suite_name,
                          err_vars, use_errcode):
        """Write a check to <outfile> to make sure the constituent properties
        are initialized. Write code to initialize the error variables and/or
        set them to error values."""
        outfile.write('', 0)
        if use_errcode:
            errcode, errmsg = self.__errcode_names(err_vars)
            outfile.write("{} = 0".format(errcode), indent+1)
            outfile.write("{} = ''".format(errmsg), indent+1)
        else:
            raise ParseInternalError("Alternative to errcode not implemented")
        # end if
        outfile.write("! Make sure that our constituent array is initialized",
                      indent+1)
        stmt = "if (.not. {}) then"
        outfile.write(stmt.format(self.constituent_prop_init_name()), indent+1)
        if use_errcode:
            outfile.write("{} = 1".format(errcode), indent+2)
            stmt = 'errmsg = "constituent properties not '
            stmt += 'initialized for suite, {}"'
            outfile.write(stmt.format(suite_name), indent+2)
            outfile.write("end if", indent+1)
        # end if (no else until an alternative error mechanism supported)

    def _write_index_check(self, outfile, indent, suite_name,
                           err_vars, use_errcode):
        """Write a check to <outfile> to make sure the "index" input
        is in bounds. Write code to set error variables if index is
        out of bounds."""
        if use_errcode:
            errcode, errmsg = self.__errcode_names(err_vars)
            if self:
                outfile.write("if (index < 1) then", indent+1)
                outfile.write("{} = 1".format(errcode), indent+2)
                stmt = "write({}, '(a,i0,a)') 'ERROR: index (',index,') "
                stmt += "too small, must be >= 1'"
                outfile.write(stmt.format(errmsg), indent+2)
                stmt = "else if (index > SIZE({})) then"
                outfile.write(stmt.format(self.constituent_prop_array_name()),
                              indent+1)
                outfile.write("{} = 1".format(errcode), indent+2)
                stmt = "write({}, '(2(a,i0))') 'ERROR: index (',index,') "
                stmt += "too large, must be <= ', SIZE({})"
                outfile.write(stmt.format(errmsg,
                                          self.constituent_prop_array_name()),
                              indent+2)
                outfile.write("end if", indent+1)
            else:
                outfile.write("{} = 1".format(errcode), indent+1)
                stmt = "write({}, '(a,i0,a)') 'ERROR: {}, "
                stmt += "has no constituents'"
                outfile.write(stmt.format(errmsg, self.name), indent+1)
            # end if
        else:
            raise ParseInternalError("Alternative to errcode not implemented")
        # end if

    def write_constituent_routines(self, outfile, indent, suite_name, err_vars):
        """Write the subroutine that, when called allocates and defines the
        suite-cap module variable describing the constituent species for
        this suite.
        Code is written to <outfile> starting at indent, <indent>."""
        # Format our error variables
        errvar_names = {x.get_prop_value('standard_name') :
                        x.get_prop_value('local_name') for x in err_vars}
        errcode_snames = ('ccpp_error_code', 'ccpp_error_message')
        use_errcode = all([x.get_prop_value('standard_name') in errcode_snames
                           for x in err_vars])
        errvar_alist = ", ".join([x for x in errvar_names.values()])
        errvar_alist2 = ", {}".format(errvar_alist) if errvar_alist else ""
        call_vnames = {'ccpp_error_code' : 'errcode',
                       'ccpp_error_message' : 'errmsg'}
        errvar_call = ", ".join(["{}={}".format(call_vnames[x], errvar_names[x])
                                 for x in errcode_snames])
        errvar_call2 = ", {}".format(errvar_call) if errvar_call else ""
        local_call = ", ".join(["{}={}".format(errvar_names[x], errvar_names[x])
                                 for x in errcode_snames])
        # Allocate and define constituents
        stmt = "subroutine {}({})".format(self.constituent_prop_init_consts(),
                                          errvar_alist)
        outfile.write(stmt, indent)
        outfile.write("! Allocate and fill the constituent property array",
                      indent + 1)
        outfile.write("!    for this suite", indent+1)
        outfile.write("! Dummy arguments", indent+1)
        for evar in err_vars:
            evar.write_def(outfile, indent+1, self, dummy=True)
        # end for
        if self:
            outfile.write("! Local variables", indent+1)
            outfile.write("integer :: index", indent+1)
            stmt = "allocate({}({}))"
            outfile.write(stmt.format(self.constituent_prop_array_name(),
                                      len(self)), indent+1)
            outfile.write("index = 0", indent+1)
        # end if
        for std_name, var in self.items():
            outfile.write("index = index + 1", indent+1)
            long_name = var.get_prop_value('long_name')
            dims = var.get_dim_stdnames()
            if 'vertical_layer_dimension' in dims:
                vertical_dim = 'vertical_layer_dimension'
            elif 'vertical_interface_dimension' in dims:
                vertical_dim = 'vertical_interface_dimension'
            else:
                vertical_dim = ''
            # end if
            advect_str = self.TF_string(var.get_prop_value('advected'))
            stmt = 'call {}(index)%initialize("{}", "{}", "{}", {}{})'
            outfile.write(stmt.format(self.constituent_prop_array_name(),
                                      std_name, long_name, vertical_dim,
                                      advect_str, errvar_call2), indent+1)
        # end for
        for evar in err_vars:
            self.__init_err_var(evar, outfile, indent+1)
        # end for
        outfile.write("{} = .true.".format(self.constituent_prop_init_name()),
                      indent+1)
        stmt = "end subroutine {}".format(self.constituent_prop_init_consts())
        outfile.write(stmt, indent)
        outfile.write("", 0)
        outfile.write("\n! {}\n".format("="*72), 1)
        # Return number of constituents
        fname = self.num_consts_funcname()
        outfile.write("integer function {}({})".format(fname, errvar_alist),
                      indent)
        outfile.write("! Return the number of constituents for this suite",
                      indent+1)
        outfile.write("! Dummy arguments", indent+1)
        for evar in err_vars:
            evar.write_def(outfile, indent+1, self, dummy=True)
        # end for
        for evar in err_vars:
            self.__init_err_var(evar, outfile, indent+1)
        # end for
        outfile.write("! Make sure that our constituent array is initialized",
                      indent+1)
        stmt = "if (.not. {}) then"
        outfile.write(stmt.format(self.constituent_prop_init_name()), indent+1)
        outfile.write("call {}({})".format(self.constituent_prop_init_consts(),
                                           local_call), indent+2)
        outfile.write("end if", indent+1)
        outfile.write("{} = {}".format(fname, len(self)), indent+1)
        outfile.write("end function {}".format(fname), indent)
        outfile.write("\n! {}\n".format("="*72), 1)
        # Return the name of a constituent given an index
        stmt = "subroutine {}(index, name_out{})"
        outfile.write(stmt.format(self.const_name_subname(), errvar_alist2),
                      indent)
        outfile.write("! Return the name of constituent, <index>", indent+1)
        outfile.write("! Dummy arguments", indent+1)
        outfile.write("integer,            intent(in)    :: index", indent+1)
        outfile.write("character(len=*),   intent(out)   :: name_out", indent+1)
        for evar in err_vars:
            evar.write_def(outfile, indent+1, self, dummy=True)
        # end for
        self._write_init_check(outfile, indent, suite_name,
                               err_vars, use_errcode)
        self._write_index_check(outfile, indent, suite_name,
                                err_vars, use_errcode)
        if self:
            stmt = "call {}(index)%standard_name(name_out{})"
            outfile.write(stmt.format(self.constituent_prop_array_name(),
                                      errvar_call2), indent+1)
        # end if
        outfile.write("end subroutine {}".format(self.const_name_subname()),
                      indent)
        outfile.write("\n! {}\n".format("="*72), 1)
        # Copy a consitituent's properties
        stmt = "subroutine {}(index, cnst_out{})"
        fname = self.copy_const_subname()
        outfile.write(stmt.format(fname, errvar_alist2), indent)
        outfile.write("! Copy the data for a constituent", indent+1)
        outfile.write("! Dummy arguments", indent+1)
        outfile.write("integer,            intent(in)    :: index", indent+1)
        stmt = "type({}), intent(out)     :: cnst_out"
        outfile.write(stmt.format(self.constituent_prop_type_name()), indent+1)
        for evar in err_vars:
            evar.write_def(outfile, indent+1, self, dummy=True)
        # end for
        self._write_init_check(outfile, indent, suite_name,
                               err_vars, use_errcode)
        self._write_index_check(outfile, indent, suite_name,
                                err_vars, use_errcode)
        if self:
            stmt = "cnst_out = {}(index)"
            outfile.write(stmt.format(self.constituent_prop_array_name()),
                          indent+1)
        # end if
        outfile.write("end subroutine {}".format(fname), indent)

    def constituent_module_name(self):
        """Return the name of host model constituent module"""
        if not ((self.parent is not None) and
                hasattr(self.parent.parent, "constituent_module")):
            emsg = "ConstituentVarDict parent not HostModel?"
            emsg += f"\nparent is '{type_name(self.parent.parent)}'"
            raise ParseInternalError(emsg)
        # end if
        return self.parent.parent.constituent_module

    def num_consts_funcname(self):
        """Return the name of the function which returns the number of
        constituents for this suite."""
        return "{}_num_consts".format(self.name)

    def const_name_subname(self):
        """Return the name of the routine that returns a constituent's
           given an index"""
        return "{}_const_name".format(self.name)

    def copy_const_subname(self):
        """Return the name of the routine that returns a copy of a
           constituent's metadata given an index"""
        return "{}_copy_const".format(self.name)

    @staticmethod
    def constituent_index_name(standard_name):
        """Return the index name associated with <standard_name>"""
        return "index_of_{}".format(standard_name)

    @staticmethod
    def write_constituent_use_statements(cap, suite_list, indent):
        """Write the suite use statements needed by the constituent
        initialization routines."""
        maxmod = max([len(s.module) for s in suite_list])
        smod = len(CONST_DDT_MOD)
        maxmod = max(maxmod, smod)
        use_str = "use {},{} only: {}"
        spc = ' '*(maxmod - smod)
        cap.write(use_str.format(CONST_DDT_MOD, spc, CONST_PROP_TYPE), indent)
        cap.write('! Suite constituent interfaces', indent)
        for suite in suite_list:
            const_dict = suite.constituent_dictionary()
            smod = suite.module
            spc = ' '*(maxmod - len(smod))
            fname = const_dict.num_consts_funcname()
            cap.write(use_str.format(smod, spc, fname), indent)
            fname = const_dict.const_name_subname()
            cap.write(use_str.format(smod, spc, fname), indent)
            fname = const_dict.copy_const_subname()
            cap.write(use_str.format(smod, spc, fname), indent)
        # end for

    @staticmethod
    def write_host_routines(cap, host, reg_funcname, num_const_funcname,
                            copy_in_funcname, copy_out_funcname, const_obj_name,
                            const_names_name, const_indices_name,
                            suite_list, err_vars):
        """Write out the host model <reg_funcname> routine which will
        instantiate constituent fields for all the constituents in <suite_list>.
        <err_vars> is a list of the host model's error variables.
        Also write out the following routines:
           <num_const_funcname>: Number of constituents
           <copy_in_funcname>: Collect constituent fields for host
           <copy_out_funcname>: Update constituent fields from host
        Output is written to <cap>.
        """
# XXgoldyXX: v need to generalize host model error var type support
        use_errcode = [x.get_prop_value('standard_name') in
                       ('ccpp_error_code' 'ccpp_error_message')
                       for x in err_vars]
        if not use_errcode:
            emsg = "Error object not supported for {}"
            raise ParseInternalError(emsg(host.name))
        # end if
        herrcode, herrmsg = ConstituentVarDict.__errcode_names(err_vars)
        err_dummy_str = "{errcode}, {errmsg}".format(errcode=herrcode,
                                                     errmsg=herrmsg)
        obj_err_callstr = "errcode={errcode}, errmsg={errmsg}"
        obj_err_callstr = obj_err_callstr.format(errcode=herrcode,
                                                 errmsg=herrmsg)
# XXgoldyXX: ^ need to generalize host model error var type support
        # First up, the registration routine
        substmt = "subroutine {}".format(reg_funcname)
        stmt = "{}(suite_list, ncols, num_layers, num_interfaces, {})"
        cap.write(stmt.format(substmt, err_dummy_str), 1)
        cap.write("! Create constituent object for suites in <suite_list>", 2)
        cap.write("", 0)
        ConstituentVarDict.write_constituent_use_statements(cap, suite_list, 2)
        cap.write("", 0)
        cap.write("! Dummy arguments", 2)
        cap.write("character(len=*),   intent(in)    :: suite_list(:)", 2)
        cap.write("integer,            intent(in)    :: ncols", 2)
        cap.write("integer,            intent(in)    :: num_layers", 2)
        cap.write("integer,            intent(in)    :: num_interfaces", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for
        cap.write("! Local variables", 2)
        spc = ' '*37
        cap.write("integer{} :: num_suite_consts".format(spc), 2)
        cap.write("integer{} :: num_consts".format(spc), 2)
        cap.write("integer{} :: index".format(spc), 2)
        cap.write("integer{} :: field_ind".format(spc), 2)
        cap.write("type({}), pointer :: const_prop".format(CONST_PROP_TYPE), 2)
        cap.write("", 0)
        cap.write("{} = 0".format(herrcode), 2)
        cap.write("num_consts = 0", 2)
        for suite in suite_list:
            const_dict = suite.constituent_dictionary()
            funcname = const_dict.num_consts_funcname()
            cap.write("! Number of suite constants for {}".format(suite.name),
                      2)
            errvar_str = ConstituentVarDict.__errcode_callstr(herrcode,
                                                              herrmsg, suite)
            cap.write("num_suite_consts = {}({})".format(funcname,
                                                         errvar_str), 2)
            cap.write("num_consts = num_consts + num_suite_consts", 2)
        # end for
        cap.write("if ({} == 0) then".format(herrcode), 2)
        cap.write("! Initialize constituent data and field object", 3)
        stmt = "call {}%initialize_table(num_consts)"
        cap.write(stmt.format(const_obj_name), 3)
        cap.write("end if", 2)
        for suite in suite_list:
            errvar_str = ConstituentVarDict.__errcode_callstr(herrcode,
                                                              herrmsg, suite)
            cap.write("if ({} == 0) then".format(herrcode), 2)
            cap.write("! Add {} constituent metadata".format(suite.name), 3)
            const_dict = suite.constituent_dictionary()
            funcname = const_dict.num_consts_funcname()
            cap.write("num_suite_consts = {}({})".format(funcname,
                                                         errvar_str), 3)
            cap.write("end if", 2)
            funcname = const_dict.copy_const_subname()
            cap.write("do index = 1, num_suite_consts", 2)
            cap.write("allocate(const_prop, stat={})".format(herrcode), 3)
            cap.write("if ({} /= 0) then".format(herrcode), 3)
            cap.write('{} = "ERROR allocating const_prop"'.format(herrmsg), 4)
            cap.write("end if", 3)
            cap.write("if ({} == 0) then".format(herrcode), 3)
            stmt = "call {}(index, const_prop, {})"
            cap.write(stmt.format(funcname, errvar_str), 4)
            cap.write("end if", 3)
            cap.write("if ({} == 0) then".format(herrcode), 3)
            stmt = "call {}%new_field(const_prop, {})"
            cap.write(stmt.format(const_obj_name, obj_err_callstr), 4)
            cap.write("end if", 3)
            cap.write("nullify(const_prop)", 3)
            cap.write("if ({} /= 0) then".format(herrcode), 3)
            cap.write("exit", 4)
            cap.write("end if", 3)
            cap.write("end do", 2)
            cap.write("", 0)
        # end for
        cap.write("if ({} == 0) then".format(herrcode), 2)
        stmt = "call {}%lock_table(ncols, num_layers, num_interfaces, {})"
        cap.write(stmt.format(const_obj_name, obj_err_callstr), 3)
        cap.write("end if", 2)
        cap.write("! Set the index for each active constituent", 2)
        cap.write("do index = 1, SIZE({})".format(const_indices_name), 2)
        stmt = "field_ind = {}%field_index({}(index), {})"
        cap.write(stmt.format(const_obj_name, const_names_name,
                              obj_err_callstr), 3)
        cap.write("if (field_ind > 0) then", 3)
        cap.write("{}(index) = field_ind".format(const_indices_name), 4)
        cap.write("else", 3)
        cap.write("{} = 1".format(herrcode), 4)
        stmt = "{} = 'No field index for '//trim({}(index))"
        cap.write(stmt.format(herrmsg, const_names_name), 4)
        cap.write("end if", 3)
        cap.write("if ({} /= 0) then".format(herrcode), 3)
        cap.write("exit", 4)
        cap.write("end if", 3)
        cap.write("end do", 2)
        cap.write("end {}".format(substmt), 1)
        # Next, write num_consts routine
        substmt = "function {}".format(num_const_funcname)
        cap.write("", 0)
        cap.write("integer {}({})".format(substmt, err_dummy_str), 1)
        cap.write("! Return the number of constituent fields for this run", 2)
        cap.write("", 0)
        cap.write("! Dummy arguments", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for
        cap.write("", 0)
        cap.write("{} = {}%num_constituents({})".format(num_const_funcname,
                                                        const_obj_name,
                                                        obj_err_callstr), 2)
        cap.write("end {}".format(substmt), 1)
        # Next, write copy_in routine
        substmt = "subroutine {}".format(copy_in_funcname)
        cap.write("", 0)
        cap.write("{}(const_array, {})".format(substmt, err_dummy_str), 1)
        cap.write("! Copy constituent field info into <const_array>", 2)
        cap.write("", 0)
        cap.write("! Dummy arguments", 2)
        cap.write("real(kind_phys),    intent(out)   :: const_array(:,:,:)", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for
        cap.write("", 0)
        cap.write("call {}%copy_in(const_array, {})".format(const_obj_name,
                                                            obj_err_callstr), 2)
        cap.write("end {}".format(substmt), 1)
        # Next, write copy_out routine
        substmt = "subroutine {}".format(copy_out_funcname)
        cap.write("", 0)
        cap.write("{}(const_array, {})".format(substmt, err_dummy_str), 1)
        cap.write("! Update constituent field info from <const_array>", 2)
        cap.write("", 0)
        cap.write("! Dummy arguments", 2)
        cap.write("real(kind_phys),    intent(in)    :: const_array(:,:,:)", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for
        cap.write("", 0)
        cap.write("call {}%copy_out(const_array, {})".format(const_obj_name,
                                                             obj_err_callstr), 2)
        cap.write("end {}".format(substmt), 1)

    @staticmethod
    def constitutent_source_type():
        """Return the source type for constituent species"""
        return ConstituentVarDict.__constituent_type

    @staticmethod
    def constituent_prop_array_name():
        """Return the name of the constituent properties array for this suite"""
        return ConstituentVarDict.__const_prop_array_name

    @staticmethod
    def constituent_prop_init_name():
        """Return the name of the array initialized flag for this suite"""
        return ConstituentVarDict.__const_prop_init_name

    @staticmethod
    def constituent_prop_init_consts():
        """Return the name of the routine to initialize the constituent
        properties array for this suite"""
        return ConstituentVarDict.__const_prop_init_consts

    @staticmethod
    def constituent_prop_type_name():
        """Return the name of the derived type which holds constituent
        properties."""
        return ConstituentVarDict.__const_prop_type_name

    @staticmethod
    def write_suite_use(outfile, indent):
        """Write use statements for any modules needed by the suite cap.
        The statements are written to <outfile> at indent, <indent>.
        """
        omsg = "use ccpp_constituent_prop_mod, only: {}"
        cpt_name = ConstituentVarDict.constituent_prop_type_name()
        outfile.write(omsg.format(cpt_name), indent)

    @staticmethod
    def TF_string(tf_val):
        """Return a string of the Fortran equivalent of <tf_val>"""
        if tf_val:
            tf_str = ".true."
        else:
            tf_str = ".false."
        # end if
        return tf_str
