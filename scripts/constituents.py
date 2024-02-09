#!/usr/bin/env python3

"""
Class and supporting code to hold all information on CCPP constituent
variables. A constituent variable is defined and maintained by the CCPP
Framework instead of the host model.
The ConstituentVarDict class contains methods to generate the necessary code
to implement this support.
"""

# CCPP framework imports
from parse_tools import ParseInternalError
from metavar import VarDictionary

########################################################################

CONST_DDT_NAME = "ccpp_model_constituents_t"
CONST_DDT_MOD = "ccpp_constituent_prop_mod"
CONST_PROP_TYPE = "ccpp_constituent_properties_t"
CONST_PROP_PTR_TYPE = "ccpp_constituent_prop_ptr_t"
CONST_OBJ_STDNAME = "ccpp_model_constituents_object"

########################################################################

class ConstituentVarDict(VarDictionary):
    """A class to hold all the constituent variables for a CCPP Suite.
    Also contains methods to generate the necessary code for runtime
    allocation and support for these variables.
    """

    __const_prop_array_name  = "ccpp_constituents"
    __const_prop_init_name  = "ccpp_constituents_initialized"
    __const_prop_init_consts = "ccpp_create_constituent_array"
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
        super().__init__(name, run_env,
                         variables=variables, parent_dict=parent_dict)

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
            outfile.write(stmt.format(CONST_PROP_TYPE,
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
        for evar in err_vars:
            self.__init_err_var(evar, outfile, indent+1)
        # end for
        for std_name, var in self.items():
            outfile.write("index = index + 1", indent+1)
            long_name = var.get_prop_value('long_name')
            units = var.get_prop_value('units')
            dims = var.get_dim_stdnames()
            default_value = var.get_prop_value('default_value')
            if 'vertical_layer_dimension' in dims:
                vertical_dim = 'vertical_layer_dimension'
            elif 'vertical_interface_dimension' in dims:
                vertical_dim = 'vertical_interface_dimension'
            else:
                vertical_dim = ''
            # end if
            advect_str = self.TF_string(var.get_prop_value('advected'))
            init_args = [f'{std_name=}', f'{long_name=}',
                         f'{units=}', f'{vertical_dim=}',
                         f'advected={advect_str}',
                         f'errcode={errvar_names["ccpp_error_code"]}',
                         f'errmsg={errvar_names["ccpp_error_message"]}']
            if default_value is not None and default_value != '':
                init_args.append(f'default_value={default_value}')
            stmt = 'call {}(index)%instantiate({})'
            outfile.write(f'if ({errvar_names["ccpp_error_code"]} == 0) then', indent+1)
            outfile.write(stmt.format(self.constituent_prop_array_name(),
                                      ", ".join(init_args)), indent+2)
            outfile.write("end if", indent+1)
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
            init_args = ['std_name=name_out',
                         f'errcode={errvar_names["ccpp_error_code"]}',
                         f'errmsg={errvar_names["ccpp_error_message"]}']
            stmt = "call {}(index)%standard_name({})"
            outfile.write(stmt.format(self.constituent_prop_array_name(),
                                      ", ".join(init_args)), indent+1)
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
        stmt = f"type({CONST_PROP_TYPE}), intent(out)     :: cnst_out"
        outfile.write(stmt, indent+1)
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
    def write_host_routines(cap, host, reg_funcname, init_funcname, num_const_funcname,
                            query_const_funcname, copy_in_funcname, copy_out_funcname,
                            const_obj_name, const_names_name, const_indices_name,
                            const_array_func, advect_array_func, prop_array_func,
                            const_index_func, suite_list, dyn_const_dict, err_vars):
        """Write out the host model <reg_funcname> routine which will
        instantiate constituent fields for all the constituents in <suite_list>.
        <err_vars> is a list of the host model's error variables.
        <dyn_const_dict> is a dictionary (key=scheme name) of dynamic constituent routines
        Also write out the following routines:
           <init_funcname>: Initialize constituent data
           <num_const_funcname>: Number of constituents
           <query_const_funcname>: Check if standard name matches existing constituent
           <copy_in_funcname>: Collect constituent fields for host
           <copy_out_funcname>: Update constituent fields from host
           <const_array_func>: Return a pointer to the constituent array
           <advect_array_func>: Return a pointer to the advected constituent array
           <prop_array_func>: Return a pointer to the constituent properties array
           <const_index_func>: Return the index of a provided constituent name
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
        substmt = f"subroutine {reg_funcname}"
        args = "suite_list, host_constituents, dynamic_constituents "
        stmt = f"{substmt}({args}, {err_dummy_str})"
        cap.write(stmt, 1)
        cap.comment("Create constituent object for suites in <suite_list>", 2)
        cap.blank_line()
        ConstituentVarDict.write_constituent_use_statements(cap, suite_list, 2)
        # Conditionally include use statements for dynamic constituent routines
        if len(dyn_const_dict) > 0:
            cap.comment("Dynamic constituent routines", 2)
        for scheme in dyn_const_dict:
            cap.write(f"use {scheme}, only: {dyn_const_dict[scheme]}", 2)
        cap.blank_line()
        cap.comment("Dummy arguments", 2)
        cap.write("character(len=*), intent(in)  :: suite_list(:)", 2)
        cap.write(f"type({CONST_PROP_TYPE}), target, intent(in)  :: " +       \
                  "host_constituents(:)", 2)
        cap.write(f"type({CONST_PROP_TYPE}), allocatable, target, intent(inout) :: " +   \
                  "dynamic_constituents(:)", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for
        cap.comment("Local variables", 2)
        spc = ' '*37
        cap.write(f"integer{spc} :: num_suite_consts", 2)
        cap.write(f"integer{spc} :: num_consts", 2)
        cap.write(f"integer{spc} :: num_dyn_consts", 2)
        cap.write(f"integer{spc} :: index, index_start", 2)
        cap.write(f"integer{spc} :: field_ind", 2)
        cap.write(f"type({CONST_PROP_TYPE}), pointer :: const_prop", 2)
        # Declare dynamic constituent properties variables
        for idx, scheme in enumerate(sorted(dyn_const_dict)):
            cap.comment(f"dynamic constituent props variable for {scheme}", 2)
            cap.write(f"type({CONST_PROP_TYPE}), allocatable, target :: dyn_const_prop_{idx}(:)", 2)
        # end for
        cap.blank_line()
        cap.write("{} = 0".format(herrcode), 2)
        cap.write("num_consts = size(host_constituents, 1)", 2)
        cap.write("num_dyn_consts = 0", 2)
        for suite in suite_list:
            const_dict = suite.constituent_dictionary()
            funcname = const_dict.num_consts_funcname()
            cap.comment(f"Number of suite constants for {suite.name}", 2)
            errvar_str = ConstituentVarDict.__errcode_callstr(herrcode,
                                                              herrmsg, suite)
            cap.write(f"num_suite_consts = {funcname}({errvar_str})", 2)
            cap.write("num_consts = num_consts + num_suite_consts", 2)
        # end for
        # Check for dynamic constituent routines
        if len(dyn_const_dict) > 0:
            cap.comment("Add in dynamic constituents", 2)
            for idx, scheme in enumerate(sorted(dyn_const_dict)):
                cap.write(f"if ({herrcode} == 0) then", 2)
                cap.write(f"call {dyn_const_dict[scheme]}(dyn_const_prop_{idx}, {obj_err_callstr})", 3)
                cap.write(f"num_dyn_consts = num_dyn_consts + size(dyn_const_prop_{idx})", 3)
                cap.write("end if", 2)
            # end for
            cap.write("num_consts = num_consts + num_dyn_consts", 2)
            cap.comment("Pack dynamic_constituents array", 2)
            cap.write(f"allocate(dynamic_constituents(num_dyn_consts), stat={herrcode})", 2)
            cap.write(f"if ({herrcode} /= 0) then", 2)
            cap.write("errmsg = 'failed to allocate dynamic_constituents'", 3)
            cap.write("else", 2)
            cap.write("index_start = 1", 3)
            for idx, scheme in enumerate(sorted(dyn_const_dict)):
                cap.write(f"do index = 1, size(dyn_const_prop_{idx}, 1)", 3)
                cap.write(f"dynamic_constituents(index + index_start - 1) = dyn_const_prop_{idx}(index)", 4)
                cap.write("end do", 3)
                cap.write(f"index_start = size(dyn_const_prop_{idx}, 1) + 1", 3)
                cap.write(f"deallocate(dyn_const_prop_{idx})", 3)
            cap.write("end if", 2)
        # end if
        cap.write("if ({} == 0) then".format(herrcode), 2)
        cap.comment("Initialize constituent data and field object", 3)
        stmt = "call {}%initialize_table(num_consts)"
        cap.write(stmt.format(const_obj_name), 3)
        # Register host model constituents
        cap.comment("Add host model constituent metadata", 3)
        cap.write("do index = 1, size(host_constituents, 1)", 3)
        cap.write(f"if ({herrcode} == 0) then", 4)
        cap.write("const_prop => host_constituents(index)", 5)
        stmt = "call {}%new_field(const_prop, {})"
        cap.write(stmt.format(const_obj_name, obj_err_callstr), 5)
        cap.write("end if", 4)
        cap.write("nullify(const_prop)", 4)
        cap.write("if ({} /= 0) then".format(herrcode), 4)
        cap.write("exit", 5)
        cap.write("end if", 4)
        cap.write("end do", 3)
        cap.write("end if", 2)
        cap.blank_line()
        # Register dynamic constituents
        if len(dyn_const_dict) > 0:
           cap.comment("Add dynamic constituent properties", 2)
           cap.write(f"if ({herrcode} == 0) then", 2)
           cap.write(f"do index = 1, size(dynamic_constituents, 1)", 3)
           cap.write(f"if ({herrcode} == 0) then", 4)
           cap.write(f"const_prop => dynamic_constituents(index)", 5)
           stmt = "call {}%new_field(const_prop, {})"
           cap.write(stmt.format(const_obj_name, obj_err_callstr), 5)
           cap.write("end if", 4)
           cap.write("nullify(const_prop)", 4)
           cap.write(f"if ({herrcode} /= 0) then", 4)
           cap.write("exit", 5)
           cap.write("end if", 4)
           cap.write("end do", 3)
           cap.write("end if", 2)
        # end if
        
        # Register suite constituents
        for suite in suite_list:
            errvar_str = ConstituentVarDict.__errcode_callstr(herrcode,
                                                              herrmsg, suite)
            cap.write(f"if ({herrcode} == 0) then", 2)
            cap.comment(f"Add {suite.name} constituent metadata", 3)
            const_dict = suite.constituent_dictionary()
            funcname = const_dict.num_consts_funcname()
            cap.write(f"num_suite_consts = {funcname}({errvar_str})", 3)
            cap.write("end if", 2)
            funcname = const_dict.copy_const_subname()
            cap.write(f"if ({herrcode} == 0) then", 2)
            cap.write("do index = 1, num_suite_consts", 3)
            cap.write(f"if ({herrcode} == 0) then", 4)
            cap.write(f"allocate(const_prop, stat={herrcode})", 5)
            cap.write("end if", 4)
            cap.write(f"if ({herrcode} /= 0) then", 4)
            cap.write(f'{herrmsg} = "ERROR allocating const_prop"', 5)
            cap.write("exit", 5)
            cap.write("end if", 4)
            cap.write(f"if ({herrcode} == 0) then", 4)
            stmt = "call {}(index, const_prop, {})"
            cap.write(stmt.format(funcname, errvar_str), 5)
            cap.write("end if", 4)
            cap.write(f"if ({herrcode} == 0) then", 4)
            stmt = "call {}%new_field(const_prop, {})"
            cap.write(stmt.format(const_obj_name, obj_err_callstr), 5)
            cap.write("end if", 4)
            cap.write("nullify(const_prop)", 4)
            cap.write(f"if ({herrcode} /= 0) then", 4)
            cap.write("exit", 5)
            cap.write("end if", 4)
            cap.write("end do", 3)
            cap.write("end if", 2)
            cap.blank_line()
        # end for
        cap.write(f"if ({herrcode} == 0) then", 2)
        stmt = "call {}%lock_table({})"
        cap.write(stmt.format(const_obj_name, obj_err_callstr), 3)
        cap.write("end if", 2)
        cap.write(f"if ({herrcode} == 0) then", 2)
        cap.comment("Set the index for each active constituent", 3)
        cap.write(f"do index = 1, SIZE({const_indices_name})", 3)
        stmt = "call {}%const_index(field_ind, {}(index), {})"
        cap.write(stmt.format(const_obj_name, const_names_name,
                              obj_err_callstr), 4)
        cap.write("if (field_ind > 0) then", 4)
        cap.write(f"{const_indices_name}(index) = field_ind", 5)
        cap.write("else", 4)
        cap.write(f"{herrcode} = 1", 5)
        stmt = "{} = 'No field index for '//trim({}(index))"
        cap.write(stmt.format(herrmsg, const_names_name), 5)
        cap.write("end if", 4)
        cap.write(f"if ({herrcode} /= 0) then", 4)
        cap.write("exit", 5)
        cap.write("end if", 4)
        cap.write("end do", 3)
        cap.write("end if", 2)
        cap.write(f"end {substmt}", 1)
        # Write constituent_init routine
        substmt = f"subroutine {init_funcname}"
        cap.blank_line()
        cap.write(f"{substmt}(ncols, num_layers, {err_dummy_str})", 1)
        cap.comment("Initialize constituent data", 2)
        cap.blank_line()
        cap.comment("Dummy arguments", 2)
        cap.write("integer,            intent(in)    :: ncols", 2)
        cap.write("integer,            intent(in)    :: num_layers", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for evar
        cap.blank_line()
        call_str = f"call {const_obj_name}%lock_data(ncols, num_layers, {obj_err_callstr})"
        cap.write(call_str, 2)
        cap.write(f"end {substmt}", 1)
        # Write num_consts routine
        substmt = f"subroutine {num_const_funcname}"
        cap.blank_line()
        cap.write(f"{substmt}(num_flds, advected, {err_dummy_str})", 1)
        cap.comment("Return the number of constituent fields for this run", 2)
        cap.blank_line()
        cap.comment("Dummy arguments", 2)
        cap.write("integer,            intent(out)   :: num_flds", 2)
        cap.write("logical, optional,  intent(in)    :: advected", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for
        cap.blank_line()
        call_str = "call {}%num_constituents(num_flds, advected=advected, {})"
        cap.write(call_str.format(const_obj_name, obj_err_callstr), 2)
        cap.write("end {}".format(substmt), 1)
        # Write query_consts routine
        substmt = f"subroutine {query_const_funcname}"
        cap.blank_line()
        cap.write(f"{substmt}(var_name, constituent_exists, {err_dummy_str})", 1)
        cap.comment(f"Return constituent_exists = true iff var_name appears in {host.name}_model_const_stdnames", 2)
        cap.blank_line()
        cap.write("character(len=*),   intent(in)    :: var_name", 2)
        cap.write("logical,            intent(out)   :: constituent_exists", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for
        cap.blank_line()
        cap.write(f"{herrcode} = 0", 2)
        cap.write(f"{herrmsg} = ''", 2)
        cap.blank_line()
        cap.write("constituent_exists = .false.", 2)
        cap.write(f"if (any({host.name}_model_const_stdnames == var_name)) then", 2)
        cap.write("constituent_exists = .true.", 3)
        cap.write("end if", 2)
        cap.blank_line()
        cap.write(f"end {substmt}", 1)
        # Write copy_in routine
        substmt = "subroutine {}".format(copy_in_funcname)
        cap.blank_line()
        cap.write("{}(const_array, {})".format(substmt, err_dummy_str), 1)
        cap.comment("Copy constituent field info into <const_array>", 2)
        cap.blank_line()
        cap.comment("Dummy arguments", 2)
        cap.write("real(kind_phys),    intent(out)   :: const_array(:,:,:)", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for
        cap.blank_line()
        cap.write("call {}%copy_in(const_array, {})".format(const_obj_name,
                                                            obj_err_callstr), 2)
        cap.write("end {}".format(substmt), 1)
        # Write copy_out routine
        substmt = "subroutine {}".format(copy_out_funcname)
        cap.blank_line()
        cap.write("{}(const_array, {})".format(substmt, err_dummy_str), 1)
        cap.comment("Update constituent field info from <const_array>", 2)
        cap.blank_line()
        cap.comment("Dummy arguments", 2)
        cap.write("real(kind_phys),    intent(in)    :: const_array(:,:,:)", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for
        cap.blank_line()
        cap.write("call {}%copy_out(const_array, {})".format(const_obj_name,
                                                             obj_err_callstr),
                  2)
        cap.write("end {}".format(substmt), 1)
        # Write constituents routine
        cap.blank_line()
        cap.write(f"function {const_array_func}() result(const_ptr)", 1)
        cap.blank_line()
        cap.comment("Return pointer to constituent array", 2)
        cap.blank_line()
        cap.comment("Dummy argument", 2)
        cap.write("real(kind_phys), pointer :: const_ptr(:,:,:)", 2)
        cap.blank_line()
        cap.write(f"const_ptr => {const_obj_name}%field_data_ptr()", 2)
        cap.write(f"end function {const_array_func}", 1)
        # Write advected constituents routine
        cap.blank_line()
        cap.write(f"function {advect_array_func}() result(const_ptr)", 1)
        cap.blank_line()
        cap.comment("Return pointer to advected constituent array", 2)
        cap.blank_line()
        cap.comment("Dummy argument", 2)
        cap.write("real(kind_phys), pointer :: const_ptr(:,:,:)", 2)
        cap.blank_line()
        cap.write(f"const_ptr => {const_obj_name}%advected_constituents_ptr()",
                  2)
        cap.write(f"end function {advect_array_func}", 1)
        # Write the constituent property array routine
        cap.blank_line()
        cap.write(f"function {prop_array_func}() result(const_ptr)", 1)
        cap.write(f"use {CONST_DDT_MOD}, only: {CONST_PROP_PTR_TYPE}", 2)
        cap.blank_line()
        cap.comment("Return pointer to array of constituent properties", 2)
        cap.blank_line()
        cap.comment("Dummy argument", 2)
        cap.write("type(ccpp_constituent_prop_ptr_t), pointer :: const_ptr(:)",
                  2)
        cap.blank_line()
        cap.write(f"const_ptr => {const_obj_name}%constituent_props_ptr()",
                  2)
        cap.write(f"end function {prop_array_func}", 1)
        # Write constituent index function
        substmt = f"subroutine {const_index_func}"
        cap.blank_line()
        cap.write(f"{substmt}(stdname, const_index, {err_dummy_str})", 1)
        cap.comment("Set <const_index> to the constituent array index " +     \
                    "for <stdname>.", 2)
        cap.comment("If <stdname> is not found, set <const_index> to -1 " +   \
                    "set an error condition", 2)
        cap.blank_line()
        cap.comment("Dummy arguments", 2)
        cap.write("character(len=*),    intent(in)    :: stdname", 2)
        cap.write("integer,             intent(out)   :: const_index", 2)
        for evar in err_vars:
            evar.write_def(cap, 2, host, dummy=True, add_intent="out")
        # end for
        cap.blank_line()
        cap.write(f"call {const_obj_name}%const_index(const_index, " +        \
                  f"stdname, {obj_err_callstr})", 2)
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
    def write_suite_use(outfile, indent):
        """Write use statements for any modules needed by the suite cap.
        The statements are written to <outfile> at indent, <indent>.
        """
        omsg = f"use ccpp_constituent_prop_mod, only: {CONST_PROP_TYPE}"
        outfile.write(omsg, indent)

    @staticmethod
    def TF_string(tf_val):
        """Return a string of the Fortran equivalent of <tf_val>"""
        if tf_val:
            tf_str = ".true."
        else:
            tf_str = ".false."
        # end if
        return tf_str
