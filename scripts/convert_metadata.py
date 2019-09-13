#!/usr/bin/env python

# Python library imports
import sys
import os.path
import re
from collections import OrderedDict
import logging
# CCPP framework imports
from parse_tools import FORTRAN_ID, init_log, set_log_level
from fortran_tools import parse_fortran_file
from common import split_var_name_and_array_reference

yes_re = re.compile(r"(?i)^\s*yes\s*$")
module_re = re.compile(r"(?i)\s*module\s+"+(FORTRAN_ID)+r"\s*.*$")
end_module_re = re.compile(r"(?i)\s*end\s*module\s+"+(FORTRAN_ID)+r"\s*.*$")
type_re = re.compile(r"(?i)\s*type\s+"+(FORTRAN_ID)+r"\s*.*$")
end_type_re = re.compile(r"(?i)\s*end\s*type\s+"+(FORTRAN_ID)+r"\s*.*$")
required_attrs = ['standard_name', 'units', 'dimensions', 'type']
warning = True
__not_found__ = 'XX_NotFound_XX'

# Configured models
MODELS = ['FV3']

########################################################################

def next_line(lines, max_line, cindex=-1):
    nindex = cindex + 1
    if nindex > max_line:
        return None, -1
    else:
        return lines[nindex].rstrip('\n'), nindex

########################################################################

def parse_module_line(line, mod_name):
    match = module_re.match(line)
    if match is not None:
        mod_name = match.group(1)
    else:
        match = end_module_re.match(line)
        if match is not None:
            mod_name = None
        # End if
    # End if
    return mod_name

########################################################################

class MetadataEntry(OrderedDict):

    def __init__(self, local_name):
        self._name = local_name
        super(MetadataEntry, self).__init__()

    @property
    def local_name(self):
        return self._name

    def write(self, mdfile):
        mdfile.write('[{}]\n'.format(self.local_name))
        for key in self.keys():
            mdfile.write("  {} = {}\n".format(key, self[key]))
        # End for

########################################################################

class MetadataTable(OrderedDict):

    def __init__(self, table_name, mod_name):
        self._name = table_name
        if (mod_name is not None) and (mod_name.lower() == table_name.lower()):
            self._type = 'module'
        elif table_name.split('_')[-1].lower() == 'type':
            self._type = 'ddt'
        else:
            self._type = 'scheme'
        # End if
        super(MetadataTable, self).__init__()

    @property
    def name(self):
        return self._name

    @property
    def type(self):
        return self._type

    def has(self, varname):
        hasvar = False
        vartest = varname.lower()
        for name in self.keys():
            if vartest == name.lower():
                hasvar = True
                break
            # End if
        # End for
        return hasvar

    def get(self, varname):
        var = None
        vartest = varname.lower()
        for name in self.keys():
            if vartest == name.lower():
                var = self[name]
                break
            # End if
        # End for
        return var

    def write(self, mdfile):
        mdfile.write('[ccpp-arg-table]\n')
        mdfile.write('  name = {}\n'.format(self._name))
        mdfile.write('  type = {}\n'.format(self._type))
        for key in self.keys():
            self[key].write(mdfile)

########################################################################

def convert_file(filename_in, filename_out, metadata_filename_out, model, logger=None):
    """Convert a file's old metadata to the new format
    Note that only the bare minimum error checking is done.
    """
    if logger:
        logger.info("Converting file {} ...".format(filename_in))
    else:
        print "Converting file {} ...".format(filename_in)
    current_module = None
    # First, suck in the old file
    do_convert = True
    if not os.path.exists(filename_in):
        raise IOError("convert_file: file, '{}', does not exist".format(filename_in))
    # End if
    if os.path.exists(filename_out):
        raise IOError("convert_file: file, '{}', already exists".format(filename_out))
    # End if

    # Lookup table local_name -> standard_name with data from ccpp_types.F90
    standard_names = {
        'cdata%blk_no'  : 'ccpp_block_number',
        'cdata%thrd_no' : 'ccpp_thread_number',
        'cdata%errflg'  : 'ccpp_error_flag',
        'cdata%errmsg'  : 'ccpp_error_message',
        'cdata%loop_cnt': 'ccpp_loop_counter',
        }
    # Lookup table local_name -> dimensions
    dimensions = {}

    # Read all lines of the file at once
    with open(filename_in, 'r') as file:
        fin_lines = file.readlines()
        for index in xrange(len(fin_lines)):
            fin_lines[index] = fin_lines[index].rstrip('\n')
            # First loop through file to build dictionary with local names versus standard names
            # and to record array dimensions from allocate statements
            words = fin_lines[index].split('|')
            if len(words)>=11:
                # Create a dictionary with local names versus standard names in file
                if words[0].strip() == '!!' and not words[1].strip() == 'local_name' and not words[2].strip() == 'standard_name' \
                        and not "---" in words[1].strip() and not "---" in words[2].strip() :
                    local_name = words[1].strip().lower()
                    standard_name = words[2].strip()
                    if not standard_name:
                        continue
                    # No duplicates allowed
                    if local_name in standard_names.keys():
                        raise Exception("Multiple definitions of local name {}".format(local_name))
                    standard_names[local_name] = standard_name
            elif 'allocate' in fin_lines[index]:
                # Find all allocate statements to identify the correct dimensions
                line_stripped = fin_lines[index].replace(' ','')
                if 'allocate(' in line_stripped:
                    var_and_dims = line_stripped[line_stripped.find("allocate(")+9:line_stripped.rfind(")")]
                    # Variable to allocate, replace code with text used in metadata
                    var = var_and_dims[:var_and_dims.find("(")].lower()
                    #
                    # Begin model and file-dependent substitutions
                    if model == 'FV3':
                        if "GFS_typedefs" in filename_in:
                            var = var.replace("model%","gfs_control%")
                            var = var.replace("interstitial%","gfs_interstitial(cdata%thrd_no)%")
                        elif "CCPP_typedefs" in filename_in:
                            var = var.replace("interstitial%","ccpp_interstitial%")
                    # End model and file-dependent substitutions
                    #
                    # Dimensions to use, replace code with text used in metadata
                    dims = var_and_dims[var_and_dims.find("(")+1:var_and_dims.rfind(")")].split(',')
                    dims = [dim.lower() for dim in dims]
                    #
                    # Begin model and file-dependent substitutions
                    if model == 'FV3':
                        if "GFS_typedefs" in filename_in:
                            dims = [dim.replace("model%","gfs_control%") for dim in dims]
                            dims = [dim.replace("interstitial%","gfs_interstitial(cdata%thrd_no)%") for dim in dims]
                        elif "CCPP_typedefs" in filename_in:
                            dims = [dim.replace("interstitial%","ccpp_interstitial%") for dim in dims]
                        # Special handling of certain variables with multiple allocation lines in GFS_typedefs.F90 / CCPP_typedefs.F90
                        if var == 'Diag%dq3dt'.lower():
                            dims = ['im', 'gfs_control%levs', 'oz_coeff+5']
                        elif var == 'ccpp_interstitial%cappa'.lower():
                            dims = ['isd:ied', 'jsd:jed', '1:npzcappa']
                        elif var in dimensions.keys() and not dims == dimensions[var]:
                            raise Exception("Multiple, conflicting allocations of variable with local name {}: {} vs {}".format(
                                            var, dimensions[var], dims))
                    # End model and file-dependent substitutions
                    else:
                        if var in dimensions.keys() and not dims == dimensions[var]:
                            raise Exception("Multiple, conflicting allocations of variable with local name {}: {} vs {}".format(
                                            var, dimensions[var], dims))
                    dimensions[var] = dims
                    # End if
                # End if
            # End if
        # End for
    # End with

    # Begin model and file-dependent substitutions
    if model == 'FV3':
        # Replace local dimensions in GFS_typedefs.F90, CCPP_typedefs.F90 and CCPP_data.F90 with correct standard names
        for key in dimensions.keys():
            for i in xrange(len(dimensions[key])):
                dim = dimensions[key][i]
                if dim == 'im':
                    dimensions[key][i] = 'horizontal_dimension'
                elif dim == 'interstitial%nvdiff':
                    dimensions[key][i] = 'number_of_vertical_diffusion_tracers'
                elif dim == 'interstitial%nn':
                    dimensions[key][i] = 'number_of_tracers_for_convective_transport'
                elif dim == 'gfs_control%levr+1':
                    dimensions[key][i] = 'number_of_vertical_layers_for_radiation_calculations_plus_one'
                elif dim == 'gfs_control%levs+1':
                    dimensions[key][i] = 'vertical_dimension_plus_one'
                elif dim == 'gfs_control%levs-1':
                    dimensions[key][i] = 'vertical_dimension_minus_one'
                elif dim == 'gfs_control%levr+ltp':
                    dimensions[key][i] = 'adjusted_vertical_layer_dimension_for_radiation'
                elif dim == 'gfs_control%levr+1+ltp':
                    dimensions[key][i] = 'adjusted_vertical_level_dimension_for_radiation'
                elif dim in [ '-2:4', '4', '-2:0', '1:4', '6', '2', '3', '5', '7' ]:
                    continue
                elif dim == 'levh2o':
                    dimensions[key][i] = 'vertical_dimension_of_h2o_forcing_data'
                elif dim == 'h2o_coeff':
                    dimensions[key][i] = 'number_of_coefficients_in_h2o_forcing_data'
                elif dim == 'levozp':
                    dimensions[key][i] = 'vertical_dimension_of_ozone_forcing_data'
                elif dim == 'oz_coeff':
                    dimensions[key][i] = 'number_of_coefficients_in_ozone_forcing_data'
                elif dim == 'oz_coeff+5':
                    dimensions[key][i] = 'number_of_coefficients_in_ozone_forcing_data_plus_five'
                elif dim == '1:gfs_control%nblks':
                    dimensions[key][i] = 'number_of_blocks'
                elif dim == 'ntrcaer':
                    dimensions[key][i] = 'number_of_aerosol_tracers_MG'
                elif dim == 'nspc1':
                    dimensions[key][i] = 'number_of_species_for_aerosol_optical_depth'
                elif dim == 'nbdlw':
                    dimensions[key][i] = 'number_of_aerosol_bands_for_longwave_radiation'
                elif dim == 'nbdsw':
                    dimensions[key][i] = 'number_of_aerosol_bands_for_shortwave_radiation'
                elif dim == 'nf_aelw':
                    dimensions[key][i] = 'number_of_aerosol_output_fields_for_longwave_radiation'
                elif dim == 'nf_aesw':
                    dimensions[key][i] = 'number_of_aerosol_output_fields_for_shortwave_radiation'
                elif dim == 'is:ie':
                    dimensions[key][i] = 'starting_x_direction_index:ending_x_direction_index'
                elif dim == 'isd:ied':
                    dimensions[key][i] = 'starting_x_direction_index_domain:ending_x_direction_index_domain'
                elif dim == 'js:je':
                    dimensions[key][i] = 'starting_y_direction_index:ending_y_direction_index'
                elif dim == 'jsd:jed':
                    dimensions[key][i] = 'starting_y_direction_index_domain:ending_y_direction_index_domain'
                elif dim == '1:npz':
                    dimensions[key][i] = '1:vertical_dimension_for_fast_physics'
                elif dim == '1:npzcappa':
                    dimensions[key][i] = '1:vertical_dimension_for_cappa_at_Lagrangian_surface'
                elif dim == '0:ccpp_interstitial%ngas':
                    dimensions[key][i] = '0:number_of_gases_for_multi_gases_physics'
                elif dim in [ 'gfs_control%nfxr',
                              'gfs_control%ntot2d',
                              'gfs_control%ntot3d',
                              'nf_clds',
                              '1:size(bk)',
                              'nf_vgas',
                              '1:size(ak)',
                              'nf_albd',
                              'n',
                            ]:
                    dimensions[key][i] = dim + "_XX_SubstituteWithStandardName_XX"
                elif not dim in standard_names.keys():
                    raise Exception("Dimension {} not defined".format(dim))
                else:
                    dimensions[key][i] = standard_names[dim]
                # End if
            # End for
        # End for
    # End model and file-dependent substitutions

    max_line = len(fin_lines) - 1
    mdconfig = list()
    in_preamble = True
    in_type = False
    ddt_references = {}
    with open(filename_out, 'w') as file:
        line, lindex = next_line(fin_lines, max_line)
        while line is not None:
            # Check for a module line
            current_module = parse_module_line(line, current_module)
            # Maintain a status of being in a DDT definition
            if (not in_type) and type_re.match(line):
                in_type = True
            elif in_type and end_type_re.match(line):
                in_type = False
            # End if
            # Check for end of preamble
            if (not in_type) and (line.lstrip()[0:8].lower() == 'contains'):
                in_preamble = False
            # End if

            # Check for beginning of new table
            words = line.split()
            # This is case sensitive
            if len(words) > 2 and words[0] in ['!!', '!>'] and '\section' in words[1] and 'arg_table_' in words[2]:
                # We have a new table, parse the header
                table_name = words[2].replace('arg_table_','')
##XXgoldyXX: Uncomment this after conversion is over
#                logger.info('Found old metadata table, {}, on line {}'.format(table_name, lindex+1))
                # The header line is not modified
                file.write(line+"\n")
                # Create the table start section
                mdtable = MetadataTable(table_name, current_module)
                mdconfig.append(mdtable)
                line, lindex = next_line(fin_lines, max_line, cindex=lindex)
                words = line.split('|')
                header_locs = {}
                dim_names = [__not_found__]*15
                # Do not work on a blank table
                if len(words) > 1:
                    # Write an include line for the metadata table
                    file.write('!! \htmlinclude {}.html\n'.format(table_name))
                    #
                    table_header = [x.strip() for x in words[1:-1]]
                    for ind in xrange(len(table_header)):
                        header_locs[table_header[ind]] = ind
                    # End for
                    # Find the local_name index (exception if not found)
                    local_name_ind = header_locs['local_name']
                    # Find the standard_name index (exception if not found)
                    standard_name_ind = header_locs['standard_name']
                    # The table header line is not output
                    line, lindex = next_line(fin_lines, max_line, cindex=lindex)
                    # Parse the entries
                    while len(words) > 1:
                        line, lindex = next_line(fin_lines, max_line, cindex=lindex)
                        words = line.split('|')
                        if len(words) <= 1:
                            # End of table, just write and continue
                            file.write(line+'\n')
                            continue
                        # End if
                        entries = [x.strip() for x in words[1:-1]]
                        # Okay, one check
                        if len(entries) != len(header_locs):
                            raise ValueError("Malformed table entry")
                        # End if
                        # First output the local name
                        local_name = entries[local_name_ind]
                        # Then check the local name, skip variables without a standard_name
                        standard_name = entries[standard_name_ind]
                        if not standard_name:
                            if logger is None:
                                raise ValueError("{} does not have a standard name in {}".format(local_name, table_name))
                            else:
                                logger.debug("{} does not have a standard name in {}".format(local_name, table_name))
                            continue
                        else:
                            # Standard names cannot have dashes or periods
                            standard_name = standard_name.replace('-', '_').replace('.', '_')
                        # Create var_name: strip old-style DDT references from local_name and try to substitute array indices
                        var_name = local_name
                        if "(" in var_name:
                            if "%" in var_name and var_name.rfind("%") > var_name.rfind(")"):
                                if mdtable.type == 'ddt':
                                    ddt_reference = var_name[:var_name.rfind('%')]
                                var_name = var_name[var_name.rfind('%')+1:]
                            else:
                                (actual_var_name, array_reference) = split_var_name_and_array_reference(var_name)
                                if mdtable.type == 'ddt':
                                    ddt_reference = actual_var_name[:actual_var_name.rfind('%')]
                                actual_var_name = actual_var_name[actual_var_name.rfind('%')+1:]
                                for index in array_reference.lstrip("(").rstrip(")").split(","):
                                    # Keep literals and colons, substitute variables
                                    match = re.match(r"[0-9]+|:", index)
                                    if match:
                                        continue
                                    else:
                                        if index.lower() in standard_names.keys():
                                            array_reference = array_reference.replace(index, standard_names[index.lower()])
                                        else:
                                            array_reference = array_reference.replace(index, index + "_XX_SubstituteWithStandardName_XX")
                                        # End if
                                    # End if
                                # End for
                                var_name = actual_var_name + array_reference
                            # End if
                        elif "%" in var_name:
                            if mdtable.type == 'ddt':
                                ddt_reference = var_name[:var_name.rfind('%')]
                            var_name = var_name[var_name.rfind('%')+1:]
                        else:
                            ddt_reference = ''
                        # End if
                        #
                        if mdtable.type == 'module':
                            ddt_reference = ''
                        if not current_module in ddt_references.keys():
                            ddt_references[current_module] = {}
                        if not table_name in ddt_references[current_module].keys():
                            ddt_references[current_module][table_name] = ddt_reference
                        elif not ddt_references[current_module][table_name] == ddt_reference:
                            raise Exception("Conflicting DDT references in table {}: {} vs {}".format(
                                            table_name, ddt_references[current_module][table_name], ddt_reference))
                        #
                        mdobj = MetadataEntry(var_name)
                        mdtable[var_name] = mdobj
                        # Now, create the rest of the entries
                        for ind in xrange(len(entries)):
                            attr_name = table_header[ind]
                            entry = entries[ind]
                            if attr_name == 'local_name':
                                # Already handled this
                                continue
                            elif attr_name == 'rank':
                                attr_name = 'dimensions'
                                rank = int(entry)
                                if rank>0:
                                    # Search for key in dimensions dictionary
                                    if local_name.lower() in dimensions.keys():
                                        dim_key = local_name.lower()
                                    # Begin model and file-dependent substitutions
                                    elif model == 'FV3':
                                        if local_name.replace("GFS_Data(cdata%blk_no)%","").lower() in dimensions.keys():
                                            dim_key = local_name.replace("GFS_Data(cdata%blk_no)%","").lower()
                                        elif local_name.replace("GFS_Data(cdata%blk_no)%Intdiag%","Diag%").lower() in dimensions.keys():
                                            dim_key = local_name.replace("GFS_Data(cdata%blk_no)%Intdiag%","Diag%").lower()
                                        elif local_name.replace("GFS_Interstitial(cdata%thrd_no)%","Interstitial%").lower() in dimensions.keys():
                                            dim_key = local_name.replace("GFS_Interstitial(cdata%thrd_no)%","Interstitial%").lower()
                                        elif local_name.replace("CCPP_Interstitial%","Interstitial%").lower() in dimensions.keys():
                                            dim_key = local_name.replace("CCPP_Interstitial%","Interstitial%").lower()
                                        else:
                                            dim_key = None
                                    # End model and file-dependent substitution
                                    else:
                                        dim_key = None

                                    # Begin model and file-dependent substitutions
                                    if model == 'FV3':
                                        if dim_key and 'n_XX_SubstituteWithStandardName_XX' in dimensions[dim_key]:
                                            if local_name in [ 'GFS_Data(cdata%blk_no)%Intdiag%sedim',
                                                               'GFS_Data(cdata%blk_no)%Intdiag%drydep',
                                                               'GFS_Data(cdata%blk_no)%Intdiag%wetdpl',
                                                               'GFS_Data(cdata%blk_no)%Intdiag%wetdpc' ]:
                                                entry = '(horizonal_dimension,number_of_chemical_tracers_for_diagnostics)'
                                            elif local_name == 'GFS_Data(cdata%blk_no)%Intdiag%duem':
                                                entry = '(horizonal_dimension,number_of_dust_bins_for_diagnostics)'
                                            elif local_name == 'GFS_Data(cdata%blk_no)%Intdiag%ssem':
                                                entry = '(horizonal_dimension,number_of_seasalt_bins_for_diagnostics)'
                                            else:
                                                raise Exception("No entry defined for variable {} with dimensions {}".format(
                                                                                            local_name, dimensions[dim_key]))
                                        elif dim_key:
                                            if not rank == len(dimensions[dim_key]):
                                                raise Exception("ERROR, mismatch of variable rank and dimensions for variable {}".format(local_name))
                                            entry = '(' + ','.join(dimensions[dim_key]) + ')'
                                        # Special handling for slices of arrays that do not have an entry in the dimensions dictionary
                                        elif local_name.endswith('(:,1)') and ('at_lowest_model_layer' in standard_name or \
                                                                               'at_lowest_model_interface' in standard_name):
                                            entry = '(horizontal_dimension)'
                                        elif 'GFS_Data(cdata%blk_no)%Tbd%phy_f2d(:,' in local_name and rank==1:
                                            entry = '(horizontal_dimension)'
                                        elif 'GFS_Data(cdata%blk_no)%Tbd%phy_f3d(:,:' in local_name and rank==2:
                                            entry = '(horizontal_dimension,vertical_dimension)'
                                        elif 'GFS_Data(cdata%blk_no)%Statein%qgrs(:,:,GFS_Control' in local_name or \
                                             'GFS_Data(cdata%blk_no)%Stateout%gq0(:,:,GFS_Control' in local_name or \
                                             'GFS_Interstitial(cdata%thrd_no)%save_q(:,:,GFS_Control' in local_name:
                                            entry = '(horizontal_dimension,vertical_dimension)'
                                        elif 'GFS_Data(cdata%blk_no)%Statein%qgrs(:,1,GFS_Control' in local_name or \
                                             'GFS_Data(cdata%blk_no)%Stateout%gq0(:,1,GFS_Control' in local_name:
                                            entry = '(horizontal_dimension)'
                                        elif ("Intdiag%du3dt" in local_name or \
                                              "Intdiag%dv3dt" in local_name or \
                                              "Intdiag%dt3dt" in local_name or \
                                              "Intdiag%dq3dt" in local_name) and rank==2:
                                            entry = '(horizontal_dimension,vertical_dimension)'
                                        elif ("GFS_Interstitial(cdata%thrd_no)%clouds(:,:" in local_name or \
                                              "GFS_Interstitial(cdata%thrd_no)%clw(:,:" in local_name) and rank==2:
                                            entry = '(horizontal_dimension,vertical_dimension)'
                                        elif "GFS_Interstitial(cdata%thrd_no)%dqdt(:,:,GFS_Control" in local_name:
                                            entry = '(horizontal_dimension,vertical_dimension)'
                                        elif local_name == "GFS_Control%input_nml_file":
                                            entry = '(number_of_lines_of_namelist_filename_for_internal_file_reads)'
                                        elif local_name == 'GFS_Control%blksz':
                                            entry = '(number_of_blocks)'
                                        elif local_name in [ 'GFS_Control%idat',
                                                             'GFS_Control%jdat',
                                                           ]:
                                            entry = '(8)'
                                        elif local_name == 'GFS_Control%idate':
                                            entry = '(4)'
                                        elif local_name in [ 'GFS_Control%psautco',
                                                             'GFS_Control%prautco', 
                                                             'GFS_Control%wminco', 
                                                             'GFS_Control%mg_ts_auto_ice',
                                                             'GFS_Control%mg_qcmin',
                                                             'GFS_Control%flgmin',
                                                             'GFS_Control%cgwf',
                                                             'GFS_Control%ccwf',
                                                             'GFS_Control%cdmbgwd',
                                                             'GFS_Control%ctei_rm',
                                                             'GFS_Control%dlqf',
                                                             'GFS_Control%psauras',
                                                             'GFS_Control%prauras',
                                                             'GFS_Control%wminras',
                                                           ]:
                                            entry = '(2)'
                                        elif local_name in [ 'GFS_Control%cs_parm' ]:
                                            entry = '(10)'
                                        elif local_name in [ 'GFS_Control%crtrh' ]:
                                            entry = '(3)'
                                        elif local_name in [ 'GFS_Control%pertz0',
                                                             'GFS_Control%pertzt',
                                                             'GFS_Control%pertshc',
                                                             'GFS_Control%pertlai',
                                                             'GFS_Control%pertalb',
                                                             'GFS_Control%pertvegf',
                                                           ]:
                                            entry = '(5)'
                                        elif 'GFS_Interstitial(cdata%thrd_no)%faerlw(:,:,:' in local_name and rank==3:
                                            entry = '(horizontal_dimension,adjusted_vertical_layer_dimension_for_radiation,number_of_aerosol_bands_for_longwave_radiation)'
                                        elif 'GFS_Interstitial(cdata%thrd_no)%faersw(:,:,:' in local_name and rank==3:
                                            entry = '(horizontal_dimension,adjusted_vertical_layer_dimension_for_radiation,number_of_aerosol_bands_for_shortwave_radiation)'
                                        elif 'GFS_Interstitial(cdata%thrd_no)%gasvmr(:,:' in local_name and rank==2:
                                            entry = '(horizontal_dimension,adjusted_vertical_layer_dimension_for_radiation)'
                                        elif 'GFS_Interstitial(cdata%thrd_no)%sfcalb(:,' in local_name and rank==1:
                                            entry = '(horizontal_dimension)'
                                        elif local_name in [
                                                             'CCPP_interstitial%delp',
                                                             'CCPP_interstitial%pt',
                                                             'CCPP_interstitial%qv',
                                                             'CCPP_interstitial%ql',
                                                             'CCPP_interstitial%qi',
                                                             'CCPP_interstitial%qr',
                                                             'CCPP_interstitial%qs',
                                                             'CCPP_interstitial%qg',
                                                             'CCPP_interstitial%qc',
                                                           ]:
                                            entry = '(starting_x_direction_index_domain:ending_x_direction_index_domain,starting_y_direction_index_domain:ending_y_direction_index_domain,1:vertical_dimension_for_fast_physics)'
                                        elif local_name in [
                                                             'CCPP_interstitial%delz',
                                                           ]:
                                            entry = '(starting_x_direction_index_domain:ending_x_direction_index_domain,starting_y_direction_index_domain:ending_y_direction_index_domain,1:vertical_dimension_for_thickness_at_Lagrangian_surface)'
                                        elif local_name in [
                                                             'CCPP_interstitial%area',
                                                             'CCPP_interstitial%phis',
                                                           ]:
                                            entry = '(starting_x_direction_index_domain:ending_x_direction_index_domain,starting_y_direction_index_domain:ending_y_direction_index_domain)'
                                        elif local_name in [
                                                             'CCPP_interstitial%peln',
                                                           ]:
                                            entry = '(starting_x_direction_index:ending_x_direction_index,1:vertical_dimension_for_fast_physics_plus_one,starting_y_direction_index:ending_y_direction_index)'
                                        elif local_name in [
                                                             'CCPP_interstitial%pkz',
                                                           ]:
                                            entry = '(starting_x_direction_index:ending_x_direction_index,starting_y_direction_index:ending_y_direction_index,1:vertical_dimension_for_fast_physics)'
                                        elif local_name in [
                                                             'CCPP_interstitial%qvi',
                                                           ]:
                                            entry = '(starting_x_direction_index_domain:ending_x_direction_index_domain,starting_y_direction_index_domain:ending_y_direction_index_domain,1:vertical_dimension_for_fast_physics,1:number_of_gases_for_multi_gases_physics)'
                                        elif local_name in [
                                                             'CCPP_interstitial%q_con',
                                                           ]:
                                            entry = '(starting_x_direction_index_domain:ending_x_direction_index_domain,starting_y_direction_index_domain:ending_y_direction_index_domain,1:vertical_dimension_for_condensed_water_at_Lagrangian_surface)'
                                        elif "CCPP_data" in filename_in and standard_name == 'GFS_data_type_instance_all_blocks':
                                            entry = '(ccpp_block_number)'
                                        elif "CCPP_data" in filename_in and standard_name == 'GFS_interstitial_type_instance_all_threads':
                                            entry = '(ccpp_thread_number)'
                                        else:
                                            entry = '(' + ','.join(dim_names[0:rank]) + ')'
                                    # End model and file-dependent substitutions
                                    else:
                                        if dim_key:
                                            if not rank == len(dimensions[dim_key]):
                                                raise Exception("ERROR, mismatch of variable rank and dimensions for variable {}".format(local_name))
                                            entry = '(' + ','.join(dimensions[dim_key]) + ')'
                                        else:
                                            entry = '(' + ','.join(dim_names[0:rank]) + ')'
                                # rank == 0
                                else:
                                    entry = '(' + ','.join(dim_names[0:rank]) + ')'
                                # End if
                            elif attr_name == 'standard_name':
                                # Parsing done earlier
                                entries[ind] = standard_name
                                entry = standard_name
                            elif attr_name == 'intent':
                                # Don't write intent attribute for variable/type definitions
                                if in_preamble:
                                    entry = ''
                                elif entry.lower() == 'none':
                                    if logger is None:
                                        raise ValueError("{} has intent = none in {}".format(var_name, table_name))
                                    else:
                                        logger.warning("{} has intent = none in {}".format(var_name, table_name))
                            elif attr_name == 'optional':
                                # Don't write optional attribute for variable/type definitions
                                if in_preamble:
                                    entry = ''
                                elif not entry in ['F', 'T']:
                                    if logger is None:
                                        raise ValueError("{} has optional = {} in {}".format(var_name, entry, table_name))
                                    else:
                                        logger.warning("{} has optional = {} in {}".format(var_name, entry, table_name))
                                    # End if
                                # End if
                            # No else needed
                            # End if
                            # Add attribute
                            if (len(entry) > 0) or (attr_name in required_attrs):
                                mdobj[attr_name] = entry
                            # End if
                        # End for (done with entry)
                    # End while (done with table)
                else:
                    # Just write the line (should be a table ending)
                    if line.strip() != '!!':
                        raise ValueError("All tables must end with !! line")
                    # End if
                    file.write(line+'\n')
                # End if (blank table)
            else:
                # Not a table, just write and continue
                file.write(line+'\n')
            # End if
            # Always load a new line
            line, lindex = next_line(fin_lines, max_line, cindex=lindex)
        # End while
    # End with (file)

    # Write out finalized metadata file
    with open(metadata_filename_out, 'w') as mdfile:
        spacer = ""
        # First pass: write type definitions,
        # second pass: write module table
        for count in xrange(2):
            for table in mdconfig:
                if (count == 0 and not table.type == 'ddt') or \
                   (count == 1 and table.type == 'ddt'):
                    continue
                if len(spacer) > 0:
                    mdfile.write(spacer)
                # End if
                table.write(mdfile)
                spacer = '\n'+72*'#'+'\n'
            # End for
        # End for
    # End with (mdfile)

    if ddt_references:
        message = """Add the following statement to the CCPP prebuild config (add to existing entry):
TYPEDEFS_NEW_METADATA = {
"""
        for module_name in ddt_references.keys():
            message += "    '{module_name}' : {{\n".format(module_name=module_name)
            for table_name in ddt_references[module_name].keys():
                message += "        '{table_name}' : '{ddt_reference}',\n".format(table_name=table_name,
                                              ddt_reference=ddt_references[module_name][table_name])
            message += "        },\n"
        message += "    }\n"
        if logger is not None:
            logger.info(message)
        else:
            print message

########################################################################

def usage(cmd):
    print("Usage:")
    print("{} <source_file> <target_file> <model>".format(cmd))
    print("")
    print("<model> can be one of '{}'".format(MODELS))
    print("")
    print("Translate the metadata in <source_file> into a new file")
    raise Exception

########################################################################

if __name__ == "__main__":
    # Process the files passed in
    num_args = len(sys.argv)
    if not num_args == 4:
        usage(sys.argv[0])
    else:
        ## Init this now so that all Exceptions can be trapped
        logger = init_log('ccpp_capgen')
        set_log_level(logger, logging.INFO)
        ## To cause convert_metadata to stop when an error condition is found
        ## (no metadata file), uncomment out the next line.
        #logger = None
        tbase = os.path.basename(sys.argv[2])
        tdir = os.path.dirname(sys.argv[2])
        if not sys.argv[3] in MODELS:
            usage(sys.argv[0])
        mdfilename = "{}.meta".format('.'.join(tbase.split('.')[:-1]))
        dest_mdfile = os.path.join(tdir, mdfilename)
        convert_file(sys.argv[1], sys.argv[2], dest_mdfile, sys.argv[3], logger)
    # End if
# End if
