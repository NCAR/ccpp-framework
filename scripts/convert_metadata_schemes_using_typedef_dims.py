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
from metadata_table import MetadataHeader
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
#MODELS = ['FV3']

METADATA_TYPEDEFS = {
    'FV3' : [
        'ccpp/physics/physics/machine.meta',
        'ccpp/physics/physics/radsw_param.meta',
        'ccpp/physics/physics/radlw_param.meta',
        'FV3/gfsphysics/CCPP_layer/CCPP_typedefs.meta',
        'FV3/gfsphysics/CCPP_layer/CCPP_data.meta',
        'FV3/gfsphysics/GFS_layer/GFS_typedefs.meta',
        ],
    }

########################################################################

def parse_metadata_tables_typedefs(model):
    # Lookup table local_name -> dimensions
    dimensions = {
        'ccpp_error_flag'    : [],
        'ccpp_error_message' : [],
        'ccpp_loop_counter'  : [],
        'ccpp_block_number'  : [],
        'ccpp_thread_number' : [],
        }
    for filename in METADATA_TYPEDEFS[model]:
        metadata_headers = MetadataHeader.parse_metadata_file(filename)
        for metadata_header in metadata_headers:
            for var in metadata_header.variable_list():
                standard_name = var.get_prop_value('standard_name')
                if standard_name in dimensions.keys():
                    raise ValueError("Duplicate standard name {} in type/variable definition metadata tables".format(standard_name))
                dimensions[standard_name] = var.get_prop_value('dimensions')
    return dimensions

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

def convert_file(filename_in, filename_out, metadata_filename_out, typedef_dimensions, logger=None):
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

    # Read all lines of the file at once
    with open(filename_in, 'r') as file:
        fin_lines = file.readlines()
        for index in xrange(len(fin_lines)):
            fin_lines[index] = fin_lines[index].rstrip('\n')
        # End for
    # End with

    max_line = len(fin_lines) - 1
    mdconfig = list()
    in_preamble = True
    in_type = False
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
                            raise ValueError("{} does not have a standard name in {}".format(local_name, table_name))
                        # Standard names cannot have dashes or periods
                        standard_name = standard_name.replace('-', '_').replace('.', '_')
                        # Create var_name: strip old-style DDT references from local_name and try to substitute array indices
                        var_name = local_name
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
                                # Search for standard_name key in typedef_dimensions dictionary
                                if not standard_name in typedef_dimensions.keys():
                                    raise ValueError("{} does not have an entry in the in typedef_dimensions dictionary".format(standard_name))
                                if not rank == len(typedef_dimensions[standard_name]):
                                    raise ValueError("Rank of {} in {} does not match with dimension information in typedef_dimensions".format(
                                                                                                                    standard_name, table_name))
                                entry = '(' + ','.join(typedef_dimensions[standard_name]) + ')'
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
                                        logger.error("{} has intent = none in {}".format(var_name, table_name))
                            elif attr_name == 'optional':
                                # Don't write optional attribute for variable/type definitions
                                if in_preamble:
                                    entry = ''
                                elif not entry in ['F', 'T']:
                                    if logger is None:
                                        raise ValueError("{} has optional = {} in {}".format(var_name, entry, table_name))
                                    else:
                                        logger.error("{} has optional = {} in {}".format(var_name, entry, table_name))
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

########################################################################

def usage(cmd):
    print("Usage:")
    print("{} <source_file> <target_file> <model>".format(cmd))
    print("")
    print("<model> can be one of '{}'".format(METADATA_TYPEDEFS.keys()))
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
        if not sys.argv[3] in METADATA_TYPEDEFS.keys():
            usage(sys.argv[0])
        mdfilename = "{}.meta".format('.'.join(tbase.split('.')[:-1]))
        dest_mdfile = os.path.join(tdir, mdfilename)
        typedef_dimensions = parse_metadata_tables_typedefs(sys.argv[3])

        convert_file(sys.argv[1], sys.argv[2], dest_mdfile, typedef_dimensions, logger)
    # End if
# End if
