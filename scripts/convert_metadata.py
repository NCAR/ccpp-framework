#!/usr/bin/env python
# Python library imports
import sys
import os.path
import re
import logging
from collections import OrderedDict
# CCPP framework imports
from parse_tools import FORTRAN_ID, init_log, set_log_level
from fortran_tools import parse_fortran_file

yes_re = re.compile(r"(?i)^\s*yes\s*$")
module_re = re.compile(r"(?i)\s*module\s+"+(FORTRAN_ID)+r"\s*$")
end_module_re = re.compile(r"(?i)\s*end\s*module\s+"+(FORTRAN_ID)+r"\s*$")
type_re = re.compile(r"(?i)\s*type\s+"+(FORTRAN_ID)+r"\s*$")
end_type_re = re.compile(r"(?i)\s*end\s*type\s+"+(FORTRAN_ID)+r"\s*$")
required_attrs = ['standard_name', 'units', 'dimensions', 'type']
warning = True
__not_found__ = 'XX_NotFound_XX'

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

def convert_file(filename_in, filename_out, metadata_filename_out, logger):
    """Convert a file's old metadata to the new format
    Note that only the bare minimum error checking is done.
    """
    current_module = None
    # First, suck in the old file
    do_convert = True
    if not os.path.exists(filename_in):
        raise IOError("convert_file: file, '{}', does not exist".format(filename_in))
    # End if
    if os.path.exists(filename_out):
        yes = raw_input("Overwrite '{}' (Yes/No)? ".format(filename_out))
        if yes_re.match(yes) is None:
            return
        # End if
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
                # Write an include line for the metadata table
                file.write('!! \htmlinclude {}.html\n'.format(table_name))
                # Create the table start section
                mdtable = MetadataTable(table_name, current_module)
                mdconfig.append(mdtable)
                line, lindex = next_line(fin_lines, max_line, cindex=lindex)
                words = line.split('|')
                header_locs = {}
                dim_names = [__not_found__]*15
                # Do not work on a blank table
                if len(words) > 1:
                    table_header = [x.strip() for x in words[1:-1]]
                    for ind in xrange(len(table_header)):
                        header_locs[table_header[ind]] = ind
                    # End for
                    # Find the local_name index (exception if not found)
                    local_name_ind = header_locs['local_name']
                    # The table header line is not output
                    line, lindex = next_line(fin_lines, max_line, cindex=lindex)
                    # This is the delimiter line, do not output
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
                        var_name = entries[local_name_ind]
                        # Strip old-style DDT references
                        var_name = var_name[var_name.rfind('%')+1:]
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
                                entry = '(' + ','.join(dim_names[0:rank]) + ')'
                            elif attr_name == 'standard_name':
                                # The standard name needs to be lowercase
                                std_name = entry.lower()
                                # Standard names cannot have dashes
                                std_name = std_name.replace('-', '_')
                                # Standard names cannot have periods
                                std_name = std_name.replace('.', '_')
                                entries[ind] = std_name
                                entry = std_name
                            elif attr_name == 'intent':
                                if in_preamble:
                                    entry = ''
                                elif entry.lower() == 'none':
                                    if logger is None:
                                        raise ValueError("{} has intent = none in {}".format(var_name, table_name))
                                    else:
                                        logger.warning("{} has intent = none in {}".format(var_name, table_name))
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
    # Read in the Fortran source to find dimension information
    mheaders = parse_fortran_file(filename_out, preproc_defs={'CCPP':1}, logger=logger)
    # Find and replace dimension information for each metadata header
    for table in mdconfig:
        # Find matching Fortran header
        mheader = None
        for mh in mheaders:
            if mh.title == table.name:
                mheader = mh
                break
            # End if
        # End for
        if mheader is None:
            if logger is not None:
                logger.warning('WARNING: Cannot find {} in {} for dimension translation'.format(table.name, filename_out))
            # End if
            continue # Skip this table
        # End for
        # The Fortran table does not have valid standard names so
        # organize them by local name.
        header_vars = {}
        for var in mheader.variable_list():
            header_vars[var.get_prop_value('local_name').lower()] = var
        # End for
        for lname in table.keys():
            var = table[lname]
            if lname.lower() in header_vars:
                fvar = header_vars[lname.lower()]
            else:
                if logger is not None:
                    logger.warning('WARNING: Cannot find variable {} in {} ({}), skipping dimension translation'.format(lname, table.name, filename_out))
                # End if
                fvar = None
            # End if
            dimstring = var['dimensions'].strip().lstrip('(').rstrip(')')
            if ':' in dimstring: # i.e., this not a scalar variable
                dims = dimstring.split(',')
                rank = len(dims)
                if fvar is None:
                    fdims = None
                else:
                    fdims = fvar.get_dimensions()
                # End if
                if fdims is None:
                    # We are dealing with an invalid variable, deal with it
                    dims = [__not_found__]*rank
                else:
                    dims = list()
                    for fdim in fdims:
                        fdlist = fdim.split(':')
                        fdlen = len(fdlist)
                        dlist = ['']*fdlen
                        for index in xrange(fdlen):
                            # For each dimension component, find the standard_name
                            # First, see if it is blank, then integer, then var
                            fd = fdlist[index]
                            if len(fd) == 0:
                                fdnum = ''
                            else:
                                fdnum = None
                            # End if
                            if len(fd) > 0:
                                try:
                                    test = int(fd)
                                    fdnum = fd
                                except Exception as e:
                                    pass
                                # End try
                            # End if
                            if (len(fd) > 0) and (fdnum is None):
                                # Try to find the standard name for this item
                                if table.has(fd):
                                    fdvar = table.get(fd)
                                    fdnum = fdvar['standard_name']
                                else:
                                    if logger is not None:
                                        logger.warning('WARNING: No local variable found for dimension, {} in {}'.format(fd, lname))
                                    # End if
                                    fdnum = 'XXnot_foundXX'
                                # End if
                            # End if
                            dlist[index] = fdnum
                        # End for
                        # Reassemble dim
                        dims.append(':'.join(dlist))
                    # End for
                # End if
                # Reset dimensions
                var['dimensions'] = '(' + ','.join(dims) + ')'
            # End if
        # End for
    # End for
    # Write out finalized metadata file
    with open(metadata_filename_out, 'w') as mdfile:
        spacer = ""
        for table in mdconfig:
            if len(spacer) > 0:
                mdfile.write(spacer)
            # End if
            table.write(mdfile)
            spacer = '\n'+72*'#'+'\n'
        # End for
    # End with (mdfile)

########################################################################

def usage(cmd):
    print("Usage:")
    print("{} <source_file> <target_file>".format(cmd))
    print("{} <source_file> [ <source_file> ...] [ <target_directory>".format(cmd))
    print("")
    print("Translate the metadata in each <source_file> into a new file")

########################################################################

if __name__ == "__main__":
    # Process the files passed in
    num_args = len(sys.argv)
    if num_args < 3:
        usage(sys.argv[0])
    else:
        ## Init this now so that all Exceptions can be trapped
        logger = init_log('ccpp_capgen')
        set_log_level(logger, logging.INFO)
        ## To cause convert_metadata to stop when an error condition is found
        ## (no metadata file), uncomment out the next line.
        #logger = None
        if os.path.isdir(sys.argv[-1]):
            target_dir = os.path.abspath(sys.argv[-1])
            num_args = num_args - 1
            for index in xrange(1, num_args):
                source_file = os.path.abspath(sys.argv[index])
                filename = os.path.basename(source_file)
                mdfilename = "{}.meta".format('.'.join(filename.split('.')[:-1]))
                dest_file = os.path.join(target_dir, filename)
                dest_mdfile = os.path.join(target_dir, mdfilename)
                convert_file(source_file, dest_file, dest_mdfile, logger)
        else:
            if num_args != 3:
                usage(sys.argv[0])
            else:
                tbase = os.path.basename(sys.argv[2])
                tdir = os.path.dirname(sys.argv[2])
                mdfilename = "{}.meta".format('.'.join(tbase.split('.')[:-1]))
                dest_mdfile = os.path.join(tdir, mdfilename)
                convert_file(sys.argv[1], sys.argv[2], dest_mdfile, logger)
            # End if
        # End if
    # End if
# End if
