import sys
import os.path
import re

yes_re = re.compile(r"(?i)^\s*yes\s*$")

########################################################################

def next_line(lines, max_line, cindex=-1):
    nindex = cindex + 1
    if nindex > max_line:
        return None, -1
    else:
        return lines[nindex].rstrip('\n'), nindex

########################################################################

def convert_file(filename_in, filename_out):
    """Convert a file's old metadata to the new format
    Note that only the bare minimum error checking is done.
    """
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
    with open(filename_out, 'w') as file:
        line, lindex = next_line(fin_lines, max_line)
        while line is not None:
            # Check for beginning of new table
            words = line.split()
            # This is case sensitive
            if len(words) > 2 and words[0] in ['!!', '!>'] and '\section' in words[1] and 'arg_table_' in words[2]:
                # We have a new table, parse the header
                table_name = words[2].replace('arg_table_','')
                # The header line is not modified
                file.write(line+"\n")
                line, lindex = next_line(fin_lines, max_line, cindex=lindex)
                words = line.split('|')
                header_locs = {}
                dim_names = [':']*15
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
                        file.write("!! [ {} ]\n".format(var_name))
                        # Now, output the rest of the entries
                        for ind in xrange(len(entries)):
                            attr_name = table_header[ind]
                            entry = entries[ind]
                            if attr_name == 'local_name':
                                # Already handled this
                                continue
                            elif attr_name == 'long_name':
                                attr_name = 'description'
                            elif attr_name == 'rank':
                                attr_name = 'dimensions'
                            # End if
                            # Guess dimension names for arrays
                            std_name = entries[header_locs['standard_name']]
                            if std_name == 'vertical_dimension':
                                dim_names[1] = std_name
                            elif std_name == 'horizontal_dimension':
                                dim_names[0] = std_name
                            elif std_name == 'horizontal_loop_extent':
                                if dim_names[0] == ':':
                                    dim_names[0] = std_name
                                # End if
                            elif std_name == 'number_of_tracers':
                                dim_names[2] = std_name
                            elif std_name[0:18] == 'number_of_tracers_':
                                if len(dim_names[2]) == 0:
                                    dim_names[2] = std_name
                                # End if
                            # End if
                            # Fix dimensions entry
                            if attr_name == 'dimensions':
                                rank = int(entry)
                                entry = "("
                                for dind in xrange(rank):
                                    if dind > 0:
                                        entry = entry + ", "
                                    # End if
                                    entry = "{}'{}'".format(entry, dim_names[dind])
                                # End for
                                entry = entry + ")"
                            # End if
                            # Output attribute
                            if len(entry) > 0:
                                file.write("!!    {} = {}\n".format(attr_name, entry))
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
        if os.path.isdir(sys.argv[-1]):
            target_dir = os.path.abspath(sys.argv[-1])
            num_args = num_args - 1
            for index in xrange(1, num_args):
                source_file = os.path.abspath(sys.argv[index])
                filename = os.path.basename(source_file)
                dest_file = os.path.join(target_dir, filename)
                convert_file(source_file, dest_file)
        else:
            if num_args != 3:
                usage(sys.argv[0])
            else:
                convert_file(sys.argv[1], sys.argv[2])
            # End if
        # End if
    # End if
# End if
