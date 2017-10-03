#!/usr/bin/env python

# Usage: ./parse_scheme_table.py filename1 [filename2 filename3 ...]
# Input: fortran filenames with doxygen-compliant and CCPP-compliant physics schemes; the argument tables should have the following format:
# !! \section arg_table_schemename_run
# !! | local var name | longname                                              | description                        | units   | rank | type    |    kind   | intent | optional |
# !! |----------------|-------------------------------------------------------|------------------------------------|---------|------|---------|-----------|--------|----------|
# !! | im             | horizontal_loop_extent                                | horizontal loop extent, start at 1 | index   |    0 | integer |           | in     | F        |
# !! | ix             | horizontal_dimension                                  | horizontal dimension               | index   |    0 | integer |           | in     | F        |
# !! | ...            | ...                                                   |                                    |         |      |         |           |        |          |
# !!
# Notes on the input format:
# - the "\section arg_table_SubroutineName" command denotes the start of the table; SubroutineName must match the name of the subroutine that the argument table describes
# - each line of the table must begin with the doxygen-delimiter '!!'
# - table headers are the first row; right now, the only ones parsed into XML (only ones required) are 'local var name' => id, 'longname' => name, units => units, rank => rank, type => type
# - the second row must have the |---|-----| format
# - after the last row of the table, there should be a blank doxygen line (only '!!') to denote the end of the table
# Output: for each filename specified, this routine converts the argument tables for all subroutines (*_init, *_run, *_finalize) into an XML file suitable to be used with mkcap (which generates the fortran code for the scheme cap)
# - the script generates a separate file for each module within the given files

import argparse  #needed for command line argument filenames
from xml.etree import ElementTree as ET #needed for writing out XML

#subroutine for writing "pretty" XML; copied from http://effbot.org/zone/element-lib.htm#prettyprint
def indent(elem, level=0):
  i = "\n" + level*"  "
  if len(elem):
    if not elem.text or not elem.text.strip():
      elem.text = i + "  "
    if not elem.tail or not elem.tail.strip():
      elem.tail = i
    for elem in elem:
      indent(elem, level+1)
    if not elem.tail or not elem.tail.strip():
      elem.tail = i
  else:
    if level and (not elem.tail or not elem.tail.strip()):
      elem.tail = i

#set up the command line argument parser
parser = argparse.ArgumentParser()

#the only arguments are a list of filenames to parse
parser.add_argument('file', help='paths to fortran source code to parse for generating CCPP scheme XML files', nargs='*')
args = parser.parse_args()

#for each filename provided, parse it and output one XML file
for i in range(len(args.file)):
    filename = args.file[i]

    #read all lines of the file at once
    with (open(filename, 'r')) as file:
        file_lines = file.readlines()

    #find all modules within the file, and save the start and end lines
    module_names = []
    mod_begin_lines = []
    mod_end_lines = []
    line_counter = 0
    for line in file_lines:
        words = line.split()
        for j in range(len(words)):
            #check for the word 'module', that it is the first word in the line, and that a module name exists afterward
            if words[j].lower() == 'module' and j+1 <= len(words)-1 and j == 0:
                mod_begin_lines.append(line_counter)
                module_names.append(words[j+1].lower().strip())
        if line.lower().find('end module') >= 0:
            mod_end_lines.append(line_counter)
        line_counter += 1

    #for each module within the file, create a separate XML file for the "scheme"
    for l in range(len(module_names)):
        #find the *_init, *_run, *_finalize, etc. subroutines, save their location within the file and their names
        line_counter = 0
        sub_lines = []
        sub_names = []
        scheme_names = []
        #only look at the lines in the current module
        for line in file_lines[mod_begin_lines[l]:mod_end_lines[l]]:
            words = line.split()
            for j in range(len(words)):
                #check for the word 'subroutine', that it is the first word in the line, and that a subroutine name exists afterward
                if words[j].lower() == 'subroutine' and j+1 <= len(words)-1 and j == 0:
                    #consider the last substring separated by a '_' of the subroutine name as a 'postfix'
                    sub_name = words[j+1].split('(')[0].strip()
                    if sub_name.find('_') >= 0:
                        #ignore subroutines that have no postfix
                        sub_lines.append(line_counter)
                        sub_names.append(sub_name.lower())
                        scheme_names.append(sub_names[-1][0:sub_names[-1].rfind('_')])
            line_counter += 1

        #check that all the subroutine "root" names in the current module are the same
        if scheme_names.count(scheme_names[0]) == len(scheme_names):
            scheme_name = scheme_names[0]
        else:
            print 'Please check that all of the subroutines have the same root name: i.e. scheme_A_init, scheme_A_run, scheme_A_finalize'
            print 'Here is a list of the subroutine names:'
            print sub_names
            print 'Here is a list of the scheme names (parsed from the subroutine names):'
            print scheme_names
            quit()


        table_header_sets = []
        var_data_sets = []
        for j in range(len(sub_names)):
            #find the argument table corresponding to each subroutine by searching "upward" from the subroutine definition line for the "arg_table_SubroutineName" section
            table_found = False
            for k in range(mod_begin_lines[l] + sub_lines[j], -1, -1):
                line = file_lines[k]
                words = line.split()
                for word in words:
                    if 'arg_table_' + sub_names[j] in word:
                        table_found = True
                        header_line = k + 1
                        break
                if table_found:
                    break

            #if an argument table is found, parse it
            if table_found:
                #separate the table headers
                table_headers = file_lines[header_line].split('|')
                table_headers = table_headers[1:-1]
                table_header_sets.append([x.strip() for x in table_headers])

                #get all of the variable information
                end_of_table = False
                k = header_line + 2
                var_data = []
                while not end_of_table:
                    line = file_lines[k]
                    words = line.split()
                    if len(words) == 1:
                        end_of_table = True
                    else:
                        var_items = line.split('|')[1:-1]
                        var_items = [x.strip() for x in var_items]
                        var_data.append(var_items)
                    k += 1
                var_data_sets.append(var_data)
            else:
                #if not table is found, just append an empty list
               table_header_sets.append([])
               var_data_sets.append([])

        #write out the XML in the format that mkcap wants
        top = ET.Element('scheme')
        top.set('module', scheme_name)
        for j in range(len(sub_names)):
            sub_sub = ET.SubElement(top, 'subroutine')
            sub_sub.set('name', sub_names[j])

            #right now, the mapping from the tables to the XML is 'local var name' => id, 'longname' => name, units => units, rank => rank, type => type
            #### this can be generalized and updated in the future using the table header information ####
            for k in range(len(var_data_sets[j])):
                sub_var = ET.SubElement(sub_sub, 'var')
                var_name = ET.SubElement(sub_var, 'name')
                var_name.text = var_data_sets[j][k][1]
                var_units = ET.SubElement(sub_var, 'units')
                var_units.text = var_data_sets[j][k][3]
                var_id = ET.SubElement(sub_var, 'id')
                var_id.text = var_data_sets[j][k][0]
                var_rank = ET.SubElement(sub_var, 'rank')
                var_rank.text = var_data_sets[j][k][4]
                var_type = ET.SubElement(sub_var, 'type')
                var_type.text = var_data_sets[j][k][5]

        indent(top)
        tree = ET.ElementTree(top)
        tree.write(scheme_name + '.xml', xml_declaration=True, encoding='utf-8', method="xml")
        print 'Parsed tables for ' + ", ".join([str(x) for x in sub_names] ) + ' in module ' + module_names[l] + '; output => ' + scheme_name + '.xml'
