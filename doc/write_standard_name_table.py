#!/usr/bin/env python

"""
Convert a metadata standard-name XML library file to a documentation format.
"""

# Python library imports
import xml.etree.ElementTree as ET
import os.path
import argparse
import sys
if __name__ == '__main__' and __package__ is None:
    cpfroot = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    sys.path.append(os.path.join(cpfroot, 'scripts'))
else:
    cpfroot = None
# End if
# CCPP framework imports
from parse_tools import validate_xml_file, read_xml_file
try:
    from metavar import standard_name_to_long_name
    have_metavar = True
except Exception as e:
    have_metavar = False
# End try

###############################################################################
def parse_command_line(args, description):
###############################################################################
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("standard_name_file",
                        metavar='<standard names filename>',
                        type=str, help="XML file with standard name library")
    parser.add_argument("--output-filename", metavar='<output filename>',
                        type=str, default='Metadata-standard-names',
                        help="Name of output file (without extension)")
    parser.add_argument("--output-format", metavar='md', type=str, default='md',
                        help="Format of output file")
    pargs = parser.parse_args(args)
    return pargs

###############################################################################
def convert_xml_to_markdown(root, library_name, snl):
###############################################################################
    snl.write('# {}\n'.format(library_name))
    # Write a table of contents
    snl.write('#### Table of Contents\n')
    for section in root:
        sec_name = section.get('name')
        snl.write("* [{name}](#{name})\n".format(name=sec_name))
    # End for
    snl.write('\n')
    for section in root:
        # Step through the sections
        sec_name = section.get('name')
        sec_comment = section.get('comment')
        snl.write('## {}\n'.format(sec_name))
        if sec_comment is not None:
            # First, squeeze out the spacing
            while sec_comment.find('  ') >= 0:
                sec_comment = sec_comment.replace('  ', ' ')
            # End while
            while sec_comment:
                sec_comment = sec_comment.lstrip()
                cind = sec_comment.find('\\n')
                if cind > 0:
                    snl.write('{}\n'.format(sec_comment[0:cind]))
                    sec_comment = sec_comment[cind+2:]
                else:
                    snl.write('{}\n'.format(sec_comment))
                    sec_comment = ''
                # End if
            # End while
        # End if
        for std_name in section:
            stdn_name = std_name.get('name')
            stdn_longname = std_name.get('long_name')
            if have_metavar and (stdn_longname is None):
                stdn_longname = standard_name_to_long_name({'standard_name':stdn_name})
            # End if
            snl.write("* `{}`: {}\n".format(stdn_name, stdn_longname))
            # Should only be a type in the standard_name text
            for item in std_name:
                if item.tag == 'type':
                    t = item.text
                    kind = item.get('kind')
                    if kind is None:
                        kstr = ''
                    else:
                        kstr = "(kind={})".format(kind)
                    # End if
                    units = item.get('units')
                    snl.write('    * `{}{}`: units = {}\n'.format(t, kstr, units))
                else:
                    raise ValueError("Unknown standard name property, '{}'".format(item.tag))
                # End if
            # End for
        # End for
    # End for

###############################################################################
def main_func():
###############################################################################
    args = parse_command_line(sys.argv[1:], __doc__)
    tree, root = read_xml_file(args.standard_name_file)
    library_name = root.get('name')
    if args.output_format != 'md':
        raise ValueError("Unsupported output format, '{}'".format(args.output_format))
    # End if
    with open("{}.{}".format(args.output_filename, args.output_format), "w") as snl:
        convert_xml_to_markdown(root, library_name, snl)


###############################################################################
if __name__ == "__main__":
    main_func()
