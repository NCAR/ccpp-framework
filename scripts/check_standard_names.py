#!/usr/bin/env python3
import argparse
import glob
import os
import requests
import xml.etree.ElementTree as ET
import sys

from ccpp_track_variables import setup_logging
from metadata_table import parse_metadata_file
from parse_checkers import registered_fortran_ddt_names
from framework_env import CCPPFrameworkEnv

def fetch_xml(branch):
    url = f"https://raw.githubusercontent.com/ESCOMP/ESMStandardNames/{branch}/standard_names.xml"
    try:
        response = requests.get(url)
        response.raise_for_status()
    except requests.exceptions.HTTPError as e:
        sys.exit(f"Failed to fetch XML: {e}")
    return response.text

def parse_standard_names(xml_text):
    root = ET.fromstring(xml_text)

    std_names = {}
    for entry in root.findall(".//standard_name"):
        std_id = entry.attrib.get("name")
        description = entry.attrib.get("long_name")
        if std_id:
            std_names[std_id] = description
    return std_names

def main(branch,xml,metafiles,debug):

    files = []
    if os.path.isfile(metafiles):
        files = [metafiles]
    else:
        files = glob.glob(os.path.join(metafiles, "*.meta"), recursive=True)
    if not files:
        raise FileNotFoundError(f"Could not find any metadata files in {metafiles}")
    logger = setup_logging(debug)

    if xml:
        print(f"Using local XML: {xml}")
        with open(xml, "r", encoding="utf-8") as f:
            xml_text = f.read()
    else:
        print(f"Fetching XML from branch: {branch}")
        xml_text = fetch_xml(branch)

    std_dict = parse_standard_names(xml_text)

    print(f"Retrieved {len(std_dict)} standard names from XML")

    meta_names = []
    for metafile in files:
        print(f"Retrieving metadata from {metafile}")
        run_env = CCPPFrameworkEnv(logger, host_files="", scheme_files="", suites="")
        metadict = parse_metadata_file(metafile,known_ddts=registered_fortran_ddt_names(),
                                       run_env=run_env)
        print(f"Retrieved {len(metadict)} metadata entries.")
    
#        print(metadict)
        # Print a sample
    #    for i, (key, val) in enumerate(metadict.items()):
    #        print(f"{key}: {val}")
        for i, table in enumerate(metadict):
    #        print(f"{table=}")
            for j, item in enumerate(table.sections()):
    #            print(f"{item.has_variables.prop_list('standard_name')=}")
                meta_names += item.has_variables.prop_list('standard_name')
#        print(f"Found {len(meta_names)} standard names in {metafile}:\n{meta_names}")

    bad_names = []
    for name in meta_names:
        if name in bad_names:
            continue
        if not std_dict.__contains__(name):
            bad_names.append(name)

    if bad_names:
        print(f"The following {len(bad_names)} standard names in {metafiles} were not found in provided XML:")
        for name in sorted(bad_names):
            print(name)
    else:
        print(f"All standard names in {metafile} are valid!")
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Fetch and parse standard_names.xml from ESMStandardNames GitHub, and output any metadata entries with standard_names not found in that dictionary.")
    parser.add_argument("--branch", "-b", type=str,
                        help="GitHub branch, tag, or hash to fetch from (default: main)")
    parser.add_argument("--xml", "-x", type=str,
                        help="Full path to xml file containing standard names (alternative to fetching from internet)")
    parser.add_argument("--metafiles", "-m", type=str, required=True,
                        help="Metadata file or directory containing metadata files to check for valid standard names")
    parser.add_argument('--debug', action='store_true', help='enable debugging output')

    args = parser.parse_args()

    if args.branch and args.xml:
        raise argparse.ArgumentError("Can not specify both --branch and --xml arguments")
    if not (args.branch or args.xml):
        #If neither specified, fall back to retrieving from main branch
        args.branch="main"

    main(args.branch,args.xml,args.metafiles,args.debug)
