#!/usr/bin/env python

"""
Utilities for checking and manipulating file status
"""

# Python library imports
from __future__ import absolute_import
from __future__ import unicode_literals

import os
import os.path
# CCPP framework imports
from parse_tools import CCPPError, ParseInternalError

###############################################################################
def check_for_existing_file(filename, description, readable=True):
###############################################################################
    'Check for file existence and access, abort on error'
    if os.path.exists(filename):
        if readable:
            if not os.access(filename, os.R_OK):
                errmsg = "No read access to {}, '{}'"
                raise CCPPError(errmsg.format(description, filename))
            # End if
        # End if (no else needed, checks all done
    else:
        raise CCPPError("{}, '{}', must exist".format(description, filename))
    # End if

###############################################################################
def check_for_writeable_file(filename, description):
###############################################################################
    '''If <filename> exists but not writable, raise an error.
    If <filename> does not exist and its directory is not writable, raise
    an error. <description> is a description of <filename>.'''
    if os.path.exists(filename) and not os.access(filename, os.W_OK):
        raise CCPPError("Cannot write {}, '{}'".format(description, filename))
    elif not os.access(os.path.dirname(filename), os.W_OK):
        raise CCPPError("Cannot write {}, '{}'".format(description, filename))
    # End if (else just return)

###############################################################################
def read_pathnames_from_file(pathsfile):
###############################################################################
    'Read path names from <pathsfile> and return them as a list'
    # We want to end up with absolute paths, treat <pathsfile> as root location
    root_path = os.path.dirname(os.path.abspath(pathsfile))
    pdesc = 'pathname in {}'.format(pathsfile)
    pathnames = list()
    with open(pathsfile, 'r') as infile:
        for line in infile.readlines():
            path = line.strip()
            # Skip blank lines and lines which appear to start with a comment.
            if (len(path) > 0) and (path[0] != '#') and (path[0] != '!'):
                # Check for an absolute path
                if os.path.isabs(path):
                    check_for_existing_file(path, "pathname")
                else:
                    # Assume relative pathnames are relative to pathsfile
                    path = os.path.normpath(os.path.join(root_path, path))
                    check_for_existing_file(path, pdesc)
                # End if
                pathnames.append(path)
            # End if (else skip blank or comment line)
        # End for
    # End with open
    return pathnames

###############################################################################
def create_file_list(files, suffices, file_type, logger, txt_files=None):
###############################################################################
    '''Create and return a master list of files from <files>.
    <files> is either a comma-separated string of filenames or a list.
    <suffices> is a list of allowed file types. Filenames in <files>
    with an allowed suffix will be added to the master list.
    Filenames with a '.txt' suffix will be parsed to look for allowed
    filenames.
    <file_type> is a description of the allowed file types.
    <logger> is a logger used to print warnings (unrecognized filename types)
    and debug messages.
    <txt_files> is an internal argument to prevent infinite recursion.
    '''
    master_list = list()
    if isinstance(files, str):
        file_list = [x.strip() for x in files.split(',')]
    elif isinstance(files, (list, tuple)):
        file_list = files
    else:
        raise ParseInternalError("Bad input, <files> = {}".format(files))
    # End if
    pdesc = '{} pathnames file'.format(file_type)
    for filename in file_list:
        suff = os.path.basename(filename).split('.')[-1]
        if suff in suffices:
            check_for_existing_file(filename, pdesc)
            apath = os.path.abspath(filename)
            if apath in master_list:
                lmsg = 'WARNING: Duplicate {} filename, {}'
                logger.warning(lmsg.format(file_type, filename))
            else:
                master_list.append(apath)
            # End if
        elif suff == 'txt':
            if txt_files and (filename in txt_files):
                lmsg = "WARNING: Ignoring duplicate '.txt' file, {}"
                logger.warning(lmsg.format(filename))
            else:
                check_for_existing_file(filename, pdesc)
                flist = read_pathnames_from_file(filename)
                if txt_files:
                    txt_files.append(filename)
                else:
                    txt_files = [filename]
                # End if
                master_list.extend(create_file_list(flist, suffices,
                                                    file_type, logger,
                                                    txt_files=txt_files))
            # End if
            lmsg = 'Reading .{} filenames from {}'
            logger.debug(lmsg.format(', .'.join(suffices), filename))
        else:
            lmsg = 'WARNING: Not reading {}, only reading .{} or .txt files'
            logger.warning(lmsg.format(filename, ', .'.join(suffices)))
        # End if
    # End for
    return master_list
