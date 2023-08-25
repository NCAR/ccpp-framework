#!/usr/bin/env python3

"""
Utilities for checking and manipulating file status
"""

# Python library imports
from __future__ import absolute_import
from __future__ import unicode_literals

import filecmp
import glob
import os
# CCPP framework imports
from parse_tools import CCPPError, ParseInternalError

# Standardize name of generated kinds file and module
KINDS_MODULE = 'ccpp_kinds'
KINDS_FILENAME = '{}.F90'.format(KINDS_MODULE)

###############################################################################
def check_for_existing_file(filename, description, readable=True):
###############################################################################
    """Check for file existence and access.
    Return a list of error strings in case <filename>
    does not exist or does not have read access and <readable> is True"""
    errors = list()
    if os.path.exists(filename):
        if readable:
            if not os.access(filename, os.R_OK):
                errmsg = "No read access to {}, '{}'"
                errors.append(errmsg.format(description, filename))
            # end if (no else, everything is fine)
        # end if (no else, everything is fine)
    else:
        errors.append("{}, '{}', must exist".format(description, filename))
    # end if
    return errors

###############################################################################
def check_for_writeable_file(filename, description):
###############################################################################
    """If <filename> exists but not writable, raise an error.
    If <filename> does not exist and its directory is not writable, raise
    an error. <description> is a description of <filename>."""
    if os.path.exists(filename) and not os.access(filename, os.W_OK):
        raise CCPPError("Cannot write {}, '{}'".format(description, filename))
    # end if
    if not os.access(os.path.dirname(filename), os.W_OK):
        raise CCPPError("Cannot write {}, '{}'".format(description, filename))
    # end if (else just return)

###############################################################################
def add_unique_files(filepath, pdesc, master_list, logger):
###############################################################################
    """Add any new files indicated by <filepath> to <master_list>.
    Check each file for readability.
    Log duplicate files
    Return a list of errors found
    Wildcards in <filepath> are expanded"""
    errors = list()
    for file in glob.glob(filepath):
        errs = check_for_existing_file(file, pdesc)
        if errs:
            errors.extend(errs)
        elif file in master_list:
            lmsg = "WARNING: Ignoring duplicate file, {}"
            logger.warning(lmsg.format(file))
        else:
            master_list.append(file)
        # end if
    # end for
    return errors

###############################################################################
def read_pathnames_from_file(pathsfile, file_type):
###############################################################################
    """Read and return path names from <pathsfile>.
    Convert relative pathnames to use <pathsfile>'s directory as root.
    Also return a list of any errors encountered
    """
    # We want to end up with absolute paths, treat <pathsfile> as root location
    root_path = os.path.dirname(os.path.abspath(pathsfile))
    file_list = list()
    pdesc = '{} pathsnames file'.format(file_type)
    errors = check_for_existing_file(pathsfile, pdesc)
    pdesc = '{} pathname in {}'.format(file_type, pathsfile)
    if not errors:
        with open(pathsfile, 'r') as infile:
            for line in infile.readlines():
                path = line.strip()
                # Skip blank lines & lines which appear to start with a comment.
                if path and (path[0] not in ['#', '!']):
                    # Check for an absolute path
                    if not os.path.isabs(path):
                        path = os.path.normpath(os.path.join(root_path, path))
                    # end if
                    file_list.append(path)
                # end if (else skip blank or comment line)
            # end for
        # end with open
    # end if (no else, we already have the errors)
    return file_list, errors

###############################################################################
def _create_file_list_int(files, suffices, file_type, logger,
                          txt_files, pathname, root_path, master_list):
###############################################################################
    """Create and return a master list of files from <files>.
    <files> is a list of pathnames which may include wildcards.
    <suffices> is a list of allowed file types. Filenames in <files>
        with an allowed suffix will be added to the master list.
        Filenames with a '.txt' suffix will be parsed to look for allowed
        filenames.
    <file_type> is a description of the allowed file types.
    <logger> is a logger used to print warnings (unrecognized filename types)
        and debug messages.
    <txt_files> is a list of previously-encountered text files (to prevent
        infinite recursion).
    <pathname> is the text file name from which <files> was read (if any).
    <master_list> is the list of files which have already been collected
    A list of error strings is also returned
    """
    errors = list()
    if pathname:
        pdesc = '{} pathname file, found in {}'.format(file_type, pathname)
    else:
        pdesc = '{} pathnames file'.format(file_type)
    # end if
    if not isinstance(files, list):
        raise ParseInternalError("'{}' is not a list".format(files))
    # end if
    for filename in files:
        # suff is filename's extension
        suff = os.path.splitext(filename)[1]
        if suff:
            suff = suff[1:]
        # end if
        if not os.path.isabs(filename):
            filename = os.path.normpath(os.path.join(root_path, filename))
        # end if
        if os.path.isdir(filename):
            for suff_type in suffices:
                file_type = os.path.join(filename, '*.{}'.format(suff_type))
                errs = add_unique_files(file_type, pdesc, master_list, logger)
                errors.extend(errs)
            # end for
        elif suff in suffices:
            errs = add_unique_files(filename, pdesc, master_list, logger)
            errors.extend(errs)
        elif suff == 'txt':
            tfiles = glob.glob(filename)
            if tfiles:
                for file in tfiles:
                    if file in txt_files:
                        lmsg = "WARNING: Ignoring duplicate '.txt' file, {}"
                        logger.warning(lmsg.format(filename))
                    else:
                        lmsg = 'Reading .{} filenames from {}'
                        logger.debug(lmsg.format(', .'.join(suffices),
                                                 file))
                        flist, errs = read_pathnames_from_file(file, file_type)
                        errors.extend(errs)
                        txt_files.append(file)
                        root = os.path.dirname(file)
                        _, errs = _create_file_list_int(flist, suffices,
                                                        file_type, logger,
                                                        txt_files, file,
                                                        root, master_list)
                        errors.extend(errs)
                    # end if
                # end for
            else:
                emsg = "{} pathnames file, '{}', does not exist"
                errors.append(emsg.format(file_type, filename))
            # end if
        else:
            lmsg = 'WARNING: Not reading {}, only reading .{} or .txt files'
            logger.warning(lmsg.format(filename, ', .'.join(suffices)))
        # end if
    # end for

    return master_list, errors

###############################################################################
def create_file_list(files, suffices, file_type, logger, root_path=None):
###############################################################################
    """Create and return a master list of files from <files>.
    <files> is either a comma-separated string of pathnames or a list.
      If a pathname is a directory, all files with extensions in <suffices>
        are included.
      Wildcards in a pathname are expanded.
    <suffices> is a list of allowed file types. Filenames in <files>
      with an allowed suffix will be added to the master list.
      Filenames with a '.txt' suffix will be parsed to look for allowed
      filenames.
    <file_type> is a description of the allowed file types.
    <logger> is a logger used to print warnings (unrecognized filename types)
      and debug messages.
    If <root_path> is not None, it is used to create absolute paths for
      <files>, otherwise, the current working directory is used.
    """
    master_list = list()
    txt_files = list() # Already processed txt files
    pathname = None
    if isinstance(files, str):
        file_list = [x.strip() for x in files.split(',') if x.strip()]
    elif isinstance(files, (list, tuple)):
        file_list = files
    else:
        raise ParseInternalError("Bad input, <files> = {}".format(files))
    # end if
    if root_path is None:
        root_path = os.getcwd()
    # end if
    master_list, errors = _create_file_list_int(file_list, suffices, file_type,
                                                logger, txt_files, pathname,
                                                root_path, master_list)
    if errors:
        emsg = 'Error processing list of {} files:\n  {}'
        raise CCPPError(emsg.format(file_type, '\n  '.join(errors)))
    # end if
    return master_list

###############################################################################
def replace_paths(dir_list, src_dir, dest_dir):
###############################################################################
    """For every path in <dir_list>, replace instances of <src_dir> with
    <dest_dir>"""
    for index, path in enumerate(dir_list):
        dir_list[index] = path.replace(src_dir, dest_dir)
    # end for

###############################################################################
def remove_dir(src_dir, force=False):
###############################################################################
    """Remove <src_dir> and its children. This operation can only succeed if
    <src_dir> contains no files or if <force> is True."""
    currdir = os.getcwd()
    src_parent = os.path.split(src_dir)[0]
    src_rel = os.path.relpath(src_dir, src_parent)
    os.chdir(src_parent) # Prevent removing the parent of src_dir
    if force:
        leaf_dirs = set()
        for root, dirs, files in os.walk(src_rel):
            for file in files:
                os.remove(os.path.join(root, file))
            # end for
            if not dirs:
                leaf_dirs.add(root)
            # end if
        # end for
        for ldir in leaf_dirs:
            os.removedirs(ldir)
        # end for
    # end if (no else, always try to remove top level
    try:
        os.removedirs(src_rel)
    except OSError:
        pass # Ignore error, fail silently
    # end try
    os.chdir(currdir)

###############################################################################
def move_modified_files(src_dir, dest_dir, overwrite=False, remove_src=False):
###############################################################################
    """For each file in <src_dir>, move it to <dest_dir> if that file is
    different in the two locations.
    if <overwrite> is True, move all files to <dest_dir>, even if unchanged.
    If <remove_src> is True, remove <src_dir> when complete."""
    src_files = {} # All files in <src_dir>
    if os.path.normpath(src_dir) != os.path.normpath(dest_dir):
        for root, _, files in os.walk(src_dir):
            for file in files:
                src_path = os.path.join(root, file)
                if file in src_files:
                    # We do not allow two files with the same name
                    emsg = "Duplicate CCPP file found, '{}', original is '{}'"
                    raise CCPPError(emsg.format(src_path, src_files[file]))
                # end if
                src_files[file] = src_path
            # end for
        # end for
        for file in src_files:
            src_path = src_files[file]
            src_file = os.path.relpath(src_path, start=src_dir)
            dest_path = os.path.join(dest_dir, src_file)
            if os.path.exists(dest_path):
                if overwrite:
                    fmove = True
                else:
                    fmove = filecmp.cmp(src_path, dest_path, shallow=False)
                # end if
            else:
                fmove = True
            # end if
            if fmove:
                os.replace(src_path, dest_path)
            else:
                os.remove(src_path)
            # end if
        # end for
        if remove_src:
            remove_dir(src_dir, force=True)
        # end if
    # end if (no else, take no action if the directories are identical)
