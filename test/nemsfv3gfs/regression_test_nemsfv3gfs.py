#!/usr/bin/env python

__doc__ = """**Introduction**

This script clones the NEMSfv3gfs code and its submodules from the default repository on GitHub,
sets up the forks and checks out the branches as requested, and runs the regression test suite.

The last step is optional and works on supported platforms only (currently: Cheyenne). By skipping
the last step, which can be changed in the configuration file, this script can be used as a tool
to quickly check out the NEMSfv3gfs code and its submodules.

For further information and prerequisites, see the ccpp-framework wiki:

https://github.com/NCAR/ccpp-framework/wiki/Development-Workflow#Test-Test-Test

**Usage**

./regression_test_nemsfv3gfs.py --help

usage: regression_test_nemsfv3gfs.py [-h] [--config=CONFIG]

Checkout NEMSfv3gfs repository and submodules.

| optional arguments:
|     -h, --help       show this help message and exit
|     --config CONFIG  name of config file (default: regression_test_nemsfv3gfs.cfg)

**Example configuration file regression_test_nemsfv3gfs_cheyenne.cfg (default values)**

| [default]
| basedir = /glade/work/heinzell/fv3/NEMSfv3gfs/automatic_testing

| [NEMSfv3gfs]
| fork = NCAR
| branch = gmtb/ccpp

| [NEMS]
| fork = NCAR
| branch = gmtb/ccpp

| [FV3]
| fork = NCAR
| branch = gmtb/ccpp

| [FMS]
| fork = NCAR
| branch = GFS-FMS

| [ccpp-framework]
| fork = NCAR
| branch = master

| [ccpp-physics]
| fork = NCAR
| branch = master

| [testing]
| runtests = True
| account = P48503002
| compiler = intel
| config = rt_ccpp.conf

**Documentation of individual subroutines**
"""

import argparse
import ConfigParser
import datetime
import os
import shutil
import subprocess
import sys

###############################################################################
# Global settings                                                             #
###############################################################################

# Template for repository URLs
REPOSITORY_URL_TEMPLATE = 'https://github.com/{fork}/{repo}.git'

# Name of default fork/branch to clone before adding own forks/branches
PARENT_REPOSITORY_DEFAULT_FORK = 'NCAR'
PARENT_REPOSITORY_DEFAULT_BRANCH = 'gmtb/ccpp'

# Name of parent repository
PARENT_REPOSITORY = 'NEMSfv3gfs'

# Names and directorieus of submodules in parent repository
SUBMODULES = {
    'NEMS' : 'NEMS',
    'FV3' : 'FV3',
    'FMS' : 'FMS',
    'ccpp-framework' : 'ccpp/framework',
    'ccpp-physics' : 'ccpp/physics',
    }

# List of all repositories (parent and submodules)
REPOSITORIES = [PARENT_REPOSITORY] + SUBMODULES.keys()

###############################################################################
# Work routines and main entry points                                         #
###############################################################################

def parse_arguments():
    """Parse command line arguments."""
    description = 'Regression test / code checkout script for NEMSfv3gfs'
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--config', action='store', help='name of regression test config', 
                        default='regression_test_nemsfv3gfs.cfg')
    args = parser.parse_args()
    return args.config

def parse_config(configfile):
    """Parse the specified config file and return configuration."""
    if os.path.exists(configfile):
        config = ConfigParser.RawConfigParser()
        config.read(configfile)
    else:
        raise Exception('Config file {0} not found'.format(configfile))
    # Base directory
    basedir = config.get('default', 'basedir')
    # Dictionaries with names of forks and branches for all repositories
    forks = {}
    branches = {}
    for repository in REPOSITORIES:
        forks[repository] = config.get(repository, 'fork')
        branches[repository] = config.get(repository, 'branch')
    # Flag whether to run the tests or not (i.e. check out code only)
    runtests = config.getboolean('testing', 'runtests')
    # Account name/number for the scheduler
    account = config.get('testing', 'account')
    # Compiler to use when running the tests
    compiler = config.get('testing', 'compiler')
    # Test configuration
    testconfig = config.get('testing', 'config')
    return (basedir, forks, branches, runtests, account, compiler, testconfig)

def execute(cmd, abort = True):
    """Runs a local command in a shell and capture the exit status.
    If abort = True, abort in case an error occurs during the
    execution of the command."""
    print "Executing '{0}'".format(cmd)
    status = os.system(cmd)
    if not status == 0:
        if abort:
            message = 'Execution of command {0} failed, return code = {1}'.format(cmd, status)
            raise Exception(message)
        else:
            message = 'WARNING: Execution of command {0} failed, return code = {1}; ignore and proceed'.format(cmd, status)
            print message
    return status

def setup_workdir(basedir):
    """Sets up the work directory for local clone of repositories
    underneath the base directory. Aborts if the base directory
    does not exist, deletes existing work directories. The work
    subdirectory is labeled as rt_YYYYMMDDTHHMMSS."""
    if not os.path.exists(basedir):
        raise Exception('Base directory {0} does not exist'.format(basedir))
    now = datetime.datetime.now()
    workdir = os.path.abspath(os.path.join(basedir, 'rt_{0}'.format(now.strftime('%Y%m%dT%H%M%S'))))
    print "Setting up work directory {0} for local clone of repositories".format(basedir)
    if os.path.exists(workdir):
        shutil.rmtree(workdir)
    os.makedirs(workdir)
    return workdir

def checkout_code(workdir, forks, branches):
    """Check out the main repository, initialize its submodules and
    switch to the requested forks/branches. Doing it this way prevents
    errors ..."""
    # Compose url of main repository for the default fork
    url = REPOSITORY_URL_TEMPLATE.format(fork=PARENT_REPOSITORY_DEFAULT_FORK, repo=PARENT_REPOSITORY)
    # Check out main repository into the work directory
    os.chdir(workdir)
    cmd = 'git clone --branch {branch} {url} {dirname}'.format(url=url,
                                                               branch=PARENT_REPOSITORY_DEFAULT_BRANCH,
                                                               dirname=os.getcwd())
    execute(cmd)
    # Initialize the submodules
    cmd = 'git submodule update --init'
    execute(cmd)
    # For each of the submodules and the main repository,
    # add the requested fork and check out the requested branch
    for repo in forks.keys():
        if repo in SUBMODULES.keys():
            subdir = os.path.join(workdir, SUBMODULES[repo])
            os.chdir(subdir)
        remote = REPOSITORY_URL_TEMPLATE.format(fork=forks[repo], repo=repo)
        print 'REMOTE:', remote
        cmd = 'git remote add myfork {remote}'.format(remote=remote)
        execute(cmd)
        cmd = 'git remote update'
        execute(cmd)
        cmd = 'git checkout myfork/{branch}'.format(branch=branches[repo])
        execute(cmd)
        if repo in SUBMODULES.keys():
            os.chdir(workdir)
    return

def run_regression_tests(workdir, account, compiler, testconfig):
    """Run the standard regression tests. This only works on Cheyenne.
    Set environment variables ACCNR=account, NEMS_COMPILER=compiler,
    and RUNDIR_ROOT=workdir/FV3_RT (where regression tests are run)."""
    testdir = os.path.join(workdir, 'tests')
    rundir = os.path.join(workdir, 'FV3_RT')
    os.chdir(testdir)
    cmd = 'ACCNR={account} NEMS_COMPILER={compiler} RUNDIR_ROOT={rundir} ./rt.sh -l {rtconf} 2>&1 | tee rt.log'.format(
                                                  account=account, compiler=compiler, rundir=rundir, rtconf=testconfig)
    execute(cmd)
    os.chdir(workdir)
    return

def main():
    """Main routine that calls subroutines for each of the steps."""
    # Parse command line arguments
    configfile = parse_arguments()
    # Parse config file
    (basedir, forks, branches, runtests, account, compiler, testconfig) = parse_config(configfile)
    # Set up workdir
    workdir = setup_workdir(basedir)
    # Check out the code
    checkout_code(workdir, forks, branches)
    # Run regression tests
    if runtests:
        run_regression_tests(workdir, account, compiler, testconfig)
    print "Location of code and regression test logs: {0}".format(workdir)

if __name__ == '__main__':
    main()
