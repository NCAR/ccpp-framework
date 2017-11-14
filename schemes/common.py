#!/usr/bin/env python

import subprocess

def execute(cmd, debug = False, abort = True):
    """Runs a local command in a shell. Waits for completion and
    returns status, stdout and stderr. If abort = True, abort in
    case an error occurs during the execution of the command."""
    
    debug = True
    
    if debug:
        print 'Executing "{0}"'.format(cmd)
    p = subprocess.Popen(cmd, stdout = subprocess.PIPE,
                         stderr = subprocess.PIPE, shell = True)
    (stdout, stderr) = p.communicate()
    status = p.returncode
    if debug:
        message = 'Execution of "{0}" returned with exit code {1}\n'.format(cmd, status)
        message += '    stdout: "{0}"\n'.format(stdout.rstrip('\n'))
        message += '    stderr: "{0}"'.format(stderr.rstrip('\n'))
        print message
    if not status == 0:
        message = 'Execution of command {0} failed, exit code {1}\n'.format(cmd, status)
        message += '    stdout: "{0}"\n'.format(stdout.rstrip('\n'))
        message += '    stderr: "{0}"'.format(stderr.rstrip('\n'))
        if abort:
            raise Exception(message)
        else:
            print message
    return (status, stdout.rstrip('\n'), stderr.rstrip('\n'))
