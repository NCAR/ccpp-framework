#!/usr/bin/env python

import logging
import subprocess


def execute(cmd, abort = True):
    """Runs a local command in a shell. Waits for completion and
    returns status, stdout and stderr. If abort = True, abort in
    case an error occurs during the execution of the command."""

    # Set debug to true if logging level is debug
    debug = logging.getLogger().getEffectiveLevel() == logging.DEBUG

    logging.debug('Executing "{0}"'.format(cmd))
    p = subprocess.Popen(cmd, stdout = subprocess.PIPE,
                         stderr = subprocess.PIPE, shell = True)
    (stdout, stderr) = p.communicate()
    status = p.returncode
    if debug:
        message = 'Execution of "{0}" returned with exit code {1}\n'.format(cmd, status)
        message += '    stdout: "{0}"\n'.format(stdout.rstrip('\n'))
        message += '    stderr: "{0}"'.format(stderr.rstrip('\n'))
        logging.debug(message)
    if not status == 0:
        message = 'Execution of command {0} failed, exit code {1}\n'.format(cmd, status)
        message += '    stdout: "{0}"\n'.format(stdout.rstrip('\n'))
        message += '    stderr: "{0}"'.format(stderr.rstrip('\n'))
        if abort:
            raise Exception(message)
        else:
            logging.error(message)
    return (status, stdout.rstrip('\n'), stderr.rstrip('\n'))


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


def encode_container(*args):
    if len(args)==3:
        container = 'MODULE_{0} SCHEME_{1} SUBROUTINE_{2}'.format(*args)
    elif len(args)==2:
        container = 'MODULE_{0} TYPE_{1}'.format(*args)
    elif len(args)==1:
        container = 'MODULE_{0}'.format(*args)
    return container


#def decode_container(container)
#    args = []
#    for arg in container.split(' '):
#        
#def dict_sweep(dictionary, value):
#    for key in dictionary.keys():
#        if dictionary.get(key) is dict:
#            print "Decending into subtree {0}".format(key)
#            dictionary[key] = dict_sweep(dictionary[key])
#        elif dictionary.get(key) is list:
#            if value in dictionary[key]:
#                print "Deleting value {0} from list {1} for key {2}".format(value, dictionary[key], key)
#                dictionary[key] = dictionary[key].remove(value)
#        elif dictionary.get(key) == value:
#            print "Deleting key-value pair {0} {1} from dictionary".format(key, value)
#            del dictionary[key]
#    return dictionary
