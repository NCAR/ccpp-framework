#!/usr/bin/env python

"""Shared logger for parse processes"""

# Python library imports
import logging
# CCPP framework imports

logger = None

def initLog(name, level=None):
    global logger
    logger = logging.getLogger(__name__)
    # Turn logging to WARNING if not set
    llevel = logger.getEffectiveLevel()
    if (level is None) and (llevel == logging.NOTSET):
        logger.setLevel(logging.WARNING)
    # End if
    return logger

def setLogLevel(level):
    logger.setLevel(level)

def removeHandlers():
    for handler in list(logger.handlers):
        logger.removeHandler(handler)

def setLogToStdout():
    logger.addHandler(logging.StreamHandler())

def setLogToNull():
    logger.addHandler(logging.NullHandler())

def setLogToFile(filename):
    logger.addHandler(logging.StreamHandler())
