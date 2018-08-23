#!/usr/bin/env python

"""Shared logger for parse processes"""

# Python library imports
import logging
# CCPP framework imports

def initLog(name, level=None):
    logger = logging.getLogger(name)
    # Turn logging to WARNING if not set
    llevel = logger.getEffectiveLevel()
    if (level is None) and (llevel == logging.NOTSET):
        logger.setLevel(logging.WARNING)
    # End if
    setLogToStdout(logger)
    return logger

def setLogLevel(logger, level):
    logger.setLevel(level)

def removeHandlers(logger):
    for handler in list(logger.handlers):
        logger.removeHandler(handler)

def setLogToStdout(logger):
    removeHandlers(logger)
    logger.addHandler(logging.StreamHandler())

def setLogToNull(logger):
    removeHandlers(logger)
    logger.addHandler(logging.NullHandler())

def setLogToFile(logger, filename):
    removeHandlers(logger)
    logger.addHandler(logging.StreamHandler())
