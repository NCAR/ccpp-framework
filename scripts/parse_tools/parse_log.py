#!/usr/bin/env python

"""Shared logger for parse processes"""

# Python library imports
import logging
# CCPP framework imports

def init_log(name, level=None):
    logger = logging.getLogger(name)
    # Turn logging to WARNING if not set
    llevel = logger.getEffectiveLevel()
    if (level is None) and (llevel == logging.NOTSET):
        logger.setLevel(logging.WARNING)
    elif level:
        logger.setLevel(level)
    # End if
    set_log_to_stdout(logger)
    return logger

def set_log_level(logger, level):
    logger.setLevel(level)

def remove_handlers(logger):
    for handler in list(logger.handlers):
        logger.removeHandler(handler)

def set_log_to_stdout(logger):
    remove_handlers(logger)
    logger.addHandler(logging.StreamHandler())

def set_log_to_null(logger):
    remove_handlers(logger)
    logger.addHandler(logging.NullHandler())

def set_log_to_file(logger, filename):
    remove_handlers(logger)
    logger.addHandler(logging.StreamHandler())

def flush_log(logger):
    for handler in list(logger.handlers):
        handler.flush()
