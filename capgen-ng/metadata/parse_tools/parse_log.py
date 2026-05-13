#!/usr/bin/env python3

"""Shared logger utilities for parse processes.

Copied from scripts/parse_tools/parse_log.py.
"""

import logging


def init_log(name, level=None):
    """Initialize and return a named logger.

    Defaults to WARNING level when *level* is not specified and the logger
    has no existing level set.

    >>> logger = init_log('test_logger')
    >>> logger.name
    'test_logger'
    """
    logger = logging.getLogger(name)
    llevel = logger.getEffectiveLevel()
    if level is None and llevel == logging.NOTSET:
        logger.setLevel(logging.WARNING)
    elif level:
        logger.setLevel(level)
    set_log_to_stdout(logger)
    return logger


def set_log_level(logger, level):
    """Set *logger*'s level to *level*."""
    logger.setLevel(level)


def remove_handlers(logger):
    """Remove all handlers from *logger*."""
    for handler in list(logger.handlers):
        logger.removeHandler(handler)


def set_log_to_stdout(logger):
    """Direct *logger* output to standard output."""
    remove_handlers(logger)
    logger.addHandler(logging.StreamHandler())


def set_log_to_null(logger):
    """Suppress all *logger* output."""
    remove_handlers(logger)
    logger.addHandler(logging.NullHandler())


def set_log_to_file(logger, filename):
    """Direct *logger* output to *filename*."""
    remove_handlers(logger)
    logger.addHandler(logging.FileHandler(filename))


def flush_log(logger):
    """Flush all pending output from *logger*."""
    for handler in list(logger.handlers):
        handler.flush()


def verbose(logger):
    """Return True if *logger* is at DEBUG level."""
    return logger.isEnabledFor(logging.DEBUG)
