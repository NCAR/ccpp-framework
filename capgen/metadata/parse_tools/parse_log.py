"""Shared logger utilities for parse processes."""

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
    logger.setLevel(level)


def _remove_handlers(logger):
    for handler in list(logger.handlers):
        logger.removeHandler(handler)


def set_log_to_stdout(logger):
    _remove_handlers(logger)
    logger.addHandler(logging.StreamHandler())


def set_log_to_null(logger):
    _remove_handlers(logger)
    logger.addHandler(logging.NullHandler())
