"""Shared logger utilities for parse processes."""

import logging


def init_log(name, level=None):
    """Initialize and return a named logger writing to stdout.

    When *level* is given it is applied; otherwise the logger inherits the
    root default (WARNING).
    """
    logger = logging.getLogger(name)
    if level:
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
