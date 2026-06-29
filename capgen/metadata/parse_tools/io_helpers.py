"""File-write helpers with no-op-if-unchanged semantics.

The original ``ccpp-prebuild`` and ``ccpp-capgen`` both avoided rewriting
generated cap files when their content was unchanged — preserving each
file's mtime so downstream build systems (CMake, Make, Ninja) don't
trigger unnecessary recompilation cascades.  This module reproduces that
behaviour for ``capgen``.

Staging strategy
----------------
A naive ``open(path, 'w')`` always touches the mtime, even when the
content is identical.  Instead each writer builds the file's content in
memory and calls :func:`write_if_changed`, which:

1. Reads the existing file at *file_path* (if any).
2. If the existing content matches the new content byte-for-byte, returns
   ``False`` without touching the filesystem.
3. Otherwise writes the new content to a sibling temp file (in the same
   directory, which sits **under the generator's output root** — never
   ``/tmp``, so this works on systems that disallow ``/tmp`` writes) and
   then ``os.replace``s it over the target.  Same-directory replace is
   atomic on POSIX and Windows; no partial writes.

For writers that already produce content via a ``with open(...) as fh``
pattern, use :func:`open_if_changed` as a drop-in replacement — it yields
a string buffer that gets staged on context exit.
"""

import io
import logging
import os
import tempfile
from contextlib import contextmanager
from typing import Iterator, Optional


def write_if_changed(
    file_path: str,
    content: str,
    encoding: str = 'utf-8',
    logger: Optional[logging.Logger] = None,
) -> bool:
    """Write *content* to *file_path* iff it differs from existing content.

    Parameters
    ----------
    file_path : str
        Absolute or relative path of the target file.
    content : str
        Full file content to write.
    encoding : str
        Encoding passed to :func:`open` for both the read-back comparison
        and the staged write.  Defaults to ``'utf-8'`` to match every
        capgen writer.
    logger : logging.Logger, optional
        When supplied, the helper logs an ``info``-level message after
        each call: ``"Wrote <path>"`` if the file was newly written or
        rewritten, or ``"Unchanged: <path>"`` if the existing content
        matched and the filesystem was left untouched.  Callers want
        this so end users can tell at a glance which generated files
        actually changed on a rerun (the original ccpp-prebuild /
        ccpp-capgen output distinguished the two cases too).

    Returns
    -------
    bool
        ``True`` if the file was written or replaced; ``False`` if the
        existing content already matched.

    Notes
    -----
    The temp file is created in the target's parent directory via
    :func:`tempfile.mkstemp` (which generates a unique name and opens it
    with ``O_EXCL`` semantics).  On any exception, the temp file is
    removed so we never leak ``.capgen_tmp_*`` artifacts.  Crucially the
    staging directory is the target's parent — which is under the
    generator's output root — so no ``/tmp`` access is required.
    """
    parent = os.path.dirname(os.path.abspath(file_path)) or '.'
    os.makedirs(parent, exist_ok=True)

    if os.path.isfile(file_path):
        try:
            with open(file_path, 'r', encoding=encoding) as fh:
                existing = fh.read()
            if existing == content:
                if logger is not None:
                    logger.info("Unchanged: %s", file_path)
                return False
        except (OSError, UnicodeDecodeError):
            # Fall through to overwrite if the existing file is
            # unreadable or has a different encoding.
            pass

    tmp_fd, tmp_path = tempfile.mkstemp(
        dir=parent,
        prefix='.capgen_tmp_',
        suffix='_' + os.path.basename(file_path),
    )
    try:
        with os.fdopen(tmp_fd, 'w', encoding=encoding) as fh:
            fh.write(content)
        os.replace(tmp_path, file_path)
    except BaseException:
        try:
            os.unlink(tmp_path)
        except OSError:
            pass
        raise
    if logger is not None:
        logger.info("Wrote %s", file_path)
    return True


@contextmanager
def open_if_changed(
    file_path: str,
    mode: str = 'w',
    encoding: str = 'utf-8',
    logger: Optional[logging.Logger] = None,
) -> Iterator[io.StringIO]:
    """Drop-in replacement for ``open(file_path, 'w', encoding=...)``.

    Yields an in-memory :class:`io.StringIO` buffer.  When the context
    exits without an exception, the buffer's contents are handed to
    :func:`write_if_changed` — so the on-disk file is only touched when
    the content actually changes.

    Only text-mode writing is supported; *mode* must be ``'w'`` (the
    parameter exists for call-site parity with the stdlib ``open``).

    Example
    -------
    Refactor::

        with open(out_path, 'w', encoding='utf-8') as fh:
            fh.write('\\n'.join(lines) + '\\n')

    into::

        with open_if_changed(out_path) as fh:
            fh.write('\\n'.join(lines) + '\\n')
    """
    if mode != 'w':
        raise ValueError(
            "open_if_changed only supports text-write mode 'w'; "
            "got mode={!r}".format(mode)
        )
    buf = io.StringIO()
    yield buf
    write_if_changed(file_path, buf.getvalue(), encoding=encoding,
                     logger=logger)
