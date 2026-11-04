"""File-write helpers with no-op-if-unchanged semantics.

This module implements the same no-op-if-unchanged semantics as the original
``ccpp-prebuild`` and ``ccpp-capgen`` — avoiding rewriting generated cap files
when their content was unchanged — preserving each file's mtime so downstream
build systems (CMake, Make, Ninja) don't trigger unnecessary recompilation
cascades.

Two separate concerns
---------------------
:func:`write_if_changed` handles *whether* to write and *how* to write
independently — they are different problems and solved by different means:

- **Whether** — the new content is compared in memory against the existing
  file.  If it is byte-for-byte identical the file is left untouched (mtime
  preserved), so CMake/Make/Ninja don't see a spurious change and trigger a
  needless recompilation cascade.  Because the comparison is in memory,
  nothing is written to disk in the common unchanged case.

- **How** — when the content *does* differ it is written to a sibling temp
  file which is then ``os.replace``d over the target.  A same-directory
  replace is atomic on POSIX and Windows, so an interrupted or failed write
  can never leave a partially written / corrupt cap in place for the build
  to compile.  The temp file lives in the target's parent directory (under
  the generator's output root) and is written with the default umask.

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
        and the staged write.  Defaults to ``'utf-8'``.
    logger : logging.Logger, optional
        When supplied, the helper logs an ``info``-level message after
        each call: ``"Wrote <path>"`` if the file was newly written or
        rewritten, or ``"Unchanged: <path>"`` if the existing content
        matched and the filesystem was left untouched.

    Returns
    -------
    bool
        ``True`` if the file was written or replaced; ``False`` if the
        existing content already matched.
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
        # mkstemp opens the temp file 0600; restore the umask-based default
        umask = os.umask(0)
        os.umask(umask)
        os.chmod(tmp_path, 0o666 & ~umask)
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
