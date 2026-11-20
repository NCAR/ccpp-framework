"""Free-form Fortran source-form limits.

Free-form Fortran source ends at column 132.  Past that column a compiler
is free to ignore what it reads, and they differ in what they do: gfortran
truncates with a warning unless ``-ffree-line-length-none`` says otherwise,
Intel truncates, LLVM flang reads the whole line.  Capgen therefore has to
treat 132 as a hard limit in both directions, and both read the number from
here:

- **Input** -- ``ccpp_validator`` checks every Fortran file it is asked to
  parse against the limit before parsing it, so capgen never reads text the
  compiler will not see (``doc/followups.md`` FU-038).
- **Output** -- the cap writers wrap every emitted line to the limit
  (``generator.fortran_lines``), so generated code compiles without a
  line-length flag.

The limit is not configurable: 132 is where the standard's free form ends,
so it is the standard's number rather than a CCPP preference.  A group
wanting a stricter limit enforces that with its own linter.
"""

FREE_FORM_MAX_COLUMNS = 132
