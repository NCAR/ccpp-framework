"""Parse utilities shared across metadata parsing and validation."""

from .parse_source import (
    CCPPError,
    ParseSyntaxError,
    ParseInternalError,
    ParseContext,
    context_string,
)
from .parse_log import init_log, set_log_level, set_log_to_null, set_log_to_stdout
from .parse_checkers import (
    check_units,
    check_dimensions,
    check_cf_standard_name,
    check_diagnostic_fixed,
    check_diagnostic_id,
    check_fortran_id,
    check_fortran_ref,
    check_fortran_type,
    check_fortran_intrinsic,
    check_molar_mass,
    FORTRAN_SCALAR_REF_RE,
)
from .fortran_conditional import FORTRAN_CONDITIONAL_REGEX
from .xml_tools import (
    read_xml_file,
    find_schema_version,
    expand_nested_suites,
    write_xml_file,
)
from .io_helpers import (
    write_if_changed,
    open_if_changed,
)
