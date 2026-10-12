# suite_allocate test

Covers the one suite-data feature the rest of the tests do not: a
suite-owned, scheme-allocated variable (allocatable = True).

scratch_workspace_field is produced by make_workspace (intent=out,
allocatable = True) and consumed by use_workspace. No host table declares
it, so capgen promotes it to a suite-owned variable stored in
ccpp_<suite>_data.

Crucially, its dimension workspace_dimension is also suite-owned: it is set
by use_workspace in the timestep_init phase (which runs after
ccpp_init/suite_data_init_fields) — even though use_workspace is listed
after make_workspace in the suite. Phases run suite-wide in order, so the
dimension set in timestep_init is available to every scheme's run. Because
the size is unknown at init, init_fields cannot allocate the array; the
producing scheme must, in the run phase. A non-allocatable version of this is
exactly what validate_init_dimensions rejects. Because it is allocatable:

- suite_data_init_fields must skip its allocation (the scheme owns it),
- the producing scheme allocates the suite-data component at run time,
- the whole allocated component is passed to the (non-allocatable) consumer dummy,
- suite_data_final_fields frees it (guarded; suite owns teardown).

The driver asserts the consumer's reduction (workspace_checksum == nw*(nw+1)/2)
and a clean error code. Built with -fcheck=all, so any double-free or
use-after-free in the scheme-allocates / suite-frees ownership split fails the
test.

This is distinct from:
- capgen — suite-owned vars allocated at init from a register-set dim
  (non-allocatable path), and a host-owned allocatable var (model_times).
- nested_suite / var_compat — standalone DDTs with module_name.
