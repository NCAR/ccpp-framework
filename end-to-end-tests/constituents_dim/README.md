# constituents_dim test

Covers variables dimensioned by the framework constituent count
number_of_ccpp_constituents (which the host never declares as a scalar — the
framework owns it). register_consts registers 3 dynamic constituents, so the
count is 3 for the rest of the suite. Three cases, each a distinct capgen path:

- Case 1 — host var dimensioned by the count. surface_upward_test_constituent_flux
  (horizontal_dimension, number_of_ccpp_constituents) is host-owned; the host
  sizes it to the runtime count and capgen passes the whole constituent axis as
  : to const_dim_producer.
- Case 2a — non-allocatable suite var, framework allocates in init_fields.
  test_constituent_workspace(number_of_ccpp_constituents) is suite-owned and
  not allocatable, so suite_data_init_fields allocates it via
  ccpp_model_constituents_obj(i)%num_layer_vars.
- Case 2b — allocatable suite var, the scheme allocates in _run.
  test_allocatable_constituent_workspace(number_of_ccpp_constituents) is
  allocatable = True; init_fields skips it and const_dim_producer allocates
  it using the count received as a scalar (number_of_ccpp_constituents -->
  ccpp_model_constituents_obj(inst)%num_layer_vars). Uses the existing
  suite-owned-allocatable path. final_fields frees both suite workspaces.

The producer fills the workspaces and verifies Case 1; the consumer verifies
Cases 2a/2b. Any mismatch sets errcode, failing the run. Built with
-fcheck=all, so allocation/teardown errors fail the test.

The 2a vs 2b contrast is the same ownership rule as suite_allocate: a
non-allocatable suite var is framework-owned (allocated in init_fields); an
allocatable = True one is scheme-owned (allocated in _run).
