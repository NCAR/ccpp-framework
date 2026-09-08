# `--legacy-auto-clone-constituents` — transient shim

A capgen CLI flag that re-enables the **auto-clone-static-constituent**
registration path the original ccpp-capgen toolchain provided to
CAM-SIMA. Off by default; turned on with a single flag and a loud
startup banner. Intended as a migration aid — every line of code it
touches is tagged so the whole feature can be removed cleanly when
legacy hosts have moved on.

## How to enable it

Pass the flag to both `capgen` and `ccpp_validator` (the
ccpp-physics build system already does this for
`end-to-end-tests/advection_auto_clone/`):

```
ccpp_capgen.py     --legacy-auto-clone-constituents  ...
ccpp_validator.py     --legacy-auto-clone-constituents  ...
```

It is **single-instance only**. If the host metadata declares the
`instance_number` + `number_of_instances` pair (capgen's
multi-instance opt-in), the run aborts with a clear error before
parsing any suite. Legacy hosts predate multi-instance support, so
this restriction matches the use case.

## What the flag does

Same shape as original capgen's auto-clone:

For every scheme argument flagged `advected = True`, `constituent = True`,
or `molar_mass = <value>`, capgen synthesises a `%instantiate(...)`
call into the generated host code, lifting field values straight from
the scheme metadata. The constituent ends up registered in the
per-suite dynamic-constituents buffer alongside any constituents the
host or an explicit register-phase scheme registered. The scheme
author writes no Fortran registration code.

These metadata attributes are accepted on scheme arguments when the
flag is on (and rejected when it is off):

| Attribute            | Type                | Passed to `%instantiate` as |
|----------------------|---------------------|-----------------------------|
| `default_value`      | real (kind_phys)    | `default_value`             |
| `min_value`          | real (kind_phys)    | `min_value`                 |
| `water_species`      | logical             | `water_species`             |
| `mixing_ratio_type`  | character           | `mixing_ratio_type`         |

Fortran-style literals (`0.0_kind_phys`, `1.0d-5`, `-3.14_8`,
`1.0d-5_kind_phys`) are accepted for the real-valued attrs, since
legacy metadata writes the values in source form.

The other `%instantiate` kwargs (`std_name`, `long_name`,
`diag_name`, `units`, `vertical_dim`, `advected`, `molar_mass`)
already had accepted spellings in capgen; the shim just wires them
into the synthesised call.

## Defaults that match original capgen

- **`long_name` is synthesised when missing.** If the scheme metadata
  has no `long_name` on a constituent arg, capgen builds one from
  the standard name by replacing underscores with spaces and
  capitalising the first character.
  Example: `cloud_liquid_dry_mixing_ratio` →
  `'Cloud liquid dry mixing ratio'`.
- **`diag_name` falls back to the metadata local name** when neither
  `diagnostic_name` nor `diagnostic_name_fixed` is set.
- **`vertical_dim` is lifted from the scheme arg's `dimensions = (..., <vdim>)`** —
  whichever entry matches `vertical_layer_dimension` or
  `vertical_interface_dimension`; otherwise
  `vertical_layer_dimension`.

## What's stricter than original capgen

Capgen's general rules apply even with the flag on. Two of them
trip up legacy fixtures:

1. **Metadata args must match the Fortran subroutine signature.**
   Original capgen tolerated a metadata arg-table that listed a
   constituent in `<scheme>_init` even when the Fortran `_init`
   didn't accept it as a dummy. Capgen passes the metadata args
   at the call site as Fortran keyword arguments, and the validator
   catches divergence. Either include the constituent as a Fortran
   dummy, or remove it from the init's arg-table.
2. **Base constituents can't be `intent = out`.** A scheme arg with
   `advected = True` (or `constituent = True` / `molar_mass = ...`)
   on a non-`tendency_of_*` standard name has to be `intent = in` or
   `intent = inout`. Only tendency args (std_name starts with
   `tendency_of_`) can be `intent = out`. If the legacy scheme
   wrote to the array in its init routine, change both the metadata
   and the Fortran subroutine to `intent = inout` — the body is
   unchanged.

Multi-instance support is also off-limits while the flag is on (see
above).

## Used in production CAM-SIMA

Most CAM-SIMA atmospheric physics schemes rely on the auto-clone
path the same way original capgen ships it: a handful of schemes
register constituents explicitly (`rrtmgp_constituents`,
`musica_ccpp`, `prescribed_aerosols`, `prescribed_ozone`), and the
~16 others (`kessler`, `zm_convr`, `dadadj`, `holtslag_boville_diff`,
`state_converters`, `geopotential_temp`, `cloud_particle_sedimentation`,
…) declare `advected = True intent = inout` on their `_run`
arguments and let the framework register the constituent. Without
the flag, capgen's runtime check fires for every consumer
because no source actually registered the species. The flag closes
that gap by re-creating the auto-clone behaviour from the metadata.

Production CAM-SIMA does not use `default_value`, `min_value`,
`water_species`, or `mixing_ratio_type` in metadata; the four
extra parser attributes exist to support the advection test (and
any future legacy host that needs the broader kwarg surface on
`%instantiate`).

## `end-to-end-tests/advection_auto_clone/` — what changed

The fixture is a port of CAM-SIMA's `ccpp_framework/test/advection_test`.
It exercises the full legacy attr surface (`default_value`,
`diagnostic_name`, `advected`) and the unusual init-phase
`intent = out`-on-base-constituent pattern. Three small edits were
needed to make the port build under capgen:

1. **`cld_liq.meta`** — in `cld_liq_init`'s `[ cld_liq_array ]`
   block, change `intent = out` to `intent = inout`.
2. **`cld_liq.F90`** — in `cld_liq_init`, change
   `real(kind=kind_phys), intent(out) :: cld_liq_array(:, :)` to
   `intent(inout)`. The body still does
   `cld_liq_array = 0.0_kind_phys`; inout is a strict superset.
3. **`cld_ice.meta`** — delete the `[ cld_ice_array ]` block inside
   `cld_ice_init`. Fortran `cld_ice_init` only takes
   `(tfreeze, errmsg, errflg)`; the run phase already triggers
   auto-clone registration of `cloud_ice_dry_mixing_ratio`.

No `long_name` additions to the metadata were necessary — capgen
synthesises the long_name from the standard name automatically
(see the defaults section above).

The CTest target `test_advection_auto_clone` passes after these edits.

## When to retire the flag

When all consumers have been moved to capgen's explicit
registration model — either by declaring constituents in the host's
`host_constituents(:)` array, or by writing a register-phase scheme
with a `ccpp_constituent_properties_t(:), intent=out` argument that
calls `%instantiate` directly. At that point the flag is no longer
needed by any host, and the shim can be removed in one cleanup
pass.
