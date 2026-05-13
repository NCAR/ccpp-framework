# Migrating from ccpp-prebuild / ccpp-capgen to capgen-ng

This document captures the **user-facing differences** a host model author
or scheme author needs to know when moving metadata, suite XML, and host
Fortran from the legacy ccpp-prebuild + ccpp-capgen toolchain to
**capgen-ng**.  It complements `doc/redesign_prompt.md` (design spec) and
`doc/redesign_analysis.md` (analysis of the old systems).

Generated as of 2026-05-13.  Current unit-test suite: 1113 passing.

**Repository layout** (post-2026-05-13 cleanup): tooling lives under
`capgen-ng/` (top-level of this repo).  Unit tests live at the top
level in `unit-tests/`; end-to-end tests in `end-to-end-tests/`.  Run
the unit suite from the repo root with `python -m pytest unit-tests/`.

## Table of contents

1. [Metadata format changes](#1-metadata-format-changes)
   1. [1.8 `horizontal_loop_extent` → `horizontal_dimension`](#18-horizontal_loop_extent--horizontal_dimension)
2. [Suite definition file (SDF) changes](#2-suite-definition-file-sdf-changes)
3. [Host Fortran requirements](#3-host-fortran-requirements)
4. [Generator CLI and build integration](#4-generator-cli-and-build-integration)
5. [Generated cap layout — what's new and what changed](#5-generated-cap-layout--whats-new-and-what-changed)
6. [Framework changes (constituents)](#6-framework-changes-constituents)
   1. [6.3 Host metadata wins over auto-provisioning](#63-host-metadata-wins-over-auto-provisioning-2026-05-12)
7. [Validator (`ccpp_validator.py`)](#7-validator)
8. [Known gaps and deferred items](#8-known-gaps-and-deferred-items)

---

## 1. Metadata format changes

### 1.1 Table types

Four `type =` values in `[ccpp-table-properties]`:

| Type    | Contents                                                |
|---------|---------------------------------------------------------|
| `control` | Control variables passed as ``ccpp_physics_*`` args.  |
| `host`    | Host-model variables imported via `use`.              |
| `ddt`     | Derived-type definitions.                              |
| `scheme`  | Scheme metadata.                                       |

The legacy `type = module` (capgen) becomes `type = host`.  The legacy
`TYPEDEFS_NEW_METADATA` Python dict (prebuild) is replaced by `type = ddt`
tables.  See `doc/redesign_prompt.md` §3.2.

### 1.2 New table-property attributes

All optional inside the `[ccpp-table-properties]` block:

| Attribute             | Applies to            | Description |
|-----------------------|-----------------------|-------------|
| `module_name`         | scheme, host, ddt     | Fortran module name; overrides "module name = table name" when they differ. |
| `dependencies`        | any                   | Comma-separated list of file paths to compile.  **May appear multiple times** in one block (new this session); single occurrence still accepted. |
| `dependencies_path`   | any                   | Relative base for `dependencies` entries.  Single-valued. |
| `source_path`         | any                   | Relative path to the Fortran source.  Single-valued. |
| `kind_spec`           | any                   | `<module>:<kind_name>=>spec` (or shorthand).  May appear multiple times. |

Example with multi-line dependencies (real CCPP physics pattern):

```
[ccpp-table-properties]
  name = GFS_rrtmg_setup
  type = scheme
  module_name = GFS_rrtmg_setup     # optional when names match
  dependencies_path = ../../
  dependencies = tools/mpiutil.F90
  dependencies = hooks/machine.F
  dependencies = Radiation/RRTMG/radlw_main.F90,Radiation/RRTMG/radsw_main.F90
```

### 1.3 New per-variable attributes

Inside a `[ var_name ]` section.  All optional.

| Attribute        | Type | Default | Notes |
|------------------|------|---------|-------|
| `top_at_one`     | bool | `False` | When host and scheme disagree, generator emits a vertical-flip transform with reverse-stride subscript on the host side.  Meaningless on variables without a vertical dimension. |
| `constituent`    | bool | `False` | Scheme metadata only.  Marks the var as a constituent reference. |
| `advected`       | bool | `False` | Scheme metadata only. |
| `molar_mass`     | float | `0.0`  | Scheme metadata only. |
| `diagnostic_name` | str | (defaults to `local_name`) | Host-tooling hint; mutually exclusive with `diagnostic_name_fixed`. |

### 1.4 Sliced local names with long subscript indices

Local names with array slices may carry CCPP standard names as subscript
tokens:

```
[ dqdt(:,:,index_of_cloud_liquid_water_mixing_ratio_in_tracer_concentration_array) ]
  standard_name = ...
```

The 63-char Fortran-identifier limit is enforced only on the base
identifier (`dqdt`), not on subscript tokens (which are CCPP standard
names resolved at codegen time and routinely exceed 63 chars).

### 1.5 Unit strings: bare vs explicit positive exponent

`m2` and `m+2` (or any `<letter><positive_integer>` vs `<letter>+<integer>`
combo) are normalised internally and treated as equivalent.  Pre-existing
unit-conversion entries don't need to be duplicated; either spelling
matches.

### 1.6 Improved error messages

- **Duplicate standard name**: error message now lists both colliding
  access paths and hints at the "sibling DDT instance" pattern (when
  applicable).
- **Subcycle bound unresolved**: error names the std_name and points
  at the control/host metadata as the fix.
- **Instance-dim used without `instance_number`**: error explains the
  paired-opt-in requirement (see §1.7).

### 1.7 Optional `instance_number` / `number_of_instances` pair

These two control variables are now **paired optional**:

- Declare **both** (`instance_number` in `type=control`,
  `number_of_instances` in `type=host`) → multi-instance API.
- Declare **neither** → single-instance API.  Public entry points drop
  `instance_number`; internal per-instance arrays size to length 1.
- Declare exactly one → hard error from the validator.

Hosts that don't need multi-instance bookkeeping can drop both declarations.

### 1.8 `horizontal_loop_extent` → `horizontal_dimension`

ccpp-prebuild / original ccpp-capgen used `horizontal_loop_extent` as
the horizontal-axis std name in scheme metadata.  capgen-ng uses
`horizontal_dimension` uniformly — the run-vs-non-run distinction
isn't expressed in scheme metadata anymore (host passes
`horizontal_loop_begin`/`horizontal_loop_end` as control vars and the
generated cap slices accordingly).

Migration paths:

1. **Edit the metadata** (recommended) — search-and-replace
   `horizontal_loop_extent` → `horizontal_dimension` in every scheme
   `.meta` you maintain.
2. **Use `--legacy-mode`** (transient) — pass `--legacy-mode` to both
   `ccpp_capgen_ng.py` and `ccpp_validator.py` and the rename happens
   at parse time.  A loud warning banner prints at startup so the
   rewrite is never invisible.  This shim *will be removed*; treat
   it as a runway, not a destination.

---

## 2. Suite definition file (SDF) changes

### 2.1 Schema v2.0 with nested-suite expansion

Capgen-ng parses v2.0 SDFs and expands `<nested_suite>` references
recursively at parse time.  See `doc/redesign_prompt.md` §3 and the
`suite_v2_0.xsd` schema.

### 2.2 `<subcycle>` with CCPP standard-name loop bound

```xml
<subcycle loop="num_subcycles_for_effr">
  <scheme>effr_pre</scheme>
</subcycle>
```

The `loop=` attribute accepts:

- **Integer literal** (`loop="3"`) — emitted verbatim.
- **CCPP standard name** (`loop="num_subcycles_for_effr"`) — resolved
  against host/control metadata; supports DDT-component access paths
  (e.g. `phys_state%num_subcycles`).
- **Absent / empty** — treated as `loop="1"`.

The loop-bound standard name is automatically included in the
introspection inputs list (`ccpp_physics_suite_variables` and
`_suite_host_data`).

### 2.3 Nested `<subcycle>` elements

```xml
<subcycle loop="3">
  <subcycle loop="2">
    <scheme>effr_calc</scheme>
  </subcycle>
</subcycle>
```

Nested subcycles produce nested `do` loops in the generated cap.  Loop
counter variables follow the convention:

- Outermost / single-level: `ccpp_loop_counter`.
- Each deeper level: `ccpp_loop_counter_2`, `ccpp_loop_counter_3`, ...

Effective iteration count = product of every level's `loop=` value.
`effr_calc` in the example runs 3·2 = 6 times.

### 2.4 Suite-level `<init>` and `<final>` schemes

```xml
<suite name="my_suite" version="2.0">
  <init>my_init_scheme</init>
  <group name="physics">...</group>
  <final>my_final_scheme</final>
</suite>
```

- Each element contains a **single** scheme name as text content.
  Multiple `<scheme>` children inside `<init>`/`<final>` is a schema
  violation.  (Group-shaped lists belong inside `<group>`.)
- The named scheme's `init` / `final` phase metadata is resolved like
  any other scheme phase; missing-phase metadata is a generator error.
- The scheme call is emitted inside `<suite>_init` / `<suite>_final`
  with USE for the scheme module + per-arg host modules, and the
  standard errflg check.
- Call ordering:
  - `<suite>_init`: after all group `state_alloc` and
    `suite_data_init_fields`, **before** the `CCPP_SUITE_FRAMEWORK_INITIALIZED`
    state transition.
  - `<suite>_final`: before the `CCPP_SUITE_UNREGISTERED` transition.

**Accepted spellings**: `<init>` and `<final>` only.  Legacy spellings
**`<initalize>`** (typo), **`<initialize>`** (correct long form), and
**`<finalize>`** are rejected with a clear error pointing at the
canonical short form.

To exercise:

1. Declare a scheme with `init` and/or `final` phases in its metadata
   (minimal sig — just `errmsg` + `errflg` — is fine).
2. Reference it in the SDF as shown above.
3. Add the scheme's `.F90` to your build's source list.

---

## 3. Host Fortran requirements

### 3.1 Required control variables

Every host's `type=control` table must declare:

| Standard name                     | Fortran type | Purpose                           |
|-----------------------------------|--------------|-----------------------------------|
| `suite_name`                      | character    | Drives suite dispatch             |
| `horizontal_loop_begin`           | integer      | Lower chunk-bound                 |
| `horizontal_loop_end`             | integer      | Upper chunk-bound                 |
| `thread_number`                   | integer      | Current thread                    |
| `number_of_threads`               | integer      | Total threads                     |
| `number_of_physics_threads`       | integer      | Physics-internal budget           |
| `ccpp_error_code`                 | integer      | Error flag                        |
| `ccpp_error_message`              | character    | Error message                     |

Optional (paired — see §1.7):

| Standard name           | Fortran type | Table type | Purpose                        |
|-------------------------|--------------|------------|--------------------------------|
| `instance_number`       | integer      | control    | Current instance index         |
| `number_of_instances`   | integer      | host       | Total instance count           |

### 3.2 Required entry-point call sequence

```
ccpp_register(suite_name, errflg, errmsg, [instance_number])
  └── per scheme that declares a register phase
ccpp_init(suite_name, errflg, errmsg, [instance_number])
  └── per scheme that declares an init phase
ccpp_physics_init(...)
  └── physics phase routines per group:
      ccpp_physics_init
      ccpp_physics_timestep_init
      ccpp_physics_run                    ← run-loop phase
      ccpp_physics_timestep_final
      ccpp_physics_final
ccpp_final(suite_name, errflg, errmsg, [instance_number])
```

`instance_number` appears in every signature only when the host
declares the `instance_number` / `number_of_instances` pair (§1.7).

### 3.3 Host module convention

The Fortran module that exports a host metadata table's variables is
typically named after the table.  When that's not the case, use the
`module_name` table-property override (§1.2):

```
[ccpp-table-properties]
  name = test_host_data
  type = host
  module_name = mod_test_host_data
```

---

## 4. Generator CLI and build integration

### 4.1 `ccpp_capgen_ng.py` invocation

```
python ccpp_capgen_ng.py \
    --host-files <host.meta>[,<ddt.meta>,...] \
    --scheme-files <scheme1.meta>[,<scheme2.meta>,...] \
    --suites <suite1.xml>[,<suite2.xml>,...] \
    --host-name <host_identifier> \
    --output-root <build_dir>/ccpp \
    [--kind-type <name>=[<module>:]<spec>] \
    [--legacy-mode] \
    [--verbose] [--verbose]
```

`--kind-type` syntax: `<name>=[<module>:]<spec>`.  When `<module>:` is
omitted, `<spec>` must be an ISO_FORTRAN_ENV constant (REAL32/REAL64/...)
and the module defaults to `iso_fortran_env`.  `kind_phys` is
auto-defaulted to `iso_fortran_env:REAL64` when not supplied.

`--legacy-mode` (transient migration shim, will be removed): silently
rewrites legacy CCPP standard names that ccpp-prebuild / original
ccpp-capgen used to their capgen-ng equivalents at parse time.
Currently translates `horizontal_loop_extent` → `horizontal_dimension`.
Prints a loud warning banner at startup so the rewrite is never
invisible.  Available on both `ccpp_capgen_ng.py` and `ccpp_validator.py`
(keep the flag consistent between the two when both are invoked from
CMake).  All translation logic is isolated in
`metadata/legacy_compat.py` and tagged with `# legacy-compat:` comments
at every touchpoint, so the shim can be cleanly removed when migration
is complete.

### 4.2 `ccpp_datafile.py` query CLI

Generated `datatable.xml` carries:

- `<capgen_files>` — generated outputs (utilities/host_files/suite_files).
- `<inspection_files>` — `<suite>.meta` and expanded SDF.
- `<schemes>` — per-scheme call lists.
- `<var_dictionaries>` — host/api/suite/group dictionaries.

Query via `ccpp_datafile.py --<flag> <datatable.xml>`.  Flags include
`--dependencies`, `--capgen-files`, `--host-files`, `--utility-files`,
`--suite-list`, `--required-variables <suite>`, `--input-variables <suite>`,
`--output-variables <suite>`, `--host-variables`, `--show`.

### 4.3 CMake helpers

`cmake/ccpp_capgen.cmake` and `cmake/ccpp_validator.cmake` provide the
`ccpp_capgen(...)` and `ccpp_validator(...)` macros.  `ccpp_datafile(...)`
queries datatable.xml at configure time.

---

## 5. Generated cap layout — what's new and what changed

### 5.1 Output files

Always generated:

- `ccpp_kinds.F90` — kind parameters.  Listed under `<utilities>`.
- `ccpp_static_api.F90` — public host-facing entry points + introspection routines.
- `ccpp_<suite>_cap.F90` — per-suite dispatcher.
- `ccpp_<suite>_<group>_cap.F90` — per-group phase implementations.
- `ccpp_<suite>_data.F90` — suite-owned interstitial DDT + module-level array.
- `ccpp_<suite>_types.F90` — pointer-wrapper types for optional args.
- `ccpp_<suite>.meta` — inspection artifact; matches the generated cap.
- `datatable.xml` — build-system + host-introspection metadata.

When any scheme registers constituents:

- `ccpp_host_constituents.F90` — owns `ccpp_model_constituents_obj(:)`
  and the host-facing constituent API.

### 5.2 Per-suite data: TARGET on the instance array

`ccpp_suite_data(:)` carries the `TARGET` attribute:

```fortran
type(ccpp_<suite>_data_t), allocatable, target, public :: ccpp_suite_data(:)
```

This makes every `ccpp_suite_data(i)%component(...)` subobject a valid
pointer-assignment target — needed for transformation temps and
optional-arg pointer wrappers.

### 5.3 Variable transformations

The generator emits three kinds of transform on a per-arg basis:

| Transform        | Trigger                                          |
|------------------|--------------------------------------------------|
| Unit conversion  | `host.units != scheme.units` with a registered conversion entry. |
| Kind conversion  | `host.kind != scheme.kind` (different strings).  |
| Vertical flip    | `host.top_at_one != scheme.top_at_one` on a var with a vertical dim. |

These compose.  A scheme arg that needs unit + flip emits a single
combined assignment through a transformation temp:

```fortran
temp_l = 1.0E-3_kind_phys*host_var(lb:ub, nlev:1:-1)  ! unit conversion: kind_phys to kind_phys; vertical flip (top_at_one mismatch)
call scheme_run(temp=temp_l, ...)
host_var(lb:ub, nlev:1:-1) = 1.0E+3_kind_phys*temp_l  ! ... reverse ...
```

Identity unit conversions (registered for dimensionally-equivalent
spellings like `J kg-1 ↔ m2 s-2`, formula `'{var}'`) are not labelled
"unit conversion" in the comment.

### 5.4 Subcycle emission

```fortran
integer :: ccpp_loop_counter
integer :: ccpp_loop_counter_2
...
do ccpp_loop_counter = 1, phys_state%num_subcycles   ! outer
  call scheme_pre(...)
  do ccpp_loop_counter_2 = 1, 2                       ! inner
    call scheme_calc(...)
  end do
end do
```

### 5.5 State machine

Per-instance integer state arrays:

- `ccpp_suite_state(:)` — suite-level (UNREGISTERED / REGISTERED /
  FRAMEWORK_INITIALIZED).
- `ccpp_group_state(:)` — group-level (UNINITIALIZED / INITIALIZED /
  IN_TIMESTEP).

Single-instance hosts get length-1 arrays indexed with literal `1`.
See `doc/redesign_prompt.md` §7.

---

## 6. Framework changes (constituents)

### 6.1 `ccpp_constituent_prop_mod` ownership flag

(Framework PR — needs upstream merge.)  Adds:

- `framework_owns_me` private flag on `ccpp_constituent_properties_t`,
  default `.false.`.
- `set_framework_owned(value)` setter (call before
  `obj%new_field(const_prop, ...)` when transferring ownership).
- `is_framework_owned()` getter.
- `ccpt_deallocate` only frees when the flag is set; otherwise just
  nullifies.

Backward-compatible.  Original capgen's auto-clone path in
`scripts/constituents.py` has been updated to call the setter.

### 6.2 capgen-ng constituent API

(See `doc/constituents.md` for the full reference.)  Highlights:

- One `ccpp_model_constituents_obj(:)` array per generator invocation,
  sized to `number_of_instances`.
- Host-facing API:
  - `ccpp_register_constituents(host_constituents, instance_number, ...)`
  - `ccpp_initialize_constituents(ncols, num_layers, instance_number, ...)`
  - `ccpp_const_get_index(stdname, const_index, instance_number, ...)`
  - `ccpp_constituents_array(instance_number) → pointer`
  - `ccpp_advected_constituents_array(instance_number) → pointer`
  - `ccpp_model_const_properties(instance_number) → pointer`
  - `ccpp_number_constituents(num_flds, advected, instance_number, ...)`
  - `ccpp_gather_constituents`, `ccpp_update_constituents`
  - `ccpp_is_scheme_constituent(var_name, ...)` (not per-instance)
- Scheme-side registration: four rules — register-phase
  `ccpp_constituent_properties_t(:)` arg, consume base via
  `advected=true intent=in/inout`, produce tendency via
  `constituent=true intent=out` + `tendency_of_<X>` std name, mismatches
  are codegen errors.

### 6.3 Host metadata wins over auto-provisioning (2026-05-12)

If the host declares a framework-named standard name
(`ccpp_constituents` / `ccpp_constituent_tendencies` /
`ccpp_constituent_properties` / `number_of_ccpp_constituents` /
`index_of_<X>`) as a regular host variable, the resolver uses the
host's declaration and skips capgen-ng auto-provisioning.  Matters
most for legacy hosts (GFS / SCM) that own their own tracer
indices — e.g. `[ntcw]` with `standard_name =
index_of_cloud_liquid_water_mixing_ratio_in_tracer_concentration_array`
resolves to the host's short local name `ntcw`, not a parallel
module-level integer named after the full standard name (which
would also blow Fortran's 63-char identifier limit).  See
`doc/constituents.md` §3.

Active design review for the next constituents iteration:
`doc/constituents_overhaul.md` (Class A vs Class B property
classification, three reform proposals).

---

## 7. Validator

`capgen-ng/ccpp_validator.py` — standalone Fortran-vs-metadata checker.
Today validates **scheme** metadata against scheme Fortran files
(subroutine signatures, optional args, paren-aware decl splitting).

Continuation-line handling covers both free-form (`&` at trailing end
of prior line only) and fixed-form / dual-form (`&` at both ends, with
the leading marker at column 6).  Comment-only and blank lines
interleaved between continuation lines are skipped as Fortran 90+
permits.

When the signature parser finds a subroutine but extracts zero args
while metadata declares many, the "Argument count mismatch" error
appends a HINT pointing at the parser rather than masquerading as a
real mismatch — common cause is an unsupported signature feature.

**Known gap**: host-metadata validation is not yet implemented.  When
invoked with non-scheme `.meta` files, the validator silently filters
to zero schemes and reports "Validation passed."  Slated for revisit
after the e2e test suite settles (`unit_conv` + `variable_transform`
complete).  See `project_validator_host_check_deferred.md` (memory).

---

## 8. Known gaps and deferred items

| Item                                       | Status                                        |
|--------------------------------------------|-----------------------------------------------|
| `ccpp_loop_counter` standard name inside nested subcycles | Maps to OUTERMOST loop var.  None of cam-sima uses this; revisit if a scheme needs the innermost value. |
| Validator host-metadata check              | Deferred; revisit after e2e tests stabilise.  |
| Constituents overhaul (Class A/B + setters) | Discussion doc at `doc/constituents_overhaul.md`. |
| Framework setters: `set_advected`, `set_diagnostic_name`, `set_default_value` | Deferred; depends on constituents-overhaul decision. |
| Codegen-time scheme-registration cross-check | Deferred; would require new `registers_std_names` metadata attr. |
| `_FRAMEWORK_CONST_DIM_INPUTS` cleanup       | **Done 2026-05-13**: hand-curated frozenset gone; framework-constituent dim refs ride on a dedicated `used_const_dim_std_names` field on `ResolvedArg`. |
| Suppress `ccpp_host_constituents.F90` when unused | Deferred; currently emitted for every build even when no scheme/host actually exercises the constituent system.  Now *correct* (empty) for SCM-style hosts thanks to the host-wins rule, but still dead code.  See `design_constituent_host_wins.md`. |
| Python linter / formatter pass              | Deferred; pick `ruff` and apply across `capgen-ng/`. |
| Generated Fortran ↔ Codee formatter idempotency | Deferred; emitted `.F90` must round-trip cleanly through the project's Codee Fortran formatter. |
| `fortran_to_metadata` developer utility    | Deferred; bootstraps a `.meta` skeleton from an existing `.F90` subroutine. |
| `--legacy-mode` shim removal               | Transient; remove `metadata/legacy_compat.py`, `unit-tests/test_legacy_compat.py`, and every `# legacy-compat:` touchpoint when scheme metadata has migrated. |
| Original capgen auto-clone path             | Intentionally dropped in favour of explicit registration; kept in memory as "Option B" fallback. |

---

## Cross-references

- `doc/redesign_prompt.md` — original design specification (sections
  marked "historic" where the implementation has evolved).
- `doc/redesign_analysis.md` — analysis of the legacy ccpp-prebuild +
  ccpp-capgen toolchains.
- `doc/constituents.md` — full constituents reference for capgen-ng.
- `doc/constituents_overhaul.md` — architecture review and reform
  proposals for the next iteration.

