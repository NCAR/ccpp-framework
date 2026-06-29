# Migrating from ccpp-prebuild / ccpp-capgen to capgen

This document captures the **user-facing differences** a host model author
or scheme author needs to know when moving metadata, suite XML, and host
Fortran from the legacy ccpp-prebuild + ccpp-capgen toolchain to
**capgen**.  It complements `doc/redesign_prompt.md` (design spec) and
`doc/redesign_analysis.md` (analysis of the old systems).

*Last revised: 2026-06-05.*  Current unit-test suite: 1516 passing.

**Repository layout** (post-2026-05-13 cleanup): tooling lives under
`capgen/` (top-level of this repo).  Unit tests live at the top
level in `unit-tests/`; end-to-end tests in `end-to-end-tests/`.  Run
the unit suite from the repo root with `python -m pytest unit-tests/`.

## Table of contents

1. [Metadata format changes](#1-metadata-format-changes)
   1. [1.8 `horizontal_loop_extent` → `horizontal_dimension`](#18-deprecated-standard-names-rewritten-by---legacy-mode)
   2. [1.10 GFS-physics vertical-dim aliases (`--gfs-dim-aliases`)](#110-gfs-physics-vertical-dim-aliases---gfs-dim-aliases)
2. [Suite definition file (SDF) changes](#2-suite-definition-file-sdf-changes)
3. [Host Fortran requirements](#3-host-fortran-requirements)
4. [Generator CLI and build integration](#4-generator-cli-and-build-integration)
5. [Generated cap layout — what's new and what changed](#5-generated-cap-layout--whats-new-and-what-changed)
6. [Framework changes (constituents)](#6-framework-changes-constituents)
   1. [6.3 Host metadata wins over auto-provisioning](#63-host-metadata-wins-over-auto-provisioning-2026-05-12)
   2. [6.4 Legacy auto-clone registration (`--legacy-auto-clone-constituents`)](#64-legacy-auto-clone-registration---legacy-auto-clone-constituents)
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

> **Standalone DDT files require `module_name`.**  A `type = ddt` table
> in a `.meta` file with **no co-located** `scheme`/`host`/`control`
> table (a "wrapper object" like `ccpp_optical_props.meta` defining
> `ty_optical_props_1scl_ccpp`) cannot inherit its module from a sibling.
> If the defining Fortran module name differs from the DDT table (type)
> name — which it almost always does for these wrappers — you **must**
> declare `module_name` explicitly.  capgen does *not* guess (e.g.
> from the file name); a DDT it can't resolve raises a clear error at
> generation time naming the type and the `module_name` remedy.

### 1.3 New per-variable attributes

Inside a `[ var_name ]` section.  All optional.

| Attribute        | Type | Default | Notes |
|------------------|------|---------|-------|
| `top_at_one`     | bool | `False` | When host and scheme disagree, generator emits a vertical-flip transform with reverse-stride subscript on the host side.  Meaningless on variables without a vertical dimension. |
| `constituent`    | bool | `False` | Scheme metadata only.  Marks the var as a constituent reference. |
| `advected`       | bool | `False` | Scheme metadata only. |
| `molar_mass`     | float | `0.0`  | Scheme metadata only. |
| `diagnostic_name` | str | (defaults to `local_name`) | Host-tooling hint; mutually exclusive with `diagnostic_name_fixed`. |
| `allocatable`    | bool | `False` | Must match the Fortran `allocatable` attribute on the dummy. Required for any array the *scheme* allocates (see §1.3.3). |

#### 1.3.1 Host `active` + scheme arg shape

When a host variable carries `active = (<condition>)`, the host's
contract with the cap is "this variable's storage is only valid when
the condition holds".  capgen honors that contract differently
depending on the matching scheme arg's optionality:

**Scheme arg is `optional = True`** — the cap uses pointer association
so the scheme observes `PRESENT()` according to the active condition:

```fortran
if (<active_local>) then
   ptr%ptr => <host_var>(<subscript>)
else
   nullify(ptr%ptr)
end if
call scheme(..., my_arg=ptr%ptr, ...)
```

**Scheme arg is non-optional** — the scheme is asserting the variable
is mandatory.  The cap emits a runtime guard before the call so an
inactive-but-required variable surfaces as a clean error rather than a
silent read of unallocated memory:

```fortran
if (.not. (<active_local>)) then
   errmsg = "scheme 'X' phase 'Y' requires variable '<std>' but " &
          // "host active condition (<expr>) is false"
   errflg = 1
   return
end if
call scheme(..., my_arg=<host_var>(<subscript>), ...)
```

The guard runs before any unit/kind/vertical-flip transform pre-call
code, so transforms never see invalid host memory.  Multiple required
arguments with `active` conditions each get their own guard block — one
per arg keeps the error messages targeted.

It is the suite designer's responsibility to schedule the call so the
host's active condition holds when a required-arg scheme runs.  The
guard converts violations from latent runtime bugs into immediate
errflg/errmsg returns.

> **Earlier (relaxed 2026-05-20)**: a previous iteration of this rule
> rejected `active` + non-optional pairings at resolution time and
> required scheme metadata to declare `optional = True`.  That forced
> scheme metadata to misrepresent the Fortran for schemes that
> legitimately require a host-conditional variable.  The current rule
> defers the check to runtime and leaves the metadata honest.

#### 1.3.2 Host/scheme metadata cross-checks

The resolver enforces three cross-metadata checks per scheme arg
against its defining source (host metadata or, for suite-owned
variables, the first scheme to write the var with `intent=out`):

| Aspect | Rule | Notes |
|---|---|---|
| **Type identity** | Strict string match after `strip().lower()`. | No coercion across `real` / `integer` / `logical` / DDT.  DDT names match identically; `external:m:t` matches `external:m:t`. |
| **Rank** | `len(host.dimensions) == len(scheme.dimensions)`. | A scheme that asks for `(horizontal_dimension)` while the host declares `()` is rejected. |
| **Per-position dimension identity** | Each entry is canonicalized to `lower:upper`; strict match per position. | See "default lower bound" below. |

**Default lower bound — three equivalent spellings:**

- bare `foo` (no explicit lower bound)
- `1:foo` (integer literal one)
- `ccpp_constant_one:foo` (the standard name)

All three collapse to a single canonical representative, so the host
declaring `(vertical_layer_dimension)` matches the scheme declaring
`(1:vertical_layer_dimension)` and vice versa.

**Every other lower bound is distinct.**  `2:nlev` is not the same
axis as `1:nlev`; `start_idx:nlev` is not the same as
`ccpp_constant_one:nlev`.  Different lower bound describes a
different sub-range and must be spelled identically on both sides.

**No upper-bound name aliasing.**  `horizontal_dimension` and
`horizontal_loop_extent` are different names at the resolver layer.
The `--legacy-mode` shim (see §3) rewrites legacy names at parse
time when enabled; without that shim the legacy spellings should not
appear in metadata at all.

**Numeric kind is *not* checked here.**  Host `kind_phys` vs scheme
`real32` silently triggers the transform-copy pipeline (§5.3).  This
is deliberate: real CCPP-physics schemes legitimately mix precisions
and rely on the cap to handle the copy.  Watch for unintended
narrowing — there is no static guard.  **Character `len=`** has its
own block: matching `len=N` values pass, mismatched specific lengths
are an error unless the *consuming* scheme uses `len=*` (wildcard).
`len=*` is only valid where a variable is *passed*, never where its
storage is *defined*: host and DDT metadata must give every character
variable a concrete length, and so must the first `intent=out` scheme
that defines a suite-owned character variable (see below).  Both are
rejected with a clear error rather than emitting an undeclarable
`character(len=*)` component.  Control variables are exempt — they are
pass-through dummy arguments (`suite_name`, `errmsg`, …) the caps
legitimately declare `character(len=*)`.

**Suite-owned variables.**  The first scheme to write a standard
name with `intent=out` (in phase→scheme order) freezes the var's
type/kind/dimensions/units on the SuiteVar; every later scheme that
consumes it goes through the same checks against the frozen fields.
Because that first writer *defines* the storage the framework
allocates in `ccpp_<suite>_data`, a character definer must declare a
concrete length (`kind = len=N`); `len=*` there is an error.  Later
consumers/writers of the same variable may use `len=*` as a wildcard.
Error messages name the source as `host`, `control`, or `suite` so
you know whose contract you're violating.

**Suite-owned storage is never default-initialized** — by design.
capgen emits the `ccpp_<suite>_data` components with no default value.
An `intent(out)` argument is the scheme's contract to define that variable
on *every* return path; the framework will not paper over an unset output
the way original capgen's zero-initialized interstitials did.  A ported
scheme that returns early (e.g. a `fixed_scon` branch) without assigning
one of its `intent(out)` dummies leaves the suite-owned storage undefined,
and a later consumer reads garbage (in a debug build, often a trap value).
This is a common porting hazard original capgen used to mask — audit
early-return paths for unset `intent(out)` args.

#### 1.3.3 `allocatable` and who owns suite-data allocation

A **suite-owned variable** (an interstitial: first written by a scheme
with `intent=out`, then consumed by another) is stored as a component
of the generated `ccpp_<suite>_data` DDT.  capgen allocates it for
you — **once**, in `suite_data_init_fields`, which runs at the very
start of `<suite>_init`.  That works only when every dimension is known
that early, i.e. a **host variable** or a value set in the **`register`**
phase.

When the size is *not* known at init — the array is dimensioned by a
quantity a scheme computes later (in `init` / `timestep_init` / `run`,
e.g. a per-timestep daylight-column count) — the suite cannot size it.
Such a variable must be declared **`allocatable`** and allocated by its
**producing scheme**:

- metadata: `allocatable = True` on the arg;
- Fortran: `allocatable, intent(out)` on the dummy, and an explicit
  `allocate(...)` in the scheme body (an `intent(out)` allocatable is
  auto-deallocated on entry, so element assignment needs it allocated
  first).

For an `allocatable` arg capgen then: (1) does **not** pre-allocate it
in `init_fields`; (2) passes the **whole** component at call sites
(`...%var`, no array section — an allocatable/assumed-shape mismatch is
otherwise a compile error); and (3) still frees it in
`suite_data_final_fields` under an `if (allocated(...))` guard.  So the
**scheme allocates, the suite (or the scheme) deallocates** — the guard
makes either order safe (no leak, no double-free).

Size it with the **authoritative dimension variable** — the standard
name in the arg's `dimensions` — not a look-alike local or a derived
expression.  If that dimension is a scheme-set quantity, pass it in as a
scalar `intent=in` arg and `allocate` with it.  (Real example: an array
declared `number_of_vertical_interfaces_in_RRTMGP` must be sized with
that value, **not** the host's `vertical_interface_dimension` nor
`nlay+1`, which differ when the scheme runs on a reduced vertical grid.)

**Generation-time guard.**  capgen rejects a *non*-`allocatable`
suite-owned array whose dimension is written by a scheme in any phase
after `register` — it would otherwise be allocated from uninitialized
memory.  The error names the variable, the offending dimension, and the
fix (declare it `allocatable`).  The check is sound but partial: it sees
only dimensions a *scheme* writes, not a host scalar the host driver
re-computes each step — those remain the author's responsibility, and
the rule "anything the scheme allocates must be `allocatable` in Fortran
and metadata" is ultimately enforced by the compiler plus `ccpp_validator`.

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
combo) are normalized internally and treated as equivalent.  Pre-existing
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

### 1.7 Paired-optional control pairs (instances and threads)

There are two symmetric `(index, count)` control pairs:
`instance_number` / `number_of_instances` and `thread_number` /
`number_of_threads`.  Both behave identically:

- Declare **both** members in `type=control` → opt into that paired
  (multi-instance / multi-threading) API.  Both flow as control dummies
  through every lifecycle and physics-phase signature.
- Declare **neither** → the single API.  Public entry points drop both
  args; where the index would appear the framework uses literal `1`
  (and, for instances, internal per-instance arrays size to length 1).
- Declare exactly one of a pair → hard error from the validator.
- Declare either count in `type=host` → hard error (must be
  `type=control`).
- Dimension a host variable by `number_of_instances` /
  `number_of_threads` without declaring its pair → hard error (the
  scalar-index collapse needs the index variable in scope; see §3.4).

Hosts that need neither multi-instance nor multi-threading can drop
both pairs entirely.

> Why symmetric: `instance_number` indexes framework-owned per-instance
> state, so `number_of_instances` is read at register/init to size it.
> `thread_number` indexes host-owned per-thread containers; the
> framework doesn't yet read `number_of_threads`, but it's carried as a
> control dummy so the framework can size per-thread state in future —
> exactly as it does for instances today.

### 1.8 Deprecated standard names rewritten by `--legacy-mode`

`--legacy-mode` is a transient migration shim that rewrites a small
set of deprecated standard names to their canonical capgen
equivalents at parse time.  The full table currently covers:

| Deprecated (legacy)            | Canonical (capgen)    |
|--------------------------------|--------------------------|
| `horizontal_loop_extent`       | `horizontal_dimension`   |
| `number_of_openmp_threads`     | `number_of_threads`      |

Why each entry:

* `horizontal_loop_extent` — ccpp-prebuild / original ccpp-capgen used
  this for the horizontal-axis std name in scheme metadata.  capgen
  uses `horizontal_dimension` uniformly; the run-vs-non-run distinction
  isn't expressed in scheme metadata anymore (host passes
  `horizontal_loop_begin` / `horizontal_loop_end` as control vars and
  the generated cap slices accordingly).
* `number_of_openmp_threads` — legacy CCPP-physics hosts (CCPP-SCM
  17p8 in particular) size per-thread DDT containers by
  `number_of_openmp_threads` (e.g. `physics%Interstitial`).  capgen
  uses `number_of_threads`, which matches the `thread_number` control
  variable, so the registered scalar-index dim table can substitute
  `physics%Interstitial(thread_number)%…` automatically (see §3.4).

The rewrite fires for both standard-name attributes AND dimension
tokens (so a host's `dimensions = (number_of_openmp_threads)` becomes
`dimensions = (number_of_threads)` before any further processing).

Migration paths:

1. **Edit the metadata** (recommended) — search-and-replace the
   legacy names in every host / scheme `.meta` you maintain.
2. **Use `--legacy-mode`** (transient) — pass `--legacy-mode` to both
   `ccpp_capgen.py` and `ccpp_validator.py` and the renames happen
   at parse time.  A loud warning banner prints at startup, listing
   every pair the shim is rewriting, so the substitution is never
   invisible.  This shim *will be removed*; treat it as a runway,
   not a destination.

### 1.9 Inline comments

`#` starts a comment **anywhere on a line**, not just at column 0.
Everything from the `#` to end-of-line is discarded before the
parser sees the rest of the line.  Trailing whitespace left behind
by the strip is also removed, so section headers and key=value
lines parse cleanly.

```
[ ap_indices ]   # legacy index slot
  standard_name = ap_indices
  units = count
  dimensions = ()  # was (nap_indices) before the refactor
  type = integer
```

No escape mechanism is provided — `#` is not a legitimate character
in any metadata value (units, kinds, identifiers, dim lists,
Fortran conditional expressions).  `;` is still accepted as a
full-line comment marker (at column 0 after whitespace), matching
the historic blank-line convention, but is *not* treated as an
inline comment marker (`;` can plausibly appear inside a
`long_name`).

### 1.10 GFS-physics vertical-dim aliases (`--gfs-dim-aliases`)

GFS-physics scheme metadata uses two spellings for what is physically
the vertical-layer axis:

- `adjusted_vertical_layer_dimension_for_radiation` (radiation schemes)
- `vertical_composition_dimension` (chemistry schemes)

Both are the **same axis** as `vertical_layer_dimension` from the
host's point of view, but legacy hosts (CCPP-SCM 17p8, GFS) carry the
three names as **distinct host variables** (each addressable as its
own scalar dim std name) — so a parse-time substitution like
`--legacy-mode` would erase the variable behind the renamed token and
break host metadata.

`--gfs-dim-aliases` collapses the three names **only inside the
resolver's per-position dimension-identity check** (upper bound only;
lower bounds never alias).  The variables themselves stay distinct
everywhere else — `[ adjusted_vertical_layer_dimension_for_radiation ]`
remains its own host `type=control` entry; the access path in
generated code is unchanged; only the resolver's
"these dims describe the same axis" comparison treats the three names
as equivalent.

Single touchpoint in the generator
(`generator/suite_resolver.py::_canonical_dim`); the validator does
not carry the flag (it never reaches the resolver's canonicaliser).
Self-contained module `metadata/dim_aliases.py`; every touchpoint
tagged `# dim-aliases:` for clean removal.

Like `--legacy-mode`, this is a transient migration shim with a loud
startup banner — drop the GFS spellings from your scheme metadata in
favour of `vertical_layer_dimension` and the flag becomes unnecessary.

---

## 2. Suite definition file (SDF) changes

### 2.1 Schema v2.0 with nested-suite expansion

Capgen parses v2.0 SDFs and expands `<nested_suite>` references
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

### 2.3.1 Passing the loop counter / extent to a scheme

A scheme inside a `<subcycle>` block may consume the current iteration
counter and the total iteration count via two CCPP standard names:

| Standard name        | Fortran type | Meaning                                                  |
|----------------------|--------------|----------------------------------------------------------|
| `ccpp_loop_counter`  | integer      | Current subcycle iteration (1 … `ccpp_loop_extent`)      |
| `ccpp_loop_extent`   | integer      | Total iterations — the `loop=` value on the `<subcycle>` |

These are **loop-context control variables**: the host model does **not**
declare them.  capgen emits them automatically as locals in the
generated group cap (the `do` loop's induction variable for the counter,
the loop bound for the extent), and resolves any scheme arg requesting
them against those locals.

Example scheme metadata fragment:

```
[iter]
  standard_name = ccpp_loop_counter
  units = index
  dimensions = ()
  type = integer
  intent = in
[niter]
  standard_name = ccpp_loop_extent
  units = index
  dimensions = ()
  type = integer
  intent = in
```

Place the scheme in a `<subcycle>` in the SDF:

```xml
<subcycle loop="2">
  <scheme>sfc_diff</scheme>
  <scheme>GFS_surface_loop_control_part1</scheme>   <!-- uses iter / niter -->
  <scheme>sfc_nst</scheme>
</subcycle>
```

The generated group cap will emit `do ccpp_loop_counter = 1, 2` and call
the scheme with `iter = ccpp_loop_counter, niter = 2` (or the loop's
resolved local name when `loop=<std_name>` is used).

**Scope is the subcycle body.**  A scheme that requests
`ccpp_loop_counter` / `ccpp_loop_extent` but is NOT inside a
`<subcycle>` block raises a clear parse-time error pointing at this
contract.

**Nested-subcycle nuance** (see §8): nested-subcycle schemes that ask
for `ccpp_loop_counter` currently get the **outermost** loop's counter,
not the innermost.  None of the in-tree physics catalogs use the
inner-counter case yet; revisit when one needs it.

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
| `number_of_physics_threads`       | integer      | Physics-internal budget           |
| `ccpp_error_code`                 | integer      | Error flag                        |
| `ccpp_error_message`              | character    | Error message                     |

Paired-optional — two symmetric `(index, count)` pairs (see §1.7).
For each pair, declare **both** members in `type=control` or
**neither**; declaring exactly one is a hard error:

| Standard name           | Fortran type | Table type | Purpose                        |
|-------------------------|--------------|------------|--------------------------------|
| `instance_number`       | integer      | control    | Current instance index         |
| `number_of_instances`   | integer      | control    | Total instance count           |
| `thread_number`         | integer      | control    | Current thread / per-thread-container index |
| `number_of_threads`     | integer      | control    | Total thread count             |

**The `type=control` table is a closed set.**  It may contain *only*
the variables in the two tables above (the 7 required plus the 4
paired-optional pair members — 11 standard names total).  Any other
variable in a `type=control` table is a hard error: a host quantity
that schemes consume belongs in a `type=host` table, and the subcycle
loop variables (`ccpp_loop_counter` / `ccpp_loop_extent`) are
generator-owned locals you never declare.

### 3.2 Required entry-point call sequence

```
ccpp_register(suite_name, errflg, errmsg, [instance_number, number_of_instances])
  └── per scheme that declares a register phase
ccpp_init(suite_name, errflg, errmsg, [instance_number, number_of_instances])
  └── per scheme that declares an init phase
ccpp_physics_init(...)
  └── physics phase routines per group:
      ccpp_physics_init
      ccpp_physics_timestep_init
      ccpp_physics_run                    ← run-loop phase
      ccpp_physics_timestep_final
      ccpp_physics_final
ccpp_final(suite_name, errflg, errmsg, [instance_number, number_of_instances])
```

The `(instance_number, number_of_instances)` pair appears in every
signature only when the host declares it (§1.7).  Both flow uniformly
through lifecycle and physics-phase calls; the framework consumes
`number_of_instances` only at register/init time but carries it
elsewhere for API symmetry.

### 3.3 Module-name convention (host, scheme, and DDT tables)

capgen trusts metadata and does **not** parse Fortran, so it derives
the Fortran module name from the metadata: by default `module name =
table name`.  When the Fortran `module` statement does not match the
`[ccpp-table-properties] name`, declare the real module name with the
`module_name` override (§1.2).  This applies to **every** table type:

```
[ccpp-table-properties]
  name = test_host_data
  type = host
  module_name = mod_test_host_data
```

The same rule bites **scheme** tables.  If a scheme file's table is named
`gravity_wave_drag_common` but the Fortran is `module gw_common`, the
generated cap emits `use gravity_wave_drag_common` and the build fails
with `Cannot open module file 'gravity_wave_drag_common.mod'`.  Fix it in
the scheme `.meta`:

```
[ccpp-table-properties]
  name = gravity_wave_drag_common
  type = scheme
  module_name = gw_common
```

Standalone `type = ddt` tables **require** `module_name` explicitly
(there is no basename fallback).  Porting a host like CAM-SIMA's
`atmospheric_physics` tree, where many scheme/DDT table names differ from
their module names, is largely a batch of `module_name` injections.

### 3.4 Registered scalar-index dimensions

A small set of CCPP standard-name dimensions are *registered*: each
one is a count that capgen auto-collapses to a paired scalar index
variable at every access site.

| Count dim (in `dimensions = (...)`) | Index var (capgen substitutes) |
|---|---|
| `number_of_instances`               | `instance_number`                 |
| `number_of_threads`                 | `thread_number`                   |

**Where these may appear**: ONLY on container DDT-instance variables in
the access path.  Example:

```
[Interstitial]
  standard_name = GFS_interstitial_type_instance
  type          = GFS_interstitial_type
  dimensions    = (number_of_threads)
```

Every scheme that reaches into `Interstitial%<field>` will see the
generator emit `physics%Interstitial(thread_number)%<field>` at the
call site — no metadata work required on the scheme side.

**Two rules govern this:**

1. *(generalized)* A container DDT-instance variable may carry any
   registered scalar-index dim — single (`(number_of_threads)`) or
   paired (`(number_of_instances, number_of_threads)`).  Dims that
   AREN'T registered flow through the normal slice machinery
   (`horizontal_loop_begin:horizontal_loop_end`, `1:vertical_*`, …)
   just like flat-array dims.
2. *(enforced — hard parse-time error)* A **leaf** variable
   (intrinsic-typed or `external:` — the kind a scheme binds to)
   **MUST NOT** declare a registered scalar-index dim.  If you write::

       [my_array]
         type       = real | kind = kind_phys
         dimensions = (number_of_threads, horizontal_dimension)   # ILLEGAL

   capgen will reject it at parse time with a message pointing
   at the wrap-in-DDT remediation pattern.  Wrap the leaf in a
   container DDT instead.

The registered table lives in
[`capgen/metadata/registered_dimensions.py`](../capgen/metadata/registered_dimensions.py).
It carries a four-step recipe at the top of the file for adding new
pairings.

---

## 4. Generator CLI and build integration

### 4.1 `ccpp_capgen.py` invocation

```
python ccpp_capgen.py \
    --host-files <host.meta>[,<ddt.meta>,...] \
    --scheme-files <scheme1.meta>[,<scheme2.meta>,...] \
    --suites <suite1.xml>[,<suite2.xml>,...] \
    --host-name <host_identifier> \
    --output-root <build_dir>/ccpp \
    [--kind-type <name>=[<module>:]<spec>] \
    [--legacy-mode] \
    [--gfs-dim-aliases] \
    [--legacy-auto-clone-constituents] \
    [--no-host-introspection] \
    [--verbose] [--verbose]
```

`--kind-type` syntax: `<name>=[<module>:]<spec>`.  When `<module>:` is
omitted, `<spec>` must be an ISO_FORTRAN_ENV constant (REAL32/REAL64/...)
and the module defaults to `iso_fortran_env`.  `kind_phys` is
auto-defaulted to `iso_fortran_env:REAL64` when not supplied.

#### Transient migration shims

Three opt-in flags exist for migrating legacy hosts.  Each is
self-contained and grep-tagged for clean removal:

**`--legacy-mode`** (transient migration shim, will be removed):
silently rewrites a small set of deprecated CCPP standard names to
their capgen equivalents at parse time — see §1.8 for the full
table (`horizontal_loop_extent` → `horizontal_dimension`,
`number_of_openmp_threads` → `number_of_threads`).  The rewrite fires
for both standard-name attributes AND dimension tokens.  Prints a
loud warning banner at startup, enumerating every pair the shim is
rewriting, so the substitution is never invisible.  Available on both
`ccpp_capgen.py` and `ccpp_validator.py` (keep the flag consistent
between the two when both are invoked from CMake).  All translation
logic is isolated in `metadata/legacy_compat.py` and tagged with
`# legacy-compat:` comments at every touchpoint.

**`--gfs-dim-aliases`** (transient migration shim, see §1.10):
treats GFS-physics names
`adjusted_vertical_layer_dimension_for_radiation` and
`vertical_composition_dimension` as equivalent to
`vertical_layer_dimension` **inside the resolver's per-position
dim-identity check only** (upper bound only).  The host variables
themselves stay distinct.  Generator-only (the validator never
reaches the dim canonicaliser).  Module `metadata/dim_aliases.py`;
touchpoints tagged `# dim-aliases:`.

**`--legacy-auto-clone-constituents`** (transient migration shim, see
`doc/auto_clone_constituents.md` for the full reference):
reinstates original ccpp-capgen's auto-clone-static-constituent
registration path.  Every `is_constituent` consumer scheme arg
(`advected = True`, `constituent = True`, or `molar_mass = …`) with
no register-phase source is auto-registered into the per-suite
dynamic-constituents buffer using values lifted straight from the
scheme metadata (with sensible defaults: `long_name` synthesised from
the standard name when missing, `diag_name` falls back to local_name,
`vertical_dim` lifted from the arg's dim list).  Adds four legacy
`%instantiate` kwargs to the parser (`default_value`, `min_value`,
`water_species`, `mixing_ratio_type`).  Available on both
`ccpp_capgen.py` and `ccpp_validator.py` (the validator must
accept the four extra attrs).  **Single-instance only** — declaring
the `instance_number` + `number_of_instances` pair while the flag is
on is a hard error before any suite is parsed.  Module
`metadata/auto_clone_constituents.py`; touchpoints tagged
`# auto-clone-constituents:`.

### 4.2 `ccpp_datafile.py` query CLI

Generated `datatable.xml` carries:

- `<capgen_files>` — generated outputs (utilities/host_files/suite_files).
- `<inspection_files>` — `<suite>.meta` and expanded SDF.
- `<schemes>` — per-scheme call lists, **scoped to schemes that are
  actually referenced by the loaded suites** (group phase calls + the
  suite-level `<init>`/`<final>` hooks).  Scheme metadata files passed
  on the CLI but never referenced are silently dropped.
- `<dependencies>` — `dependencies = …` from host/control/ddt tables
  (always) plus the same per-scheme list as `<schemes>` (filtered to
  the used set).  Build systems that compile against
  `ccpp_datafile.py --dependencies` therefore only pull in scheme deps
  for compiled schemes; missing transitive deps in scheme metadata
  surface as link errors and should be fixed in the `.meta` file.
- `<var_dictionaries>` — host/api/suite/group dictionaries.

Query via `ccpp_datafile.py --<flag> <datatable.xml>`.  Flags include
`--dependencies`, `--capgen-files`, `--host-files`, `--utility-files`,
`--suite-files`, `--scheme-files`, `--suite-list`,
`--required-variables <suite>`, `--input-variables <suite>`,
`--output-variables <suite>`, `--host-variables`, `--show`.

`--suite-files` returns capgen-generated cap files (`ccpp_<suite>_cap.F90`,
etc.).  `--scheme-files` returns the **user-supplied scheme `.F90` sources**
that the loaded suites actually reference — the filtered compile manifest.
Each used scheme's source is resolved as `<source_path>/<meta_basename>.<ext>`
(extension preference order: `.F90`, `.f90`, `.F`, `.f`); missing files are
warned about and the canonical `.F90` guess is emitted so the build-system
query stays useful.

### 4.3 CMake helpers

`cmake/ccpp_capgen.cmake` and `cmake/ccpp_validator.cmake` provide the
`ccpp_capgen(...)` and `ccpp_validator(...)` macros.  `ccpp_datafile(...)`
queries datatable.xml at configure time.

### 4.4 No-op regeneration preserves mtimes

Every generated file (caps, `datatable.xml`, `ccpp_kinds.F90`, expanded
SDFs, `.meta` artifacts) goes through `write_if_changed`: the new content
is staged to a sibling temp file under the output root and atomically
replaces the target only when the bytes actually differ.  Reruns with
identical inputs therefore leave on-disk mtimes untouched, so CMake /
Make / Ninja do not trigger a downstream rebuild cascade.  Matches the
behavior of legacy `ccpp-prebuild` / `ccpp-capgen`.  The staging temp
file lives in the target's parent directory (always under
`--output-root`), so no `/tmp` access is required.

### 4.5 Driving an existing capgen-based build: the CAM-SIMA compatibility layer

A host whose build system was written against **original ccpp-capgen's
Python API** can adopt capgen without rewriting that build system, by
inserting a thin facade.  CAM-SIMA does exactly this with
`cime_config/capgen_compat/` (in the CAM-SIMA tree, not in capgen).
CAM-SIMA's `cam_autogen.py`, `generate_registry_data.py`, and
`write_init_files.py` are unmodified; they import the facade instead of
original capgen and keep calling the same object surface
(`cap_database.host_model_dict()`, `cap_database.call_list(phase)`,
`Var.get_prop_value(...)`, `Var.source.ptype`, …).

The facade re-implements that surface on top of capgen's outputs:

- `_runner.py` invokes `ccpp_capgen.py` and returns the resolver
  results plus the `datatable.xml`.
- `_cap_database.py` (`CapDatabase`) exposes `host_model_dict()` over the
  flat `host_dict` and `call_list(phase)` over the per-(scheme, phase)
  `ResolvedArg` lists, mapping original-capgen phase spellings
  (`initialize`/`finalize`) onto capgen's (`init`/`final`).
- `_var_wrapper.py` (`_VarWrapper`) reconstructs original capgen's
  per-variable accessors over a `HostVarEntry` (host path) or a
  `ResolvedArg` (call-list path).
- `metadata_table.py` / `parse_*` shim the metadata-parsing entry points
  the registry generator expects.

Two contracts matter when writing or maintaining such an adapter, both
learned from the CAM-SIMA bring-up:

1. **Drop suite-internal args.** A `ResolvedArg` with
   `source == 'suite'` is produced by one scheme and consumed by another
   within the same suite (it lives in `<suite>_data`, never in the host
   dict).  The adapter must NOT surface it on the call list, or the host's
   init-file generator will mis-flag it as a "missing required host
   variable".
2. **Key constituent handling on the source.** Treat a `ResolvedArg`
   with `source == 'constituent'` as supplied by the constituents object
   (skip host USE-import and skip the initial-conditions read).  Do **not**
   key on `ResolvedArg.is_constituent`: that flag reflects whether the
   *scheme* flagged the arg, and an unflagged rule-b consumer (§6.5) of a
   constituent or `tendency_of_<X>` carries `is_constituent = False` while
   still being framework-supplied.  Getting this wrong was the `se_cslam`
   "Missing required host variables: tendency_of_water_vapor_…" failure
   (`doc/constituents_overhaul.md` §4.15).

This facade is how capgen currently drives the `kessler`, `rrtmgp`,
and `se_cslam`/CSLAM (FCAM7 `cam7`) CAM-SIMA cases end-to-end on Derecho
— building and running to completion under both **gnu and intel**, with
bit-comparable results.  A short shareable brief (for the original
ccpp-capgen author) is `doc/capgen_compat_layer.md`; the full developer
reference is `cime_config/capgen_compat/README.md` in the CAM-SIMA tree.

---

## 5. Generated cap layout — what's new and what changed

### 5.1 Output files

Always generated:

- `ccpp_kinds.F90` — kind parameters.  Listed under `<utilities>`.
- `<host>_ccpp_cap.F90` — public host-facing entry points + introspection routines.
  Filename and emitted `module <host>_ccpp_cap` name are both driven by the
  required `--host-name <host>` CLI argument so multiple host integrations
  can co-exist in one executable.  The public sub names inside
  (`ccpp_register`, `ccpp_init`, `ccpp_physics_*`, `ccpp_final`) are
  unchanged regardless of `<host>`.
- `ccpp_<suite>_cap.F90` — per-suite dispatcher.
- `ccpp_<suite>_<group>_cap.F90` — per-group phase implementations.
- `ccpp_<suite>_data.F90` — suite-owned interstitial DDT + module-level array.
- `ccpp_<suite>_types.F90` — pointer-wrapper types for optional args.
- `ccpp_<suite>_data.meta` — inspection artifact; pairs with `ccpp_<suite>_data.F90` (`.meta` ↔ `.F90` filename convention).
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

Transforms only smooth over *representation* differences.  Anything
the cap cannot bridge with a per-call copy — type identity, rank,
or per-position dimension identity — is rejected by the resolver as
a hard error (see §1.3.2).  In particular, the kind-conversion entry
above is *not* gated on convertibility: any kind-string difference
triggers an implicit conversion copy.  Watch for unintended
narrowing.

These compose.  A scheme arg that needs unit + flip emits a single
combined assignment through a transformation temp:

```fortran
temp_l = 1.0E-3_kind_phys*host_var(lb:ub, nlev:1:-1)  ! unit conversion: kind_phys to kind_phys; vertical flip (top_at_one mismatch)
call scheme_run(temp=temp_l, ...)
host_var(lb:ub, nlev:1:-1) = 1.0E+3_kind_phys*temp_l  ! ... reverse ...
```

Identity unit conversions (registered for dimensionally-equivalent
spellings like `J kg-1 ↔ m2 s-2`, formula `'{var}'`) are not labeled
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

**Idempotent entry points.**  `ccpp_physics_init`, `ccpp_physics_final`, and
`ccpp_final` are all silently idempotent — repeat calls return cleanly with
`errflg=0` rather than erroring.  `ccpp_physics_final` additionally silent-skips
when issued *after* `ccpp_final` has torn the suite down (state array
deallocated on the last instance, or `== UNREGISTERED` on any other instance).
The other physics phases (`timestep_init`, `run`, `timestep_final`) still
hard-error on a state mismatch.  `ccpp_init` does *not* silent-skip when the
state array is unallocated — there, "not allocated" really does mean
"you forgot `ccpp_register`" and continues to be a hard error.

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
capgen's `--legacy-auto-clone-constituents` shim (§6.4)
synthesises `%instantiate(...)` directly on slots of the per-suite
dynamic-constituents buffer, so the properties objects are owned by
the buffer from creation — no ownership transfer call needed.

### 6.2 capgen constituent API

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
- Scheme-side registration rules — register-phase
  `ccpp_constituent_properties_t(:)` arg declares new constituents;
  flag a base species with `advected=true intent=in/inout`; produce a
  tendency with `constituent=true intent=out` + `tendency_of_<X>` std
  name; a constituent-flagged `intent=out` that is not a `tendency_of_*`
  is a codegen error.  A scheme that only READS a constituent or a
  `tendency_of_<X>` need not re-flag it — see §6.5.
- **`_register` is called exactly once per scheme** (2026-06-08).
  capgen packs each constituent scheme's returned
  `ccpp_constituent_properties_t(:)` array into the per-suite buffer in a
  single append pass, so a register routine may safely allocate persistent
  module state.  (An earlier two-pass count+copy called register twice and
  broke any non-idempotent register, e.g. `prescribed_aerosols_register`
  allocating a module-level map.)

### 6.3 Host metadata wins over auto-provisioning (2026-05-12)

If the host declares a framework-named standard name
(`ccpp_constituents` / `ccpp_constituent_tendencies` /
`ccpp_constituent_properties` / `number_of_ccpp_constituents` /
`index_of_<X>`) as a regular host variable, the resolver uses the
host's declaration and skips capgen auto-provisioning.  Matters
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

### 6.4 Legacy auto-clone registration (`--legacy-auto-clone-constituents`)

For hosts that ship metadata in original ccpp-capgen's shape — most
notably CAM-SIMA's atmospheric_physics tree, where ~16 of the ~20
constituent-touching schemes declare `advected = True` (or
`constituent = True`, or `molar_mass = …`) in `_run` arg tables and
rely on the framework to register the constituent — pass
`--legacy-auto-clone-constituents` to both `ccpp_capgen.py` and
`ccpp_validator.py`.

What changes:

- The parser accepts four extra scheme-arg attributes
  (`default_value`, `min_value`, `water_species`,
  `mixing_ratio_type`).  Fortran-style literal suffixes
  (`0.0_kind_phys`, `1.0d-5`, `-3.14_8`) are accepted on the real
  fields, since legacy metadata writes the values in source form.
- For every unique standard name that appears as an `is_constituent`
  consumer with no register-phase source, the suite cap emits a
  synthesised `%instantiate(...)` call into the per-suite
  dynamic-constituents buffer.  The scheme author writes no Fortran
  registration code.
- `long_name` is auto-synthesised from the standard name when missing
  (`cloud_liquid_dry_mixing_ratio` → `'Cloud liquid dry mixing
  ratio'`); `diag_name` falls back to local_name; `vertical_dim` is
  lifted from the arg's `dimensions = (...)` entry.
- Schemes that pass the whole constituents buffer (e.g.
  `apply_constituent_tendencies_run` with `ccpp_constituents` /
  `ccpp_constituent_tendencies` / `index_of_*` args) are excluded
  from auto-clone — those resolve through the framework
  whole-buffer path, not as individual registrations.

What capgen's other rules still require (the shim does **not**
relax them):

- `intent = inout` on base constituents (`advected = True` on a
  non-`tendency_of_*` std_name).  `intent = out` is reserved for
  tendency args.
- Metadata arg tables must match the Fortran subroutine signature.
  Declaring a constituent in `<scheme>_init`'s arg table when
  `<scheme>_init` doesn't take it as a Fortran dummy is rejected by
  the validator.

Single-instance only: declaring `instance_number` +
`number_of_instances` while the flag is on aborts before any suite is
parsed.  Legacy hosts predate multi-instance support, so this matches
the use case.

Full reference: `doc/auto_clone_constituents.md`.  E2e fixture:
`end-to-end-tests/advection_auto_clone/` (a port of CAM-SIMA's
`advection_test`).

### 6.5 Reading constituents and tendencies without re-flagging (rule b, 2026-06-05)

Whether a given standard name is a constituent or an ordinary variable
is the **host's** decision: CAM-SIMA exposes water vapor as a
constituent; CCPP-SCM may expose the same name as an ordinary host
variable.  A scheme that merely **reads** such a name therefore must
**not** repeat the `advected` / `constituent` flag — only the
declaring/producing scheme (or the host) does.

capgen infers constituent-ness for an unflagged consumer from the
scheme-metadata-wide set of names that *some* scheme flags
(`VariableResolver.constituent_stdnames()`):

- an unflagged `intent=in/inout` read of a flagged base name resolves to
  `…%vars_layer(:, …, index_of_<X>)`;
- an unflagged `intent=in` read of `tendency_of_<X>` resolves to
  `…%vars_layer_tend(:, …, index_of_<X>)` — the same column a constituent
  tendency *producer* wrote.

**Host / earlier-suite provision wins**: if the host declares the name,
or an earlier scheme already produced it as an ordinary variable, normal
host/suite resolution takes over and no constituent column is used.

This is what lets the CAM-SIMA `cam7` suite work unchanged: the
convection/stratiform schemes write `tendency_of_water_vapor_…` as a
flagged constituent tendency and the `sima_diagnostics` schemes read it
back unflagged.  Host adapters that post-process the resolver output
(e.g. CAM-SIMA's `write_init_files` via the compatibility layer, §4.5)
must key constituent handling on `ResolvedArg.source == 'constituent'`,
**not** on `ResolvedArg.is_constituent` — an inferred consumer carries
`is_constituent = False` by design.

### 6.6 `number_of_ccpp_constituents` as a dimension

A scheme (or a suite-owned interstitial) may be dimensioned by the
framework constituent count `number_of_ccpp_constituents`.  capgen
resolves that count for *any* variable: call-site subscripts emit `:`
for the constituent axis, and `<suite>_data` allocations size the axis
from the per-instance constituent object's `%num_layer_vars`.  This is
what allows whole-buffer schemes (e.g. constituent advection) to declare
`dimensions = (horizontal_dimension, vertical_layer_dimension,
number_of_ccpp_constituents)`.  E2e fixture:
`end-to-end-tests/constituents_dim/`.

---

## 7. Validator

`capgen/ccpp_validator.py` — standalone Fortran-vs-metadata checker.
Validates **scheme** metadata against scheme Fortran files, and (since
2026-06-01) **host** and **DDT** metadata against host module-level
declarations and derived-type definitions.

### 7.1 What the validator checks

For every `(scheme, phase)` declared in the supplied `.meta` files:

1. The Fortran subroutine `<scheme>_<phase>` **exists** in the source
   tree (auto-discovered via `source_path` on the table, or supplied
   explicitly with `--source-files`).
2. **Argument count** — the number of dummy arguments matches the
   metadata, after subtracting any optional-only-in-Fortran args (see
   §7.2).
3. **Argument names** — the set of metadata `local_name` values
   matches the Fortran dummy-arg list (order-insensitive,
   case-insensitive).
4. For every argument present on **both sides**, per-attribute
   consistency:

   | Attribute  | Behavior |
   |------------|----------|
   | `intent`   | Strict match (`in` / `out` / `inout`).  Metadata declares it but Fortran omits → error. |
   | `type`     | Case-insensitive match.  `double precision` / `doubleprecision` / `double  precision` are normalized to the same form.  DDT names match the Fortran `type(name)` / `class(name)` wrapper — metadata `type = ty_rad_lw` matches Fortran `type(ty_rad_lw)`.  External types match by typename — metadata `type = external:mpi_f08:mpi_comm` matches Fortran `type(mpi_comm)` (the module qualifier is metadata-only). |
   | `kind`     | Case-insensitive match.  **Character length must be CONSISTENT** — the metadata mirrors the Fortran exactly: `len=*` matches only `len=*`, and `len=N` only the identical `len=N` (no wildcarding; changed 2026-06-08 — the validator runs first, so a Fortran `len=*` can only pair a metadata `len=*`, and vice versa).  Old-style F77 forms (`character*64`, `character*(*)`, per-entity `c*5` / `d(10)*8`) are normalized to the `len=` form before comparison. |
   | `rank`     | Number of dimensions only.  Reads both `dimension(...)` line attributes and var-attached `foo(:,:)` syntax.  Per-dimension bound comparison is NOT done. |

### 7.2 Asymmetric `optional` rule

| Metadata        | Fortran                  | Outcome  |
|-----------------|--------------------------|----------|
| (absent)        | `optional`               | warning  |
| (absent)        | required (no `optional`) | error    |
| `optional=False`| `optional`               | warning  |
| `optional=False`| required (no `optional`) | OK       |
| `optional=True` | `optional`               | OK       |
| `optional=True` | required (no `optional`) | **error**|

Reason for the asymmetry: a metadata-side `optional=True` is a promise
to the cap that the value may be absent at the call site.  If Fortran
requires the dummy, the cap's `present()` check is invalid.  The
reverse direction (Fortran allows optional, metadata always passes it)
is a valid subset of the Fortran contract — the arg is always present
and any optional Fortran dummy can accept that — so we warn but don't
fail the build.

### 7.3 Continuation-line handling

Covers both free-form (`&` at trailing end of prior line only) and
fixed-form / dual-form (`&` at both ends, with the leading marker at
column 6).  Comment-only and blank lines interleaved between
continuation lines are skipped as Fortran 90+ permits.  When the
signature parser finds a subroutine but extracts zero args while
metadata declares many, the "Argument count mismatch" error appends a
HINT pointing at the parser rather than masquerading as a real
mismatch — common cause is an unsupported signature feature.

### 7.4 Host and DDT metadata validation (2026-06-01)

Pass `--host-files` to validate `type = host` and `type = ddt` tables
against module-level declarations and derived-type definitions in the
same `--source-files` Fortran tree:

```
ccpp_validator.py \
    --scheme-files scheme1.meta,scheme2.meta \
    --host-files   host.meta,physics_types.meta \
    --source-files scheme1.F90,scheme2.F90,host.F90,physics_types.F90
```

Per-table behaviour:

| Table type | Check |
|---|---|
| `type = host` | For each variable, find a module-level declaration of the same `local_name` (lowercased; subscripted spellings like `tk(:,:)` strip to `tk`) in the Fortran module named by `module_name` (or `table_name` when not overridden).  Compare type / kind / rank using the same rules as the scheme-side check (intent is silently ignored — host vars carry none).  Missing module or missing variable → clear error. |
| `type = ddt` | For each component, find a matching member of the Fortran derived type whose name equals `table_name` (the DDT name).  The type definition may live in any parsed module (a flat cross-file index is built from `--source-files`).  Same type / kind / rank rules as host vars. |
| `type = control` | Silent skip with an INFO log line — control vars are framework-injected at the cap call sites, no host Fortran backs them. |
| `type = scheme` in `--host-files` | Hard error — schemes must be passed via `--scheme-files` so the validator can find the per-phase subroutines. |
| `type = host` / `control` / `suite` in `--scheme-files` | Hard error — symmetric to the rule above.  Misclassified `.meta` files fail fast with a pointer at the correct flag.  **`type = ddt` is allowed in `--scheme-files`**: schemes routinely co-locate their own derived-type definitions (e.g. radiation schemes carrying `ty_rad_lw` / `ty_rad_sw` in the same `.meta` as the scheme phase blocks).  Scheme-co-located DDTs go through the same per-component validation as host-side DDTs. |

**Inputs contract.**  At least one of `--scheme-files` or `--host-files`
must be supplied.  Passing neither raises a clear error rather than
the older silent "Validation passed."  Either flag alone is fine;
both together is the common case.

The same per-attribute rules apply that the scheme-side check uses,
which is one rule fewer than the scheme side: there is no `optional`
flag on module vars / DDT components, and host metadata carries no
`intent`, so the asymmetric-optional rule (§7.2) does not apply here.
Character length is matched exactly (§7.1).  Additionally, because
host / DDT metadata *defines* its character storage (a module variable
or a derived-type component, neither of which may be assumed-length),
`len=*` is rejected there outright — host and DDT character variables
must declare a concrete `len=N`.  Assumed length is valid only for
dummy arguments (scheme args and control/lifecycle variables).

---

## 8. Known gaps and deferred items

| Item                                       | Status                                        |
|--------------------------------------------|-----------------------------------------------|
| `ccpp_loop_counter` standard name inside nested subcycles | Maps to OUTERMOST loop var.  None of cam-sima uses this; revisit if a scheme needs the innermost value. |
| Validator host-metadata check              | **Landed 2026-06-01**: pass `--host-files`; see §7.4. |
| Constituents overhaul (Class A/B + setters) | Discussion doc at `doc/constituents_overhaul.md`. |
| Framework setters: `set_advected`, `set_diagnostic_name`, `set_default_value` | Deferred; depends on constituents-overhaul decision. |
| Codegen-time scheme-registration cross-check | Deferred; would require new `registers_std_names` metadata attr. |
| `_FRAMEWORK_CONST_DIM_INPUTS` cleanup       | **Done 2026-05-13**: hand-curated frozenset gone; framework-constituent dim refs ride on a dedicated `used_const_dim_std_names` field on `ResolvedArg`. |
| Suppress `ccpp_host_constituents.F90` when unused | Deferred; currently emitted for every build even when no scheme/host actually exercises the constituent system.  Now *correct* (empty) for SCM-style hosts thanks to the host-wins rule, but still dead code.  See `design_constituent_host_wins.md`. |
| Python linter / formatter pass              | Deferred; pick `ruff` and apply across `capgen/`. |
| Generated Fortran ↔ Codee formatter idempotency | Deferred; emitted `.F90` must round-trip cleanly through the project's Codee Fortran formatter. |
| `fortran_to_metadata` developer utility    | Deferred; bootstraps a `.meta` skeleton from an existing `.F90` subroutine. |
| `--legacy-mode` shim removal               | Transient; remove `metadata/legacy_compat.py`, `unit-tests/test_legacy_compat.py`, and every `# legacy-compat:` touchpoint when scheme metadata has migrated. |
| `--gfs-dim-aliases` shim removal           | Transient; remove `metadata/dim_aliases.py`, `unit-tests/test_dim_aliases.py`, and every `# dim-aliases:` touchpoint when GFS metadata stops spelling `vertical_layer_dimension` as `adjusted_vertical_layer_dimension_for_radiation` / `vertical_composition_dimension`. |
| `--legacy-auto-clone-constituents` shim removal | Transient; remove `metadata/auto_clone_constituents.py`, `unit-tests/test_auto_clone_constituents.py`, sample files under `unit-tests/sample_files/scheme_auto_clone_consumer.meta` + `sample_suite_files/suite_auto_clone.xml`, and every `# auto-clone-constituents:` touchpoint when consumers have moved to explicit `host_constituents(:)` declaration or register-phase scheme registration. |
| `ccpp_datafile.py` query CLI rework        | Deferred (2026-05-13); collapse `--host-files` / `--suite-files` / `--utility-files` into `--capgen-files`, then repurpose `--host-files` as a filtered list of **input** host metadata files (parallel to `--scheme-files`).  Most hosts pack all host data into a handful of shared files, so the filtering pay-off is small — the draw is API symmetry. |

---

## Cross-references

- `doc/redesign_prompt.md` — original design specification (sections
  marked "historic" where the implementation has evolved).
- `doc/redesign_analysis.md` — analysis of the legacy ccpp-prebuild +
  ccpp-capgen toolchains.
- `doc/constituents.md` — full constituents reference for capgen.
- `doc/constituents_overhaul.md` — architecture review and reform
  proposals for the next iteration.
- `doc/capgen_compat_layer.md` — short brief on the CAM-SIMA ↔ capgen
  compatibility layer (§4.5); full reference is
  `cime_config/capgen_compat/README.md` in the CAM-SIMA tree.

