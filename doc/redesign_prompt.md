# CCPP Framework Code Generator — Redesign Specification

*Last revised: 2026-05-13 (late evening — SCM-driven session).*

## Purpose

This document is a complete implementation specification for a new CCPP Framework code
generator (`ccpp-capgen`). It supersedes both `ccpp-prebuild` and `ccpp-capgen`. An
implementer should be able to build the new generator from scratch using this document
alone, supplemented by the real-world examples in `redesign_analysis.md`.

The spec is essentially as-implemented as of the date above.  User-facing
deltas relative to ccpp-prebuild and the original ccpp-capgen are
collected in `doc/migration.md`; section 18 of this document is a rolling
"outstanding work" tracker.

---

## 1. Background and Motivation

The CCPP Framework couples host NWP models (UFS Weather Model, NEPTUNE, CCPP-SCM,
CAM-SIMA) to physics parameterization schemes by auto-generating Fortran interface
("cap") code. Two generators exist today:

- **`ccpp-prebuild`** — simple, procedural Python; fast; DDT arguments; used in
  production by UFS, NEPTUNE, SCM. Does not support framework-owned variables.
- **`ccpp-capgen`** — complex OO Python; flat-field arguments; used in CAM-SIMA. The
  deep class hierarchy (`VarDictionary`, `VarCompatObj`, `CCPPDatabaseObj`, etc.) makes
  it unmaintainable. Three developers spent considerable time trying to add DDT argument
  passing and could not succeed.

The redesign starts fresh, drawing lessons from both. The guiding principle is:
**simplicity of prebuild, feature set of capgen**.

The primary failures that triggered the redesign:
1. capgen passes flat fields to group caps — infeasible at UFS/NEPTUNE scale (1200+
   variables), breaks under compiler debug flags for optional variables.
2. capgen's scope-chain variable promotion is the source of most complexity.
3. Nobody on the team fully understands capgen.

---

## 2. Toolchain Structure

The redesign produces **two separate tools** that share the same metadata parsing library:

### 2.1 Validator (`ccpp_validator.py`)

Parses both Fortran source files and metadata files, compares them, and reports
discrepancies. Run by developers before invoking the generator — e.g., during scheme
development or in CI. Does **not** generate any Fortran output.

For each scheme phase declared in a `.meta` file, the validator checks that the
corresponding Fortran subroutine: (1) exists in the source tree, (2) has the same
number of dummy arguments, (3) the argument names match the `local_name` values in
the metadata (order-insensitive), and (4) for every argument present in both sides,
the `intent`, `type`, `kind`, and dimension *rank* agree.  `character` arguments
treat `len=*` on either side as a wildcard.  DDT names compare against the Fortran
`type(name)` / `class(name)` wrapper; `external:<module>:<typename>` metadata
compares against the Fortran `type(typename)` (the module qualifier is
metadata-only).

The `optional` attribute is asymmetric: metadata `optional=True` paired with a
Fortran dummy that is *not* declared `optional` is a hard error (the cap's
`present()` check would be invalid on a Fortran-required dummy); the reverse
direction (Fortran-only `optional`) is a warning, since always passing the arg is
a valid subset of the Fortran contract.

Fortran source files can be supplied explicitly on the CLI (`--source-files`). When
omitted, the validator auto-discovers the Fortran source for each scheme table using the
`source_path` table-level property (Section 3.5): it looks for a `.F90` file with the
same base name as the `.meta` file, in the directory given by `source_path`.

### 2.2 Code Generator (`ccpp_capgen.py`)

Parses metadata only. Assumes metadata correctly describes the Fortran source — performs
no Fortran parsing. Generates all cap files and supporting modules.

**Both tools import the same metadata parsing module.** No duplication of metadata
parsing logic between the two tools.

---

## 3. Metadata Format

### 3.1 File format

The existing ini-file format is preserved unchanged. Every metadata file consists of
`[ccpp-table-properties]` header blocks followed by `[ccpp-arg-table]` variable listing
blocks, exactly as in the current framework.

The `[ccpp-table-properties]` + `[ccpp-arg-table]` pair is redundant for non-scheme
tables (the distinction is vestigial for host/DDT/suite tables) but is preserved for
symmetry with scheme metadata tables.

### 3.2 Table types (`type =` in `[ccpp-table-properties]`)

Five table types are supported:

| `type =` | Ownership | Import mechanism |
|---|---|---|
| `scheme` | Physics scheme | Intent args on scheme subroutines |
| `host` | Host model | Module USE (direct or via DDT member) |
| `control` | Framework runtime layer | Explicit args to `ccpp_physics_*` entry points |
| `suite` | Generated suite cap | Module USE of generated suite data module |
| `ddt` | Type definition | Structural — describes DDT fields, no instance info |

Notes:
- `type = module` from capgen is renamed to `type = host`. Breaking change, intentional.
- `type = suite` tables are **written by the generator** (never hand-authored). They
  appear on disk for inspection and debugging only.
- `type = ddt` describes the structure of a Fortran derived type. It contains no
  instance information — only field definitions.

### 3.3 Per-variable attributes

All existing per-variable attributes are preserved: `standard_name`, `long_name`,
`units`, `dimensions`, `type`, `kind`, `intent`, `optional`, `active`, `protected`.

`protected = True` means: any scheme that declares `intent` other than `in` for this
variable is a metadata error, caught at generation time. This is how constants are
handled — a constants DDT is declared `type = host` with all fields `protected = True`.
No separate `type = constants` is needed.

### 3.4 DDT type definitions

A DDT type definition uses `type = ddt`:

```ini
[ccpp-table-properties]
  name = gfs_statein_type
  type = ddt

[ccpp-arg-table]
  name = gfs_statein_type
  type = ddt

[phii]
  standard_name = geopotential_at_interface
  long_name = geopotential at model layer interfaces
  units = m2 s-2
  dimensions = (horizontal_dimension, vertical_interface_dimension)
  type = real
  kind = kind_phys
```

### 3.5 Table-level properties

The `[ccpp-table-properties]` block supports the following table-level keys beyond `name`
and `type`:

| Key | Applies to | Purpose |
|---|---|---|
| `source_path` | `scheme` | Relative path from the `.meta` file directory to the directory containing the corresponding Fortran `.F90` source file. Defaults to the `.meta` file's own directory if absent. Used by the validator for auto-discovery of Fortran source. |
| `dependencies` | `scheme`, `host` | Comma-separated list of dependency file names or relative paths. Resolved to absolute paths using `dependencies_path` as a base directory (or the `.meta` file's directory if `dependencies_path` is absent). |
| `dependencies_path` | `scheme`, `host` | Optional subdirectory (relative to the `.meta` file's directory) used as the base when resolving entries in `dependencies`. Has no effect if `dependencies` is absent or `none`. |

Example:

```ini
[ccpp-table-properties]
  name = my_scheme
  type = scheme
  source_path = ../src
  dependencies_path = ../deps
  dependencies = utility_module.F90, shared_constants.F90
```

The resolved `dependencies` paths are collected across all scheme tables and written to the
`<dependencies>` section of `datatable.xml`. The validator uses `source_path` (not
`dependencies`) for locating the Fortran `.F90` corresponding to each `.meta` file.

**Parser implementation note:** The INI parser applies these table-level properties to
the `MetadataTable` object before transitioning to any `[ccpp-arg-table]` section. A
`flush_table_props()` call must happen at every parser-state transition (new table
header, first arg-table header, end-of-file) to avoid silently discarding the properties.

### 3.6 DDT instances

A DDT instance is declared as a regular variable entry inside a `type = host` table.
The enclosing `[ccpp-table-properties]` block's `name` attribute identifies the Fortran
module from which the instance is imported via `use`. No separate `module` attribute is
needed on the variable entry.

```ini
[ccpp-table-properties]
  name = CCPP_data
  type = host
  dependencies = CCPP_typedefs.F90,GFS_typedefs.F90

[ccpp-arg-table]
  name = CCPP_data
  type = host

[gfs_statein]
  standard_name = gfs_statein
  long_name = GFS state input for all instances
  units = mixed
  dimensions = (number_of_instances)
  type = gfs_statein_type
```

The generator looks up the `gfs_statein_type` DDT table, traverses its fields, and
constructs access paths of the form
`gfs_statein(instance_number)%fieldname(loop_begin:loop_end, 1:nlevs)`.

For scalar DDT instances (no instance dimension), dimensions is `()`.
For nested DDTs, the same mechanism applies recursively.

### 3.7 Control variable declarations

Control variables are declared in a `type = control` table. The generator resolves
each control variable by its standard name and uses whatever local Fortran name the
host declared:

```ini
[ccpp-table-properties]
  name = my_host_control_module
  type = control

[ccpp-arg-table]
  name = my_host_control_module
  type = control

[loop_begin]
  standard_name = horizontal_loop_begin
  long_name = start of horizontal loop
  units = index
  dimensions = ()
  type = integer
```

---

## 4. Control Variables

The generator recognizes the following standard names for control variables. Local
Fortran names are host-defined (resolved from the `type = control` metadata table).

### 4.1 Entry point arguments (non-register phases)

All required control variables are unconditional — every host must declare all of them.
Models that don't use a variable pass the neutral value: `1` for integers, `''` for
character arguments.

| Standard name | Expected type | Role |
|---|---|---|
| `suite_name` | `character` | Suite name for runtime dispatch |
| `horizontal_loop_begin` | `integer` | Start of horizontal slice (chunk bounds for `ccpp_physics_run`; `1` for all other phases) |
| `horizontal_loop_end` | `integer` | End of horizontal slice (chunk bounds for `ccpp_physics_run`; `ncols` for all other phases) |
| `number_of_physics_threads` | `integer` | Thread budget for physics-internal OpenMP; pass `1` if none |
| `ccpp_error_message` | `character` | Error message string |
| `ccpp_error_code` | `integer` | Integer error return code |

**Two symmetric paired-optional `(index, count)` control pairs** —
`instance_number` / `number_of_instances` and `thread_number` /
`number_of_threads`.  For each pair, declare **both** members in
`type=control` for the multi-instance / multi-threading API, or
**neither** for the single API; declaring exactly one is a hard error.
When a pair is absent, the static API drops the index argument and the
framework uses literal `1` where it would appear (and, for instances,
per-instance state arrays size to 1).  A host variable may be
dimensioned by a count standard name only when its pair is declared —
the `SCALAR_INDEX_DIMS` collapse substitutes the index local name
(`instance_number` / `thread_number`) and errors if it isn't in scope.

The asymmetry between the pairs is in *who reads the count*, not in the
rules: the framework reads `number_of_instances` to size its own
per-instance state; it does not yet read `number_of_threads`
(per-thread containers are host-owned) but carries it for future
symmetry.

`group_name` is **not** in the required set. It is included in the static API signature
only if the host declares it in their `type=control` table. When absent: the static API
calls all groups in declared order; no dispatch argument is generated; the generator
warns (not errors) if any loaded suite has more than one group. When present: it is a
required (non-optional) `character` argument; the value `''` (empty string) or `'all'`
calls all groups in order; any other value dispatches to the named group only.

The generator validates the required set at startup (after host metadata is parsed):
every required standard name must be present with the expected Fortran type (rank-0
scalar). All failures are collected and reported together before halting.

### 4.2 Loop-generated control variables (subcycles only)

| Standard name | Role |
|---|---|
| `ccpp_loop_counter` | Current subcycle iteration (1..ccpp_loop_extent) |
| `ccpp_loop_extent` | Total subcycle iterations; value comes from the `loop=N` attribute on the `<subcycle>` element in the suite XML definition file |

These are **not** passed as `ccpp_physics_*` arguments from the host. They are set by
the generated `do` loop inside the group cap and are available to any scheme called
within that loop. Outside a subcycle loop, these variables are not in scope.

### 4.3 Registered dimension standard names

The generator has built-in semantic knowledge of these dimension standard names:

| Standard name | Indexing semantic |
|---|---|
| Any key of `SCALAR_INDEX_DIMS` (currently `number_of_instances`, `number_of_threads`) | Scalar extraction: substitute the paired index variable's local Fortran name (currently `instance_number`, `thread_number`).  See `capgen/metadata/registered_dimensions.py` for the full table and the contract. |
| `horizontal_dimension` | **At scheme call sites**: always `horizontal_loop_begin:horizontal_loop_end` (using control variable local names), for all phases. **For suite-owned array allocation sizing**: local name of `horizontal_dimension` from the host `type=host` table (accessed via module USE, not the control variable). |
| `vertical_*` | Slice: `1:<local name of vertical_* variable>` |

`horizontal_dimension` and `vertical_*` are "registered" because the generator knows
their slicing semantics, but they are resolved to local names the same way as arbitrary
dimensions — by looking up the variable with that standard name in the host metadata.
**Scheme metadata always uses `horizontal_dimension`** for the horizontal extent, regardless of which phase the entrypoint belongs to. The standard name `horizontal_loop_extent` does not exist in the new design. The distinction between a chunk call and a full-domain call is handled entirely by what the host passes for `horizontal_loop_begin` and `horizontal_loop_end` — invisible to scheme developers and to the cap generator's slicing logic.

All other dimension standard names are resolved identically: look up the variable with
that standard name, get its local Fortran name, emit `1:local_name`.

Registered scalar-index dims are subject to one hard contract (Rule 2 in
`metadata/registered_dimensions.py`): they may appear only on **container
DDT-instance variables** in the access path, never on leaf data variables
(intrinsic- or `external:`-typed).  Leaves that declare them are rejected
at parse time with a remediation pointer.  See `doc/migration.md` §3.4.

---

## 5. Entry Points

Eight entry points are generated in the static API. Two tiers:

### 5.1 Framework lifecycle (no group_name dispatch)

These operate on the entire suite at once. They take `suite_name`,
`ccpp_error_code`, and `ccpp_error_message` (plus `instance_number` when the
host opts into the multi-instance pair, §4.1).  No scheme `_run/_init/_final`
calls.

| Entry point | Purpose |
|---|---|
| `ccpp_register(suite_name, errcode, errmsg, [instance_number])` | Calls each scheme's `_register` entrypoint; transitions suite state to `REGISTERED`.  Auto-provisions `ccpp_model_constituents_obj(:)` and friends in `ccpp_host_constituents.F90` when any register-phase scheme declares `ccpp_constituent_properties_t(:)` (constituents are not a formal arg). |
| `ccpp_init(suite_name, errcode, errmsg, [instance_number])` | Allocates integer state arrays in all group caps; allocates suite-owned interstitial data; calls the suite-level `<init>` scheme if declared (§5.5); no per-group scheme calls. |
| `ccpp_final(suite_name, errcode, errmsg, [instance_number])` | Calls the suite-level `<final>` scheme if declared (§5.5); deallocates integer state arrays and suite-owned data; no per-group scheme calls. |

Constituents are **opt-in**: a separate generated module
`ccpp_host_constituents.F90` declares `ccpp_model_constituents_obj(:)` and a
host-facing API (`ccpp_register_constituents`, `ccpp_initialize_constituents`,
`ccpp_const_get_index`, `ccpp_constituents_array(instance_number)`,
`ccpp_advected_constituents_array`, `ccpp_model_const_properties`,
`ccpp_number_constituents`, `ccpp_gather_constituents`,
`ccpp_update_constituents`, `ccpp_is_scheme_constituent`).  The host calls
these directly — they are not formal arguments of `ccpp_register` / `ccpp_init`.
See `doc/constituents.md`.

`instance_number` appears in every framework-lifecycle signature only when
the host declares the `instance_number` / `number_of_instances` pair (§4.1).
When present it propagates: `ccpp_init` → `<suite>_init` → each group's
`state_alloc(number_of_instances, ...)`.

### 5.2 Physics group invocation (dispatched by suite_name + group_name)

| Entry point | Calls scheme phase |
|---|---|
| `ccpp_physics_init(...)` | `_init` |
| `ccpp_physics_timestep_init(...)` | `_timestep_init` |
| `ccpp_physics_run(...)` | `_run` |
| `ccpp_physics_timestep_final(...)` | `_timestep_final` |
| `ccpp_physics_final(...)` | `_final` |

All five take the full required control variable argument list (Section 4.1) — a uniform
signature across all phases. If `group_name` is declared in the host's `type=control`
table, it is also included; `''` or `'all'` calls all groups in order, any other value
dispatches to the named group only. If `group_name` is absent from the control table,
no dispatch argument is generated and all groups are called in order.

The host is responsible for passing appropriate horizontal bounds: actual chunk bounds
for `ccpp_physics_run`; `1` and `ncols` (full domain) for all other phases. The cap
always uses `(horizontal_loop_begin:horizontal_loop_end)` for array slices — no
phase-specific special-casing.

### 5.3 Naming note

`finalize` is renamed to `final` throughout (e.g., `ccpp_physics_final`, not
`ccpp_physics_finalize`). Breaking change, intentional for symmetry:
`ccpp_init`/`ccpp_final`, `ccpp_physics_init`/`ccpp_physics_final`,
`ccpp_physics_timestep_init`/`ccpp_physics_timestep_final`.

The SDF likewise accepts only the canonical short element names:
`<init>` and `<final>` (§5.5).  The legacy long spellings — `<initalize>`
(old typo), `<initialize>` (correct long form), `<finalize>` — are
rejected at parse time with a clear error pointing at the short form.

### 5.5 Suite-level lifecycle hooks (`<init>` / `<final>`)

The SDF root may declare a **single** scheme that runs at suite-init
and/or suite-final time:

```xml
<suite name="my_suite" version="2.0">
  <init>my_init_scheme</init>     <!-- optional, at most one -->
  <group name="g">…</group>
  <final>my_final_scheme</final>  <!-- optional, at most one -->
</suite>
```

The named scheme's `init` (resp. `final`) phase is resolved from the
scheme metadata and called from inside `<suite>_init` (resp.
`<suite>_final`).  Ordering:

- `<suite>_init`: after all group `state_alloc` and
  `suite_data_init_fields`, **before** the `CCPP_SUITE_FRAMEWORK_INITIALIZED`
  state transition.  An errflg from the init scheme prevents the
  state transition.
- `<suite>_final`: before the `CCPP_SUITE_UNREGISTERED` transition.

Constraints:

- One scheme per `<init>` / `<final>`.  Multiple `<scheme>` children
  inside (the "group" shape) is a schema violation.
- The named scheme must have the matching phase in its metadata.
  Missing-phase metadata is a generator error.

### 5.4 Suite introspection routines

In addition to the eight entry points above, the static API exposes **five**
suite-introspection subroutines that let a host query, at runtime, what is
compiled into the API. These mirror the equivalent routines in the original
capgen (`scripts/ccpp_suite.py` — `write_inspection_routines`) and are
used by CMake integration and host-side build glue.

| Entry point | Purpose |
|---|---|
| `ccpp_physics_suite_list(suites)` | Return all suite names compiled into the API |
| `ccpp_physics_suite_part_list(suite_name, part_list, errmsg, errflg)` | Return the list of group ("part") names for a given suite |
| `ccpp_physics_suite_schemes(suite_name, scheme_list, errmsg, errflg)` | Return the list of scheme module names that compose a suite |
| `ccpp_physics_suite_variables(suite_name, variable_list, errmsg, errflg, [input_vars], [output_vars], [struct_elements])` | Standard-name list a suite consumes/produces; optional flags filter by intent and whether DDT sub-fields are flattened |
| `ccpp_physics_suite_host_data(suite_name, variable_list, errmsg, errflg)` | Standard-name list of host data the suite reads — DDT-collapsed view, excludes generated control variables |

These routines do not advance the state machine and do not call any scheme
entrypoints. All inputs derive from generator-time data already held in
`SuiteResolution` plus the host/scheme metadata; no new metadata is required.
The `_variables` vs `_host_data` split distinguishes the flat-leaf view
(every DDT field that is actually consumed) from the DDT-collapsed view
(parent DDT instances), and excludes capgen-generated control
variables from `_host_data` since the host owns those.

---

## 6. Cap Hierarchy

All three levels are fully auto-generated. No hand-written components in the cap layer.

### 6.1 Static API (`<host>_ccpp_cap.F90`)

- Imports all host DDTs and flat fields via `module use` (resolved from host metadata)
- Does not USE `ccpp_kinds` directly: the static API has no kind-typed declarations of
  its own (it dispatches by `suite_name` and forwards control args). `ccpp_kinds` is
  USEd only by files that declare kind-typed variables: group caps, the suite types
  module, and the suite data module.
- Dispatches all eight entry points by `suite_name` to the appropriate suite cap
- Does not own constituent state; constituents are accessed via the separate
  `ccpp_host_constituents.F90` module by both the host and group caps
- Holds no physics state

### 6.2 Suite cap (`ccpp_<suite>_cap.F90`)

- Imports the generated suite data module (`ccpp_<suite>_data.F90`)
- Contains the suite-level integer state array: `integer, allocatable :: ccpp_suite_state(:)` indexed by instance
- Implements the suite-level state machine (see Section 7)
- On `ccpp_register`: calls all scheme `_register` entrypoints across the suite
- On `<suite>_init(number_of_instances, errmsg, errflg)`: calls `state_alloc` for every
  group, passing `number_of_instances` (or literal `1` for single-instance hosts);
  also allocates suite-owned interstitial data. The `number_of_instances` argument is
  conditional on the host declaring it (Section 7.2.1).
- On `ccpp_final`: deallocates all of the above
- Routes `ccpp_physics_*` calls by `group_name` to the appropriate group cap function;
  passes `instance_number` (if present) through to group cap `_init` and `_final` subs

### 6.3 Group cap (`ccpp_<suite>_<group>_cap.F90`)

- Imports `ccpp_<suite>_data` (suite-owned interstitial data)
- Imports `ccpp_<suite>_types` (shared wrapper types)
- Contains the group-level integer state array: `integer, allocatable :: ccpp_group_state(:)` indexed by instance
- Implements the group-level state machine (see Section 7)
- Contains the actual scheme call sites for each phase:
  - Loop bound locals
  - Optional variable pointer arrays (thread-dimensioned)
  - Fixed-index extraction locals
  - Unit/kind conversion locals
  - Subcycle `do` loops
  - Scheme calls with full argument lists

---

## 7. State Machine

Integer state parameters are defined as **private named parameters directly inside each
generated group cap module** — they are NOT imported from a shared framework library
module. Each group cap file declares:

```fortran
integer, parameter, private :: CCPP_GROUP_UNINITIALIZED = 0
integer, parameter, private :: CCPP_GROUP_INITIALIZED   = 1
integer, parameter, private :: CCPP_GROUP_IN_TIMESTEP   = 2
```

This means the integer values are replicated across generated files (acceptable — the
names are the contract, not the values). No generated file USEs a framework state module.

Two levels, both indexed by `instance_number`.

### 7.1 Suite-level state (in suite cap)

```fortran
integer, parameter :: CCPP_SUITE_UNREGISTERED         = 0
integer, parameter :: CCPP_SUITE_REGISTERED           = 1
integer, parameter :: CCPP_SUITE_FRAMEWORK_INITIALIZED = 2
```

| Entry point | Required state | State after |
|---|---|---|
| `ccpp_register` | `== UNREGISTERED` | `REGISTERED` |
| `ccpp_init` | `== REGISTERED` | `FRAMEWORK_INITIALIZED` |
| `ccpp_physics_*` (non-final) | `== FRAMEWORK_INITIALIZED` | (unchanged) |
| `ccpp_physics_final` | (idempotent: silent skip if state array unallocated or `== UNREGISTERED`); otherwise `== FRAMEWORK_INITIALIZED` | (unchanged) |
| `ccpp_final` | (idempotent: silent skip if state array unallocated or `== UNREGISTERED`); otherwise any `>= REGISTERED` | `UNREGISTERED` (state array deallocated on last-to-leave) |

### 7.2 Group-level state (in each group cap)

```fortran
integer, parameter :: CCPP_GROUP_UNINITIALIZED = 0
integer, parameter :: CCPP_GROUP_INITIALIZED   = 1
integer, parameter :: CCPP_GROUP_IN_TIMESTEP   = 2
```

| Entry point | Required state | State after |
|---|---|---|
| `ccpp_physics_init` | `< INITIALIZED` (idempotent silent skip if `== INITIALIZED`) | `INITIALIZED` |
| `ccpp_physics_timestep_init` | `== INITIALIZED` | `IN_TIMESTEP` |
| `ccpp_physics_run` | `== IN_TIMESTEP` | `IN_TIMESTEP` |
| `ccpp_physics_timestep_final` | `== IN_TIMESTEP` | `INITIALIZED` |
| `ccpp_physics_final` | `>= INITIALIZED` (idempotent silent skip if `== UNINITIALIZED`) | `UNINITIALIZED` |

The idempotency rule for `ccpp_physics_init`: if the group is already in state
`INITIALIZED`, return immediately without calling any scheme `_init` routines. This
allows the host to call `ccpp_physics_init` multiple times safely. Any further call
after the first must result in no change (idempotency is a scheme contract).

The same rule applies to `ccpp_physics_final`: a repeat call (or a call issued
after `ccpp_final` has torn the suite down) must return cleanly with `errflg=0`
rather than erroring. This is enforced at both levels — the suite-cap dispatcher
silent-returns when `ccpp_suite_state` is unallocated or `== UNREGISTERED`, and
the group cap silent-returns when `ccpp_group_state(inst) == UNINITIALIZED`.

`ccpp_final` itself is also silently idempotent for the same reason: the
first call's last-to-leave block deallocates `ccpp_suite_state`, so on a
single-instance host the unallocated state *is* the normal post-`ccpp_final`
condition. Both checks (`.not. allocated(ccpp_suite_state)` and
`ccpp_suite_state(inst) == CCPP_SUITE_UNREGISTERED`) silent-return rather
than erroring. By contrast, `ccpp_init`'s "not allocated" branch keeps
erroring with "ccpp_register has not been called" — there, the unallocated
state really does indicate a missed `ccpp_register` call.

#### 7.2.1 State array allocation and instance indexing

Each group cap declares an allocatable module-level array:

```fortran
integer, private, allocatable :: ccpp_group_state(:)
```

Two generated subroutines manage it:

```fortran
! Always takes number_of_instances as an explicit arg — never USEs a host module.
subroutine ccpp_<suite>_<group>_state_alloc(number_of_instances, errmsg, errflg)
  integer, intent(in) :: number_of_instances
  ...
  allocate(ccpp_group_state(number_of_instances))
  ccpp_group_state(:) = CCPP_GROUP_UNINITIALIZED

subroutine ccpp_<suite>_<group>_state_dealloc(errmsg, errflg)
  ...
  if (allocated(ccpp_group_state)) deallocate(ccpp_group_state)
```

`state_alloc` is called from the suite cap's `<suite>_init` subroutine. The count is
passed as an explicit argument: the local name of `number_of_instances` from host
metadata (multi-instance), or the integer literal `1` (single-instance):

```fortran
! Multi-instance (host provides number_of_instances with local name ninstances):
subroutine test_suite_init(ninstances, errmsg, errflg)
  integer, intent(in) :: ninstances
  ...
  call ccpp_test_suite_physics_state_alloc(ninstances, errmsg, errflg)

! Single-instance (no number_of_instances in host metadata):
subroutine test_suite_init(errmsg, errflg)
  ...
  call ccpp_test_suite_physics_state_alloc(1, errmsg, errflg)
```

State array **indexing** in the phase subroutines uses the local name of
`instance_number` (e.g. `inst_num`) when the host provides it, otherwise the literal
`1`:

```fortran
subroutine ccpp_<suite>_<group>_init(inst_num, ...)
  if (ccpp_group_state(inst_num) >= CCPP_GROUP_INITIALIZED) return
  ...
  ccpp_group_state(inst_num) = CCPP_GROUP_INITIALIZED
```

`instance_number` is injected into the `_init` and `_final` phase subroutine signatures
even when no scheme in those phases uses it directly — the state guard and state
transition require it. It does **not** appear in `_run`, `_timestep_init`, or
`_timestep_final` unless a scheme in those phases explicitly requests it.

These two integer arrays replace both the boolean `initialized(:)` array from prebuild
and the string-based `ccpp_suite_state` from CAM-SIMA.

---

## 8. Scheme Metadata and Variable Matching

### 8.1 Scheme metadata structure

Each scheme source file has a companion `.meta` file with `type = scheme` tables — one
table per public phase subroutine (`scheme_name_init`, `scheme_name_run`,
`scheme_name_timestep_init`, etc.). The section header for each variable entry is the
**local variable name** as it appears in the scheme's Fortran subroutine argument list.

The internal metadata store is keyed as:

```
metadata[scheme_name][phase][standard_name] → {local_name, units, kind, dimensions,
                                                intent, optional, active, ...}
```

Keying by `scheme_name` then `phase` enables cross-phase queries ("does this scheme
have a register phase?", "what are all variables of scheme X?") and matches the
conceptual model of the suite XML.

### 8.2 Reading order

All metadata files (host + scheme + DDT) are read in one pass without resolving DDT
type references. After the full read, the generator builds the known DDT list, then
resolves all type references. This avoids ordering dependencies between metadata files.

### 8.3 Known DDT list

After reading all metadata, the generator assembles the set of known DDT types from
`type = ddt` tables. Two categories:

**Framework-defined DDTs**: declared in `type = ddt` metadata tables. The generator
knows their fields, dimensions, and access paths.

**External DDTs**: types from external libraries (MPI, ESMF, etc.) that the generator
cannot introspect. These are declared in variable entries using an extended `type`
syntax:

```ini
[mycomm]
  standard_name = mpi_communicator
  type = external:mpi_f08:mpi_comm
  ...
```

The format is `external:<module_name>:<type_name>`. The generator emits
`use mpi_f08, only: mpi_comm` and treats the variable as an opaque type — no field
traversal, no dimension indexing beyond what the metadata declares.

### 8.4 Variable matching: scheme vs. host

For each argument in a scheme's phase function (looked up by standard name):

1. **Found in host+control flat dict** → use the resolved access path. If `units` or
   `kind` differ from what the scheme declares, generate a transformation (Section 9).
2. **Not found, first use is `intent(out)`** → suite-owned variable. Add to suite data,
   generate declaration in `ccpp_<suite>_data.F90`.
3. **Not found, first use is `intent(in)` or `intent(inout)`** → **error**: variable
   used before it is provided by any scheme or host.
4. **Found in suite data (from a prior scheme)** → use the suite data access path.
   Apply transformation if needed.

### 8.5 Cap call argument construction

The generator builds the argument list for each scheme call from the scheme's metadata
argument order. For each argument:

- **Direct pass-through** (no transformation, not optional): inline host access
  expression — no local variable declared
- **Transformation**: cap-local temporary named after the scheme's local variable name
  (from scheme metadata section header); see Section 10.2 for naming rules
- **Optional**: cap-local pointer array named `<scheme_local_name>_p`; see Section 10.3
- **Optional + transformation**: combined in the `if (active) then` block

The generator does not parse Fortran source. All local names, types, kinds, dimensions,
and intents come exclusively from metadata.

---

## 9. Variable Resolution and Access Path Construction

### 9.1 Flat storage model (host+control+suite)

The generator flattens the DDT hierarchy at parse time. After parsing, all host,
control, and suite variables are stored in a flat dictionary keyed by standard name.
Each entry contains:
- The Fortran local name
- The fully-qualified access path (e.g., `gfs_statein(instance_number)%phii`)
- The module to USE
- Dimension information with registered/arbitrary classification

The DDT hierarchy is discarded after the flat dict is built. No live DDT object tree
is maintained during code generation.

### 9.2 Access path construction

For each variable, the generator constructs the call-site expression by applying
dimension rules to each dimension in order:

1. **Registered scalar-index dim** (key in `SCALAR_INDEX_DIMS`; currently
   `number_of_instances` → `instance_number`,
   `number_of_threads` → `thread_number`) → scalar extraction using the
   paired index variable's local Fortran name.  Only permitted on
   container DDT-instance variables, never on leaves (Rule 2; see
   `capgen/metadata/registered_dimensions.py`).
2. **`horizontal_dimension`** → always substitute `horizontal_loop_begin:horizontal_loop_end`
   (using control variable local names) at scheme call sites. For suite-owned array
   allocation sizing, `horizontal_dimension` from the host `type=host` table is used directly.
3. **`vertical_*`** → substitute `1:local_vertical_dimension`
4. **Arbitrary dimension** → resolve to local name via its own metadata entry, emit
   `1:local_name`
5. **`active` condition** → generate optional pointer-association guard (see Section 10.3)

### 9.3 Module USE

For each variable used in a group cap, the generator emits a `use module, only: varname`
statement. The module name comes from the enclosing `[ccpp-table-properties]` block name.
For suite-owned variables, the module is the generated `ccpp_<suite>_data`.

### 9.4 Eliminating TYPEDEFS_NEW_METADATA

The manually-maintained `TYPEDEFS_NEW_METADATA` Python dict from prebuild is eliminated.
All information previously in that dict is now in metadata:
- The DDT type structure → `type = ddt` table
- The module-level instance → variable entry in the `type = host` table, module
  implied by enclosing table name

---

## 10. Variable Transformations and Optional Variables

Variable transformations (unit/kind conversions) and optional variable handling are
combined — both occur within the same `if (active) then` block when a variable is
optional.

### 10.1 Supported transformations

- **Unit conversions** (e.g., Pa → hPa): formula from built-in conversion table (shared
  with validator), keyed on source/target unit pair from metadata `units` attribute
- **Kind conversions** (e.g., r8 → kind_phys): from `kind` metadata attribute comparison

The transformation framework is generic and pluggable — additional transformation types
(e.g., vertical flipping) can be added without restructuring the generator.

### 10.2 Local variable naming

The local variable name for a transformation temporary or optional pointer is derived
from the **scheme's local variable name** as declared in the scheme's metadata section
header (e.g., `[phii]` → local name is `phii`). The generator has this name without
parsing any Fortran.

- Transformation temporary: scheme's local name + `_l` (e.g., `phii_l`)
- Optional pointer: scheme's local name + `_p` (e.g., `phii_p`)
- Conflict resolution: if two schemes in the same group cap use the same local name for
  different standard names, append a numeric suffix before the suffix (e.g., `phii_2_l`)

The generator validates that all generated local variable names and generated subroutine
names stay within Fortran's 63-character identifier limit. Violations are code-generation
errors — the developer must use a shorter local name in their metadata.

The `active` expression in metadata is a Fortran logical expression written using
**CCPP standard names** (not local names). The generator translates all standard names
in the expression to their local Fortran names before emitting.

Transformations **always** use a local temporary variable. The host variable is never
modified in-place — required for bit-for-bit reproducibility and to leave host data
uncorrupted if an exception occurs. Every conversion line carries an inline Fortran
comment (e.g., `! unit conversion: Pa to hPa`). Transformation mismatches (unknown
unit pair, unknown kind pair) are code-generation errors — no stdout/stderr from
generated code.

### 10.3 The four cases

The generator handles exactly four combinations per variable:

**Case 1: No pointer, no transformation** (not optional, no unit/kind mismatch)

No local variable is declared. The host access expression is used inline at the call
site:
```fortran
call scheme_run(..., gfs_statein(instance_number)%phii(lb:ub,1:nlevs), ...)
```

**Case 2: Pointer only** (optional, no transformation)

Pointer array declared at function top; conditional association in `if (active)` block:
```fortran
! declaration:
type(real_kind_phys_rank1_ptr_type), target :: phii_p(number_of_threads)

! before call:
if (<active_condition>) then
  phii_p(thread_number)%ptr => gfs_statein(instance_number)%phii(lb:ub,1:nlevs)
else
  nullify(phii_p(thread_number)%ptr)
end if

call scheme_run(..., phii_p(thread_number)%ptr, ...)

! after call:
nullify(phii_p(thread_number)%ptr)
```

**Case 3: Transformation only** (not optional, unit/kind mismatch)

Local temporary `phii_l` (scheme's local name + `_l`); intent-driven emission:

| `intent` | Pre-call | Post-call |
|---|---|---|
| `in` | `phii_l = host_phii(...) * factor  ! unit conversion: X to Y` | nothing |
| `out` | nothing | `host_phii(...) = phii_l / factor  ! unit conversion: Y to X` |
| `inout` | pre-call as above | post-call as above |

```fortran
! declaration:
real(kind=kind_phys) :: phii_l(lb:ub, 1:nlevs)  ! or appropriate rank/kind

! before call (intent in/inout):
phii_l = gfs_statein(instance_number)%phii(lb:ub,1:nlevs) * 0.01_kind_phys  ! unit conversion: Pa to hPa

call scheme_run(..., phii_l, ...)

! after call (intent inout/out):
gfs_statein(instance_number)%phii(lb:ub,1:nlevs) = phii_l * 100.0_kind_phys  ! unit conversion: hPa to Pa
```

**Case 4: Pointer and transformation** (optional + unit/kind mismatch)

Two local variables: `phii_l` (transformation temporary) and `phii_p` (pointer array).
Sequence: (1) apply transformation to `phii_l` depending on intent, (2) assign pointer
to `phii_l`, (3) call scheme, (4) nullify pointer, (5) apply back-transformation from
`phii_l` to host depending on intent. All within the `if (active)` block:

```fortran
! declarations:
real(kind=kind_phys) :: phii_l(lb:ub, 1:nlevs)
type(real_kind_phys_rank1_ptr_type), target :: phii_p(number_of_threads)

! before call:
if (<active_condition>) then
  ! step 1: apply forward transformation (intent in/inout)
  phii_l = gfs_statein(instance_number)%phii(lb:ub,1:nlevs) * 0.01_kind_phys  ! unit conversion: Pa to hPa
  ! step 2: assign pointer to transformed local
  phii_p(thread_number)%ptr => phii_l
else
  nullify(phii_p(thread_number)%ptr)
end if

call scheme_run(..., phii_p(thread_number)%ptr, ...)

! after call:
if (<active_condition>) then
  ! step 4: nullify pointer
  nullify(phii_p(thread_number)%ptr)
  ! step 5: apply back-transformation (intent inout/out)
  gfs_statein(instance_number)%phii(lb:ub,1:nlevs) = phii_l * 100.0_kind_phys  ! unit conversion: hPa to Pa
end if
```

The wrapper types (`real_kind_phys_rank1_ptr_type` etc.) are defined once in the
generated shared types module (`ccpp_<suite>_types.F90`), not re-declared inside every
group cap function. Passing an unassociated pointer is safe under all compiler modes.

---

## 11. Subcycle Loops

When a group in the suite XML contains `<subcycle loop="N">`, the generator emits a
Fortran `do` loop in the group cap:

```fortran
do <ccpp_loop_counter_local> = 1, N  ! subcycle: N iterations from suite XML
  ! ... scheme calls ...
end do
```

`ccpp_loop_counter` and `ccpp_loop_extent` are **loop-context variables** — a special
class that does not fit any of the five table types:
- NOT `type = control`: not passed as `ccpp_physics_*` arguments from the host
- NOT `type = host`: not from host module USE
- NOT `type = suite`: not persistent allocated data

The generator has built-in knowledge of these two standard names. `ccpp_loop_extent`
value comes from the `loop=N` attribute in the suite XML definition file.
`ccpp_loop_counter` is the do loop induction variable. Both exist only within the
generated loop scope — they are not in scope outside a subcycle block.

Any scheme that requests `ccpp_loop_counter` or `ccpp_loop_extent` by standard name
receives the loop variables at the call site. Their local names in the cap are derived
from the scheme's metadata section headers as for any other variable (Section 10.2).

---

## 12. Init/Finalize Deduplication

If the same scheme appears more than once within a single group (e.g., via subcycles),
having its `_init` called multiple times is a **code generator bug** — the generator
**errors out** rather than silently deduplicating. The suite XML must not list the same
scheme multiple times in the same group for non-run phases.

If the same scheme appears in multiple groups, its `_init` is called once per group.
This is acceptable — idempotency is a contract all scheme `_init` routines must satisfy.

The same rule applies to `_timestep_init`, `_timestep_final`, and `_final`.

---

## 13. Suite-Owned Data

### 13.1 Discovery

The generator identifies suite-owned variables during variable resolution: variables
requested by schemes that are not satisfied by host metadata (`type = host` or
`type = control`).

**Error condition**: if a variable is determined to be suite-owned (not provided by the
host) and the first scheme that uses it does not have it as `intent(out)`, the generator
errors out. A suite-owned variable that is first read before it is written would be
used uninitialized.

### 13.2 Generated files and allocation

Suite-owned variables are declared in a generated suite data module
(`ccpp_<suite>_data.F90`) as fields of a Fortran DDT. The suite cap and all group caps
`use` this module.

Allocation happens in `ccpp_init` (suite cap). Deallocation happens in `ccpp_final`.
Suite-owned arrays are allocated for the **full `horizontal_dimension`** — threads
access their respective horizontal chunk (`horizontal_loop_begin:horizontal_loop_end`)
at scheme call sites. This avoids per-call allocation overhead. If this proves to
consume too much memory at scale, a future revision may move allocation to per-phase
with chunk-sized arrays; start with the full-dimension approach.

Subsetting (applying horizontal loop bounds, instance index) happens at scheme call
sites in the group cap, not in the suite cap.

### 13.3 Metadata

The generator also writes a `type = suite` metadata table (`ccpp_<suite>_data.meta`)
as a byproduct. The `_data` suffix matches the companion Fortran file
`ccpp_<suite>_data.F90`, satisfying the `.meta` ↔ `.F90` filename pairing. This
file is for inspection and debugging — it is not consumed by the generator on
subsequent runs.

---

## 14. Constituent API

> **Status (2026-05-12).** The constituent API in capgen has evolved past
> the sketch below.  The current implementation is:
>
> - One `ccpp_model_constituents_obj(:)` array (sized to
>   `number_of_instances`), declared and owned by the **generator** in
>   `ccpp_host_constituents.F90` — not by the host.
> - Host-facing API: `ccpp_register_constituents(host_constituents,
>   instance_number, ...)`, `ccpp_initialize_constituents`,
>   `ccpp_number_constituents`, `ccpp_const_get_index`,
>   `ccpp_constituents_array(instance_number)`, etc.  All per-instance.
> - Schemes follow four rules: register-phase
>   `ccpp_constituent_properties_t(:), intent=out, allocatable`;
>   physics-phase consume via `advected=true intent=in/inout`; tendency
>   produce via `constituent=true intent=out` + `tendency_of_<X>`
>   std_name; mismatched combos are codegen errors.
> - **Authoritative reference**: `doc/constituents.md` (full lifecycle +
>   API + examples).
> - **Architecture review and proposed reforms**: `doc/constituents_overhaul.md`
>   (2026-05-12, meeting-quality discussion of original capgen vs
>   capgen vs cam-sima needs, bugs/flaws, class-A/B property
>   classification, three proposals A/B/C).
>
> The historic text below is retained for context but does not describe
> the live system.

### 14.1 Type definition (historic)

`ccpp_model_constituents_t` is unchanged from CAM-SIMA. The type definition lives in
the framework library (`ccpp_constituent_prop_mod`), not in generated code.

### 14.2 Ownership and lifecycle (historic — superseded)

The **host model** declares and owns the constituent object:

```fortran
use ccpp_constituent_prop_mod, only: ccpp_model_constituents_t
type(ccpp_model_constituents_t) :: constituents  ! unallocated initially
```

The host passes it to `ccpp_register`, which allocates and populates it. After
`ccpp_register` returns, the host holds a fully allocated object ready for `lock_table`,
`const_index`, `copy_in`, `copy_out`.

The `constituents` argument to `ccpp_register` is mandatory.

### 14.3 Register phase mechanics (historic — superseded)

The suite cap's register routine:
1. Iterates over all constituent-providing schemes in the suite
2. Calls each scheme's `_register` entrypoint, which returns a
   `ccpp_constituent_properties_t` array
3. Collects these arrays and populates the constituent object

No `group_name` dispatch is needed for register — it operates on the whole suite.

---

## 15. Generated Output Files

All files are written to `--output-root`.

| File | Contents |
|---|---|
| `ccpp_kinds.F90` | Kind parameter definitions. **Always generated.** Re-exports specs from `iso_fortran_env` (default) or host-supplied modules as `integer, parameter, public :: <name> = <spec>`. If no `--kind-type` is supplied, `kind_phys=iso_fortran_env:REAL64` is injected automatically (logged at INFO). |
| `<host>_ccpp_cap.F90` | Static API — host imports, suite_name dispatch (filename and module name derived from `--host-name`) |
| `ccpp_<suite>_cap.F90` | Suite cap — suite data import, state machine, group dispatch |
| `ccpp_<suite>_<group>_cap.F90` | Group cap — scheme call sites, state array, optionals, transformations. USEs `ccpp_kinds` for any kind referenced in transformation temporaries. |
| `ccpp_<suite>_data.F90` | Suite data module — framework-owned interstitial DDT. USEs `ccpp_kinds` for any kind referenced in suite-var declarations. |
| `ccpp_<suite>_types.F90` | Shared cap types — optional pointer wrapper types, transformation locals. USEs `ccpp_kinds` for any kind referenced in pointer wrappers. |
| `ccpp_<suite>_data.meta` | Generated `type = suite` metadata table — pairs with `ccpp_<suite>_data.F90` (output-only, for inspection) |
| `datatable.xml` | Generator database for `ccpp_datafile.py` queries. `ccpp_kinds.F90` appears under `<ccpp_files><utilities>`; `<host>_ccpp_cap.F90` appears under `<ccpp_files><host_files>`. |

`ccpp_kinds.F90` is a dependency of all generated Fortran files that reference any kind parameter (group cap, suite types, suite data). The static API and suite cap have no kind references and do not USE it.

---

## 16. CLI Invocation

```
ccpp_capgen.py
  --host-name    <name>
  --host-files   <f1.meta,f2.meta,...>
  --scheme-files <f1.meta,f2.meta,...>
  --suites       <s1.xml,s2.xml,...>
  --output-root  <path>
  --kind-type    NAME=[MODULE:]SPEC    # repeatable, see § 16.1
  --verbose                            # once = info; twice = debug
```

The generator also supports programmatic Python invocation (import and call directly),
using the same internal code paths as the CLI.

### 16.1 Kind specifications

Each `--kind-type` maps a CCPP-visible kind name to a Fortran precision constant. Syntax:

```
--kind-type <name>=[<module>:]<spec>
```

* `<name>` — kind name as published in `ccpp_kinds` and referenced in scheme metadata
  (e.g. `kind_phys`).
* `<spec>` — name of a precision constant (kind parameter) defined in some Fortran
  module.
* `<module>` — Fortran module that defines `<spec>`. **Optional**: when omitted,
  `<spec>` must be a standard `ISO_FORTRAN_ENV` constant (`REAL32`, `REAL64`, `INT32`,
  ...) and the module defaults to `iso_fortran_env`. If `<spec>` is not a known ISO
  constant, omitting `<module>` is an error.

Examples:

* `--kind-type kind_phys=REAL64` →
  `use iso_fortran_env, only: REAL64; integer, parameter, public :: kind_phys = REAL64`
* `--kind-type kind_phys=my_host_kinds:kind_r8` →
  `use my_host_kinds, only: kind_r8; integer, parameter, public :: kind_phys = kind_r8`

The flag may be specified multiple times. `ccpp_kinds.F90` is **always generated**. If
no `--kind-type` is supplied (or `kind_phys` is omitted from a non-empty list), the
generator injects `kind_phys=iso_fortran_env:REAL64` and logs an INFO message.

### 16.2 datatable.xml and ccpp_datafile.py

The generator emits `datatable.xml` encoding the full relationships between suites,
groups, schemes, and variables. A separate query utility (`ccpp_datafile.py`) provides
a rich query interface used by CMake and other build systems. The full query surface of
the existing `ccpp_datafile.py` is preserved — the simplified `--ccpp-files` query is
one of many.

The XML structure is:

```xml
<ccpp_datatable version="1.0">
  <ccpp_files>
    <utilities>
      <file>/abs/path/ccpp_kinds.F90</file>
    </utilities>
    <host_files>
      <file>/abs/path/<host>_ccpp_cap.F90</file>
    </host_files>
    <suite_files>
      <file>/abs/path/ccpp_<suite>_cap.F90</file>
      ...
    </suite_files>
  </ccpp_files>
  <schemes>
    <scheme name="<scheme_name>">
      <run name="<scheme_name>" subroutine_name="<scheme_name>_run" module="<scheme_name>">
        <call_list>
          <var name="<standard_name>" intent="in|out|inout" local_name="<local_name>"/>
          ...
        </call_list>
      </run>
      <init .../>  <!-- if phase present -->
    </scheme>
    ...
  </schemes>
  <api>
    <suites>
      <suite name="<suite_name>">
        <group name="<group_name>">
          <scheme><scheme_name></scheme>
          ...
        </group>
      </suite>
    </suites>
  </api>
  <dependencies>
    <dependency>/abs/path/to/dep.F90</dependency>
    ...
  </dependencies>
</ccpp_datatable>
```

The `<dependencies>` section is populated from the `dependencies` table-level property
of all scheme metadata files (Section 3.5). Paths are resolved to absolute paths at
generation time, then sorted and deduplicated before writing.

### 16.3 CMake integration pattern

The generator runs at CMake configure time via `execute_process`. Generated sources are
discovered by querying `ccpp_datafile.py --ccpp-files`. Host and scheme Fortran sources
are found by replacing `.meta` with `.F90` (same base name convention).

---

## 17. Design Decisions Not Carried Forward

The following patterns from prebuild or capgen are explicitly **not** carried forward:

| Pattern | Reason |
|---|---|
| `ccpp_t` / `cdata` struct | Replaced by explicit named control variable arguments |
| `TYPEDEFS_NEW_METADATA` Python dict | Replaced by DDT instance declarations in metadata |
| String-based `ccpp_suite_state` | Replaced by integer state arrays with named parameters |
| Boolean `initialized(:)` array | Replaced by integer state arrays |
| Flat-field arguments to group caps | DDT arguments are used instead (as in prebuild) |
| Scope-chain variable promotion | Suite-owned variables explicitly discovered and declared |
| Fortran-vs-metadata validation in generator | Moved to standalone validator tool |
| Re-declaration of pointer wrapper types per function | Declared once in shared types module |
| `ccpp_physics_suite_init/finalize` | Replaced by `ccpp_init`/`ccpp_final` |
| `type = module` (capgen) | Renamed to `type = host` |
| `finalize` phase name | Renamed to `final` |
| Array size checks in caps | Not generated by default; rely on compiler bounds checking |
| Auto-clone of `is_constituent` scheme args into framework `%instantiate` calls | Replaced by explicit registration (host_constituents arg + register-phase `ccpp_constituent_properties_t`); the auto-clone path is also available behind the opt-in `--legacy-auto-clone-constituents` shim for legacy hosts (single-instance only — see `doc/auto_clone_constituents.md`) |
| `ConstituentVarDict` synthetic scope between suite and host | Removed; constituents are a `source='constituent'` classification on `ResolvedArg` |

---

## 18. Outstanding Work

See `MEMORY.md` (auto-memory index) and `project_implementation_status.md`
(deferred items) for the canonical list.  Snapshot as of 2026-05-13:

### Landed in the 2026-05-12 session

- **`instance_number` / `number_of_instances` paired opt-in** — hosts
  may omit both for a single-instance API.
- **Module-name override** — `module_name = <name>` on
  `[ccpp-table-properties]` for scheme/host/ddt.
- **Vertical-flip transform** — `top_at_one` per-var attribute;
  composes with unit/kind transforms.
- **Multiple `dependencies = …` lines** per `[ccpp-table-properties]`.
- **Sliced local names** with long subscript-token CCPP standard names
  no longer trip the 63-char Fortran-id limit.
- **Unit normalization** — `m2` ≡ `m+2` (and friends).
- **Subcycle bound = CCPP std name** — including DDT-component access
  paths (`phys_state%num_subcycles`).
- **Nested `<subcycle>`** — preserved end-to-end as nested `do` loops.
- **Active-expression + subcycle bounds** included in introspection
  inputs.
- **TARGET on `ccpp_suite_data(:)`** module-level array.
- **Group-state alloc idempotency** (matches suite-state alloc).
- **Framework PR**: `ccpt_deallocate` ownership tracking via
  `framework_owns_me` flag.  Backward-compatible.  Landed in
  capgen's vendored framework copy; still needs upstream merge
  to ccpp-framework + original ccpp-capgen.
- **Identity unit conversions** no longer emit misleading "unit
  conversion: kind_phys to kind_phys" comment.
- **Improved duplicate-standard-name error** lists both colliding
  access paths.
- **Suite-level `<init>` / `<final>`** SDF elements consumed: named
  scheme's init/final phase emitted inside `<suite>_init` /
  `<suite>_final`.  Single scheme only; long-form spellings
  (`<initalize>`, `<initialize>`, `<finalize>`) rejected.
- **Constituent resolver — host metadata wins**: hosts that declare
  framework-named std_names (`ccpp_constituents`, `index_of_<X>`, ...)
  short-circuit capgen's auto-provisioning so legacy hosts (GFS,
  SCM) keep using their own short local names (e.g. `ntcw`) without
  blowing Fortran's 63-char identifier limit.

### Landed 2026-05-13 (morning + afternoon)

- **`--legacy-mode` shim** — transient parse-time rewrite of legacy
  CCPP standard names (`horizontal_loop_extent` →
  `horizontal_dimension`).  Available on `ccpp_capgen.py` and
  `ccpp_validator.py`; loud banner at startup.  Isolated in
  `metadata/legacy_compat.py` and tagged `# legacy-compat:` for clean
  removal once scheme metadata has been migrated.
- **`_FRAMEWORK_CONST_DIM_INPUTS` cleanup** — the hand-curated
  frozenset in `generator/host_cap.py` was removed; framework-
  constituent dimension references now ride on a dedicated
  `used_const_dim_std_names` field on `ResolvedArg`.
- **`active` expression case-folding** — mixed-case standard names
  in `active = (...)` are now lowercased at parse time so they match
  the canonical lowercase host_dict keys (Fortran is case-insensitive,
  so embedded logical operators are unaffected).

### Landed 2026-05-13 (late evening — SCM-driven session)

- **`SCALAR_INDEX_DIMS` registered table** — `metadata/registered_dimensions.py`
  carries the single source of truth for count-dim ↔ scalar-index pairings
  (`number_of_instances → instance_number`, `number_of_threads → thread_number`).
  Drops the old `instance_dimension` placeholder.  Rule 2 (leaves never carry
  registered dims) enforced at parse time with rich error messages.
- **Loop-context resolver wired** — scheme args declaring
  `ccpp_loop_counter` / `ccpp_loop_extent` resolve inside `<subcycle>` to
  the generated do-loop locals (or the loop's literal/host-resolved
  bound for the extent).  Outside-subcycle raises a clear parse-time
  error pointing at the SDF contract.
- **Write-if-changed** — every generated cap file goes through
  `metadata/parse_tools/io_helpers.py::write_if_changed`; unchanged
  files keep their mtime so CMake/Make/Ninja don't trigger a rebuild
  cascade on regenerate.  Staging temp lives next to the target under
  `--output-root`, never `/tmp`.  Logger emits `"Wrote …"` vs
  `"Unchanged: …"`.
- **`--scheme-files` query** — `datatable.xml` carries a `<scheme_files>`
  section listing the user-supplied scheme `.F90` source paths actually
  referenced by some loaded suite (group phases + suite-level
  `<init>` / `<final>` hooks).  Companion `<dependencies>` filter applied
  to scheme tables (host/control/ddt deps still flow unconditionally).
- **`ccpp_<suite>_cap.F90` group dispatch + `ccpp_physics_*` suite
  dispatch case-default** — unknown `group_name` / `suite_name` now
  sets `errflg=1` and writes a clear message, no silent fall-through.
- **Missing-scheme parse-time detection** — `resolve_suite` walks every
  scheme reference in the SDF (group phases + `<init>` + `<final>`)
  and raises with the full list when any aren't in the scheme store.
  Replaces silent empty-group-cap emission.
- **Validator continuation look-ahead** — `_join_continuation` now
  detects continuation when the *current* line has no trailing `&`
  but the next line has a column-6 `&` marker (fixed-form F77).
  Fixes `sfc_sice.f::sfc_sice_run` and similar legacy CCPP-physics
  signatures.
- **Metadata error enrichment** — `MetaVar.set_attr` wraps every
  `check_X` helper failure with variable name + attribute name + raw
  value + source location, so `'' is not a valid unit` becomes
  actionable across a 60+ file load.
- **Character pointer-wrapper name encodes len** — `_ptr_type_name`
  bakes the length into the wrapper name so two `character(len=N)`
  args of different lengths don't collide on a single
  `character_rank1_ptr_type` symbol.  `len=:` → `_deferred`; `len=*`
  rejected; unparseable lengths rejected.
- **DDT-instance non-registered-dim diagnostic** — a DDT-instance
  variable with dims none of which are registered scalar-index AND
  with flattenable fields raises at parse time with the concrete
  would-be-broken access pattern.  Empty DDTs (e.g.
  `ccpp_constituent_prop_ptr_t`) flow through unchanged.
- **`build_ddt_module_map` honors per-DDT `module_name` override** —
  CCPP-physics `radsw_param.meta` declares `cmpfsw_type` in a
  scheme-less file with explicit `module_name`; previously skipped.
  Precedence: DDT's own `module_name` wins > co-located non-DDT
  table's resolved module > skipped.
- **`_resolve_single_bound` substitutes scalar-idx placeholders** —
  dim bounds that resolve through a per-thread/per-instance DDT
  field no longer leak the std-name placeholder in nested
  subscripts.
- **Legacy-mode adds second pair** — `--legacy-mode` now also
  rewrites `number_of_openmp_threads → number_of_threads`.  Banner
  enumerates every pair automatically; no hard-coded text per
  pairing.

### Landed in the 2026-05-14 → 2026-05-20 window

- **`--no-host-introspection` flag** (2026-05-14) — stubs the bodies
  of the five suite-introspection routines in `<host>_ccpp_cap.F90`,
  dropping the file from ~33k lines to ~800 for the 10-suite SCM
  build (the case-blocks were making even `-O1` compilation
  effectively hang).  Signatures stay so existing host callers still
  link; stubbed bodies return `errflg = 1` with a clear `errmsg`.
- **Per-instance dynamic-constituents buffer** (2026-05-18) — the
  per-suite buffer that holds register-phase-allocated constituents
  was lifted from "shared across instances" to a per-instance wrapper
  DDT array.  Surfaced by the new combined multi-instance +
  constituents end-to-end test (`instances_advection`).  Fixes a
  latent set_const_index conflict and a class-B setter-mutation
  problem.
- **Final-path silent idempotency** (2026-05-15) — both
  `ccpp_physics_final` and `ccpp_final` return `errflg = 0` on
  repeats; `ccpp_physics_final` additionally silent-skips after
  `ccpp_final` teardown.
- **Validator per-arg attribute checks** (2026-05-20) — `intent`,
  `type`, `kind`, and `rank` checked per argument; asymmetric
  `optional` rule; DDT + `external:<module>:<typename>` normalisation;
  character `len=*` wildcard.  Caught 67 real metadata/Fortran
  disagreements in the SCM physics tree on landing day.
- **Host/scheme metadata cross-checks** (late 2026-05-20) — resolver
  enforces type identity, rank, and per-position dim entries between
  host metadata (or first-writer suite-owned var) and every consuming
  scheme arg.  Bare `X` ≡ `1:X` ≡ `ccpp_constant_one:X` collapse;
  every other lower bound stays distinct; numeric kind stays lenient
  (transform path).
- **Host `active` + scheme arg runtime guard** (late 2026-05-20) —
  replaces the earlier static rule that required scheme metadata to
  declare `optional = True` for any host-active variable.  Optional
  scheme arg → pointer-association (PRESENT()-aware); non-optional
  scheme arg → group-cap runtime guard before any transform.
- **`ccpp_static_api.F90` → `<host>_ccpp_cap.F90`** — generated public
  entry-point file is now per-host (filename and module name driven
  by `--host-name`).  Multiple host integrations can coexist in one
  build.

### Landed 2026-05-21

- **`--gfs-dim-aliases` shim** — transient CLI flag that treats GFS
  radiation/composition vertical-dim names
  (`adjusted_vertical_layer_dimension_for_radiation` and
  `vertical_composition_dimension`) as equivalent to
  `vertical_layer_dimension` **inside the resolver's
  per-position dim-identity check only** (upper bound only).  Host
  variables stay distinct everywhere else.  Single touchpoint at
  `generator/suite_resolver.py::_canonical_dim`; module
  `metadata/dim_aliases.py`; touchpoints tagged `# dim-aliases:` for
  clean removal.  Generator-only (the validator never reaches the
  canonicaliser).  Required for CCPP-SCM 17p8 to build under
  capgen.
- **`--legacy-auto-clone-constituents` shim** — transient CLI flag
  that reinstates original ccpp-capgen's auto-clone-static-constituent
  registration path.  Every `is_constituent` consumer scheme arg
  (`advected = True` / `constituent = True` / `molar_mass = …`) with
  no register-phase source is auto-registered into the per-suite
  dynamic-constituents buffer using values lifted straight from the
  scheme metadata.  Adds four legacy `%instantiate` kwargs to the
  parser (`default_value`, `min_value`, `water_species`,
  `mixing_ratio_type`).  Synthesises `long_name` from std_name when
  missing; falls back `diag_name` to local_name; lifts `vertical_dim`
  from the arg's dim list.  Available on both `ccpp_capgen.py` and
  `ccpp_validator.py`.  **Single-instance only** — aborts before
  parsing if the host declares `instance_number` +
  `number_of_instances`.  Module
  `metadata/auto_clone_constituents.py`; touchpoints tagged
  `# auto-clone-constituents:`.  New e2e fixture
  `end-to-end-tests/advection_auto_clone/` is a port of CAM-SIMA's
  `advection_test`.  Full reference: `doc/auto_clone_constituents.md`.

### Test status

- **Unit tests**: 1426 passing (1438 with doctests; as of 2026-06-01).
  Run via `python unit-tests/run_tests.py [--doctest]`.
- **End-to-end tests** (10 passing): `advection`,
  `advection_auto_clone`, `capgen`, `chunked_data`, `ddthost`,
  `instances`, `instances_advection`, `nested_suite`, `opt_arg`,
  `var_compat`.  SCM running against ccpp-physics continues to be
  the active driver — most of the landings since 2026-05-13 were
  surfaced by SCM build/runtime failures.  Tree is off-limits for
  in-session edits — user-driven.

### Still deferred

- **Constituents overhaul** — discussion doc at
  `doc/constituents_overhaul.md` (2026-05-12).  Three proposals on the
  table (A bugfix-only / B class-A/B split + setters / C host-only
  registration).  Pending decision in upcoming meeting.
- **Framework setter additions** — `set_advected`, `set_diagnostic_name`,
  `set_default_value`, possibly `set_mixing_ratio_type`.  Coordinated with
  the overhaul.
- ~~**Validator host-metadata check**~~ — **Landed 2026-06-01**:
  `ccpp_validator.py --host-files` validates `type=host` and `type=ddt`
  tables against module-level decls and derived-type definitions in
  the same `--source-files` Fortran tree.  `type=control` is silent-
  skipped; `type=scheme` in `--host-files` is a hard error.  Per-arg
  type/kind/rank checks reuse `_check_arg_attributes`.  See
  `doc/migration.md` §7.4.
- **Codegen-time scheme-registration cross-check** — new metadata attr
  `registers_std_names = a, b, c` on register-phase tables; replaces
  current runtime `int_unassigned` check with codegen-time error.
- **Suppress `ccpp_host_constituents.F90` when unused** — currently
  emitted for every build; now *correct* (empty) for SCM-style hosts
  thanks to the host-wins rule, but still dead code.
- **`--legacy-mode` shim removal** — transient; remove
  `metadata/legacy_compat.py`, `unit-tests/test_legacy_compat.py`, and
  every `# legacy-compat:` touchpoint when scheme metadata has
  migrated.
- **`--gfs-dim-aliases` shim removal** (added 2026-05-21) —
  transient; remove `metadata/dim_aliases.py`,
  `unit-tests/test_dim_aliases.py`, and every `# dim-aliases:`
  touchpoint when GFS metadata stops spelling
  `vertical_layer_dimension` as
  `adjusted_vertical_layer_dimension_for_radiation` /
  `vertical_composition_dimension`.
- **`--legacy-auto-clone-constituents` shim removal** (added
  2026-05-21) — transient; remove
  `metadata/auto_clone_constituents.py`,
  `unit-tests/test_auto_clone_constituents.py`, the sample fixtures
  (`unit-tests/sample_files/scheme_auto_clone_consumer.meta`,
  `unit-tests/sample_suite_files/suite_auto_clone.xml`), and every
  `# auto-clone-constituents:` touchpoint when consumers have moved
  to explicit `host_constituents(:)` declaration or register-phase
  scheme registration.
- **Nested subcycle `ccpp_loop_counter` semantics**: a scheme inside a
  nested subcycle requesting `ccpp_loop_counter` would get the
  OUTERMOST counter, not the innermost.  None of the cam-sima schemes
  use this — revisit if a real scheme needs the innermost.
- **Python linter / formatter pass** — pick `ruff` and apply across
  `capgen/`.
- **Generated Fortran ↔ Codee formatter idempotency** — emitted `.F90`
  must round-trip cleanly through the project's Codee formatter.
- **`fortran_to_metadata` developer utility** — bootstrap a `.meta`
  skeleton from an existing `.F90` subroutine.

### Where to find the migration summary

`doc/migration.md` — user-facing single-page summary of metadata + SDF
+ host-Fortran requirements after all the above changes.  Read it
first when porting a host model.
