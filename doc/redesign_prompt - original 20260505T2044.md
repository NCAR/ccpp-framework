# CCPP Framework Code Generator — Redesign Specification

## Purpose

This document is a complete implementation specification for a new CCPP Framework code
generator (`ccpp-capgen-ng`). It supersedes both `ccpp-prebuild` and `ccpp-capgen`. An
implementer should be able to build the new generator from scratch using this document
alone, supplemented by the real-world examples in `redesign_analysis.md`.

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

### 2.2 Code Generator (`ccpp_capgen_ng.py`)

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

### 3.5 DDT instances

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

### 3.6 Control variable declarations

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

| Standard name | Role | Conditional? |
|---|---|---|
| `suite_name` | Suite name for runtime dispatch | No |
| `group_name` | Group name for runtime dispatch | No |
| `horizontal_loop_begin` | Start of horizontal chunk/domain | No |
| `horizontal_loop_end` | End of horizontal chunk/domain | No |
| `thread_number` | Current thread index (1..number_of_threads) | No |
| `number_of_threads` | Host blocking loop thread count; allocation bound for thread-dimensioned suite data | No |
| `number_of_physics_threads` | Thread budget for physics-internal OpenMP use | No |
| `ccpp_error_message` | Error message string | No |
| `ccpp_error_code` | Integer error return code | No |
| `instance_number` | Current model instance index | **Conditional** |

`instance_number` is included only if `instance_dimension` appears anywhere in the
parsed host metadata. Single-instance models omit it from all entry point signatures.
The generator detects this automatically at parse time.

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
| `instance_dimension` | Scalar extraction: `var(instance_number)` — `instance_number` is the control variable |
| `horizontal_dimension` | Slice: `horizontal_loop_begin:horizontal_loop_end` (run phase) or `1:<local name of horizontal_dimension variable>` (non-run) |
| `vertical_*` | Slice: `1:<local name of vertical_* variable>` |

`horizontal_dimension` and `vertical_*` are "registered" because the generator knows
their slicing semantics, but they are resolved to local names the same way as arbitrary
dimensions — by looking up the variable with that standard name in the host metadata.
There is no special-casing in the resolution mechanism, only in the indexing expression
emitted.

All other dimension standard names are resolved identically: look up the variable with
that standard name, get its local Fortran name, emit `1:local_name`.

The timing of `instance_dimension` substitution — whether at parse time (when building
the flat dict access path) or at call-string generation time (like other registered
dimensions) — is an implementation decision left to the developer. Either is correct;
choose whichever is easier to implement, understand, and maintain.

---

## 5. Entry Points

Eight entry points are generated in the static API. Two tiers:

### 5.1 Framework lifecycle (no group_name dispatch)

These operate on the entire suite at once. They take `suite_name` plus
`ccpp_error_message` and `ccpp_error_code`. No scheme `_run/_init/_final` calls.

| Entry point | Purpose |
|---|---|
| `ccpp_register(suite_name, constituents, errmsg, errcode)` | Calls each scheme's `_register` entrypoint; allocates and populates the `ccpp_model_constituents_t` object passed by the host |
| `ccpp_init(suite_name, errmsg, errcode)` | Allocates integer state arrays in all group caps; allocates suite-owned interstitial data; no scheme calls |
| `ccpp_final(suite_name, errmsg, errcode)` | Deallocates integer state arrays and suite-owned data; no scheme calls |

`constituents` in `ccpp_register` is `intent(inout)`: unallocated on entry, allocated
and populated on exit. The host declares and owns this object (imports
`ccpp_model_constituents_t` from the framework library). The argument is mandatory.

### 5.2 Physics group invocation (dispatched by suite_name + group_name)

| Entry point | Calls scheme phase |
|---|---|
| `ccpp_physics_init(...)` | `_init` |
| `ccpp_physics_timestep_init(...)` | `_timestep_init` |
| `ccpp_physics_run(...)` | `_run` |
| `ccpp_physics_timestep_final(...)` | `_timestep_final` |
| `ccpp_physics_final(...)` | `_final` |

All five take the full control variable argument list (Section 4.1). `group_name` is
optional — if omitted, the suite cap calls all groups in declared order. If specified,
only that group is invoked.

Non-run phases pass `horizontal_loop_begin=1` and
`horizontal_loop_end=<local_horizontal_dimension>` — the cap code is uniform across
phases, with no special-casing for run vs. non-run.

### 5.3 Naming note

`finalize` is renamed to `final` throughout (e.g., `ccpp_physics_final`, not
`ccpp_physics_finalize`). Breaking change, intentional for symmetry:
`ccpp_init`/`ccpp_final`, `ccpp_physics_init`/`ccpp_physics_final`,
`ccpp_physics_timestep_init`/`ccpp_physics_timestep_final`.

---

## 6. Cap Hierarchy

All three levels are fully auto-generated. No hand-written components in the cap layer.

### 6.1 Static API (`ccpp_static_api.F90`)

- Imports all host DDTs and flat fields via `module use` (resolved from host metadata)
- Imports `ccpp_kinds` from `ccpp_kinds.F90`
- Dispatches all eight entry points by `suite_name` to the appropriate suite cap
- Passes `ccpp_model_constituents_t` through as an explicit argument (does not own it)
- Holds no physics state

### 6.2 Suite cap (`ccpp_<suite>_cap.F90`)

- Imports the generated suite data module (`ccpp_<suite>_data.F90`)
- Contains the suite-level integer state array: `integer, allocatable :: ccpp_suite_state(:)` indexed by instance
- Implements the suite-level state machine (see Section 7)
- On `ccpp_register`: calls all scheme `_register` entrypoints across the suite
- On `ccpp_init`: allocates suite-level state array; allocates `ccpp_group_state(:)` in
  all group caps for this suite; allocates suite-owned interstitial data
- On `ccpp_final`: deallocates all of the above
- Routes `ccpp_physics_*` calls by `group_name` to the appropriate group cap function

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

Integer state parameters are defined in a shared framework library module (not
generated). Two levels, both indexed by `instance_number`.

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
| `ccpp_physics_*` | `== FRAMEWORK_INITIALIZED` | (unchanged) |
| `ccpp_final` | `== FRAMEWORK_INITIALIZED` | `REGISTERED` |

### 7.2 Group-level state (in each group cap)

```fortran
integer, parameter :: CCPP_GROUP_UNINITIALIZED = 0
integer, parameter :: CCPP_GROUP_INITIALIZED   = 1
integer, parameter :: CCPP_GROUP_IN_TIMESTEP   = 2
```

| Entry point | Required state | State after |
|---|---|---|
| `ccpp_physics_init` | `< INITIALIZED` (idempotent if `== INITIALIZED`) | `INITIALIZED` |
| `ccpp_physics_timestep_init` | `== INITIALIZED` | `IN_TIMESTEP` |
| `ccpp_physics_run` | `== IN_TIMESTEP` | `IN_TIMESTEP` |
| `ccpp_physics_timestep_final` | `== IN_TIMESTEP` | `INITIALIZED` |
| `ccpp_physics_final` | `>= INITIALIZED` | `UNINITIALIZED` |

The idempotency rule for `ccpp_physics_init`: if the group is already in state
`INITIALIZED`, return immediately without calling any scheme `_init` routines. This
allows the host to call `ccpp_physics_init` multiple times safely. Any further call
after the first must result in no change (idempotency is a scheme contract).

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

1. **`instance_dimension`** → substitute `instance_number` (scalar extraction)
2. **`horizontal_dimension`** → substitute `horizontal_loop_begin:horizontal_loop_end`
   (run phase) or `1:local_horizontal_dimension` (non-run)
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

The generator also writes a `type = suite` metadata table (`ccpp_<suite>.meta`) as a
byproduct. This file is for inspection and debugging — it is not consumed by the
generator on subsequent runs.

---

## 14. Constituent API

### 14.1 Type definition

`ccpp_model_constituents_t` is unchanged from CAM-SIMA. The type definition lives in
the framework library (`ccpp_constituent_prop_mod`), not in generated code.

### 14.2 Ownership and lifecycle

The **host model** declares and owns the constituent object:

```fortran
use ccpp_constituent_prop_mod, only: ccpp_model_constituents_t
type(ccpp_model_constituents_t) :: constituents  ! unallocated initially
```

The host passes it to `ccpp_register`, which allocates and populates it. After
`ccpp_register` returns, the host holds a fully allocated object ready for `lock_table`,
`const_index`, `copy_in`, `copy_out`.

The `constituents` argument to `ccpp_register` is mandatory.

### 14.3 Register phase mechanics

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
| `ccpp_kinds.F90` | Kind parameter definitions from `--kind-type` CLI args |
| `ccpp_static_api.F90` | Static API — host imports, suite_name dispatch |
| `ccpp_<suite>_cap.F90` | Suite cap — suite data import, state machine, group dispatch |
| `ccpp_<suite>_<group>_cap.F90` | Group cap — scheme call sites, state array, optionals, transformations |
| `ccpp_<suite>_data.F90` | Suite data module — framework-owned interstitial DDT |
| `ccpp_<suite>_types.F90` | Shared cap types — optional pointer wrapper types, transformation locals |
| `ccpp_<suite>.meta` | Generated `type = suite` metadata table (output-only, for inspection) |
| `datatable.xml` | Generator database for `ccpp_datafile.py` queries |

`ccpp_kinds.F90` is a dependency of all other generated Fortran files.

---

## 16. CLI Invocation

```
ccpp_capgen_ng.py
  --host-name    <name>
  --host-files   <f1.meta,f2.meta,...>
  --scheme-files <f1.meta,f2.meta,...>
  --suites       <s1.xml,s2.xml,...>
  --output-root  <path>
  --kind-type    KIND=PRECISION        # repeatable, e.g. --kind-type kind_phys=REAL64
  --verbose                            # once = info; twice = debug
```

The generator also supports programmatic Python invocation (import and call directly),
using the same internal code paths as the CLI.

### 16.1 Kind specifications

Kind mappings are passed at the CLI level, not in metadata. The generator substitutes
kind names in all generated Fortran declarations. Example:
`--kind-type kind_phys=REAL64 --kind-type kind_dyn=REAL32`.

### 16.2 datatable.xml and ccpp_datafile.py

The generator emits `datatable.xml` encoding the full relationships between suites,
groups, schemes, and variables. A separate query utility (`ccpp_datafile.py`) provides
a rich query interface used by CMake and other build systems. The full query surface of
the existing `ccpp_datafile.py` is preserved — the simplified `--ccpp-files` query is
one of many.

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
