# CCPP capgen — Constituents Reference

*Last revised: 2026-05-13.*

This document is the authoritative reference for **constituent variables** in
capgen — what they are, how scheme authors declare them in metadata, what
the host model has to do to plumb them through, what the generator emits, and
how the per-instance lifecycle works.

> If you are migrating a host or scheme from the original capgen, jump to
> [§9 Differences from original capgen](#9-differences-from-original-capgen)
> first.

---

## Table of Contents

1. [What is a constituent?](#1-what-is-a-constituent)
2. [The four rules (scheme-author conventions)](#2-the-four-rules-scheme-author-conventions)
3. [Required host metadata + Fortran](#3-required-host-metadata--fortran)
4. [Host-side lifecycle (call sequence)](#4-host-side-lifecycle-call-sequence)
5. [Public API reference](#5-public-api-reference)
6. [Generated code structure](#6-generated-code-structure)
7. [Multi-instance design](#7-multi-instance-design)
8. [Limitations and gotchas](#8-limitations-and-gotchas)
9. [Differences from original capgen](#9-differences-from-original-capgen)
10. [Worked example](#10-worked-example)

---

## 1. What is a constituent?

A **constituent** is a model variable owned by the host's dynamical core (or
its constituent infrastructure) that is read and updated by physics schemes —
typically a tracer / mass mixing ratio (water vapor, cloud liquid, ozone,
chemistry species) — together with its **tendency**, the rate of change that
physics writes back so the dycore can advect/integrate it forward.

In capgen, the constituent layer has three concerns:

1. **Registration** — declaring at model startup which constituents exist
   (their standard name, units, vertical layout, advection flag, …).
2. **Storage** — the framework owns one `ccpp_model_constituents_t` DDT per
   host instance (see [§7](#7-multi-instance-design)) which holds the
   constituent values (`%vars_layer`), tendencies (`%vars_layer_tend`), and
   metadata (`%const_metadata`).
3. **Access** — schemes reference constituents by standard name in their
   metadata; the resolver translates those references to
   `ccpp_model_constituents_obj(inst)%vars_layer(slice, index_of_<X>)`
   subscripts at code-gen time.

All constituent state lives in **one generated module**:
`ccpp_host_constituents.F90` (one per generator run, emitted only when at
least one suite touches constituent state).  Public symbols from this module
are also re-exported by `<host>_ccpp_cap` (the per-host static API; filename
and module name derived from `--host-name`), so most host code only needs

```fortran
use <host>_ccpp_cap, only: ccpp_register_constituents, ccpp_initialize_constituents, &
                           ccpp_constituents_array, ccpp_const_get_index, ...
```

---

## 2. The four rules (scheme-author conventions)

These four rules govern every scheme-arg metadata pattern related to
constituents.  They derive from a 2026-05-11 audit of all 12 cam-sima scheme
metadata files that touch constituent attributes.

### Rule 1 — Register a new constituent (register phase)

A scheme that creates a new constituent declares it in the **register**
phase via an `intent=out, allocatable` array of
`ccpp_constituent_properties_t`:

```
[ccpp-arg-table]
  name = my_scheme_register
  type = scheme
[ dyn_const ]
  standard_name = dynamic_constituents_for_my_scheme
  long_name     = per-scheme constituent array
  units         = none
  dimensions    = (:)
  type          = ccpp_constituent_properties_t
  allocatable   = True
  intent        = out
[ errmsg ]
  ...
[ errflg ]
  ...
```

The scheme's Fortran register routine `allocate`s this array, populates
each entry via `%instantiate(std_name=..., long_name=..., units=...,
vertical_dim=..., advected=..., ...)` and returns it.  The framework
captures every register-phase scheme's array, packs them into a per-suite
buffer (`<suite>_dynamic_constituents`), and merges them into each
host-instance's constituent object during `ccpp_register_constituents`.

This is the **only path** for declaring a new constituent.

### Rule 2 — Consume a base constituent (any physics phase)

A scheme that reads (or reads + writes) an existing base constituent
declares the variable with `is_constituent` set (any of `advected`,
`constituent`, or `molar_mass` non-default) and `intent=in` or `intent=inout`:

```
[ cldliq ]
  standard_name = cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water
  units         = kg kg-1
  dimensions    = (horizontal_dimension, vertical_layer_dimension)
  type          = real | kind = kind_phys
  intent        = in        ! or inout
  advected      = true
```

The resolver translates this scheme arg to
`ccpp_model_constituents_obj(<inst>)%vars_layer(<slice>, index_of_<std_name>)`
in the generated group cap.  No host metadata declaration is needed for
the variable.

**Consumers need not re-flag (rule b, 2026-06-05).**  Whether a standard
name is a constituent or an ordinary variable is the **host's** decision
(CAM-SIMA exposes water vapor as a constituent; CCPP-SCM may expose the
same name as an ordinary host variable), so a scheme that only **reads**
a constituent — the base species, or a `tendency_of_<X>` — does **not**
repeat the `advected` / `constituent` flag.  capgen infers
constituent-ness for an unflagged `intent=in/inout` consumer from the
scheme-metadata-wide set of names *some* scheme flags
(`VariableResolver.constituent_stdnames()`): an unflagged read of the
base resolves to `%vars_layer(...)`, an unflagged read of
`tendency_of_<X>` to `%vars_layer_tend(..., index_of_<X>)` — the same
column a tendency producer (Rule 3) wrote.  **Host / earlier-suite
provision wins**: if the host declares the name, or an earlier scheme
already produced it as an ordinary variable, normal host/suite
resolution takes over.  (This is what lets the CAM-SIMA `cam7`
`sima_diagnostics` schemes read `tendency_of_water_vapor_…` that the
convection schemes produce.)

### Rule 3 — Produce a tendency (any physics phase)

A scheme that writes a constituent tendency declares the variable with
`is_constituent` set, `intent=out`, and a standard name that **starts
with `tendency_of_`**:

```
[ tend_cldliq ]
  standard_name = tendency_of_cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water
  units         = kg kg-1 s-1
  dimensions    = (horizontal_dimension, vertical_layer_dimension)
  type          = real | kind = kind_phys
  intent        = out
  constituent   = true
```

The resolver translates this scheme arg to
`ccpp_model_constituents_obj(<inst>)%vars_layer_tend(<slice>, index_of_<base_X>)`
where `<base_X>` is the std_name with the `tendency_of_` prefix stripped.
The tendency variable is implicitly tied to the base constituent of the
same name.

### Rule 4 — Mismatched combinations are hard errors

One combination is rejected by the resolver at code-gen time:

| Mismatch | Error |
|---|---|
| `is_constituent=True` + `intent=out` + std_name does NOT start with `tendency_of_` | *"Physics phases may only produce constituent tendencies; new base constituents must be declared via a `ccpp_constituent_properties_t` argument in a register-phase scheme."* |

> **Changed 2026-06-05:** consuming a constituent **tendency**
> (`intent=in/inout` on a `tendency_of_*` standard name) is **no longer**
> an error.  It resolves to `%vars_layer_tend(..., index_of_<X>)` — the
> same column a tendency producer writes — so a diagnostics scheme can
> read a tendency another scheme produced.  See "Consumers need not
> re-flag (rule b)" under Rule 2.

### Direct framework-array access

A scheme may also access the framework's bulk arrays directly by
declaring an arg with one of these standard names:

| Standard name | Maps to |
|---|---|
| `ccpp_constituents` | `ccpp_model_constituents_obj(<inst>)%vars_layer<sub>` (3D) |
| `ccpp_constituent_tendencies` | `ccpp_model_constituents_obj(<inst>)%vars_layer_tend<sub>` (3D) |
| `ccpp_constituent_properties` | `ccpp_model_constituents_obj(<inst>)%const_metadata<sub>` (1D of `ccpp_constituent_prop_ptr_t`) |
| `number_of_ccpp_constituents` | `ccpp_model_constituents_obj(<inst>)%num_layer_vars` (scalar integer) |
| `index_of_<X>` | module-level `integer :: index_of_<X>` (no per-instance access — the index is identical for every instance) |

The trailing dimension `number_of_ccpp_constituents` in a 3D scheme arg
is emitted as `:` (whole-axis slice).

---

## 3. Required host metadata + Fortran

### Host metadata (`type=host` table)

The host **must** declare:

```
[ <name> ]
  standard_name = number_of_instances
  units         = count
  dimensions    = ()
  type          = integer
```

… **only when the host actually wants multi-instance support**.  When
absent, every per-instance allocation falls back to size `1` and the
host effectively runs single-instance.

The host **does not** need to declare:

- `ccpp_model_constituents_object` — the constituent object is owned
  by the generator (in `ccpp_host_constituents`); the host doesn't
  declare it in metadata.
- `ccpp_constituents`, `ccpp_constituent_tendencies`,
  `ccpp_constituent_properties`, `number_of_ccpp_constituents`,
  `index_of_<X>` — all auto-provided by the generator.

#### Host metadata wins over auto-provisioning

If the host **does** declare any of the framework-named standard
names above as a regular host variable, the resolver uses the host's
declaration instead of auto-provisioning.  This matters for legacy
hosts (GFS / SCM) that own their own tracer indices:

```meta
[ ntcw ]
  standard_name = index_of_cloud_liquid_water_mixing_ratio_in_tracer_concentration_array
  units         = index
  type          = integer
  protected     = True
  dimensions    = ()
```

A scheme arg requesting the same standard name resolves to the host's
short local name (`ntcw`), not a parallel module-level integer in
`ccpp_host_constituents` named after the full standard name (which
would blow the Fortran 63-character identifier limit).  Auto-provisioning
only fires for framework-named standard names the host has **not**
claimed.

### Host control-table requirements

The host's `type=control` table must declare:

```
[ <name> ]
  standard_name = instance_number
  units         = 1
  dimensions    = ()
  type          = integer
```

… so the framework signature knows the index for per-instance state.
Same caveat as `number_of_instances` — required only when multi-instance
is wanted.

### Host Fortran code

The host's Fortran code only needs to:

1. Maintain its own `integer :: <local_name>` for `number_of_instances`
   in a module that's USE'd by the generator.  (Same module that owns
   the metadata.)
2. Build its **host constituents** array (water vapor, ozone, etc. —
   the constituents that the host model owns directly, separately from
   any scheme-registered ones).  Pass this to
   `ccpp_register_constituents`.

The host does **not** need to allocate or own a
`type(ccpp_model_constituents_t)` variable.

---

## 4. Host-side lifecycle (call sequence)

```
                          ┌─ host startup ─┐
                          │
                          ▼
        ┌──────────────────────────────────────┐
        │ for each instance:                   │
        │   ccpp_register(suite_name,          │
        │                 errmsg, errflg,      │
        │                 instance_number)     │  ─── per-instance ───┐
        └──────────────────────────────────────┘                      │
                          │                                          │
                          ▼                                          │
        ┌──────────────────────────────────────┐                     │
        │ allocate host_constituents(:)        │                     │
        │ host_constituents(1)%instantiate(    │  ─── once ─────────┘
        │   std_name='water_vapor_specific_humidity', ...)           │
        │ ...                                  │                     │
        └──────────────────────────────────────┘                     │
                          │                                          │
                          ▼                                          │
        ┌──────────────────────────────────────┐                     │
        │ for each instance:                   │                     │
        │   ccpp_register_constituents(        │                     │
        │     host_constituents,               │                     │
        │     instance_number, errflg, errmsg) │  ─── per-instance ──┤
        └──────────────────────────────────────┘                     │
                          │                                          │
                          ▼                                          │
        ┌──────────────────────────────────────┐                     │
        │ for each instance:                   │                     │
        │   ccpp_initialize_constituents(      │                     │
        │     ncols, num_layers,               │                     │
        │     instance_number, errflg, errmsg) │  ─── per-instance ──┤
        └──────────────────────────────────────┘                     │
                          │                                          │
                          ▼                                          │
        ┌──────────────────────────────────────┐                     │
        │ for each instance:                   │                     │
        │   ccpp_init(suite_name,              │                     │
        │             errmsg, errflg,          │                     │
        │             instance_number)         │  ─── per-instance ──┤
        └──────────────────────────────────────┘                     │
                          │                                          │
                          ▼ (model time-stepping)                    │
        ┌──────────────────────────────────────┐                     │
        │ ccpp_physics_*(...)                  │  ─── per-instance ──┤
        └──────────────────────────────────────┘                     │
                          │                                          │
                          ▼ (host shutdown)                          │
        ┌──────────────────────────────────────┐                     │
        │ for each instance:                   │                     │
        │   ccpp_deallocate_dynamic_constituents(                    │
        │     instance_number)                 │  ─── per-instance ──┤
        │   ccpp_final(suite_name,             │                     │
        │              errmsg, errflg,         │                     │
        │              instance_number)        │                     │
        └──────────────────────────────────────┘                     │
                                                                     │
                          ┌────────────────────────────────────────┘
                          │ ◀── last-to-leave dealloc fires
                          │     automatically inside the per-instance
                          │     calls when the final instance finishes.
                          ▼
```

### Important ordering rules

- `ccpp_register_constituents` **must** be called *after* `ccpp_register`
  (per instance).  The latter populates the per-suite dynamic-constituent
  buffers via `<suite>_register`; the former merges them into the
  per-instance constituent object.
- `ccpp_initialize_constituents` **must** be called *after*
  `ccpp_register_constituents` (per instance).  It calls `%lock_data`
  on the per-instance object — which can only happen once
  `%lock_table` has fired (which `ccpp_register_constituents` does).
- Physics phases (`ccpp_init`, `ccpp_physics_run`, etc.) require
  the constituent state to be locked + bound (i.e.,
  `ccpp_initialize_constituents` already called).
- `ccpp_deallocate_dynamic_constituents` is per-instance with
  last-to-leave teardown.  Once the last instance calls it, the shared
  per-suite buffers and the constituent object array are deallocated
  automatically.

### Built-in constituents vs scheme-registered constituents

`ccpp_register_constituents` takes one explicit argument: an array of
`ccpp_constituent_properties_t` describing the **host's own constituents**
(typically water vapor and any other tracers the dycore carries
intrinsically).  The framework then merges those entries with every
suite's per-suite dynamic-constituent buffer (populated during
`ccpp_register` from each register-phase scheme's output).

Pass an empty (zero-size) array if the host has no built-in constituents
of its own.

---

## 5. Public API reference

All routines below live in `ccpp_host_constituents` and are also
re-exported from `<host>_ccpp_cap` for convenience.  The dummy-argument
name `instance_number` is the **standard name**; the actual emitted
dummy uses the host's local name for it (typically also
`instance_number` or `inst_num`).

### `ccpp_register_constituents(host_constituents, instance_number, errflg, errmsg)`

| Arg | Direction / Type | Purpose |
|---|---|---|
| `host_constituents` | `type(ccpp_constituent_properties_t), target, intent(in) :: (:)` | Host-owned constituent declarations (water vapor, etc.). May be zero-size. |
| `instance_number` | `integer, intent(in)` | Per-instance index. |
| `errflg` | `integer, intent(out)` | Error flag (0 = success). |
| `errmsg` | `character(len=*), intent(out)` | Error message. |

**Effect**:
- On the first call across instances, allocates
  `ccpp_model_constituents_obj(number_of_instances)`.
- Calls `obj(instance_number)%initialize_table(num_consts)` where
  `num_consts = size(host_constituents) + sum(size(<suite>_dynamic_constituents))`.
- Iterates `host_constituents` first, then every suite's
  `<suite>_dynamic_constituents` buffer, calling
  `obj(instance_number)%new_field(const_prop, errcode=errflg, errmsg=errmsg)`
  for each entry.
- Calls `obj(instance_number)%lock_table(...)`.

**Preconditions**: every `<suite>_register` call (across all suites) for
this instance has already happened (so the per-suite buffers are
populated).

### `ccpp_initialize_constituents(ncols, num_layers, instance_number, errflg, errmsg)`

| Arg | Direction / Type | Purpose |
|---|---|---|
| `ncols` | `integer, intent(in)` | Horizontal extent for this instance's chunk. |
| `num_layers` | `integer, intent(in)` | Vertical layer count. |
| `instance_number` | `integer, intent(in)` | |
| `errflg` / `errmsg` | `intent(out)` | |

**Effect**:
- Calls `obj(instance_number)%lock_data(ncols, num_layers, ...)` —
  allocates `obj(inst)%vars_layer` and `%vars_layer_tend` arrays.
- Registers a singleton pointer with
  `ccpp_scheme_utils.ccpp_initialize_constituent_ptr(obj(inst))` so
  cam-sima schemes that call `ccpp_constituent_index` see the
  constituent table.  **First instance wins** — see
  [§8 Limitations](#8-limitations-and-gotchas).
- Queries `obj(instance_number)%const_index(index_of_<X>, '<X>', ...)` for
  every constituent `<X>` known at code-gen time; populates the
  module-level integer `index_of_<X>`.  These integers are identical
  across instances; the last call to set them wins (benign — the
  constituent table is the same per instance).

**Preconditions**: `ccpp_register_constituents` has been called for this
instance.

### `ccpp_is_scheme_constituent(var_name, constituent_exists, errflg, errmsg)`

| Arg | Direction / Type | Purpose |
|---|---|---|
| `var_name` | `character(len=*), intent(in)` | Standard name to query. |
| `constituent_exists` | `logical, intent(out)` | True iff *var_name* matches one of the constituent std names known to capgen at code-gen time. |
| `errflg` / `errmsg` | `intent(out)` | |

**No `instance_number`** — the data lookup is against the module-level
`character(len=N), parameter :: ccpp_model_const_stdnames(K)` array
(compile-time constant, identical across instances).

### `ccpp_number_constituents(num_flds, advected, instance_number, errflg, errmsg)`

| Arg | Direction / Type | Purpose |
|---|---|---|
| `num_flds` | `integer, intent(out)` | Constituent count returned. |
| `advected` | `logical, optional, intent(in)` | If `.true.`, count advected only. |
| `instance_number` | `integer, intent(in)` | |
| `errflg` / `errmsg` | `intent(out)` | |

Wraps `obj(instance_number)%num_constituents(num_flds, advected=advected, ...)`.

> Even though every `obj(i)` returns the same count (registration is
> identical across instances), `instance_number` is part of the
> signature so the caller can guarantee they're querying an
> already-locked instance.  Useful for hosts that lifecycle one
> instance at a time.

### `ccpp_gather_constituents(const_array, instance_number, errflg, errmsg)`

| Arg | Direction / Type | Purpose |
|---|---|---|
| `const_array` | `real(kind=kind_phys), intent(out) :: (:,:,:)` | Destination buffer for constituent values. |
| `instance_number` | `integer, intent(in)` | |
| `errflg` / `errmsg` | `intent(out)` | |

Wraps `obj(instance_number)%copy_in(const_array, ...)`.  Use this to
pull the per-instance constituent values into a host-side array.

### `ccpp_update_constituents(const_array, instance_number, errflg, errmsg)`

| Arg | Direction / Type | Purpose |
|---|---|---|
| `const_array` | `real(kind=kind_phys), intent(in) :: (:,:,:)` | Source buffer with updated constituent values. |
| `instance_number` | `integer, intent(in)` | |
| `errflg` / `errmsg` | `intent(out)` | |

Wraps `obj(instance_number)%copy_out(const_array, ...)`.  Use this to
push host-side updates back into the per-instance constituent object.

### `ccpp_const_get_index(stdname, const_index, instance_number, errflg, errmsg)`

| Arg | Direction / Type | Purpose |
|---|---|---|
| `stdname` | `character(len=*), intent(in)` | Constituent standard name to look up. |
| `const_index` | `integer, intent(out)` | Returned index into the constituent array (or `int_unassigned` on miss). |
| `instance_number` | `integer, intent(in)` | |
| `errflg` / `errmsg` | `intent(out)` | |

Wraps `obj(instance_number)%const_index(standard_name=stdname,
index=const_index, ...)`.  For constituents whose std names are known
at code-gen time, prefer using the module-level `index_of_<X>` integer
directly (no call needed; it's bound during
`ccpp_initialize_constituents`).

### `ccpp_constituents_array(instance_number) result(const_ptr)`

Returns `real(kind=kind_phys), pointer :: const_ptr(:,:,:)` →
`obj(instance_number)%field_data_ptr()`.

### `ccpp_advected_constituents_array(instance_number) result(const_ptr)`

Returns `real(kind=kind_phys), pointer :: const_ptr(:,:,:)` →
`obj(instance_number)%advected_constituents_ptr()`.  Subset of the
full constituent array containing only those flagged `advected=.true.`.

### `ccpp_model_const_properties(instance_number) result(const_ptr)`

Returns `type(ccpp_constituent_prop_ptr_t), pointer :: const_ptr(:)` →
`obj(instance_number)%constituent_props_ptr()`.

### `ccpp_deallocate_dynamic_constituents(instance_number)`

| Arg | Direction / Type | Purpose |
|---|---|---|
| `instance_number` | `integer, intent(in)` | |

**Per-instance reset + last-to-leave teardown**:
1. `obj(instance_number)%reset()` — unlocks the table for this instance.
2. Iterates every `obj(i)` and checks `%const_props_locked()`.  If any
   instance is still locked, the routine returns.
3. If **every** instance has been reset (none still locked), the routine
   tears down the shared state:
   - Deallocates every `<suite>_dynamic_constituents` buffer.
   - Deallocates `ccpp_model_constituents_obj(:)`.
   - Resets every `index_of_<X>` integer to 0.

The host should call this for every instance that successfully called
`ccpp_register_constituents`.

---

## 6. Generated code structure

When any suite touches constituent state, capgen emits one extra
module per generator run: **`ccpp_host_constituents.F90`**.

### Module declarations

```fortran
module ccpp_host_constituents
  use ccpp_kinds, only: kind_phys
  use ccpp_constituent_prop_mod, only: &
    ccpp_model_constituents_t, &
    ccpp_constituent_properties_t, &
    ccpp_constituent_prop_ptr_t

  implicit none
  private

  ! ----- public state ----------------------------------------------------
  public :: ccpp_model_constituents_obj
  public :: index_of_<X1>            ! one per known constituent std name
  public :: index_of_<X2>
  public :: ccpp_model_const_stdnames ! parameter array

  ! ----- public routines (also re-exported from <host>_ccpp_cap) --------
  public :: ccpp_register_constituents
  public :: ccpp_initialize_constituents
  public :: ccpp_is_scheme_constituent
  public :: ccpp_number_constituents
  public :: ccpp_gather_constituents
  public :: ccpp_update_constituents
  public :: ccpp_const_get_index
  public :: ccpp_constituents_array
  public :: ccpp_advected_constituents_array
  public :: ccpp_model_const_properties
  public :: ccpp_deallocate_dynamic_constituents
  public :: <suite_A>_dynamic_constituents  ! one per suite with register-phase producers
  public :: <suite_B>_dynamic_constituents

  ! ----- module-level state ---------------------------------------------
  type(ccpp_model_constituents_t), target, allocatable :: ccpp_model_constituents_obj(:)
  type(ccpp_constituent_properties_t), allocatable, target :: <suite_A>_dynamic_constituents(:)
  type(ccpp_constituent_properties_t), allocatable, target :: <suite_B>_dynamic_constituents(:)
  integer :: index_of_<X1> = 0
  integer :: index_of_<X2> = 0
  character(len=N), parameter :: ccpp_model_const_stdnames(K) = (/ &
    '<X1>     ', &
    '<X2>     ' /)

contains
  ! ... routines as documented in §5 ...
end module ccpp_host_constituents
```

### Suite-cap responsibilities

`ccpp_<suite>_cap.F90` does NOT own constituent state.  Its
`<suite>_register` routine packs each register-phase scheme's
constituent array into the suite's `<suite>_dynamic_constituents`
buffer (USE'd from `ccpp_host_constituents`):

```fortran
! Outer wrapper sized to number_of_instances on first call (any instance).
if (.not. allocated(<suite>_dynamic_constituents)) then
  allocate(<suite>_dynamic_constituents(number_of_instances))
end if

! Single pass: call each scheme's _register EXACTLY ONCE and append its
! returned array to THIS instance's slot.
allocate(<suite>_dynamic_constituents(inst)%items(0))
call <scheme1>_register(dyn_const=scheme_consts, ...)
if (errflg /= 0) return
<suite>_dynamic_constituents(inst)%items = &
    [<suite>_dynamic_constituents(inst)%items, scheme_consts]
deallocate(scheme_consts)
! ... one block like the above per constituent-registering scheme ...
```

Each instance owns its own `%items` slot (the per-instance buffer, so
`ccpp_register_constituents` can `set_const_index` independently per
instance); the suite state-machine guard ensures each instance populates
it exactly once.  Each scheme's `_register` is called **exactly once** —
it may safely allocate persistent module state (the earlier two-pass
count+copy called it twice).  The host-wide merge happens in
`ccpp_register_constituents`.

### Group-cap call sites

`ccpp_<suite>_<group>_cap.F90` USE's the constituent symbols it needs
from `ccpp_host_constituents`:

```fortran
use ccpp_host_constituents, only: ccpp_model_constituents_obj, &
                                  index_of_cloud_liquid_water_mixing_ratio
```

… and emits scheme call sites with the per-instance access expression:

```fortran
call cld_liq_run( &
    ...
    cldliq=ccpp_model_constituents_obj(inst_num)%vars_layer(lb:ub, 1:nlev, &
                                                            index_of_cloud_liquid_water_mixing_ratio), &
    tend_cldliq=ccpp_model_constituents_obj(inst_num)%vars_layer_tend(lb:ub, 1:nlev, &
                                                                       index_of_cloud_liquid_water_mixing_ratio), &
    ...)
```

The `instance_number` dummy is auto-injected into the group-cap
subroutine signatures by `_extra_dim_ctrl_entries` because the
resolver adds `instance_number` to every constituent arg's
`used_dim_std_names`.

### Framework F90 dependencies

`ccpp_host_constituents.F90` and the suite caps depend on these
framework files (listed under `<utilities>` in `datatable.xml`):

| File | Why |
|---|---|
| `ccpp_constituent_prop_mod.F90` | Provides `ccpp_model_constituents_t`, `ccpp_constituent_properties_t`, `ccpp_constituent_prop_ptr_t`. |
| `ccpp_hashable.F90` | Transitive dep of `ccpp_constituent_prop_mod`. |
| `ccpp_hash_table.F90` | Transitive dep. |
| `ccpp_scheme_utils.F90` | Provides `ccpp_initialize_constituent_ptr` + `ccpp_constituent_index` (used by cam-sima rrtmgp / mmm schemes). |

The host's CMake should query `ccpp_datafile.py --utility-files` to
get the absolute paths to these files at the right output location.

---

## 7. Multi-instance design

In capgen, **per-instance state** means: each "instance" (typically
an OpenMP team / chunk-domain partition) has its own copy of the
state arrays, indexed by `instance_number ∈ [1, number_of_instances]`.

### What's per-instance

| State | Storage |
|---|---|
| Constituent values + tendencies | `ccpp_model_constituents_obj(:)` — one DDT per instance |
| Suite-level state machine | `ccpp_suite_state(:)` — declared in each suite cap |
| Suite-owned data | `ccpp_suite_data(:)` — declared in each suite-data module |
| Group-level state machine | `ccpp_group_state(:)` — declared in each group cap |

### What's shared across instances

| State | Reason |
|---|---|
| `<suite>_dynamic_constituents(:)` per-suite buffers | Registration is identical per instance — populated by the first instance to call `<suite>_register` and reused by the rest |
| `index_of_<X>` integers | The constituent table is identical per instance, so the indices are too |
| `ccpp_model_const_stdnames` parameter array | Compile-time constant |

### Sizing

`number_of_instances` is the single source of truth.  The host declares
it in metadata + Fortran; the generator USE's it from the host module
wherever per-instance allocation happens.  See the prior memo
[*Where the total number of instances comes from*](#) for the call
chain (and matching values across all four state arrays:
`ccpp_suite_state`, `ccpp_suite_data`, `ccpp_group_state`,
`ccpp_model_constituents_obj`).

If the host doesn't declare `number_of_instances`, every per-instance
allocation falls back to `1` and the framework runs single-instance.

### Two host-side lifecycle patterns

Both work; pick whichever fits your model.

**Pattern A: all instances registered first**
```
do isuite = 1, num_suites
  do iinst = 1, num_instances
    call ccpp_register(suite_names(isuite), errmsg, errflg, iinst)
  end do
end do
do iinst = 1, num_instances
  call ccpp_register_constituents(host_constituents, iinst, errflg, errmsg)
  call ccpp_initialize_constituents(ncols, num_layers, iinst, errflg, errmsg)
end do
do isuite = 1, num_suites
  do iinst = 1, num_instances
    call ccpp_init(suite_names(isuite), errmsg, errflg, iinst)
  end do
end do
! ... time-stepping ...
do iinst = 1, num_instances
  call ccpp_deallocate_dynamic_constituents(iinst)
  ...
end do
```

**Pattern B: serial per instance**
```
do iinst = 1, num_instances
  do isuite = 1, num_suites
    call ccpp_register(suite_names(isuite), errmsg, errflg, iinst)
  end do
  call ccpp_register_constituents(host_constituents, iinst, errflg, errmsg)
  call ccpp_initialize_constituents(ncols, num_layers, iinst, errflg, errmsg)
  do isuite = 1, num_suites
    call ccpp_init(suite_names(isuite), errmsg, errflg, iinst)
  end do
  ! ... per-instance time-stepping ...
  call ccpp_deallocate_dynamic_constituents(iinst)
end do
```

### Last-to-leave teardown

`ccpp_deallocate_dynamic_constituents(inst)`:
1. Per-instance `obj(inst)%reset()`.
2. Iterates every `obj(i)`; if any has `%const_props_locked() == .true.`,
   returns early.
3. Otherwise (every instance reset): deallocates the shared per-suite
   buffers, deallocates `ccpp_model_constituents_obj(:)`, and zeros every
   `index_of_<X>` integer.

This works for both lifecycle patterns above.

---

## 8. Limitations and gotchas

> **Note (2026-05-12).** Several items in this section are under active
> discussion for an upcoming framework + generator overhaul.  See
> `doc/constituents_overhaul.md` for the full architectural review and
> three reform proposals.

### Framework property ownership (2026-05-12)

The framework's `ccpp_constituent_properties_t` now carries a private
`framework_owns_me` flag (default `.false.`) with
`is_framework_owned()` getter and `set_framework_owned(value)` setter.
`ccpt_deallocate` only deallocates the underlying prop when the flag
is `.true.`; otherwise it just nullifies its pointer.

Under capgen's explicit-registration model, all
`ccpp_constituent_properties_t` objects are **target-owned by the
caller** (the host's `host_constituents(:)` array, or the per-suite
`<suite>_dynamic_constituents(:)` buffer).  We never set the flag, so
the framework correctly skips deallocation.  Hosts that hand-allocate
property objects on the heap and want the framework to free them must
call `set_framework_owned(.true.)` before passing to `%new_field`.

### Missing setters (framework gap)

The framework lacks setters for `advected`, `diagnostic_name`,
`default_value` (and `mixing_ratio_type`).  This means once a
constituent is `%instantiate`d, those properties cannot be changed.
If your host needs to override a scheme-supplied `diagnostic_name` or
`advected` value, you currently cannot — open item in the constituents
overhaul proposal.

### `ccpp_scheme_utils` singleton

`ccpp_scheme_utils.ccpp_initialize_constituent_ptr` accepts only one
singleton pointer.  It's a framework-level convenience used by cam-sima
schemes that call `ccpp_constituent_index` from `ccpp_scheme_utils`.

`ccpp_initialize_constituents` calls
`ccpp_initialize_constituent_ptr(obj(inst))` on each instance, but
**only the first call across instances actually sets the pointer**
(the routine is internally guarded by an `initialized` flag).
Subsequent calls are silent no-ops.

For multi-instance hosts, schemes that use
`ccpp_scheme_utils.ccpp_constituent_index` will see only the first
instance's object — a known limitation inherited from the framework
module's design.  Schemes that use the per-instance accessors
(`obj(inst)%const_index(...)` via `ccpp_const_get_index`) are
unaffected.

### Constituent metadata is identical across instances

The constituent table (which constituents exist, their properties, the
`index_of_<X>` mapping) is **identical** for every instance.  Every
instance's `obj(i)` has the same hash table, populated identically by
its own `ccpp_register_constituents` call.

This means:

- `ccpp_number_constituents` returns the same value regardless of
  `instance_number`.
- `ccpp_const_get_index` returns the same index regardless of
  `instance_number`.
- The `index_of_<X>` integers are populated identically by every
  instance's `ccpp_initialize_constituents` (last-write-wins is fine
  since every write is the same value).

`instance_number` is still in the signatures of these routines — see
[§5](#5-public-api-reference) for the rationale.

### Forbidden patterns recap

This is rejected at code-gen time (Rule 4 of [§2](#2-the-four-rules-scheme-author-conventions)):

- `is_constituent + intent=out + non-tendency std_name` — physics phases
  may only produce tendencies, not new base constituents.

(As of 2026-06-05, `intent=in/inout + tendency_of_*` is **allowed** — a
constituent tendency may be *consumed*, resolving to `%vars_layer_tend`.
Only *producing* a tendency uses `intent=out`.)

### Subscript indices in sliced local_names must be standard names

If a host metadata variable is declared with a sliced local name
like `q(:,:,index_of_water_vapor_specific_humidity)`, every subscript
token (other than `:` and integer literals) must be a known standard
name.  Otherwise the resolver raises a `CCPPError` with a clear
message naming the offending token.

### Open work items

- **Unconditional `ccpp_host_constituents.F90` emission.** The
  generator currently emits `ccpp_host_constituents.F90` for every
  build, even when no scheme or host actually uses the constituent
  system (no `ccpp_constituent_properties_t(:)` register-phase arg,
  no `is_constituent`-flagged scheme arg, no framework-named
  `index_of_<X>` / `ccpp_constituents` / etc. claimed by capgen).
  When the host owns its own indices (SCM/GFS) and no scheme exercises
  the constituent path, the generated file is dead code that should be
  suppressed.  Tracked as a deferred item; the `host_dict` precedence
  rule above already keeps the file *correct* (empty) in that case.

---

## 9. Differences from original capgen

| Aspect | Original capgen | capgen |
|---|---|---|
| Constituent object location | Generated `<host>_ccpp_cap.F90` module | `ccpp_host_constituents.F90` (one per generator run) |
| Per-instance | No (single instance) | Yes (`obj(:)` allocatable, sized to `number_of_instances`) |
| Routine name prefix | `<host>_ccpp_register_constituents`, etc. | `ccpp_register_constituents`, etc. (no host prefix; one set per generator run) |
| Routine signatures | No `instance_number` arg | `instance_number` in every per-instance routine |
| Host metadata for constituent obj | None (auto-created by generator) | None (still auto-created by generator) |
| Module-level pointers | `<host>_constituents_array` etc. as functions returning pointers | Same idea, now per-instance via `instance_number` arg |
| Scheme std-name set | `<host>_model_const_stdnames` parameter array | `ccpp_model_const_stdnames` parameter array (no host prefix) |
| Host-facing API surface | `<host>_ccpp_register_constituents`, `<host>_ccpp_initialize_constituents`, `<host>_ccpp_number_constituents`, `<host>_ccpp_is_scheme_constituent`, `<host>_ccpp_gather_constituents`, `<host>_ccpp_update_constituents`, `<host>_ccpp_deallocate_dynamic_constituents`, `<host>_constituents_array`, `<host>_advected_constituents_array`, `<host>_model_const_properties`, `<host>_const_get_index` | Same surface, no `<host>_` prefix |
| Dynamic constituent buffer dimensionality | 1D, per host | 1D **per instance** (wrapper-DDT array indexed by `instance_number`; was shared across instances pre-2026-05-18, until the combined multi-instance + constituents e2e test surfaced a latent set_const_index conflict) |
| Static suite constituents | Auto-cloned by `ConstituentVarDict.find_variable` and registered via `<suite>_constituents_copy_const` accessors | Default behaviour: tracked at code-gen time via `is_constituent` flag; included in the constituent table only if a register-phase scheme produces them (rule 1).  Schemes that *consume* a base constituent (rule 2) don't trigger registration — the constituent must be registered by SOMEONE (host or another scheme's register) for the access to work at runtime.  **Opt-in shim** `--legacy-auto-clone-constituents` (2026-05-21, transient) reinstates original capgen's auto-clone path for legacy hosts; single-instance only.  See `doc/auto_clone_constituents.md`. |

### Migration notes for cam-sima hosts

- **Scheme metadata**: no changes needed for the 4 schemes that
  register constituents via `ccpp_constituent_properties_t` (rule 1) —
  those work unchanged.  For the ~16 schemes that rely on original
  capgen's auto-clone path (`advected = True` on a `_run` arg with no
  matching register-phase source), pass
  `--legacy-auto-clone-constituents` to `ccpp_capgen.py` and
  `ccpp_validator.py` — capgen then auto-registers those
  constituents into the per-suite dynamic-constituents buffer the same
  way original capgen did.  See `doc/auto_clone_constituents.md`.
- **Host metadata**: drop any explicit declaration of
  `ccpp_model_constituents_object` if you carried one over from a
  previous capgen experiment — the generator owns it now.
- **Host Fortran**: change all `<host>_ccpp_*_constituents` calls to
  the unprefixed names (`ccpp_register_constituents` etc.) and add
  `instance_number` to every call site.

---

## 10. Worked example

A minimal cam-sima-style suite with one scheme that consumes a base
constituent and produces its tendency.

### Scheme metadata (`consume_constituent.meta`)

```
[ccpp-table-properties]
  name = consume_constituent
  type = scheme

[ccpp-arg-table]
  name = consume_constituent_run
  type = scheme
[ cldliq ]
  standard_name = cloud_liquid_water_mixing_ratio
  units         = kg kg-1
  dimensions    = (horizontal_dimension, vertical_layer_dimension)
  type          = real | kind = kind_phys
  intent        = in
  advected      = .true.
[ tend_cldliq ]
  standard_name = tendency_of_cloud_liquid_water_mixing_ratio
  units         = kg kg-1 s-1
  dimensions    = (horizontal_dimension, vertical_layer_dimension)
  type          = real | kind = kind_phys
  intent        = out
  constituent   = .true.
[ errmsg ]
  ...
[ errflg ]
  ...
```

### Host metadata (`my_host.meta`)

```
[ccpp-table-properties]
  name = my_host
  type = host

[ccpp-arg-table]
  name = my_host
  type = host
[ ncols ]
  standard_name = horizontal_dimension
  units         = count
  dimensions    = ()
  type          = integer
[ nlev ]
  standard_name = vertical_layer_dimension
  units         = count
  dimensions    = ()
  type          = integer
[ ninstances ]
  standard_name = number_of_instances
  units         = count
  dimensions    = ()
  type          = integer
```

(Plus a `type=control` table declaring `instance_number`,
`horizontal_loop_begin`, `horizontal_loop_end`, `ccpp_error_message`,
`ccpp_error_code`, etc.)

### Suite XML (`my_suite.xml`)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<suite name="my_suite" version="1.0">
  <group name="phys">
    <scheme>consume_constituent</scheme>
  </group>
</suite>
```

### Generated `ccpp_host_constituents.F90` (excerpt)

```fortran
module ccpp_host_constituents
  use ccpp_kinds, only: kind_phys
  use ccpp_constituent_prop_mod, only: &
    ccpp_model_constituents_t, ccpp_constituent_properties_t, &
    ccpp_constituent_prop_ptr_t

  implicit none
  private

  public :: ccpp_model_constituents_obj
  public :: index_of_cloud_liquid_water_mixing_ratio
  public :: ccpp_register_constituents, ccpp_initialize_constituents
  public :: ccpp_is_scheme_constituent, ccpp_number_constituents
  public :: ccpp_gather_constituents, ccpp_update_constituents
  public :: ccpp_const_get_index, ccpp_constituents_array
  public :: ccpp_advected_constituents_array, ccpp_model_const_properties
  public :: ccpp_deallocate_dynamic_constituents
  public :: ccpp_model_const_stdnames

  type(ccpp_model_constituents_t), target, allocatable :: ccpp_model_constituents_obj(:)
  integer :: index_of_cloud_liquid_water_mixing_ratio = 0
  character(len=31), parameter :: ccpp_model_const_stdnames(1) = (/ &
    'cloud_liquid_water_mixing_ratio' /)

contains
  ! ... full subroutine bodies as in §5 ...
end module ccpp_host_constituents
```

### Host code skeleton (single-instance illustration)

```fortran
subroutine my_host_run()
  ! my_host_ccpp_cap is the per-host static API module
  ! (filename and module name derived from --host-name).
  use my_host_ccpp_cap, only: ccpp_register, ccpp_register_constituents,    &
                              ccpp_initialize_constituents, ccpp_init,      &
                              ccpp_physics_run, ccpp_final,                 &
                              ccpp_deallocate_dynamic_constituents
  use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

  type(ccpp_constituent_properties_t), allocatable :: host_consts(:)
  integer :: errflg
  character(len=512) :: errmsg
  integer, parameter :: inst = 1

  ! 1. Run register phase: populates per-suite dynamic-constituent buffers.
  call ccpp_register('my_suite', errmsg, errflg, inst)

  ! 2. Build host's own constituent declarations (water vapor, etc.).
  allocate(host_consts(1))
  call host_consts(1)%instantiate( &
    std_name='water_vapor_specific_humidity', long_name='water vapor', &
    units='kg kg-1', vertical_dim='vertical_layer_dimension', &
    advected=.true., errcode=errflg, errmsg=errmsg)

  ! 3. Merge host + suite-side constituents into obj(inst).
  call ccpp_register_constituents(host_consts, inst, errflg, errmsg)

  ! 4. Allocate vars_layer + bind cached indices.
  call ccpp_initialize_constituents(ncols, nlev, inst, errflg, errmsg)

  ! 5. Framework init phase.
  call ccpp_init('my_suite', errmsg, errflg, inst)

  ! 6. Time-stepping (omitted).
  call ccpp_physics_run('my_suite', 'phys', col_start, col_end, &
                        thread_num, nthreads, nphys_threads,    &
                        errflg, errmsg, inst)

  ! 7. Shutdown.
  call ccpp_final('my_suite', errmsg, errflg, inst)
  call ccpp_deallocate_dynamic_constituents(inst)
  deallocate(host_consts)
end subroutine my_host_run
```

For multi-instance, wrap each per-instance call in
`do iinst = 1, ninstances ... end do` per the patterns in
[§7](#7-multi-instance-design).
