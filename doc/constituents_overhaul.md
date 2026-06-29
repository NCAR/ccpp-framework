# CCPP Constituents — Architecture Review & Overhaul Discussion

**Authors:** Dom Heinzeller (lead), Claude (assistant)
**Date drafted:** 2026-05-12
**Last revised:** 2026-06-05
**Intended audience:** CCPP framework team, CAM-SIMA team
**Status:** Discussion document — no decisions are final.  Proposals
A/B/C below remain pending the upcoming meeting; the bug fix from
Proposal A (the `ccpt_deallocate` ownership flag) and the capgen
internal cleanup from Proposal B (§4.8) have landed; the missing
setters from Proposal A and the `is_match` relaxation from Proposal B
have not.  Independent of A/B/C, the per-suite dynamic_constituents
buffer was made per-instance on 2026-05-18 to fix a multi-instance
mutation conflict — see §4.13.  Since 2026-06-03 capgen drives the
real CAM-SIMA build (via the `cime_config/capgen_compat/` facade): the
`kessler`, `rrtmgp`, and `se_cslam`/CSLAM (FCAM7 `cam7`) cases all build
and run on Derecho.  That integration added the **rule-b** consumer path
(read a constituent or `tendency_of_<X>` without re-flagging — §2.2.3)
and surfaced the host-adapter fix in §4.15; neither changes the A/B/C
decision surface.

---

## Executive summary

CCPP's "constituent" mechanism — how schemes declare and how the framework
manages tracer species like water vapor, cloud liquid, prescribed ozone,
etc. — has grown organically over the last few years. The result works,
but it carries:

- **A latent framework bug** in `ccpp_constituent_prop_mod` that crashes on
  teardown of explicitly-registered (target-passed) constituent property
  arrays. Fixed in capgen's framework copy 2026-05-12; needs to land
  upstream.
- **Architectural confusion** about which properties are *physics-portable*
  (the scheme owns them) versus *host-configuration* (the host owns them).
  Today schemes are forced to supply host-specific values (`diag_name` is
  the worst offender) at `%instantiate` time.
- **Setter API gaps**: properties that the host wants to override after
  scheme-side registration (`advected`, `diagnostic_name`, `default_value`)
  have no setters; `is_match` is overly strict about properties hosts
  should be free to change.
- **Two registration models** coexist — original capgen's auto-clone of
  is_constituent scheme args, and capgen's/capgen's explicit register-phase +
  host-side declaration. Capgen deliberately dropped auto-clone.

This document is a structured brief for a discussion this week. It does
NOT pre-commit to any decision; it lays out what exists, what's broken,
what we audited, and what proposals are on the table.

---

## Table of contents

1. [How original capgen handles constituents](#1-how-original-capgen-handles-constituents)
2. [How capgen handles constituents](#2-how-capgen-handles-constituents)
3. [What CAM-SIMA actually needs (audit)](#3-what-cam-sima-actually-needs-audit)
4. [Bugs and design flaws](#4-bugs-and-design-flaws)
5. [Property classification (Class A vs Class B)](#5-property-classification-class-a-vs-class-b)
6. [What to remove, replace, improve](#6-what-to-remove-replace-improve)
7. [Open design questions](#7-open-design-questions)
8. [Three proposals — minimal / clean / deep](#8-three-proposals--minimal--clean--deep)
9. [Appendix: framework setter inventory](#9-appendix-framework-setter-inventory)

---

## 1. How original capgen handles constituents

### 1.1 Mental model

Original capgen treats constituents as a **separate scope** between
suite and host:

```
group  →  suite  →  ConstituentVarDict  →  host
```

A scheme arg flagged `constituent = True` in metadata is matched first
against group/suite/ConstituentVarDict, and only against host as a last
resort. The ConstituentVarDict is a synthetic dictionary whose entries
are auto-created by `find_variable()` when a scheme metadata declares a
constituent dependency.

### 1.2 Auto-clone of `is_constituent` scheme args

Every scheme arg with non-default `advected`, `constituent`, or
`molar_mass` is treated as a *registration*. The generator emits, into
the host cap, a routine `<suite>_constituents_ccpp_create_constituent_array`
that:

1. Allocates a `ccpp_constituent_properties_t` pointer per scheme arg.
2. Calls `%instantiate(...)` populating fields **from the scheme
   metadata directly** — `std_name`, `long_name`, `diagnostic_name`,
   `units`, `default_value`, `advected`, `vertical_dim`, etc. (See
   `scripts/constituents.py:565`.)
3. Adds it to the model constituents object via `%new_field`.

After this auto-clone runs, the host's hand-written
`host_constituents(:)` array is appended, then `%lock_table` finalizes
the hash table.

### 1.3 The host-cap-owned `ccpp_model_constituents_obj`

Original capgen generates **one** `ccpp_model_constituents_obj` per
generator invocation, declared module-level in `<host>_ccpp_cap.F90`.
Single global; not per-instance. (CAM-SIMA runs one host per
executable, so single-instance is fine for them.)

### 1.4 Scheme-side `%instantiate` registration (the other path)

A scheme may also register constituents via a register-phase argument:

```fortran
type(ccpp_constituent_properties_t), allocatable, intent(out) :: dyn_const(:)
```

The scheme allocates the array, calls `%instantiate` per entry, and
returns it. Original capgen wires this through a per-suite
"dynamic constituents" buffer and merges it during host-cap setup,
alongside the auto-cloned set.

So original capgen really supports **three** registration sources:

- Host: hand-written `host_constituents(:)` arg.
- Suite-dynamic: register-phase scheme args.
- Suite-static: auto-cloned from any `is_constituent` consumer.

All three flow into one `%new_field` table.

### 1.5 Lifecycle

- `<host>_ccpp_register_constituents(host_constituents, ...)` runs the
  three-source merge.
- `ccpp_initialize_constituent_ptr(const_obj, ...)` (in
  `ccpp_scheme_utils`) caches a pointer for the `ccpp_constituent_index`
  lookup.
- Phase entry points access `vars_layer` / `vars_layer_tend` via cached
  `index_of_<X>` integers.

### 1.6 What's good about original capgen's approach

- Schemes declare a constituent dependency once in metadata; no manual
  Fortran registration ever needed for "static" tracers.
- Host doesn't have to enumerate every species every scheme wants.
- Works for CAM-SIMA's current scheme catalog.

### 1.7 What's painful about original capgen's approach

- The auto-clone path is **invisible** to anyone reading the scheme
  Fortran — the registration happens in generated code.
- `ConstituentVarDict` is a synthetic scope, conceptually subtle, and
  doesn't generalize cleanly to multi-instance.
- The auto-clone path lifts `diagnostic_name` and `default_value` from
  scheme metadata, but those values are often host-specific (see §4.4).
- Three sources of registration with overlap mean two registrations of
  the same `std_name` may collide; original capgen relies on
  `is_match` (units, advected, thermo_active, water_species) to dedup,
  which means schemes accidentally diverge on `advected` and trip the
  "incompatible constituent" error.

---

## 2. How capgen handles constituents

### 2.1 Mental model

No synthetic scope. Constituents are *one of four* sources for any
scheme arg:

```
control | host | suite | constituent
```

The resolver classifies each scheme arg into exactly one source. A
`constituent` source means the value will be accessed at runtime as
`ccpp_model_constituents_obj(<inst>)%vars_layer(:, :, index_of_<X>)`
(or `%vars_layer_tend(...)` for `tendency_of_<X>` outputs).

### 2.2 The scheme-author rules

(See `doc/constituents.md` for full details; this is the summary.)

1. **Register** — register-phase scheme args of type
   `ccpp_constituent_properties_t(:), intent=out, allocatable` declare
   new constituents the scheme contributes.
2. **Flag what you own** — a physics-phase arg flagged `advected=true`
   (or `molar_mass=...` or `constituent=true`) marks its standard name as
   a constituent: a base species read via `%vars_layer`
   (`intent=in/inout`), or — when the name is `tendency_of_<X>` — a
   constituent tendency *written* via `%vars_layer_tend` (`intent=out`).
   A base constituent therefore uses `intent=inout` (read-modify-write the
   shared column), never `intent=out`; `intent=out` is reserved for
   tendencies. (This is why CAM-SIMA's `state_converters` dry→moist
   converters declare the moist mixing ratios `intent=inout`.)
3. **Consume without re-flagging (rule b)** — a scheme that merely READS a
   name some *other* scheme flags as a constituent does **not** repeat the
   flag. Whether a standard name is a constituent or an ordinary variable
   is the **host's** decision (CAM-SIMA exposes water vapor as a
   constituent; CCPP-SCM may expose the same name as an ordinary host
   variable), so capgen infers it from the scheme-metadata-wide set of
   flagged names (`VariableResolver.constituent_stdnames()`) rather than
   from the consumer's own metadata. An unflagged `intent=in` read of the
   base name resolves to `%vars_layer(...)`; an unflagged `intent=in` read
   of `tendency_of_<X>` resolves to `%vars_layer_tend(:, index_of_<X>)`
   — the same column a tendency *producer* wrote. **Host/earlier-suite
   provision wins**: if the host declares the name, or an earlier scheme
   already produced it as an ordinary variable, normal host/suite
   resolution takes over. (Worked example: in the CAM-SIMA `cam7` suite
   the convection/stratiform schemes write `tendency_of_water_vapor_...`
   as a flagged constituent tendency, and the unflagged `sima_diagnostics`
   schemes read it back via this rule — see §4.15.)
4. **Mismatched combinations are errors** — a constituent-FLAGGED
   `intent=out` arg whose name is not a `tendency_of_*` is a codegen-time
   error: physics phases may only PRODUCE tendencies; new base
   constituents must be declared in the register phase.

### 2.3 Two registration sources (no auto-clone)

- **Host**: hand-written `host_constituents(:)`, passed into
  `ccpp_register_constituents(host_constituents, instance_number, ...)`.
- **Suite-dynamic**: register-phase scheme args, accumulated into a
  per-suite buffer `<suite>_dynamic_constituents(:)` by `<suite>_register`,
  drained into `ccpp_model_constituents_obj(inst)` by
  `ccpp_register_constituents`.

The auto-clone-from-metadata path is **gone from capgen's default
behaviour**.  If a scheme declares `advected=true` on an arg but no
source registers that standard name, capgen emits a runtime check
during `ccpp_initialize_constituents` that errors with the missing
name.

**Legacy escape hatch** (added 2026-05-21): the opt-in CLI flag
`--legacy-auto-clone-constituents` reinstates the original
auto-clone path for hosts whose scheme metadata predates explicit
registration (production CAM-SIMA's atmospheric_physics tree is the
immediate consumer).  This is a transient migration shim — see
`doc/auto_clone_constituents.md` for the full reference and
removal procedure.  It is single-instance only and explicitly
flagged so future capgen work is *not* expected to keep it
indefinitely.  The reform proposals in §6–§8 below are unchanged by
the shim's existence: capgen's chosen architecture is still
explicit registration.

### 2.4 Per-instance state

Everything is per-instance:

```fortran
type(ccpp_model_constituents_t), allocatable :: ccpp_model_constituents_obj(:)
                                ! indexed by instance_number
```

All host-facing entry points take `instance_number`:

```
ccpp_register_constituents   (host_constituents, instance_number, errflg, errmsg)
ccpp_initialize_constituents (ncols, num_layers, instance_number, errflg, errmsg)
ccpp_number_constituents     (num_flds, advected, instance_number, errflg, errmsg)
ccpp_gather_constituents     (const_array, instance_number, errflg, errmsg)
ccpp_update_constituents     (const_array, instance_number, errflg, errmsg)
ccpp_const_get_index         (stdname, const_index, instance_number, errflg, errmsg)
ccpp_constituents_array      (instance_number)         => pointer
ccpp_advected_constituents_array (instance_number)     => pointer
ccpp_model_const_properties  (instance_number)         => pointer
ccpp_deallocate_dynamic_constituents (instance_number, ...)
```

`ccpp_is_scheme_constituent(var_name, ...)` and the
`ccpp_model_const_stdnames(:)` parameter array are NOT per-instance —
the standard-name catalog is identical across instances.

### 2.5 Lifecycle

```
ccpp_register(suite_name, instance_number, ...)
   └─ <suite>_register → packs scheme-dynamic constituents into
                          <suite>_dynamic_constituents(instance)%items
                          (per-instance wrapper-DDT array; each instance
                          allocates and fills its own slot — see §4.13)
                          ↓
ccpp_register_constituents(host_constituents, instance_number, ...)
   └─ initialize_table(num_host_consts + num_suite_consts)
   └─ new_field(host_consts ...)
   └─ new_field(<suite>_dynamic_constituents ...)
   └─ lock_table
                          ↓
ccpp_initialize_constituents(ncols, num_layers, instance_number, ...)
   └─ lock_data (allocates vars_layer, vars_layer_tend, vars_minvalue)
   └─ ccpp_initialize_constituent_ptr (first instance wins; documented limit)
   └─ %const_index('<X>') for each enumerated constituent
   └─ post-lookup int_unassigned check → clear error message
                          ↓
ccpp_init(suite_name, instance_number, ...)
   └─ <suite>_init → binds module-level pointers
                          ↓
... physics phases ...
                          ↓
ccpp_final(suite_name, instance_number, ...)
   └─ <suite>_final → nullifies + last-to-leave deallocates
                          ↓
ccpp_deallocate_dynamic_constituents(instance_number, ...)
   └─ ccp_model_constituents_obj(inst)%reset
                          ↓ (in <suite>_final, last-to-leave)
   deallocate(<suite>_dynamic_constituents)
```

### 2.6 What's good

- Explicit. Every constituent registration is visible in someone's
  Fortran source.
- Multi-instance from day one.
- The "four rules" are small enough to fit on a slide.
- Resolver-time + codegen-time + runtime checks catch the most common
  mistakes.

### 2.7 What's still painful

Covered in §4.

---

## 3. What CAM-SIMA actually needs (audit)

### 3.1 Scheme-side registration usage

We audited `EXT/cam-sima/atmospheric_physics/schemes/` for use of the
register-phase `ccpp_constituent_properties_t(:)` pattern:

| Scheme | File | Registers |
|---|---|---|
| RRTMGP constituents | `schemes/rrtmgp/rrtmgp_constituents.meta` | radiative-active species |
| MUSICA chemistry | `schemes/musica/musica_ccpp.meta` | chemical species from MUSICA |
| Prescribed aerosols | `schemes/chemistry/prescribed_aerosols.meta` | aerosol species |
| Prescribed ozone | `schemes/chemistry/prescribed_ozone.meta` | ozone |

**Total: 4 of 128 schemes** in the atmospheric_physics tree use
scheme-side registration. The other 124 only **consume** constituents
(`advected=true` + `intent=in/inout` in metadata, accessed via the
framework's `vars_layer`).

This is a small enough number that an alternative "host-only
registration" model is feasible: move those 4 register calls into the
host (or into helper modules the host calls), and the rest of the
catalog only consumes.

### 3.2 Host-side patterns

`EXT/cam-sima/CAM-SIMA/src/physics/utils/cam_constituents.F90` wraps
the framework setters and exposes:

- `const_set_thermo_active(const_obj | const_ind, value)`
- `const_set_water_species(const_obj | const_ind, value)`
- `const_set_minimum(...)`

CAM-SIMA actively **calls these setters at runtime** — schemes don't
supply `thermo_active` at instantiate time; the host configures it
afterwards. This is direct evidence that the "post-instantiation
override" pattern is real and used today, and that the framework's
setter API is load-bearing.

### 3.3 What CAM-SIMA does **not** do

- It does not rely on auto-clone for `diag_name`. The scheme-side
  register calls in the 4 schemes do supply `diag_name`, but those
  values are CAM-SIMA's; a different host would need different ones.
- It does not use `ccpp_constituent_index` (the
  `ccpp_scheme_utils`-singleton-based lookup) extensively — most
  access goes through the framework's `index_of_<X>` integers.

### 3.4 What CAM-SIMA's host-cap-owned constituent object looks like

Because original capgen generates **one** `ccpp_model_constituents_obj`
per generator invocation, and CAM-SIMA uses one generator invocation per
executable, CAM-SIMA effectively runs single-instance today. A
multi-instance CAM-SIMA (sub-columns, ensembles) would expose the
single-global limitation immediately.

---

## 4. Bugs and design flaws

This section lists known issues across the three layers (framework,
original capgen, capgen). Items marked **(FIXED)** were resolved
2026-05-12 and either are or will be PRs; items marked **(OPEN)** are
intentionally left for this discussion.

### 4.1 Framework: `ccpt_deallocate` ownership bug (FIXED in capgen tree, needs upstream PR)

- **Location**: `src/ccpp_constituent_prop_mod.F90`, `ccpt_deallocate`
  + `ccpt_set`.
- **Symptom**: `free(): invalid size` crash when
  `ccp_model_const_reset` is called on a properly-locked table whose
  entries came from pointer-assigned targets (the common pattern
  under capgen's explicit registration; also potentially under
  original capgen's `host_constituents` path).
- **Root cause**: `ccpt_set` does pointer assignment (`this%prop =>
  const_ptr`); `ccpt_deallocate` does an unconditional
  `deallocate(this%prop)`. The deallocate is correct only when the
  caller allocated `const_ptr` on the heap and transferred ownership.
- **Why it didn't surface earlier**: original capgen's advection test
  only calls `deallocate` once between a *failing* register and a
  *successful* one — at that point `lock_table` has not populated
  `const_metadata`, so the broken inner loop is skipped. Capgen
  triggers it because its teardown calls `reset` after a successful
  lock.
- **Fix landed 2026-05-12**: added `framework_owns_me` private flag on
  `ccpp_constituent_properties_t` (default `.false.`) with
  `is_framework_owned()` getter and `set_framework_owned(value)`
  setter; `ccpt_deallocate` now only deallocates when the flag is set.
  Original capgen's auto-clone path in `scripts/constituents.py`
  updated to call `set_framework_owned(.true.)` after `allocate`.
  Diffs in `src/ccpp_constituent_prop_mod.F90` (and capgen's
  parallel copy) + `scripts/constituents.py`.
- **Status**: framework tests pass; capgen unit-test suite (1127 passing
  as of 2026-05-13) is green.  Still needs upstream PR to ccpp-framework +
  original ccpp-capgen.

### 4.2 Framework: missing setters (OPEN)

| Property | Optional in `%instantiate`? | Has setter? | `is_match`-checked? |
|---|---|---|---|
| `std_name`         | required | — | (lookup key) |
| `long_name`        | required | — | no |
| `diag_name`        | required | **NO** | no |
| `units`            | required | — | **yes** |
| `vertical_dim`     | required | — | no |
| `advected`         | optional (default .false.) | **NO** | **yes** |
| `default_value`    | optional | **NO** | no |
| `min_value`        | optional | `set_minimum` | no |
| `molar_mass`       | optional | `set_molar_mass` | no |
| `water_species`    | optional (default .false.) | `set_water_species` | **yes** |
| `mixing_ratio_type`| optional | **NO** | no |
| `thermo_active`    | not in instantiate | `set_thermo_active` | **yes** |
| `const_index`      | internal | `set_const_index` | no |

**Pain points**:

- `advected` is `is_match`-checked AND has no setter. Once registered,
  immutable. If a scheme and the host disagree, you get the
  "incompatible constituent" error and you cannot reconcile from
  Fortran.
- `diag_name` is required (cannot be omitted at instantiate) AND has
  no setter. A scheme must pick a value at registration time; that
  value is then frozen.
- `default_value` is silently optional. If omitted, the constituent
  array initializes to `huge(real)` and downstream comparisons fail
  in surprising ways (we burnt half a day on this 2026-05-12).
- `thermo_active` is the only property in the "post-instantiate-only"
  shape: it has a setter but isn't a `%instantiate` arg. The
  asymmetry is confusing.

### 4.3 Framework: `is_match` is too strict (OPEN)

`is_match` (in `ccp_is_match`) checks `units`, `advected`,
`thermo_active`, `water_species`. Three of those four (`advected`,
`thermo_active`, `water_species`) are properties the host legitimately
overrides post-registration. Two registrations of the same `std_name`
with the same `units` but different `advected` should be a
duplicate-dedup (host wins), not a hard error.

### 4.4 Framework: `diag_name` portability problem (OPEN)

Diagnostic output names are host-specific. CAM-SIMA names cloud
liquid mixing ratio `CLDLIQ`; UFS would call it something else. Yet
`%instantiate` makes `diag_name` a *required* arg, forcing schemes to
either:

- Pick a host-specific value (couples the scheme to a host), or
- Pick a "neutral" default that no host's diagnostic tooling
  recognizes.

The current de-facto pattern in CAM-SIMA scheme code is to pick a
CAM-SIMA-flavoured value and ship it. Any port to UFS would need to
either monkey-patch or fork the scheme.

A clean fix:
1. Make `diag_name` optional at `%instantiate` (default to empty
   string or `std_name`).
2. Add `set_diagnostic_name(value)` setter.
3. Host overrides per-registration after `ccpp_register_constituents`.

### 4.5 Original capgen: implicit registration (OPEN — observation)

The auto-clone path is generator magic. Reading scheme metadata
doesn't tell you whether the scheme's args result in registration; you
have to know that `advected=true` triggers it. This is a documentation
+ comprehension problem more than a bug.

### 4.6 Original capgen: single-instance `ccpp_model_constituents_obj` (OPEN — limitation)

The host cap declares one global. Multi-instance hosts would need to
either generate one cap per instance or restructure.

### 4.7 Original capgen: `ConstituentVarDict` complexity (OPEN — observation)

The synthetic scope between suite and host serves correctness but
adds a code path that most contributors don't read. If we drop it
(capgen has), the variable-matching algorithm shrinks.

### 4.8 Capgen: `_FRAMEWORK_CONST_DIM_INPUTS` cleanup (LANDED 2026-05-13)

`generator/host_cap.py` no longer carries the hand-curated frozenset of
standard names; framework-constituent dimension references now ride on a
dedicated `used_const_dim_std_names` field on `ResolvedArg`.  Closes the
"hand-curated → structured field" REVISIT note that was in the code.

### 4.9 Capgen: no codegen-time cross-check of scheme registration (OPEN)

The resolver knows every `is_constituent` arg's standard name (in
`SuiteResolution.constituent_index_names`) but doesn't know what each
scheme's `_register` subroutine actually `%instantiate`s. Today's
guarantee is a runtime check (the `int_unassigned` validation we
added 2026-05-12). Stronger options:

- (a) New metadata attribute `registers_std_names = a, b, c` on
  register-phase tables; codegen errors at generation time.
- (b) Parse scheme `_register` Fortran for `%instantiate(std_name=…)`
  calls and cross-check.
- (c) Keep runtime check as authoritative, document the gap.

### 4.10 Capgen: scheme-metadata `diagnostic_name` for is_constituent args is host-specific (OPEN)

Same issue as §4.4 but in capgen's metadata layer. Today's
`diagnostic_name` attribute on a scheme metadata arg flows into
`datatable.xml` and is then trusted as "the" diagnostic name. If we
adopt setter-based class-B overrides, this attribute should either be
dropped for constituent args or marked as a default-only hint.

### 4.11 Capgen: `ccpp_scheme_utils` singleton (OPEN — documented limit)

`ccpp_initialize_constituent_ptr(const_obj)` stores a single module-level
pointer. Schemes that use `ccpp_constituent_index(stdname)` get that
pointer back. Under multi-instance, only the first instance's
pointer is retained — `ccpp_constituent_index` queries from
within a scheme will always reflect instance 1. CAM-SIMA's 4
scheme-registering schemes don't rely on this; documented in
`doc/constituents.md` §8. Real fix requires either threading
`instance_number` through `ccpp_constituent_index` (interface
change) or maintaining a per-instance pointer table.

### 4.12 Capgen: drop `diagnostic_name_fixed`, keep only `diagnostic_name` (OPEN — proposed simplification)

Today the metadata layer carries two mutually-exclusive scheme-arg
attributes:

- `diagnostic_name = X` — emits `diagnostic_name="X"` in `datatable.xml`;
  defaults to `local_name` when absent.
- `diagnostic_name_fixed = Y` — emits `diagnostic_name_fixed="Y"` in
  `datatable.xml`; the `diagnostic_name` slot stays empty (no
  auto-default to `local_name`).

The behavioral difference is purely *which attribute name* host
tooling sees in `datatable.xml` — both attributes carry the same kind
of value (a Fortran-identifier-shaped string), and both are passed
through unmodified. `_fixed` is a signal to the host "use verbatim, do
not decorate or transform"; but `diagnostic_name = X` already means
exactly that — the cap code never decorates the value, and any host
tooling that wants to decorate would have to opt in by parsing a
separate attribute (or by syntactic convention on the value itself).

**Proposal:** Remove `diagnostic_name_fixed` from the metadata layer
and the parser. Keep `diagnostic_name` with the existing defaulting
rule (explicit → use it; absent → fall back to `local_name`).  Hosts
that today rely on the `_fixed` semantic ("don't auto-default to
`local_name`") get the same outcome by simply *setting*
`diagnostic_name` to the desired exact value.

Touchpoints to retire:

- `metadata/parse_tools/parse_checkers.py::check_diagnostic_fixed` and
  the mutual-exclusion block at the top of `check_diagnostic_id`.
- `metadata_table.py::MetaVar._KNOWN_ATTRS` entry and the
  `@property diagnostic_name` fallback that returns `''` when
  `_diagnostic_name_fixed` is set.
- `generator/datatable.py:267-269` emission of the
  `diagnostic_name_fixed` XML attribute.
- Existing unit-test coverage for `diagnostic_name_fixed` becomes
  obsolete and is removed (not migrated).

**Why it's worth doing as part of the overhaul:** the attribute has no
unique semantics that `diagnostic_name` can't express, and dropping it
shrinks the metadata-layer surface area at the same time the
`set_diagnostic_name(value)` framework setter (§4.4 / §4.10) is being
added on the framework side. Hosts that want runtime override get
`set_diagnostic_name`; hosts that want metadata-declared values get
`diagnostic_name`. There is no third use case that needs `_fixed`.

**Risk:** non-CCPP-ng metadata in the wild may carry
`diagnostic_name_fixed`. Mitigation: a one-line legacy-mode rewrite
(`metadata/legacy_compat.py`) translates the deprecated attribute to
`diagnostic_name` at parse time with a loud warning, identical in
spirit to the existing `horizontal_loop_extent → horizontal_dimension`
shim. Remove the rewrite once known consumers are migrated.

### 4.13 Capgen: per-suite `dynamic_constituents` buffer was shared across instances (FIXED 2026-05-18)

- **Location**: `capgen/generator/host_constituents.py` (buffer
  declaration + `ccpp_register_constituents` iteration);
  `capgen/generator/suite_cap.py::_register_lines` (the two-pass
  count→allocate→pack inside `<suite>_register`).
- **Symptom**: with two or more instances and any register-phase
  scheme that produces constituents, the second per-instance
  `ccpp_register_constituents` call fails with `ccp_set_const_index
  ccpp_constituent_properties_t const index is already set`.
- **Root cause**: the per-suite buffer
  `<suite>_dynamic_constituents(:)` was declared as a single shared
  1-D array of `ccpp_constituent_properties_t`, filled exactly once on
  first instance entry (`.not. allocated(buf)` gate).
  `ccpp_register_constituents` then iterates that shared buffer per
  instance and calls `%new_field(const_prop)` on each property
  object.  `%new_field` calls `ccp_set_const_index`, which **mutates
  the property object** by writing `const_ind`.  Instance 1 set
  `const_ind` on every shared object; instance 2's call tripped the
  "set exactly once" guard.
- **Latent companion bug**: the same shared-mutation pattern means
  that once Proposal B's class-B setters (`set_advected`,
  `set_diagnostic_name`, `set_water_species` per-instance, etc.) are
  exercised, instance 1's setter call would silently corrupt instance
  2's view of the property.  No "already set" guard exists on those
  setters today.
- **Why it didn't surface earlier**: the advection end-to-end test is
  single-instance; the instances end-to-end test has no constituents.
  Surfaced by the new `instances_advection` combined test
  (`end-to-end-tests/instances_advection/`) on first run.
- **Fix landed 2026-05-18**: the per-suite buffer is now a wrapper-DDT
  array indexed by `instance_number`:
  ```fortran
  type :: ccpp_dyn_const_buffer_t
    type(ccpp_constituent_properties_t), allocatable :: items(:)
  end type
  type(ccpp_dyn_const_buffer_t), allocatable, target :: <suite>_dynamic_constituents(:)
  ```
  The outer array is allocated to `number_of_instances` on first call;
  each instance independently runs the two-pass count+pack into its
  own `%items` slot.  `ccpp_register_constituents` iterates
  `<suite>_dynamic_constituents(instance)%items` so each instance's
  `new_field` calls operate on **distinct** property objects.
  Scheme `_register` routines are now called N times instead of once
  (negligible cost — typical register bodies are a few `%instantiate`
  calls), in exchange for clean per-instance isolation.
- **Cost**: ~50 lines across the two generator emitters, plus updates
  to six pinned unit tests.  No CAM-SIMA / NEPTUNE / SCM coordination
  needed (host-facing API unchanged).
- **Status**: framework tests pass; full unit-test suite
  (1319 tests at fix landing, 1426 as of 2026-06-01) is green; all 10
  end-to-end tests pass.
- **Position relative to Proposals A/B/C**: orthogonal — none of the
  three proposed touching the buffer.  Independently adopted.

### 4.14 Capgen: error-output keyword inconsistency across emitted public API (OPEN — observation)

- **Location**: `capgen/generator/host_cap.py:370,446,*` (lifecycle
  subs) vs `capgen/generator/host_constituents.py` (the entire
  constituent wrapper family).
- **Symptom**: the public Fortran argument carrying the CCPP error
  flag does not have a consistent name across the cap's surface area.
  - **Lifecycle subs** (`ccpp_register`, `ccpp_init`, `ccpp_physics_*`,
    `ccpp_final`, plus the five `ccpp_physics_suite_*` introspection
    routines) read the host's `ccpp_error_code` control-var
    `local_name` from `host_dict` and use *that* as the public arg
    name.  A host that calls `[errflg]` `errflg` ends up with
    `subroutine ccpp_register(suite_name, errflg, errmsg)`; a host
    that calls `[errcode]` `errcode` ends up with
    `subroutine ccpp_register(suite_name, errcode, errmsg)`.  This is
    the host-controlled side.
  - **Constituent wrappers** (`ccpp_register_constituents`,
    `ccpp_initialize_constituents`, `ccpp_is_scheme_constituent`,
    `ccpp_number_constituents`, `ccpp_gather_constituents`,
    `ccpp_update_constituents`, `ccpp_const_get_index`) hard-code
    `errcode` regardless of what the host declared.  As of
    2026-06-03 the hard-code is `errcode` (renamed from `errflg` for
    consistency with the framework methods on
    `ccpp_constituent_properties_t` and `ccpp_model_constituents_t`,
    which all expose `errcode=`).  Before 2026-06-03 it was `errflg`,
    which broke any host whose control-var convention was
    `errcode` -- including the CAM-SIMA build.
- **Resulting cross-cutting hazard**: in a host where the
  `ccpp_error_code` local name happens to be `errflg`, the caller
  writes
  ```fortran
  call ccpp_register(suite_name, errflg=errflg, errmsg=errmsg)               ! host-name keyword
  call ccpp_register_constituents(host_consts, errcode=errflg, errmsg=errmsg) ! hardcoded keyword
  ```
  Two different keyword names for the same conceptual argument on
  adjacent calls.  Confusing but compiles; the host's local variable
  is bound by name to whichever keyword the callee defines.
- **Why the constituent wrappers are hardcoded**: the wrappers are
  thin shims around framework methods
  (`ccpp_model_constituents_t%new_field`, `%lock_table`,
  `%num_constituents`, etc.) that all take `errcode=` per
  `capgen/src/ccpp_constituent_prop_mod.F90`.  Hardcoding `errcode`
  on the wrapper means the wrapper body just forwards
  `errcode=errcode` instead of `errcode=<host_local_name>` -- one
  less host-dict lookup, but at the cost of breaking the
  "host names what they want" contract.
- **Options to resolve**:
  - (a) Plumb the host's `ccpp_error_code` local name through
    `host_constituents.py` the same way `host_cap.py` does (via
    `_ctrl_local(host_dict, 'ccpp_error_code') or 'errcode'`).
    The constituent wrappers' public arg then tracks the host's
    convention.  Adds 1 dictionary lookup per emitted sub; no
    other change.
  - (b) Standardise the lifecycle subs on `errcode` too, ignoring
    the host's `ccpp_error_code` local name.  Simpler internally
    but breaks every existing host that ships
    `[errflg] standard_name = ccpp_error_code`.
  - (c) Status quo (the constituent wrappers' `errcode` hardcode):
    document it loudly and live with the cross-API split.
- **Status**: currently option (c).  The post-rename build of CAM-SIMA
  works because CAM-SIMA's caller code uses `errcode=errflg` (passing
  its local var `errflg` to the hardcoded keyword `errcode`).  Hosts
  with the opposite convention (`[errcode] standard_name =
  ccpp_error_code` -> lifecycle subs expose `errcode=`,
  constituent wrappers also expose `errcode=`) coincidentally see
  consistent keywords today; the hazard is invisible for them.
- **Recommended fix**: option (a).  Lines up with the lifecycle
  emitter's already-established host-driven pattern.  Trivial
  implementation cost; eliminates the cross-cutting confusion for
  any host whose `ccpp_error_code` local name is not `errcode`.

### 4.15 CAM-SIMA compat layer: `write_init_files` mis-flagged unflagged constituent-tendency consumers (FIXED 2026-06-05)

- **Location**: `cime_config/capgen_compat/_var_wrapper.py` in CAM-SIMA
  — the facade that lets capgen drive CAM-SIMA's *unchanged*
  `write_init_files.py` / `cam_autogen.py` — method
  `_VarWrapper.from_resolved_arg`.
- **Symptom**: the `se_cslam` (FCAM7 `cam7`) build failed AFTER cap
  generation, inside CAM-SIMA's own init-file generator:
  `Error: Missing required host variables:
  tendency_of_water_vapor_mixing_ratio_wrt_moist_air_and_condensed_water`.
- **Mechanism**: in `cam7` the convection/stratiform schemes (`dadadj`,
  `zm_conv_evap`, `rk_stratiform`, `zm_convr`,
  `cloud_particle_sedimentation`) write that tendency as a FLAGGED
  constituent tendency (`constituent=true intent=out`) → capgen routes
  them to `%vars_layer_tend` (`source='constituent'`, NOT recorded in
  `suite_vars`) and the name enters `const_stds`.  The four
  `sima_diagnostics` schemes read it back `intent=in` UNFLAGGED → rule b
  (§2.2.3) → `source='constituent'`, but `ResolvedArg.is_constituent` is
  taken from the consumer's OWN flag = `False`.  The compat wrapper
  derived `advected`/`constituent` only from
  `is_constituent`/`is_constituent_arg` → both `False`, and
  `source='constituent'` was not in its suite-internal drop set, so
  `write_init_files.gather_ccpp_req_vars` saw intent=in + not-constituent
  + not-in-host-dict → "missing host variable".
- **Fix**: key the wrapper's `advected`/`constituent` on
  `arg.source == 'constituent'` (a strict superset of the two old flags).
  `write_init_files` skips constituents from BOTH USE-import and the
  initial-conditions read — the constituents object supplies them at
  runtime — so flagging the tendency consumer is correct, not a mask.
  Verified 43/43 `capgen_compat` + 16/16 `test_write_init_files`, then
  confirmed by full `se_cslam` runs to completion under both gnu and
  intel (bit-comparable results).
- **Takeaway**: `ResolvedArg.is_constituent` answers "did the SCHEME flag
  it"; `source == 'constituent'` answers "is this supplied by the
  constituents framework".  Any host adapter (the CAM-SIMA compat layer
  today, any future one) must key constituent handling on the *source*,
  because rule-b inferred consumers legitimately carry
  `is_constituent == False`.
- **Position relative to Proposals A/B/C**: orthogonal — a host-adapter
  bug exposed by rule b, not a framework constituent-model change.

---

## 5. Property classification (Class A vs Class B)

Proposed in `design_constituents_mutability.md` 2026-05-12. Each
constituent property is conceptually owned by either the scheme
(physics-portable, immutable once instantiated) or the host
(host-configuration, mutable post-instantiation).

### Class A — scheme-intrinsic (immutable)

| Property | Why class A |
|---|---|
| `std_name`        | Identity. Cannot change. |
| `long_name`       | Human-readable name of the *species*. Not host-specific. |
| `units`           | Physics correctness. `is_match`-checked. |
| `vertical_dim`    | Scheme's structural expectation (interface vs layer). |
| `molar_mass`      | Physical constant of the species. |
| `default_value`   | (Debatable — see §7) Scheme-appropriate initial value. |

### Class B — host-configuration (mutable post-instantiation)

| Property | Why class B |
|---|---|
| `advected`        | Whether the host's dycore advects this — host decision. |
| `diag_name`       | Host-specific diagnostic system name. |
| `thermo_active`   | Host model configuration. |
| `min_value`       | Host runtime guardrail. |
| `water_species`   | (Borderline — see §7) Physical classification but also host-config. |
| `mixing_ratio_type` | (Borderline — see §7) Depends on dycore convention. |

### Consequences if adopted

- `is_match` should check **only class A**. Today it checks 3 of 4
  class-B properties.
- Class B properties need setters. Today
  `advected`, `diag_name`, (and `mixing_ratio_type` if it stays
  class B) have none.
- `%instantiate` can demote class B from "required + optional" to
  "all optional with sane defaults" — `diag_name=''`,
  `advected=.false.`, etc. Schemes wouldn't need to set them at all.

---

## 6. What to remove, replace, improve

### Remove (or stop requiring)

- **Scheme-metadata `diagnostic_name` on is_constituent args** — host
  will override. Keep the attribute valid on non-constituent args
  (where it's host tooling documentation, no portability issue).
- **`is_match` checks on advected / water_species / thermo_active** —
  class B should not block dedup.
- **The `diag_name` requirement at `%instantiate`** — demote to
  optional with `''` default.
- **(Not adopting)** Original capgen's auto-clone path. Already gone
  in capgen; this discussion does not propose bringing it back.
  Listed for completeness because the option is in memory.

### Replace

- **`ConstituentVarDict`** as a concept — capgen already runs
  without it. If the framework or future generator code references
  it, dropping is fine.
- **Single-global `ccpp_model_constituents_obj`** — capgen's
  per-instance array is the replacement. Original capgen could be
  retrofitted, but the priority depends on whether multi-instance
  enters the original capgen's roadmap.

### Improve

- **Add the missing setters**: `set_advected`, `set_diagnostic_name`,
  `set_default_value` (if `default_value` becomes class B),
  `set_mixing_ratio_type` (if class B).
- **Add a convenience routine** like
  `ccpp_get_constituent_props_by_std_name(stdname, instance_number, prop_ptr, errflg, errmsg)`
  so hosts can lookup a single constituent's property wrapper by
  name without iterating.
- **Codegen-time cross-check** of scheme `_register` calls vs
  metadata declarations (preferred: §4.9 option (a) — new
  `registers_std_names` attr).
- **Document the lifecycle** clearly. `doc/constituents.md` is
  ~960 lines; targeted additions for "register-then-override"
  workflow once the new setters land.
- **Capgen-internal cleanup** (LANDED 2026-05-13): replaced
  `_FRAMEWORK_CONST_DIM_INPUTS` with a `used_const_dim_std_names`
  field on `ResolvedArg`.

---

## 7. Open design questions

These are the calls we need to make in the meeting.

### Q1. `default_value` — class A or class B?

- **Class A argument**: the scheme knows what the species
  should be initialized to (zero for "starts empty"; small positive
  for "starts at background"); the host doesn't typically override.
- **Class B argument**: hosts may want non-default starting values
  (chemistry runs with prescribed initial profiles).
- **Today's reality**: framework has no setter, so it's de-facto
  class A. The advection-test issue 2026-05-12 surfaced because we
  removed the `default_value=0._kind_phys` from cld_liq.F90's
  scheme-side register and had no way to put it back; restoring it
  in the scheme fixed the test but cements the class-A treatment.
- **Recommendation**: leave class A for now. Revisit when a real
  host-override use case appears.

### Q2. `water_species` — class A or class B?

- The current `is_match` check on `water_species` treats it as
  identity-defining (class A semantics). But the actual *meaning* of
  the bit is mostly host bookkeeping ("does the dycore treat this as
  water?"). CAM-SIMA has a `set_water_species` wrapper and uses it.
- **Recommendation**: class B, with the caveat that schemes whose
  numerics depend on a constituent *being* water should declare that
  in metadata as a hard requirement (different mechanism — not the
  `is_match` machinery).

### Q3. `mixing_ratio_type` — class A or class B?

- The scheme's calculations assume `wrt_dry` or `wrt_moist`; this
  feels class A.
- But hosts using different dycores might want to interpret the
  same `std_name` differently — feels class B.
- **Recommendation**: class A. The mismatch should manifest as
  different `std_name`s (`cloud_liquid_dry_mixing_ratio` vs
  `cloud_liquid_wet_mixing_ratio`), not the same name with a runtime
  override. Need cam-sima input.

### Q4. After `is_match` relaxation: what happens on disagreement?

- If two registrations of the same std_name agree on class A but
  disagree on class B (e.g., `advected=.false.` from a scheme,
  `advected=.true.` from the host), the second registration's class
  B values should win without error. Effectively: the host overrides
  the scheme.
- Order matters: today the host appends *after* the dynamic
  constituents. Should we reverse so the host appends *first*?
  Probably not — the "first registration wins on class A; host
  setters override class B" model is conceptually clearer.
- **Recommendation**: silently dedup on matching class A; for class
  B disagreements, the *later* registration's class B values are
  ignored. Hosts use setters to override after registration
  finalizes.

### Q5. Should `%instantiate` accept class-B args at all?

- **Option Y**: keep `%instantiate` accepting class B args (with
  defaults). Schemes can supply them as hints; hosts can override.
  Backward-compatible.
- **Option N**: remove class-B args from `%instantiate`. Schemes
  *must* leave them to the host. Breaks the 4 cam-sima
  scheme-registering schemes.
- **Recommendation**: option Y. The cost of breaking 4 schemes for
  marginal clarity isn't worth it.

### Q6. `ccpp_scheme_utils` singleton

- Today: `ccpp_initialize_constituent_ptr(const_obj)` stores one
  pointer module-wide. First instance wins.
- Fix options:
  - (a) Maintain a per-instance pointer table; threading
    `instance_number` through `ccpp_constituent_index`.
  - (b) Document the limitation, route around it (no scheme uses
    `ccpp_constituent_index` under multi-instance — capgen
    already enforces `index_of_<X>` everywhere).
- **Recommendation**: (b). It's a one-line doc note and zero code
  change.

### Q7. The `_layer` suffix — was a parallel `_interfaces` storage ever intended? (raised 2026-06-25, walkthrough prep)

- **Observation.** The per-instance constituent object stores values and
  tendencies as `%vars_layer(:,:,:)` and `%vars_layer_tend(:,:,:)`
  (`src/ccpp_constituent_prop_mod.F90:167-168`), both allocated over the full
  constituent axis (`:1558`, `num_values()` = every registered constituent).
  The `_layer` qualifier in the names implies an anticipated **parallel
  interface storage** (`vars_interface` / `vars_interface_tend`) that was
  never added.
- **Evidence it was anticipated, not accidental.** The type carries
  `is_layer_var` (`:637`, tests `vertical_layer_dimension`) **and**
  `is_interface_var` (`:652`, tests `vertical_interface_dimension`)
  predicates — so the design already distinguishes layer- vs
  interface-located constituents, but only layer storage exists.
- **Latent gap.** A constituent declared on `vertical_interface_dimension`
  has no storage slot today; `is_interface_var` would return true but there
  is nowhere to put it. Whether any host/scheme actually needs interface
  constituents is unknown (CAM-SIMA audit in §3 did not surface one).
- **Questions for discussion.** (a) Was `_interfaces` intended and dropped, or
  is `_layer` just a (now-misleading) name? (b) Does any consumer need
  interface-level constituents? (c) If **no** → drop the `_layer` suffix to
  simplify; if **yes** → add the parallel `vars_interface` / `_tend` arrays and
  route `is_interface_var` constituents to them.

### Q8. Should a constituent always be a triplet — base + tendency + index? (raised 2026-06-25, walkthrough prep)

- **Today (per `constituents.md` four rules):** a constituent is **not** a
  forced triplet. It is *one registered base* (Rule 1, the only declaration
  path), *zero-or-more optional* `tendency_of_<X>` references (Rule 3 — a
  scheme emits one only if it has a tendency), and a *framework-derived*
  `index_of_<X>` (never user-declared; filled at init via `%const_index`).
- **But storage already half-implies the triplet.** `%vars_layer_tend` is
  allocated over the **whole** constituent axis (`:1558`), so every
  constituent has a tendency *column* whether or not any scheme writes it.
  So the "triplet" is already true at the **storage** level, but optional at
  the **metadata/registration** level.
- **Question for discussion.** Should registration *force* the triplet
  (declare base + tendency + index together, uniformly)?
  - *For:* uniform mental model; matches the storage; removes the "did anyone
    register a tendency?" ambiguity; could let the resolver validate
    tendency producers against registered bases.
  - *Against:* many constituents have no physics tendency (the column is
    already there regardless, so forcing a declaration buys little); the
    index is implicit by design and exposing it as a required member
    re-introduces the index bookkeeping capgen deliberately hid; the
    base is the only thing that *must* be registered.
  - *Open sub-question:* if not forced, should the resolver at least **warn**
    when a `tendency_of_<X>` is produced for an `<X>` that no register scheme
    declared? (relates to §4.9 — no codegen-time cross-check of registration.)

---

## 8. Three proposals — minimal / clean / deep

### Proposal A — bugfix only

**Scope**:
- Land the `ccpt_deallocate` ownership fix (done 2026-05-12).
- Update `scripts/constituents.py` for original capgen's auto-clone
  path to pass `owned=.true.` (done).
- Add the three missing setters (`set_advected`,
  `set_diagnostic_name`, `set_default_value`) without changing
  semantics. Doesn't touch `is_match` or `%instantiate`.
- Document the gaps in `doc/constituents.md`.

**Cost**: ~50 lines framework code + tests. No cam-sima changes
required.

**Benefit**: closes the immediate bug, gives hosts the override
mechanism they need today (specifically for `diag_name`), unblocks
the advection test's deferred-property pattern.

**Limit**: leaves `is_match` strict — hosts that disagree with a
scheme on `advected` still hit the "incompatible constituent" error.

### Proposal B — class A/B split + setters

**Scope** (in addition to A):
- Relax `is_match` to check only class A (`units` and possibly
  `mixing_ratio_type`).
- Make all class-B properties optional in `%instantiate` with sane
  defaults; deprecate (but keep accepting) class-B kwargs.
- Adopt the recommendation in Q4: silently dedup; host setters
  override.
- Update `doc/constituents.md` with the register-then-override
  workflow.
- (capgen) Reject `diagnostic_name` on `is_constituent=True`
  scheme args at parse time, or downgrade it to a default-only hint.
- (capgen) **DONE 2026-05-13**: replaced `_FRAMEWORK_CONST_DIM_INPUTS`
  with a `ResolvedArg.used_const_dim_std_names` field.

**Cost**: ~150 lines framework + ~50 lines capgen + tests.
CAM-SIMA host code can stay as-is (the 4 scheme-side registrations
continue to work with their existing class-B values; they're just
not enforced anymore). Optional: tidy the 4 schemes to pass class-A
only.

**Benefit**: physics schemes become genuinely portable across
hosts. The class-B override pattern that CAM-SIMA already uses for
`thermo_active` and `water_species` generalizes.

**Limit**: does not change the registration model (still
explicit-only in capgen, still auto-clone in original capgen).

### Proposal C — host-only registration

**Scope** (in addition to B):
- Move the 4 cam-sima scheme-side register calls into a CAM-SIMA
  helper module called from `cam_comp.F90`'s initialization.
- Drop register-phase `ccpp_constituent_properties_t(:)` support
  from capgen (and possibly original capgen). Schemes only
  consume constituents; only the host registers.
- Codegen-time enforcement: any `advected=true` scheme arg whose
  std_name is not in the host's enumeration → codegen error.
- Eliminates the `<suite>_dynamic_constituents` per-suite buffer
  entirely.

**Cost**: ~300 lines code total; requires coordinated PRs across
ccpp-framework, ccpp-capgen, ccpp-capgen, atmospheric_physics, and
CAM-SIMA. The 4 schemes need their `_register` routines deleted (or
made no-ops); the host needs a new helper.

**Benefit**: one source of truth for what constituents exist
(the host). Removes the auto-clone / scheme-register conceptual
overlap. Simplifies generator and runtime.

**Limit**: changes the contract for the 4 scheme authors. Risk of
breaking yet-undiscovered downstream users of the scheme-side
registration model.

### Comparison

| Aspect | A | B | C |
|---|---|---|---|
| Lines changed | ~50 | ~200 | ~500+ |
| Coordination needed | framework only | framework + capgen | framework + both generators + cam-sima |
| Fixes the crash | yes | yes | yes |
| Fixes `diag_name` portability | yes (host overrides) | yes | yes |
| Relaxes `is_match` | no | yes | yes |
| Removes scheme-side register | no | no | yes |
| Risk to existing CAM-SIMA workflows | none | low | medium |

### Recommendation

**Adopt A immediately (mostly done), aim for B over the next 4–6
weeks, table C until the framework PR for B is in and we have a
clearer signal on whether the scheme-side register pattern is worth
keeping.**

---

## 9. Appendix: framework setter inventory

(For reference during the meeting. Reproduced from
`design_constituents_mutability.md`.)

`ccpp_constituent_properties_t` methods (`src/ccpp_constituent_prop_mod.F90`):

```
Instantiation
  procedure :: instantiate     ! takes std_name, long_name, diag_name (REQUIRED),
                               !   units, vertical_dim, plus optional
                               !   advected, default_value, min_value, molar_mass,
                               !   water_species, mixing_ratio_type
  procedure :: deallocate

Getters (subset)
  procedure :: standard_name
  procedure :: long_name
  procedure :: diagnostic_name
  procedure :: units
  procedure :: vertical_dimension
  procedure :: is_advected
  procedure :: is_thermo_active
  procedure :: is_water_species
  procedure :: is_mass_mixing_ratio
  procedure :: is_volume_mixing_ratio
  procedure :: is_number_concentration
  procedure :: is_dry / is_moist / is_wet
  procedure :: minimum
  procedure :: molar_mass
  procedure :: default_value
  procedure :: has_default
  procedure :: is_framework_owned         ! NEW 2026-05-12

Setters (changes after instantiate)
  procedure :: set_const_index
  procedure :: set_thermo_active
  procedure :: set_water_species
  procedure :: set_minimum
  procedure :: set_molar_mass
  procedure :: set_framework_owned        ! NEW 2026-05-12
  procedure :: set_advected               ! GAP
  procedure :: set_diagnostic_name        ! GAP
  procedure :: set_default_value          ! GAP (or keep class A)
  procedure :: set_mixing_ratio_type      ! GAP (if class B)

Identity / equality
  procedure :: equivalent                 ! full equality
  procedure :: is_match                   ! checks units + (class-B props ← too strict)
```

`ccpp_constituent_prop_ptr_t` is the pointer wrapper. Has parallel
setters that delegate to the underlying `ccpp_constituent_properties_t`.

---

## Cross-references

- `doc/constituents.md` — capgen's user-facing constituents reference.
- `design_constituent_api.md` (memory) — capgen's per-instance option-A design.
- `design_constituents_mutability.md` (memory) — extended design notes incl. class A/B classification.
- `project_implementation_status.md` (memory) — current implementation state and deferred items.
- `scripts/constituents.py` — original capgen's host-cap generator.
- `src/ccpp_constituent_prop_mod.F90` — framework.
- `capgen/generator/host_constituents.py` — capgen's host-side module emitter.
- `capgen/generator/suite_resolver.py` (`_resolve_constituent_arg`) — capgen's resolver routing.
- `EXT/cam-sima/CAM-SIMA/src/physics/utils/cam_constituents.F90` — CAM-SIMA's host-side wrappers around framework setters.

