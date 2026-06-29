# capgen — Briefing for CCPP Framework Developers & Power Users

*Prepared for the 2026-05-14 walk-through; last revised 2026-06-05.
Companion document to `doc/migration.md` (the detailed migration
guide) and `doc/redesign_prompt.md` (the implementation spec).*

---

## 1. Why a new generator?

The CCPP Framework runs two code generators today:

- **`ccpp-prebuild`** — simple, procedural Python; fast; DDT-argument
  passing; in production use by NOAA UFS Weather Model, Navy NEPTUNE,
  and CCPP-SCM.  Reliable but feature-light.  No framework-owned
  variables.
- **`ccpp-capgen`** — complex, deeply object-oriented Python; flat-field
  argument passing; in use by NCAR CAM-SIMA.  Many advanced features
  designed but never implemented; at UFS/NEPTUNE scale (1200+ variables)
  flat-field passing prevents the use of strict error-checking flags
  (`-check all`, `-fcheck=all`) required for operational implementation,
  and even when it does compile it produces unmaintainably large source
  files; nobody on the team fully understands it.

**`capgen`** starts fresh, drawing lessons from both.  Guiding
principle: **simplicity of prebuild, feature set of capgen**.

What we wanted to fix:

1. **Flat fields → DDT arguments at all scales.**  No "flat-field
   group cap" failure mode.
2. **No scope-chain variable promotion.**  Variables flow through
   metadata, not through a runtime synthetic dictionary stacking.
3. **Code anyone can read and extend.**  No 10-deep class hierarchy.
4. **One generator, one CLI, one query tool** for both prebuild-style
   and capgen-style hosts.

---

## 2. What capgen is (in one paragraph)

capgen reads metadata for the **host model**, the **physics
schemes**, and the **suite definition files** (SDFs), produces a
small set of Fortran cap modules that bridge them, and writes a
`datatable.xml` describing the result for CMake / Make to consume.
At runtime the host calls a small set of public entry points
(`ccpp_register`, `ccpp_init`, `ccpp_physics_init`,
`ccpp_physics_run`, `ccpp_physics_*_init`/`_final`, `ccpp_final`); the
generated caps dispatch by `suite_name` (and optionally `group_name`)
to the right scheme.

---

## 3. Core concepts

### 3.1 Five metadata table types

| `type = `   | Owner            | How it reaches the cap     |
|-------------|------------------|----------------------------|
| `scheme`    | Physics scheme   | Intent args on scheme subs |
| `host`      | Host model       | Module USE (direct / DDT)  |
| `control`   | Framework runtime layer | `ccpp_physics_*` args |
| `ddt`       | Type definition  | Structural — fields only   |
| `suite`     | Generated suite cap     | Module USE          |

### 3.2 Three layers of generated cap

- **Static API** (`<host>_ccpp_cap.F90`) — public entry points; one per
  host build (filename and module name derived from `--host-name`).
  Dispatches by `suite_name` → suite cap.
- **Suite cap** (`ccpp_<suite>_cap.F90`) — per-suite state machine, plus
  dispatch by `group_name` → group cap.  Suite-owned interstitial data
  lives in a sibling `ccpp_<suite>_data.F90`.
- **Group cap** (`ccpp_<suite>_<group>_cap.F90`) — scheme call sites
  with full argument lists, unit/kind/vertical-flip transforms,
  optional-arg pointer wrappers, subcycle `do` loops.

### 3.3 Two-level integer state machine

Replaces both the boolean `initialized(:)` array from prebuild and
the string-based `ccpp_suite_state` from CAM-SIMA capgen.  Per
instance:

- **Suite-level**: `UNREGISTERED → REGISTERED → FRAMEWORK_INITIALIZED`.
- **Group-level**: `UNINITIALIZED → INITIALIZED → IN_TIMESTEP`.

### 3.4 Six scheme phases

`register`, `init`, `timestep_init`, `run`, `timestep_final`, `final`.
`register` is new — schemes that contribute to the constituent table
do so here.  `final` replaces the older `finalize` (breaking change,
intentional).

### 3.5 Variable resolution

For each scheme arg:

1. Found in host+control metadata → use the access path.  If units /
   kind / vertical orientation differ, generate a transform.
2. Not found, first use is `intent(out)` → **suite-owned** variable
   (interstitial); add to `ccpp_<suite>_data.F90`.
3. Not found, first use is `intent(in/inout)` → **error**.
4. Found in suite data (a prior scheme provided it) → use suite data
   access path.

### 3.6 Two tools, one parser

- `ccpp_capgen.py` — the code generator.  Trusts metadata; no
  Fortran parsing.
- `ccpp_validator.py` — the standalone Fortran-vs-metadata checker.
  The ONE place capgen parses Fortran.  Run by developers /
  CMake before generation.  Checks per-arg `intent`, `type`, `kind`,
  and dimension rank; character length must match exactly
  (`len=*`↔`len=*`, `len=N`↔`len=N`, no wildcard; old-style F77
  `character*N` / `character*(*)` parsed); DDT and
  `external:<module>:<typename>` types compare against the Fortran
  `type(name)` wrapper.  `optional` is asymmetric: metadata
  `optional=True` against a Fortran-required dummy is an error; the
  reverse is a warning.  See `doc/migration.md` §7 for the full
  rule table.

Both share the same metadata-parsing library (`metadata/`).

---

## 4. How capgen differs from `ccpp-prebuild`

| Topic                       | prebuild                          | capgen                                         |
|-----------------------------|-----------------------------------|---------------------------------------------------|
| Host metadata mechanism     | Hard-coded Python dict (`TYPEDEFS_NEW_METADATA`) | Regular `type = ddt` + `type = host` tables |
| Framework-owned variables   | Not supported                     | First-class (suite-owned interstitial via Case 2) |
| Constituents                | Hand-rolled, host-specific glue   | Standardized opt-in mechanism with auto-provision |
| `register` phase            | Doesn't exist                     | First phase; schemes declare dynamic constituents |
| Multi-instance API          | Implicit, ad-hoc                  | Paired-opt-in (`instance_number` / `number_of_instances`) |
| Subcycle loop counter       | Host plumbs it manually           | Registered std names `ccpp_loop_counter` / `ccpp_loop_extent` resolve to the do-loop locals automatically inside `<subcycle>` |
| Suite introspection        | Limited                           | Five runtime queries (`ccpp_physics_suite_list`, `_part_list`, `_schemes`, `_variables`, `_host_data`) |

---

## 5. How capgen differs from `ccpp-capgen`

| Topic                       | capgen                            | capgen                                         |
|-----------------------------|-----------------------------------|---------------------------------------------------|
| Group-cap arguments         | Flat fields (1200+ at UFS scale) | DDT arguments (as in prebuild)                    |
| Variable matching algorithm | Five-layer scope-chain promotion  | Flat host+control dict + suite-owned discovery (inherited from prebuild — primary reason runtime is comparable to prebuild) |
| External types (MPI f08 comm, ESMF clock) | Tabled (solution complexity) | First-class via `type = external:<module>:<typename>` |
| `type = module` in metadata | Yes                               | Renamed `type = host`                             |
| `is_constituent` scheme args | Auto-cloned by generator         | Schemes register constituents explicitly in the `register` phase via `ccpp_constituent_properties_t(:)`; original capgen's auto-clone path is available behind the opt-in `--legacy-auto-clone-constituents` shim for legacy hosts (single-instance only) |
| `ConstituentVarDict`        | Synthetic scope between suite + host | Removed; constituents are one of four sources (`control`/`host`/`suite`/`constituent`) on `ResolvedArg` |
| `<suite>_state` runtime check | String                          | Integer (named parameters)                        |
| Fortran-vs-metadata check   | Inside the generator              | Separate tool (`ccpp_validator.py`)               |
| Code complexity             | Deep OO hierarchy                 | Flat data classes + procedural resolver           |

---

## 6. Breaking metadata changes hosts must make

Comprehensive list — see `doc/migration.md` for full detail.

### 6.1 Table types

- `type = module` → **`type = host`**.

### 6.2 Phase names

- `<scheme>_finalize` → **`<scheme>_final`** in both metadata and
  Fortran source.

### 6.3 Standard names

- `horizontal_loop_extent` → **`horizontal_dimension`** uniformly in
  scheme metadata.  (The chunk-vs-full-domain distinction is driven
  by what the host passes for `horizontal_loop_begin` /
  `horizontal_loop_end`.)
- `number_of_openmp_threads` → **`number_of_threads`** (matches the
  `thread_number` control variable convention).

Both are rewritten on the fly by **`--legacy-mode`** for a transition
period; the shim prints a banner listing every rewrite it performs
and is marked for clean removal.

### 6.3b Transient migration shims — full set

Three opt-in CLI flags exist for migrating legacy hosts.  Each lives
in its own self-contained module, prints a loud banner at startup, and
every touchpoint is grep-tagged for clean removal:

| Flag                                  | What it does                                                                                                                                                                                                 | Removal grep                              |
|---------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------|
| `--legacy-mode`                       | Parse-time substitution of two deprecated CCPP standard names (see §6.3 above).  Active on both `ccpp_capgen.py` and `ccpp_validator.py`.                                                                  | `legacy-compat`                           |
| `--gfs-dim-aliases`                   | Treats GFS-physics names `adjusted_vertical_layer_dimension_for_radiation` and `vertical_composition_dimension` as equivalent to `vertical_layer_dimension` **inside the upper-bound dim identity check only** (the variables themselves stay distinct).  Resolver-only, so `ccpp_capgen.py` carries the flag; `ccpp_validator.py` does not (the validator never reaches the dim canonicaliser). | `dim-aliases`                              |
| `--legacy-auto-clone-constituents`    | Reinstates original ccpp-capgen's auto-clone-static-constituent registration path: every `is_constituent` consumer (`advected = True` / `constituent = True` / `molar_mass = …`) with no register-phase source is auto-registered into the per-suite dynamic-constituents buffer using values lifted straight from the scheme metadata.  Adds four legacy `%instantiate` kwargs to the parser (`default_value`, `min_value`, `water_species`, `mixing_ratio_type`).  **Single-instance only** — declaring the `instance_number` + `number_of_instances` pair while the flag is on is a hard error.  Available on both `ccpp_capgen.py` and `ccpp_validator.py`. | `auto-clone-constituents`                  |

All three flags are listed as runways, not destinations: drop the
underlying legacy spelling from host/scheme metadata and the flag can
be retired.  See `doc/auto_clone_constituents.md` for the full
auto-clone reference.

### 6.4 Required host `type = control` table

Every host MUST declare scalar integers (and one character) with
these CCPP standard names:

- `suite_name`, `horizontal_loop_begin`, `horizontal_loop_end`,
  `number_of_physics_threads`, `ccpp_error_code`, `ccpp_error_message`.

**Two paired-optional control pairs** — declare *both* members of a
pair (in `type = control`) or *neither*; declaring exactly one is a
hard error:

- `instance_number` + `number_of_instances` → opt into the
  multi-instance API.
- `thread_number` + `number_of_threads` → opt into the multi-threading
  API.

The two pairs are fully symmetric.  Declaring a pair makes the index a
per-call control argument; omitting it drops both args and the
framework uses literal `1` where the index would go.  A host variable
may be dimensioned by `number_of_instances` / `number_of_threads` only
when its pair is declared (otherwise the scalar-index collapse can't
find the index variable and errors).

### 6.5 DDT-instance variables with scalar-index dims

Container DDT-instance variables (`physics%Interstitial`,
`physics%Coupling`, ...) dimensioned by a count standard name
(`number_of_threads`, `number_of_instances`) get their scalar index
inserted **automatically** by capgen.  The host metadata declares
the dim; the generator emits
`physics%Interstitial(thread_number)%alpha(...)` at every call site.

The host's Fortran can keep its existing OpenMP-thread-private DDT
layout — no glue code needed on the host side.

### 6.6 Leaf variables MUST NOT carry registered scalar-index dims

Rule 2 of the registered-scalar-index-dimension contract: scalar
variables (real / integer / character / DDT-typed leaves the scheme
binds to) cannot declare `number_of_threads` or `number_of_instances`
as a dimension.  Wrap them in a container DDT instead.  This is
enforced at parse time with an explicit remediation message; existing
CCPP-physics, UFS-WM, and CAM-SIMA host metadata is already
compliant.

### 6.7 No more `cdata` / `ccpp_t` struct passing

The framework-owned bag-of-state struct is replaced by explicit
control-variable arguments to the public entry points.

---

## 7. What capgen does NOT support (yet)

### 7.1 Deferred — to be resolved in upcoming work

- **Constituents overhaul.**  Three reform proposals on the table
  (`doc/constituents_overhaul.md`); decision pending an upcoming
  meeting.  Pieces involved: framework setter additions
  (`set_advected`, `set_diagnostic_name`, `set_default_value`),
  `is_match` relaxation, Class A vs Class B property classification.
- ~~**Validator host-metadata check.**~~  **Landed 2026-06-01**:
  `ccpp_validator.py --host-files` validates `type = host` and
  `type = ddt` tables against the Fortran (`doc/migration.md` §7.4).
- **Codegen-time scheme-registration cross-check.**  Today's
  registration check is at runtime
  (`ccpp_initialize_constituents`).  Stronger options: new metadata
  attribute `registers_std_names = a, b, c` on register-phase
  tables; cross-check at codegen.
- **Nested-subcycle `ccpp_loop_counter` semantics.**  When a scheme
  inside a deeply nested subcycle asks for `ccpp_loop_counter`, it
  currently resolves to the **outermost** loop's counter.  None of
  the in-tree physics catalogs uses the inner-counter case.
- **`ccpp_datafile.py --host-files` repurpose.**  The current
  `--host-files` returns the generated host-API file; should be a
  filtered list of *input* host metadata files (parallel to the new
  `--scheme-files`).  Deferred.
- **`ccpp_host_constituents.F90` suppression** when no suite touches
  constituents (file is correct-but-empty under host-wins; should
  not be emitted at all).
- **Python linter / formatter pass.**  Pick `ruff`, apply across
  `capgen/`.

### 7.2 Intentionally NOT supported

- **`_finalize` phase spelling.**  Use `_final`.  No legacy-mode
  shim — rename in metadata + Fortran.
- **`type = module`.**  Use `type = host`.
- **Flat-field scheme call arguments** (capgen's failure mode).
- **`character(len=*)` as a DDT component** (Fortran disallows it;
  we error at parse time with a remediation pointing at
  `character(len=:)` deferred-length).
- **Multiple registration sources for the same constituent** with
  silent dedup.  Today's behavior is to error on conflict; the
  proposed reform sets a clear precedence rule (host-set Class B
  properties win) — pending the constituents-overhaul decision.
- **`ConstituentVarDict`** synthetic scope between suite and host.
  Gone for good.

---

## 8. Validation and error reporting

A deliberate design choice across capgen: **errors are loud,
specific, and actionable**.  Examples surfaced during the SCM
shake-down:

- Empty `units =` line → error names file, line, variable,
  attribute, raw value, AND inner reason.
- Scheme metadata file passed via `--scheme-files` but missing from
  the SDF → silently ignored (and dropped from `<dependencies>` so
  CMake doesn't compile orphan code).
- Scheme listed in the SDF but its metadata not supplied → single
  CCPPError listing every missing scheme + pointer to
  `--scheme-files`.  Replaces silent empty-cap emission.
- DDT-instance variable with a non-registered scalar-index dim AND
  flattenable fields → error shows the broken access pattern
  capgen WOULD have emitted and quotes the Fortran compiler
  error verbatim ("Component to the right of a part reference with
  nonzero rank must not have the POINTER attribute").
- Generated `case default` on `select case(suite_name)` /
  `select case(group_name)` → unknown suite or group at runtime
  produces a clear errflg + errmsg, not silent fall-through.

---

## 9. Build-system integration (capsule view)

```cmake
# In your CMakeLists.txt
set(SCHEME_METADATA_FILES   …list of .meta paths…)
set(HOST_METADATA_FILES     …list of host .meta paths…)
set(SUITE_FILES             …list of suite XML paths…)

# Validate before generation (developer step, optional in CI)
ccpp_validator(SOURCE_FILES   ${SCHEME_FORTRAN_FILES}
               METADATA_FILES ${SCHEME_METADATA_FILES})

# Run the code generator
ccpp_capgen(HOSTFILES   ${HOST_METADATA_FILES}
            SCHEMEFILES ${SCHEME_METADATA_FILES}
            SUITES      ${SUITE_FILES}
            HOST_NAME   ${HOST}
            OUTPUT_ROOT ${OUTPUT_ROOT})

# Pull the manifest from the datatable
ccpp_datafile(DATATABLE "${OUTPUT_ROOT}/datatable.xml"
              REPORT_NAME "--scheme-files")
set(SCHEME_FORTRAN_FILES ${CCPP_FILES})

ccpp_datafile(DATATABLE "${OUTPUT_ROOT}/datatable.xml"
              REPORT_NAME "--dependencies")
set(CAPGEN_DEPENDENCIES ${CCPP_FILES})

ccpp_datafile(DATATABLE "${OUTPUT_ROOT}/datatable.xml"
              REPORT_NAME "--capgen-files")
set(CAPGEN_FILES ${CCPP_FILES})

add_library(scm-ccpp STATIC
  ${CAPGEN_DEPENDENCIES}
  ${SCHEME_FORTRAN_FILES}
  ${HOST_FORTRAN_FILES}
  ${CAPGEN_FILES})
```

Regenerating on every CMake configure is cheap — `write_if_changed`
preserves mtimes when content hasn't changed, so `make` / `ninja`
don't rebuild downstream objects unless something actually moved.

---

## 10. Where things stand right now

- **Unit tests**: 1516 passing on `feature/capgen` (as of
  2026-06-05).
- **End-to-end tests passing** (12): `advection`,
  `advection_auto_clone`, `capgen`, `chunked_data`,
  `constituents_dim`, `ddthost`, `instances`, `instances_advection`,
  `nested_suite`, `opt_arg`, `suite_allocate`, `var_compat`.  The two
  newest — `constituents_dim` (a variable dimensioned by
  `number_of_ccpp_constituents`) and `suite_allocate` (suite-owned
  allocatable interstitials sized by a scheme-written dimension) — were
  added while hardening the CAM-SIMA HPC build.
- **Code size**: ~17.8k LOC of Python under `capgen/` (includes
  docstrings, inline comments, and the three transient shim modules)
  + ~18k LOC of unit/doctest under `unit-tests/`.  Still procedural,
  still flat data classes.
- **Three transient migration shims now live** (see §6.3b):
  `--legacy-mode` (2026-05-13), `--gfs-dim-aliases` (2026-05-21),
  and `--legacy-auto-clone-constituents` (2026-05-21).  Each is
  isolated in its own module + grep-tag so removal is a single
  cleanup pass once the underlying legacy spelling is gone from
  host/scheme metadata.
- **CCPP-SCM**: actively driving development — every build / runtime
  failure surfaced this month landed as a fix in capgen (rather
  than being patched around in the host).  Most of the `phys_ps` group
  now builds end-to-end via `--legacy-mode` + `--gfs-dim-aliases`.
  On 2026-05-20 the per-arg-attribute validator caught **67 real
  metadata/Fortran disagreements** in the SCM physics tree (12 missing
  `kind = kind_phys` + 42 intent mismatches + a mix of optional-flag
  and bare-`real` cases); all fixed.
- **Validator** now checks per-argument `intent`, `type`, `kind`, and
  dimension rank in addition to the original name/count check.
  Asymmetric `optional` rule, DDT + `external:<module>:<typename>`
  type normalisation, exact character-length match (no `len=*`
  wildcard; old-style F77 `character*N` parsed).
- **Resolver cross-metadata checks** (late 2026-05-20): host/scheme
  (and suite-owned-var first-writer/follow-on) consistency on type,
  rank, and per-position dimension entries.  Default lower bound has
  three equivalent spellings (bare `X`, `1:X`, `ccpp_constant_one:X`);
  other lower bounds stay distinct.  Numeric kind remains lenient
  (transform path).  See `doc/migration.md` §1.3.2.
- **Host `active` + scheme arg shape**: when the scheme arg is
  optional, the cap uses pointer association (PRESENT()-aware); when
  the scheme arg is non-optional, the cap emits a runtime guard
  (`if (.not. (active)) errflg = 1; return`) before the call.
  Replaces an earlier static rule that forced scheme metadata to lie
  about optionality.  See `doc/migration.md` §1.3.1.
- **`--no-host-introspection`** (2026-05-14): stubs the bodies of
  the five suite-introspection routines in `<host>_ccpp_cap.F90`,
  shrinking the file from ~33k lines to ~800 for the 10-suite SCM
  build (the introspection case-blocks were making even `-O1`
  compilation effectively hang).  Signatures stay so existing host
  callers still link; stubbed bodies return `errflg = 1` with a clear
  `errmsg`.
- **NEPTUNE**: cleanup and acceptance testing in progress.
  Regular/lower-atmosphere physics builds and runs and produces
  results within tolerance (deviations similar to compiler changes).
  High-altitude physics testing is next.
- **UFS Weather Model**: not yet attempted; SCM is the proving
  ground first.  An anticipated complication is the "fast physics"
  called directly from the FV3 dynamical core as a separate group.
- **CAM-SIMA**: **reconnected (2026-06-03 → 06-05).**  capgen now
  drives the real CAM-SIMA build on Derecho via a thin compatibility
  layer (`cime_config/capgen_compat/`, in the CAM-SIMA tree) that
  re-implements original ccpp-capgen's Python API surface
  (`cap_database`, `host_model_dict`, `call_list`, the per-variable
  `Var` accessors) on top of capgen's `datatable.xml` +
  `ResolvedArg` / `HostVarEntry`.  CAM-SIMA's `cam_autogen.py`,
  `generate_registry_data.py`, and `write_init_files.py` are unchanged.
  Three cases build **and run to completion** on Derecho under **both
  gnu and intel** (bit-comparable results): `kessler`, `rrtmgp`, and
  `se_cslam` / CSLAM (the FCAM7 `cam7` suite — the full convection +
  stratiform + radiation + gravity-wave physics).  A short shareable
  brief on the compatibility layer is `doc/capgen_compat_layer.md`.
  `--legacy-auto-clone-constituents` is still the no-decision-needed
  bridge for the ~16 schemes that rely on auto-clone.  Bring-up this
  week produced three reusable lessons baked into the docs: the
  consume-without-re-flagging rule (`doc/migration.md` §6.5), the
  adapter must key constituent handling on `ResolvedArg.source`
  (`doc/constituents_overhaul.md` §4.15), and `module_name` overrides
  are required wherever a Fortran module name differs from its
  `.meta` table name (`doc/migration.md` §3.3).

---

## 11. Walk-through outline (suggested order for the meeting)

1. Live `ccpp_capgen.py --help` (CLI shape).
2. Show one scheme's `.meta` + its generated group-cap fragment.
3. Run the generator twice — note the `Unchanged: …` messages on the
   second pass (write-if-changed in action).
4. Run `ccpp_datafile.py --scheme-files datatable.xml` to show the
   filtered manifest.
5. Demonstrate a deliberately-broken metadata (`units =` empty, or
   missing scheme, or invalid `case default` group) to show the
   error UX.
6. Walk through the registered scalar-index dimension table and the
   two rules.
7. Open the floor — focus areas for the audience:
   - **Host metadata maintainers**: anything in §6 that surprises
     you for your model?
   - **Scheme metadata maintainers**: anything in §6.2 / §6.3 that
     can't be migrated cleanly?
   - **Framework devs**: §7.1 — which deferred items block your
     downstream work?
