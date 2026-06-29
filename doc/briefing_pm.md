# capgen — Briefing for Project Management

*Companion to `doc/briefing.md` (the developer walk-through) and
`doc/redesign_analysis.md` (the deep-dive technical comparison of
prebuild and capgen).  This document targets project leadership and
program managers; it summarises the case for `capgen` in terms of
product risk, schedule, and cross-organization impact rather than
implementation detail.*

*Last revised: 2026-06-05.*

---

## TL;DR

The CCPP Framework today ships **two** code generators that solve the
same problem differently:

- **`ccpp-prebuild`** powers NOAA UFS, Navy NEPTUNE, and CCPP-SCM.
  Simple and reliable, but feature-light — does not support features
  CAM-SIMA needs (constituents, framework-owned variables,
  introspection).
- **`ccpp-capgen`** powers NCAR CAM-SIMA.  Feature-rich, but built on
  technical choices that **do not scale** to UFS or NEPTUNE and that
  **do not support multi-instance hosts** at all.

Neither generator can be the basis for a single shared toolchain.
**`capgen`** is a third generator, started in early May 2026,
designed to do everything both other generators do, in code small
enough for a few people to own, with the architectural choices that
make it work at UFS/NEPTUNE scale and beyond.  The redesign is
running on the SCM as proving ground; UFS / NEPTUNE / CAM-SIMA
re-integration is sequenced behind that.

This document explains, in plain language, **why we did not extend
capgen instead**, what risks the redesign retires, and where things
stand.

---

## 1. The three generators in one paragraph each

**`ccpp-prebuild`** (NOAA/NAVY/DTC, in production for UFS/NEPTUN/SCM).
Procedural Python; reads metadata; emits Fortran caps; passes
host-defined derived-type (DDT) arguments to scheme call sites.  In
production for several years; bug rate is low; the team understands
it (those who worked with it).  What it doesn't do: framework-owned
variables, the constituent mechanism CAM-SIMA needs, and runtime
introspection.  Treated as the **baseline for simplicity and
reliability**.

**`ccpp-capgen`** (NCAR, in production for CAM-SIMA).  Heavy
object-oriented Python (deep class hierarchy, ~tens of thousands of
lines); reads metadata; emits Fortran caps that pass **flat scalar
fields** to scheme call sites instead of DDTs; supports the
constituent mechanism, suite-owned variables, introspection, and a
few other features prebuild lacks.  **It is the only existing
generator with those features.**  But — see §3 — it has structural
limits that make it impractical for UFS, NEPTUNE, or multi-instance
hosts, and the implementation is concentrated enough that few people
can extend it safely (if at all - primary developer gone).

**`capgen`** (new, 2026-05).  Procedural Python (~17.8k lines
including inline comments and the three transient shim modules; flat
data classes); reads the same metadata format; passes
arguments like prebuild; supports the features capgen pioneered
(constituents, suite-owned variables, introspection); supports
multi-instance, an integer state machine, six explicit scheme
phases, vertical-flip / unit / kind transforms, registered
scalar-index dimensions for threading and ensembles, write-if-changed
build integration, and a separate Fortran-vs-metadata validator
tool.  Designed so the same generator works for prebuild-style hosts
(UFS / NEPTUNE / SCM) and capgen-style hosts (CAM-SIMA).

---

## 2. Why this matters now

Three pressures converged in 2025/26:

1. **Framework unification heavily delayed.**  A fully-functional
   capgen that supports UFS / NEPTUNE / SCM and replaces prebuild
   was promised for years, and never delivered. Pressure from
   project management and sponsors is building.
2. **UFS / NEPTUNE want the capgen feature set.**  Constituents
   in particular are increasingly central to atmospheric physics
   (chemistry, aerosols, deep atmosphere), and re-implementing the
   prebuild-side glue per host is duplicated effort.  Extending
   capgen to UFS-scale runs into the flat-field problem (§3.1) —
   not a small refactor, a fundamental data-shape change. The
   performance of capgen generating multi-suite caps is up to
   20 times slower than that of prebuild for the CCPP SCM. This
   is caused by fundamental design choices (five layers of
   classes inheriting from each other) that are integral to capgen.
3. **The team owning capgen has limited bandwidth to extend it.**
   The class hierarchy is intricate; understanding the
   `ConstituentVarDict` scope-chain or the auto-clone path requires
   reading several modules together.  Realistically, only one or two
   people on the framework team can change capgen without breaking
   something downstream.  One of them now lives overseas. This is an
   unacceptable **bus-factor risk** that the redesign retires.

---

## 3. What capgen does that does not extend to UFS / NEPTUNE / multi-instance

This section is for the project lead who came from the capgen side:
none of these are critiques of capgen as a *product*.  They are
specific architectural choices that worked for CAM-SIMA's
single-instance design and don't generalize.  Each is sourced from
the technical analysis in `doc/redesign_analysis.md` and validated
by the SCM / multi-instance test work this month.

### 3.1 Flat-field argument passing fails at UFS / NEPTUNE scale

CAM-SIMA's group caps pass **every individual variable as a separate
argument** to the scheme dispatch routine.  At CAM-SIMA's roughly
two-hundred-variable scale this works.  At UFS scale (~1200
variables per group), the generated Fortran exceeds compiler limits,
prevents the use of strict error-checking flags (`-check all`,
`-fcheck=all`) required for operational implementation, and even
when it does compile produces unmaintainably large source files.
**This is one technical reason capgen cannot drive UFS today**,
independent of any other concern.

`capgen` reverts to prebuild's DDT-argument convention.  Host
authors pass their physics DDTs by reference (one or a few arguments
per scheme call); component access happens **at the scheme call level**.
This works at every scale we've measured.

### 3.2 Single-instance constituents are baked into the generated code

CAM-SIMA runs one host per executable, so capgen generates a single
module-level `ccpp_model_constituents_obj`.  The constituent
mechanism — the central feature capgen inherited from capgen —
references that global directly.  Re-targeting capgen to
multi-instance is not a configuration toggle; it requires
re-emitting the constituent module per-instance throughout the
generator, plus refactoring the framework setters.

`capgen` was multi-instance **from day one**: every constituent
entry point takes an `instance_number` argument; the property
storage, the state machine, the dynamic-constituent buffers are all
per-instance.  As of 2026-05-18, the per-suite dynamic-constituents
buffer was also moved per-instance after the new combined
multi-instance + constituents end-to-end test surfaced a latent bug
that capgen would never have hit (because capgen never supported
multi-instance).  **The redesign is finding bugs the legacy
toolchain hid.**

### 3.3 Constituent registration has three competing paths in capgen

capgen accepts constituent declarations from (a) host-supplied
arrays, (b) scheme `register`-phase Fortran subroutines, and (c) an
**auto-clone path** that scans scheme metadata for the
`is_constituent` attribute and silently generates a registration in
the host cap.  The auto-clone path is invisible from the scheme
Fortran — to know whether a scheme registers a constituent you have
to know the generator semantics.  This makes scheme code harder to
read, harder to port between hosts, and harder to debug when
registrations collide.

`capgen` keeps only the first two (explicit) paths.  Auto-clone
is deliberately gone from the default behaviour — see
`doc/constituents_overhaul.md` §2.3.  For legacy hosts that already
ship metadata in the original-capgen shape (production CAM-SIMA's
atmospheric_physics tree is the immediate consumer; ~16 of the ~20
schemes that touch constituents rely on auto-clone today), an opt-in
shim `--legacy-auto-clone-constituents` reinstates the original path
behind a single CLI flag with a loud startup banner; see
`doc/auto_clone_constituents.md`.  The shim is single-instance only
and is marked for removal once consumers migrate to explicit
registration.

### 3.4 Host-specific values baked into scheme metadata

capgen requires `diagnostic_name` (host's diagnostic-output label,
e.g. `CLDLIQ` for CAM-SIMA but something else for UFS) at
constituent instantiation time.  Schemes therefore embed
host-specific strings into their own metadata.  Porting a scheme
between hosts requires either editing the scheme or maintaining a
fork.

`capgen` is moving `diagnostic_name` (and a handful of other
host-configuration properties) to a host-side override mechanism;
schemes carry physics-portable defaults only.  The reform is
documented in `doc/constituents_overhaul.md`; the decision is on the
agenda for one of the next framework-team meetings.

### 3.5 Synthetic variable-resolution scopes are hard to extend

capgen introduces a five-layer deep synthetic dictionary
(`ConstituentVarDict`) between the suite and host scopes during
variable matching.  The mechanism works for capgen's use cases
but is a code path most contributors don't read.  Extending the
resolver to handle multi-instance dimensions, scalar-index
substitution, or constituent host-wins semantics required
undoing parts of the synthetic scope.

`capgen`'s resolver is flat: each scheme arg is classified into
exactly one source (control / host / suite / constituent), recorded
on a small data class (`ResolvedArg`), and used directly by the
emitter.  No synthetic dictionary. **This design inherits from
`prebuild` and is the primary reason `capgen` is comparable
in performance to `prebuild`.

### 3.6 Code volume and team coverage

capgen is roughly an order of magnitude larger than prebuild, with a
deeply layered class hierarchy.  This is not a moral failing — it
reflects the feature set — but the practical consequence is that
the maintenance burden falls on a small subset of the framework
team.  capgen is comparable to prebuild in *shape* (procedural
Python with small data classes — no deep class hierarchy), and the
generator itself sits at ~17.8k lines.
The "who can fix this" pool is closer to "anyone with
framework context".  capgen comes with ~1.4k docstring + unit
tests (~18k lines of test code), plus an end-to-end test suite of
12 fixtures that covers all of prebuild's and capgen's existing
end-to-end tests and adds new ones for multi-instance + constituents
(`instances_advection`), the auto-clone-constituents shim
(`advection_auto_clone`), constituent-count dimensions
(`constituents_dim`), and suite-owned allocatable interstitials
(`suite_allocate`).  Including these tests and the rich inline
comments puts capgen's full tree on the same order of magnitude as
capgen — about half of which is test coverage and human-readable
prose, not load-bearing logic.

---

## 4. What `capgen` does better than capgen — at any scale

For audiences who already accept the multi-instance and UFS-scale
arguments, the day-to-day quality-of-life improvements that apply
even to CAM-SIMA-shape problems:

| Topic | capgen | capgen |
|---|---|---|
| Scheme call argument shape | Flat fields | DDT references |
| Variable resolution | Scope-chain promotion via synthetic dict | Flat 4-source classification on `ResolvedArg` |
| Suite state runtime check | String comparison | Integer-named-parameter state machine |
| Fortran-vs-metadata validation | Embedded in generator | Standalone tool (`ccpp_validator.py`) — run by developers or CMake before generation |
| Generator code style | Deep class hierarchy | Flat data classes + procedural resolver |
| Error reporting | Variable amount of context | "Loud, specific, actionable" enforced — every parse-time error names file, line, variable, attribute, value, and reason |
| Constituent registration | Three sources (one invisible) | Two sources, both explicit |
| `is_constituent` auto-clone | Yes (host-specific values baked into scheme metadata) | Removed by default; reinstated for legacy hosts behind opt-in `--legacy-auto-clone-constituents` shim (single-instance only) |
| `_finalize` vs `_final` phase name | `_finalize` | `_final` (renamed to keep symmetry with init/timestep_init/timestep_final) |

---

## 5. Additional features of `capgen` compared to `capgen`

Features that exist only in capgen (some exist in prebuild):

| Capability | Why it matters |
|---|---|
| **Multi-instance host support** (per-instance state machine, per-instance constituent objects, per-instance dynamic-constituents buffers as of 2026-05-18) | Required by NEPTUNE (prebuild has basic solution) |
| **Registered scalar-index dimensions** | When metadata says a variable is dimensioned by `number_of_threads` or `number_of_instances`, capgen injects the right per-call subscript automatically; the host's OpenMP-thread-private DDT layout works unchanged |
| **Subcycle loop-counter automation** | Schemes inside a `<subcycle loop="N">` element can access `ccpp_loop_counter` / `ccpp_loop_extent` directly; the generator emits the Fortran `do` loop and binds the locals |
| **`--legacy-mode` migration shim** | One CLI flag enables silent rewrite of two known-good deprecated standard names (`horizontal_loop_extent` → `horizontal_dimension`, `number_of_openmp_threads` → `number_of_threads`) with a loud warning — buys time for host metadata to migrate |
| **`--gfs-dim-aliases` migration shim** (2026-05-21) | One CLI flag treats GFS-physics names (`adjusted_vertical_layer_dimension_for_radiation`, `vertical_composition_dimension`) as equivalent to `vertical_layer_dimension` in the dim-identity check only — variables remain distinct everywhere else.  Resolver-only; clean grep-revert.  Required for CCPP-SCM v17p8 to build under capgen. |
| **`--legacy-auto-clone-constituents` migration shim** (2026-05-21) | One CLI flag reinstates original ccpp-capgen's auto-clone-static-constituent registration path for the ~16 production-CAM-SIMA schemes that depend on it.  Single-instance only (predates multi-instance); fails fast if a multi-instance host is supplied.  This is the no-decision-needed bridge that lets capgen accept CAM-SIMA's atmospheric_physics metadata before any constituent-overhaul work lands. |
| **`--no-host-introspection` flag** | The five runtime introspection routines (`ccpp_physics_suite_list`, etc.) emit large `select case` blocks at SCM scale; this flag stubs the bodies, dropping the generated static API from ~33,000 lines to ~800 for the SCM build (the introspection routines were making `-O1` compilation effectively hang) |
| **Consistent handling of external types** (MPI f08 communicator, ESMF clock) | Tabled in capgen because of the complexity of the solution |

---

## 6. Where things stand right now (2026-06-05)

- **Unit tests**: 1516 passing.  No known failures.
- **End-to-end tests**: 12 passing — `advection`,
  `advection_auto_clone` (CAM-SIMA advection_test port exercising the
  auto-clone shim), `capgen`, `chunked_data`, `constituents_dim`,
  `ddthost`, `instances`, `instances_advection`
  (multi-instance + constituents), `nested_suite`, `opt_arg`,
  `suite_allocate`, `var_compat`.  The two newest (`constituents_dim`,
  `suite_allocate`) were added while hardening the CAM-SIMA HPC build.
- **Code size**: ~17.8k lines of Python under `capgen/` including
  inline comments and the three transient shim modules; ~18k lines of
  unit/doctest under `unit-tests/`.  Still procedural; still flat
  data classes; still well below capgen.
- **CCPP-SCM**: actively driving development.  Each build / runtime
  issue surfaced this month landed as a fix in capgen rather than
  a host-side workaround.  All available suites in CCPP-SCM now
  build and run end-to-end via `--legacy-mode` + `--gfs-dim-aliases`.
- **Three transient migration shims in place** (see §5).  Each is
  isolated in its own module with a single grep tag, so removal once
  hosts migrate is a single cleanup pass.
- **Auto-clone shim landed 2026-05-21**.  Reinstates original capgen's
  auto-clone path behind `--legacy-auto-clone-constituents`.  This is
  the no-decision-needed bridge for CAM-SIMA — the ~16 schemes that
  declare `advected = True` in `_run` arg-tables and rely on the
  framework to register the constituent will now work under capgen
  without metadata edits.
- **Multi-instance + constituents fix landed 2026-05-18**.  The new
  combined end-to-end test surfaced a latent shared-buffer mutation
  bug; the fix moves the per-suite dynamic-constituents buffer
  per-instance.  No coordination with CAM-SIMA / UFS / NEPTUNE
  required (host-facing API unchanged).
- **NEPTUNE**: Final cleanup and acceptance testing in progress.
  All regression tests (~300) pass with the three mandatory
  compilers (Intel LLVM, GCC, LLVM native) for regular physics,
  mid-altitude, and high-altitude physics (feature-complete).
- **UFS Weather Model**: not yet attempted; SCM is the proving
  ground first.  Expecting updates due to the "fast physics"
  called directly from the FV3 dynamical core as separate group.
- **CAM-SIMA**: **re-connected (2026-06-03 → 06-05).**  capgen now
  drives the production CAM-SIMA build on the Derecho supercomputer
  through a small compatibility layer that lets CAM-SIMA's existing
  build scripts call capgen without being rewritten.  Three
  configurations build **and run to completion under both the Intel and
  GNU compilers**, with bit-comparable results: `kessler`, `rrtmgp`,
  and `se_cslam`/CSLAM — the last being the full CAM7 physics suite
  (deep + shallow convection, stratiform microphysics, RRTMGP
  radiation, gravity-wave drag) on a cubed-sphere/CSLAM-advection
  configuration.  This is the first time the redesigned generator has
  produced a complete, running CAM-SIMA model.  The constituent
  overhaul decision (see §7) remains a separate track and was not on
  the critical path for this milestone.

---

## 7. What is intentionally NOT decided yet

The redesign is opinionated about the architectural choices (DDT
arguments, per-instance everything, integer state machine, two-tool
split).  It is **not** opinionated about the framework-level
constituent reform.

`doc/constituents_overhaul.md` lays out three reform proposals on
the table:

- **Proposal A** (mostly landed): bug-fix on the deallocate path +
  add missing host setters for properties the host wants to
  override.  Conservative.
- **Proposal B** (recommended for the next 4–6 weeks): relax the
  identity-equality check, formally classify properties as
  "scheme-intrinsic" (immutable) vs "host-configuration" (mutable
  after registration).  Physics schemes using constituents become
  genuinely portable across hosts.
- **Proposal C** (tabled): drop scheme-side constituent
  registration entirely; only the host registers.  Cleaner but
  requires coordinated PRs across the framework, both generators,
  the CAM-SIMA atmospheric_physics tree, and CAM-SIMA itself.

These are open questions for the framework-team meeting, not
capgen decisions.  capgen is structured so all three
proposals are implementable on top of it.

---

## 8. Risk register (project-management view)

| Risk | Status | Mitigation |
|---|---|---|
| capgen diverges from capgen feature set | LOW | Cross-checked by `doc/redesign_analysis.md`; the feature comparison table in §4 / §5 is exhaustive |
| Host metadata break for UFS / NEPTUNE / CAM-SIMA | LOW | Three transient shims (`--legacy-mode`, `--gfs-dim-aliases`, `--legacy-auto-clone-constituents`) together cover the known-incompatible standard-name pair, the GFS radiation/composition vertical-dim spellings, and original capgen's auto-clone registration path.  Remaining required changes (e.g., `_finalize` → `_final`) are mechanical and listed in `doc/migration.md` §3 |
| Constituent overhaul stalls | LOW | Proposal A unblocks the immediate bug; capgen works with the current framework today; `--legacy-auto-clone-constituents` lets CAM-SIMA's atmospheric_physics build without an overhaul decision; the overhaul is a separate decision track |
| Bus-factor on capgen itself | MEDIUM | Procedural code style + flat data classes + 1426-test safety net; significantly lower than capgen's bus factor |
| Two host call-shape conventions (prebuild-style vs capgen-style) coexist forever | LOW | capgen emits one shape; downstream host conversions are tracked in `doc/migration.md` |
| Regression discovered during NEPTUNE / UFS testing | EXPECTED | SCM proving ground catches most; remaining issues become capgen tickets, not host-side patches |
| ccpp-prebuild end-of-life requires a sunset plan | OPEN | Not yet scoped; both generators currently coexist in the framework repo |

---

## 9. The pragmatic case (for the meeting)

Three points worth raising explicitly:

1. **Extending capgen to UFS/NEPTUNE scale is not a configuration
   change — it is a refactor of the same magnitude as a redesign.**
   The flat-field convention is load-bearing throughout capgen's
   variable-matching, resolution, and emission code.  Once that
   change is made, the resulting generator looks substantially
   like capgen anyway.
2. **The features capgen pioneered (constituents, suite-owned
   variables, introspection) are kept and improved — not
   discarded.**  capgen is genuinely the successor, not a
   parallel project.  The contributions made on the capgen side are
   what made the capgen feature set possible.  A significant
   portion of capgen's code, in particular metadata parsing,
   Fortran-metadata validation, and constituents, were imported
   into capgen.
3. **The team owning capgen can be larger than the team owning
   capgen.**  This is the most important practical point for
   long-term program health.  A framework that three organizations
   can maintain is more resilient than a framework that one
   organization (or one individual in that organization) can maintain.

---

## 10. References

- `doc/briefing.md` — developer walk-through; same outline, more
  technical detail.
- `doc/redesign_analysis.md` — deep-dive technical comparison of
  prebuild and capgen with named-product examples.
- `doc/migration.md` — host-author migration guide.
- `doc/constituents_overhaul.md` — the constituent-reform discussion
  document.
- `doc/capgen_compat_layer.md` — short brief on the CAM-SIMA ↔ capgen
  compatibility layer (for the original ccpp-capgen author).
- `end-to-end-tests/` — the working examples (`instances_advection`
  is the newest, exercises everything end-to-end).
