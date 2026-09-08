# capgen — Briefing for Project Management

*Companion to `doc/briefing.md` (the developer walk-through) and
`doc/redesign_analysis.md` (the deep-dive technical comparison of
prebuild and capgen).  This document targets project leadership and
program managers; it summarises the case for `capgen` in terms of
product risk, schedule, and cross-organization impact rather than
implementation detail.*

*Last revised: 2026-09-01.*

---

## TL;DR

**Status as of 2026-09-01: three of the four host models have
transitioned to `capgen`.**  NOAA UFS, Navy NEPTUNE and CCPP-SCM — all
previously on `ccpp-prebuild` — now build with the new generator.
**CAM-SIMA is the one remaining transition**, and it is the gate on
finishing the job: the plan of record is to merge `capgen` into
`develop` and delete *both* older generators when/after CAM-SIMA
moves.  The rest of this section is the situation that motivated the
work.

The CCPP Framework shipped **two** code generators that solved the
same problem differently:

- **`ccpp-prebuild`** powered NOAA UFS, Navy NEPTUNE, and CCPP-SCM.
  Simple and reliable, but feature-light — does not support features
  CAM-SIMA needs (constituents, framework-owned variables,
  introspection).  **No longer has a production consumer.**
- **`ccpp-capgen`** powers NCAR CAM-SIMA.  Feature-rich, but built on
  technical choices that **do not scale** to UFS or NEPTUNE and that
  **do not support multi-instance hosts** at all.  **Still in
  production for CAM-SIMA.**

Neither generator could be the basis for a single shared toolchain.
**`capgen`** is a third generator, started in early May 2026,
designed to do everything both other generators do, in code small
enough for a few people to own, with the architectural choices that
make it work at UFS/NEPTUNE scale and beyond.  The SCM was the proving
ground; UFS and NEPTUNE followed and are now transitioned, and
CAM-SIMA re-integration is the remaining step.

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

## 6. Where things stand right now (2026-09-01)

- **Three of four host models have transitioned to capgen.**  NEPTUNE,
  CCPP-SCM and the UFS Weather Model — the entire `ccpp-prebuild` user
  base — now build with the new generator and track its development
  branch directly.  `ccpp-prebuild` has no production consumer left.
  **CAM-SIMA is the remaining transition** and is still on the older
  `ccpp-capgen`.
- **The endgame is defined and has a single trigger.**  When/after
  CAM-SIMA transitions, capgen merges into `develop` and *both* older
  generators are deleted from the tree — one operation.  That makes
  CAM-SIMA's transition the critical path for the whole programme, and
  promotes its two gating items (the constituent-ordering re-baseline
  and the retirement of the CAM-SIMA compatibility layer) to
  programme-level blockers rather than CAM-SIMA-local work.  Schedule
  risk concentrates there; see §8.
- **Unit tests**: 1564 passing.  No known failures.
- **End-to-end tests**: 13 passing — `advection`,
  `advection_auto_clone` (CAM-SIMA advection_test port exercising the
  auto-clone shim), `capgen`, `capgen_ng`, `chunked_data`,
  `constituents_dim`, `ddthost`, `instances`, `instances_advection`
  (multi-instance + constituents), `nested_suite`, `opt_arg`,
  `suite_allocate`, `var_compat`.  `constituents_dim` and
  `suite_allocate` were added while hardening the CAM-SIMA HPC build.
- **Code size**: ~17.8k lines of Python under `capgen/` including
  inline comments and the three transient shim modules; ~18k lines of
  unit/doctest under `unit-tests/`.  Still procedural; still flat
  data classes; still well below capgen.
- **CCPP-SCM**: **transitioned.**  It drove most of the generator's
  hardening — each build / runtime issue it surfaced landed as a fix in
  capgen rather than a host-side workaround, which is why it was the
  proving ground for the other prebuild hosts.  All available suites
  build and run end-to-end, via `--legacy-mode` + `--gfs-dim-aliases`.
- **Three transient migration shims in place** (see §5).  Each is
  isolated in its own module with a single grep tag, so the
  framework-side removal is a single cleanup pass.  **The hosts
  transitioned *with* these shims rather than migrating their metadata
  first**, so retiring one is now a coordinated host-side migration —
  a scheduling item, not a cleanup.  It does not block the merge.
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
- **NEPTUNE**: **transitioned.**  All regression tests (~300) pass with
  the three mandatory compilers (Intel LLVM, GCC, LLVM native) for
  regular physics, mid-altitude, and high-altitude physics
  (feature-complete).  High-altitude physics — the last acceptance item
  outstanding in June — works with capgen as expected.
- **UFS Weather Model**: **transitioned.**  The largest of the hosts,
  and the one the older capgen could never have served (§3.1).  The
  anticipated complication was the "fast physics" called directly from
  the FV3 dynamical core as a separate group; that group works as
  expected and needed no special handling.
- **CAM-SIMA**: **not yet transitioned — still on the older
  `ccpp-capgen`, and the critical path (see above).**  capgen v1
  support lives on branches maintained for testing and review; the
  production configuration has not moved.  On those branches, capgen
  drives the real CAM-SIMA build on the Derecho supercomputer through a
  small compatibility layer that lets CAM-SIMA's existing build scripts
  call capgen without being rewritten.  Three configurations build
  **and run to completion under both the Intel and GNU compilers**,
  with bit-comparable results: `kessler`, `rrtmgp`, and
  `se_cslam`/CSLAM — the last being the full CAM7 physics suite
  (deep + shallow convection, stratiform microphysics, RRTMGP
  radiation, gravity-wave drag) on a cubed-sphere/CSLAM-advection
  configuration.  That was the first time the redesigned generator
  produced a complete, running CAM-SIMA model.  Remaining before
  transition: a re-baseline caused by a change in constituent ordering
  (a known, understood floating-point difference, not a defect) and
  retirement of the compatibility layer.  The constituent overhaul
  decision (see §7) remains a separate track.

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
| Bus-factor on capgen itself | MEDIUM | Procedural code style + flat data classes + 1564-test safety net; significantly lower than capgen's bus factor |
| **CAM-SIMA transition slips, delaying the whole programme** | **MEDIUM — the main schedule risk as of 2026-09-01** | The `develop` merge and the deletion of both older generators are gated on this one transition (§6), so its two remaining items — the constituent-ordering re-baseline and retirement of the compatibility layer — are programme-level blockers.  Both are understood and scoped; neither is a defect.  Mitigation is to track them as such rather than as CAM-SIMA-local work, and to decide the auto-clone shim's fate as part of the transition |
| Two host call-shape conventions (prebuild-style vs capgen-style) coexist forever | LOW | capgen emits one shape; downstream host conversions are tracked in `doc/migration.md` |
| Regression discovered during NEPTUNE / UFS testing | LARGELY RETIRED (2026-09-01) | Both models have transitioned; NEPTUNE passes ~300 regression tests on three compilers including high-altitude physics, and the anticipated UFS FV3 fast-physics complication did not materialise.  The SCM proving-ground approach worked as intended — issues became capgen fixes, not host-side patches |
| ccpp-prebuild end-of-life requires a sunset plan | SCOPED (2026-09-01) | Decided: `ccpp-prebuild` **and** the older `ccpp-capgen` are both deleted from the framework repo in the same operation that merges capgen into `develop`, triggered when/after CAM-SIMA transitions.  prebuild already has no production consumer.  The residual risk is schedule, not scope — see the CAM-SIMA row above |

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
