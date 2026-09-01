# Follow-up work — single source of truth

This file is the **only** list of deferred items, open questions, and
transient shims for the capgen v1 effort.  It is tracked in git, so it
travels between machines; auto-memory does not.

## How to use this file

- **Do not start a second list.**  Other documents may *reference* items by
  ID (`FU-014`); they must not restate them.  Three parallel lists in
  `migration.md`, `briefing.md` and `redesign_prompt.md` drifted apart and
  were merged here on 2026-07-28.
- **Cite by ID.**  IDs are permanent and never reused.
- **Closed items keep their row**, with the date and the reason.  A closed
  item that is deleted gets re-proposed six months later; the reasoning is
  the valuable part.  See FU-009 for why this matters.
- **Detail is a pointer, not a copy.**  Give `file:line` and the document
  section that argues the item.  Arguments live in design docs, not here.
- **Areas with their own register of record** keep it — see §2.  This file
  indexes into them rather than absorbing them.

Status values: `open`, `in progress`, `blocked`, `closed`.

---

## 1. Open items

| ID | Item | Repo | Raised | Status | Detail |
|----|------|------|--------|--------|--------|
| FU-001 | `ccpp_loop_counter` inside nested subcycles resolves to the OUTERMOST loop variable | framework | 2026-05 | open | No in-tree physics catalog uses the innermost-counter case.  Revisit when a real scheme needs it. |
| FU-002 | Codegen-time cross-check of scheme constituent registration | framework | 2026-05 | open | Today's check is at runtime in `ccpp_initialize_constituents`.  Would need a new metadata attribute `registers_std_names = a, b, c` on register-phase tables.  See `constituents_overhaul.md` §4.9. |
| FU-003 | Framework setters: `set_advected`, `set_diagnostic_name`, `set_default_value`, possibly `set_mixing_ratio_type` | framework | 2026-05 | blocked | Gated on the constituents-overhaul proposal decision (FU-020).  See `constituents_overhaul.md` §4.2. |
| FU-004 | Python linter / formatter pass across `capgen/` | framework | 2026-05 | open | Pick `ruff` and apply. |
| FU-005 | Generated Fortran ↔ Codee formatter idempotency | framework | 2026-05 | open | Emitted `.F90` must round-trip cleanly through the project's Codee Fortran formatter.  Highest-frequency offender is the multi-import single-line `use <mod>, only: …` — break after `only:`, one import per continued line.  Worth a single pass adding a shared wrapping helper (e.g. in `generator/io_helpers.py`) called by every cap writer, rather than piecemeal; other Codee rules (space after `.not.`, …) follow once this lands. |
| FU-006 | `fortran_to_metadata` developer utility | framework | 2026-05 | open | Bootstrap a `.meta` skeleton from an existing `.F90` subroutine. |
| FU-007 | `ccpp_datafile.py` query CLI rework | framework | 2026-05-13 | open | Collapse `--host-files` / `--suite-files` / `--utility-files` into `--capgen-files`, then repurpose `--host-files` as a filtered list of **input** host metadata files (parallel to `--scheme-files`).  Most hosts pack host data into a handful of files, so the filtering pay-off is small — the draw is API symmetry. |
| FU-015 | Validator: capture `protected` and `allocatable` from Fortran declarations | framework | 2026-07-28 | open | `_ArgAttrs` (`ccpp_validator.py:135`) carries only type/kind/intent/optional/rank; `_parse_decl_line:352-354` explicitly discards `protected`, `parameter` and `allocatable`.  `allocatable` is the more consequential of the two — metadata declares it (`metadata_table.py:493`) and it *changes codegen* (subscript emission at call sites), so a mismatch is silently wrong output rather than a missing error.  A `protected` check must accept Fortran `parameter` as satisfying it: CAM-SIMA `create_readnl_files.py:422` writes `protected = True` for namelist array dimensions that `:523` declares `integer, public, parameter`.  Cost note: `_ArgAttrs` reprs appear in 7 doctests in `ccpp_validator.py`.  **Deprioritised 2026-07-28** — CAM-SIMA never invokes `ccpp_validator` (no call site in `cime_config/`), so this is CI/developer value only, and FU-014 catches the same class of error where it is load-bearing. |
| FU-016 | Expose `advected` on `ResolvedArg` | framework | 2026-07-26 | open | `capgen_compat/_var_wrapper.py:~320` currently *infers* advectedness from the constituent standard-name shape (`_is_base_constituent_name`) because capgen does not surface the flag.  The inference is close but not exact; exposing the real flag would make it exact. |
| FU-017 | `cime_config/host_framework_deps.py` may now be redundant | cam-sima | 2026-07-28 | open | It was added 2026-07-27 so CAM-SIMA's host code could compile `ccpp_constituent_prop_mod` in constituent-free builds.  Making `ccpp_host_constituents.F90` unconditional (FU-009) put the four framework `.F90` files back into `<utilities>` unconditionally, which likely covers the same ground.  ~90 lines plus 8 tests plus 4 documentation sections.  Verify end-to-end before the next Derecho run and remove if genuinely redundant.  See `constituents_overhaul.md` §4.17. |
| FU-018 | MPAS 120km cam4 aux test fails on constituent ordering | cam-sima | 2026-07 | open | Known failure, distinct from `fadiab` (which also fails on `develop`).  Analysis in `doc/cam4_fwaut_constituent_order.md`.  The framework-side fix and the re-baseline decision are FU-030.  Post-sign-off cleanup: strip the inert DBG-FP instrumentation (`schemes/utilities/debug_fingerprint.F90` + its call sites) from both `EXT/cam-sima-ng` and `EXT/cam-sima-ng-reference`.  **On the critical path as of 2026-09-01** via FU-030 — see FU-034. |
| FU-019 | Delete pushed branch `bugfix/constituents_camsima_july2026` | framework | 2026-07-27 | open | Housekeeping.  The branch carried framework commit `501d1c0`, which was wrong and has been reverted; `feature/capgen-v1` is the live branch. |
| FU-024 | Confirm FU-014 Check B does not fire in a production CAM-SIMA build | cam-sima | 2026-07-29 | open | The unit tests and fixtures are green, but only a Derecho aux-test run exercises the real registry against the real suites.  Risk assessed low — the three registry variables that carry `access="protected"` (`fracis`, `do_lagrangian_vertical_coordinate`, `dycore_calculates_geopotential_using_logarithms`) are all consumed `intent = in` (§5) — but `access="protected"` is not the only source: `allocatable="parameter"` also emits `protected = True` (`generate_registry_data.py:694-695`), as does `create_readnl_files.py:422` for namelist array dimensions.  Fold the result back here. |
| FU-025 | Revisit capgen's logging-output scheme | framework | 2026-07-15 | open | Per-variable transform logging (`group_cap.py:_log_one_transform`, ~:545) is **temporarily emitted at WARNING** (see the `TEMPORARY level choice` comment at ~:553) purely so it shows in a default run — capgen's default level is WARNING; INFO needs `-v`.  Decide its real home (INFO + `-v`, a dedicated `--report-transforms` flag [Dom's likely preference: targeted, no flood], or leave) then drop the WARNING abuse.  Same pass: reclassify non-warning WARNINGs — the three shim banners (`legacy_compat.py:~85`, `dim_aliases.py`, `auto_clone_constituents.py`; fire every CAM-SIMA/SCM run) and the per-scheme "no Fortran source found … fallback" (`ccpp_capgen.py:~1162`) are informational.  Flipping the default to INFO is not an option — `write_if_changed` logs per file. |
| FU-026 | `GFS_debug.F90` successor + generated debug/docs/diff utility family | framework | 2026-05-14 | open | A family of opt-in utilities generated from the resolved suite/host metadata, replacing hand-maintained duplicate state: (1) per-(suite,phase) debug scheme (min/max/mean/checksum) — the `GFS_debug.F90` successor; (2) variable provenance / first-written-last-read tracker; (3) range/NaN validator (needs new `min_value`/`max_value` attrs — overlaps constituents Class-A, FU-020); (4) per-suite reference-doc generator (Markdown first); (5) `ccpp_datafile.py diff <suiteA> <suiteB>`.  Substantial CCPP-team design discussion — **do not start unilaterally**.  Items 1 and 5 are the smallest first steps.  Distinct from FU-006 (metadata bootstrap) and from `--no-host-introspection` (which *removes* introspection). |
| FU-027 | Consolidate emitter scoping + host-vs-suite audit (generator hardening) | framework | 2026-06-04 | open | Remaining two of a four-item hardening plan (items 1–2 done: e2e already compiles+links+runs every cap; `suite_allocate` + `constituents_dim` corpus tests landed).  **#3** — one shared helper that, given a `ResolvedArg`, returns its USE-requirements and access expression, called by all four emitters (`group_cap`/`static_api`/`suite_cap`/`suite_data`); the register-USE divergence bug could not have existed if both paths shared it.  **#4** — proactive walk of the four emitters reconciling host-vs-suite handling (USE / dimensions / allocatable / DDT-module / naming) in one pass.  Kills the divergence bug class rather than patching instances.  e2e tree off-limits without explicit permission. |
| FU-028 | CAM-SIMA schemes: undefined `intent(out)` on an early-return path | cam-sima | 2026-06-08 | open | Original capgen zero/false-initialised interstitial storage, masking schemes that leave an `intent(out)` unset on an early-return branch; capgen-ng deliberately does **not** default-init suite-owned vars, so these read heap garbage at runtime.  **Decision (Dom 2026-06-08): fix each scheme in place; do NOT add suite-var default-init to capgen-ng** (that would re-mask the whole class).  Expect more to surface one-by-one as suites run under capgen-ng.  First instance fixed: `solar_irradiance_data_init` (set `do_spectral_scaling = .false.` before the `fixed_scon` return).  Edits live in the `EXT/cam-sima-ng/src/physics/ncar_ccpp` submodule. |
| FU-029 | Decide `timestep_init` / `timestep_final` phase-call-count semantics | framework | 2026-06-10 | open | For a scheme that appears multiple times in a suite, original capgen calls its `timestep_init`/`final` **once per appearance**; capgen-ng calls it **once per group** (measured cam4: `qneg_timestep_final` 2 vs 12).  Benign for cam4 (the affected phases are idempotent/guarded) but a latent b4b/correctness hazard the moment such a phase is stateful (accumulates, zeroes a buffer).  CCPP intent is once-per-timestep; neither matches strictly when a scheme spans groups.  Decide the intended semantics and make capgen-ng's behaviour intentional + documented.  Reproduce via the standalone-capgen driver, diffing `_timestep_(init|final)` call counts. |
| FU-030 | Deterministic + documented constituent registration order in the generator | framework | 2026-06-11 | open | Root cause of the cam4 FWAUT b4b diff (the framework side of FU-018): capgen-ng registers water species alphabetically ([cloud_ice, cloud_liquid, water_vapor]) vs original's declaration order ([cloud_liquid, cloud_ice, water_vapor]), and trace gases differ too, so `air_composition`'s `thermodynamic_active_species_idx` order → `get_hydrostatic_energy` water-sum FP order → energy fixer → pervasive roundoff.  Proven b4b by a flag-guarded reorder hack.  **Decision (Dom): RE-BASELINE** — give capgen-ng a deterministic, documented order (qv first; an understandable rule for how constituents land in the array), then CAM-SIMA re-baselines against the original-capgen reference; not match-the-old-order.  Levers: `host_constituents.py` / the legacy-auto-clone path (FU-012) / `ccpp_register_constituents` emission; intersects the constituents overhaul (FU-020).  Analysis: `doc/cam4_fwaut_constituent_order.md`.  **On the critical path as of 2026-09-01** — CAM-SIMA's transition gates the `develop` merge and the removal of capgen v0 + prebuild (FU-034), and this is one of its two gating items. |
| FU-031 | Long-term redesign of the `ccpp_static_api.F90` runtime listings | framework | 2026-05-14 | open | The suite-variable / suite-host-data listings made the introspection module ~33k lines (`-O3` effectively hangs).  Immediate pressure is off — `--no-host-introspection` stubs them (→ ~800 lines) — so this is **no longer blocking**, but the long-term redesign stays open for team discussion: move the listings to a runtime read of `datatable.xml` (preferred — no recompile when listings change), or a separate `-O0` file, or static string `data` tables, or lazy-emit only the routines the host calls.  Do not redesign unilaterally. |
| FU-032 | Generator-owned locals can silently shadow a host import — auto-uniquify | framework | 2026-08-07 | open | `_check_host_control_local_collisions` (`capgen/generator/group_cap.py`) now hard-errors when a host variable's local name collides with a control-variable dummy (issue #774 — the silent wrong-value case, closed by that check + `unit-tests/test_suite_resolver.py::TestHostControlLocalNameCollision`).  Two other subroutine-scope locals can shadow a use-associated host import the same way but are **generator-owned**, so the right fix is to rename *them*, not error: transformation temporaries (`<name>_l` / `<name>_p`) and subcycle loop counters (`ccpp_loop_counter*`).  Seed the temp uniquifier (`used_local_names_phase`, `suite_resolver.py:2446`) with the group's host-import symbols + control-dummy names so `_local_name_conflict` renames generator locals away from them.  Rare in practice (suffixed/reserved names) but closes the class.  Deliberately deferred out of the #774 fix (Step 2, 2026-08-07). |
| FU-033 | Vertical flip on an allocatable host array is rejected, not supported | framework | 2026-08-12 | open | `_resolve_one_arg` (`capgen/generator/suite_resolver.py`, just after `needs_vert_flip` is computed) hard-errors when a host variable is `allocatable = True` **and** needs a vertical flip (host/scheme `top_at_one` disagree on a var with a vertical dim).  An allocatable actual must omit subscripts, so the reverse-stride flip subscript cannot be encoded; silently dropping it would hand the scheme vertically-reversed data.  **Decision (2026-08-12, PR #762 review finding from jimmielin): error for now** rather than emit a wrong-but-compiling cap.  Test: `unit-tests/test_suite_resolver.py::TestVerticalFlipTransform::test_allocatable_host_plus_flip_raises`.  Any future support is limited to the allocatable-host → *non-allocatable* (plain assumed-shape) scheme-dummy sub-case, where a flipped section `host(:, ub:lb:-1)` is legal; an allocatable dummy can never receive a flipped section.  Parallels FU-032 (guard now, enhance later). |
| FU-034 | Merge `feature/capgen-v1` to `develop` and delete capgen v0 + `ccpp-prebuild` — gated on CAM-SIMA | framework | 2026-09-01 | open | **Decision (Dom, 2026-09-01): the merge to `develop`, and the removal of both capgen v0 and `ccpp-prebuild` from the tree, happen WHEN/AFTER CAM-SIMA transitions to capgen v1.**  One operation, one trigger — the sequencing is settled, not open.  Context: NEPTUNE, CCPP-SCM and UFS transitioned on 2026-09-01 and build directly off **`feature/capgen-v1`** (`briefing.md` §10); CAM-SIMA is still on capgen v0 with v1 on testing/review branches, and is the last host holding either predecessor alive.  Consequences: (1) **CAM-SIMA's transition is the critical path for the entire v1 rollout**, which promotes its gating items — the constituent-ordering re-baseline (FU-018/FU-030) and the compat-layer removal plan (FU-013) — from CAM-SIMA-local concerns to blockers on the whole merge; (2) until then `feature/capgen-v1` is a long-lived release branch with three production consumers, so treat it as released — no force-push, no breaking generated-API change without notice; (3) PR #762 is the umbrella PR and the natural vehicle; (4) `--legacy-auto-clone-constituents` (FU-012) is CAM-SIMA's shim, so it also sits on this critical path — decide it as part of the transition rather than inheriting it (§3). |
| FU-035 | Constituent index evidence is scheme-metadata-only — a runtime-registered constituent nothing flags needs a host declaration | framework | 2026-09-01 | open | Fallout of the `index_of_*` fix (2026-09-01): `index_of_<X>` is auto-provisioned as a constituent index only when capgen has **positive evidence** that X is a constituent, i.e. some scheme flags `X` or `tendency_of_X` `advected`/`constituent`/`molar_mass` (`_is_known_constituent`, `capgen/generator/suite_resolver.py`).  That is the only constituent knowledge available at codegen — register-phase Fortran `%instantiate(std_name=…)` is not parsed (FU-002).  **Residual gap:** a constituent registered at runtime whose base name is flagged in *no* scheme metadata, and whose index some scheme consumes, now raises the missing-provider error instead of resolving.  Workaround (and what every in-tree host already does): declare the index in host metadata — host declarations win before any constituent path.  No such case exists in CAM-SIMA, CCPP-SCM or the e2e corpus today; all nine CAM-SIMA `index_of_*` names are either registry-declared or scheme-produced `intent=out`.  Closing FU-002 would remove the gap entirely by making registration itself the evidence. |

---

## 2. Constituents overhaul

**Register of record: `doc/constituents_overhaul.md`.**  That document
maintains its own status taxonomy (§4.1–4.18 marked OPEN/FIXED, §7 Q1–Q8
open design questions, §8 the three proposals).  Do not duplicate its
content here — this is a scannable index so the items are visible from the
single list.

| ID | Item | Section | Status |
|----|------|---------|--------|
| FU-020 | **Decide between Proposal A (bugfix only) / B (class A/B split + setters) / C (host-only registration)** — gates FU-003 and several items below | §8 | blocked on meeting |
| — | Framework: `is_match` is too strict | §4.3 | open |
| — | Framework: `diag_name` portability problem | §4.4 | open |
| — | Original capgen: implicit registration | §4.5 | open (observation) |
| — | Original capgen: single-instance `ccpp_model_constituents_obj` | §4.6 | open (limitation) |
| — | Original capgen: `ConstituentVarDict` complexity | §4.7 | open (observation) |
| — | Capgen: scheme-metadata `diagnostic_name` for `is_constituent` args is host-specific | §4.10 | open |
| — | Capgen: `ccpp_scheme_utils` singleton | §4.11 | open (documented limit) |
| — | Capgen: drop `diagnostic_name_fixed`, keep only `diagnostic_name` | §4.12 | open (proposed simplification) |
| — | Capgen: error-output keyword inconsistency across emitted public API | §4.14 | open (observation) |
| — | Capgen: register-phase constituents are invisible to codegen | §4.16 | open |
| FU-021 | **May host code `use` framework modules directly, or is `<host>_ccpp_cap` the whole contract?** Settled sub-part: adding `ccpp_constituent_prop_ptr_t` and `ccpp_constituent_properties_t` to `constituent_pub_syms` is correct regardless — 72 of CAM-SIMA's ~78 direct imports are those two types | §4.18 | open |
| — | Q1–Q8 open design questions (`default_value` class, `water_species`, `mixing_ratio_type`, post-relaxation disagreement, `%instantiate` class-B args, singleton, `_layer` suffix, constituent triplet) | §7 | open |

`FU-020` and `FU-021` carry IDs because they are cited from outside that
document; the rest are indexed by section only.

---

## 3. Transient shims awaiting removal

Each has an explicit removal trigger.  Remove the module, its unit tests,
its fixtures, and every marked touchpoint together.

**2026-09-01 — FU-010 and FU-011 are now pinned by production hosts.**
NEPTUNE, CCPP-SCM and UFS transitioned to capgen v1 *with* the shims
rather than by migrating their metadata first (`briefing.md` §6.3b status
note, §10).  The removal triggers below are unchanged and still correct,
but FU-010 and FU-011 are now coordinated host-metadata migrations rather
than framework-side cleanups — pinned by the GFS-physics-derived hosts
(CCPP-SCM, UFS; NEPTUNE to confirm).  Estimate the host-side work before
scheduling either.

**FU-012 is the exception.**  Its consumer is CAM-SIMA's ~16 auto-clone
schemes, and CAM-SIMA is **still on capgen v0** — its v1 support is on
testing/review branches.  So FU-012 is pinned only by branch work, not by
a production build, and it is the one shim whose removal can still be
folded into a migration that has not happened yet.  Since that migration
now gates the `develop` merge (FU-034), FU-012 is on the critical path:
decide it as part of CAM-SIMA's transition rather than inheriting it
afterwards.

| ID | Shim | Remove when | Touchpoints |
|----|------|-------------|-------------|
| FU-010 | `--legacy-mode` | scheme metadata has migrated | `capgen/metadata/legacy_compat.py`, `unit-tests/test_legacy_compat.py`, every `# legacy-compat:` marker |
| FU-011 | `--gfs-dim-aliases` (added 2026-05-21) | GFS metadata stops spelling `vertical_layer_dimension` as `adjusted_vertical_layer_dimension_for_radiation` / `vertical_composition_dimension` | `capgen/metadata/dim_aliases.py`, `unit-tests/test_dim_aliases.py`, every `# dim-aliases:` marker |
| FU-012 | `--legacy-auto-clone-constituents` (added 2026-05-21) | consumers have moved to explicit `host_constituents(:)` declaration or register-phase scheme registration | `capgen/metadata/auto_clone_constituents.py`, `unit-tests/test_auto_clone_constituents.py`, `unit-tests/sample_files/scheme_auto_clone_consumer.meta`, `unit-tests/sample_suite_files/suite_auto_clone.xml`, every `# auto-clone-constituents:` marker |
| FU-013 | CAM-SIMA `cime_config/capgen_compat/` | phased removal plan A–G in that directory's `README.md` completes — **on the critical path as of 2026-09-01**, one of the two items gating CAM-SIMA's transition and therefore the FU-034 merge | whole directory; brief at `doc/capgen_compat_layer.md` |

---

## 4. Closed

| ID | Item | Closed | Outcome |
|----|------|--------|---------|
| FU-008 | Validator host-metadata check | 2026-06-01 | **Landed.**  `ccpp_validator.py --host-files` validates `type = host` and `type = ddt` tables against module-level declarations and derived-type definitions in the `--source-files` tree.  `type = control` is silent-skipped; `type = scheme` in `--host-files` is a hard error.  Per-variable checks reuse `_check_arg_attributes`.  See `migration.md` §7.4. |
| FU-009 | Suppress `ccpp_host_constituents.F90` when no suite touches constituent state | 2026-07-27 | **Decided against — do not re-propose.**  The host cap re-exports this module's public API, so gating it on suite content would make `<host>_ccpp_cap`'s interface expand and contract with the suite.  That is not a usable API: CAM-SIMA's `cam_comp.F90` USEs six of these entry points, and its dycore coupling and analytic-IC modules use more, all compiled for every configuration.  A host cannot `#ifdef` around a generator decision it cannot see, so "no constituents" must be an *answer* (zero-size table), not a missing symbol.  Original capgen took the same position.  Rationale in the `_generate_host_constituents` docstring, `capgen/generator/host_constituents.py`; consequences in `constituents_overhaul.md` §4.17.  This item had been listed as deferred in three separate documents. |
| FU-014 | Enforce `protected`: a scheme must not write a protected host variable | 2026-07-29 | **Done**, framework commit `e68b6fb`.  **A** — `protected = True` with an `intent` other than `in` rejected in `MetaVar.validate()`, `capgen/metadata/metadata_table.py:793`.  **B** — scheme `intent(out\|inout)` on a protected host variable rejected in `_resolve_one_arg`, `capgen/generator/suite_resolver.py:1717`.  Original capgen had both (`origin/develop:scripts/metavar.py:332`, `:415`); capgen v1 had neither, though `metadata_table.py:442` documented the rule.  7 tests added; 1555 unit tests and 13/13 end-to-end pass.  Check B immediately found two real fixture bugs: `end-to-end-tests/{advection,advection_auto_clone}/test_host_data.meta` marked `test_banana_constituent_indices` protected while `test_host_data.F90:24` declares it with no `protected` attribute and `const_indices.F90:29` writes it (stray attribute removed); and the CAM-SIMA fixture in FU-023.  Production confirmation is FU-024. |
| FU-023 | Fix `test_protected_reg_write_init` fixture, which violated FU-014 Check B | 2026-07-29 | **Done** (CAM-SIMA, on top of `d599908`).  `protected_reg.xml` had `theta` / `potential_temperature` `access="protected"` while the shared `temp_adjust.meta` declares that standard name `intent = inout` — invalid Fortran that went unnoticed because the test compares generated text and never compiles a cap.  `access="protected"` moved to `slp` / `air_pressure_at_sea_level`, which `temp_adjust.meta` reads `intent = in`; the two golden files regenerated per §6.  The whole golden diff is the swap and nothing else: `protected_vars` and `initialized_vars` exchange elements, `theta` gains a `read_field` call and `slp` gains the `endrun('… is a protected variable')`, so both branches stay covered — and the read now exercises the 2-D path (`read_field(..., 'lev', ...)`) rather than the 1-D one.  160 CAM-SIMA python unit tests pass. |
| FU-022 | `_FRAMEWORK_CONST_DIM_INPUTS` cleanup | 2026-05-13 | **Done.**  The hand-curated frozenset is gone; framework-constituent dimension references ride on a dedicated `used_const_dim_std_names` field on `ResolvedArg`. |

---

## 5. Notes worth keeping

**CAM-SIMA's registry does support `protected`.**  Via `access="protected"`
on a `<variable>` (`src/data/generate_registry_data.py:567-570`), and via
`allocatable="parameter"` — both emit `protected = True` into the generated
`.meta` (`:694-695`).  The real registry has three: `fracis` /
`fraction_of_water_insoluble_convectively_transported_species`,
`do_lagrangian_vertical_coordinate`, and
`dycore_calculates_geopotential_using_logarithms`.  Every in-tree scheme
consuming them declares `intent = in`, so FU-014 Check B is not expected to
fire in a production build — but the Derecho aux tests are what prove it
(FU-024).
Namelist variables are protected too (`create_readnl_files.py:422, :440`),
read-only by construction.

## 6. Regenerating CAM-SIMA golden test files

`test/unit/python/test_write_init_files.py` compares generated output
byte-for-byte (`filecmp.cmp(..., shallow=False)`) against committed samples
in `test/unit/python/sample_files/write_init_files/`.  There is no
`--update-golden` flag.  Output is written to `test/unit/python/tmp/...`,
which is gitignored (`.gitignore:12`), so:

```bash
python -m pytest test/unit/python/test_write_init_files.py -k <selector>
# fails on the comparison, but still writes the output files
cp test/unit/python/tmp/write_init_files/<name>.F90 \
   test/unit/python/sample_files/write_init_files/
git diff        # review this — it is the only safeguard
python -m pytest test/unit/python/test_write_init_files.py -q
```

## 7. Reconciliation log

Auto-memory, TODO lists and task lists are **per-machine** and do not travel.
Each machine records here when its local stores were last swept into this
file, per the procedure in the repository's `CLAUDE.md`.

| Machine | Last reconciled | By |
|---------|-----------------|-----|
| `dutchman` | 2026-09-01 | Host-adoption update: NEPTUNE, CCPP-SCM and UFS have transitioned to capgen v1 on `feature/capgen-v1`; **CAM-SIMA has not — it is still on capgen v0 with v1 on testing/review branches**, so it is the remaining migration, not a fourth consumer.  Added FU-034, which records Dom's decision that the `develop` merge **and** the deletion of capgen v0 + `ccpp-prebuild` happen when/after CAM-SIMA transitions — one operation, one trigger.  That puts CAM-SIMA's transition on the critical path for the whole rollout, so FU-013, FU-018 and FU-030 gained critical-path annotations.  Added the §3 pinning note — FU-010/FU-011 were *not* retired by adoption, they gained production consumers, while FU-012 is pinned only by CAM-SIMA branch work.  Closed two long-standing unknowns in `briefing.md` §10: NEPTUNE high-altitude physics and the UFS FV3 fast-physics group both work with v1 as expected.  Refreshed `briefing.md` §1/§6.3b/§10 (§10 host bullets were stale on all three models; test counts corrected 1516→1564 verified and e2e 12→13, `capgen_ng` was missing) and dropped the stale §7.1 bullet claiming `protected` is unenforced (FU-014 closed 2026-07-29).  Swept this machine's auto-memory: every deferred entry already maps to an existing FU row, nothing new to fold in; wrote one new memory for the adoption fact pointing at FU-034.  Later the same day, added FU-035 as fallout of the `index_of_*` positive-evidence fix: constituent auto-provisioning now requires positive evidence that a name IS a constituent, and the residual runtime-registration gap is recorded there (FU-002 would close it). |
| `dutchman` | 2026-08-12 | PR #762 review-fix session (jimmielin's Claude-generated findings): added FU-033 (vertical-flip + allocatable-host → hard error).  The other two findings — backward-transform temp-name collision (`suite_resolver`) and incomplete `[ccpp-table-properties]` missing `name`/`type` now erroring (`metadata_table`) — are fixed, committed, and PR'd, so they live in git, not restated here.  Nothing new in this machine's auto-memory to fold beyond the above. |
| `dutchman` | 2026-08-07 | folded the issue #772 / #774 session: added FU-032 (generator-local shadow follow-up).  #772 shown to be a non-issue in v1 (cld_shadow e2e reproducer) and #774 detect-and-error landed in `group_cap.py` — both tracked in GitHub, not restated here |
| `dutchman` | 2026-08-06 | first sweep of this machine; folded its auto-memory investigation notes into new rows FU-025…FU-031, added Codee `use…only:` detail to FU-005, cross-linked FU-018↔FU-030 |
| `ip-10-0-0-98.ec2.internal` | 2026-07-29 | swept on adding FU-024; local stores unchanged since the previous sweep, nothing new to fold in |
| `ip-10-0-0-98.ec2.internal` | 2026-07-28 | initial migration — merged `migration.md` §8, `briefing.md` §7.1, `redesign_prompt.md` "Still deferred", plus open items from this machine's auto-memory |

---

## Cross-references

- `doc/constituents_overhaul.md` — register of record for the constituents area (§2 above).
- `doc/migration.md` — porting guide; §8 points here.
- `doc/briefing.md` — status brief; §7.1 points here.  §7.2 "Intentionally NOT supported" stays there: it is a design stance, not a work queue.
- `doc/redesign_prompt.md` — original design specification.
- `doc/capgen_compat_layer.md` — CAM-SIMA ↔ capgen compatibility layer brief (FU-013).
