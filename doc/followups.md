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
| FU-005 | Generated Fortran ↔ Codee formatter idempotency | framework | 2026-05 | open | Emitted `.F90` must round-trip cleanly through the project's Codee Fortran formatter. |
| FU-006 | `fortran_to_metadata` developer utility | framework | 2026-05 | open | Bootstrap a `.meta` skeleton from an existing `.F90` subroutine. |
| FU-007 | `ccpp_datafile.py` query CLI rework | framework | 2026-05-13 | open | Collapse `--host-files` / `--suite-files` / `--utility-files` into `--capgen-files`, then repurpose `--host-files` as a filtered list of **input** host metadata files (parallel to `--scheme-files`).  Most hosts pack host data into a handful of files, so the filtering pay-off is small — the draw is API symmetry. |
| FU-014 | Enforce `protected`: a scheme must not write a protected host variable | framework | 2026-07-28 | in progress | **Framework side implemented 2026-07-28, uncommitted.**  **A** — `protected = True` with `intent` other than `in` rejected in `MetaVar.validate()`, `capgen/metadata/metadata_table.py:793`.  **B** — scheme `intent(out\|inout)` on a protected host var rejected in `_resolve_one_arg`, `capgen/generator/suite_resolver.py:1712`.  Original capgen had both (`origin/develop:scripts/metavar.py:332`, `:415`); capgen v1 had neither, though `metadata_table.py:442` documented the rule.  7 tests added; 1555 unit tests and 13/13 end-to-end pass.  Check B immediately found a real fixture bug — `end-to-end-tests/{advection,advection_auto_clone}/test_host_data.meta` marked `test_banana_constituent_indices` protected while `test_host_data.F90:24` declares it with no `protected` attribute and `const_indices.F90:29` writes it; the stray attribute was removed.  Remaining work is CAM-SIMA-side: FU-023. |
| FU-015 | Validator: capture `protected` and `allocatable` from Fortran declarations | framework | 2026-07-28 | open | `_ArgAttrs` (`ccpp_validator.py:135`) carries only type/kind/intent/optional/rank; `_parse_decl_line:352-354` explicitly discards `protected`, `parameter` and `allocatable`.  `allocatable` is the more consequential of the two — metadata declares it (`metadata_table.py:493`) and it *changes codegen* (subscript emission at call sites), so a mismatch is silently wrong output rather than a missing error.  A `protected` check must accept Fortran `parameter` as satisfying it: CAM-SIMA `create_readnl_files.py:422` writes `protected = True` for namelist array dimensions that `:523` declares `integer, public, parameter`.  Cost note: `_ArgAttrs` reprs appear in 7 doctests in `ccpp_validator.py`.  **Deprioritised 2026-07-28** — CAM-SIMA never invokes `ccpp_validator` (no call site in `cime_config/`), so this is CI/developer value only, and FU-014 catches the same class of error where it is load-bearing. |
| FU-016 | Expose `advected` on `ResolvedArg` | framework | 2026-07-26 | open | `capgen_compat/_var_wrapper.py:~320` currently *infers* advectedness from the constituent standard-name shape (`_is_base_constituent_name`) because capgen does not surface the flag.  The inference is close but not exact; exposing the real flag would make it exact. |
| FU-017 | `cime_config/host_framework_deps.py` may now be redundant | cam-sima | 2026-07-28 | open | It was added 2026-07-27 so CAM-SIMA's host code could compile `ccpp_constituent_prop_mod` in constituent-free builds.  Making `ccpp_host_constituents.F90` unconditional (FU-009) put the four framework `.F90` files back into `<utilities>` unconditionally, which likely covers the same ground.  ~90 lines plus 8 tests plus 4 documentation sections.  Verify end-to-end before the next Derecho run and remove if genuinely redundant.  See `constituents_overhaul.md` §4.17. |
| FU-018 | MPAS 120km cam4 aux test fails on constituent ordering | cam-sima | 2026-07 | open | Known failure, distinct from `fadiab` (which also fails on `develop`).  Analysis in `doc/cam4_fwaut_constituent_order.md`. |
| FU-019 | Delete pushed branch `bugfix/constituents_camsima_july2026` | framework | 2026-07-27 | open | Housekeeping.  The branch carried framework commit `501d1c0`, which was wrong and has been reverted; `feature/capgen-v1` is the live branch. |
| FU-023 | Fix `test_protected_reg_write_init` fixture, which violates FU-014 Check B | cam-sima | 2026-07-28 | open | `test/unit/python/sample_files/write_init_files/protected_reg.xml` declares `theta` / `potential_temperature` `access="protected"`, while the shared `temp_adjust.meta` declares the same standard name `intent = inout`.  Host says read-only, scheme writes it — invalid Fortran that has gone unnoticed because the test compares generated text and never compiles a cap.  With FU-014 applied: 159 pass, this one fails.  **Decided approach:** move `access="protected"` from `theta` to `slp` / `air_pressure_at_sea_level`, which `temp_adjust.meta` reads `intent = in`.  Do *not* protect `eddy_len` — it is not CCPP-required (`phys_var_num = 2`), so the test would stop exercising the protected path.  `slp` keeps `<ic_file_input_names>`, so the "protected variable is skipped rather than read" behaviour is still covered.  Then regenerate the two golden files (see §6). |

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

| ID | Shim | Remove when | Touchpoints |
|----|------|-------------|-------------|
| FU-010 | `--legacy-mode` | scheme metadata has migrated | `capgen/metadata/legacy_compat.py`, `unit-tests/test_legacy_compat.py`, every `# legacy-compat:` marker |
| FU-011 | `--gfs-dim-aliases` (added 2026-05-21) | GFS metadata stops spelling `vertical_layer_dimension` as `adjusted_vertical_layer_dimension_for_radiation` / `vertical_composition_dimension` | `capgen/metadata/dim_aliases.py`, `unit-tests/test_dim_aliases.py`, every `# dim-aliases:` marker |
| FU-012 | `--legacy-auto-clone-constituents` (added 2026-05-21) | consumers have moved to explicit `host_constituents(:)` declaration or register-phase scheme registration | `capgen/metadata/auto_clone_constituents.py`, `unit-tests/test_auto_clone_constituents.py`, `unit-tests/sample_files/scheme_auto_clone_consumer.meta`, `unit-tests/sample_suite_files/suite_auto_clone.xml`, every `# auto-clone-constituents:` marker |
| FU-013 | CAM-SIMA `cime_config/capgen_compat/` | phased removal plan A–G in that directory's `README.md` completes | whole directory; brief at `doc/capgen_compat_layer.md` |

---

## 4. Closed

| ID | Item | Closed | Outcome |
|----|------|--------|---------|
| FU-008 | Validator host-metadata check | 2026-06-01 | **Landed.**  `ccpp_validator.py --host-files` validates `type = host` and `type = ddt` tables against module-level declarations and derived-type definitions in the `--source-files` tree.  `type = control` is silent-skipped; `type = scheme` in `--host-files` is a hard error.  Per-variable checks reuse `_check_arg_attributes`.  See `migration.md` §7.4. |
| FU-009 | Suppress `ccpp_host_constituents.F90` when no suite touches constituent state | 2026-07-27 | **Decided against — do not re-propose.**  The host cap re-exports this module's public API, so gating it on suite content would make `<host>_ccpp_cap`'s interface expand and contract with the suite.  That is not a usable API: CAM-SIMA's `cam_comp.F90` USEs six of these entry points, and its dycore coupling and analytic-IC modules use more, all compiled for every configuration.  A host cannot `#ifdef` around a generator decision it cannot see, so "no constituents" must be an *answer* (zero-size table), not a missing symbol.  Original capgen took the same position.  Rationale in the `_generate_host_constituents` docstring, `capgen/generator/host_constituents.py`; consequences in `constituents_overhaul.md` §4.17.  This item had been listed as deferred in three separate documents. |
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
fire in a production build — but the Derecho aux tests are what prove it.
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
| `ip-10-0-0-98.ec2.internal` | 2026-07-28 | initial migration — merged `migration.md` §8, `briefing.md` §7.1, `redesign_prompt.md` "Still deferred", plus open items from this machine's auto-memory |

---

## Cross-references

- `doc/constituents_overhaul.md` — register of record for the constituents area (§2 above).
- `doc/migration.md` — porting guide; §8 points here.
- `doc/briefing.md` — status brief; §7.1 points here.  §7.2 "Intentionally NOT supported" stays there: it is a design stance, not a work queue.
- `doc/redesign_prompt.md` — original design specification.
- `doc/capgen_compat_layer.md` — CAM-SIMA ↔ capgen compatibility layer brief (FU-013).
