# capgen repository — file catalogue (DRAFT)

> **Status: temporary draft for the code-walkthrough prep.** One row per file, except
> the many test/example *input* fixtures, which are collapsed. Once reviewed, the
> relevant sections will be folded into `README.md` / `doc/DevelopersGuide/`.
> External checkouts under `EXT/` (UFS reference + capgen integration trees) are
> intentionally excluded — they are not part of this repository.

## Top level

| File | Description |
|------|-------------|
| `README.md` | Repository overview and entry point. |
| `LICENSE` | License. |
| `end-to-end-tests.sh` | Driver script that builds and runs all end-to-end tests. |
| `ccpp_constituent_prop_mod.F90.patch` | Patch applied to the runtime constituent-properties module for host integrations. |
| `CODEOWNERS`, `.codecov.yml`, `.codee-format`, `.gitignore` | Repo/CI configuration (code owners, coverage, formatter, ignore rules). |
| `.github/` | GitHub Actions CI workflows (unit tests, end-to-end tests, doxygen). |

## `capgen/` — command-line entry points

| File | Description |
|------|-------------|
| `__init__.py` | Package marker (“next-generation CCPP code generator”). |
| `ccpp_capgen.py` | **Main generator CLI.** Parses metadata + the SDF, resolves variables, and writes the caps, `ccpp_kinds.F90`, and `datatable.xml`. Hosts flags like `--kind-type`, `--trace`, `--no-host-introspection`, and the compat shims. |
| `ccpp_datafile.py` | CLI to query the generated `datatable.xml` (generated files, scheme files, dependencies) for build systems / CMake. |
| `ccpp_validator.py` | **Standalone validator** — checks scheme Fortran source against its `.meta` (intent/type/kind/rank/dimensions). Separate tool from the generator; owns the one Fortran parser. |

## `capgen/generator/` — cap code generation

| File | Description |
|------|-------------|
| `__init__.py` | Package marker. |
| `datatable.py` | Writes/reads `datatable.xml` mapping suites → generated files, scheme modules, and dependencies (the build-system interface). |
| `suite_xml.py` | Parses the Suite Definition File (SDF) XML into the suite object model (groups, subcycles, subcolumns). |
| `suite_types.py` | Object model for suites/groups/schemes, incl. intrinsic-vs-external scheme classification. |
| `suite_resolver.py` | Resolves a suite end-to-end: matches variables across schemes + host, constituents, index symbols, unit normalization. |
| `suite_cap.py` | Emits the **suite-level cap** (`ccpp_physics_run`/`_init`/… dispatching to groups; register-before-init contract). |
| `group_cap.py` | Emits the **per-group caps** that call the schemes, with argument marshalling and inline transforms. |
| `host_cap.py` | Emits the **host cap** (registration + runtime introspection API; introspection routines stubbed under `--no-host-introspection`). |
| `host_constituents.py` | Host-side constituent handling (`type=host` constituent tables). |
| `suite_data.py` | Emits the generated suite **data module** — pointer-wrapper DDTs plus transform local temporaries. |
| `kinds_writer.py` | Writes `ccpp_kinds.F90` (kind definitions the caps `use`). |
| `trace.py` | Shared helpers emitting the gated `if (trace) write(...) 'CCPP TRACE …'` lines in every cap (toggled by `--trace`). |

## `capgen/metadata/` — metadata parsing & variable resolution

| File | Description |
|------|-------------|
| `__init__.py` | Package marker. |
| `metadata_table.py` | Parser for `.meta` metadata-table files (`[ccpp-table-properties]` / `[ccpp-arg-table]`). |
| `variable_resolver.py` | Core variable matching/transform engine — unit + kind conversions, vertical flip, DDT typing. |
| `unit_conversion.py` | Unit-conversion formula table (`{var}` substitution) feeding the auto-inserted unit transforms. |
| `registered_dimensions.py` | Registry of count-dim ↔ index-var pairings (`SCALAR_INDEX_DIMS`) and framework count dimensions. |
| `legacy_compat.py` | **Transient shim** — rewrites legacy CCPP standard names (e.g. `horizontal_loop_extent`) at parse time. |
| `dim_aliases.py` | **Transient shim** — collapses equivalent GFS-physics dimension names. |
| `auto_clone_constituents.py` | **Transient shim** — reinstates original-capgen auto-cloning of static constituents. |

## `capgen/metadata/parse_tools/` — shared parse utilities

| File | Description |
|------|-------------|
| `__init__.py` | Package marker. |
| `parse_source.py` | Parsing primitives: parse context + exception types. |
| `parse_checkers.py` | Metadata field validators (`check_units`, `check_dimensions`, `check_cf_standard_name`, …). |
| `parse_log.py` | Shared logging utilities for parse processes. |
| `io_helpers.py` | File-write helpers with write-if-changed (no-op-if-unchanged) semantics. |
| `fortran_conditional.py` | Builds Fortran conditional expressions (in local names) for active/optional-argument handling. |
| `xml_tools.py` | XML helpers — entity expansion and pretty-printed writing (SDF / datatable). |

## `capgen/schema/` & `capgen/src/` — schema + shipped runtime Fortran

| File | Description |
|------|-------------|
| `schema/suite_v1_0.xsd` | XML schema for SDF v1.0. |
| `schema/suite_v2_0.xsd` | XML schema for SDF v2.0 (adds suite-level `<init>`/`<final>`). |
| `src/ccpp_constituent_prop_mod.F90` (+ `.meta`) | Runtime constituent-properties DDT module shipped with the framework. |
| `src/ccpp_hash_table.F90` | Runtime hash-table support. |
| `src/ccpp_hashable.F90` | Hashable base type used by the hash table. |
| `src/ccpp_scheme_utils.F90` | Runtime scheme utility routines. |

## `unit-tests/` — pytest suite (one row per driver; fixtures collapsed)

| File | Description |
|------|-------------|
| `run_tests.py`, `conftest.py`, `__init__.py` | Test runner, pytest fixtures, package marker. |
| `test_metadata_table.py` | Tests for `metadata/metadata_table.py`. |
| `test_variable_resolver.py` | Tests for `metadata/variable_resolver.py`. |
| `test_registered_dimensions.py` | Tests for `metadata/registered_dimensions.py`. |
| `test_dim_aliases.py` | Tests for `metadata/dim_aliases.py`. |
| `test_legacy_compat.py` | Tests for `metadata/legacy_compat.py`. |
| `test_auto_clone_constituents.py` | Tests for `metadata/auto_clone_constituents.py`. |
| `test_io_helpers.py` | Tests for `parse_tools/io_helpers.py`. |
| `test_suite_xml.py` | Tests for `generator/suite_xml.py`. |
| `test_suite_types.py` | Tests for `generator/suite_types.py`. |
| `test_suite_resolver.py` | Tests for `generator/suite_resolver.py`. |
| `test_suite_cap.py` | Tests for `generator/suite_cap.py`. |
| `test_suite_data.py` | Tests for `generator/suite_data.py`. |
| `test_host_cap.py` | Tests for `generator/host_cap.py`. |
| `test_host_constituents.py` | Tests for `generator/host_constituents.py`. |
| `test_kinds_writer.py` | Tests for `generator/kinds_writer.py`. |
| `test_datatable.py` | Tests for `generator/datatable.py`. |
| `test_trace.py` | Tests for `generator/trace.py`. |
| `test_ccpp_datafile.py` | Tests for `ccpp_datafile.py`. |
| `test_validator.py` | Tests for `ccpp_validator.py` (incl. the Fortran parser). |
| `test_control_validation.py` | Tests for control-variable validation rules. |
| `test_integration.py` | End-to-end generator integration tests (full parse → resolve → emit). |
| `sample_files/`, `sample_suite_files/` | **~100 metadata / SDF / Fortran fixtures** consumed by the tests above — not catalogued individually. |

## `end-to-end-tests/` — full build-and-run cases (one row per case; fixtures collapsed)

Each case directory bundles host + scheme Fortran, `.meta`, an SDF, a `*_test_reports.py`
comparison driver, and CMake glue. The fixtures are collapsed; the row describes what the case exercises.

| Case | What it exercises |
|------|-------------------|
| `capgen/` | **Overall generator capabilities** — multiple suites & groups, DDT usage (incl. an undocumented DDT member), `ccpp_constant_one:N` and bare-`N` dimensions, non-standard/integer dimensions, variables promoted to suite level, dimensions set in the register phase and used to allocate module-level interstitials, and threading. |
| `advection/` | Constituent advection — cloud liquid/ice constituents with tendency application (`apply_constituent_tendencies` invoked twice); includes a deliberate error suite (`cld_suite_error.xml`) to exercise diagnostics. |
| `advection_auto_clone/` | Same fixtures as `advection/`, run through the `--legacy-auto-clone-constituents` shim path. |
| `ddthost/` | A host whose CCPP data is carried in a derived type (`host_ccpp_ddt`); runs the temp + DDT suites against it. |
| `var_compat/` | The variable-compatibility object (`VarCompatObj`): unit conversions (forward & reverse), vertical flip (`top_at_one`), kind conversions, and combinations — plus subcycles (nested, dynamic vs fixed iteration length, shared length-defining standard names). |
| `nested_suite/` | Nested suites (a suite that includes a sub-suite), expanded at and inside groups; SDF schema **2.0**; suite-level single `<init>`/`<final>` schemes. Inherited from `var_compat`. |
| `constituents_dim/` | Variables dimensioned by the framework constituent count `number_of_ccpp_constituents` (host never declares it); covers host-owned, framework-allocated, and scheme-allocated count-dim cases, plus consuming constituents without re-flagging (rule b). |
| `suite_allocate/` | A suite-owned, **scheme-allocated** (`allocatable`) variable promoted to `ccpp_<suite>_data`; its dimension is also suite-owned and set in the `timestep_init` phase (so it can't be allocated at init). |
| `instances/` | **Multiple model instances** — `instance`/`number_of_instances` paired control; the host loops `ccpp_physics_run` over instances with per-instance data (unit-conversion schemes are just the vehicle). |
| `instances_advection/` | Multiple instances combined with constituent advection — scheme-registered constituents with a per-instance buffer (`ninstances` × cloud-liquid + tendency application). |
| `opt_arg/` | Optional-argument handling — present/absent dummy arguments (pointer association vs runtime guard). |
| `chunked_data/` | Chunked/blocked host data — `chunk_begin`/`chunk_end` bounds over `nchunks` chunks. |
| `*_test_reports.py` (where present) | Per-case driver that builds, runs, and diffs expected vs actual output (older cases; newer cases run via `ctest`/CMake). |
| `CMakeLists.txt`, `cmake/`, `utils/` | Shared CMake configuration and helpers for the e2e harness. |

## `doc/` — documentation

| File | Description |
|------|-------------|
| `README.md` | Documentation index. |
| `redesign_prompt.md`, `redesign_analysis.md`, `redesign_analysis_original_*.md` | Original redesign brief and analysis that motivated capgen. |
| `briefing.md`, `briefing_pm.md` | Design briefings. |
| `migration.md` | Guide for migrating a host from ccpp-prebuild/original-capgen to capgen. |
| `capgen_compat_layer.md` | Documents the transient compatibility shims (legacy names, dim aliases, auto-clone). |
| `constituents.md` | Constituent-handling design. |
| `constituents_overhaul.md` | Proposed constituent-model overhaul (proposals A/B/C). |
| `auto_clone_constituents.md` | Design notes for the auto-clone-constituents shim. |
| `cam4_fwaut_constituent_order.md` | Case study: CAM4 FWAUT constituent-ordering b4b investigation. |
| `Doxyfile.in` | Doxygen configuration. |
| `CMakeLists.txt` | Build glue for the docs. |
| `DevelopersGuide/` | Developers Guide (`README.md`, generated PDFs, LaTeX style) — bundle, not catalogued per file. |
| `HelloWorld/` | Worked “hello world” host + scheme + suite + build example — bundle, not catalogued per file. |
| `img/` | Documentation images. |
