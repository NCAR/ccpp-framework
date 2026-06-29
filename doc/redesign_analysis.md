# CCPP Framework Code Generator — Technical Analysis for Redesign

*Analysis date: 2026-05-04. Clarifications added: 2026-05-05.*

This document is a deep-dive technical analysis of the two existing CCPP Framework code generators —
`ccpp-prebuild` and `ccpp-capgen` — produced as input to a planned complete redesign.
It covers execution flow, data structures, feature sets, build system integration, and
key architectural differences.

---

## Table of Contents

1. [Background and motivation](#1-background-and-motivation)
2. [ccpp-prebuild — detailed analysis](#2-ccpp-prebuild--detailed-analysis)
3. [ccpp-capgen — detailed analysis](#3-ccpp-capgen--detailed-analysis)
4. [Shared infrastructure](#4-shared-infrastructure)
5. [Feature comparison](#5-feature-comparison)
6. [Build system integration](#6-build-system-integration)
7. [Key architectural differences](#7-key-architectural-differences)
8. [Design considerations for the redesign](#8-design-considerations-for-the-redesign)
9. [Real-world example: CCPP Single Column Model (SCM)](#9-real-world-example-ccpp-single-column-model-scm)
10. [Real-world example: CAM-SIMA (capgen)](#10-real-world-example-cam-sima-capgen)
11. [Real-world example: UFS Weather Model (prebuild)](#11-real-world-example-ufs-weather-model-prebuild)
12. [Real-world example: Navy NEPTUNE (prebuild, restricted)](#12-real-world-example-navy-neptune-prebuild-restricted)
13. [Cross-cutting design decision: how host data enters the cap chain](#13-cross-cutting-design-decision-how-host-data-enters-the-cap-chain)

---

## 1. Background and motivation

The CCPP Framework is a code generator that analyzes metadata describing variables required
by physical parameterizations in numerical weather prediction (NWP) models, compares them
against metadata provided by a host model, and generates Fortran interface ("cap") code that
connects the two.

There are two generations of the generator:

**`ccpp-prebuild`** (`scripts/ccpp_prebuild.py`):
- Simple, mostly procedural Python
- Used in: NOAA UFS Weather Model, Navy NEPTUNE, CCPP-SCM
- Extremely reliable in research, development, and operations
- Fewer capabilities; simpler design

**`ccpp-capgen`** (`scripts/ccpp_capgen.py`):
- Highly complex, object-oriented Python taken to the extreme
- Used in: NCAR CAM-SIMA (still mostly a research/development model)
- Many advanced features designed but never implemented (funding/priority gaps)
- Notoriously difficult to develop; no remaining team member fully understands it

**The original plan** was to update `ccpp-capgen` with missing features from `ccpp-prebuild`
and transition all models to it. **This plan has been abandoned** in favor of a complete
redesign that draws the best lessons from both generations.

The immediate trigger for abandoning capgen was the failure — after considerable effort by
three developers — to make capgen pass DDT arguments to group caps the way prebuild does.
This is the root cause of capgen's severe performance problem (seconds for prebuild,
10+ minutes for capgen on the same suite set) and of its broken handling of optional
variables under Fortran compiler debugging flags.

---

## 2. ccpp-prebuild — detailed analysis

### 2.1 Command-line arguments and configuration

Entry point: `scripts/ccpp_prebuild.py`, `main()`.

Arguments parsed by `argparse`:

| Argument | Required | Purpose |
|---|---|---|
| `--config` | yes | Path to host-model Python config module |
| `--suites` | no | Comma-separated suite names (without `.xml`) |
| `--builddir` | no | Override build directory from config |
| `--namespace` | no | Appended to static API module name |
| `--debug` | no | Insert Fortran array-size checks in generated caps |
| `--clean` | no | Remove generated files and exit |
| `--verbose` | no | Set logging to DEBUG |

The `--config` file is a plain Python module imported dynamically via `importlib`.
Key variables it must define:

| Config variable | Purpose |
|---|---|
| `VARIABLE_DEFINITION_FILES` | List of host-model Fortran sources with metadata hooks |
| `SCHEME_FILES` | List of physics scheme Fortran sources |
| `CAPS_DIR` | Output directory for generated cap `.F90` files |
| `SUITES_DIR` | Directory containing suite definition XML files |
| `STATIC_API_DIR` | Output directory for `ccpp_static_api.F90` |
| `TYPEDEFS_MAKEFILE/CMAKEFILE/SOURCEFILE` | Paths for typedef build snippets |
| `SCHEMES_MAKEFILE/CMAKEFILE/SOURCEFILE` | Paths for scheme build snippets |
| `CAPS_MAKEFILE/CMAKEFILE/SOURCEFILE` | Paths for cap build snippets |
| `HTML_VARTABLE_FILE`, `LATEX_VARTABLE_FILE` | Documentation output paths |
| `TYPEDEFS_NEW_METADATA` | Optional: dict enabling DDT member name translation bridge |

The config file can contain arbitrary Python expressions — computed file lists,
conditional logic, environment-variable lookups — making it very flexible.

### 2.2 Step-by-step execution pipeline

```
1. Import config module dynamically via importlib

2. gather_variable_definitions()
       for each file in VARIABLE_DEFINITION_FILES:
           parse_variable_tables(file)   [metadata_parser.py]
       → metadata_define: OrderedDict[standard_name → [mkcap.Var]]

3. collect_physics_subroutines()
       for each file in SCHEME_FILES:
           parse_scheme_tables(file)     [metadata_parser.py]
       → metadata_request:    OrderedDict[standard_name → [mkcap.Var, ...]]
       → arguments_request:   OrderedDict[scheme → OrderedDict[subroutine → [std_names]]]
       → dependencies_request: OrderedDict[scheme → [abs_paths]]
       → schemes_in_files:    OrderedDict[scheme → abs_path]

4. compare_metadata()   [batch matching]
       for each std_name in metadata_request:
           check exists in metadata_define
           check type/kind/rank compatibility
           register unit conversions in var.actions
           copy local_name as var.target
       → metadata: OrderedDict[std_name → [Var]]  (targets and actions set)

5. check_optional_arguments()  [warnings only]

6. For each requested suite XML:
       Suite.parse(xml)  [mkstatic.py]   → Suite + Group objects
       Group.write()                      → ccpp_<suite>_<group>_cap.F90
       Suite.write()                      → ccpp_<suite>_cap.F90

7. API.write()  [mkstatic.py]
       → ccpp_static_api[_<namespace>].F90

8. Write build-system snippets  [mkcap.py writers]
       → CCPP_CAPS.cmake/mk/sh
       → CCPP_SCHEMES.cmake/mk/sh
       → CCPP_TYPEDEFS.cmake/mk/sh
       → CCPP_API.cmake/sh

9. mkdoc.metadata_to_html()   → HTML variable table
   mkdoc.metadata_to_latex()  → LaTeX variable table
```

### 2.3 Data structures — the "flat dict" model

Everything in prebuild lives in flat Python `OrderedDict` structures. There is no object
hierarchy; variables are simple Python objects with plain attributes.

```python
# Top-level data containers
metadata_define:    OrderedDict[standard_name  →  [mkcap.Var]]         # 1 Var per std_name
metadata_request:   OrderedDict[standard_name  →  [mkcap.Var, ...]]    # N Vars (one per scheme×subroutine)
arguments_request:  OrderedDict[scheme_name    →  OrderedDict[subroutine_name → [std_names]]]
dependencies_request: OrderedDict[scheme_name  →  [abs_paths]]
schemes_in_files:   OrderedDict[scheme_name    →  abs_path]
```

`mkcap.Var` attributes:

| Attribute | Type | Description |
|---|---|---|
| `standard_name` | str | CF-convention unique identifier |
| `long_name` | str | Human-readable description |
| `units` | str | Physical units |
| `local_name` | str | Fortran local name (may be DDT member reference) |
| `type` | str | Fortran type (real, integer, logical, or DDT name) |
| `kind` | str | Fortran kind parameter |
| `dimensions` | list[str] | Dimension standard names |
| `intent` | str | in / out / inout |
| `active` | str | `'T'`, `'F'`, or expression string |
| `optional` | str | `'T'` or `'F'` |
| `pointer` | bool | Whether Fortran POINTER attribute needed |
| `target` | str | Set during matching: the host model local_name |
| `actions` | dict | `{'in': fn, 'out': fn}` for unit conversions |
| `container` | str | Encoded provenance: `MODULE_foo SCHEME_bar SUBROUTINE_baz` |

**Performance note on `container` and `target`**: these two attributes act as a lookup
cache computed once during the `compare_metadata()` batch step. The `container` string
encodes where each variable lives in the host model (module and, if applicable, the
DDT member chain). The `target` records the resolved Fortran local name. Both are
computed once and then used directly during Fortran cap generation — no further dictionary
lookups are needed. This is a major contributor to prebuild's speed advantage.

### 2.4 Metadata parsing and the bridge to capgen

`metadata_parser.py` is a shared module that acts as a bridge. It detects whether a
metadata section in a Fortran source file uses the old pipe-delimited format (deprecated,
warning emitted) or the new `.meta` format (triggered by `!! \htmlinclude <name>.html`
in the Fortran source comment hook).

For `.meta` files, `read_new_metadata()` in `metadata_parser.py`:
1. Calls capgen's `metadata_table.parse_metadata_file()` → `[MetadataTable]`
2. Converts each `metavar.Var` to a `mkcap.Var`
3. Normalizes `active` to `'T'`/`'F'`/expression, `optional` to `'T'`/`'F'`

The `TYPEDEFS_NEW_METADATA` config variable (when provided) triggers an additional
pass via `convert_local_name_from_new_metadata()` which translates flat
standard-name-style local names into DDT member references such as
`Atm(blk_no)%q(:,:,:,graupel_index)`. This is the bridge that makes the newer
`.meta` format work with the older DDT-heavy host model code.

### 2.5 Variable matching — `compare_metadata()`

A single batch function processes all matching. For each standard name in `metadata_request`:

1. Check it exists in `metadata_define` — error if missing
2. Check there is exactly one definition — error if ambiguous
3. Call `var.compatible(other_var)` — checks equality of `standard_name`, `type`, `kind`, and rank
4. Register unit conversions: if units differ, `var.convert_from()` / `var.convert_to()`
   stores a conversion function in `var.actions`
5. Check `active` attribute: if host variable is conditionally allocated and scheme variable
   is not `optional`, issue a warning (not an error)
6. Copy `local_name` from the define side as `var.target`
7. Build module use list from container strings

Result: `metadata` dict where each `Var` has `.target` set to the host model local name
and `.actions` populated with any needed unit conversion functions.

### 2.6 Generated Fortran files

#### Group cap: `ccpp_<suite>_<group>_cap.F90`

One module per group. For each CCPP stage (tsinit, init, run, tsfinal, finalize), a subroutine:

```fortran
module ccpp_suite_A_physics_cap
  use scheme_module, only: scheme_run
  use host_module_A, only: ddt_A       ! DDT, not flat fields
  use host_module_B, only: ddt_B
  implicit none
  contains

  subroutine suite_A_physics_run_cap(ddt_A, ddt_B, im, iaend, ierr, ...)
    type(ddt_A_type), intent(inout), target :: ddt_A   ! entire DDT passed
    type(ddt_B_type), intent(inout), target :: ddt_B
    integer,          intent(in)            :: im, iaend  ! loop bounds
    integer,          intent(out)           :: ierr
    logical, save :: initialized(200) = .false.
    ! optional variable: local pointer, conditionally associated
    real(kind_phys), pointer :: opt_var(:) => null()
    if (ddt_A%active_flag) then
        opt_var => ddt_A%opt_field
    end if
    ! unit conversion: local variable
    real(kind_phys) :: converted_var(im)
    converted_var(:) = ddt_B%field(:im) * conversion_factor
    ! fixed-index extraction: local pointer for a specific tracer
    real(kind_phys), pointer :: q_water_vapor(:,:) => null()
    q_water_vapor => ddt_A%q(:,:,ntqv)   ! ntqv = water vapor index in tracer array
    ! call scheme with loop-bound application and extracted variables at the call site
    call scheme_run(                          &
        arg1    = ddt_A%field1(1:im),        &  ! horizontal loop-bound applied here
        arg2    = ddt_A%field2(1:im,:),      &  ! loop-bound + all levels
        qv      = q_water_vapor(1:im,:),     &  ! specific tracer, loop-bound applied
        arg3    = converted_var,             &  ! unit-converted local var
        opt_arg = opt_var,                   &  ! optional pointer
        ...)
    if (ierr /= 0) return
  end subroutine
end module
```

Key points:
- **DDTs are passed as arguments, not flat fields.** Hundreds of variables arrive as
  one or a small number of DDT arguments. This is the fundamental architectural choice
  that makes prebuild fast and safe with compiler debugging flags.
- **Two distinct "subsetting" operations happen at the scheme call site:**
  1. *Loop-bound application*: horizontal range `1:im` (or `im` for scalar extents)
     applied in the scheme call argument expressions.
  2. *Fixed-index extraction*: a specific element along one dimension is selected,
     e.g. `q_water_vapor => ddt%q(:,:,ntqv)` extracts the water vapor tracer from the
     full tracer array. A local pointer (or local variable for unit conversions) is
     declared just before the scheme call and passed as the scheme argument. The group
     cap always receives the full data; these extractions are local to the group cap.
- **Optional variables** are handled by declaring a local `pointer` variable and
  conditionally associating it with the DDT field based on the `active` expression.
  An unassociated pointer is passed to the scheme if the variable is inactive. This
  avoids compiler exceptions when mandatory debugging flags are enabled, because the
  unallocated field is never directly referenced — only the already-null pointer is.
- `logical :: initialized(200), save` — per-instance initialization tracking. The
  200 is the maximum number of complete model instances that can coexist in memory
  simultaneously (used in ensemble approaches where multiple copies of the full model
  state live in memory at once). Each instance has its own initialization flag.
- For the `run` phase, `im` and `iaend` (or similar) carry `horizontal_loop_begin`
  and `horizontal_loop_end`, enabling OpenMP thread-level parallelism where each
  thread processes a horizontal slice.
- Explicit keyword argument passing in scheme calls.
- Unit conversion: a local variable is declared and populated before the call; the
  local variable is then passed to the scheme.
- Error check after each scheme call; returns immediately on error.
- `--debug` flag inserts Fortran array-size assertions.

#### Suite cap: `ccpp_<suite>_cap.F90`

Imports all group cap functions and exposes one function per stage that chains group calls.

#### Static API: `ccpp_static_api[_<namespace>].F90`

A single Fortran module `ccpp_static_api` with one subroutine per stage:

```fortran
subroutine ccpp_physics_run(cdata, suite_name, group_name, ierr)
  character(len=*), intent(in) :: suite_name, group_name
  select case(trim(suite_name))
    case('suite_A')
      select case(trim(group_name))
        case('physics')
          call suite_A_physics_run_cap(cdata, ierr)
        ...
      end select
    ...
  end select
end subroutine
```

This is the **single entry point** the host model calls. The host model passes `suite_name`
and `group_name` at runtime; the static API dispatches to the appropriate cap function.

### 2.7 Build system snippet files generated

Six output files (Makefile, CMakefile, shell source) for three variable sets:

| File | Content |
|---|---|
| `CCPP_CAPS.cmake` | `set(CAPS /abs/path/cap1.F90 /abs/path/cap2.F90 ...)` |
| `CCPP_SCHEMES.cmake` | `set(SCHEMES /abs/path/scheme1.F90 ...)` |
| `CCPP_TYPEDEFS.cmake` | `set(TYPEDEFS module1 module2 ...)` (module names, not paths) |
| `CCPP_API.cmake` | `set(API /abs/path/ccpp_static_api.F90)` |

All files are written as `.tmp` first and compared against the existing version; they are
replaced only if the content changed, which avoids unnecessary recompilation of downstream
Fortran targets.

### 2.8 What `mkcap.py`, `mkstatic.py`, and `mkdoc.py` each do

**`mkcap.py`**:
- Defines the `mkcap.Var` class (prebuild's variable data class)
- Defines six file-writer classes: `CapsMakefile`, `CapsCMakefile`, `CapsSourcefile`,
  `SchemesMakefile`, `SchemesCMakefile`, `SchemesSourcefile`, `TypedefsMakefile`,
  `TypedefsCMakefile`, `TypedefsSourcefile`
- Each writer has a `write(file_list)` method that produces a formatted include file
- Does NOT generate any Fortran

**`mkstatic.py`**:
- Defines `Suite`, `Group`, `Subcycle` classes that parse suite definition XML and
  generate Fortran caps
- `Suite.parse()`: reads SDF XML via `xml.etree.ElementTree`, builds `Group` and
  `Subcycle` objects
- `Suite.write()`: drives cap generation for all groups and the suite-level cap
- `Group.write()`: generates the group cap Fortran — argument list construction,
  module `use` statements, unit conversion code, scheme calls, error handling
- Defines `API` class: generates the static API Fortran module (suite_name/group_name
  dispatch switch)
- `CCPP_SUITE_VARIABLES` dict: mandatory variables always included (error message,
  error code, loop counter, loop extent)
- Helper functions `extract_parents_and_indices_from_local_name()` and
  `extract_dimensions_from_local_name()` handle complex DDT member access like
  `Atm(blk_no)%q(:,:,:,graupel_index)` — these are critical for DDT-heavy host models

**`mkdoc.py`**:
- `metadata_to_html()`: produces an HTML table of all host-model provided variables
  (standard_name, long_name, units, rank, type, kind, source, local_name)
- `metadata_to_latex()`: produces a LaTeX table combining host-defined and scheme-requested
  variables, annotating which schemes use each variable and whether unit conversion is needed
- Informational outputs only; do not affect the build

---

## 3. ccpp-capgen — detailed analysis

### 3.1 Command-line arguments

Entry point: `scripts/ccpp_capgen.py`, `_main_func()`.
Arguments parsed via `framework_env.parse_command_line()` into a `CCPPFrameworkEnv` object:

| Argument | Required | Purpose |
|---|---|---|
| `--host-files` | yes | `.meta` files or `.txt` indirect file lists |
| `--scheme-files` | yes | Same format |
| `--suites` | yes | `.xml` SDF files or `.txt` lists |
| `--output-root` | no | Directory for generated files |
| `--host-name` | no | If given, generates a host cap |
| `--ccpp-datafile` | no | Path for datatable XML (default: `datatable.xml`) |
| `--kind-type` | no (repeatable) | Fortran kind mappings, syntax `<name>=[<module>:]<spec>`. Module defaults to `iso_fortran_env` for ISO_FORTRAN_ENV specs. Examples: `kind_phys=REAL64`, `kind_phys=my_host_kinds:kind_r8`. If omitted, `kind_phys=iso_fortran_env:REAL64` is injected. |
| `--preproc-directives` | no | Fortran preprocessor macros |
| `--use-error-obj` | no | Use error object instead of scalar error variables |
| `--force-overwrite` | no | Always regenerate output |
| `--clean` | no | Remove files listed in datatable and exit |
| `--verbose` | no (repeatable) | Increase log verbosity |

`CCPPFrameworkEnv` (defined in `framework_env.py`) consolidates all settings into typed
properties and stores a `kind_dict` mapping CCPP kind names to `[kind_spec, module]` pairs.

### 3.2 Step-by-step execution pipeline

```
1. create_file_list()
       expand .txt indirect file lists, validate .meta extensions

2. register_ddts(scheme_files)
       pre-scan all scheme .meta files
       register DDT type names via register_fortran_ddt_name()
       (so the host parser can recognize them as non-intrinsic types)

3. parse_host_model_files()
       for each host .meta file:
           metadata_table.parse_metadata_file() → [MetadataTable]
           find_associated_fortran_file()         → matching .F90 path
           parse_fortran_file()                   → Fortran declarations (via fortran_tools)
           check_fortran_against_metadata()       → cross-validation (type, kind, rank, intent)
       accumulate MetadataSection headers: DDT, module, host types

4. HostModel(table_dict, host_name, run_env)
       process DDT headers:  → DDTLibrary  +  ddt_dict (VarDictionary)
       process module/host headers: → main VarDictionary  +  __var_locations
       add ConstituentVarDict synthetically for ccpp_model_constituents_t

5. API(sdfs, host_model, scheme_headers, run_env)
       for each SDF XML:
           Suite construction:
               auto-create 5 phase groups: register, initialize, timestep_initial,
                   timestep_final, finalize
               parse <group> elements → Group objects (RUN_PHASE_NAME)
               parse <init>/<finalize> tags → Scheme objects in full-phase groups
           Suite.analyze(host_model, scheme_library, ddt_library, run_env):
               Group.analyze() → Scheme.analyze():
                   for each scheme argument:
                       VarDictionary.find_variable()  [scope chain search]
                       Var.compatible()               [→ VarCompatObj with transformations]
                       loop dim substitution for _run phase
                       register constituent if constituent=True
               variable promotion: group outputs → suite level if needed by later group

6. ccpp_api.write(outdir, run_env)
       suite cap .F90 per suite
       group caps (embedded or separate)
       host cap .F90  (if --host-name given)
       ccpp_kinds.F90

7. generate_ccpp_datatable()  → datatable.xml
```

### 3.3 Object hierarchy

```
API  (ccpp_suite.py)
  └── Suite  (extends VarDictionary)   [one per SDF XML]
        parent → ConstituentVarDict  (extends VarDictionary)
                   parent → API
        ├── Group  (suite_objects.py, extends VarDictionary)  [one per <group>]
        │     call_list: CallList  (extends VarDictionary)
        │     ├── Subcycle  (suite_objects.py)
        │     │     └── Scheme  (suite_objects.py, extends SuiteObject)
        │     └── Scheme  (for full-phase groups: init, register, etc.)
        └── (auto groups: register, initialize, timestep_initial,
                          timestep_final, finalize)

HostModel  (host_model.py, extends VarDictionary)
  ├── ddt_lib: DDTLibrary
  │     └── {ddt_name → MetadataSection}
  ├── ddt_dict: VarDictionary   (all DDT field variables, expanded)
  └── loop_vars: VarDictionary  (run-time dimension variables)

VarDictionary  (metavar.py)
  ├── {standard_name → Var}
  └── parent_dict → VarDictionary   ← scope chain for find_variable()

Var  (metavar.py)
  └── __prop_dict: {property_name → validated_value}

VarDDT  (ddt_library.py, extends Var)
  └── __field: Var | VarDDT   ← recursive DDT traversal chain
```

### 3.4 Variable matching — scope-chain and VarCompatObj

Unlike prebuild's single batch `compare_metadata()`, capgen performs incremental,
scope-aware matching during the suite analysis phase.

For each scheme argument in `Scheme.analyze()`:
1. `VarDictionary.find_variable(standard_name)` — searches scope chain:
   local group dict → suite dict → ConstituentVarDict → host model dict
2. `Var.compatible(other, run_env)` returns a `VarCompatObj` — not a bool.
   `VarCompatObj` carries:
   - Whether the variables are equivalent (no transformation needed)
   - Whether they are compatible with transformations (unit conversion, dimension
     substitution, `top_at_one` flip)
   - The reason for any incompatibility (for error messages)
3. For `_run` phase: `horizontal_dimension` is automatically substituted with
   `horizontal_loop_begin:horizontal_loop_end`
4. For `constituent = True` variables: auto-registered in `ConstituentVarDict`;
   allocation/management code is generated
5. Variable promotion: if a Group produces a variable needed by a later Group, it is
   promoted to Suite-level scope

`VarCompatObj` compatibility considers:
- Type equality
- Kind equality (with ISO kind aliases)
- Units compatibility (triggers unit conversion if compatible)
- Dimension substitutability (horizontal loop vs. full dimension, vertical extent)
- `top_at_one` orientation (triggers flip if needed)
- `protected` status (cannot be an output if protected)
- `CCPP_HORIZONTAL_DIMENSIONS`, `CCPP_VERTICAL_DIMENSIONS`, `CCPP_LOOP_DIM_SUBSTS`
  from `var_props.py`

### 3.5 `metavar.Var` properties

`metavar.Var` stores all properties in a validated `__prop_dict`. Properties:

**Specification properties** (all metadata contexts):

| Property | Type | Notes |
|---|---|---|
| `local_name` | str | Valid Fortran identifier |
| `standard_name` | str | CF-convention, lowercase+underscores |
| `long_name` | str | Human-readable description |
| `units` | str | Physical units string |
| `dimensions` | list | Dimension standard names or `()` |
| `type` | str | Intrinsic or registered DDT name |
| `kind` | str | Fortran kind parameter |
| `active` | str | Conditional allocation expression |
| `optional` | bool | Whether scheme can handle missing var |
| `protected` | bool | Cannot be written by schemes |
| `allocatable` | bool | Has ALLOCATABLE attribute |
| `state_variable` | bool | Persists across timesteps |
| `persistence` | str | `timestep` or `run` |
| `default_value` | str | Fortran expression |
| `diagnostic_name` | str | Diagnostic output name |
| `target` | bool | Has TARGET attribute |
| `polymorphic` | bool | CLASS(*) type |
| `top_at_one` | bool | Vertical ordering: top at index 1 |

**Scheme-only properties**:

| Property | Type | Notes |
|---|---|---|
| `intent` | str | in / out / inout |

**Constituent properties**:

| Property | Type | Notes |
|---|---|---|
| `constituent` | bool | Is a CCPP-managed constituent (tracer) |
| `advected` | bool | Is advected by the dynamical core |
| `molar_mass` | float | Molecular weight (positive) |

### 3.6 Capgen-only features

**Fortran cross-validation** (`check_fortran_against_metadata()`):
- Parses the actual `.F90` file alongside the `.meta` file
- Checks that every metadata entry matches the real Fortran declaration:
  variable count, local_name, type, kind, intent (for schemes), dimension rank/names
- Catches bugs where metadata was updated but the Fortran source was not (or vice versa)

**State machine** (`ccpp_state_machine.py`, `state_machine.py`):
- `CCPP_STATE_MACH`: a `StateMachine` instance with 6 transitions
- Valid state sequence: `register → uninitialized → initialized → in_time_step`
- Suite caps include a `character(len=16) :: ccpp_suite_state` variable
- State-checking code at the start of each phase function enforces correct call ordering
- `CCPP_STATE_MACH.function_match()` uses compiled regex to identify which CCPP phase
  a subroutine name belongs to

**Constituent variable support** (`constituents.py`):
- `ConstituentVarDict` (extends `VarDictionary`) manages traceable species (tracers)
- When a scheme declares `constituent = True`, `find_variable()` auto-creates the variable
- Allocation code for the constituent array is auto-generated
- Constants: `CONST_DDT_NAME = "ccpp_model_constituents_t"`,
  `CONST_PROP_TYPE = "ccpp_constituent_properties_t"`

**DDT library** (`ddt_library.py`):
- `VarDDT(Var)`: represents a DDT field variable at any nesting level
- Traversal chain: `VarDDT → VarDDT → ... → Var` (innermost is the actual leaf field)
- `DDTLibrary`: dictionary of DDT `MetadataSection` objects
- `collect_ddt_fields()` expands DDT variables into component fields in `ddt_dict`

**Host cap generation** (`host_cap.py`):
- Generated only when `--host-name` is given
- Produces `<host_model>_ccpp_cap.F90`
- Subroutines: `<host_model>_ccpp_physics_<stage>(api_vars)`
  that call into suite cap functions

**`ccpp_kinds.F90`**:
- Simple Fortran module `ccpp_kinds`. **Always generated**, even when no `--kind-type`
  is supplied (in that case `kind_phys=iso_fortran_env:REAL64` is injected
  automatically and an INFO log line is emitted).
- One `use <module>, only: <specs>` line per module (modules sorted; specs deduped per
  module). Each kind is then re-exported as
  `integer, parameter, public :: <name> = <spec>`.
- Supports host-supplied kind modules: `--kind-type kind_phys=my_host_kinds:kind_r8`
  emits `use my_host_kinds, only: kind_r8` and
  `integer, parameter, public :: kind_phys = kind_r8`.
- Listed in `<ccpp_files><utilities>` of `datatable.xml` (matches original capgen) so
  the build system picks it up via `ccpp_datafile.py --ccpp-files`.
- USEd by all generated Fortran files that declare kind-typed variables — the group
  cap, the suite types module, and the suite data module. The static API and suite
  cap have no kind references and do not USE it.

**Datatable XML** (`ccpp_datafile.py`):
- Produced after generation; lists all generated files, scheme entries, variable properties,
  suite configurations
- Queryable by the build system via `ccpp_datafile.py <datatable> --suite-files` etc.
- Supports `--clean` workflow: reads the file list, removes all generated files, deletes itself
- `DatatableReport` class provides a programmatic query API

**In-memory database** (`ccpp_database_obj.py`):
- `CCPPDatabaseObj`: wraps `HostModel` and `API` for programmatic access to capgen results
- Returned when `capgen()` is called with `return_db=True`
- Provides `host_model_dict()`, `suite_list()`, `constituent_dictionary(suite)`

**Variable tracking tool** (`ccpp_track_variables.py`):
- Standalone diagnostic: traces a specific variable through a suite, showing which schemes
  use it and with what intent
- Uses prebuild's `import_config` and capgen's `Suite`/`parse_metadata_file` together

**Fortran-to-metadata tool** (`ccpp_fortran_to_metadata.py`):
- Standalone utility: parses annotated Fortran source files and generates skeleton `.meta`
  files — used to bootstrap new scheme metadata

---

## 4. Shared infrastructure

### 4.1 Module sharing map

| Module | Used by prebuild | Used by capgen | Notes |
|---|---|---|---|
| `metadata_parser.py` | yes | partial | **Bridge module**: calls capgen's parser, returns mkcap.Var |
| `metadata_table.py` | via bridge | yes (primary) | Native `.meta` format parser |
| `metavar.py` | no | yes | Primary `Var` class, `VarDictionary` |
| `var_props.py` | no | yes | `VariableProperty`, `VarCompatObj`, dimension constants |
| `mkcap.py` | yes | no | `mkcap.Var` class + build-snippet writers |
| `mkstatic.py` | yes | no | Suite/Group/API Fortran generators |
| `mkdoc.py` | yes | no | HTML/LaTeX documentation generators |
| `common.py` | yes | partial | `CCPP_STAGES`, container encoding |
| `framework_env.py` | dummy instance | yes (primary) | `CCPPFrameworkEnv` |
| `file_utils.py` | no | yes | `create_file_list`, `move_modified_files` |
| `code_block.py` | no | yes | Structured Fortran output |
| `ddt_library.py` | no | yes | `DDTLibrary`, `VarDDT` |
| `host_model.py` | no | yes | `HostModel` class |
| `host_cap.py` | no | yes | Host cap generation |
| `ccpp_suite.py` | no | yes | `Suite`, `API` classes |
| `suite_objects.py` | no | yes | `Scheme`, `Group`, `Subcycle`, `CallList` |
| `constituents.py` | no | yes | `ConstituentVarDict` |
| `ccpp_datafile.py` | no | yes | Datatable XML |
| `ccpp_database_obj.py` | no | yes | `CCPPDatabaseObj` |
| `ccpp_state_machine.py` | no | yes | `CCPP_STATE_MACH` |
| `state_machine.py` | no | yes | `StateMachine` base class |
| `ccpp_fortran_to_metadata.py` | no | yes | Fortran→metadata bootstrap tool |
| `ccpp_track_variables.py` | partial | partial | Uses both worlds |

**The key architectural debt**: `metadata_parser.py` is a prebuild module that internally
calls capgen's `metadata_table.parse_metadata_file()` and converts results to `mkcap.Var`
objects. This creates a one-way dependency (prebuild → capgen's parser infrastructure)
while presenting a prebuild-style API to `ccpp_prebuild.py`. It exists only because
prebuild predates the `.meta` format.

### 4.2 The `.meta` file format

The `.meta` format is the native format for capgen and the expected format for all new
scheme development. The Fortran source file contains a comment hook pointing to the `.meta`
file:

```fortran
!! \section arg_table_scheme_name_run  Argument Table
!! \htmlinclude scheme_name_run.html
```

The `.meta` file itself uses an INI-style format:

```ini
[ccpp-table-properties]
  name = scheme_name
  type = scheme
  source_path = ../src
  dependencies_path = ../some/path
  dependencies = utility_module.F90, another.F90

[ccpp-arg-table]
  name = scheme_name_run
  type = scheme
[ im ]
  standard_name = horizontal_dimension
  long_name = horizontal dimension
  units = count
  type = integer
  dimensions = ()
  intent = in
[ dz ]
  standard_name = layer_thickness
  long_name = thickness of each model layer
  units = m
  type = real
  kind = kind_phys
  dimensions = (horizontal_dimension, vertical_layer_dimension)
  intent = in
```

Multiple `[ccpp-arg-table]` sections are allowed in a scheme file (one per phase:
`_init`, `_run`, `_finalize`, `_timestep_init`, `_timestep_finalize`).
Singleton tables (DDT, module, host) allow only one section.

The three table-level properties in `[ccpp-table-properties]` that carry path information:

| Property | Purpose | Resolution |
|---|---|---|
| `source_path` | Relative path from the `.meta` file's directory to the directory containing the corresponding `.F90` Fortran source file | `os.path.normpath(os.path.join(meta_dir, source_path))`. Defaults to `meta_dir` when absent. |
| `dependencies_path` | Optional subdirectory relative to `meta_dir`; used as the base directory for resolving entries in `dependencies` | `os.path.normpath(os.path.join(meta_dir, dependencies_path))`. Defaults to `meta_dir` when absent. |
| `dependencies` | Comma-separated list of dependency file names or relative paths | Each entry resolved via `os.path.normpath(os.path.join(dep_base, entry))` where `dep_base` is the resolved `dependencies_path`. The value `none` is ignored. |

**Implementation note — `flush_table_props` pattern:** The INI parser processes the
`[ccpp-table-properties]` and `[ccpp-arg-table]` headers in one streaming pass. Extra
table-level keys (`source_path`, `dependencies_path`, `dependencies`) are collected in a
`pending_props` dict alongside `name` and `type`. The parser must apply these properties
to the `MetadataTable` object — via a `flush_table_props()` call — at every
state-transition point (first `[ccpp-arg-table]` header, next `[ccpp-table-properties]`
header, and end-of-file) **before** resetting `pending_props`. Without this, the extra
properties are silently discarded.

### 4.3 Variable property validation (`var_props.py`)

`VariableProperty` encapsulates a single metadata property with its name, Python type,
optionality, default, valid-value constraints, and a check function. Check functions used:

| Checker | What it validates |
|---|---|
| `check_local_name` | Valid Fortran identifier |
| `check_cf_standard_name` | Lowercase, underscores, alphanumeric only |
| `check_fortran_type` | Intrinsic type or registered DDT name |
| `check_units` | Valid unit string (normalizes `+` in exponents) |
| `check_dimensions` | Valid dimension specification |
| `check_default_value` | Valid Fortran expression |
| `check_molar_mass` | Positive float (for constituents) |

`CCPP_HORIZONTAL_DIMENSIONS`, `CCPP_VERTICAL_DIMENSIONS`, `CCPP_LOOP_DIM_SUBSTS`
in `var_props.py` define the recognized dimension forms and the run-time substitution
map (e.g., `horizontal_dimension → horizontal_loop_begin:horizontal_loop_end`).

---

## 5. Feature comparison

| Feature | prebuild | capgen | Notes |
|---|---|---|---|
| **Input formats** | | | |
| Native `.meta` format | via bridge | yes | |
| Old pipe-delimited format | deprecated warn | not supported | |
| **Parsing and validation** | | | |
| Fortran source cross-validation | no | yes | capgen parses actual .F90 to cross-check |
| Preprocessor directive support | no | yes | `--preproc-directives` |
| **Variable handling** | | | |
| Variable data class | `mkcap.Var` (flat attrs) | `metavar.Var` (validated prop dict) | |
| Scope-chain variable search | no | yes | group→suite→constituent→host |
| Variable promotion group→suite | no | yes | |
| Unit conversion | yes | yes | |
| Optional/active variables | yes (fully) | yes | both: local pointer + conditional ASSOCIATE |
| DDT library (first-class) | no | yes | `VarDDT` recursive chain |
| **Suite and cap generation** | | | |
| Suite definition (SDF XML) | yes | yes | Same XML format |
| Subcycle loops | yes | yes | |
| State machine in generated caps | no | yes | Runtime state enforcement |
| Static API module (dispatch switch) | yes | no | `ccpp_static_api.F90` |
| Host cap generation | no | yes | `<host>_ccpp_cap.F90` |
| `ccpp_kinds.F90` | no | yes | |
| **Constituent/tracer support** | | | |
| Constituent variable management | no | yes | Auto-allocation, `ConstituentVarDict` |
| **Build system output** | | | |
| CMake/Makefile file-list snippets | yes | no | Six snippet files |
| Datatable XML (queryable) | no | yes | `ccpp_datafile.py` |
| Clean via datatable | no | yes | |
| **Documentation** | | | |
| HTML variable table | yes | no (stub, raises error) | `mkdoc.metadata_to_html` |
| LaTeX variable table | yes | no | `mkdoc.metadata_to_latex` |
| **Developer tools** | | | |
| Variable tracking diagnostic | yes | no | `ccpp_track_variables.py` |
| Fortran-to-metadata bootstrap | no | yes | `ccpp_fortran_to_metadata.py` |
| **Runtime API** | | | |
| In-memory database object | no | yes | `CCPPDatabaseObj` |
| **Debug / developer aids** | | | |
| Debug array-size checks in caps | yes (`--debug`) | no | |
| Namespace suffix for API name | yes (`--namespace`) | no | |
| **Configuration** | | | |
| Config mechanism | Python module (flexible) | CLI args only | |

**Known gaps and corrections:**

- Capgen's `--generate-docfiles` is declared in the CLI but raises
  `CCPPError("not yet supported")` — documentation generation is unimplemented.
- Prebuild handles `TYPEDEFS_NEW_METADATA` for mixed old/new metadata deployments;
  capgen has no equivalent because it only accepts the new format.
- Capgen validates Fortran source against metadata; prebuild trusts metadata and never
  reads Fortran code.
- Capgen has no `--namespace` equivalent for the generated API module name.
- Capgen's `CCPPDatabaseObj` and datatable XML allow programmatic querying; prebuild
  has no equivalent.
- Prebuild's static API pattern (single Fortran module with runtime dispatch) is absent
  from capgen, which uses a different host-cap integration model.
- **Capgen cannot pass DDTs to group caps** — it passes everything as flat fields.
  Despite considerable effort by multiple developers, this has not been fixed. This is
  the primary reason capgen is being abandoned.
- **Capgen does not support multiple model instances in memory** (ensemble approach).
  Prebuild's `initialized(200)` array handles this correctly.
- **Capgen does not own or allocate any data.** Wait — this is a prebuild characteristic.
  Capgen *does* allocate data for physics-internal variables (variables used only within
  the physics, not provided by the host model) at the suite level. Prebuild requires the
  host model to provide and own all data, including any physics-internal scratch space.

---

## 6. Build system integration

### 6.1 How a host model invokes ccpp-prebuild

Direct call (as in the test suite):
```bash
python ../../scripts/ccpp_prebuild.py \
    --config=ccpp_prebuild_config.py \
    --builddir=build \
    --suites=suite_A,suite_B \
    [--debug] [--namespace mymodel]
```

Typical CMake integration:
```cmake
# Run prebuild at configure time
execute_process(
    COMMAND ${Python3_EXECUTABLE}
            ${CCPP_FRAMEWORK}/scripts/ccpp_prebuild.py
            --config=${HOST_CCPP_PREBUILD_CONFIG}
            --builddir=${CMAKE_CURRENT_BINARY_DIR}
            --suites=${CCPP_SUITES}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    RESULT_VARIABLE PREBUILD_RESULT
)
if(NOT PREBUILD_RESULT EQUAL 0)
    message(FATAL_ERROR "ccpp_prebuild.py failed")
endif()

# Consume the generated snippet files
include(${CMAKE_CURRENT_BINARY_DIR}/CCPP_CAPS.cmake)     # → ${CAPS}
include(${CMAKE_CURRENT_BINARY_DIR}/CCPP_SCHEMES.cmake)  # → ${SCHEMES}
include(${CMAKE_CURRENT_BINARY_DIR}/CCPP_TYPEDEFS.cmake) # → ${TYPEDEFS}
include(${CMAKE_CURRENT_BINARY_DIR}/CCPP_API.cmake)      # → ${API}

add_library(ccpp_physics OBJECT ${CAPS} ${SCHEMES} ${API})
```

### 6.2 How a host model invokes ccpp-capgen

Direct call:
```bash
python scripts/ccpp_capgen.py \
    --host-files host_data.meta,host_model.meta \
    --scheme-files scheme1.meta,scheme2.meta \
    --suites suite_A.xml,suite_B.xml \
    --output-root ${BUILD_DIR}/ccpp \
    --host-name my_host \
    --kind-type kind_phys=REAL64 \
    --ccpp-datafile ${BUILD_DIR}/ccpp/datatable.xml
```

Typical CMake integration:
```cmake
# Run capgen at configure time
execute_process(
    COMMAND ${Python3_EXECUTABLE}
            ${CCPP_FRAMEWORK}/scripts/ccpp_capgen.py
            --host-files ${HOST_META_FILES}
            --scheme-files ${SCHEME_META_FILES}
            --suites ${SUITE_SDFS}
            --output-root ${CMAKE_CURRENT_BINARY_DIR}/ccpp
            --host-name ${HOST_MODEL_NAME}
            --ccpp-datafile ${CMAKE_CURRENT_BINARY_DIR}/ccpp/datatable.xml
    RESULT_VARIABLE CAPGEN_RESULT
)

# Query the datatable for generated file lists
execute_process(
    COMMAND ${Python3_EXECUTABLE}
            ${CCPP_FRAMEWORK}/scripts/ccpp_datafile.py
            ${CMAKE_CURRENT_BINARY_DIR}/ccpp/datatable.xml
            --suite-files
    OUTPUT_VARIABLE SUITE_CAPS OUTPUT_STRIP_TRAILING_WHITESPACE
)
execute_process(
    COMMAND ${Python3_EXECUTABLE}
            ${CCPP_FRAMEWORK}/scripts/ccpp_datafile.py
            ${CMAKE_CURRENT_BINARY_DIR}/ccpp/datatable.xml
            --host-files
    OUTPUT_VARIABLE HOST_CAP OUTPUT_STRIP_TRAILING_WHITESPACE
)

add_library(ccpp_physics OBJECT ${SUITE_CAPS} ${HOST_CAP})
```

### 6.3 Available datatable query flags

```
--host-files        → generated host cap .F90 files
--suite-files       → generated suite cap .F90 files
--utility-files     → generated utility .F90 files (e.g. ccpp_kinds.F90)
--ccpp-files        → all generated .F90 files
--process-list      → physics process types in the suite
--module-list       → Fortran module names needed
--dependencies      → scheme dependency files
--suite-list        → configured suite names
--required-variables → variables required by all suites
--input-variables   → input-only variables for a suite
--output-variables  → output variables for a suite
--host-variables    → variables provided by the host model
```

---

## 7. Key architectural differences

### 7.1 Data model

| Dimension | ccpp-prebuild | ccpp-capgen |
|---|---|---|
| Variable representation | `mkcap.Var` with plain Python attributes | `metavar.Var` with validated `__prop_dict` |
| Variable storage | Two flat `OrderedDict`s | Scope-chain `VarDictionary` tree |
| Container encoding | Encoded string: `MODULE_foo SCHEME_bar SUBROUTINE_baz` | Explicit class hierarchy |
| DDT handling | Encoded as string in `local_name`; helper regexes to extract | First-class `VarDDT` recursive chain |
| Variable matching | One batch `compare_metadata()` call | Incremental during suite analysis |
| Matching result | `bool` + side effects on `.target` / `.actions` | Rich `VarCompatObj` with transformation info |
| **Cap argument style** | **DDTs passed to group caps** | **Flat fields passed to group caps** |
| Subsetting location | At the scheme call site inside the group cap | Done at a higher level, before group cap |
| Data ownership | Host model owns all data including physics-internal | Capgen allocates physics-internal suite-level data |
| Multiple model instances | Yes — `initialized(200)` array, one flag per instance | No |
| Optional variable handling | Local pointer, conditionally associated | Same mechanism, but blocked by flat-field issue |

### 7.2 Error handling

| Aspect | ccpp-prebuild | ccpp-capgen |
|---|---|---|
| Style | `(success, result)` tuples + `logging.error()` | `CCPPError` / `ParseInternalError` exceptions |
| Collection | Errors accumulate via `logging`; `main()` checks success | Raised immediately at point of detection |
| Location info | Filename from context; line numbers sometimes | `ParseContext` objects with file + line number |
| User errors vs bugs | Not distinguished | `CCPPError` (user) vs `ParseInternalError` (programmer) |

### 7.3 Extensibility

| Aspect | ccpp-prebuild | ccpp-capgen |
|---|---|---|
| New metadata property | Add to `VALID_ITEMS` dict + `mkcap.Var` attribute | Add one `VariableProperty` entry + checker fn |
| New CCPP phase | Update `CCPP_STAGES` + regenerate static API template | Add one transition tuple to `CCPP_STATE_MACH` |
| New compatibility rule | Modify `var.compatible()` in `mkcap.py` | Extend `VarCompatObj` in `var_props.py` |
| New host model | Write a new Python config file | New `.meta` files + CLI invocation |

### 7.4 Performance

Prebuild generates caps for multiple suites in seconds. Capgen, on the same suite set
with the same physics, takes more than 10 minutes. Two independent causes:

**Cause 1 — Repeated scope-chain traversal.** Every variable lookup in capgen traverses
a five-level `VarDictionary` parent chain (group → suite → constituent dict → host model
→ DDT dict) for every scheme argument in every group in every suite. Prebuild's
`compare_metadata()` does one flat dict lookup per standard name, once, and caches the
result in `var.container` and `var.target`. All subsequent use during Fortran generation
reads these cached attributes directly.

**Cause 2 — Flat-field cap arguments.** This is likely the dominant cost. Capgen resolves
every scheme argument down to its individual flat field, generates a `use` statement and
an explicit argument for each one, and emits them in the generated Fortran. A DDT with
200 fields becomes 200 individual argument declarations, 200 `use` statements, and 200
argument positions in the scheme call. Prebuild passes the DDT itself — one argument,
one `use` statement — and then subsets at the call site.

**Consequence for correctness.** Passing flat fields in capgen also breaks optional
variable handling under Fortran compiler debugging flags. When a field inside a DDT is
conditionally allocated (optional), passing it as a flat field requires dereferencing the
DDT to extract the field — which the compiler will flag as an error if debugging is on
and the field happens to be unallocated. Prebuild avoids this entirely by passing the
DDT and using a local pointer at the scheme call site.

### 7.5 Team comprehension and maintainability

This is the critical real-world difference. `ccpp-prebuild` is understood by the whole
team because it is procedural Python: you can read `ccpp_prebuild.py` top-to-bottom and
follow what happens. The data structures are flat dicts; the control flow is linear.

`ccpp-capgen` has a five-level class hierarchy, scope-chain dictionary lookups,
`VarCompatObj` carrying transformation state, `ConstituentVarDict` as a pluggable
scope-chain node, and a `StateMachine` with regex-based dispatch. No remaining team
member fully understands all of it. Development is extremely slow and risky.

The failed effort to make capgen pass DDTs instead of flat fields is the concrete proof
point: three developers spent considerable time and could not fix it without fully
understanding the interplay between `VarDDT`, `DDTLibrary`, `VarDictionary` scope chains,
and the Fortran writer. This is the proximate reason for the redesign.

---

## 8. Design considerations for the redesign

The following observations from this analysis should inform the redesign:

### 8.1 What to keep from prebuild
- Procedural, top-down control flow — easy to read and debug
- Config file as a Python module — extremely flexible without adding CLI arguments
- The static API pattern (`ccpp_static_api.F90` with runtime suite/group dispatch) —
  proven, simple integration for the host model
- **DDT arguments in group caps** — pass DDTs, not flat fields; this is the core correctness
  and performance requirement
- **Subsetting at the scheme call site** — group caps always receive full data; loop-bound
  application and fixed-index extraction happen in the individual scheme call expressions
  or via a local variable/pointer declared just before the call
- **Optional variable pattern** — local pointer declared in the group cap, conditionally
  associated based on the `active` expression, then passed to the scheme; this is safe
  under all compiler debugging modes
- The `initialized(N)` per-instance tracking — handles multiple simultaneous model
  instances in memory (ensemble approach); `N` is the max number of instances
- **Framework-owned data needs a simpler design** — capgen's variable promotion and
  `ConstituentVarDict` scope-chain approach is too complex; a cleaner mechanism for
  framework-allocated physics-internal data is needed (to be designed)
- HTML and LaTeX documentation generation
- The six CMake/Makefile/shell snippet output files — simple and direct (can be revisited)

### 8.2 What to keep from capgen
- Native `.meta` file parsing (eliminate the `metadata_parser.py` bridge entirely)
- Fortran source cross-validation (`check_fortran_against_metadata()`) — catches real bugs
- Rich compatibility reporting (`VarCompatObj`-style) — better error messages
- `ccpp_kinds.F90` generation — important for portability
- Datatable XML as output accounting (strictly better than six include files)
- `--preproc-directives` support
- Constituent variable support (needed for CAM-SIMA)
- State machine enforcement (optional feature, but architecturally clean)

### 8.3 What to eliminate
- The `mkcap.Var` / `metavar.Var` duality — one variable class, natively reading `.meta`
- The `metadata_parser.py` bridge module — it exists only because of the old format
- The scope-chain `VarDictionary` hierarchy — replace with flat, explicit lookup:
  one host dict, one scheme dict; no parent-chain traversal
- The five-level class inheritance (Suite → VarDictionary → ParseSource → ...)
- `ConstituentVarDict` as a scope-chain node — a simple explicit constituent registry suffices
- Capgen's variable promotion (group → suite level) — this complexity exists only because
  capgen allocates physics-internal data; if the host always owns all data, promotion
  is unnecessary
- Capgen's flat-field cap generation — DDT arguments must be the foundation

### 8.4 Framework-owned data — open design question

Capgen's variable promotion mechanism (promoting a variable from group scope to suite scope
when a later group needs it) and the `ConstituentVarDict` complexity exist because capgen
allocates and manages physics-internal data — variables used only within the physics,
not visible to the host model. This capability is **wanted** in the redesign: the host
model should not have to declare and own scratch variables that are purely internal to the
physics.

The problem is not the concept but the implementation. Capgen's approach — weaving
framework-allocated variables into the `VarDictionary` scope chain and promoting them
upward — produces the complexity that made capgen unmaintainable.

**Open question for the redesign:** What is a simpler mechanism for the framework to
allocate, own, and pass physics-internal variables? Candidate approaches (to be evaluated
with real-world examples):

- A completely separate, flat "framework data" dictionary, distinct from the host variable
  lookup, populated during analysis and passed explicitly to the caps as a dedicated
  argument (e.g., a framework-managed DDT or allocatable array container).
- A simplified promotion concept: variables are statically promoted to the widest scope
  that needs them during the analysis phase, but stored in a simple flat dict rather than
  via a scope-chain lookup.
- Constituent variables (tracers) as a special sub-case with their own well-defined
  allocation interface, separate from generic physics-internal data.

This question will be revisited once real-world examples clarify how many and what kind of
physics-internal variables actually need to be managed.

### 8.5 Critical design decisions for the redesign prompt

1. **DDT cap arguments are non-negotiable.** Group caps must receive DDTs. The entire
   subsetting, optional-variable, and performance story depends on this.

2. **Data ownership**: host-owns-all (prebuild model) vs. generator-allocates-internals
   (capgen model). This single decision determines whether variable promotion and
   suite-level allocation are needed.

3. **Integration pattern**: static API (prebuild style, `suite_name` + `group_name` dispatch)
   vs. host cap (capgen style, separate host-side Fortran glue). Models currently using
   each pattern depend on it.

4. **Config mechanism**: Python module (prebuild style, flexible) vs. pure CLI + file lists
   (capgen style, scriptable). The Python module config is very powerful for complex models.

5. **DDT member access parsing**: `extract_parents_and_indices_from_local_name()` and
   `extract_dimensions_from_local_name()` in `mkstatic.py` handle expressions like
   `Atm(blk_no)%q(:,:,:,graupel_index)`. The redesign needs a clean, explicit design for
   parsing and emitting these — not an afterthought regex patch.

6. **Output accounting**: datatable XML (capgen) is the right answer. The six CMake snippet
   files (prebuild) are redundant and harder to extend.

7. **Multiple model instances**: the redesign must preserve the `initialized(N)` pattern
   or an equivalent. The value of `N` may need to be configurable.

8. **Backward compatibility of generated Fortran interfaces**: real-world model examples
   will define exactly which naming conventions, argument orders, and module structures the
   host models depend on.

### 8.6 Implementation decisions made during redesign

The following decisions were made during implementation of `capgen` and are recorded
here as amendments to the analysis above.

**State machine parameters are local to each generated group cap module.**  
The original redesign prompt described the integer state constants as coming from a
shared framework library module. In practice they are generated as `private` named
parameters directly inside each group cap module:

```fortran
integer, parameter, private :: CCPP_GROUP_UNINITIALIZED = 0
integer, parameter, private :: CCPP_GROUP_INITIALIZED   = 1
integer, parameter, private :: CCPP_GROUP_IN_TIMESTEP   = 2
```

This keeps generated files self-contained — no implicit dependency on a framework
runtime library at the caps level. The values are replicated across all generated group
cap files, but the names are the contract.

**`source_path` is used by the validator, not the generator.**  
The generator trusts metadata and never opens Fortran source files. `source_path` is
meaningful only to the standalone validator tool, which uses it to auto-discover the
`.F90` file paired with each `.meta` file (same base name, different directory).

**`dependencies` paths are written to `datatable.xml`.**  
The resolved absolute paths from each scheme's `dependencies` table-level property are
collected and written to the `<dependencies>` section of `datatable.xml`, sorted and
deduplicated. The CMake build system reads these via `ccpp_datafile.py` to add external
dependency files to the build graph.

**Optional variable (pointer wrapper) implementation decisions.**  
Optional arguments (Case 2 and Case 4) use per-suite Fortran derived types for pointer
wrappers. All unique `(type, kind, rank)` combinations needed by any optional arg across
all groups in a suite are collected and written to `ccpp_<suite>_types.F90`. Each type
name is generated as `{type}_{kind}_rank{N}_ptr_type` (e.g. `real_kind_phys_rank1_ptr_type`).
Group cap modules `USE` this file. The types file is omitted entirely when no optional
args exist in the suite. The active condition for a pointer assignment is inherited from
the **host variable's** `active` attribute when the scheme itself specifies no `active`.

**Character length (`len=N` / `len=*`) rules.**  
Character kind declarations follow specific compatibility rules enforced by the resolver:

- `len=*` is valid only where a character variable is **passed**, never where its
  storage is **defined**.  Host and DDT metadata must give every character variable
  a concrete `len=N`; so must the first `intent=out` scheme that defines a
  suite-owned character variable (it freezes the storage the framework allocates in
  `ccpp_<suite>_data`).  `len=*` in any of those positions is a **metadata error**.
  Control variables are exempt — they are pass-through dummy arguments the caps
  declare `character(len=*)`.
- `len=*` in a **consuming/later** scheme is always compatible with the defining
  `len=<N>` — assumed-length dummy arguments accept any declared length. No transform.
- Matching specific `len=N` on both sides requires no transform (naturally equal).
- Mismatched specific lengths (`len=512` definer vs `len=128` consumer) are a
  **metadata error**; the consuming scheme must declare `len=*` or match exactly.

The resolver raises `CCPPError` for the illegal cases. No kind transform is ever generated
for character variables — lengths are a Fortran compatibility constraint, not a unit conversion.

**`source_path` is used by the validator, not the generator.**  
The group cap's `state_alloc` subroutine always takes `number_of_instances` as an
explicit `intent(in)` integer argument — it never USEs any host module to obtain it.
The call chain is: `ccpp_init` → `<suite>_init` → each group's `state_alloc`. At each
level the argument is conditional:

- **Multi-instance host** (`number_of_instances` declared in host metadata with local
  name e.g. `ninstances`):
  - `ccpp_init(suite_name, ninstances, errmsg, errflg)` — static API receives it
  - `<suite>_init(ninstances, errmsg, errflg)` — suite cap threads it through
  - `state_alloc(ninstances, errmsg, errflg)` — group cap allocates array of that size
- **Single-instance host** (no `number_of_instances` in host metadata):
  - All three signatures omit the argument
  - `state_alloc(1, errmsg, errflg)` — the literal `1` is passed

State array **indexing** uses `instance_number`'s local name (e.g. `inst_num`) from
the control metadata. For single-instance hosts the literal `1` is used. `instance_number`
is injected into the group cap's `_init` and `_final` subroutine signatures even when no
scheme in those phases uses it — the state guard and state transition require it:

```fortran
subroutine ccpp_<suite>_<group>_init(inst_num, ...)
  if (ccpp_group_state(inst_num) >= CCPP_GROUP_INITIALIZED) return
  ! ... scheme _init calls ...
  ccpp_group_state(inst_num) = CCPP_GROUP_INITIALIZED
```

This injection does **not** happen for `_run`, `_timestep_init`, or `_timestep_final`
unless a scheme in those phases explicitly requests `instance_number`. The suite cap's
`<suite>_physics_init` and `<suite>_physics_final` dispatch subroutines similarly pass
`instance_number` to the group cap calls when the host provides it.

**Control variable validation — flat unconditional required set.**  
All required control variables (`suite_name`, `horizontal_loop_begin`, `horizontal_loop_end`,
`thread_number`, `number_of_threads`, `number_of_physics_threads`, `ccpp_error_code`,
`ccpp_error_message`, `instance_number`) are unconditional — every host must declare all
of them. Single-threaded or single-instance models pass `1` or `''` for any they don't
actively use. The generator validates the complete set after parsing host metadata,
collects all missing-variable errors together, and halts before emitting any code.
`instance_number` in particular is NOT conditional on `instance_dimension` usage —
it is always required.

**`group_name` is conditionally included, not in the required set.**  
`group_name` is included in the static API signature only if the host declares it in
their `type=control` table. When absent, the cap calls all groups unconditionally. The
generator warns (not errors) if `group_name` is absent and any suite has multiple groups.
When present: a required (non-optional) character argument; `''` or `'all'` calls all
groups in order; any other value dispatches to the named group only.

**`horizontal_loop_extent` eliminated; schemes always use `horizontal_dimension`.**  
Scheme metadata always declares `horizontal_dimension` as the horizontal extent
dimension, regardless of phase. There is no `horizontal_loop_extent` standard name in
the new design. The distinction between run-phase chunked processing and full-domain
init/final processing is handled entirely at the host level — the host passes actual
chunk bounds to `ccpp_physics_run` and `1`/`ncols` to all other phases. The cap always
generates `(horizontal_loop_begin:horizontal_loop_end)` for scheme call-site array
slices. For suite-owned array allocation sizing, `horizontal_dimension` from the host
`type=host` table (module USE) is used directly. This separation means allocation
correctness does not depend on the host passing any particular control variable values.

**Uniform signature across all `ccpp_physics_*` entry points.**  
All five physics entry points (`ccpp_physics_init`, `ccpp_physics_timestep_init`,
`ccpp_physics_run`, `ccpp_physics_timestep_final`, `ccpp_physics_final`) share the
same control argument set. No per-phase signature variations. `horizontal_loop_begin`
and `horizontal_loop_end` are in scope for all phases — a `_init` scheme that declares
`horizontal_dimension` correctly receives `(lb:ub)` slicing just as a `_run` scheme
would, with the host responsible for passing the right values.

**Both `ccpp_physics_final` and `ccpp_final` are silently idempotent.**  
Symmetric to `ccpp_physics_init`'s silent skip when already `INITIALIZED`,
both final-path entry points return cleanly with `errflg=0` on every repeat
invocation. Three cap levels participate:

- The suite-cap `<suite>_physics_final` dispatcher silent-returns when
  `ccpp_suite_state` is unallocated (last-instance post-`ccpp_final` deallocation)
  or `ccpp_suite_state(inst_num) == CCPP_SUITE_UNREGISTERED` (any other
  instance post-`ccpp_final`). The `state /= FRAMEWORK_INITIALIZED` error is
  retained so calling `physics_final` after only `ccpp_register` (no `ccpp_init`)
  still errors.
- The group-cap `<group>_final` entry guard silent-returns when
  `ccpp_group_state(inst_num) == CCPP_GROUP_UNINITIALIZED`. (Since `UNINITIALIZED`
  is the only value `< INITIALIZED`, the previously generated error block became
  unreachable and is no longer emitted.)
- The suite-cap `<suite>_final` body itself silent-returns on the same two
  conditions (unallocated state array, or `== UNREGISTERED` for this instance).
  After the first call's last-to-leave block deallocates `ccpp_suite_state`,
  the unallocated state *is* the normal post-final condition — so on a
  single-instance host the second call would otherwise trip a misleading
  "`ccpp_register` has not been called" error.

`<suite>_init` is intentionally *not* made idempotent on unallocated — there,
the unallocated state really does mean "you forgot `ccpp_register`", and
emitting an error is the correct behavior.

The other physics phases (`init`, `timestep_init`, `run`, `timestep_final`) are
unchanged — they still hard-error with `errflg=1` on any state mismatch.

---

## 9. Real-world example: CCPP Single Column Model (SCM)

*Source:* `EXT/ccpp-scm/` — uses `ccpp-prebuild`.

The SCM is a horizontally degenerate model (always `im = 1`, no OpenMP threading) but
it compiles the largest set of suites in the CCPP ecosystem, making it the most complete
real-world picture of what prebuild must handle.

**Scale:** 63 suites, 257 scheme files (137 scheme entries in config, many containing
multiple modules), 300 generated cap files, ~1,200+ host model variables, ~550 optional
(conditionally active) variables.

---

### 9.1 The `TYPEDEFS_NEW_METADATA` bridge — the DDT accessor map

This is the most important SCM-specific configuration. It maps each DDT type name to the
Fortran expression used to access an instance of that type from the host model's top-level
scope. It is what allows the code generator to convert a `local_name` like `tgrs` (declared
inside `GFS_statein_type`) into the cap argument expression
`physics%Statein%tgrs(...)`.

```python
TYPEDEFS_NEW_METADATA = {
    'GFS_typedefs': {
        'GFS_diag_type'    : 'physics%Diag',
        'GFS_control_type' : 'physics%Model',
        'GFS_cldprop_type' : 'physics%Cldprop',
        'GFS_tbd_type'     : 'physics%Tbd',
        'GFS_sfcprop_type' : 'physics%Sfcprop',
        'GFS_coupling_type': 'physics%Coupling',
        'GFS_statein_type' : 'physics%Statein',
        'GFS_radtend_type' : 'physics%Radtend',
        'GFS_grid_type'    : 'physics%Grid',
        'GFS_stateout_type': 'physics%Stateout',
        'GFS_typedefs'     : '',
    },
    'CCPP_typedefs': {
        'GFS_interstitial_type': 'physics%Interstitial(cdata%thrd_no)',
        'CCPP_typedefs'        : '',
    },
    'scm_type_defs': {
        'physics_type': 'physics',
        'scm_type_defs': '',
    },
    'ccpp_types': {
        'ccpp_t'  : 'cdata',
        'ccpp_types': '',
        'MPI_Comm': '',
    },
    # ... plus 8 more entries for physics-side modules (machine, radsw_param, etc.)
}
```

**How it works:** For a variable with `local_name = tgrs` declared in `GFS_statein_type`,
the generator looks up `'GFS_statein_type'` in the map, finds `'physics%Statein'`, and
constructs the target as `physics%Statein%tgrs`. For the thread-indexed interstitial DDT,
`physics%Interstitial(cdata%thrd_no)%<member>` is produced automatically.

This dictionary is the **entire** mechanism by which the prebuild bridge converts flat
metadata into correct DDT-member accessor expressions. It is a hand-maintained workaround
that the redesigned generator must **eliminate**: all information needed to derive these
accessor expressions is already present in the CCPP metadata, provided the metadata storage
model is designed correctly to capture the DDT hierarchy and instance/thread indexing.

---

### 9.2 Host model DDT structure

```
! Module-level variables accessible globally:
physics    (type physics_type,    from module scm_type_defs)
cdata      (type ccpp_t,          from module ccpp_types)
one        (integer parameter = 1, from module ccpp_types)

! physics_type contains:
physics%Model       → GFS_control_type     (control parameters: integers, logicals, 1D arrays)
physics%Statein     → GFS_statein_type     (input atmospheric state: 2D/3D real arrays)
physics%Stateout    → GFS_stateout_type    (output tendencies)
physics%Sfcprop     → GFS_sfcprop_type     (surface properties: 2D real arrays)
physics%Coupling    → GFS_coupling_type    (coupling fields)
physics%Grid        → GFS_grid_type        (grid geometry)
physics%Tbd         → GFS_tbd_type         (to-be-determined / miscellaneous)
physics%Cldprop     → GFS_cldprop_type     (cloud microphysics properties)
physics%Radtend     → GFS_radtend_type     (radiation tendencies)
physics%Diag        → GFS_diag_type        (diagnostic output arrays)
physics%Interstitial(1:thrd_cnt) → GFS_interstitial_type   (per-thread scratch space)
```

The interstitial DDT is an array indexed by thread number. Even though the SCM is
single-threaded, all caps use `physics%Interstitial(cdata%thrd_no)` (i.e., index 1).
This is the pattern that enables OpenMP parallelism in the full UFS models.

---

### 9.3 The horizontal dimension in the SCM

The SCM uses a **chunked** horizontal loop even though `im = 1`. The chunk mechanism is:

```fortran
chunk_begin = physics%Model%chunk_begin(cdata%chunk_no)
chunk_end   = physics%Model%chunk_end(cdata%chunk_no)
```

All 2D and 3D array slice expressions in caps use this pattern:
```fortran
physics%Statein%tgrs(chunk_begin:chunk_end, one:levs)
```

In the SCM, `chunk_begin = chunk_end = 1` always, but the pattern is general enough for
multi-column models. The `one` lower bound (a named integer constant = 1) is a framework
convention used consistently throughout all caps.

---

### 9.4 Four categories of local variables in group caps

Every group cap generates four categories of local variable declarations before its scheme
calls:

**Category 1 — Loop bounds and scalars (always present):**
```fortran
integer :: chunk_begin, chunk_end
integer :: levs
chunk_begin = physics%Model%chunk_begin(cdata%chunk_no)
chunk_end   = physics%Model%chunk_end(cdata%chunk_no)
levs        = physics%Model%levs
```

**Category 2 — Fixed-index extractions (tracer indices, surface-level slices):**

For a tracer `qgrs(:,:,ntqv)`:
```fortran
! No local variable declared — the expression is used inline at the call site:
call scheme_run(qv = physics%Statein%qgrs(chunk_begin:chunk_end, one:levs, physics%Model%ntqv), ...)
```

For a surface-level slice `prsi(:,1)`:
```fortran
call scheme_run(prsi_sfc = physics%Statein%prsi(chunk_begin:chunk_end, 1), ...)
```

The fixed index may be a literal integer (`1`) or a runtime scalar variable from a DDT
field (`physics%Model%ntqv`). Both are inlined at the call site.

**Category 3 — Optional variable pointer arrays:**

One pointer-array type and one pointer-array variable are declared for each optional
variable. They are dimensioned by thread count:
```fortran
type :: real_kind_phys_rank2_ptr_arr_type
    real(kind_phys), dimension(:,:), pointer :: p => null()
end type real_kind_phys_rank2_ptr_arr_type
type(real_kind_phys_rank2_ptr_arr_type), dimension(1:cdata%thrd_cnt) :: sfc_wts_1_ptr_array
```

Before each scheme call that uses the variable, the condition is evaluated and the pointer
either associated or left null:
```fortran
if (physics%Model%lndp_type /= 0) then
    sfc_wts_1_ptr_array(cdata%thrd_no)%p => &
        physics%Coupling%sfc_wts(chunk_begin:chunk_end, one:physics%Model%n_var_lndp)
end if
```

Passed to the scheme as a keyword argument:
```fortran
call gfs_surface_generic_pre_run(..., sfc_wts=sfc_wts_1_ptr_array(cdata%thrd_no)%p, ...)
```

After the call, the pointer is nullified:
```fortran
if (physics%Model%lndp_type /= 0) then
    nullify(sfc_wts_1_ptr_array(cdata%thrd_no)%p)
end if
```

**Category 4 — Unit conversion local variables:**

Not present in the SCM (GFS uses consistent SI units throughout). When present in other
models, a local array is declared, populated before the call, and passed as the argument:
```fortran
real(kind_phys) :: converted_var(chunk_begin:chunk_end)
converted_var(:) = physics%Statein%source_field(chunk_begin:chunk_end) * conversion_factor
call scheme_run(..., target_arg=converted_var, ...)
```

---

### 9.5 Array size checks

Every array argument — mandatory or optional — has a size check immediately before the
scheme call. The check uses `size()` and computes the expected size from dimension variables:

```fortran
! Mandatory variable — outer condition is always .true.
if (.true.) then
    if (size(physics%Statein%tgrs(chunk_begin:chunk_end, one:levs)) /= &
        (chunk_end-chunk_begin+1)*(levs-one+1)) then
        write(cdata%errmsg, '(a,i8,a,i8)') &
            'Detected size mismatch for variable tgrs: expected ', expected, ' but got ', actual
        ierr = 1
        return
    end if
end if

! Optional variable — outer condition mirrors the active= expression
if (physics%Model%lndp_type /= 0) then
    if (associated(sfc_wts_1_ptr_array(cdata%thrd_no)%p)) then
        if (size(sfc_wts_1_ptr_array(cdata%thrd_no)%p) /= expected_size) then
            ...error...
        end if
    end if
end if
```

---

### 9.6 The `initialized(200)` array and instance management

```fortran
logical, dimension(200), save :: initialized = .false.
```

`cdata%ccpp_instance` is a 1-based integer assigned to each independent CCPP state object.
In an ensemble, each ensemble member gets a different instance number (1–200). The `init_cap`
sets `initialized(cdata%ccpp_instance) = .true.` at the end of successful init. The
`run_cap` checks `if (.not. initialized(cdata%ccpp_instance))` and aborts with an error
if init was never called for that instance. The `final_cap` resets the flag to `.false.`.

The value 200 is hardcoded — it is the maximum supported number of simultaneous model
instances. This could be made configurable.

---

### 9.7 Suite and group cap hierarchy

Three-level cap hierarchy:

```
ccpp_static_api.F90  (module ccpp_static_api)
    → dispatches by suite_name + optional group_name
    → owns physics, cdata, constants via module use
    → calls suite-level caps:

ccpp_scm_gfs_v16_cap.F90  (module ccpp_scm_gfs_v16_cap)
    → aggregates arguments from all groups
    → calls group caps in order per phase:

ccpp_scm_gfs_v16_time_vary_cap.F90   (module ccpp_scm_gfs_v16_time_vary_cap)
ccpp_scm_gfs_v16_radiation_cap.F90   (module ccpp_scm_gfs_v16_radiation_cap)
ccpp_scm_gfs_v16_phys_ps_cap.F90     (module ccpp_scm_gfs_v16_phys_ps_cap)
ccpp_scm_gfs_v16_phys_ts_cap.F90     (module ccpp_scm_gfs_v16_phys_ts_cap)
```

Each level is a pure Fortran module. Argument passing is explicit keyword-argument style
at every level; no implicit global data (except in the static API, which uses `use`).

---

### 9.8 Static API: module-level variable ownership

The static API module uses all host-model modules and accesses their variables at module
scope. It does **not** take host data as subroutine arguments — instead it fills the
group cap arguments from its own module-use-associated variables:

```fortran
module ccpp_static_api
    use scm_type_defs,       only: physics
    use ccpp_types,          only: cdata, one
    use scm_physical_constants, only: con_g, con_pi, con_t0c, ...
    use gfs_typedefs,        only: ltp
    use ccpp_scm_gfs_v16_cap, only: scm_gfs_v16_run_cap, ...
    ...
contains
    subroutine ccpp_physics_run(cdata, suite_name, group_name, ierr)
        ! cdata passed in, others accessed from module scope
        select case (to_lower(trim(suite_name)))
        case ('scm_gfs_v16')
            if (present(group_name)) then
                select case (to_lower(trim(group_name)))
                case ('phys_ps')
                    ierr = scm_gfs_v16_phys_ps_run_cap(one=one, physics=physics, cdata=cdata, ...)
                ...
                end select
            else
                ierr = scm_gfs_v16_run_cap(one=one, physics=physics, cdata=cdata, ...)
            end if
        case ('scm_gfs_v17_p8')
            ...
        end select
    end subroutine
end module
```

This design means the static API file must be recompiled whenever any host-model module
changes (because it `use`s them), and it must be regenerated whenever suites change.
Its location in the **source tree** (not build tree) is a deliberate SCM design choice:
the file is committed to the repository as a generated artifact.

---

### 9.9 Build system

Prebuild runs at **cmake configure time** via `execute_process()`, before any compilation
starts. This is unusual but simplifies the cmake dependency graph.

```cmake
execute_process(
    COMMAND ccpp/framework/scripts/ccpp_prebuild.py
            --config=ccpp/config/ccpp_prebuild_config.py
            --suites=${CCPP_SUITES}
            --builddir=${CMAKE_CURRENT_BINARY_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../..
    OUTPUT_FILE ${PROJECT_BINARY_DIR}/ccpp_prebuild.out
    ERROR_FILE  ${PROJECT_BINARY_DIR}/ccpp_prebuild.err
)
include(${CMAKE_CURRENT_BINARY_DIR}/ccpp/physics/CCPP_CAPS.cmake)     # → ${CAPS}
include(${CMAKE_CURRENT_BINARY_DIR}/ccpp/physics/CCPP_SCHEMES.cmake)  # → ${SCHEMES}
include(${CMAKE_CURRENT_BINARY_DIR}/ccpp/physics/CCPP_TYPEDEFS.cmake) # → ${TYPEDEFS}
include(scm/src/CCPP_STATIC_API.cmake)                                # → ${API}
```

**Suite selection:** If `CCPP_SUITES` is not set by the user, a helper script
`suite_info.py` selects a compiler-appropriate subset. The full set of 63 suites is
used for production; subsets speed up development builds.

---

### 9.10 Observations relevant to the redesign

1. **`TYPEDEFS_NEW_METADATA` is a workaround that the redesign must eliminate.** The
   DDT accessor information (which type lives at which accessor path) can be fully derived
   from the CCPP metadata itself, given a well-designed metadata storage model. The
   redesign must derive DDT accessor expressions automatically from the metadata rather
   than requiring a separate hand-maintained dictionary. This is one of the primary
   motivations for the new metadata storage design.

2. **Three-level cap hierarchy (group → suite → static API) should be preserved.**
   It provides clean separation: group caps are independently testable, suite caps
   aggregate phases, the static API is the single host-callable entry point.

3. **The static API's module-level `use` of host data is model-specific.** In models
   where host data is not module-level (e.g., passed as subroutine arguments), the
   static API pattern changes. The SCM is the simplest case because `physics` and `cdata`
   are global module variables.

4. **Instance and thread indexing are two orthogonal dimensions of host data access.**
   Host model data uses two distinct indexing patterns that must be handled correctly:

   - **Regular state data** (Statein, Stateout, Sfcprop, etc.): dimensioned by instance
     number — `physics%Statein(ccpp_instance_number)%array(1:horizontal_dimension, 1:vertical_dimension, ...)`.
     In models supporting multiple in-memory model instances (ensemble), the top-level
     DDT is an array indexed by `cdata%ccpp_instance`.

   - **Interstitial (per-thread scratch) data**: dimensioned by both instance and thread —
     `physics%Interstitial(ccpp_instance_number, ccpp_thread_number)%array(1:horizontal_loop_extent, ...)`.
     Critically, the horizontal dimension of interstitial arrays is sized to
     `horizontal_loop_extent` (one OpenMP thread's chunk), not `horizontal_dimension`
     (the full column count). `max_number_of_threads` instances are allocated per model
     instance. Interstitial data can only be used during the **run phase** — this is a
     known limitation of ccpp-prebuild that the redesign should address or at minimum
     preserve explicitly.

5. **Optional variable pointer arrays dimensioned by thread count** are the current
   solution to thread-safe optional variable handling. This pattern is verbose (one
   derived type + one array per optional variable per cap function) but correct.
   The redesign could simplify this.

6. **~550 optional variables in this model.** Optional/conditional variables are not
   a corner case — they are a first-class feature. The redesign must handle them
   efficiently and correctly.

7. **Array size checks are debug-only and should not appear in the redesign by default.**
   In prebuild they are only generated when the `--debug` flag is passed. The redesigned
   generator should not produce them in normal mode — out-of-bounds access is caught at
   runtime by compiler flags (e.g., `-fcheck=bounds` with gfortran, `-check bounds` with
   ifort). The 12,991-line group cap is partly a consequence of generating these checks
   unconditionally in the debug mode artifact examined here.

8. **No unit conversions appear in GFS/SCM.** Unit conversion infrastructure must be
   present in the redesign but the GFS physics package is self-consistent in units.
   Unit conversions are more relevant for other host models.

9. **The `one` constant** (integer parameter = 1) is passed as an explicit argument
   everywhere and used as the lower bound in all array slices. This is a framework
   convention. The redesign should decide whether this convention is preserved or
   whether array lower bounds are handled differently.

10. **Subcycles produce actual Fortran `do` loops inside the generated group cap.**
    The loop from `1` to `cdata%loop_max` is generated directly in the cap function,
    not left to the host model:
    ```fortran
    cdata%loop_max = 2
    do cdata%loop_cnt = 1, cdata%loop_max
        call scheme_A_run(...)
        if (ierr /= 0) return
        call scheme_B_run(...)
        if (ierr /= 0) return
    end do
    ```
    `cdata%loop_max` is set at the start of the subcycle block (from the `loop=` attribute
    in the SDF XML) and `cdata%loop_cnt` is the current iteration counter, both visible
    to schemes via the `ccpp_t` DDT.

---

## 10. Real-world example: CAM-SIMA (capgen)

*Source:* `EXT/cam-sima/` — uses `ccpp-capgen`.

CAM-SIMA is the only model currently using capgen. It is still primarily a research model.
Unlike the SCM it uses a full 3D grid with OpenMP parallelism, but exposes host model
data as flat module variables rather than DDTs in the metadata layer. This example reveals
both what capgen can do and where it fundamentally fails.

**Scale:** 1 suite (`cam7`), 2 run groups (`physics_before_coupler`, `physics_after_coupler`),
~75 scheme calls, 18 host `.meta` files, 893-line host cap, 2865-line suite cap.

---

### 10.1 Suite structure

`suite_cam7.xml` has two groups and no subcycles:

| Group | Schemes (approx.) | Purpose |
|---|---|---|
| `physics_before_coupler` | 52 scheme calls | Cloud fraction, energy checks, dry adiabatic adjustment, Zhang-McFarlane deep convection full cycle, constituent tendency application |
| `physics_after_coupler` | ~20 scheme calls | Tropopause diagnostics, gravity wave drag (7 parameterizations + diagnostics), tendency application, energy consistency |

CCPP phases in use: register, initialize, timestep_initial, run (per group), timestep_final, finalize.

---

### 10.2 Host model variable structure

**18 host `.meta` files, all of type `module`.** There are no `host` or `ddt` table types
anywhere. All host variables are flat scalars or arrays in Fortran modules.

CAM-SIMA does **not** expose its physics DDTs (`phys_state`, `phys_tend`, `cam_in`, etc.)
through metadata. These types exist in `physics_types.F90` but have no `.meta` file.
The generated host cap accesses them directly via `use physics_types, only: phys_state, ...`
and passes individual DDT members as flat keyword arguments:
```fortran
! In cam_ccpp_cap.F90 — direct access to non-metadataized DDT members:
call cam7_physics_before_coupler(..., pint=phys_state%pint, t=phys_state%t, &
    dtdt_total=phys_tend%dtdt_total, landfrac=cam_in%landfrac, ...)
```

This means capgen has no knowledge of how host data is structured. The host cap is
partly machine-generated and partly depends on manually wiring non-metadataized sources.
**This is a fundamental architectural gap** — changes to `physics_types` are invisible
to the framework.

**Key host variables by module:**

| Module | Key variables |
|---|---|
| `physics_grid` | `columns_on_task` (horizontal_dimension), `col_start`, `col_end`, lat, lon, area |
| `vert_coord` | `pver` (vertical_layer_dimension), `pverp` (vertical_interface_dimension) |
| `physconst` | ~35 physical constants, all `protected = True` |
| `cam_constituents` | `num_advected` (count of advected tracers) |
| `spmd_utils` | `mpicom`, `masterproc`, `npes`, `iam` |

No instance indexing (`physics(1)`) and no thread-indexed DDTs appear — CAM-SIMA uses
a fundamentally different data model from the GFS/SCM stack.

---

### 10.3 The two-cap architecture

Capgen generates two distinct Fortran files:

**`cam_ccpp_cap.F90` — the host cap (893 lines)**
- Module `cam_ccpp_cap`
- Imports non-metadataized host variables directly via `use physics_types`, `use physconst`, etc.
- Manages the constituent object (`ccpp_model_constituents_t`) — registration, initialization, gather/scatter, index lookup
- Public subroutines: `cam_ccpp_physics_run`, `cam_ccpp_physics_initialize`, etc. — the entry points the host model calls
- Dispatches to the suite cap, passing ~61–76 flat keyword arguments

**`ccpp_cam7_cap.F90` — the suite cap (2865 lines)**
- Module `ccpp_cam7_cap`
- No host-specific imports — knows nothing about `physics_types`, `phys_state`, etc.
- All arguments are flat scalars and arrays, fully matched to metadata standard_names
- Contains all scheme calls, suite-level persistent variables, local temporaries, state machine
- The suite cap could in principle be used with any host model that provides the same standard names

This two-cap split is **architecturally correct**: it separates host-specific binding
from physics-neutral dispatch. The redesign should preserve this separation.

---

### 10.4 The flat-field argument problem — concrete evidence

The run-phase subroutines expose the core problem with capgen's approach directly:

```fortran
subroutine cam7_physics_before_coupler(errflg, errmsg, col_start, col_end, pver, dtime, &
    gravit, pint, te_ini_dyn, teout, amiroot, iulog, ptend_s, temp, dtdt_total, cpair, &
    lagrang, layer_surf, layer_toa, interface_surf, interface_toa, ncnst, piln, pmid, pdel, &
    rpdel, qv, carr, cprops, rair, zvir, zi, zm, cp_or_cv_dycore, u, v, pintdry, phis, &
    te_cur_phys, te_cur_dyn, tw_cur, latice, latvap, energy_formula_physics,          &
    energy_formula_dycore, cappa, q_tend, const_tend, qmin, pverp, cpwv, cpliq, rh2o, lat, &
    long, pblh, mcon, tpert, dlf, rprd, ql, rliq, landfrac, cpair3, ttend_dp, tmelt,  &
    top_lev, ke, ke_lnd, cldfrc, domomtran, momcu, momcd, il1g, nstep,                &
    dudt_total, dvdt_total, fracis, dpdry, ps)
```

**61 dummy arguments for one group cap.** `physics_after_coupler` has 76. These are
individual flat arrays and scalars — no DDT in sight. This is exactly the problem that
three developers failed to fix: in the GFS/UFS context, this would be 1,200+ arguments.
The GFS physics stack simply cannot be connected to capgen in its current form.

In contrast, the prebuild equivalent for the same data would pass `physics` (one DDT argument)
and `cdata` — two arguments covering hundreds of variables.

---

### 10.5 Suite-level persistent variables — the framework-owned data pattern

The suite cap allocates and owns arrays that persist across group calls within a timestep.
These are allocated in `cam7_initialize` and deallocated in `cam7_finalize`:

```fortran
! Suite-level persistent (allocated in initialize, freed in finalize):
real(kind_phys), allocatable :: windu_tend(:,:)   ! GW drag u-tendency accumulator
real(kind_phys), allocatable :: windv_tend(:,:)   ! GW drag v-tendency accumulator
real(kind_phys), allocatable :: scaling_dycore(:,:) ! energy scaling factor
real(kind_phys), allocatable :: tend_te_tnd(:)    ! energy tendency accumulator
real(kind_phys), allocatable :: tend_tw_tnd(:)    ! water tendency accumulator
real(kind_phys), allocatable :: temp_ini(:,:)     ! temperature saved at timestep start
real(kind_phys), allocatable :: z_ini(:,:)        ! height saved at timestep start
real(kind_phys), allocatable :: flx_vap(:), flx_cnd(:), flx_ice(:), flx_sen(:)
logical, allocatable :: doconvtran(:)             ! per-constituent convection flag
type(coords1d) :: p                               ! pressure coordinate DDT for GW drag
```

These are physics-internal variables — the host model does not know about them, does not
own them, and does not need to. This is the capgen "data ownership" model: the suite cap
is the data owner for variables that only matter within the physics.

**This pattern is correct and desirable.** The complexity in capgen comes not from the
concept but from how these variables are discovered during analysis (scope-chain promotion)
and passed around (via VarDictionary). The redesign needs a simpler mechanism to achieve
the same result: statically enumerate physics-internal variables during analysis and have
the suite cap own them as named allocatables.

During the run phase, suite-level persistent arrays are subsetted when passed to schemes:
```fortran
call gw_common_run(..., windu_tend=windu_tend(col_start:col_end, 1:pver), ...)
```

Run-phase local temporaries (e.g., `cape`, `cme`, `mu`, `md`) are allocated at function
entry and deallocated at exit:
```fortran
allocate(cape(col_start:col_end))
...
call zm_convr_run(..., cape=cape, ...)
...
deallocate(cape)
```

These temporaries use `col_start` as the lower bound so that assumed-shape dummy arguments
in schemes see a 1-based array — a subtle but important detail.

---

### 10.6 Horizontal chunking model

CAM-SIMA uses `col_start`/`col_end` (passed as arguments to every run subroutine) to
define the current horizontal chunk:

```fortran
ncol = col_end - col_start + 1
```

Schemes declare `horizontal_loop_extent` and receive `ncol`. The horizontal dimension
in the host (storage dimension) is `columns_on_task`. The subsetting from storage to
loop extent happens at the boundary between host cap and suite cap — the host cap
passes the right subsections:

```fortran
! In cam_ccpp_cap.F90:
call cam7_physics_before_coupler(..., col_start=col_start, col_end=col_end, &
    pmid=phys_state%pmid, ...)  ! full arrays passed; suite cap subsets internally
```

Inside the suite cap, persistent arrays are subsetted explicitly when passed to schemes:
```fortran
windu_tend(col_start:col_end, 1:pver)
```
Local temporaries allocated as `allocate(cape(col_start:col_end))` are already
correctly sized and passed as assumed-shape `(:)`.

---

### 10.7 State machine

The suite cap has a character module variable tracking lifecycle state:

```fortran
character(len=16) :: ccpp_suite_state = 'uninitialized'
```

Transitions: `uninitialized` → register → `uninitialized` → initialize → `initialized`
→ timestep_initial → `in_time_step` → (run, no state change) → timestep_final →
`initialized` → finalize → `uninitialized`.

Each phase entry point checks the expected prior state:
```fortran
if (trim(ccpp_suite_state) /= 'in_time_step') then
  errflg = 1
  write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state), &
      "' in cam7_physics_before_coupler"
  return
end if
```

Non-run phases also include an OpenMP thread guard:
```fortran
#ifdef _OPENMP
  if (omp_get_thread_num() > 1) then
    errflg = 1
    errmsg = "Cannot call initialize routine from a threaded region"
    return
  end if
#endif
```

The state machine is simple, complete, and useful. The redesign should preserve it.

---

### 10.8 Constituent variable handling

CAM-SIMA demonstrates the full constituent lifecycle:

```fortran
! In cam_ccpp_cap.F90:
type(ccpp_model_constituents_t), target :: cam_constituents_obj

! Registration (scheme-declared constituents):
call suite_cam7_constituents_num_consts(num_consts)
call suite_cam7_constituents_const_name(iconst, const_name)
call cam_constituents_obj%new_field(const_name, ...)

! Initialization (host-declared constituents like water vapor):
cam_model_const_stdnames(1) = "water_vapor_mixing_ratio_wrt_moist_air_and_condensed_water"
call cam_constituents_obj%new_field(cam_model_const_stdnames(1), ...)

! Per-timestep gather from host:
call cam_ccpp_gather_constituents(phys_state%q, ...)

! Passing to suite cap:
call cam7_physics_before_coupler(...,
    qv  = cam_constituents_obj%vars_layer(:, :, cam_model_const_indices(1)),
    carr = cam_constituents_obj%vars_layer,
    cprops = cam_constituents_obj%const_metadata, ...)

! Per-timestep scatter back to host:
call cam_ccpp_update_constituents(phys_state%q, ...)
```

The suite cap sees constituents as:
- `carr(:,:,:)` — the full rank-3 constituent array (ncol, nlev, ncnst)
- `qv(:,:)` — water vapor slice extracted in the host cap: `cam_constituents_obj%vars_layer(:,:,cam_model_const_indices(1))`
- `cprops(:)` — array of `ccpp_constituent_prop_ptr_t` metadata objects
- `doconvtran(1:ncnst)` — suite-level logical array set by scheme init indicating which constituents are convected

This constituent API is sophisticated and worth preserving or improving in the redesign.

---

### 10.9 Known defects in the capgen output

**Repeated scheme init/final calls.** Capgen generates one init call per occurrence of
a scheme name in the XML, without deduplication:
- `qneg_init` called 5 times (once per `qneg` entry in the suite XML)
- `qneg_timestep_final` called 5 times
- `check_energy_chng_init` called twice
- `save_ttend_from_convect_deep_timestep_init` called 3 times

If these routines have internal state, allocations, or side effects, this is a correctness
defect. The redesign must deduplicate init/final calls by unique scheme name.

**Unit conversion embedded silently in the cap.** Before `zm_conv_convtran_run`:
```fortran
dpdry_local(:,1:pver) = 1.0E-2_kind_phys * dpdry(:,1:pver)   ! Pa → hPa
```
This is generated from the metadata units mismatch but appears as an opaque transform
in the cap. The redesign should make this visible (e.g., a comment naming the standard
name, the source units, and the target units).

---

### 10.10 Build system — capgen invocation

Capgen is invoked from Python (`cam_autogen.py`), not from cmake:

```python
from ccpp_capgen import capgen
capgen_db = capgen(run_env, return_db=True)
```

This is a programmatic API call, not a subprocess. The `CCPPDatabaseObj` returned
(`capgen_db`) is then used directly in Python to query scheme lists, constituent names,
and file paths — avoiding the datatable XML query step that cmake-based invocations need.

Output files consumed by the build:
- `cam_ccpp_cap.F90` — compiled into the atmosphere component
- `ccpp_cam7_cap.F90` — compiled into the atmosphere component
- `ccpp_kinds.F90` — compiled into the atmosphere component
- Utility files from `ccpp_framework/src/` (copied to build dir)
- `ccpp_datatable.xml` — queried by the build system for file lists

---

### 10.11 Observations relevant to the redesign

1. **The two-cap split (host cap + suite cap) is the right architecture.** It cleanly
   separates host-specific binding from physics-neutral dispatch. The redesign must
   preserve this.

2. **Flat-field arguments in the suite cap are the critical failure.** 61–76 dummy
   arguments per run subroutine is already large for a research model; for UFS/GFS
   with 1,200+ variables it is completely infeasible. The redesign must pass DDTs.

3. **The CAM-SIMA host does not use DDTs in metadata.** All host variables are flat
   module variables. This is a fundamentally different host model architecture from
   GFS/SCM. The redesign must support both styles: flat-module hosts (CAM-SIMA) and
   deep-DDT hosts (GFS/SCM).

4. **Non-metadataized variables hardwired into the host cap is a serious gap.**
   `phys_state`, `phys_tend`, `cam_in` from `physics_types` have no `.meta` files.
   The host cap accesses them directly. This means the framework cannot verify or
   track these variables. The redesign should either require full metadata coverage
   or have an explicit mechanism for declaring non-metadataized pass-through variables.

5. **Suite-level persistent variables (framework-owned data) work well in practice.**
   `windu_tend`, `scaling_dycore`, `temp_ini`, etc. are owned by the suite cap, invisible
   to the host, and persist across group calls. This is the right pattern for
   physics-internal state. The redesign needs this but with a simpler discovery mechanism
   than capgen's scope-chain promotion.

6. **Deduplicate init/final calls.** The redesign must deduplicate `_init`, `_finalize`,
   `_timestep_init`, and `_timestep_final` calls by unique scheme name (not by occurrence
   in the XML).

7. **The constituent API in the host cap is comprehensive.** The `ccpp_model_constituents_t`
   object with its register/init/gather/scatter/index API is sophisticated and should be
   preserved or improved.

8. **The suite-variables introspection subroutine** (`ccpp_physics_suite_variables`,
   enumerating 83 standard names as inputs/outputs) is a useful capability for build
   system integration and should be in the redesign.

9. **The programmatic Python API** (`capgen(run_env, return_db=True)`) is valuable
   for hosts like CAM-SIMA that invoke the generator from Python. The redesign should
   support both CLI and programmatic invocation.

10. **Unit conversions must be annotated in the generated cap**, not silently embedded
    as magic-number multiplications. A comment with source units, target units, and the
    standard name involved is the minimum.

11. **The horizontal chunking model** (`col_start`/`col_end` as explicit arguments,
    `ncol = col_end - col_start + 1` computed at entry) works and is clean. Suite-level
    persistent arrays are allocated full-size and subsetted at call sites.

12. **No optional variables in this model.** CAM-SIMA does not exercise optional/active
    variable handling. This feature must be in the redesign but is not demonstrated here.

---

## 11. Real-world example: UFS Weather Model (prebuild)

The UFS Weather Model is the most complex and production-critical of the three examples.
It is a fully-coupled, 3-D operational NWP model. The CCPP physics is used in the
atmospheric component (`UFSATM`). Unlike the SCM (column model, process-split only) and
CAM-SIMA (capgen, flat-field arguments), UFS uses prebuild in a 3-D blocked/threaded
configuration that is architecturally distinct from both prior examples.

The two suites analyzed here are:
- `FV3_GFS_v17_coupled_p8` — the primary operational GFS suite
- `FV3_GFS_v17_coupled_p8_ugwpv1` — a variant replacing `unified_ugwp` with `ugwpv1`

The ugwpv1 suite is structurally identical to the base suite except for the `phys_ps`
group (4 extra scheme calls), so all observations below apply to both.

---

### 11.1 Suite structure

The primary suite has 5 groups:

| Group | Subcycles | Scheme calls | Phase called |
|-------|-----------|-------------|--------------|
| `time_vary` | 1 | 4 | timestep_init (domain-level, no blocking) |
| `radiation` | 1 | 8 | run (block/thread loop) |
| `phys_ps` | 3 (loop=1, loop=2, loop=1) | 21 | run (block/thread loop) |
| `phys_ts` | 3 (loop=1, loop=1, loop=1) | 12 | run (block/thread loop) |
| `stochastics` | 1 | 2 | run (block/thread loop) |

The `time_vary` group is the only one called at timestep_init/finalize. All other groups
are called from the run phase via the OpenMP blocked loop. This is a fundamentally
different usage pattern from SCM (which runs everything sequentially) and CAM-SIMA
(which has no run phase at all for the groups analyzed).

The `phys_ps` group has a surface iteration subcycle with `loop="2"`, which generates an
actual Fortran `do` loop in the cap body:
```fortran
! Start of next subcycle
cdata%loop_max = 2
do cdata%loop_cnt = 1, cdata%loop_max
   ! ... sfc_diff, sfc_nst, noahmpdrv, sfc_land, sfc_cice, sfc_sice ...
end do
```

---

### 11.2 Cap hierarchy and scale

The three-level hierarchy is preserved from prebuild:

```
ccpp_static_api.F90          (627 lines)   ← suite+group name dispatch
  ↓
ccpp_fv3_gfs_v17_coupled_p8_cap.F90        (363 lines)   ← calls all group caps in order
  ↓
ccpp_fv3_gfs_v17_coupled_p8_time_vary_cap.F90   (1404 lines)
ccpp_fv3_gfs_v17_coupled_p8_radiation_cap.F90   (967 lines)
ccpp_fv3_gfs_v17_coupled_p8_phys_ps_cap.F90     (4226 lines)   ← 200 optional ptr arrays
ccpp_fv3_gfs_v17_coupled_p8_phys_ts_cap.F90     (1953 lines)
ccpp_fv3_gfs_v17_coupled_p8_stochastics_cap.F90 (443 lines)
```

The ugwpv1 variant generates another 10,220 lines of largely redundant code (identical
caps with one suite-name prefix change and minor scheme-list differences). Total for both
suites: 18,333 lines of generated Fortran.

This redundancy is a key motivation for the redesign: suite variants that share groups
should not regenerate identical cap code. The redesign should support group-level cap
sharing across suite variants.

---

### 11.3 Host model DDT structure

All host data lives in `CCPP_data.F90` as module-level `save, target` variables:

```fortran
type(GFS_control_type)                         :: GFS_control    ! config/control
type(GFS_statein_type)                         :: GFS_statein    ! atmospheric state in
type(GFS_stateout_type)                        :: GFS_stateout   ! atmospheric state out
type(GFS_grid_type)                            :: GFS_grid       ! grid geometry
type(GFS_tbd_type)                             :: GFS_tbd        ! temporal interp data
type(GFS_cldprop_type)                         :: GFS_cldprop    ! cloud properties
type(GFS_sfcprop_type)                         :: GFS_sfcprop    ! surface properties
type(GFS_radtend_type)                         :: GFS_radtend    ! radiation tendencies
type(GFS_coupling_type)                        :: GFS_coupling   ! coupling fields
type(GFS_diag_type)                            :: GFS_intdiag    ! diagnostics
type(GFS_interstitial_type), allocatable (:)   :: GFS_interstitial  ! scratch, per thread
```

Plus three `ccpp_t` instances for different levels of parallelism (see §11.5).

This is structurally similar to the SCM's `physics` DDT hierarchy, but with one key
difference: all DDTs are at the same flat level rather than nested (no `physics%Statein`,
only `GFS_statein`). Each DDT maps to a distinct functional role.

The `GFS_typedefs.F90` file (not auto-generated) defines all DDT types along with ~30
physical constants (`con_pi`, `con_g`, `con_rd`, etc.) that also appear in the metadata.

---

### 11.4 DDT arguments in the cap chain

The static API imports all DDTs and physical constants from `CCPP_data` and `GFS_typedefs`
via `use` statements, then passes them as named arguments to group cap functions. This is
the full DDT-argument pattern that prebuild implements:

```fortran
! In ccpp_static_api.F90:
use ccpp_data, only: gfs_control, gfs_statein, gfs_sfcprop, ...
use gfs_typedefs, only: con_pi, con_g, con_rd, ...

ierr = fv3_gfs_v17_coupled_p8_phys_ps_run_cap( &
    one=one, gfs_control=gfs_control, cdata=cdata,  &
    gfs_statein=gfs_statein, gfs_sfcprop=gfs_sfcprop, &
    con_g=con_g, con_pi=con_pi, ...                  &
    gfs_interstitial=gfs_interstitial)
```

The group cap receives these as typed `intent(*), target` dummy arguments and uses them
directly to construct call-site subsections. This means **the group cap is fully portable
— it does not use any host module directly**, only what it receives as arguments.

The `target` attribute is required because the cap creates pointer sections of these DDTs
(array subsections via pointer assignment) when handling optional variables.

---

### 11.5 The dual cdata architecture

UFS uses two distinct sets of `ccpp_t` handles with different scopes:

**Domain-level (`cdata_domain`)**: Used for non-run phases (init, finalize, time_vary
timestep_init/finalize). Called once per step, no blocking:
```fortran
cdata_domain%blk_no = 1;  cdata_domain%chunk_no = 1
cdata_domain%thrd_no = 1;  cdata_domain%thrd_cnt = 1
```

**Block/thread-level (`cdata_block(nb, nt)`)**: Used for run phase (radiation, phys_ps,
phys_ts, stochastics). Allocated as a 2-D array `(1:nblks, 1:nthrdsX)` where `nthrdsX`
accounts for non-uniform last-block sizing:
```fortran
cdata_block(nb,nt)%blk_no   = nb
cdata_block(nb,nt)%chunk_no = nb   ! block number = chunk number
cdata_block(nb,nt)%thrd_no  = nt
cdata_block(nb,nt)%thrd_cnt = nthrdsX
```

The redesign must support this dual cdata usage: a single `cdata` handle for domain-level
phases and a 2-D array of handles for blocked run phases.

---

### 11.6 OpenMP threading model

Non-run phases allow internal threading in physics schemes:
```fortran
GFS_control%nthreads = nthrds    ! all N threads available to physics
call ccpp_physics_timestep_init(cdata_domain, ...)
```

Run phase uses all threads for blocking, so physics must not spawn additional threads:
```fortran
GFS_control%nthreads = 1         ! no internal threading allowed
!$OMP parallel num_threads(nthrds) ...
!$OMP do schedule(dynamic,1)
do nb = 1, nblks
    call GFS_Interstitial(nt)%create(ixs=chunk_begin(nb), ixe=chunk_end(nb), model=GFS_control)
    call ccpp_physics_run(cdata_block(nb,nt), group_name="phys_ps", ...)
    call GFS_Interstitial(nt)%destroy(GFS_control)
end do
!$OMP end do
!$OMP end parallel
```

The `nt = omp_get_thread_num()+1` pattern (1-based thread index) is used throughout.
Each thread owns one `GFS_Interstitial(nt)` and one `cdata_block(nb,nt)` per block
iteration. The dynamic schedule means different threads process different blocks at
different times, which is why the interstitial must be created/destroyed per-iteration
rather than pre-allocated per-thread.

---

### 11.7 Horizontal dimension: the chunk_begin/chunk_end pattern

For non-run phases, the full horizontal dimension is used at every call site:
```fortran
tgrs(one:gfs_control%ncols, one:gfs_control%levs)
```

For run phases, the chunk range is looked up from the control DDT using the block number:
```fortran
tgrs(gfs_control%chunk_begin(cdata%chunk_no) : gfs_control%chunk_end(cdata%chunk_no), &
     one:gfs_control%levs)
```

The chunk size (horizontal extent `im`) is retrieved as:
```fortran
im = gfs_control%blksz(cdata%blk_no)
```

`blksz(nb)` handles **non-uniform block sizes**: the last block may be smaller than the
others if the domain size is not divisible by the number of blocks. The `chunk_begin`/
`chunk_end` arrays (indexed by chunk number = block number) give the global offset range.

This is a cleaner pattern than SCM's `chunk_begin`/`chunk_end` as explicit dummy
arguments, because UFS looks them up from the already-passed `gfs_control` DDT.

**Critical implication for the redesign**: The subsetting pattern `(chunk_begin:chunk_end)`
appears at every single array call site in the run phase — literally hundreds of times in
the phys_ps cap alone. This boilerplate is generated by prebuild from the metadata. In
the redesign, this subsetting must remain at the call site (not higher up) to allow each
thread to process its own chunk independently.

---

### 11.8 The GFS_interstitial — pointer-based scratch DDT

`GFS_interstitial_type` (defined in `CCPP_typedefs.F90`) is a DDT where **every field is
a pointer**, initialized to null:
```fortran
type GFS_interstitial_type
    real(kind_phys), pointer :: adjsfculw_land(:) => null()
    real(kind_phys), pointer :: del(:,:)          => null()
    ! ... ~200+ pointer fields
end type
```

This is dramatically different from the SCM's interstitial (which is a regular allocatable
DDT allocated once per thread at startup). The UFS interstitial is:
1. **Created** (`GFS_Interstitial(nt)%create(ixs, ixe, model)`) before each block — this
   allocates all required fields to the chunk size `ixe-ixs+1`
2. **Reset** (`GFS_Interstitial(nt)%reset(model)`) to zero before radiation and phys_ps
3. **Destroyed** (`GFS_Interstitial(nt)%destroy(model)`) after each block — deallocates

This design exists because different blocks (especially the last block) can have different
sizes. Pre-allocating to the maximum size wastes memory at scale; per-block allocation
ensures exact sizing. The pointer-based design also allows the `create()` method to
selectively allocate only the fields needed for the current physics configuration.

In the caps, the interstitial is accessed as:
```fortran
gfs_interstitial(cdata%thrd_no)%del(chunk_begin:chunk_end, one:levs)
```

The interstitial array is 1-D (indexed by thread, not by `(instance, thread)` as in SCM).
This works because UFS has only one model instance at runtime — no ensemble-in-memory.

---

### 11.9 Optional variables — the pointer array pattern at scale

The phys_ps run cap has **200 optional pointer arrays** in its local variable section.
Each looks like:
```fortran
type :: real_kind_phys_rank1_ptr_arr_type
    real(kind_phys), dimension(:), pointer :: p => null()
end type real_kind_phys_rank1_ptr_arr_type
type(real_kind_phys_rank1_ptr_arr_type), dimension(1:cdata%thrd_cnt) :: sfc_wts_1_ptr_array
```

Usage pattern (consistent with SCM but with threading dimension):
```fortran
if (gfs_control%lndp_type /= 0) then
    sfc_wts_1_ptr_array(cdata%thrd_no)%p => &
        gfs_coupling%sfc_wts(chunk_begin:chunk_end, one:gfs_control%n_var_lndp)
end if
! ... scheme call ...
if (gfs_control%lndp_type /= 0) then
    nullify(sfc_wts_1_ptr_array(cdata%thrd_no)%p)
end if
```

The array is dimensioned by `cdata%thrd_cnt` (total thread count) and indexed by
`cdata%thrd_no` (current thread number). This handles the threaded run phase where
multiple threads are simultaneously executing the same run cap function with different
chunk ranges. Each thread independently associates and nullifies its own pointer slot.

200 optional variables in `phys_ps` alone. This is the regime for which the SCM had ~550
total optional vars — confirming that operational 3-D GFS physics is heavily optional-var
driven. The design is sound but generates enormous boilerplate.

A key observation: the type definition for each pointer wrapper (`integer_..._ptr_arr_type`,
`real_kind_phys_rank1_ptr_arr_type`, etc.) is **re-declared inside every single function
that needs it**. This results in duplicate type definitions across all group caps. The
redesign should define these wrapper types once in a shared module.

---

### 11.10 Physical constants as metadata variables

The UFS static API has an extensive USE list of physical constants from `gfs_typedefs`:
```
con_pi, con_g, con_t0c, con_hfus, con_solr_2008, con_solr_2002, con_c, con_plnk,
con_boltz, con_rd, ltp, con_zero, con_rerth, con_p0, con_rv, con_cp, con_rgas,
con_amd, con_amw, con_avgd, con_hvap, con_eps, con_omega, con_fvirt, con_ttp,
con_thgni, con_epsm1, con_rog, con_rocp, con_tice, con_sbc, con_jcal, con_rhw0,
rlapse, rhowater, karman, con_1ovg, con_cliq, con_cvap, rainmin, con_epsm1  (30+ total)
```

These travel through the full chain: static API USE → suite cap argument → group cap
argument → scheme call argument. Each constant is declared as a separate scalar dummy
argument (`real(kind_phys), intent(in), target :: con_pi`) in every group cap that needs
it.

This is correct but verbose. The redesign should consider whether constants should be
gathered into a dedicated DDT (e.g., `gfs_constants_type`) so the cap chain carries one
argument instead of 30. This would also eliminate the need to explicitly enumerate which
constants each group needs — they could all come along in the constants DDT.

---

### 11.11 The `one` lower-bound anchor

The integer constant `one = 1` (from `ccpp_types`) is passed as an explicit argument
throughout the UFS cap chain for the same reason as in SCM: it anchors lower array bounds
without triggering association-status issues:
```fortran
type(gfs_interstitial_type), intent(inout), target :: gfs_interstitial(one:)
tgrs(one:gfs_control%ncols, one:gfs_control%levs)
```

This pattern is ubiquitous and is a known prebuild idiom.

---

### 11.12 No framework-owned persistent variables

Unlike CAM-SIMA (which allocates scheme-persistent variables in the suite cap), the UFS
has no framework-owned persistent state in any cap. All persistent state lives in the host
DDTs (`GFS_tbd`, `GFS_sfcprop`, etc.). The interstitial DDT (`GFS_interstitial`) is purely
transient — created and destroyed each block.

This is consistent with UFS's prebuild-based architecture. Whether framework-owned
persistent variables would be beneficial for UFS is an open question for the redesign.

---

### 11.13 Build system and driver

Prebuild is invoked from CMake (not programmatically) and generates:
- Group cap files (one per group × suites)
- Suite cap files (one per suite)
- `ccpp_static_api.F90`
- `CCPP_CAPS.cmake`, `CCPP_SCHEMES.cmake`, `CCPP_TYPEDEFS.cmake` — consumed by CMake to
  enumerate files to compile

The host driver (`CCPP_driver.F90`) is **hand-written**, not auto-generated. It owns the
OpenMP loop, the cdata allocation/setup, the interstitial create/destroy, and the
diagnostic bucket zeroing. This is a significant difference from CAM-SIMA where the
equivalent driver code is partially generated. In the redesign, this host driver code
should remain hand-written — it encodes model-specific threading and blocking decisions
that cannot be derived from metadata alone.

---

### 11.14 Observations relevant to the redesign

1. **The DDT-argument cap chain is fully validated at UFS scale.** Passing 10+ DDTs plus
   30+ scalar constants as named arguments through three cap levels works correctly in
   production. The redesign must replicate this exactly.

2. **The chunk_begin/chunk_end subsetting at call sites is non-negotiable.** Hundreds of
   array sections per group cap. The generator must produce this from the metadata
   `horizontal_dimension` standard name and the `active` flag for optional variables.
   This is prebuild's core value at 3-D scale.

   *Design direction*: Rather than carrying `chunk_no` in cdata and having the cap look
   up `gfs_control%chunk_begin(chunk_no)`, the redesign should pass
   `horizontal_loop_begin` and `horizontal_loop_end` as explicit arguments directly to
   `ccpp_physics_run()` (and analogous calls). This decouples the cap from knowing about
   the host's internal chunk-lookup arrays. The host driver sets these for each block
   iteration and passes them in; the cap uses them directly.

3. **The domain-vs-block execution contexts must be supported, but the cdata object is
   not necessarily the right mechanism.** The key information is: instance number, thread
   number, horizontal_loop_begin, horizontal_loop_end, error flag/message. If all of
   these are explicit named arguments to `ccpp_physics_*`, the cdata object becomes
   redundant scaffolding. This is an open design question to be discussed separately, but
   the UFS analysis shows that cdata carries exactly these values — the object is a
   transport container, not a framework abstraction.

4. **The `blksz` non-uniform block size is a first-class concern.** The generator must
   produce `im = gfs_control%blksz(cdata%blk_no)` (or an equivalent `horizontal_loop_extent`
   computed from the explicit begin/end) for the horizontal extent argument in run phases.

5. **GFS_interstitial as a pointer-DDT is the correct design for 3-D models.** Creating
   and destroying per block avoids memory waste from over-allocation to the maximum chunk
   size. The pointer-based field design enables selective allocation. The redesign should
   document this pattern and support it. (Whether the generator should emit the
   `type(X_interstitial_type)` DDT definition itself or only the caps is TBD.)

6. **200 optional pointer arrays in one group cap is manageable but the wrapper type
   proliferation is not.** The 4 wrapper types (`integer_r1_ptr_arr_type`,
   `real_r1_ptr_arr_type`, `real_r2_ptr_arr_type`, `character_len3_r1_ptr_arr_type`)
   should be defined once in a shared module (e.g., `ccpp_types.F90`) and reused across
   all caps, eliminating thousands of duplicate lines.

7. **Physical constants as metadata variables must be gathered into a constants DDT.**
   The redesign will collect all physics constants into a single `constants_type` DDT
   (or equivalent), reducing 30+ individual scalar arguments in the cap chain to one
   argument. This requires a metadata declaration mechanism for compound read-only
   objects (i.e., constants do not need intent tracking the way state variables do).

8. **No framework-owned persistent variables in UFS** confirms that this feature is
   optional and model-specific. The redesign needs to support it (for CAM-SIMA-like
   models) but should not force it on models that do not need it.

9. **The host driver is correctly hand-written.** The OpenMP blocking, interstitial
   lifecycle, diagnostic bucket management — these are model-specific decisions that
   belong in the host driver, not in generated code. The redesign should not try to
   generate the driver.

10. **Suite variant cap redundancy is not a concern.** For research/development, multiple
    suites are active simultaneously and generated code size doesn't matter. For
    production, only one suite is compiled and used at a time. The redesign need not
    prioritize eliminating redundant group cap code across suite variants.

---

## 12. Real-world example: Navy NEPTUNE (prebuild, restricted)

The NEPTUNE source code cannot be shared. The following is based on architectural
description provided by the lead developer.

NEPTUNE uses `ccpp-prebuild` with the same GFS physics as UFS and nearly identical suites.
Its unique distinguishing feature is **multiple coexisting CCPP physics instances** — it
is the only model among the four examples that exercises this capability at runtime.

---

### 12.1 Multiple instances — the N-dimensioned DDT array mechanism

In NEPTUNE, the host model allocates N copies of all GFS DDTs as 1-D arrays indexed by
instance number:

```fortran
type(GFS_sfcprop_type),  allocatable :: gfs_sfcprop(1:N)
type(GFS_statein_type),  allocatable :: gfs_statein(1:N)
type(GFS_stateout_type), allocatable :: gfs_stateout(1:N)
! ... all GFS DDTs dimensioned 1:N
type(GFS_control_type),  allocatable :: gfs_control(1:N)
```

The static API imports these module-level arrays via `use` statements (same as UFS).
The instance selection happens at the call site inside the group cap, using
`cdata%ccpp_instance` as the array index:

```fortran
call foo_run(                                                                   &
    tair = gfs_statein(cdata%ccpp_instance)%tair(                               &
               gfs_control(cdata%ccpp_instance)%chunk_begin(cdata%chunk_no) :   &
               gfs_control(cdata%ccpp_instance)%chunk_end(cdata%chunk_no),      &
               1:nvertical),                                                     &
    ...)
```

Three things are happening simultaneously at each call-site array section:
1. **Instance selection**: `gfs_statein(cdata%ccpp_instance)` picks the correct DDT from
   the N-element array
2. **Chunk subsetting**: `chunk_begin(chunk_no):chunk_end(chunk_no)` applies the run-phase
   horizontal slice
3. **Vertical bound**: explicit `1:nvertical`

This is the same pattern as UFS except the DDTs are 1-D arrays rather than scalars.
The generator must produce this instance-indexed subsetting when the host declares its
DDTs as arrays.

---

### 12.2 What NEPTUNE tells us about `cdata%ccpp_instance`

The `initialized(200)` array in every group cap (confirmed in both SCM and UFS caps) now
has its full motivation: it handles up to 200 simultaneous instances without requiring
per-instance cap code. The `cdata%ccpp_instance` value (1-based) is the runtime selector
into both the host DDT arrays and the `initialized` guard array.

NEPTUNE is the reason `200` is not `1`. In single-instance models (UFS, SCM, CAM-SIMA)
`cdata%ccpp_instance` is always 1 and the N-dimensioned DDT arrays have `N=1`.

---

### 12.3 Observations relevant to the redesign

1. **Multiple instances require only one change at the call site**: inserting the instance
   index at the correct dimension position. Everything else (chunking, optional variables,
   threading) composes with this unchanged.

2. **The instance dimension can appear anywhere in any host variable — not just as an
   index into an array of DDTs.** A flat array `flat_field(1:ninstance, 1:nhoriz, 1:nvert)`
   is equally valid; its call site becomes:
   ```fortran
   flat_field(instance_number, horiz_begin:horiz_end, 1:nvert)
   ```
   The generator handles this by classifying each dimension by its declared standard name.
   `instance_dimension` is a registered standard name (like `horizontal_dimension` and
   `vertical_dimension`) — the generator knows its semantics regardless of where it
   appears in the dimension list or whether the variable is a DDT array element or a
   plain array. See §13.4 for the full dimension classification model.

3. **No new cap-level mechanism is needed for multi-instance.** The instance number
   (from the control layer, see §13) is sufficient. The cap code shape is the same;
   only the call-site indexing expression differs based on the declared dimension roles.

---

## 13. Cross-cutting design decision: how host data enters the cap chain

Across all four models, two mechanisms are used for getting host model data into the
generated caps:

| Mechanism | Models using it | Description |
|-----------|----------------|-------------|
| **Module USE** | UFS, SCM, CAM-SIMA, NEPTUNE | Static API has `use ccpp_data, only: gfs_statein, ...`. Data module name is known at generation time. |
| **Command-line arguments** | capgen (optional) | Generator accepts host variable access paths as CLI flags; generated caps receive data as explicit dummy arguments. |

### 13.1 The capgen dual-mechanism problem

Capgen supports both mechanisms, and this is a direct source of its complexity. The
variable-matching logic, VarDictionary scope chains, and `CCPPDatabaseObj` all exist
partly to handle the routing of variables that may arrive via either path. Maintaining
two entry points to the data layer doubles the surface area that must be tested and
reasoned about.

### 13.2 The proposed single-mechanism approach

The redesign will use **module USE exclusively** for all host data. The reasoning:

- All four production models already use module USE, including CAM-SIMA (the capgen
  model), which does not use capgen's CLI-argument path in practice.
- Module names are stable, known at generation time, and make the generated code
  self-documenting (`use ccpp_data, only: gfs_statein` is unambiguous).
- Eliminating the CLI-argument entry path eliminates an entire class of generator
  complexity.

### 13.3 Runtime control variables — the thin explicit layer

While all *data* enters via module USE, a set of *control* variables must be passed at
runtime because they change from call to call. These are not physics data; they tell the
cap *how* to index into the data it already has access to:

| Variable | Purpose | When it matters |
|----------|---------|----------------|
| `ccpp_instance` | Select the instance dimension in host variables | NEPTUNE (N>1); others use 1 |
| `ccpp_thread_no` | Index optional pointer arrays per thread | Run phase with OpenMP |
| `horizontal_loop_begin` | Start of horizontal chunk to process | Run phase |
| `horizontal_loop_end` | End of horizontal chunk to process | Run phase |
| `ccpp_nthreads` | Max threads available for internal physics use | Non-run phases (currently `gfs_control%nthreads`) |
| `errmsg` / `errflg` | Error reporting return path | All phases |

These are exactly the values that `cdata` carries in the current implementation.
Whether they are packaged as a `ccpp_t` struct or passed as individual named arguments to
`ccpp_physics_*` is an open design question for implementation. Either way, the generator
only needs to know about these variables and their standard names — it does not need to
accept host data paths on the command line.

### 13.4 The dimension classification model

A host variable's metadata declares the **standard name of each of its dimensions** in
order. The generator classifies every dimension into one of three categories and
constructs the call-site expression accordingly.

**Category 1 — Registered dimensions.** The generator knows the semantics of these
standard names and generates special call-site expressions for them:

| Standard name | Call-site expression | Notes |
|--------------|---------------------|-------|
| `instance_dimension` | `instance_number` (scalar index) | Omitted if variable has no instance dimension |
| `horizontal_dimension` | `1:horizontal_dimension` (non-run) or `horiz_begin:horiz_end` (run) | Phase-dependent |
| `vertical_dimension` | `1:vertical_dimension` | Fixed range |

`instance_dimension` has the same registered status as `horizontal_dimension` and
`vertical_dimension`. Single-instance models simply do not declare any variables with
an `instance_dimension`, and the generator omits that index entirely.

**Category 2 — Arbitrary host-declared dimensions.** Any dimension whose standard name
is not in the registered set. These are declared in host metadata pointing to a Fortran
expression accessible via module USE — either a flat module variable or a DDT member
(e.g. `gfs_control%ntrac`, `gfs_control%kice`). The generator emits `1:expression`
at the call site, resolved at generation time from the metadata. Fixed-index extractions
(e.g. `gfs_statein%qgrs(..., gfs_control%ntqv)`) are a special case: the dimension
value is a scalar index rather than a range upper bound, and the metadata must declare
which case applies.

**Category 3 — Optional selector.** Not a dimension per se, but a boolean `active`
condition declared in variable metadata. Generates a pointer-association guard around
the call site (the pattern described in §9 and §11).

This three-category model works uniformly regardless of host layout:
- `gfs_statein(instance)%tair(horiz, vert)` — registered instance + registered horizontal + registered vertical
- `flat_field(instance, horiz, vert, ntrac)` — registered + registered + registered + arbitrary
- `flat_field(horiz, vert)` — no instance dimension, single-instance model

No special-casing per host model is needed in the generator.

### 13.5 `type = control` — metadata declaration for runtime control variables

The registered dimensions (§13.4 Category 1) are *dimension names* that appear in a
variable's `dimensions = (...)` list. Their actual *runtime values* are supplied by a
separate set of variables declared with `type = control` in host metadata.

| `type = control` standard name | Fills in registered dimension / purpose |
|-------------------------------|----------------------------------------|
| `ccpp_instance` | `instance_dimension` — scalar index selecting the active instance |
| `ccpp_thread_no` | Not a dimension; indexes optional pointer arrays per thread |
| `horizontal_loop_begin` | Lower bound of `horizontal_dimension` in run phase |
| `horizontal_loop_end` | Upper bound of `horizontal_dimension` in run phase |
| `ccpp_nthreads` | Not a dimension; max threads available for internal physics use |
| `errmsg` / `errflg` | Error reporting return path |

Variables declared `type = control` are:
- **Passed explicitly as runtime arguments** to `ccpp_physics_*` by the host driver
  (not accessed via module USE, because their values change per call)
- **Used by the generator** to construct call-site indexing expressions for registered
  dimensions, and to generate the `ccpp_nthreads` assignment before non-run scheme calls
- **Available to physics schemes** by standard name like any other variable — if a scheme
  declares a variable with a matching standard name (e.g. `ccpp_nthreads`,
  `horizontal_loop_begin`), the framework passes it as a scheme argument in the normal way

This is similar in concept to capgen's `type = host` annotation but with a narrower,
well-defined scope. The name `control` is intentional: these variables *control* how
the cap indexes into the data, not what the data is.

The set of recognized standard names for `type = control` variables is fixed and small.
Declaring them explicitly in metadata — rather than having the generator recognize magic
names — keeps the mechanism open and self-documenting.

### 13.6 Consequences for the generator

1. The generator reads host metadata to learn:
   - Module names for all host data variables (emitted as `use` statements in the static API)
   - The dimension standard names of each variable (for call-site expression construction)
   - Which variables are `type = control` (for the runtime argument layer)
2. At cap generation time, the static API's `use` statements are emitted from the module
   names — no runtime flexibility, no CLI data routing.
3. Call-site subsetting for every variable is constructed purely from its declared
   dimension standard names: registered dimensions use the Category 1 rules; arbitrary
   dimensions are resolved to Fortran expressions via the host metadata.
4. The only runtime inputs to the cap are the `type = control` variables. Their values
   are supplied by the host driver for each `ccpp_physics_*` call.
