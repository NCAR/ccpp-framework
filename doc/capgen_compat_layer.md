# The CAM-SIMA ↔ capgen compatibility layer — short brief

*For the original ccpp-capgen author, to orient a feedback pass.
Full reference: `cime_config/capgen_compat/README.md` in the CAM-SIMA
tree. Last revised 2026-06-05.*

## Why it exists

capgen is replacing original ccpp-capgen as CAM-SIMA's CCPP code
generator. Rather than rewrite CAM-SIMA's autogen pipeline up front, a
thin **compatibility layer** lets that pipeline keep calling original
capgen's Python surface while capgen does the generation underneath.
`cam_autogen.py`, `generate_registry_data.py`, `write_init_files.py`,
and `hist_config.py` are **unmodified** — they import the facade instead
of original capgen. CAM-SIMA owns this directory; capgen owns nothing
in it. It is **transient scaffolding** (see "Convergence goal").

## What the facade reconstructs

| Original-capgen surface | Rebuilt over (capgen) | File |
|---|---|---|
| `cap_database.host_model_dict()` / `.call_list(phase)` | flat `host_dict` + per-phase `ResolvedArg` lists | `_cap_database.py` |
| per-variable `Var` accessors (`get_prop_value`, `source.ptype`, `array_ref`, `intrinsic_elements`, `call_string`, …) | `HostVarEntry` (host path) / `ResolvedArg` (call-list path) | `_var_wrapper.py` |
| `MetaVar` / `MetadataSection` accessors used by the registry generator | capgen `MetaVar` / `MetadataSection` via monkey-patch | `metadata_table.py` |
| `ParseObject`, `FortranWriter`, the richer `ParseContext` | **vendored verbatim from original capgen** (CAM-SIMA's own scripts use these; capgen does not) | `parse_object.py`, `fortran_write.py`, `parse_source.py` |

## Three design contracts worth your eyes

capgen classifies every scheme argument into exactly **one** source —
`control | host | suite | constituent` — on a flat `ResolvedArg` (there
is no `ConstituentVarDict`/scope-chain). The adapter keys on that:

1. **`source='suite'` is dropped from `call_list`.** These are
   interstitials produced and consumed within one suite (they live in
   `<suite>_data`), so they are not host variables; surfacing them would
   trip `write_init_files`' "missing required host variable" check.
2. **`source='constituent'` is mapped to `advected/constituent=True`** so
   `write_init_files` routes it through the constituents object (skip
   USE-import, skip the IC read). Key detail: the adapter keys on the
   **source**, not on a per-arg `is_constituent` flag. capgen now lets
   an *unflagged* scheme consume a constituent — base *or* `tendency_of_*`
   — because whether a name is a constituent is the **host's** decision;
   such consumers carry `is_constituent=False` while still being
   framework-supplied.
3. **`type = module` → `type = host`.** capgen renamed your
   `type = module`; the shim rewrites it at parse time and records which
   tables were "module" so `write_init_files` still gets `ptype='module'`
   (allocate + initialise) vs `ptype='host'` (passed via the arg list).

## Convergence goal (the important framing)

End state: this directory **does not exist**. CAM-SIMA talks to capgen
through **three CLI utilities** — `ccpp_validator.py`,
`ccpp_capgen.py`, `ccpp_datafile.py` — plus the on-disk
`datatable.xml` contract. A well-defined, feature-equivalent Python
API to these three utilities is also discussed in 
`cime_config/capgen_compat/README.md`. No production CAM-SIMA path should
depend on capgen's Python internals; today's `return_state=True` hook handing
back `(host_dict, suite_resolutions)` is scaffolding for this layer only.
The README has a phased retirement plan (A–G) with a measurable LOC drop
per phase.

## Status

`kessler`, `rrtmgp`, and `se_cslam` (the full `cam7` suite) build **and
run to completion** on Derecho under **both gnu and intel**, with
bit-comparable results.

## Feedback we'd value

1. Does the four-source model (`control/host/suite/constituent`) capture
   everything `ConstituentVarDict` did for CAM-SIMA?
2. Are there `cap_database` / `Var` accessors that `write_init_files` or
   `generate_registry_data` rely on that we've under- or mis-modeled?
3. Is **CLI + `datatable.xml`** a sufficient convergence interface for
   everything CAM-SIMA currently reads out of original-capgen Python
   objects — or is there state that has no on-disk equivalent yet?
