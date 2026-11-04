# capgen v1.1 review

Dom Heinzeller, 2026-10-16

Thanks for this — the design underneath it is real, and the write-up made it
easy to review.  I don't think it can merge in its present shape, though most
of what stands in the way is mechanical: four changes alter capgen v1
behaviour for CPU-only builds, one new generated symbol runs back into the
Intel 90-character symbol limit that #787 just cleared, and the generator has
picked up a small Fortran parser.  We can take some of that on.  The one
substantial ask is that the GPU path work for NEPTUNE and UFS before it lands.
Everything below was measured on the branch at `f0d2519` rather than taken from
the write-up.

## Verified, not assumed

| Claim in the write-up | Measured on `f0d2519` |
| --- | --- |
| 1,604 unit tests pass | 1,676 pass, 0 fail (`python3 unit-tests/run_tests.py`) |
| With the new features off, output is byte-identical in all 12 end-to-end directories | `datatable.xml` is identical in all 12.  **Four** suite caps differ, not three: `advection`, `advection_auto_clone`, `constituents_dim`, `instances_advection` |
| What those four diffs contain | Only the constituent-append rewrite.  Order and deep-copy semantics are preserved, so constituent ordering (FU-030) is unaffected |
| Branch currency | One squashed commit on `e21f3db` (8 Sep).  Missing #790 (full access path for non-control variables) and #787 (suite-cap symbol length) |
| Size | 87 files, +9,829 / -94.  New modules `device_residency.py` (1,032 lines), `cxx_schemes.py` (909), `device_emit.py` (762), `timers.py` (167); new `unit-tests/test_device_residency.py` (1,333 lines, 93 tests) |
| Four changes are on by default | Confirmed: constituent append, deferred allocation, per-call DDT lifetime, module-name inference.  Two of them fire nowhere in the end-to-end corpus |

That last row is the one I'd watch, more than the identical-output row.
`suite_allocate` doesn't trigger deferred allocation and nothing in the corpus
triggers the per-call lifetime rule, so two of the four default-on changes rest
on unit tests alone.  Identical generated output isn't quite the same thing as
unchanged behaviour.

## What holds up

The residency model reads as an abstraction rather than a pile of special
cases.  Each array carries a handoff state, `{H}`, `{H,D}` or `{D}`, derived
from its `host_access` metadata; every group phase starts from that state and
restores it before returning, so groups stay independent by construction.  The
module docstring in `device_residency.py` is good enough to review the design
from, which is rarer than it should be.

- **Restrictions fail loudly.**  Every unsupported case is an explicit
  `CCPPError` naming suite, scheme, phase, argument and standard name, raised
  during planning.  I found no silently wrong code in the device path.
- **The new metadata attributes are validated as input.**  `memory_space`,
  `host_access` and `language` are checked against allowed-value sets, scoped
  by table type, and use `''` for "not set" so an explicit value stays
  distinguishable from an inherited one.
- **The tests are behavioural.**  `test_device_residency.py` runs the planner
  on synthetic suites and asserts on the plan: zero-iteration subcycles,
  optional-argument guards, both backends, and a negative case for every
  exclusion in `assign_run_local_ddts`.
- **`doc/compiler_issues.md` earns its place**, and the reconciliation sweep
  was run, with four `derecho6` entries in the follow-up log.  Both noted and
  appreciated.
- **One thing that looks wrong and isn't.**  The device-copy teardown guard
  compares against `CCPP_SUITE_FRAMEWORK_INITIALIZED` rather than a range,
  which matches v1's own idiom at `suite_cap.py:906`.

What follows isn't about code quality.  It's about a few invariants capgen v1
commits to, where this branch takes a different view.

## What we need before it lands: UFS and NEPTUNE

Our condition for taking anything from this branch onto `feature/capgen-v1` is
that the UFS and NEPTUNE regression tests run, and that everything capgen v1
supports today keeps working.

A feature that's unsupported *only when dispatching to the GPU* is completely
fine.  Transformations are the example: an argument needing a unit, kind or
vertical-flip conversion that is also marked `memory_space = device` is better
refused than guessed at, as long as the refusal is immediate and names the
variable and the reason.  That's already how the branch behaves — every
rejection sits inside `if directive:` (`capgen/ccpp_capgen.py:1121`), raised
during planning before any cap is written — and it's worth writing down,
because it's the property most easily lost in a later refactor.

Two changes sit outside that gate, which is the next section.  First, though,
what the regression tests will and won't show.  With no new flags, UFS and
NEPTUNE exercise the four default-on changes and nothing else; the GPU path
isn't touched.  And as the restrictions stand, the GPU path can't run on either
host: multi-instance interstitials are rejected (`inst_access != '(1)'`), which
rules out NEPTUNE, and host variables reached through an instance or thread
subscript are rejected, which rules out blocked and chunked host data and so
UFS.

I'd rather we didn't stop there.  A GPU path that serves single-instance,
unblocked hosts and leaves everyone else to arrange something of their own is
roughly how capgen v0 arrived at the state we're replacing.  So I'd ask for
both of those before this lands rather than as follow-up rows.  Until they're
in, "the UFS and NEPTUNE tests pass" doesn't tell us much about the feature,
because on those two hosts the feature is switched off.

```text
                              .-----------------------.
                              |  --directive given?   |
                              '-----------------------'
                      no  <---'                       '--->  yes
  +---------------------------------+   +---------------------------------------+
  | Without --directive             |   | With --directive acc or omp           |
  |                                 |   |                                       |
  | The full capgen v1 feature set. |   | The device residency plan runs.       |
  | No code in device_residency.py  |   | Unsupported cases raise CCPPError     |
  | is reachable.                   |   | during planning, before any cap.      |
  +---------------------------------+   +---------------------------------------+
  Transforms, multi-instance hosts                      |
  and blocked host data keep working                    v
  exactly as in v1.                     +---------------------------------------+
                                        | Rejected, naming the variable         |
                                        |                                       |
                                        | ! multi-instance interstitials        |
                                        |   (NEPTUNE)                           |
                                        | ! instance- or thread-subscripted     |
                                        |   host data (UFS)                     |
                                        |   transforms (unit, kind, vert. flip) |
                                        |   host control-table arrays           |
                                        |   derived-type arrays                 |
                                        |   constituent properties              |
                                        |   the register phase                  |
                                        +---------------------------------------+
                                        ! the two we'd need before this lands

  ####################################################################
  # Outside the gate: on for every host, with or without directives  #
  #                                                                  #
  # Deferred allocation replaces the v1 allocate-everything rule;    #
  #   --strict-allocatable restores it.                              #
  # assign_run_local_ddts changes the lifetime of suite-owned        #
  #   storage for every host.                                        #
  # Our ask: opt-in for the first, and leave the second out.         #
  ####################################################################
```

## Defaults: keeping v1 behaviour as the default

Two of the four default-on changes alter behaviour for every host, GPU or not.
I'd like v1 behaviour to stay what you get without asking for anything.

**Deferred allocation.**  v1's rule is that a non-allocatable suite variable
sized by a quantity written in the init phase is an error, and that the
variable should be allocatable and scheme-allocated.  The branch turns that
around: `validate_init_dimensions(suite_res, strict=False)` allows it and CCPP
allocates after the last init-phase writer of the dimension, with
`--strict-allocatable` bringing the old rule back.  FU-050 asks which way the
default should go, so: I'd keep v1's rule, drop `--strict-allocatable`, and add
a flag that turns deferred allocation on explicitly — which is what you
suggested in your email.

One guard looks missing.  Nothing checks that no *earlier* init-phase scheme
reads the variable before the allocation point; v1's "allocate everything in
`suite_data_init_fields`" made that impossible by construction.  A hard error
naming that earlier reader, raised in the pass that places the allocation,
would close it, and since the resolver already has the init-phase call order
it's a loop rather than new machinery.  Two smaller things in that path, worth
fixing either way: `suite_data._deferred_dim_expr` reads `entry.is_control`
after `host_dict.get(token)`, so an absent token raises `AttributeError` rather
than a `CCPPError` with context; and the same function appends
`entry.local_name` to the generated `use ..., only:` list, which for a variable
reached through a derived type is the leaf rather than the root object — the
class of bug #790 fixed in `group_cap.py`.  `_dim_local_expr` may be worth a
look alongside it.

**The per-call derived-type lifetime.**  `assign_run_local_ddts`
(`suite_resolver.py:1380`, called unconditionally at `ccpp_capgen.py:1117`)
gives a scalar non-allocatable derived-type suite variable a per-call lifetime
when every run-phase use is in one group, the first is `intent(out)`, it's used
in no phase after `run`, it has no device or C++ use, and it needs no
transform.  The group cap declares `ccpp_rl_<name>` as a local of `<group>_run`, and the suite-data copy
stays behind for register and init.  The motivation is clear from your report:
v0 made RRTMGP's 13 objects group locals, v1 persists them, `rrtmgp_inputs`
takes them `intent(out)` so Fortran frees their allocatable components on
entry, RRTMGP has no matching `!$acc exit data`, and the next allocation
overlaps the stale mapping.  The rule picks exactly v0's 13 objects and the run
comes out bit-for-bit identical to v0, which is a nice piece of detective work.

I'd still rather we left it out, and not as an opt-in flag either.  Mixed
ownership — some interstitials group-owned, some suite-owned — was one of the
things capgen v1 set out to get rid of, and v1 has a single rule: suite-owned
storage lives in the suite data module for the whole run, per instance.  The
only per-call locals a group cap declares are the ones that marshal data for a
call: transform temporaries, pointer wrappers for optional or
possibly-unallocated arguments, and the C++ device scratch block, which isn't
part of the physics-suite logic.  An interstitial doesn't really belong in that
set, and nothing in a `.meta` file would tell a host whether its object
persists.  Two details if the rule is ever revisited: init-phase uses keep the
suite-data copy, so the object exists twice and an init-phase write is dropped
at the first run call (`unit-tests/test_suite_resolver.py:5816` asserts that
split); and the object becomes automatic storage, so large non-allocatable
components move to the stack.

That leaves the RRTMGP problem with you, which I know is the less convenient
answer.  The stale mappings are RRTMGP's own — it issues the `enter data` on
components CCPP never maps and mostly can't name, so the matching `exit data`
has to go where the `enter data` is.  Steering a device present table by
changing host storage duration isn't something capgen should be doing from the
outside, and the practical consequence is that RRTMGP won't run on the GPU
until that fix is in.

## Fortran parsing: we'd like to keep it in the validator

`variable_resolver._source_module()` opens the scheme's paired `.F90`, `.f90`,
`.F` or `.f` file and regex-matches `module <name>` to infer `module_name` when
the metadata doesn't declare it.  The implementation is careful — it returns
`''` unless the file holds exactly one module, and the negative lookahead keeps
`module procedure` out — so my concern isn't that it's buggy.  It's that capgen
has one Fortran parser, in `ccpp_validator.py`, and the generator trusts
metadata and never reads Fortran.  That separation is much of why the generator
can be reasoned about, and it's written down as an invariant, so I'd rather not
make an exception.

The other half is that the inference removes an error that used to tell a
developer to declare something.  `module_name =` in
`[ccpp-table-properties]` is genuinely required when the Fortran module name
differs from the table name, and the validator is the right place to say so, by
file and line, quoting the line to add.  CAM-SIMA's
`compute_subgrid_vertical_velocity.F90`, with two schemes in one file, would
get that line rather than an inference.

## What we can take off your plate

Three of these are mechanical and none touches the design.  Happy to do them on
top of the branch and hand back something pullable, if that's useful.

1. **Bring the branch current.**  The two commits it's missing, #790 and #787,
   plus anything else that lands on `feature/capgen-v1` in the meantime.
2. **The Intel symbol limit.**  `device_emit.suite_sync_sub_name()` is
   `'{}_device_update_{}'.format(suite_name, direction)`, public in the suite
   cap and imported by the host cap under that name.  For the 29-character UFS
   suite behind #786, `ccpp_<suite>_cap` is 38 characters, the subroutine is
   50, and Intel's mangled `<module>_mp_<sub>` comes to 92 against a 90-char
   cap.  #787 made every suite-cap subroutine prefix-free for this reason, with
   `_SUITE_CAP_OWN_SYMS` as the collision guard and the host cap aliasing them
   as `use ..., only: <alias> => <target>`.  So: spell these
   `suite_device_update_<direction>`, register them in the guard, alias in the
   host cap.  It only bites on ifx, with a long suite name, with `--directive`,
   which is why the SCM tests couldn't have caught it.
3. **Follow-up row numbering.**  FU-036 to FU-038 are already taken on
   `feature/capgen-v1`, and the branch's closed table marks FU-037 done, which
   collides with one that's open and on our critical path.  Those three rows
   would become FU-051 to FU-053; FU-039 to FU-050 are free and keep their
   numbers.  It follows from forking before those landed, so nothing to
   apologise for.

The flag inversion, taking the Fortran parser out, the C++ tests and the CMake
wiring would be yours once the questions below are settled — we're not
proposing to rewrite the branch.

## The rest

**Timers in their own PR.**  `--timer-style` emits `use perf_mod` (GPTL, CESM)
or `use mpp_mod` (FMS, UFS) into generated caps, and generated code depends
only on capgen's own three utilities.  This is the one piece here that isn't
really generalized yet.  If timers are wanted — and they probably are — the
host could declare them in metadata: separate discussion, separate PR.

**Maybe collapse the three debug flags.**  `--device-debug`, `--device-trace`
and `--device-sync-debug` each emit different code.  One levelled flag would do
the same work, and every independent switch multiplies the combinations
somebody has to test.

**Both features together, if the C++ side gets to the same standard.**  The
design and documentation of `cxx_schemes.py` are as good as the device
planner's; its module docstring is effectively an ABI specification, down to
why neither Fortran `logical` nor `logical(c_bool)` is a safe C `bool` on NVHPC
25.9 or ifx, and it has 16 explicit error raises carrying scheme, phase and
variable.  The tests aren't there yet, and that part is measured:

| Measure | `device_residency.py` | `cxx_schemes.py` |
| --- | --- | --- |
| generator lines | 1,032 | 909 |
| dedicated unit-test module | `test_device_residency.py`, 1,333 lines | none |
| tests | 83 | 10, in a class inside the device-residency file |
| negative tests (`assertRaises`) | 13 | 0, against 16 error paths |
| end-to-end directories | 2, outside CI | 2, outside CI |

Both could land together once the C++ side has a `test_cxx_schemes.py` of
comparable depth covering those 16 error paths.  If that's more work than it's
worth right now, splitting it and letting the GPU data movement go first seems
entirely reasonable.

**The four end-to-end tests in the CMake harness, for whatever CI can run.**
They arrive with a hand-written `build.sh`, a `test.cfg` per directory and a
PBS script, which the existing harness doesn't know about; CI is ubuntu with
gfortran 9 to 12 and cmake, while `build.sh` knows nvfortran and ifx.  Of the
modes it defines, `off` and `cpu` (`--directive acc`, CPU build, directives
ignored) need no GPU, and `cpu` is the valuable one — it shows that generated
code carrying directives still compiles and runs identically on the host.
Those would fit in `end-to-end-tests/CMakeLists.txt`, which needs `CXX` on its
`LANGUAGES` line.  The negative fixtures (`bad_host_access`, `bad_intent`,
`bad_realloc`, `bad_mask`, `cxx_error`) are code generation only and need no
compiler at all.  The true GPU modes need NVHPC and an A100, so a script is
the right home for them.

**The new metadata attributes, in the PR description as well as the code.**
Their defaults match v1 behaviour, which I checked: `language` defaults to
`fortran` (`metadata_table.py:1097`); `memory_space` is unset and
`effective_memory_space` resolves it to `host` (`metadata_table.py:974`);
`host_access` is unset and resolves to `read_write`
(`variable_resolver.py:340`), the most conservative handoff.  The last two are
read only under `--directive`, so existing metadata is unaffected either way.
Still, an attribute that extends the metadata format is a contract across every
CCPP host, so it's worth calling out explicitly however safe the default is,
and it needs the usual metadata review rather than code review alone.

**The constituent append we'd take as it is.**  It's the only permanent change
to generated output when every new feature is off.  v1 emits
`items = [items, scheme_consts]`, and on NVHPC 25.9 and 26.1 that corrupts the
array it appends to: `ccpp_constituent_properties_t` holds
`character(len=:), allocatable` components, and after the reallocating
assignment many of the *old* elements keep the right string lengths and hold
garbage.  It's size-dependent (41 items then 7 corrupted 32 of 48 names, while
3 then 28 was clean), doesn't reproduce with a plain derived type, and ifx is
fine.  In CAM-SIMA's `FPHYStest` GPU run it showed up as the constituent table
failing to lock: `Bad vertical dimension, '<garbage>imension'`.

The replacement sidesteps the self-reference:

```fortran
block
  type(ccpp_constituent_properties_t), allocatable :: prev(:)
  call move_alloc(items, prev)
  allocate(items(size(prev) + size(scheme_consts)))
  items(1:size(prev)) = prev
  items(size(prev)+1:) = scheme_consts
end block
```

Same order, same deep-copy semantics, standard Fortran, and it changes four
suite caps and nothing else.  Gating it on the compiler would mean capgen
emitting different code per vendor, which seems worse than a workaround that's
correct everywhere.  `doc/compiler_issues.md` issue 5 documents it with a
reproducer, and that file should stay.

**The documentation is two different kinds of file.**
`doc/gpu_data_movement_plan.md` (160 lines) is a design document and belongs in
`doc/` as it is.  `doc/gpu_data_movement_report.md` (754 lines) is two things
stapled together: sections 1 to 9, about 390 lines, are a usable guide, and the
remaining 360 lines are dated run logs — the CAM-SIMA `aux_sima` check, Kessler
on the GPU, the timer measurements, the SCM regression runs.  Those read as
evidence for the PR rather than reference material, and in `doc/` they'll be
stale within a release.  Keeping the guide beside the plan, or folded into it,
and moving the logs into the PR description would be my suggestion.

**One PR per semantic change, if you can face it.**  The allocation default,
the per-call lifetime, the module-name inference and the GPU data movement are
four independent decisions, and in one commit they have to be accepted or
rejected together.  Split, each can go at its own pace, and the three that
aren't the GPU feature are small.

## How this review was produced, and what it didn't cover

- Branch fetched at `f0d2519` and compared against its merge base `e21f3db` in
  a worktree.
- Unit suite run from that worktree with `python3 unit-tests/run_tests.py`:
  1,676 pass, 0 fail.
- Generated output compared by regenerating all 12 end-to-end tests with both
  trees and diffing, using the flags each test's `CMakeLists.txt` implies
  (`--legacy-auto-clone-constituents` for `advection_auto_clone`,
  `--kind-type kind_phys=REAL64` for `capgen`).
- Nothing above is quoted from the write-up without checking it.  Where the two
  disagree, the measured figure is the one above.
- **Nothing was compiled and no GPU was involved.**  The A100 timings, the
  CAM-SIMA `aux_sima` results and the SCM `supported_ifx` comparison come from
  the write-up and are untested here.  The Intel symbol-length finding is
  arithmetic on the generated names rather than an ifx run, and it's worth
  confirming that way before anyone spends time on it.
- `cxx_schemes.py` was reviewed at the level of its interface, its documented
  ABI, its error paths and its test coverage, not line by line.
- The review was produced with Claude Code, at my direction.  The judgements
  throughout are mine.
