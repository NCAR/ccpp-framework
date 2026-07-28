# CCPP framework — capgen v1

## Follow-up work: one list, tracked in git

`doc/followups.md` is the **single source of truth** for deferred items, open
questions, and transient shims awaiting removal.

- Add new follow-ups there.  Do not start a second list in another document.
- Cite items by ID (`FU-014`).  Other documents reference IDs; they must not
  restate the items.
- Closed items keep their row, with the date and the reason.
- `doc/constituents_overhaul.md` is the register of record for the
  constituents area and keeps its own status taxonomy; `doc/followups.md` §2
  indexes into it rather than duplicating it.

## Memory reconciliation (this project spans several machines)

Work on this repository happens on **more than one machine**, and each has
its own auto-memory, task list, and scratch notes.  None of that travels.
`doc/followups.md` is what travels.

**Run a reconciliation sweep when any of these is true:**

1. The user asks for one.
2. You are about to add a new item to `doc/followups.md`.
3. This machine is missing from the reconciliation log in `doc/followups.md`
   §5, or its entry is older than the newest commit touching that file.

**The sweep:**

1. Read every local, machine-scoped store you have access to:
   - the auto-memory directory for this project (`MEMORY.md` and the
     individual memory files it indexes),
   - any task/todo list held by the session or the harness,
   - scratch notes and `RESUME.md`-style files in working directories
     outside the repository (e.g. sibling repro directories).
2. For each item found, decide what it is:
   - **Durable project work** — deferred work, an open question, a decision
     and its rationale, a shim-removal trigger, a known test failure, a
     cross-repo dependency.  These belong in `doc/followups.md`.
   - **Machine-local fact** — where clones live on *this* machine, local
     paths, shell/toolchain setup, personal working preferences.  These stay
     in auto-memory and must not be copied into the repository.
   - **Session-scoped noise** — intermediate reasoning, superseded plans.
     Discard.
3. For each durable item, check whether `doc/followups.md` already covers it
   (match on substance, not wording).  If not, add a row with a new ID, the
   date it was raised, and a `file:line` or document-section pointer.  If it
   is covered but the local store has extra detail — a rationale, a
   reproduction, a decision that was made — fold that detail in.
4. If a local memory contradicts `doc/followups.md`, the *newer* evidence
   wins; correct the stale one and say which you changed.
5. Update the reconciliation log (`doc/followups.md` §5) with this machine's
   hostname and the date.
6. Report what you added, folded in, or corrected.  Do not silently rewrite
   existing rows.

**Do not delete auto-memory entries just because they were copied into
`doc/followups.md`** — replace the durable content with a one-line pointer to
the ID so the local store stays useful without becoming a rival list.

## Verifying claims about original capgen

When the question is "what did original capgen actually emit?", read
`origin/develop:test/*/*_host_integration.F90` — those files record the real
expected call lists and are the ground truth.  Reasoning from an early
return in `scripts/suite_objects.py:match_variable` (or any other single
code path) is **not** proof and has produced a wrong, pushed commit
(`501d1c0`, since reverted: it claimed original capgen never put
register-phase `ccpp_constituent_properties_t` args on a call list;
`test_advection_host_integration.F90` lists them in both `test_outvars1`
and `test_reqvars1`).

## Documentation cross-references

Committed documents must not cite auto-memory files.  Memory is per-machine,
so such a reference is dangling for every other machine and for every
reviewer.  Four such references accumulated before 2026-07-28
(`project_implementation_status.md`, `design_constituent_api.md`,
`design_constituents_mutability.md`, `design_constituent_host_wins.md`) and
none of the targets existed in this clone.  Cite a committed document, a
`file:line`, or a `doc/followups.md` ID instead.
