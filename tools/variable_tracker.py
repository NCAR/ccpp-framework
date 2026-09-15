#!/usr/bin/env python3
"""
variable_tracker.py - Trace a CCPP argument (by standard_name or local_name) across
subroutine call lists in a CCPP "datatable" XML file.

Background on the file format
------------------------------
A CCPP datatable XML describes, for every physics scheme, the argument list
("call_list") of each of its lifecycle subroutines (init / timestep_init /
run / final). Each argument is identified by a *standard_name* that is
unique and consistent across the whole model -- two different subroutines
that both have a <var name="X" .../> entry in their call_list are, in
effect, passed "the same argument" (X), even though each subroutine may use
a different local_name for it and a different intent (in/out/inout).

<schemes>
  <scheme name="SchemeA">
    <run name="SchemeA" subroutine_name="SchemeA_run" module="SchemeA">
      <call_list>
        <var name="air_temperature" intent="inout" local_name="gt0" .../>
        ...

<suites>
  <suite name="MySuite">
    <group name="physics">
      <scheme>SchemeA</scheme>
      <scheme>SchemeB</scheme>
      ...

The <suites> section gives the actual run-time call *order* of schemes,
grouped by physics group, for each suite. Combining the two lets us answer
"where does argument X come from, and where does it go, in what order?"

Usage
-----
  # List every subroutine call that touches a standard_name, in file order
  python3 variable_tracker.py list <file.xml> air_temperature

  # Trace a standard_name in actual execution order for one suite
  python3 variable_tracker.py trace <file.xml> air_temperature --suite MPAS_GFS

  # Same, but you only know the local (Fortran) variable name used
  # somewhere -- it will be resolved to the standard_name(s) that share it
  python3 variable_tracker.py trace <file.xml> gt0 --by local_name --suite MPAS_GFS

  # Search for standard_names matching a substring (helps find the exact name)
  python3 variable_tracker.py search <file.xml> ozone

  # List available suites/groups/schemes
  python3 variable_tracker.py suites <file.xml>
  python3 variable_tracker.py scheme <file.xml> GFS_photochemistry
"""

import argparse
import sys
import xml.etree.ElementTree as ET
from collections import defaultdict, namedtuple

PHASES = ("init", "timestep_init", "run", "final")

Call = namedtuple(
    "Call",
    ["scheme", "phase", "subroutine_name", "module",
     "intent", "local_name", "diagnostic_name", "standard_name"],
)


class CCPPDataTable:
    def __init__(self, xml_path):
        self.tree = ET.parse(xml_path)
        self.root = self.tree.getroot()

        # standard_name -> list[Call]  (in document order)
        self.by_standard_name = defaultdict(list)
        # local_name -> set of standard_names that use it anywhere
        self.local_to_standard = defaultdict(set)
        # scheme_name -> {phase: {"subroutine_name":.., "module":.., "calls":[Call,...]}}
        self.schemes = {}
        # suite_name -> [ (group_name, [scheme_name, ...]), ... ]
        self.suites = {}

        self._parse_schemes()
        self._parse_suites()

    def _parse_schemes(self):
        schemes_el = self.root.find(".//schemes")
        if schemes_el is None:
            return
        for scheme_el in schemes_el.findall("scheme"):
            sname = scheme_el.get("name")
            self.schemes[sname] = {}
            for phase in PHASES:
                phase_el = scheme_el.find(phase)
                if phase_el is None:
                    continue
                subroutine_name = phase_el.get("subroutine_name")
                module = phase_el.get("module")
                calls = []
                call_list_el = phase_el.find("call_list")
                if call_list_el is not None:
                    for var_el in call_list_el.findall("var"):
                        std = var_el.get("name")
                        intent = var_el.get("intent")
                        local = var_el.get("local_name")
                        diag = var_el.get("diagnostic_name")
                        call = Call(sname, phase, subroutine_name, module,
                                    intent, local, diag, std)
                        calls.append(call)
                        self.by_standard_name[std].append(call)
                        if local:
                            self.local_to_standard[local].add(std)
                self.schemes[sname][phase] = {
                    "subroutine_name": subroutine_name,
                    "module": module,
                    "calls": calls,
                }

    def _parse_suites(self):
        suites_el = self.root.find(".//suites")
        if suites_el is None:
            return
        for suite_el in suites_el.findall("suite"):
            sname = suite_el.get("name")
            groups = []
            for group_el in suite_el.findall("group"):
                gname = group_el.get("name")
                scheme_names = [s.text for s in group_el.findall("scheme")]
                groups.append((gname, scheme_names))
            self.suites[sname] = groups

    # ------------------------------------------------------------------
    # Lookup helpers
    # ------------------------------------------------------------------
    def resolve_standard_names(self, name, by="standard_name"):
        """Return a list of standard_names matching `name`."""
        if by == "standard_name":
            return [name] if name in self.by_standard_name else []
        elif by == "local_name":
            return sorted(self.local_to_standard.get(name, []))
        else:
            raise ValueError("`by` must be 'standard_name' or 'local_name'")

    def search_standard_names(self, substring):
        substring = substring.lower()
        return sorted(n for n in self.by_standard_name if substring in n.lower())

    def calls_for(self, standard_name):
        return self.by_standard_name.get(standard_name, [])

    def ordered_calls_for(self, standard_name, suite_name):
        """
        Return the calls touching `standard_name`, ordered the way they
        actually execute within `suite_name` (group order, then scheme
        order within the group, then phase order init->run->final).
        Calls made by schemes not part of the suite are appended at the end
        under 'other'.
        """
        if suite_name not in self.suites:
            raise KeyError(f"Unknown suite: {suite_name}")

        target_calls = self.calls_for(standard_name)
        if not target_calls:
            return []

        # index target calls by (scheme, phase) for quick lookup
        lut = defaultdict(list)
        for c in target_calls:
            lut[(c.scheme, c.phase)].append(c)

        ordered = []
        seen = set()
        for gname, scheme_names in self.suites[suite_name]:
            for sname in scheme_names:
                for phase in PHASES:
                    for c in lut.get((sname, phase), []):
                        ordered.append((gname, c))
                        seen.add(id(c))

        # anything not covered by this suite's scheme list (e.g. scheme
        # exists in the table but isn't part of this suite) goes last
        leftovers = [c for c in target_calls if id(c) not in seen]
        for c in leftovers:
            ordered.append((None, c))

        return ordered


# ----------------------------------------------------------------------
# CLI presentation helpers
# ----------------------------------------------------------------------

def fmt_call(c, group=None):
    loc = c.local_name or "?"
    prefix = f"[{group}] " if group else ""
    return (f"{prefix}{c.scheme}.{c.phase} "
            f"({c.subroutine_name} in {c.module}): "
            f"intent={c.intent:<6} local_name={loc}")


def cmd_list(args):
    table = CCPPDataTable(args.xml_file)
    names = table.resolve_standard_names(args.name, by=args.by)
    if not names:
        print(f"No match for '{args.name}' (by {args.by}).", file=sys.stderr)
        print("Try `search` to find the right standard_name.", file=sys.stderr)
        return 1
    for std in names:
        calls = table.calls_for(std)
        print(f"\n=== {std}  ({len(calls)} call(s)) ===")
        for c in calls:
            print("  " + fmt_call(c))
    return 0


def cmd_trace(args):
    table = CCPPDataTable(args.xml_file)
    names = table.resolve_standard_names(args.name, by=args.by)
    if not names:
        print(f"No match for '{args.name}' (by {args.by}).", file=sys.stderr)
        print("Try `search` to find the right standard_name.", file=sys.stderr)
        return 1

    for std in names:
        print(f"\n=== Trace of '{std}' in suite '{args.suite}' ===")
        try:
            ordered = table.ordered_calls_for(std, args.suite)
        except KeyError as e:
            print(f"  {e}", file=sys.stderr)
            return 1
        if not ordered:
            print("  (not used in this suite)")
            continue
        for i, (group, c) in enumerate(ordered, 1):
            direction = {
                "in": "reads ",
                "out": "WRITES",
                "inout": "R/W   ",
            }.get(c.intent, c.intent or "?")
            print(f"  {i:>2}. {direction}  " + fmt_call(c, group))
    return 0


def cmd_search(args):
    table = CCPPDataTable(args.xml_file)
    matches = table.search_standard_names(args.substring)
    if not matches:
        print("No matches.")
        return 1
    for m in matches:
        print(m)
    return 0


def cmd_suites(args):
    table = CCPPDataTable(args.xml_file)
    for sname, groups in table.suites.items():
        print(f"\nSuite: {sname}")
        for gname, scheme_names in groups:
            print(f"  Group: {gname}")
            for s in scheme_names:
                print(f"    - {s}")
    return 0


def cmd_scheme(args):
    table = CCPPDataTable(args.xml_file)
    info = table.schemes.get(args.scheme_name)
    if info is None:
        print(f"Unknown scheme: {args.scheme_name}", file=sys.stderr)
        return 1
    print(f"Scheme: {args.scheme_name}")
    for phase in PHASES:
        if phase not in info:
            continue
        p = info[phase]
        print(f"\n  [{phase}] {p['subroutine_name']} (module {p['module']})")
        for c in p["calls"]:
            print(f"    intent={c.intent:<6} local_name={c.local_name:<20} "
                  f"standard_name={c.standard_name}")
    return 0


def main():
    parser = argparse.ArgumentParser(
        description="Trace CCPP arguments across subroutine call lists.")
    sub = parser.add_subparsers(dest="command", required=True)

    p_list = sub.add_parser("list", help="List all calls that use a given name")
    p_list.add_argument("xml_file")
    p_list.add_argument("name")
    p_list.add_argument("--by", choices=["standard_name", "local_name"],
                         default="standard_name")
    p_list.set_defaults(func=cmd_list)

    p_trace = sub.add_parser(
        "trace", help="Trace a name in execution order for one suite")
    p_trace.add_argument("xml_file")
    p_trace.add_argument("name")
    p_trace.add_argument("--suite", required=True)
    p_trace.add_argument("--by", choices=["standard_name", "local_name"],
                          default="standard_name")
    p_trace.set_defaults(func=cmd_trace)

    p_search = sub.add_parser("search", help="Search standard_names by substring")
    p_search.add_argument("xml_file")
    p_search.add_argument("substring")
    p_search.set_defaults(func=cmd_search)

    p_suites = sub.add_parser("suites", help="List suites/groups/schemes")
    p_suites.add_argument("xml_file")
    p_suites.set_defaults(func=cmd_suites)

    p_scheme = sub.add_parser("scheme", help="Show a scheme's full call lists")
    p_scheme.add_argument("xml_file")
    p_scheme.add_argument("scheme_name")
    p_scheme.set_defaults(func=cmd_scheme)

    args = parser.parse_args()
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
