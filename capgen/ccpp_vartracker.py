#!/usr/bin/env python3

"""ccpp_vartracker — trace a variable through a capgen ``datatable.xml``.

This is a read-side companion to ``ccpp_datafile.py``.  It is a pure-Python,
dependency-free reader (only the stdlib ``xml.etree.ElementTree``) of the
``datatable.xml`` written by :mod:`generator.datatable`.

Arguments are not passed directly from one subroutine to the next in the
datatable.  Instead, every scheme phase lists the arguments of its subroutine
in a ``<call_list>``, and each argument is identified by a CCPP standard name.
Two subroutines that list the same standard name are therefore passed "the
same" variable, even though each may use a different local name and a
different intent.  The ``<api><suites>`` section supplies the order in which
the schemes are called, which lets this script report where a variable is
written and where it is read.

Usage
-----
Exactly one report action is required per invocation::

    ccpp_vartracker.py <datatable.xml> --trace <name> --suite <suite>
    ccpp_vartracker.py <datatable.xml> --list-calls <name>
    ccpp_vartracker.py <datatable.xml> --origin <name>
    ccpp_vartracker.py <datatable.xml> --search <substring>
    ccpp_vartracker.py <datatable.xml> --suite-structure <suite>
    ccpp_vartracker.py <datatable.xml> --scheme-calls <scheme>
    ccpp_vartracker.py <datatable.xml> --group-call-list <group> \\
        --suite <suite>

``<name>`` is a standard name unless ``--by local_name`` is given, in which
case it is resolved to every standard name that uses it as a local name.

Examples::

    ccpp_vartracker.py datatable.xml --search ozone
    ccpp_vartracker.py datatable.xml --trace air_temperature --suite MPAS_GFS
    ccpp_vartracker.py datatable.xml --trace gt0 --by local_name \\
        --suite MPAS_GFS

Notes specific to capgen
------------------------
* Phase elements are discovered by tag, not by name, as in
  ``ccpp_datafile.py``.  Within a scheme, phases are reported in call order
  (register, init, timestep_init, run, timestep_final, final) even though
  the generator writes them alphabetically.
* ``--origin`` and ``--group-call-list`` rely on the ``<var_dictionaries>``
  section.  Datatables written by older generators carry only the host
  dictionary, so suite-owned (interstitial) variables and group call lists
  are not available from them.

Exit codes
----------
0 — success
1 — user error (unknown name, missing section, unreadable file, etc.)
2 — internal error (bug in this script)
"""

import argparse
import logging
import sys
import xml.etree.ElementTree as ET
from typing import Dict, List, Optional, Tuple


########################################################################
# Logging
########################################################################

def _init_log(name: str) -> logging.Logger:
    """Return a named logger that writes to a stream handler."""
    logger = logging.getLogger(name)
    if not logger.handlers:
        logger.addHandler(logging.StreamHandler())
    return logger


_LOGGER = _init_log('ccpp_vartracker')


########################################################################
# Constants
########################################################################

# Order in which a suite calls the phases of a scheme.  Phase elements in
# the datatable are written alphabetically, so they must be re-ordered.
_PHASE_ORDER = ['register', 'init', 'timestep_init', 'run',
                'timestep_final', 'final']

# Short descriptions of an argument intent, used by --trace.
_INTENT_LABELS = {'in': 'reads ', 'out': 'WRITES', 'inout': 'R/W   '}

# Reports that cannot be answered without a suite name.
_SUITE_REPORTS = ['trace', 'group_call_list']

_VALID_REPORTS = [
    {'report': 'trace', 'metavar': 'NAME',
     'help': ("Trace a variable through the call lists of suite <SUITE>, "
              "in call order (requires --suite)")},
    {'report': 'list_calls', 'metavar': 'NAME',
     'help': ("List every subroutine call that has a variable in its "
              "call list, in datatable order")},
    {'report': 'origin', 'metavar': 'NAME',
     'help': ("Report whether a variable is supplied by the host or is "
              "suite-owned (interstitial), and where it is first produced")},
    {'report': 'search', 'metavar': 'SUBSTRING',
     'help': "Return the standard names that contain <SUBSTRING>"},
    {'report': 'suite_structure', 'metavar': 'SUITE_NAME',
     'help': "Return the groups and schemes of suite <SUITE_NAME>"},
    {'report': 'scheme_calls', 'metavar': 'SCHEME_NAME',
     'help': "Return the call list of every phase of scheme <SCHEME_NAME>"},
    {'report': 'group_call_list', 'metavar': 'GROUP_NAME',
     'help': ("Return the deduplicated external call list of group "
              "<GROUP_NAME> (requires --suite)")},
]


########################################################################
# Errors
########################################################################

class CCPPVarTrackerError(ValueError):
    """User-facing error found while tracing a variable in a datatable."""


########################################################################
# CLI
########################################################################

def _command_line_parser() -> argparse.ArgumentParser:
    """Build and return the argument parser.

    Returns
    -------
    argparse.ArgumentParser
    """
    parser = argparse.ArgumentParser(
        prog='ccpp_vartracker.py',
        description=("Trace a variable through the subroutine call lists "
                     "recorded in a ccpp_capgen datatable.  Exactly one "
                     "report action is required."),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        'datatable',
        type=str,
        help='Path to a data table XML file created by capgen',
    )
    # Only one action per call
    group = parser.add_mutually_exclusive_group(required=True)
    for report in _VALID_REPORTS:
        group.add_argument(
            '--{}'.format(report['report'].replace('_', '-')),
            required=False,
            type=str,
            metavar=report['metavar'],
            default='',
            help=report['help'],
        )
    parser.add_argument(
        '--suite',
        required=False,
        type=str,
        metavar='SUITE_NAME',
        default='',
        dest='suite_name',
        help='Suite to use for --trace and --group-call-list',
    )
    defval = 'standard_name'
    parser.add_argument(
        '--by',
        required=False,
        choices=['standard_name', 'local_name'],
        default=defval,
        help=("Treat the variable name given to --trace, --list-calls and "
              "--origin as a standard name or a local name "
              "(default: {})".format(defval)),
    )
    return parser


def parse_command_line(args: Optional[List[str]]) -> argparse.Namespace:
    """Parse and return the command-line arguments."""
    return _command_line_parser().parse_args(args)


########################################################################
# Accessor functions to retrieve information from a datatable file
########################################################################

def _read_datatable(datatable: str) -> ET.Element:
    """Read XML file *datatable* and return its root node.

    >>> _read_datatable('/no/such/file.xml')
    Traceback (most recent call last):
    ...
    ccpp_vartracker.CCPPVarTrackerError: Cannot read datatable, '/no/such/file.xml': [Errno 2] No such file or directory: '/no/such/file.xml'
    """
    try:
        return ET.parse(datatable).getroot()
    except (OSError, ET.ParseError) as exc:
        emsg = "Cannot read datatable, '{}': {}"
        raise CCPPVarTrackerError(emsg.format(datatable, exc))


def _find_table_section(table: ET.Element, elem_type: str) -> ET.Element:
    """Look for and return an element type, <elem_type>, in <table>.
    Raise an exception if the element is not found.

    # Test present section
    >>> table = ET.fromstring("<ccpp_datatable><schemes></schemes></ccpp_datatable>")
    >>> _find_table_section(table, 'schemes').tag
    'schemes'

    # Test missing section
    >>> table = ET.fromstring("<ccpp_datatable><api></api></ccpp_datatable>")
    >>> _find_table_section(table, 'schemes').tag
    Traceback (most recent call last):
    ...
    ccpp_vartracker.CCPPVarTrackerError: Element type, 'schemes', not found in table
    """
    found = table.find(elem_type)
    if found is None:
        emsg = "Element type, '{}', not found in table"
        raise CCPPVarTrackerError(emsg.format(elem_type))
    return found


def _phase_sort_key(phase: str) -> int:
    """Return the sort key that puts <phase> in call order.
    Phases that are not in the standard list are placed last.

    >>> sorted(['final', 'run', 'init'], key=_phase_sort_key)
    ['init', 'run', 'final']
    >>> sorted(['banana', 'run'], key=_phase_sort_key)
    ['run', 'banana']
    """
    if phase in _PHASE_ORDER:
        return _PHASE_ORDER.index(phase)
    return len(_PHASE_ORDER)


def _retrieve_calls(table: ET.Element) -> List[Dict[str, str]]:
    """Return one entry for each variable of each scheme phase in <table>.

    Entries are in datatable order.  Each is a dictionary with keys
    ``scheme``, ``phase``, ``subroutine_name``, ``module``,
    ``standard_name``, ``intent``, ``local_name`` and
    ``diagnostic_name_fixed``.  Any attribute missing from the datatable
    is returned as an empty string.

    >>> table = ET.fromstring("<ccpp_datatable><schemes><scheme name='s'>"\
                "<run name='s' subroutine_name='s_run' module='s'>"\
                "<call_list><var name='air_temperature' intent='in' "\
                "local_name='tgrs'/></call_list></run></scheme></schemes>"\
                "</ccpp_datatable>")
    >>> calls = _retrieve_calls(table)
    >>> [(c['scheme'], c['phase'], c['standard_name'], c['local_name']) for c in calls]
    [('s', 'run', 'air_temperature', 'tgrs')]
    """
    calls = list()
    for scheme in _find_table_section(table, 'schemes').findall('scheme'):
        for phase in scheme:
            call_list = phase.find('call_list')
            if call_list is None:
                continue
            for var in call_list.findall('var'):
                calls.append({
                    'scheme': scheme.get('name', ''),
                    'phase': phase.tag,
                    'subroutine_name': phase.get('subroutine_name', ''),
                    'module': phase.get('module', ''),
                    'standard_name': var.get('name', ''),
                    'intent': var.get('intent', ''),
                    'local_name': var.get('local_name', ''),
                    'diagnostic_name_fixed':
                        var.get('diagnostic_name_fixed', ''),
                })
    return calls


def _retrieve_suites(
    table: ET.Element,
) -> Dict[str, List[Tuple[str, List[str]]]]:
    """Return the groups and schemes of each suite in <table>.

    The result maps a suite name to a list of (group name, scheme names)
    in call order.  Returns an empty dictionary if <table> has no suites.

    >>> table = ET.fromstring("<ccpp_datatable><api><suites>"\
                "<suite name='fruit'><group name='g1'>"\
                "<scheme>apple</scheme><scheme>pear</scheme></group>"\
                "</suite></suites></api></ccpp_datatable>")
    >>> _retrieve_suites(table)
    {'fruit': [('g1', ['apple', 'pear'])]}
    """
    suites = dict()
    api = table.find('api')
    suites_elem = None if api is None else api.find('suites')
    if suites_elem is None:
        return suites
    for suite in suites_elem.findall('suite'):
        groups = list()
        for group in suite.findall('group'):
            schemes = [s.text for s in group.findall('scheme')]
            groups.append((group.get('name'), schemes))
        suites[suite.get('name')] = groups
    return suites


def _retrieve_var_dictionaries(table: ET.Element) -> List[Dict]:
    """Return the variable dictionaries of <table> in datatable order.

    Each entry has keys ``name``, ``type``, ``parent``, ``suite`` and
    ``variables``.  ``variables`` maps a standard name to the attributes of
    its ``<var>`` element.  ``suite`` is the name of the suite that
    contains the dictionary, or an empty string for the host and API
    dictionaries.  Group names repeat from suite to suite, so the suite is
    tracked by position: the generator writes each suite dictionary
    directly before its group dictionaries.

    >>> table = ET.fromstring("<ccpp_datatable><var_dictionaries>"\
                "<var_dictionary name='h' type='host'><variables>"\
                "<var name='a' local_name='la'/></variables>"\
                "</var_dictionary>"\
                "<var_dictionary name='s1' type='suite' parent='ccpp_api'>"\
                "<variables/></var_dictionary>"\
                "<var_dictionary name='g' type='group' parent='s1'>"\
                "<variables/></var_dictionary>"\
                "</var_dictionaries></ccpp_datatable>")
    >>> [(d['name'], d['type'], d['suite']) for d in _retrieve_var_dictionaries(table)]
    [('h', 'host', ''), ('s1', 'suite', 's1'), ('g', 'group', 's1')]
    >>> _retrieve_var_dictionaries(table)[0]['variables']
    {'a': {'name': 'a', 'local_name': 'la'}}
    """
    result = list()
    dicts = table.find('var_dictionaries')
    if dicts is None:
        return result
    suite_name = ''
    for vdict in dicts.findall('var_dictionary'):
        dtype = vdict.get('type', '')
        if dtype == 'suite':
            suite_name = vdict.get('name', '')
        elif dtype in ('host', 'api'):
            suite_name = ''
        variables = dict()
        vars_elem = vdict.find('variables')
        if vars_elem is not None:
            for var in vars_elem.findall('var'):
                variables[var.get('name')] = dict(var.attrib)
        result.append({'name': vdict.get('name', ''), 'type': dtype,
                       'parent': vdict.get('parent', ''),
                       'suite': suite_name, 'variables': variables})
    return result


def _resolve_standard_names(calls: List[Dict[str, str]], name: str,
                            by: str) -> List[str]:
    """Return the sorted standard names that <name> refers to.
    If <by> is 'standard_name', <name> is looked up directly; if it is
    'local_name', every standard name that has <name> as a local name in
    some call list is returned.

    >>> calls = [{'standard_name': 'air_temperature', 'local_name': 'gt0'},\
                 {'standard_name': 'air_temperature', 'local_name': 'tgrs'},\
                 {'standard_name': 'x_wind', 'local_name': 'gt0'}]
    >>> _resolve_standard_names(calls, 'air_temperature', 'standard_name')
    ['air_temperature']
    >>> _resolve_standard_names(calls, 'gt0', 'local_name')
    ['air_temperature', 'x_wind']
    >>> _resolve_standard_names(calls, 'banana', 'standard_name')
    Traceback (most recent call last):
    ...
    ccpp_vartracker.CCPPVarTrackerError: No variable found for standard_name, 'banana'.  Use --search to look for a standard name.
    """
    if by == 'local_name':
        names = sorted({c['standard_name'] for c in calls
                        if c['local_name'] == name})
    else:
        names = sorted({c['standard_name'] for c in calls
                        if c['standard_name'] == name})
    if not names:
        emsg = ("No variable found for {}, '{}'.  "
                "Use --search to look for a standard name.")
        raise CCPPVarTrackerError(emsg.format(by, name))
    return names


def _require_suite(suites: Dict[str, List[Tuple[str, List[str]]]],
                   suite_name: str) -> None:
    """Raise CCPPVarTrackerError if <suite_name> is not a key of <suites>.

    >>> _require_suite({'fruit': []}, 'fruit')
    >>> _require_suite({'fruit': []}, 'veggie')
    Traceback (most recent call last):
    ...
    ccpp_vartracker.CCPPVarTrackerError: Unknown suite, 'veggie'.  Valid suites: fruit
    """
    if suite_name not in suites:
        emsg = "Unknown suite, '{}'.  Valid suites: {}"
        raise CCPPVarTrackerError(
            emsg.format(suite_name, ', '.join(sorted(suites)) or '(none)'))


def _order_calls(calls: List[Dict[str, str]],
                 suites: Dict[str, List[Tuple[str, List[str]]]],
                 suite_name: str) -> List[Tuple[str, Dict[str, str]]]:
    """Return the entries of <calls> that suite <suite_name> executes, as
    (group name, call) pairs in execution order: group order, then scheme
    order within the group, then phase order within the scheme.  Calls from
    schemes that the suite does not use are left out.

    >>> suites = {'fruit': [('g1', ['pear', 'apple'])]}
    >>> calls = [{'scheme': 'apple', 'phase': 'run'},\
                 {'scheme': 'pear', 'phase': 'run'},\
                 {'scheme': 'pear', 'phase': 'init'},\
                 {'scheme': 'kiwi', 'phase': 'run'}]
    >>> [(g, c['scheme'], c['phase']) for g, c in _order_calls(calls, suites, 'fruit')]
    [('g1', 'pear', 'init'), ('g1', 'pear', 'run'), ('g1', 'apple', 'run')]
    """
    _require_suite(suites, suite_name)
    ordered = list()
    for group_name, scheme_names in suites[suite_name]:
        for scheme_name in scheme_names:
            scheme_calls = [c for c in calls if c['scheme'] == scheme_name]
            # sorted is stable, so arguments stay in call-list order
            scheme_calls.sort(key=lambda c: _phase_sort_key(c['phase']))
            ordered.extend((group_name, c) for c in scheme_calls)
    return ordered


def _is_protected(attrs: Dict[str, str]) -> bool:
    """Return True if the host variable attributes <attrs> say protected.

    >>> _is_protected({'name': 'a', 'protected': 'True'})
    True
    >>> _is_protected({'name': 'a'})
    False
    """
    return attrs.get('protected') == 'True'


########################################################################
# Report formatting
########################################################################

def _format_call(call: Dict[str, str], group_name: Optional[str] = None) -> str:
    """Return a one-line description of <call>, prefixed with
    <group_name> if it is given.

    >>> call = {'scheme': 's', 'phase': 'run', 'subroutine_name': 's_run',\
                'module': 's', 'intent': 'in', 'local_name': 'tgrs'}
    >>> _format_call(call, 'physics')
    '[physics] s.run (s_run in s): intent=in     local_name=tgrs'
    >>> _format_call(call)
    's.run (s_run in s): intent=in     local_name=tgrs'
    """
    prefix = '' if group_name is None else '[{}] '.format(group_name)
    return '{}{}.{} ({} in {}): intent={:<6} local_name={}'.format(
        prefix, call['scheme'], call['phase'], call['subroutine_name'],
        call['module'], call['intent'], call['local_name'] or '?')


def _report_list_calls(table: ET.Element, name: str, by: str) -> str:
    """Return every call that has variable <name> in its call list."""
    calls = _retrieve_calls(table)
    host_vars = dict()
    for vdict in _retrieve_var_dictionaries(table):
        if vdict['type'] == 'host':
            host_vars.update(vdict['variables'])
    report = ''
    for std_name in _resolve_standard_names(calls, name, by):
        var_calls = [c for c in calls if c['standard_name'] == std_name]
        tag = ''
        if std_name in host_vars:
            tag = ' [host var'
            if _is_protected(host_vars[std_name]):
                tag += ', protected'
            tag += ']'
        report += '\n=== {}  ({} call(s)){} ===\n'.format(
            std_name, len(var_calls), tag)
        for call in var_calls:
            report += '  {}\n'.format(_format_call(call))
    return report


def _report_trace(table: ET.Element, name: str, by: str,
                  suite_name: str) -> str:
    """Return the calls that have variable <name> in their call list, in
    the order suite <suite_name> executes them.  Calls by schemes that the
    suite does not use are not reported (see --list-calls for those)."""
    calls = _retrieve_calls(table)
    suites = _retrieve_suites(table)
    _require_suite(suites, suite_name)
    var_dicts = _retrieve_var_dictionaries(table)
    report = ''
    for std_name in _resolve_standard_names(calls, name, by):
        report += "\n=== Trace of '{}' in suite '{}' ===\n".format(
            std_name, suite_name)
        for vdict in var_dicts:
            if (vdict['type'] == 'suite' and vdict['name'] == suite_name
                    and std_name in vdict['variables']):
                attrs = vdict['variables'][std_name]
                report += ('  (suite-owned variable, allocated by capgen; '
                           'first produced in {}.{})\n'.format(
                               attrs.get('source_scheme', '?'),
                               attrs.get('source_phase', '?')))
        var_calls = [c for c in calls if c['standard_name'] == std_name]
        ordered = _order_calls(var_calls, suites, suite_name)
        if not ordered:
            report += '  (not used in this suite)\n'
        for index, (group_name, call) in enumerate(ordered, 1):
            label = _INTENT_LABELS.get(call['intent'], call['intent'] or '?')
            report += '  {:>2}. {}  {}\n'.format(
                index, label, _format_call(call, group_name))
    return report


def _report_origin(table: ET.Element, name: str, by: str) -> str:
    """Return where variable <name> comes from: the host model, or a suite
    that owns it.

    >>> table = ET.fromstring("<ccpp_datatable><schemes><scheme name='s'>"\
                "<run name='s' subroutine_name='s_run' module='s'>"\
                "<call_list><var name='a' intent='in' local_name='la'/>"\
                "<var name='b' intent='out' local_name='lb'/></call_list>"\
                "</run></scheme></schemes><var_dictionaries>"\
                "<var_dictionary name='h' type='host'><variables>"\
                "<var name='a' local_name='la' protected='True'/></variables>"\
                "</var_dictionary>"\
                "<var_dictionary name='fruit' type='suite' parent='ccpp_api'>"\
                "<variables><var name='b' local_name='lb' units='K' "\
                "source_scheme='s' source_phase='run'/></variables>"\
                "</var_dictionary></var_dictionaries></ccpp_datatable>")
    >>> print(_report_origin(table, 'a', 'standard_name').rstrip())
    <BLANKLINE>
    a: provided by the HOST (local_name=la, protected)
    >>> print(_report_origin(table, 'b', 'standard_name').rstrip())
    <BLANKLINE>
    b: suite-owned in 'fruit'
      local_name: lb
      units: K
      source_scheme: s
      source_phase: run

    # A host variable that no scheme uses is still found
    >>> table.find('var_dictionaries/var_dictionary/variables').append(\
            ET.fromstring("<var name='unused' local_name='lu'/>"))
    >>> print(_report_origin(table, 'lu', 'local_name').rstrip())
    <BLANKLINE>
    unused: provided by the HOST (local_name=lu)
    """
    var_dicts = _retrieve_var_dictionaries(table)
    # Provenance comes from the dictionaries, so a variable that no scheme
    # takes as an argument (e.g., a host control variable) must be found too.
    known = _retrieve_calls(table)
    for vdict in var_dicts:
        for std_name, attrs in vdict['variables'].items():
            known.append({'standard_name': std_name,
                          'local_name': attrs.get('local_name', '')})
    report = ''
    for std_name in _resolve_standard_names(known, name, by):
        found = False
        for vdict in var_dicts:
            attrs = vdict['variables'].get(std_name)
            if attrs is None:
                continue
            if vdict['type'] == 'host':
                found = True
                report += '\n{}: provided by the HOST (local_name={}{})\n'.format(
                    std_name, attrs.get('local_name'),
                    ', protected' if _is_protected(attrs) else '')
            elif vdict['type'] == 'suite':
                found = True
                report += "\n{}: suite-owned in '{}'\n".format(
                    std_name, vdict['name'])
                for key in ('local_name', 'units', 'type', 'kind',
                            'dimensions', 'source_scheme', 'source_phase',
                            'allocatable'):
                    if attrs.get(key):
                        report += '  {}: {}\n'.format(key, attrs[key])
        if not found:
            report += ("\n{}: not found in the host or suite dictionaries "
                       "(it may be supplied by the framework, e.g., a "
                       "constituent, or the datatable may predate the "
                       "var_dictionaries chain)\n".format(std_name))
    return report


def _report_search(table: ET.Element, substring: str) -> str:
    """Return the standard names that contain <substring>, one per line.

    >>> table = ET.fromstring("<ccpp_datatable><schemes><scheme name='s'>"\
                "<run name='s' subroutine_name='s_run' module='s'>"\
                "<call_list><var name='ozone_forcing' intent='in'/>"\
                "<var name='air_temperature' intent='in'/></call_list>"\
                "</run></scheme></schemes></ccpp_datatable>")
    >>> _report_search(table, 'OZONE')
    'ozone_forcing'
    >>> _report_search(table, 'banana')
    Traceback (most recent call last):
    ...
    ccpp_vartracker.CCPPVarTrackerError: No standard names contain 'banana'
    """
    names = sorted({c['standard_name'] for c in _retrieve_calls(table)
                    if substring.lower() in c['standard_name'].lower()})
    if not names:
        raise CCPPVarTrackerError(
            "No standard names contain '{}'".format(substring))
    return '\n'.join(names)


def _report_suite_structure(table: ET.Element, suite_name: str) -> str:
    """Return the groups and schemes of suite <suite_name>."""
    suites = _retrieve_suites(table)
    _require_suite(suites, suite_name)
    report = 'Suite: {}\n'.format(suite_name)
    for group_name, scheme_names in suites[suite_name]:
        report += '  Group: {}\n'.format(group_name)
        for scheme_name in scheme_names:
            report += '    - {}\n'.format(scheme_name)
    return report


def _report_scheme_calls(table: ET.Element, scheme_name: str) -> str:
    """Return the call list of each phase of scheme <scheme_name>."""
    report = ''
    for scheme in _find_table_section(table, 'schemes').findall('scheme'):
        if scheme.get('name') != scheme_name:
            continue
        report = 'Scheme: {}\n'.format(scheme_name)
        phases = sorted(scheme, key=lambda p: _phase_sort_key(p.tag))
        for phase in phases:
            report += '\n  [{}] {} (module {})\n'.format(
                phase.tag, phase.get('subroutine_name'), phase.get('module'))
            call_list = phase.find('call_list')
            if call_list is None:
                continue
            for var in call_list.findall('var'):
                report += '    intent={:<6} local_name={:<20} {}\n'.format(
                    var.get('intent', ''), var.get('local_name', ''),
                    var.get('name'))
        return report
    emsg = "Unknown scheme, '{}'"
    raise CCPPVarTrackerError(emsg.format(scheme_name))


def _report_group_call_list(table: ET.Element, group_name: str,
                            suite_name: str) -> str:
    """Return the deduplicated external call list of group <group_name>
    in suite <suite_name>."""
    suites = _retrieve_suites(table)
    _require_suite(suites, suite_name)
    list_name = '{}_call_list'.format(group_name)
    for vdict in _retrieve_var_dictionaries(table):
        if (vdict['type'] == 'group_call_list' and vdict['name'] == list_name
                and vdict['suite'] == suite_name):
            variables = vdict['variables']
            report = ("=== External call list of group '{}' in suite '{}' "
                      "({} var(s)) ===\n".format(group_name, suite_name,
                                                  len(variables)))
            for std_name in sorted(variables):
                attrs = variables[std_name]
                report += '  intent={:<6} local_name={:<20} {}\n'.format(
                    attrs.get('intent', '?'), attrs.get('local_name', '?'),
                    std_name)
            return report
    emsg = ("No call list found for group '{}' in suite '{}'.  Use "
            "--suite-structure to check the names; the datatable may "
            "predate the var_dictionaries chain.")
    raise CCPPVarTrackerError(emsg.format(group_name, suite_name))


def vartracker_report(datatable: str, action: str, value: str,
                      suite_name: str = '', by: str = 'standard_name') -> str:
    """Perform a lookup <action> on <datatable> and return the result.

    Parameters
    ----------
    datatable : str
        Path to a datatable XML file created by capgen.
    action : str
        One of the ``report`` entries of ``_VALID_REPORTS``.
    value : str
        The variable, scheme, suite or group the report is about.
    suite_name : str, optional
        Suite name, required by the ``trace`` and ``group_call_list``
        actions.
    by : str, optional
        'standard_name' or 'local_name'; how <value> names a variable.

    Returns
    -------
    str
    """
    if action not in [x['report'] for x in _VALID_REPORTS]:
        raise CCPPVarTrackerError("Invalid action, '{}'".format(action))
    if (action in _SUITE_REPORTS) and not suite_name:
        emsg = "--{} requires --suite"
        raise CCPPVarTrackerError(emsg.format(action.replace('_', '-')))
    table = _read_datatable(datatable)
    if action == 'trace':
        result = _report_trace(table, value, by, suite_name)
    elif action == 'list_calls':
        result = _report_list_calls(table, value, by)
    elif action == 'origin':
        result = _report_origin(table, value, by)
    elif action == 'search':
        result = _report_search(table, value)
    elif action == 'suite_structure':
        result = _report_suite_structure(table, value)
    elif action == 'scheme_calls':
        result = _report_scheme_calls(table, value)
    else:
        result = _report_group_call_list(table, value, suite_name)
    return result


########################################################################
# Main entry point
########################################################################

def main(argv: Optional[List[str]] = None) -> int:
    """Command-line entry point.

    Parameters
    ----------
    argv : list of str, optional
        Override ``sys.argv[1:]`` (used by tests).

    Returns
    -------
    int
        Exit code: 0 = success, 1 = user error, 2 = internal error.
    """
    if argv is None:
        argv = sys.argv[1:]
    pargs = parse_command_line(argv)
    action = None
    value = ''
    for report in _VALID_REPORTS:
        if getattr(pargs, report['report']):
            action = report['report']
            value = getattr(pargs, report['report'])
    try:
        result = vartracker_report(pargs.datatable, action, value,
                                   suite_name=pargs.suite_name, by=pargs.by)
    except CCPPVarTrackerError as exc:
        _LOGGER.error("%s", exc)
        return 1
    except Exception as exc:  # pylint: disable=broad-except
        _LOGGER.error("Internal error: %s", exc, exc_info=True)
        return 2
    print("{}".format(result.rstrip()))
    return 0


if __name__ == '__main__':
    sys.exit(main())
