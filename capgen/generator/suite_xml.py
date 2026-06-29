#!/usr/bin/env python3

"""Suite Definition File (SDF) parser for ccpp-capgen.

A Suite Definition File is an XML document that describes which physics
schemes to run, in which order, and how to group them.  This module:

1. Reads and schema-validates the SDF (v1.0 or v2.0).
2. For v2.0 SDFs: expands ``<nested_suite>`` references recursively.
3. Writes the expanded XML to ``<output_root>/ccpp_<suite_name>_expanded.xml``.
4. Builds an in-memory :class:`Suite` object that the code generator uses.

Suite XML format (v2.0)
-----------------------
::

    <?xml version="1.0" encoding="UTF-8"?>
    <suite name="SUITE_NAME" version="2.0">

      <!-- optional: a single scheme called once at suite init -->
      <init>suite_init_scheme</init>

      <group name="GROUP_NAME">
        <scheme>scheme_a</scheme>

        <!-- subcycle: repeat the enclosed schemes N times  -->
        <!-- loop="2" is a literal count; loop="some_stdname" is resolved
             against control variable metadata at cap-generation time.      -->
        <subcycle loop="N_OR_STDNAME">
          <scheme>scheme_b</scheme>
          <!-- subcycles may be nested -->
        </subcycle>

      </group>

      <!-- additional groups ... -->

      <!-- optional: a single scheme called once at suite final -->
      <final>suite_final_scheme</final>

    </suite>

Backward compatibility
----------------------
* Schema v1.0 suites are accepted (no nested-suite expansion).
* The old element spellings ``<initalize>`` (typo) and ``<finalize>`` are
  accepted but a :mod:`logging` warning is emitted directing the author to
  use ``<init>`` / ``<final>`` instead.

Expanded XML file
-----------------
After expanding all ``<nested_suite>`` references the result is written as::

    <output_root>/ccpp_<suite_name>_expanded.xml

This file is for human inspection only; it is not consumed by subsequent
generator runs.

Schema location
---------------
The XSD files live in ``capgen/schema/``.  Their path is resolved relative
to this source file at runtime, so no extra configuration is needed.
"""

import logging
import os
import xml.etree.ElementTree as ET
from typing import Dict, List, Optional, Union

from metadata.parse_tools import (
    CCPPError,
    read_xml_file,
    find_schema_version,
    expand_nested_suites,
    write_xml_file,
)
from metadata.parse_tools.xml_tools import validate_xml_file

########################################################################
# Constants
########################################################################

#: Directory containing the XSD schemas, relative to this source file.
_SCHEMA_DIR = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    'schema',
)

#: Accepted ``<init>`` element name.  The old ccpp-prebuild schema also
#: tolerated ``<initalize>`` (typo) and ``<initialize>``; capgen
#: rejects both with a hard error so SDFs migrate to the canonical
#: short name.
_INIT_TAG = 'init'

#: Accepted ``<final>`` element name.  The old ccpp-prebuild schema
#: also tolerated ``<finalize>``; capgen rejects it.
_FINAL_TAG = 'final'

#: Element names that were valid in the old schema but are rejected in
#: capgen's v2.0 SDF format.  Map to the canonical short form so the
#: error message can point at the right replacement.
_REJECTED_INIT_TAGS = frozenset({'initalize', 'initialize'})
_REJECTED_FINAL_TAGS = frozenset({'finalize'})


########################################################################
# In-memory suite data model
########################################################################

class SuiteScheme:
    """A single scheme call within a group or subcycle.

    Parameters
    ----------
    name : str
        Scheme name — must match a ``type = scheme`` metadata table.

    Examples
    --------
    >>> SuiteScheme('my_scheme').name
    'my_scheme'
    """

    def __init__(self, name: str):
        self.name: str = name.strip()

    def scheme_names(self) -> List[str]:
        """Return this scheme's name in a one-element list."""
        return [self.name]

    def __repr__(self) -> str:
        return f"SuiteScheme({self.name!r})"


class SuiteSubcycle:
    """A ``<subcycle>`` loop wrapping one or more scheme call sites.

    Parameters
    ----------
    loop : str or None
        Loop-count value as written in the XML ``loop`` attribute.
        May be:

        * An integer literal (e.g. ``"2"`` or ``"10"``) — a compile-time
          constant.
        * A Fortran identifier (e.g. ``"num_subcycles_for_scheme6"``) — a
          CCPP standard name resolved against control variable metadata at
          cap-generation time; becomes the value of ``ccpp_loop_extent``
          inside the loop.
        * ``None`` if the attribute is absent (treated as a single iteration).

    items : list
        Ordered sequence of :class:`SuiteScheme`, :class:`SuiteSubcycle`,
        or :class:`SuiteSubcol` children.

    Examples
    --------
    >>> subcycle = SuiteSubcycle(loop='2', items=[SuiteScheme('sch_a')])
    >>> subcycle.loop
    '2'
    >>> subcycle.is_literal_count
    True
    >>> subcycle2 = SuiteSubcycle(loop='num_subcycles_for_ag', items=[])
    >>> subcycle2.is_literal_count
    False
    """

    def __init__(self, loop: Optional[str], items: list):
        self.loop: Optional[str] = loop.strip() if loop else None
        self.items: list = items

    @property
    def is_literal_count(self) -> bool:
        """Return True if the loop count is an integer literal."""
        if self.loop is None:
            return True
        try:
            int(self.loop)
            return True
        except ValueError:
            return False

    def scheme_names(self) -> List[str]:
        """Return all scheme names referenced inside this subcycle."""
        names = []
        for item in self.items:
            names.extend(item.scheme_names())
        return names

    def __repr__(self) -> str:
        return f"SuiteSubcycle(loop={self.loop!r}, nitems={len(self.items)})"


class SuiteSubcol:
    """A ``<subcol>`` sub-column processing element.

    Sub-column processing uses a ``gen`` routine to generate sub-columns
    from the GCM column and an ``avg`` routine to average them back.

    Parameters
    ----------
    gen_routine : str
        Fortran identifier of the sub-column generation routine.
    avg_routine : str
        Fortran identifier of the sub-column averaging routine.
    items : list
        Ordered sequence of children (schemes and/or subcycles).
    """

    def __init__(self, gen_routine: str, avg_routine: str, items: list):
        self.gen_routine: str = gen_routine.strip()
        self.avg_routine: str = avg_routine.strip()
        self.items: list = items

    def scheme_names(self) -> List[str]:
        """Return all scheme names referenced inside this subcol."""
        names = []
        for item in self.items:
            names.extend(item.scheme_names())
        return names

    def __repr__(self) -> str:
        return (f"SuiteSubcol(gen={self.gen_routine!r}, "
                f"avg={self.avg_routine!r}, nitems={len(self.items)})")


# Type alias for any element that can appear inside a group.
GroupItem = Union[SuiteScheme, SuiteSubcycle, SuiteSubcol]


class SuiteGroup:
    """A named ``<group>`` within a suite.

    Parameters
    ----------
    name : str
        Group name (must be a valid Fortran identifier, unique within suite).
    items : list of GroupItem
        Ordered sequence of scheme calls, subcycles, and subcol elements.

    Examples
    --------
    >>> grp = SuiteGroup('dynamics', [SuiteScheme('dyn_scheme')])
    >>> grp.name
    'dynamics'
    >>> grp.scheme_names()
    ['dyn_scheme']
    """

    def __init__(self, name: str, items: List[GroupItem]):
        self.name: str = name.strip()
        self.items: List[GroupItem] = items

    def scheme_names(self) -> List[str]:
        """Return all unique scheme names called in this group (ordered, no dedup)."""
        names = []
        for item in self.items:
            names.extend(item.scheme_names())
        return names

    def unique_scheme_names(self) -> List[str]:
        """Return unique scheme names preserving first-occurrence order."""
        seen = set()
        result = []
        for name in self.scheme_names():
            if name not in seen:
                seen.add(name)
                result.append(name)
        return result

    def __repr__(self) -> str:
        return f"SuiteGroup({self.name!r}, nitems={len(self.items)})"


class Suite:
    """In-memory representation of a fully-parsed and expanded SDF.

    Parameters
    ----------
    name : str
        Suite name from the ``name`` attribute of ``<suite>``.
    version : list of int
        Schema version ``[major, minor]``.
    source_file : str
        Absolute path to the original ``.xml`` source file.
    groups : list of SuiteGroup
        Ordered list of groups.
    init_scheme : str or None
        Name of the suite-level init scheme (``<init>`` element), or ``None``.
    final_scheme : str or None
        Name of the suite-level final scheme (``<final>`` element), or ``None``.
    expanded_file : str or None
        Path to the written expanded XML file, set after :func:`parse_suite_xml`
        writes it.

    Examples
    --------
    >>> g = SuiteGroup('grp', [SuiteScheme('sch')])
    >>> s = Suite('my_suite', [2, 0], '/path/to/f.xml', [g], None, None)
    >>> s.name
    'my_suite'
    >>> s.group_names()
    ['grp']
    >>> s.all_scheme_names()
    ['sch']
    """

    def __init__(
        self,
        name: str,
        version: List[int],
        source_file: str,
        groups: List[SuiteGroup],
        init_scheme: Optional[str],
        final_scheme: Optional[str],
        expanded_file: Optional[str] = None,
    ):
        self.name: str                  = name
        self.version: List[int]         = version
        self.source_file: str           = source_file
        self.groups: List[SuiteGroup]   = groups
        self.init_scheme: Optional[str] = init_scheme
        self.final_scheme: Optional[str] = final_scheme
        self.expanded_file: Optional[str] = expanded_file

    def group_names(self) -> List[str]:
        """Return the group names in declaration order."""
        return [g.name for g in self.groups]

    def get_group(self, name: str) -> Optional[SuiteGroup]:
        """Return the group with *name*, or ``None``."""
        for grp in self.groups:
            if grp.name == name:
                return grp
        return None

    def all_scheme_names(self) -> List[str]:
        """Return all unique scheme names across all groups (first-occurrence order)."""
        seen = set()
        result = []
        for grp in self.groups:
            for name in grp.scheme_names():
                if name not in seen:
                    seen.add(name)
                    result.append(name)
        return result

    def __repr__(self) -> str:
        return (f"Suite({self.name!r}, version={self.version}, "
                f"ngroups={len(self.groups)})")


########################################################################
# XML-to-object conversion
########################################################################

def _parse_group_items(parent: ET.Element) -> List[GroupItem]:
    """Parse the child elements of a ``<group>`` or ``<subcycle>``/``<subcol>``
    into a list of :class:`GroupItem` objects.

    Parameters
    ----------
    parent : xml.etree.ElementTree.Element
        The containing XML element.

    Returns
    -------
    list of GroupItem
    """
    items: List[GroupItem] = []
    for child in parent:
        tag = child.tag.lower()
        if tag == 'scheme':
            name = (child.text or '').strip()
            if not name:
                raise CCPPError(
                    f"Empty <scheme> element inside <{parent.tag}>"
                )
            items.append(SuiteScheme(name))
        elif tag == 'subcycle':
            loop_val = child.get('loop')
            sub_items = _parse_group_items(child)
            items.append(SuiteSubcycle(loop=loop_val, items=sub_items))
        elif tag == 'subcol':
            gen = child.get('gen', '').strip()
            avg = child.get('avg', '').strip()
            if not gen or not avg:
                raise CCPPError(
                    "<subcol> requires both 'gen' and 'avg' attributes"
                )
            sub_items = _parse_group_items(child)
            items.append(SuiteSubcol(gen, avg, sub_items))
        # Anything else (whitespace text nodes, comments) is silently ignored
        # after nested_suite expansion (those elements are already gone).
    return items


def _build_suite(root: ET.Element, source_file: str,
                 version: List[int], logger: logging.Logger) -> Suite:
    """Build a :class:`Suite` from an *expanded* XML root element.

    This function must be called after :func:`expand_nested_suites` has
    already resolved all ``<nested_suite>`` references.

    Parameters
    ----------
    root : xml.etree.ElementTree.Element
        The ``<suite>`` root element (expanded).
    source_file : str
        Path to the original ``.xml`` file (for error messages).
    version : list of int
        Schema version ``[major, minor]``.
    logger : logging.Logger

    Returns
    -------
    Suite
    """
    suite_name = root.get('name', '').strip()
    if not suite_name:
        raise CCPPError(
            f"Suite XML '{source_file}' is missing the 'name' attribute "
            "on the <suite> element"
        )

    init_scheme: Optional[str]  = None
    final_scheme: Optional[str] = None
    groups: List[SuiteGroup]    = []

    for child in root:
        tag = child.tag.lower()

        if tag == _INIT_TAG:
            name = (child.text or '').strip()
            if not name:
                raise CCPPError(
                    f"SDF '{source_file}': empty <{child.tag}> element"
                )
            init_scheme = name

        elif tag in _REJECTED_INIT_TAGS:
            raise CCPPError(
                f"SDF '{source_file}': element <{child.tag}> is not "
                f"accepted; use the short form <{_INIT_TAG}> "
                f"(single scheme name as text content)."
            )

        elif tag == _FINAL_TAG:
            name = (child.text or '').strip()
            if not name:
                raise CCPPError(
                    f"SDF '{source_file}': empty <{child.tag}> element"
                )
            final_scheme = name

        elif tag in _REJECTED_FINAL_TAGS:
            raise CCPPError(
                f"SDF '{source_file}': element <{child.tag}> is not "
                f"accepted; use the short form <{_FINAL_TAG}> "
                f"(single scheme name as text content)."
            )

        elif tag == 'group':
            grp_name = child.get('name', '').strip()
            if not grp_name:
                raise CCPPError(
                    f"SDF '{source_file}': <group> is missing 'name' attribute"
                )
            items = _parse_group_items(child)
            groups.append(SuiteGroup(grp_name, items))

        elif tag == 'nested_suite':
            # Should not occur after expansion; warn and skip.
            logger.warning(
                "SDF '%s': unexpanded <nested_suite> element found after "
                "expansion — it will be ignored.", source_file
            )

        # else: whitespace / unknown elements — silently ignored

    if not groups:
        logger.warning(
            "SDF '%s': suite '%s' contains no <group> elements.",
            source_file, suite_name
        )

    # Check for duplicate group names (schema enforces xs:ID uniqueness but
    # validation may be skipped or xmllint may not be installed).
    seen_groups: Dict[str, bool] = {}
    for grp in groups:
        if grp.name in seen_groups:
            raise CCPPError(
                f"SDF '{source_file}': duplicate group name '{grp.name}' "
                f"in suite '{suite_name}'"
            )
        seen_groups[grp.name] = True

    return Suite(
        name=suite_name,
        version=version,
        source_file=source_file,
        groups=groups,
        init_scheme=init_scheme,
        final_scheme=final_scheme,
    )


########################################################################
# Public API
########################################################################

def parse_suite_xml(
    suite_file: str,
    output_root: str,
    logger: Optional[logging.Logger] = None,
    schema_path: Optional[str] = None,
    skip_validation: bool = False,
) -> Suite:
    """Parse a Suite Definition File, expand nested suites, and return a
    :class:`Suite` object.

    Processing steps:

    1. Read and XML-parse the file.
    2. Extract the schema version.
    3. Validate against the bundled XSD (unless *skip_validation* is set).
    4. For v2 suites: expand all ``<nested_suite>`` references.
    5. Re-validate the expanded XML.
    6. Write the expanded XML to
       ``<output_root>/ccpp_<suite_name>_expanded.xml``.
    7. Build and return the in-memory :class:`Suite` object.

    Parameters
    ----------
    suite_file : str
        Path to the ``.xml`` SDF.
    output_root : str
        Directory where the expanded XML is written.  Created if absent.
    logger : logging.Logger, optional
        Logger.  A module-level logger is used if ``None``.
    schema_path : str, optional
        Directory containing XSD files.  Defaults to the bundled
        ``capgen/schema/`` directory.
    skip_validation : bool
        If ``True``, skip XML schema validation (useful in test environments
        where ``xmllint`` is not available).

    Returns
    -------
    Suite
        Fully parsed suite with all nested suites expanded.

    Raises
    ------
    CCPPError
        On any structural, schema, or content error.

    Examples
    --------
    Parse a simple suite without writing to disk (using *skip_validation*
    and a temp directory)::

        suite = parse_suite_xml('my_suite.xml', '/tmp/capgen_out',
                                skip_validation=True)
        print(suite.name)
        print(suite.group_names())
    """
    log = logger or logging.getLogger(__name__)
    sdir = schema_path or _SCHEMA_DIR

    if not os.path.isfile(suite_file):
        raise CCPPError(f"Suite XML file '{suite_file}' does not exist")

    log.info("Reading suite XML: %s", suite_file)
    _, root = read_xml_file(suite_file, log)
    try:
        version = find_schema_version(root)
    except CCPPError as verr:
        raise CCPPError(
            f"{verr} in suite XML file '{suite_file}'"
        ) from verr
    log.debug("Suite XML schema version: %d.%d", *version)

    # ---- schema validation (pre-expansion) --------------------------------
    if not skip_validation:
        validate_xml_file(suite_file, 'suite', version, log, schema_path=sdir)

    # ---- expand nested suites (v2 only) -----------------------------------
    if version[0] >= 2:
        suite_dir = os.path.dirname(os.path.abspath(suite_file))
        expand_nested_suites(root, suite_dir, logger=log)

    # ---- build in-memory Suite object -------------------------------------
    suite = _build_suite(root, suite_file, version, log)

    # ---- write expanded XML to output_root --------------------------------
    os.makedirs(output_root, exist_ok=True)
    expanded_name = f"ccpp_{suite.name}_expanded.xml"
    expanded_path = os.path.join(output_root, expanded_name)
    # write_xml_file logs "Wrote <path>" or "Unchanged: <path>" via
    # write_if_changed when the logger is provided — no need to duplicate
    # here.
    write_xml_file(root, expanded_path, log)
    suite.expanded_file = expanded_path

    # ---- re-validate the expanded XML (catches duplicate xs:ID errors) ----
    if not skip_validation:
        validate_xml_file(expanded_path, 'suite', version, log, schema_path=sdir)

    return suite


def parse_suite_xml_files(
    suite_files: List[str],
    output_root: str,
    logger: Optional[logging.Logger] = None,
    schema_path: Optional[str] = None,
    skip_validation: bool = False,
) -> List[Suite]:
    """Parse a list of SDF files and return a :class:`Suite` per file.

    Wrapper around :func:`parse_suite_xml` for processing multiple suites
    in one call.

    Parameters
    ----------
    suite_files : list of str
        Paths to ``.xml`` SDF files.
    output_root : str
        Passed to :func:`parse_suite_xml`.
    logger : logging.Logger, optional
    schema_path : str, optional
    skip_validation : bool

    Returns
    -------
    list of Suite
    """
    suites = []
    for fpath in suite_files:
        suites.append(parse_suite_xml(
            fpath, output_root,
            logger=logger, schema_path=schema_path,
            skip_validation=skip_validation,
        ))
    return suites
