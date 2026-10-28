#!/usr/bin/env python3

"""Registered scalar-index dimensions for capgen.

This module is the **single source of truth** for the small set of CCPP
standard-name dimensions that capgen treats specially: each one is a
*count* (e.g. ``number_of_instances``, ``number_of_threads``) whose access
pattern collapses to a paired scalar *index* variable (e.g.
``instance_number``, ``thread_number``).

Where this is used
------------------

A container DDT-instance variable in host metadata may carry one of these
dimensions, e.g.::

    [Interstitial]
      standard_name = GFS_interstitial_type_instance
      type          = GFS_interstitial_type
      dimensions    = (number_of_threads)        # <-- registered scalar dim

When a scheme reaches into a field of that container, capgen emits the
scalar index automatically::

    physics%Interstitial(thread_number)%alpha(lb:ub, 1:nlev)
                       ^^^^^^^^^^^^^^^
                       substituted from the registered scalar-index pair

The same machinery applies anywhere capgen needs to subscript a
container by an index variable that the host carries as a separate
control/host variable.

The two rules
-------------

**Rule 1 (generalized, NOT enforced as a hard gate)**:
A container DDT-instance variable may carry registered scalar-index
dimensions in its ``dimensions`` clause.  When it does, capgen emits
the paired index variable's local Fortran name at every call site that
reaches into the container.  Anything *not* in :data:`SCALAR_INDEX_DIMS`
flows through the normal slice/bounds machinery
(``horizontal_loop_begin:horizontal_loop_end``, ``1:vertical_*``, …)
exactly like flat-array dims.

**Rule 2 (ENFORCED at host-dict-build time)**:
A **leaf** variable (intrinsic-typed or ``external:`` Fortran type — the
kind a physics scheme actually binds to) MUST NOT declare a registered
scalar-index dim.  Leaves see only spatial / tracer / count dims; scalar
indexing is a container-DDT concept.  Violations raise a ``CCPPError``
at parse time that names the offending variable, the offending dim, the
expected index variable, and points the user back to this file.

Why both rules matter
---------------------

* Rule 1 generalization lets capgen support multi-instance
  (``number_of_instances``) **and** per-thread (``number_of_threads``)
  container DDTs from one mechanism — no per-dimension code path.
* Rule 2 keeps the substitution mechanism contained: the rule "leaves
  never carry registered scalar dims" means the scheme-call-site dim
  handler (``_one_dim_part`` in ``generator/suite_resolver.py``) never
  has to special-case scalar-index dims.  One code path for leaves, one
  code path for containers.

How to extend the table
-----------------------

Adding a new pairing (e.g. a new ``number_of_blocks`` ↔ ``block_number``
convention) is a four-step process:

1. **Add an entry below** mapping the *count* standard name to the
   *index* standard name.  Both are CCPP standard names the host must
   declare.
2. **Verify the host metadata declares both**: a ``[ccpp-table-properties]``
   block with ``type = control`` (or ``type = host``) carrying a scalar
   integer with the index standard name, plus a similar declaration for
   the count.
3. **Add a unit test** to ``unit-tests/test_registered_dimensions.py``
   exercising the new pairing.
4. **Update** ``doc/migration.md`` §3 "Registered scalar-index
   dimensions" and ``doc/redesign_prompt.md`` §4.3.

The contract for users (host model authors)
-------------------------------------------

If you see the error message::

    Variable '<name>' (standard_name='<std>') declares dimension '<dim>'
    on a leaf-data variable, but '<dim>' is a registered scalar-index
    dimension reserved for DDT-instance containers (see
    capgen/metadata/registered_dimensions.py).
    [...]

it means you wrote something like::

    [my_array]
      standard_name = some_leaf_quantity
      type          = real | kind = kind_phys
      dimensions    = (number_of_threads, horizontal_dimension)

The fix is to **wrap** the leaf in a per-thread container DDT, e.g.::

    [Interstitial]
      type          = my_interstitial_type
      dimensions    = (number_of_threads)

with ``my_interstitial_type`` declaring the leaf with only its spatial
dims::

    [some_leaf_quantity]
      type       = real | kind = kind_phys
      dimensions = (horizontal_dimension)

This mirrors how every CCPP host in production today (UFS, NEPTUNE,
CAM-SIMA, ccpp-scm) structures per-thread / per-instance state.
"""

from typing import Dict, FrozenSet, Optional


########################################################################
# The registered scalar-index dimension table
########################################################################

#: Map from *count* dimension standard name → paired *index* variable
#: standard name.  The host metadata MUST declare the index variable as
#: a scalar integer (in a ``type = control`` table when it is a
#: framework-lifecycle variable, or in a ``type = host`` table
#: otherwise).
#:
#: Every entry here is treated as a hard convention across the entire
#: CCPP ecosystem.  Adding an entry binds capgen to a specific
#: standard-name pairing; once an entry lands and hosts adopt it,
#: removing or renaming it is a breaking change.
SCALAR_INDEX_DIMS: Dict[str, str] = {
    # Multi-instance API: the framework's instance_number paired opt-in.
    # Hosts that declare instance_number + number_of_instances opt into
    # the multi-instance API; capgen auto-substitutes (instance_number)
    # wherever a container DDT carries this dimension.
    'number_of_instances': 'instance_number',

    # Per-thread DDT containers (e.g. ``physics%Interstitial(thread_number)``)
    # — the host's openmp-thread index.  (thread_number, number_of_threads) is
    # a paired-optional control pair (see ccpp_capgen._PAIRED_OPTIONAL_CTRL_VARS
    # and doc/migration.md §3.1).  A host that dimensions a variable by
    # ``number_of_threads`` MUST declare the pair — otherwise the collapse
    # below cannot find ``thread_number`` and raises.
    'number_of_threads': 'thread_number',
}


########################################################################
# Public helpers
########################################################################

def scalar_index_for(dim_std_name: str) -> Optional[str]:
    """Return the paired scalar-index std name for *dim_std_name*, or None.

    >>> scalar_index_for('number_of_instances')
    'instance_number'
    >>> scalar_index_for('number_of_threads')
    'thread_number'
    >>> scalar_index_for('horizontal_dimension') is None
    True
    """
    return SCALAR_INDEX_DIMS.get(dim_std_name)


def is_scalar_index_dim(dim_std_name: str) -> bool:
    """Return True iff *dim_std_name* is a registered scalar-index dimension.

    >>> is_scalar_index_dim('number_of_instances')
    True
    >>> is_scalar_index_dim('horizontal_dimension')
    False
    """
    return dim_std_name in SCALAR_INDEX_DIMS


def registered_count_dims() -> FrozenSet[str]:
    """Return the set of all registered count-side dimension std names.

    Equivalent to ``frozenset(SCALAR_INDEX_DIMS)``; provided as a helper
    so consumers don't have to reach into the dict.
    """
    return frozenset(SCALAR_INDEX_DIMS)


def registered_index_vars() -> FrozenSet[str]:
    """Return the set of all registered scalar-index variable std names.

    Equivalent to ``frozenset(SCALAR_INDEX_DIMS.values())``.
    """
    return frozenset(SCALAR_INDEX_DIMS.values())
