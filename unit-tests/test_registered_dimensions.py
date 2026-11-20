"""Tests for :mod:`metadata.registered_dimensions`.

This module is the single source of truth for capgen's registered
scalar-index dimension table.  Tests here cover:

  * Every entry in :data:`SCALAR_INDEX_DIMS` is present (regression
    against accidental removal during refactors).
  * Helper functions return the expected values for entries inside and
    outside the table.
  * Adding a new pairing follows a clear, documented recipe — the
    test_table_shape test exists so anyone extending the dict sees what
    they need to update.

When you add a new dim → index pair, add a corresponding test below
following the existing pattern.  See the module's top docstring for
the four-step extension recipe.
"""

import unittest

from metadata.registered_dimensions import (
    SCALAR_INDEX_DIMS,
    is_scalar_index_dim,
    registered_count_dims,
    registered_index_vars,
    scalar_index_for,
)


class TestSCalarIndexDimsContents(unittest.TestCase):
    """The registered table must contain at least the two pairings that
    capgen has committed to.  Removing or renaming either is a
    breaking change for hosts in production."""

    def test_number_of_instances_pair(self):
        self.assertEqual(
            SCALAR_INDEX_DIMS['number_of_instances'],
            'instance_number',
        )

    def test_number_of_threads_pair(self):
        self.assertEqual(
            SCALAR_INDEX_DIMS['number_of_threads'],
            'thread_number',
        )

    def test_no_dead_instance_dimension_entry(self):
        """The pre-2026-05-13 'instance_dimension' name was never used
        in any real metadata; it shipped only in the original design
        prompt.  It MUST stay out of the registered table."""
        self.assertNotIn('instance_dimension', SCALAR_INDEX_DIMS)


class TestHelpers(unittest.TestCase):

    def test_scalar_index_for_registered(self):
        self.assertEqual(scalar_index_for('number_of_instances'),
                         'instance_number')
        self.assertEqual(scalar_index_for('number_of_threads'),
                         'thread_number')

    def test_scalar_index_for_unregistered_returns_none(self):
        self.assertIsNone(scalar_index_for('horizontal_dimension'))
        self.assertIsNone(scalar_index_for('vertical_layer_dimension'))
        self.assertIsNone(scalar_index_for('number_of_ccpp_constituents'))

    def test_is_scalar_index_dim(self):
        self.assertTrue(is_scalar_index_dim('number_of_instances'))
        self.assertTrue(is_scalar_index_dim('number_of_threads'))
        self.assertFalse(is_scalar_index_dim('horizontal_dimension'))
        self.assertFalse(is_scalar_index_dim('instance_dimension'))

    def test_registered_count_dims_returns_keys(self):
        self.assertEqual(registered_count_dims(),
                         frozenset(SCALAR_INDEX_DIMS))

    def test_registered_index_vars_returns_values(self):
        self.assertEqual(registered_index_vars(),
                         frozenset(SCALAR_INDEX_DIMS.values()))


class TestExtensionRecipe(unittest.TestCase):
    """Anyone extending SCALAR_INDEX_DIMS should:
      1. Add an entry.
      2. Add a regression test in TestSCalarIndexDimsContents above.
      3. Update doc/migration.md §3 and doc/redesign_prompt.md §4.3.
      4. Add a unit test exercising the new pairing in the resolver.
    This single test exists so the table's shape is asserted in one
    place — if you add a key, this test fails and points you at the
    above checklist.
    """

    def test_table_size(self):
        # Update this assertion when you add a new pairing.  See the
        # docstring on this class for the full checklist.
        self.assertEqual(
            len(SCALAR_INDEX_DIMS), 2,
            "Registered scalar-index table grew without test update — "
            "see TestExtensionRecipe checklist",
        )


if __name__ == '__main__':
    unittest.main()
