"""Tests for generator.suite_types — pointer-wrapper types module emitter.

Focuses on the type-clause builder and DDT USE emission.  Doctests on
the helpers themselves cover the small cases; this file exercises the
end-to-end ``_generate_suite_types`` output structure.
"""

import unittest

from generator.suite_types import (
    _collect_ddt_uses,
    _fortran_type_str_simple,
    _generate_suite_types,
    _ptr_type_name,
)
from metadata.parse_tools import CCPPError


class TestFortranTypeStrSimple(unittest.TestCase):

    def test_intrinsic_with_kind(self):
        self.assertEqual(
            _fortran_type_str_simple('real', 'kind_phys'),
            'real(kind=kind_phys)',
        )

    def test_intrinsic_no_kind(self):
        self.assertEqual(_fortran_type_str_simple('integer', ''), 'integer')

    def test_character_len(self):
        self.assertEqual(
            _fortran_type_str_simple('character', 'len=512'),
            'character(len=512)',
        )

    def test_ddt_gets_type_wrapper(self):
        """The bug: a bare ``cmpfsw_type, pointer :: ptr(:)`` is invalid
        Fortran.  Must emit ``type(cmpfsw_type)``."""
        self.assertEqual(
            _fortran_type_str_simple('cmpfsw_type', ''),
            'type(cmpfsw_type)',
        )

    def test_external_gets_type_wrapper(self):
        self.assertEqual(
            _fortran_type_str_simple('external:mpi_f08:mpi_comm', ''),
            'type(mpi_comm)',
        )

    def test_ddt_ignores_kind(self):
        """Kind is meaningless on DDTs (no kind parameters) — silently
        dropped rather than emitted as ``type(foo)(kind=...)``."""
        self.assertEqual(
            _fortran_type_str_simple('my_ddt', 'kind_phys'),
            'type(my_ddt)',
        )


class TestPtrTypeName(unittest.TestCase):

    def test_intrinsic(self):
        self.assertEqual(
            _ptr_type_name('real', 'kind_phys', 1),
            'real_kind_phys_rank1_ptr_type',
        )

    def test_ddt_unchanged(self):
        self.assertEqual(
            _ptr_type_name('cmpfsw_type', '', 1),
            'cmpfsw_type_rank1_ptr_type',
        )

    def test_external_drops_module_prefix(self):
        """External types should produce a wrapper name keyed on the
        typename only — otherwise it would carry colons (illegal in a
        Fortran identifier)."""
        self.assertEqual(
            _ptr_type_name('external:mpi_f08:mpi_comm', '', 0),
            'mpi_comm_rank0_ptr_type',
        )

    def test_character_len_is_part_of_wrapper_name(self):
        """Two ``character`` arguments of different lengths must produce
        DIFFERENT wrapper types — Fortran disallows ``character(len=*)``
        as a DDT component, so the wrapper must bake in the literal
        length.  Regression for the SCM build failure where
        ``character(len=10)`` and ``character(len=3)`` both got the
        same ``character_rank1_ptr_type`` name and the compiler rejected
        the second declaration as a duplicate."""
        n10 = _ptr_type_name('character', 'len=10', 1)
        n3  = _ptr_type_name('character', 'len=3',  1)
        self.assertEqual(n10, 'character_len10_rank1_ptr_type')
        self.assertEqual(n3,  'character_len3_rank1_ptr_type')
        self.assertNotEqual(n10, n3)

    def test_character_len_deferred(self):
        """``character(len=:), allocatable`` / ``pointer`` (deferred-
        length string) is legal as a DDT component when paired with
        ``pointer``.  The wrapper name's len suffix must NOT contain
        ``:`` (illegal Fortran identifier char); emit ``_deferred``."""
        self.assertEqual(
            _ptr_type_name('character', 'len=:', 1),
            'character_len_deferred_rank1_ptr_type',
        )

    def test_character_len_parameter_symbol(self):
        """``len=MY_LEN`` (a Fortran parameter constant) is a valid
        identifier — keep it verbatim in the wrapper name."""
        self.assertEqual(
            _ptr_type_name('character', 'len=MY_LEN', 1),
            'character_lenMY_LEN_rank1_ptr_type',
        )

    def test_character_len_assumed_rejected(self):
        """``character(len=*)`` cannot appear as a DDT component, so
        capgen-ng cannot synthesise a wrapper.  Error must explain that
        and suggest using a concrete length or deferred-length."""
        with self.assertRaisesRegex(CCPPError, 'cannot appear as a DDT component'):
            _ptr_type_name('character', 'len=*', 1)

    def test_character_len_expression_rejected(self):
        """Length specs that aren't identifiers (e.g. ``N+1``) can't be
        encoded in a Fortran type name — error rather than emit an
        illegal identifier."""
        with self.assertRaisesRegex(CCPPError, 'Fortran-identifier-safe'):
            _ptr_type_name('character', 'len=N+1', 1)


class TestCollectDdtUses(unittest.TestCase):

    def test_intrinsics_skipped(self):
        combos = {('real', 'kind_phys', 1), ('integer', '', 0)}
        self.assertEqual(_collect_ddt_uses(combos, {}), {})

    def test_ddt_grouped_by_module(self):
        combos = {('cmpfsw_type', '', 1), ('cmpfsw_type', '', 2)}
        ddt_map = {'cmpfsw_type': 'module_radsw_parameters'}
        uses = _collect_ddt_uses(combos, ddt_map)
        self.assertEqual(uses, {'module_radsw_parameters': {'cmpfsw_type'}})

    def test_external_module_parsed_from_type(self):
        combos = {('external:mpi_f08:mpi_comm', '', 0)}
        uses = _collect_ddt_uses(combos, None)
        self.assertEqual(uses, {'mpi_f08': {'mpi_comm'}})

    def test_missing_ddt_in_map_raises(self):
        combos = {('some_ddt', '', 1)}
        with self.assertRaisesRegex(CCPPError, "no defining module"):
            _collect_ddt_uses(combos, {})


class TestGenerateSuiteTypesIncludesDdtUse(unittest.TestCase):
    """End-to-end at the source-line level: an optional DDT-typed
    argument's pointer wrapper must come with the matching ``use`` so
    ``type(<ddt>)`` resolves at compile time, AND the declaration must
    be wrapped (``type(<ddt>), pointer`` — not bare ``<ddt>, pointer``).
    Regression for the SCM build failure on
    ``ccpp_SCM_GFS_v17_p8_types.F90`` where ``cmpfsw_type, pointer ::
    ptr(:)`` was emitted bare.
    """

    def test_ddt_pointer_wrapper_well_formed(self):
        combos = {('cmpfsw_type', '', 1)}
        ddt_map = {'cmpfsw_type': 'module_radsw_parameters'}
        lines = _generate_suite_types('demo', combos, ddt_map)
        text = '\n'.join(lines)
        self.assertIn(
            'use module_radsw_parameters, only: cmpfsw_type', text,
            "DDT USE line missing — declarations will not compile",
        )
        self.assertIn(
            'type(cmpfsw_type), pointer :: ptr(:) => null()', text,
            "DDT pointer declaration missing type(...) wrapper",
        )
        self.assertNotIn(
            '\n    cmpfsw_type, pointer ::', text,
            "Bare DDT name in pointer declaration — invalid Fortran",
        )

    def test_intrinsic_only_emits_no_ddt_use(self):
        combos = {('real', 'kind_phys', 1)}
        lines = _generate_suite_types('demo', combos, {})
        text = '\n'.join(lines)
        self.assertIn('use ccpp_kinds, only: kind_phys', text)
        # Sanity: only one USE line total.
        self.assertEqual(text.count('  use '), 1)

    def test_distinct_character_lengths_get_distinct_wrappers(self):
        """End-to-end: two ``character`` ptr-wrappers of different
        lengths must emit two distinct ``type`` declarations.  The
        ``public ::`` list also carries both names (no duplicates).
        Regression for the SCM types-module duplicate-symbol bug."""
        combos = {
            ('character', 'len=10', 1),
            ('character', 'len=3',  1),
        }
        lines = _generate_suite_types('demo', combos, {})
        text = '\n'.join(lines)
        # Each length appears in exactly one ``type :: ... ptr_type`` block.
        self.assertIn(
            'type :: character_len10_rank1_ptr_type', text,
        )
        self.assertIn(
            'type :: character_len3_rank1_ptr_type', text,
        )
        # And the declarations themselves carry the right Fortran len=.
        self.assertIn(
            'character(len=10), pointer :: ptr(:) => null()', text,
        )
        self.assertIn(
            'character(len=3), pointer :: ptr(:) => null()', text,
        )
        # No duplicate symbol — a name appears exactly twice (``public ::``
        # line and the ``type :: NAME`` opener; ``end type NAME`` does
        # not count since ``end`` precedes it).
        for name in ('character_len10_rank1_ptr_type',
                     'character_len3_rank1_ptr_type'):
            self.assertEqual(
                text.count('public :: {}'.format(name)), 1,
                'expected one public:: declaration for {}, got: {!r}'.format(
                    name,
                    [l for l in lines if name in l],
                ),
            )


if __name__ == '__main__':
    unittest.main()
