from mkstatic import extract_parents_and_indices_from_local_name

import pytest


@pytest.mark.parametrize(
    "input_,expected",
    [
        (
            r"Atm(mytile)%q(:,:,:,Atm2(mytile2)%graupel)",
            ("Atm", ["Atm2", "mytile", "mytile2"]),
        ),
        (r"Atm(mytile)%q(:,:,:,simple_ind)", ("Atm", ["mytile", "simple_ind"])),
        (r"Atm%q(random)", ("Atm", ["random"])),
    ],
)
def test_extract_parents_and_indices_from_local_name(input_, expected):
    expected_parent, expected_inputs = expected
    parent, inputs = extract_parents_and_indices_from_local_name(input_)

    assert parent == expected_parent
    assert set(inputs) == set(expected_inputs)
