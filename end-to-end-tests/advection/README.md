# Advection Test

Contains tests to exercise the capabilities of the constituents object, including:
- Adding run-time constituents from the host via a register phase
- Adding run-time constituents from schemes via a register phase
  - Also tests that trying to add a constituent outside of the register phase errors as expected
- Passing around and modifying the constituent array
- Accessing and modifying a constituent tendency variable
- Passing around the constituent tendency array
- Dimensions are case-insensitive
