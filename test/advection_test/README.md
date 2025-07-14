# Advection Test

Contains tests to exercise the capabilities of the constituents object, including:
- Adding a build-time constituent via metadata property
- Adding a run-time constituent via a register phase
  - Also tests that trying to add a constituent outside of the register phase errors as expected
- Passing around and modifying the constituent array
- Accessing and modifying a constituent tendency variable
- Passing around the constituent tendency array
- Dimensions are case-insensitive

## Building/Running

To explicitly build/run the advection test host, run:

```bash
$ cmake -S<path_to_project_root> -B<path_to_build> -DCCPP_RUN_ADVECTION_TEST=ON
$ cd <path_to_build>
$ make
$ ctest
```
