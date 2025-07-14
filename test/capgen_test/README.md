# Capgen Test

Contains tests for overall capgen capabilities such as:
- Multiple suites
- Multiple groups
- General DDT usage
- Dimensions with `ccpp_constant_one:N` and just `N`
- Non-standard dimensions (not just horizontal and vertical) (including integer dimensions)
- Variables that should be promoted to suite level

## Building/Running

To explicitly build/run the capgen test host, run:

```bash
$ cmake -S<path_to_project_root> -B<path_to_build> -DCCPP_RUN_CAPGEN_TEST=ON
$ cd <path_to_build>
$ make
$ ctest
```
