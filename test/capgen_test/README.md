# Capgen Test

Contains tests for overall capgen capabilities such as:
- Multiple suites
- Multiple groups
- General DDT usage
- DDT with undocumented DDT member variable
- Dimensions with `ccpp_constant_one:N` and just `N`
- Non-standard dimensions (not just horizontal and vertical) (including integer dimensions)
- Variables that should be promoted to suite level
- Dimensions that are set in the register phase and used to allocate module-level
  interstitial variables

## Building/Running

To explicitly build/run the capgen test host, run:

```bash
$ cmake --fresh -S<path_to_project_root> -B<path_to_build> -DCCPP_RUN_CAPGEN_TEST=ON
$ cd <path_to_build>
$ make
$ ctest
```
