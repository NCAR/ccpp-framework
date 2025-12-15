# Nested Suite Test

Tests the capability to process nested suites:
- Inherited from the variable compatibility test as of 2025/10/01
  - Perform same tests as variable compatibility test at that date
- Parse new XML schema 2.0
- Expand nested suites at the group level and inside groups

## Building/Running

To explicitly build/run the nested suite test host, run:

```bash
$ cmake --fresh -S<path_to_project_root> -B<path_to_build> -DCCPP_RUN_NESTED_SUITE_TEST=ON
$ cd <path_to_build>
$ make
$ ctest
```
