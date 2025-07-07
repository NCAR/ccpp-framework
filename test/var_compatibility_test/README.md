# Variable Compatibility Test

## Building/Running

To explicitly build/run the variable compatibility test host, run:

```bash
$ cmake -S<path_to_project_root> -B<path_to_build> -DCCPP_RUN_VAR_COMPATIBILITY_TEST=ON
$ cd <path_to_build>
$ make
$ ctest
```
