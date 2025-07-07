# Capgen Test

## Building/Running

To explicitly build/run the capgen test host, run:

```bash
$ cmake -S<path_to_project_root> -B<path_to_build> -DCCPP_RUN_CAPGEN_TEST=ON
$ cd <path_to_build>
$ make
$ ctest
```
