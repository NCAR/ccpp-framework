# DDT Host Test

Contains tests to exercise more DDT functionality:
- Passing around and modifying a DDT
- Making DDT in host model & using it in CCPP-ized physics code

## Building/Running

To explicitly build/run the ddt test host, run:

```bash
$ cmake -S<path_to_project_root> -B<path_to_build> -DCCPP_RUN_DDT_HOST_TEST=ON
$ cd <path_to_build>
$ make
$ ctest
```
