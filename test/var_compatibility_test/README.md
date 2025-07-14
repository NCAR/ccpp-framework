# Variable Compatibility Test

Tests the variable compatibility object (`VarCompatObj`):
- Unit conversions (forward & reverse)
- Vertical array flipping (`top_at_one=true`)
- Kind conversions (`kind_phys <-> 8`)
- And various combinations thereof of the above cases

## Building/Running

To explicitly build/run the variable compatibility test host, run:

```bash
$ cmake -S<path_to_project_root> -B<path_to_build> -DCCPP_RUN_VAR_COMPATIBILITY_TEST=ON
$ cd <path_to_build>
$ make
$ ctest
```
