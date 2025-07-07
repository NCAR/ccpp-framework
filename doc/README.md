# Building the documentation

Similar to building the source code, building the documentation can be done by:

```bash
$ cmake -S<project_root_source_directory> -B<build_directory> -DCCPP_FRAMEWORK_BUILD_DOCUMENTATION=ON
cd build_directory
make doc
```

and the compiled documentation should be in `<build_directory>/doc/html`.
