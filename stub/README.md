# How to build the stub

Note: build is in-source for now

1. Set compiler environment as appropriate for your system
2. Run the following commands:
```
cd stub
../scripts/ccpp_prebuild.py --config=ccpp_prebuild_config.py
cmake . 2>&1 | tee log.cmake
make 2>&1 | tee log.make
```
