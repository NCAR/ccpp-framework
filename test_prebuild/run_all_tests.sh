#!/usr/bin/env bash

set -e

echo "" && echo "Running unit_tests       " && cd unit_tests        && ./run_tests.sh && cd ..
echo "" && echo "Running test_opt_arg     " && cd test_opt_arg      && ./run_test.sh  && cd ..
echo "" && echo "Running test_blocked_data" && cd test_blocked_data && ./run_test.sh  && cd ..
echo "" && echo "Running test_chunked_data" && cd test_chunked_data && ./run_test.sh  && cd ..
echo "" && echo "Running test_unit_conv"    && cd test_unit_conv    && ./run_test.sh  && cd ..

echo "" && echo "Running test_track_variables" && pytest test_track_variables.py