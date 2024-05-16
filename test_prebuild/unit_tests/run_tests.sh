#!/usr/bin/env bash

THIS_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
FRAMEWORK_DIR="${THIS_DIR}/../.."

export PYTHONPATH="${FRAMEWORK_DIR}/scripts/parse_tools:${FRAMEWORK_DIR}/scripts:${PYTHONPATH}"

set -ex
python3 ./test_metadata_parser.py
python3 ./test_mkstatic.py
