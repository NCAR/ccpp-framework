#!/usr/bin/env bash

files=(
    "src/ccpp_constituent_prop_mod.F90"
    "src/ccpp_hashable.F90"
    "src/ccpp_hash_table.F90"
    "src/ccpp_scheme_utils.F90"
    "src/ccpp_types.F90"
)

for entry  in "${files[@]}"; do
  file=${entry}
  git checkout origin/develop -- $file
  codee format --verbose --on-error force $file
  echo ""
  echo "-------------------------------------------------"
done
