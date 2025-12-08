#!/usr/bin/env bash

files=(
    "src/ccpp_constituent_prop_mod.F90:free"
    "src/ccpp_hashable.F90:free"
    "src/ccpp_hash_table.F90:free"
    "src/ccpp_scheme_utils.F90:free"
    "src/ccpp_types.F90:free"
)

for entry  in "${files[@]}"; do
  file="${entry%%:*}"
  ext="${file##*.}"
  fmt="${entry##*:}"
  git checkout origin/develop -- $file
  codee format --verbose --extensions=$ext --on-error force $file
  echo ""
  echo "-------------------------------------------------"
done
