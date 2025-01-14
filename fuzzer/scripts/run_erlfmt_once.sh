#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
#
# This source code is licensed under the Apache 2.0 license found in
# the LICENSE file in the root directory of this source tree.

# NOT -e, since detecting crashes and treating them correctly is the whole point of this program
set -uo pipefail

main() {
  max_memory_in_kb=2000000
  timeout_in_s=30

  ulimit -m ${max_memory_in_kb}

  filename="$1"
  directory="${filename%/*}" # remove the shortest suffix that matches the pattern /*
  # "-k 1" ensures that we kill erlc if it somehow keeps going 1s after it received the timeout signal
  timeout -k 1 ${timeout_in_s} erlfmt -o "${directory}" "${filename}"
  erlfmt_result=$?
  if [[ ${erlfmt_result} == 0 ]]; then
    echo "File ${filename}: completed normally"
    exit 0
  else
    echo "INTERESTING: erlfmt either crashed or timed out on ${filename}!"
    exit ${erlfmt_result}
  fi
}

main "$1"
