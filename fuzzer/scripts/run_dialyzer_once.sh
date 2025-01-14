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
  output_filename="${filename%.*}.output"
  # "-k 1" ensures that we kill erlc if it somehow keeps going 1s after it received the timeout signal
  # "-n" to avoid wasting time rechecking the PLT
  # "-q" for quiet
  timeout -k 1 ${timeout_in_s} dialyzer -n -q -o "${output_filename}" --src "${filename}"
  dialyzer_result=$?
  rm "${output_filename}"
  if [[ ${dialyzer_result} == 0 ]]; then
    echo "File ${filename}: completed normally"
    exit 0
  elif [[ ${dialyzer_result} == 2 ]]; then
    echo "File ${filename}: warnings, but no crash"
    exit 0
  elif [[ ${dialyzer_result} == 124 ]] || [[ ${dialyzer_result} == 137 ]]; then
    echo "dialyzer timed out on ${filename}"
    exit 0
  else
    echo "(error code: ${dialyzer_result}) INTERESTING: dialyzer crashed on ${filename}!"
    exit ${dialyzer_result}
  fi
}

main "$1"
