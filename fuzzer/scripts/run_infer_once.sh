#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
#
# This source code is licensed under the Apache 2.0 license found in
# the LICENSE file in the root directory of this source tree.

# NOT -e, since detecting crashes and treating them correctly is the whole point of this program
set -uo pipefail

main() {
  max_memory_in_kb=2000000
  infer_timeout_in_s=30

  ulimit -m ${max_memory_in_kb}

  filename="$1"
  directory="${filename%/*}" # remove the shortest suffix that matches the pattern /*
  bare_filename="${filename##*/}" # remove the longest prefix that matches the pattern */
  module_name="${bare_filename%.*}" # remove the shortest suffix that matches the pattern .*
  beam_filename="${filename%.*}.beam" # remove the shortest suffix that matches the pattern .*, then add '.beam'

  # It is important that each instance of infer stores its result in its own directory
  # Otherwise, trying to run this on several cores in parallel results in a corrupted results directory,
  #   which infer detects, and it refuses to run anymore.
  results_dir="${directory}/${module_name}-infer-out/"
  mkdir "${results_dir}"
  timeout -k 1 "${infer_timeout_in_s}" infer --pulse-max-cfg-size 1000000000 --pulse-only --results-dir "${results_dir}" -- erlc -o "${directory}" "${filename}"
  infer_result=$?
  rm -rf "${results_dir}"
  rm "${beam_filename}"
  if [[ ${infer_result} == 0 ]]; then
    echo "File ${filename}: completed normally"
    exit 0
  elif [[ ${infer_result} == 124 ]] || [[ ${infer_result} == 137 ]]; then
    echo "infer timed out on ${filename}"
    exit 0
  else
    echo "(error code: ${infer_result}) INTERESTING: infer crashed on ${filename}!"
    exit ${infer_result}
  fi
}

main "$1"
