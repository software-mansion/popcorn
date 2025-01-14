#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
#
# This source code is licensed under the Apache 2.0 license found in
# the LICENSE file in the root directory of this source tree.

# NOT -e, since detecting crashes and treating them correctly is the whole point of this program
set -uo pipefail

main() {
  max_memory_in_kb=2000000
  timeout_in_s=10

  ulimit -m ${max_memory_in_kb}

  filename="$1"
  directory="${filename%/*}" # remove the shortest suffix that matches the pattern /*
  bare_filename="${filename##*/}" # remove the longest prefix that matches the pattern */
  module_name="${bare_filename%.*}" # remove the shortest suffix that matches the pattern .*

  # eqwalizer wants a very specific directory layout, with build_info.json at the top level, then /app_name/src/app_name.erl
  mkdir "${directory}/${module_name}/"
  mkdir "${directory}/${module_name}/src/"
  cp "$1" "${directory}/${module_name}/src/"

  json_filename="${directory}/build_info_${module_name}.json"
  {
    echo '{ "apps": [{'
    echo "\"name\": \"${module_name}\","
    echo "\"dir\": \"${module_name}\""
    echo "}]}"
  } >| "${json_filename}"

  # "-k 1" ensures that we kill eqwalizer if it somehow keeps going 1s after it received the timeout signal
  timeout -k 1 ${timeout_in_s} elp eqwalize-all --project "${json_filename}"
  eqwalizer_result=$?
  exit_code=0
  if [[ ${eqwalizer_result} == 0 ]]; then
    echo "File ${filename}: completed normally"
  elif [[ ${eqwalizer_result} == 124 ]] || [[ ${eqwalizer_result} == 137 ]]; then
    # 124 is the error code returned by timeout if it soft-kills its target
    # 137 is the error code returned by timeout if it hard-kills its target after the soft-kill was ignored
    echo "File ${filename}: timeout"
  else
    echo "INTERESTING: eqwalizer crashed on ${filename}!"
    exit_code=${eqwalizer_result}
  fi

  rm "${json_filename}"
  rm -rf "${directory:?}/${module_name:?}/"

  exit ${exit_code}
}

main "$1"
