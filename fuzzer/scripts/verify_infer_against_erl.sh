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
  erl_timeout_in_s=5

  ulimit -m ${max_memory_in_kb}

  filename="$1"
  directory="${filename%/*}" # remove the shortest suffix that matches the pattern /*
  bare_filename="${filename##*/}" # remove the longest prefix that matches the pattern */
  module_name="${bare_filename%.*}" # remove the shortest suffix that matches the pattern .*
  beam_filename="${filename%.*}.beam" # remove the shortest suffix that matches the pattern .*, then add '.beam'
  report_filename="${filename%.*}_report.txt"
  grep_results="${filename%.*}_grep.txt"

  # It is important that each instance of infer stores its result in its own directory
  # Otherwise, trying to run this on several cores in parallel results in a corrupted results directory,
  #   which infer detects, and it refuses to run anymore.
  results_dir="${directory}/${module_name}-infer-out/"
  mkdir -p "${results_dir}"
  timeout -k 1 "${infer_timeout_in_s}" infer --pulse-only --results-dir "${results_dir}" -- erlc -o "${directory}" "${filename}" 1> /dev/null 2> /dev/null
  infer_result=$?
  if [[ ${infer_result} == 124 ]] || [[ ${infer_result} == 137 ]]; then
    echo "infer timed out on ${filename}"
    rm -rf "${results_dir}"
    rm "${beam_filename}"
    exit 0
  elif [[ ${infer_result} != 0 ]]; then
    echo "(error code: ${infer_result}) INTERESTING: infer crashed on ${filename}!"
    rm -rf "${results_dir}"
    rm "${beam_filename}"
    exit ${infer_result}
  fi

  # infer ran correctly, so let's now try to run the testcases to have an oracle to compare against
  infer report --results-dir "${results_dir}" --issues-tests "${report_filename}"
  rm -rf "${results_dir}"
  # At this stage $report_filename contains one line per bug detected by infer, and they contain the name of the wrapper that leads to that bug.

  exit_code=0
  # All of this is just iterating over all functions named wrapperXY() present in the erlang file
  grep '^wrapper' "${filename}" | sed 's/\(wrapper[0-9]*\).*/\1/' > "${grep_results}"
  while IFS= read -r wrapper_name <&3
  do
    {
      timeout -k 1 "${erl_timeout_in_s}" erl -noshell -pa "${directory}" -s "${module_name}" "${wrapper_name}" -s init stop 1> /dev/null 2> /dev/null
      erl_result=$?
      if [[ ${erl_result} == 124 ]] || [[ ${erl_result} == 137 ]]; then
        # timeout
        continue
      fi
      grep -F -q "${wrapper_name}/0" "${report_filename}"
      grep_result=$? # 0 means a match means that infer found a bug. any other code means no bug found.
      if [[ ${erl_result} == 0 ]] && [[ ${grep_result} == 0 ]]; then
        echo "INTERESTING: infer reported a bug, but the code ran fine, function name: ${module_name}:${wrapper_name}"
        exit_code=42
        break
      elif [[ ${erl_result} != 0 ]] && [[ ${grep_result} != 0 ]]; then
        echo "INTERESTING: infer said that the code was fine, but it crashed, function name: ${module_name}:${wrapper_name}"
        exit_code=43
        break
      fi
    } 3<&- # erl tends to mess with stdin, which interrupts the loop early if not for this voodoo from stack overflow
  done 3< "${grep_results}"

  rm "${beam_filename}"
  rm "${report_filename}"
  rm "${grep_results}"
  exit "${exit_code}"
}

main "$1"
