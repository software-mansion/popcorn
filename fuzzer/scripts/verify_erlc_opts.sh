#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
#
# This source code is licensed under the Apache 2.0 license found in
# the LICENSE file in the root directory of this source tree.

# NOT -e, since detecting crashes and treating them correctly is the whole point of this program
set -uo pipefail

main() {
  max_memory_in_kb=2000000
  timeout_in_s=3
  erlc_timeout_in_s=30

  ulimit -m ${max_memory_in_kb}

  filename="$1"
  directory="${filename%/*}" # remove the shortest suffix that matches the pattern /*
  beam_filename="${filename%.*}.beam" # remove the shortest suffix that matches the pattern .*, then add '.beam'
  # "-k 1" ensures that we kill erlc if it somehow keeps going 1s after it received the timeout signal
  timeout -k 1 ${erlc_timeout_in_s} erlc -W0 -o "${directory}" "${filename}"
  erlc_result=$?
  if [[ ${erlc_result} != 0 ]]; then
    echo "INTERESTING: erlc (with optimizations) either crashed or timed out on ${filename}!"
    exit 1
  fi

  bare_filename="${filename##*/}" # remove the longest prefix that matches the pattern */
  module_name="${bare_filename%.*}" # remove the shortest suffix that matches the pattern .*
  opt_output_filename="${filename%.*}.opt_output"
  basic_output_filename="${filename%.*}.basic_output"

  timeout -k 1 "${timeout_in_s}" erl -noshell -pa "${directory}" -s "${module_name}" wrapper0 -s init stop > "${opt_output_filename}"
  erl_result=$?
  rm "${beam_filename}"
  if [[ ${erl_result} == 124 ]] || [[ ${erl_result} == 137 ]]; then
    echo "Discarded: erl timed out on (optimized) ${module_name}:wrapper0/0"
    rm "${opt_output_filename}"
    exit 0
  elif [[ ${erl_result} != 0 ]]; then
    echo "INTERESTING: erl crashed on (optimized) ${beam_filename}!"
    rm "${opt_output_filename}"
    exit 2
  fi

  timeout -k 1 ${erlc_timeout_in_s} erlc +no_copt +no_fold +no_alias +no_bool_opt +no_share_opt +no_recv_opt +no_bsm_opt +no_ssa_opt +no_throw_opt +no_postopt +no_stack_trimming -W0 -o "${directory}" "${filename}"
  erlc_result=$?
  if [[ ${erlc_result} != 0 ]]; then
    echo "INTERESTING: erlc (without optimizations) either crashed or timed out on ${filename}!"
    exit 3
  fi

  timeout -k 1 "${timeout_in_s}" erl -noshell -pa "${directory}" -s "${module_name}" wrapper0 -s init stop > "${basic_output_filename}"
  erl_result=$?
  rm "${beam_filename}"
  if [[ ${erl_result} == 124 ]] || [[ ${erl_result} == 137 ]]; then
    echo "Discarded: erl timed out on (unoptimized) ${module_name}:wrapper0/0"
    rm "${opt_output_filename}"
    rm "${basic_output_filename}"
    exit 0
  elif [[ ${erl_result} != 0 ]]; then
    echo "INTERESTING: erl crashed on (unoptimized) ${beam_filename}!"
    rm "${opt_output_filename}"
    rm "${basic_output_filename}"
    exit 4
  fi

  # And finally compare the results
  # We must first remove the effectively random numbers that are attached to Funs.
  sed --separate --in-place -e "s/#Fun<[a-zA-Z0-9_.]*>/Fun/g" "${opt_output_filename}" "${basic_output_filename}"
  if ! cmp -s "${opt_output_filename}" "${basic_output_filename}"; then
    echo "INTERESTING: divergence between optimized and non-optimized output on ${filename}!"
    diff "${opt_output_filename}" "${basic_output_filename}"
    rm "${opt_output_filename}"
    rm "${basic_output_filename}"
    exit 5
  fi
  echo "Same output for optimized and non-optimized code on ${filename}"
  rm "${opt_output_filename}"
  rm "${basic_output_filename}"
  exit 0
}

main "$1"
