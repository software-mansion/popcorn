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
    # Contrary to erl, erlc is not supposed to ever hang forever on valid Erlang input
    echo "INTERESTING: erlc either crashed or timed out on ${filename}!"
    exit 42
  fi

  bare_filename="${filename##*/}" # remove the longest prefix that matches the pattern */
  module_name="${bare_filename%.*}" # remove the shortest suffix that matches the pattern .*
  jit_output_filename="${filename%.*}.jit_output"
  emu_output_filename="${filename%.*}.emu_output"

  # First run with the jit disabled
  echo "timeout -k 1 ${timeout_in_s} cerl -emu_flavor emu -noshell -pa ${directory} -s ${module_name} wrapper0 -s init stop > ${emu_output_filename}"
  timeout -k 1 "${timeout_in_s}" cerl -emu_flavor emu -noshell -pa "${directory}" -s "${module_name}" wrapper0 -s init stop > "${emu_output_filename}"
  emu_result=$?
  if [[ ${emu_result} == 124 ]] || [[ ${emu_result} == 137 ]]; then
    echo "cerl -emu_flavor emu timed out on ${module_name}:wrapper0/0"
    rm "${emu_output_filename}"
    rm "${beam_filename}"
    exit 0
  elif [[ ${emu_result} != 0 ]]; then
    echo "INTERESTING: cerl -emu_flavor emu crashed on ${beam_filename}, produced from: ${filename}!"
    rm "${emu_output_filename}"
    rm "${beam_filename}"
    exit "${emu_result}"
  fi
  # Then with the jit enabled
  timeout -k 1 "${timeout_in_s}" cerl -emu_flavor jit -noshell -pa "${directory}" -s "${module_name}" wrapper0 -s init stop > "${jit_output_filename}"
  jit_result=$?
  if [[ ${jit_result} == 124 ]] || [[ ${jit_result} == 137 ]]; then
    echo "cerl -emu_flavor jit timed out on ${module_name}:wrapper0/0"
    rm "${emu_output_filename}"
    rm "${jit_output_filename}"
    rm "${beam_filename}"
    exit 0
  elif [[ ${jit_result} != 0 ]]; then
    echo "INTERESTING: cerl -emu_flavor jit crashed on ${beam_filename}, produced from: ${filename}!:"
    rm "${emu_output_filename}"
    rm "${jit_output_filename}"
    rm "${beam_filename}"
    exit "${jit_result}"
  fi
  # And finally compare the results
  # We must first remove the effectively random numbers that are attached to Funs.
  sed --separate --in-place -e "s/#Fun<[a-zA-Z0-9_.]*>/Fun/g" "${emu_output_filename}" "${jit_output_filename}"
  if ! cmp -s "${emu_output_filename}" "${jit_output_filename}"; then
    echo "INTERESTING: divergence between jit and emu flavors on ${beam_filename}, produced from ${filename}!"
    diff "${emu_output_filename}" "${jit_output_filename}"
    rm "${emu_output_filename}"
    rm "${jit_output_filename}"
    rm "${beam_filename}"
    exit 43
  fi
  echo "Same output for jit and emu on ${beam_filename}"
  rm "${beam_filename}"
  rm "${emu_output_filename}"
  rm "${jit_output_filename}"
}

main "$1"
