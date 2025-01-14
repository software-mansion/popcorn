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
  if [[ ${erlc_result} == 0 ]]; then
    bare_filename="${filename##*/}" # remove the longest prefix that matches the pattern */
    module_name="${bare_filename%.*}" # remove the shortest suffix that matches the pattern .*
    timeout -k 1 ${timeout_in_s} cerl -asan -noshell -pa "${directory}" -s "${module_name}" start -s init stop 1> /dev/null
    erl_result=$?
    if [[ ${erl_result} == 0 ]]; then
      echo "File ${beam_filename}: completed normally"
      rm "${beam_filename}"
      exit 0
    elif [[ ${erl_result} == 124 ]] || [[ ${erl_result} == 137 ]]; then
      # 124 is the error code returned by timeout if it soft-kills its target
      # 137 is the error code returned by timeout if it hard-kills its target after the soft-kill was ignored
      # It is expected that fuzzer-generated code will often lead the VM to timeout, so we don't consider this interesting
      echo "File ${beam_filename}: timeout"
      rm "${beam_filename}"
      exit 0
    else
      echo "INTERESTING: erl crashed on ${beam_filename}, produced from: ${filename}!"
      rm "${beam_filename}"
      exit ${erl_result}
    fi
  else
    # Contrary to erl, erlc is not supposed to ever hang forever on valid Erlang input
    echo "INTERESTING: erlc either crashed or timed out on ${filename}!"
    exit 42
  fi
}

main "$1"
