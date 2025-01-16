#!/bin/bash
set -uo pipefail
main() {
  atomvm_timeout_in_s=30
  atomvm_path=""
  packbeam_path=""
  avm_lib_path="../fission_lib/_build/dev/fission_lib.avm" 

  if [ "$#" -eq 0 ]; then
    echo "Usage: $0 <path_to_erlang_file>"
    exit 1
  fi
  filename="$1"

  if ! [[ -f "$filename" ]]; then
    echo "Error: File '$filename' does not exist."
    exit 1
  fi

  directory="${filename%/*}"
  beam_filename="${filename%.*}.beam"
  avm_filename="${filename%.*}.avm"

  timeout -k 1 ${atomvm_timeout_in_s} erlc -W0 -o "${directory}" "${filename}"
  erlc_result=$?
  if [[ ${erlc_result} == 0 ]]; then
    timeout -k 1 ${atomvm_timeout_in_s} "${packbeam_path}" -i "${avm_filename}" "${beam_filename}" "${avm_lib_path}"
    timeout -k 1 ${atomvm_timeout_in_s} "${atomvm_path}" "${avm_filename}" 1> /dev/null
    erl_result=$?   
    if [[ ${erl_result} == 1 ]]; then
      echo "File ${beam_filename}: completed normally"
      rm "${beam_filename}"
      rm "${avm_filename}"
      exit 0
    elif [[ ${erl_result} == 124 ]] || [[ ${erl_result} == 137 ]] || [[ ${erl_result} == 1 ]]; then
      echo "File ${beam_filename}: timeout"
      rm "${beam_filename}"
      rm "${avm_filename}"
      exit 0
    else
      echo "INTERESTING: AVM crashed on ${beam_filename}, produced from: ${filename} with error code ${erl_result}!"
      rm "${beam_filename}"
      rm "${avm_filename}"
      exit ${erl_result}
    fi
  else
    echo "INTERESTING: erlc either crashed or timed out on ${filename}!"
    exit 42
  fi
}
main "$@"
