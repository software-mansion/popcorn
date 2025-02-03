#!/bin/bash
set -euo pipefail

grep_command() {
    if command -v rg 2>&1 >/dev/null
    then
        rg "$@"
    else
        grep "$@"
    fi
}

if [[ -z "$1" ]]; then
  echo "Usage: $0 <path_to_directory>"
  exit 1
fi

directory="$1"

mkdir -p "${directory}/nbits"
mkdir -p "${directory}/asan"
mkdir -p "${directory}/operand"
mkdir -p "${directory}/compilation_error"
mkdir -p "${directory}/timeout"
mkdir -p "${directory}/abort"
mkdir -p "${directory}/badarg"
mkdir -p "${directory}/function_clause"
mkdir -p "${directory}/other"

for file in "${directory}"/*.stderr; do
  # e.g. interesting/test11
  test_n="${directory}/$(basename $file .stderr)"
  test_stdout="${test_n}.stdout"

  if [[ ! -s "${test_stdout}" ]]; then
    # remove empty stdout files
    rm "${test_stdout}"
  fi

  if [[ ! -s "${file}" ]]; then
    mv "${test_n}."* "${directory}/compilation_error/"
  else
    if grep_command -q "Unexpected nbits value @" "${file}"; then
      mv "${test_n}."* "${directory}/nbits/"
    elif grep_command -q "AddressSanitizer:DEADLYSIGNAL" "${file}"; then
      mv "${test_n}."* "${directory}/asan/"
    elif grep_command -q "Unexpected operand" "${file}"; then
      mv "${test_n}."* "${directory}/operand/"
    elif grep_command -q "Killed: 9" "${file}"; then
      mv "${test_n}."* "${directory}/timeout/"
    elif grep_command -q "Abort trap: 6" "${file}"; then
      mv "${test_n}."* "${directory}/abort/"
    elif grep_command -q "badarg" "${file}"; then
      mv "${test_n}."* "${directory}/badarg/"
    elif grep_command -q "function_clause" "${file}"; then
      mv "${test_n}."* "${directory}/function_clause/"
    else
      mv "${test_n}."* "$directory/other/"
    fi
  fi
done

echo "Analysis complete."
