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
mkdir -p "${directory}/other"

for file in "${directory}"/*.stderr; do
  # e.g. interesting/test11
  test_n="${directory}/$(basename $file .stderr)"

  if [[ ! -s "${file}" ]]; then
    mv "${test_n}."* "${directory}/compilation_error/"
  else
    if grep_command -q "Unexpected nbits value @" "${file}"; then
      mv "${test_n}."* "${directory}/nbits/"
    elif grep_command -q "AddressSanitizer:DEADLYSIGNAL" "${file}"; then
      mv "${test_n}."* "${directory}/asan/"
    elif grep_command -q "Unexpected operand" "${file}"; then
      mv "${test_n}."* "${directory}/operand/"
    else
      mv "${test_n}."* "$directory/other/"
    fi
  fi
done

echo "Analysis complete."
