#!/bin/bash
set -euo pipefail

# Usage: ./run [n]
# Run fuzzer, generating n samples. Defaults to 1000 runs.

if [[ ! -f .env ]]; then
    echo '.env file not found!'
    exit 1
fi

# automatically export all variables
set -a
source .env
set +a

# run 1000 times by default
n="${1-1000}"

read -r -p 'Clear previous outputs? [y/n] ' yn
case "${yn}" in
    [Yy]* ) echo 'Clearing previous outputs...'; rm -r out interesting || true;;
    [Nn]* ) ;;
    * ) echo "Please type 'y' or 'n'"; exit 1;;
esac

mkdir -p out
mkdir -p interesting

# Generate n samples using run_atomvm_test.sh to determine if sample is failing.
# Failing samples are written to interesting/ directory with `test{}` template names.
echo ""
echo "Generating outputs (timeout: ${AVM_FUZZ_TIMEOUT_S:-5}s)..."
seq "${n}" | parallel --bar --line-buffer \
    "${AVM_ERLFUZZ_BIN}" --disable-maybe \
    fuzz -c ./run-atomvm-test.sh \
    --tmp-directory out --interesting-directory interesting \
    'test{}'
echo 'Sorting output...'
./error-segregation.sh interesting
