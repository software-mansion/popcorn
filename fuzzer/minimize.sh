#!/bin/bash
set -euo pipefail

# Usage: ./minimize.sh <test case name> | <directory ending with slash> | all
# Run minimizer on passed sample(s)

if [[ ! -f .env ]]; then
    echo '.env file not found!'
    exit 1
fi

# automatically export all variables
set -a
source .env
set +a

if [[ "$#" -eq 0 ]]; then
  echo "Please pass sample name, directory with slash or 'all'"
  exit 1
fi

if [[ "$1" == */ ]]; then
    dirname=${1/%?/}
    samples=$(find "./interesting/${dirname}" -type f -name "*.erl")
elif [[ "$1" == 'all' ]]; then
    samples=$(find . -type f -name '*.erl')
else
    samples=$(find . -type f -name "$1".erl)
fi

rm -r out || true
mkdir -p out
mkdir -p minimized

seeds=()
names=()
for sample in $samples; do
    seed=$(egrep -o -- '--seed \d+' "${sample}" | sed 's/--seed //')
    name=$(basename -s '.erl' "${sample}")

    seeds+=("${seed}")
    names+=("${name}")
done

parallel --bar --line-buffer "${AVM_ERLFUZZ_BIN}" \
    --max-size 200 --max-recursion-depth 50 \
    reduce \
    --seed '{1}' '{2}' \
    --tmp-directory out --minimized-directory minimized -c ./run-atomvm-test.sh \
::: "${seeds[@]}" :::+ "${names[@]}"
