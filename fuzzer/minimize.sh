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

read -r -p 'Clear previous minimized outputs? [y/n] ' yn
case "${yn}" in
    [Yy]* ) echo 'Clearing previous outputs...'; rm -r out minimized || true;;
    [Nn]* ) ;;
    * ) echo "Please type 'y' or 'n'"; exit 1;;
esac

mkdir -p out
mkdir -p minimized

if [[ "$1" == */ ]]; then
  dirname=${1/%?/}
  samples=$(find "./interesting/${dirname}" -type f -name "*.erl")
elif [[ "$1" == 'all' ]]; then
  samples=$(find ./interesting -type f -name '*.erl')
else
  samples=$(find ./interesting -type f -name "$1".erl)
fi

seeds=()
names=()
for sample in $samples; do
  seed=$(egrep -o -- '--seed \d+' "${sample}" | sed 's/--seed //')
  name=$(basename -s '.erl' "${sample}")
  seeds+=("${seed}")
  names+=("${name}")
done

parallel --bar --line-buffer \
  "${AVM_ERLFUZZ_BIN}" --disable-maybe \
  --max-size 300 --max-recursion-depth 20 \
  reduce \
  --seed '{1}' '{2}' \
  --tmp-directory out --minimized-directory minimized -c ./run-atomvm-test.sh \
::: "${seeds[@]}" :::+ "${names[@]}"
