#!/usr/bin/env bash
# ShellChecked

set -eu
set -o pipefail

for test in "${1:-examples}"/*.miniml; do
    if grep -q CHECK "$test"; then
        ./miniml -e "$test" | FileCheck "$test"
        ./miniml -c "$test" | lua | FileCheck "$test"
    fi
done
