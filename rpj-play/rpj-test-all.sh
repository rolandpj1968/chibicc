#! /bin/bash

total=0
fail=0

for test in rpj-*.c; do

    total=$((total+1))
    
    if ! ./rpj-test.sh $test; then
	fail=$((fail+1))
    fi

done

echo
echo "$total tests run - $fail failures"
