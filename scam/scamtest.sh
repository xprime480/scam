#!/usr/bin/bash

if [ -f testcases.txt ]; then
    files=`cat testcases.txt`
else
    files=`find tests -name '*.scm'`
fi

for X in $files; do
    ./scam -t $X
    status=$?
    if [ 0 -ne $status ]; then
        echo "Test file: " $X
        exit $status
    fi

done

exit 0

