#!/usr/bin/bash

for X in `find tests -name '*.scm'`; do
    ./scam -t lib/test/narc.scm $X
    status=$?
    if [ 0 -ne $status ]; then
	echo "Test file: " $X
        exit $status
    fi

done

exit 0

