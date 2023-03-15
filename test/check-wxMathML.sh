#!/bin/sh

MAXIMA_COMMAND="$1"
WXMATHML_FILE="$2"
INPUT="$3"
EXPECTED_OUTPUT="$4"
MAXIMA_OUTPUT=$(echo "$INPUT" | "$MAXIMA_COMMAND" --quiet --init-lisp="$WXMATHML_FILE")
if echo "$MAXIMA_OUTPUT" |grep "$EXPECTED_OUTPUT"; then
    echo "Success!"
    exit 0
else
    echo "Failure!"
    echo "Maxima's output:"
    echo "$MAXIMA_OUTPUT"
    echo "Regex for expected output:"
    echo "$EXPECTED_OUTPUT"
    exit 1
fi
