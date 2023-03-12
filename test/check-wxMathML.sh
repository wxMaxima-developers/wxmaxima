#!/bin/sh

MAXIMA_COMMAND="$1"
WXMATHML_FILE="$2"
INPUT="$3"
EXPECTED_OUTPUT="$4"
MAXIMA_OUTPUT=$(echo "$INPUT" | "$MAXIMA_COMMAND" --quiet --init-lisp="$WXMATHML_FILE" | grep "<math>.*</math>")
echo "Maxima's output:"
echo "$MAXIMA_OUTPUT"
echo "Regex for expected output:"
echo "$EXPECTED_OUTPUT"
echo "$MAXIMA_OUTPUT" |grep "$EXPECTED_OUTPUT"
