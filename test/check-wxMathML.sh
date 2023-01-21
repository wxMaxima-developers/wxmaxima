#!/bin/sh

MAXIMA_COMMAND="$1"
WXMATHML_FILE="$2"
INPUT="$3"
EXPECTED_OUTPUT="$4"
MAXIMA_OUTPUT=$(echo "$INPUT" | "$MAXIMA_COMMAND" --quiet --init-lisp="$WXMATHML_FILE" | grep "<math>.*</math>")
test "$MAXIMA_OUTPUT" = "$EXPECTED_OUTPUT"
