#!/bin/sh
# Checks the shell completions wxmaxima-genopts wrote from
# src/CommandLineOptions.h: that each one parses, and -- for bash and fish,
# whose completion can be run from a script -- that it actually completes
# options and offers the right files.
#
# Usage: check-shell-completion.sh COMPLETIONS_DIR
# A shell that isn't installed is skipped; if none is, the test is skipped
# (exit status 77).

dir=$(cd "$1" && pwd) || exit 1
failures=0
tested=0

fail() {
    echo "FAIL: $*" >&2
    failures=$((failures + 1))
}

# A scratch directory with one file wxMaxima opens and one it doesn't.
work=$(mktemp -d) || exit 1
trap 'rm -rf "$work"' EXIT
: > "$work/sheet.wxmx"
: > "$work/notes.txt"
cd "$work" || exit 1

# --- bash ---------------------------------------------------------------
bash_completion=
for f in /usr/share/bash-completion/bash_completion \
         /usr/local/share/bash-completion/bash_completion \
         /opt/homebrew/share/bash-completion/bash_completion \
         /opt/local/share/bash-completion/bash_completion; do
    [ -f "$f" ] && bash_completion=$f && break
done
if command -v bash > /dev/null && [ -n "$bash_completion" ]; then
    tested=1
    bash -n "$dir/wxmaxima" || fail "bash: syntax error in $dir/wxmaxima"
    # Prints what bash completes the given command line with, one per line.
    # The last argument is the word being completed.
    complete_bash() {
        bash --norc --noprofile -c '
            . "$1"; . "$2"; shift 2
            COMP_WORDS=("$@")
            COMP_CWORD=$(( ${#COMP_WORDS[@]} - 1 ))
            COMP_LINE="$*"; COMP_POINT=${#COMP_LINE}
            COMPREPLY=()
            _wxmaxima wxmaxima "${COMP_WORDS[COMP_CWORD]}" \
                "${COMP_WORDS[COMP_CWORD-1]}" 2> /dev/null
            printf "%s\n" "${COMPREPLY[@]}"' \
            sh "$bash_completion" "$dir/wxmaxima" "$@"
    }
    expect_bash() { # WANTED UNWANTED WORDS...
        wanted=$1 unwanted=$2; shift 2
        result=$(complete_bash "$@")
        [ -z "$wanted" ] || echo "$result" | grep -qx -- "$wanted" ||
            fail "bash: '$*' does not offer $wanted (got: $(echo $result))"
        if [ -n "$unwanted" ] && echo "$result" | grep -qx -- "$unwanted"; then
            fail "bash: '$*' offers $unwanted"
        fi
    }
    expect_bash --open=         ""         wxmaxima --op
    expect_bash --single_thread ""         wxmaxima --single
    expect_bash -b              ""         wxmaxima -
    # Completing an empty word lists no files outside a real completion
    # context (bash-completion quotes it to '' and compgen only dequotes
    # inside one), so these complete a first letter.
    expect_bash sheet.wxmx      ""         wxmaxima s
    expect_bash ""              notes.txt  wxmaxima n
    # -b is a switch: what follows is the file to run, not its value.
    expect_bash sheet.wxmx      ""         wxmaxima -b s
    expect_bash ""              notes.txt  wxmaxima -o n
    expect_bash notes.txt       ""         wxmaxima --ini n
    expect_bash ""              sheet.wxmx wxmaxima --use-version s
else
    echo "bash or bash-completion not installed: bash completion not tested"
fi

# --- zsh ------------------------------------------------------------------
# A zsh completion function only runs inside the line editor, so this
# checks the syntax only.
if command -v zsh > /dev/null; then
    tested=1
    zsh -n "$dir/_wxmaxima" || fail "zsh: syntax error in $dir/_wxmaxima"
    head -n 1 "$dir/_wxmaxima" | grep -q '^#compdef wxmaxima' ||
        fail "zsh: $dir/_wxmaxima does not start with #compdef"
else
    echo "zsh not installed: zsh completion not tested"
fi

# --- fish -----------------------------------------------------------------
if command -v fish > /dev/null; then
    tested=1
    fish --no-execute "$dir/wxmaxima.fish" ||
        fail "fish: syntax error in $dir/wxmaxima.fish"
    complete_fish() {
        fish --no-config -c 'source $argv[1]; complete -C $argv[2]' \
            "$dir/wxmaxima.fish" "$1" | cut -f 1
    }
    complete_fish "wxmaxima --op" | grep -qx -- --open ||
        fail "fish: 'wxmaxima --op' does not offer --open"
    complete_fish "wxmaxima -o " | grep -qx sheet.wxmx ||
        fail "fish: 'wxmaxima -o ' does not offer sheet.wxmx"
    complete_fish "wxmxdiff --di" | grep -qx -- --diff ||
        fail "fish: 'wxmxdiff --di' does not offer --diff"
else
    echo "fish not installed: fish completion not tested"
fi

if [ "$tested" = 0 ]; then
    echo "No shell to test the completions with."
    exit 77
fi
[ "$failures" = 0 ]
