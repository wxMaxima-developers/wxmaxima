#!/bin/sh
# Smoke test for the side-by-side diff viewer (wxmxdiff / --diff).
#
# The diff viewer is a GUI window that stays open, so we can't use the usual
# --batch exit. Instead we launch it on two files, give it time to load, parse,
# diff and render, then fail if it crashed or printed an assertion / sanitizer
# error during that. The viewer has no other test, and it has historically
# broken silently when surrounding code changed.
#
# Args: <wxmaxima-binary> <file1.wxmx> <file2.wxmx>
# A display is expected (the suite runs under `xvfb-run ctest`).
set -u
BIN="$1"; F1="$2"; F2="$3"
ERRLOG="$(mktemp)"
rc=0

# --exit-on-error keeps any error dialog from blocking headlessly.
"$BIN" --debug --logtostderr --exit-on-error -d "$F1" "$F2" >"$ERRLOG" 2>&1 &
PID=$!

# Let it load both files, run the alignment and render the diff.
sleep 10

if kill -0 "$PID" 2>/dev/null; then
    # Still alive => it loaded and rendered without dying. Shut it down cleanly
    # (SIGTERM also triggers wxMaxima's handler that reaps any child Maxima).
    kill -TERM "$PID" 2>/dev/null
    sleep 2
    kill -9 "$PID" 2>/dev/null
    wait "$PID" 2>/dev/null
else
    wait "$PID"; ec=$?
    if [ "$ec" -ne 0 ]; then
        echo "FAIL: diff viewer exited with code $ec before we closed it (crash?)" >&2
        rc=1
    fi
fi

# Fail on an assertion / sanitizer finding / segfault even if it didn't crash
# the process outright (e.g. a debug assert that is logged but continued).
if grep -Eiq "assert .* failed|AddressSanitizer|UndefinedBehaviorSanitizer|runtime error:|Segmentation fault" "$ERRLOG"; then
    echo "FAIL: diff viewer reported an assertion / sanitizer error:" >&2
    grep -Ei "assert .* failed|AddressSanitizer|UndefinedBehaviorSanitizer|runtime error:|Segmentation fault" "$ERRLOG" | head >&2
    rc=1
fi

[ "$rc" -ne 0 ] && { echo "--- diff viewer output ---" >&2; cat "$ERRLOG" >&2; }
rm -f "$ERRLOG"
exit "$rc"
