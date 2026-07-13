#!/bin/sh
# Smoke test: opening a corrupt / hostile .wxmx must not hang or crash.
#
# A .wxmx is a ZIP archive; feeding wxMaxima a damaged one used to wedge a
# --batch run (it tried to auto-save the unloadable session, which re-opened a
# Save-As dialog from every idle cycle and never reached its own exit), and a
# crafted content.xml could drive a multi-billion-iteration loop
# (variables_num / activecell) that looked like a hang. This test loads a set
# of hostile files in --batch mode and fails if any of them hangs (killed by
# our own timeout) or crashes / asserts. A graceful "could not open" that exits
# on its own is the expected, passing outcome.
#
# Args: <wxmaxima-binary> <corrupt-file> [more corrupt files...]
# A display is expected (the suite runs under `xvfb-run ctest`).
set -u
BIN="$1"; shift

# How long a single load may take before we call it a hang. Maxima startup plus
# a load attempt is a few seconds; 45s is comfortably above that.
PER_FILE_TIMEOUT=45
rc=0

for f in "$@"; do
    name=$(basename "$f")
    ERRLOG="$(mktemp)"

    # No --exit-on-error: that flag masks the historical hang (it force-closes
    # on the first error). Plain --batch is the path that used to wedge.
    "$BIN" --batch --logtostderr "$f" >"$ERRLOG" 2>&1 &
    PID=$!

    # Poll for exit up to the timeout.
    waited=0
    while kill -0 "$PID" 2>/dev/null && [ "$waited" -lt "$PER_FILE_TIMEOUT" ]; do
        sleep 1
        waited=$((waited + 1))
    done

    if kill -0 "$PID" 2>/dev/null; then
        echo "FAIL: '$name' did not terminate within ${PER_FILE_TIMEOUT}s (hang)." >&2
        kill -TERM "$PID" 2>/dev/null; sleep 2; kill -9 "$PID" 2>/dev/null
        wait "$PID" 2>/dev/null
        rc=1
    else
        wait "$PID"; ec=$?
        # A graceful load failure exits with a non-zero code, which is fine.
        # A crash (segfault/abort) or a fired assertion is not.
        case "$ec" in
            134|139|132|136|135)
                echo "FAIL: '$name' crashed (exit $ec)." >&2; rc=1 ;;
        esac
    fi

    if grep -Eiq "assert .* failed|AddressSanitizer|UndefinedBehaviorSanitizer|runtime error:|Segmentation fault" "$ERRLOG"; then
        echo "FAIL: '$name' reported an assertion / sanitizer error:" >&2
        grep -Ei "assert .* failed|AddressSanitizer|UndefinedBehaviorSanitizer|runtime error:|Segmentation fault" "$ERRLOG" | head >&2
        rc=1
    fi

    [ "$rc" -ne 0 ] && { echo "--- $name output ---" >&2; tail -20 "$ERRLOG" >&2; }
    rm -f "$ERRLOG"
done

exit "$rc"
