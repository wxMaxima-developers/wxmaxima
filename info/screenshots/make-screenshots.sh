#!/bin/sh
# Makes the manual's screenshots from the scenes in info/screenshots/scenes.
#
# Usage: make-screenshots.sh [--language=<lang>] [--english=<dir>]
#                            <wxmaxima binary> <output directory> [scene...]
#
# Each scene is <name>.ini, plus <name>.wxm or <name>.wxmx if it has a
# worksheet; the screenshot is written to <output directory>/<name>.png.
# Without scene names every scene is made. Needs an X11 display: run it
# under xvfb-run, as the "screenshots" build target does. See README.md.
#
# With --language the user interface is shown in that language (wxMaxima's
# own --language option), and a worksheet in scenes/<lang>/ is used instead
# of the English one where there is one.
#
# With --english, a screenshot that comes out byte for byte the same as the
# English one in <dir> is not kept: the scene shows no translated text (a
# table, a plot), and the translated manual falls back to the English image
# anyway. Keeping such copies only makes every package carry the same file
# several times, which rpmlint reports as files-duplicated-waste.
set -u

language=
english=
while [ $# -gt 0 ]; do
    case "$1" in
        --language=*) language=${1#--language=} ;;
        --english=*)  english=${1#--english=} ;;
        *) break ;;
    esac
    shift
done

if [ $# -lt 2 ]; then
    echo "Usage: $0 [--language=<lang>] [--english=<dir>] <wxmaxima binary> <output directory> [scene...]" >&2
    exit 2
fi
# wxWidgets loads a translation only if the system has a locale for the
# language; without one these would be English screenshots filed as
# translated ones.
if [ -n "$language" ] && command -v locale >/dev/null 2>&1 &&
       ! locale -a | grep -qi "^${language}\([._@]\|$\)"; then
    echo "There is no $language locale (locale -a); generate one first, e.g. with locale-gen." >&2
    exit 1
fi
wxmaxima=$1
outdir=$2
shift 2
scenes=$(CDPATH= cd -- "$(dirname -- "$0")/scenes" && pwd)
mkdir -p "$outdir" || exit 1
outdir=$(CDPATH= cd -- "$outdir" && pwd)

if [ $# -eq 0 ]; then
    set --
    for ini in "$scenes"/*.ini; do
        name=$(basename "$ini" .ini)
        [ "$name" = common ] || set -- "$@" "$name"
    done
fi

# The scenes run in a directory of their own, with a short name: a scene
# like BatchImage shows the path of the file it loads.
work=${TMPDIR:-/tmp}/wxMaxima-screenshots${language:+-$language}
failed=0
for name in "$@"; do
    if [ ! -f "$scenes/$name.ini" ]; then
        echo "$name: no such scene" >&2
        failed=1
        continue
    fi
    rm -rf "$work"
    mkdir -p "$work/home" || exit 1
    cp "$scenes"/*.mac "$scenes"/*.png "$work"/ 2>/dev/null
    # Loaded before the scene starts, so that loading packages (and the
    # compiler notes some Lisps print while doing so) is not in the picture.
    cp "$scenes/wxmaxima-init.mac" "$work/home/"
    # A fresh configuration for every scene: the common settings first (they
    # are top-level keys, which have to come before the scene's groups).
    cat "$scenes/common.ini" "$scenes/$name.ini" > "$work/scene.ini"
    document=
    for ext in wxm wxmx; do
        source="$scenes/$name.$ext"
        # A translated worksheet, if the scene has one for this language.
        if [ -n "$language" ] && [ -f "$scenes/$language/$name.$ext" ]; then
            source="$scenes/$language/$name.$ext"
        fi
        if [ -f "$source" ]; then
            cp "$source" "$work/"
            document="$work/$name.$ext"
        fi
    done
    languageoption=
    [ -n "$language" ] && languageoption="--language=$language"
    echo "Making ${language:+$language/}$name.png"
    # HOME and MAXIMA_USERDIR keep the user's own Maxima and wxMaxima
    # settings (maxima-init.mac, the font cache, ...) out of the picture.
    HOME="$work/home" MAXIMA_USERDIR="$work/home" \
        "$wxmaxima" --ini="$work/scene.ini" $languageoption \
        --screenshot="$outdir/$name.png" $document
    status=$?
    if [ $status -ne 0 ] || [ ! -s "$outdir/$name.png" ]; then
        echo "$name: failed (exit code $status)" >&2
        failed=1
    elif [ -n "$english" ] && cmp -s "$outdir/$name.png" "$english/$name.png"; then
        echo "${language:+$language/}$name.png is the same as the English one; not kept"
        rm -f "$outdir/$name.png"
    fi
done
rm -rf "$work"
exit $failed
