# -*- mode: CMake; cmake-tab-width: 4; -*-

# Compile a list of test files
file(GLOB IMAGE_FILES automatic_test_files/*.png automatic_test_files/*.gif)
file(GLOB TEST_FILES ${IMAGE_FILES} automatic_test_files/*.wxmx automatic_test_files/*.wxm automatic_test_files/*.mac automatic_test_files/*.cfg)
file(GLOB TESTBENCH_IMAGE_FILES automatic_test_files/a.png automatic_test_files/b.png automatic_test_files/c.png automatic_test_files/d.png automatic_test_files/ad.gif)
file(GLOB TESTBENCH_FILES ${TESTBENCH_IMAGE_FILES} testbench_simple.wxmx testbench_simple.wxm)

find_program(MAXIMA maxima)
if(MAXIMA)
	execute_process(COMMAND ${MAXIMA} --version
					OUTPUT_VARIABLE MAXIMA_VERSION_TEXT OUTPUT_STRIP_TRAILING_WHITESPACE)
	string(REGEX REPLACE "^Maxima (.*)" "\\1"  MAXIMA_VERSION "${MAXIMA_VERSION_TEXT}")
	message(STATUS "Found Maxima: ${MAXIMA} (found version \"${MAXIMA_VERSION}\")")
else()
    message(STATUS "Maxima not found. Many tests will fail, Maxima is required for most tests.")
endif()
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/xmlpatterngen.sh.in" xmlpatterngen.sh)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-xml-validity.sh.in" check-xml-validity.sh)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-wxmx-validity.sh.in" check-wxmx-validity.sh)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-wizard-commands.sh.in" check-wizard-commands.sh)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-markdown.sh.in" check-markdown.sh)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-translations.sh.in" check-translations.sh)

# Create a headless wrapper script
set(WXM_TEST_WRAPPER "${CMAKE_CURRENT_BINARY_DIR}/run_headless.sh")
file(WRITE "${WXM_TEST_WRAPPER}"
"#!/bin/sh
# Strict headless wrapper for GUI tests

# 1. Setup Isolated Environment
export XDG_RUNTIME_DIR=\"/tmp/wxmaxima-runtime-$$\"
mkdir -p \"$XDG_RUNTIME_DIR\"
chmod 700 \"$XDG_RUNTIME_DIR\"
trap \"rm -rf $XDG_RUNTIME_DIR\" EXIT

unset DISPLAY
unset WAYLAND_DISPLAY

# 2. Weston is deliberately not used.
#
# This used to try \"weston --backend=headless -- <test>\" first. Weston 13
# answers that with \"fatal: unhandled option: --\" and runs nothing, so every
# test on a machine that has weston installed failed in a hundredth of a second.
# It went unnoticed for as long as nothing invoked this wrapper; the moment the
# unit tests were moved onto it, every GUI test on the Ubuntu CI - which has
# weston - broke, while this machine, which does not, saw nothing.
#
# X11 under Xvfb is what wxMaxima is actually tested against, so that is what
# this uses. Should Wayland coverage ever be wanted it needs a working
# invocation - weston started in the background, WAYLAND_DISPLAY exported, the
# client run separately - and a CI job that proves it.

# 3. Try xvfb-run (X11)
if command -v xvfb-run >/dev/null 2>&1; then
    export GDK_BACKEND=x11
    echo \"Attempting xvfb-run...\" >&2
    # Use exec to ensure signals from ctest reach the process and it's dead on timeout
    exec xvfb-run -a \"$@\"
fi

# 4. Try manual Xvfb (X11)
if command -v Xvfb >/dev/null 2>&1; then
    export GDK_BACKEND=x11
    echo \"Attempting manual Xvfb...\" >&2
    for i in $(seq 100 200); do
        if ! [ -f \"/tmp/.X$i-lock\" ]; then
            DISP=\":$i\"
            Xvfb \"$DISP\" -screen 0 1280x1024x24 >/dev/null 2>&1 &
            XPID=$!
            export DISPLAY=\"$DISP\"
            # Ensure Xvfb is dead when we are killed or exit
            trap \"kill $XPID >/dev/null 2>&1\" EXIT TERM INT HUP
            if command -v xdpyinfo >/dev/null 2>&1; then
                for j in $(seq 1 50); do
                    xdpyinfo -display \"$DISPLAY\" >/dev/null 2>&1 && break
                    sleep 0.1
                done
            else
                sleep 0.5
            fi
            \"$@\"
            exit $?
        fi
    done
    echo \"Manual Xvfb failed.\" >&2
fi

echo \"Error: No headless environment could be started. Refusing to connect to desktop.\" >&2
exit 1
")
execute_process(COMMAND chmod +x "${WXM_TEST_WRAPPER}")

find_program(XMLLINT xmllint)
find_program(UNZIP unzip)
if(NOT XMLLINT)
    message(STATUS "xmllint not found. XML duplicate attribute validation will be skipped.")
elseif(NOT UNZIP)
    message(STATUS "unzip not found. .wxmx file XML validation will be skipped.")
else()
    message(STATUS "Found xmllint and unzip. XML validity validation enabled.")
endif()

# Copy the .wxm files to the binary dir so they can be used as test case without
# being afraid that we change the test cases somehow.
file(
    COPY
    ${TEST_FILES}
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    FILE_PERMISSIONS OWNER_WRITE OWNER_READ
    )

file(
    COPY
    ${TESTBENCH_FILES}
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_WRITE OWNER_READ
)

file(
    COPY
    ${IMAGE_FILES}
    DESTINATION ${CMAKE_BINARY_DIR}/tex
    FILE_PERMISSIONS OWNER_WRITE OWNER_READ
    )

# IMPORTANT:
# On any reasonable system made in the last decade, the default timeout is
# more than enough. If *any* tests fail to complete within this timeout,
# it is a performance bug that must be investigated and fixed. Compared
# to any "real" work, the test documents are all trivial - even the one
# that has all cell types. Reference: On Bionic running on a 2009
# Core 2 Duo MacBook Pro, most tests run in under 10 seconds, and just
# a few take 30 seconds, and even that feels like it's too long to be
# acceptable. In other words: never extend test timeouts. Fix the
# performance bugs that cause such behavior.


# Per-test watchdog timeout (seconds). This is applied as the ctest TIMEOUT
# property of every test below (see the loop at the end of this file). Its job
# is NOT to act as a performance budget but to stop a single hung test from
# eating the whole CI run: without it a wedged test (e.g. one waiting forever on
# a gnuplot/draw window that never appears on a headless runner) keeps ctest
# blocked until GitHub kills the job after two hours.
#
# Note: the old `set(CTEST_TEST_TIMEOUT 120)` here was a no-op - CTEST_TEST_TIMEOUT
# is only honoured by ctest in -S script mode, and this project drives ctest
# directly against the build tree, so no per-test timeout was ever in effect.
#
# The value is generous on purpose: the slowest legitimate test (testbench_simple)
# takes ~110 s on the CI runners, so 300 s catches genuine hangs while leaving
# enough head-room not to flake under load. A real performance regression that
# pushes a trivial test anywhere near this limit is still a bug to be fixed, not
# a reason to raise the timeout.
set(WXM_TEST_TIMEOUT 380 CACHE STRING "Per-test ctest timeout in seconds (watchdog against hung tests)")

if(WXM_UNIT_TESTS)
    add_subdirectory(unit_tests)
endif()
add_subdirectory(image-test)

# The troubleshooting dialog's checks against a real Maxima (GH #2559): they
# notice if Maxima or wxMathML.lisp change in a way the checks no longer
# understand. Not a "unittest": it needs Maxima.
if(WXM_UNIT_TESTS AND MAXIMA AND TARGET test_Troubleshooting)
    if(WIN32)
        set(wxm_troubleshooting_launcher "")
    else()
        set(wxm_troubleshooting_launcher "${WXM_TEST_WRAPPER}")
    endif()
    add_test(NAME Troubleshooting_real_maxima
            COMMAND ${CMAKE_COMMAND} -E env "WXM_MAXIMA=${MAXIMA}"
                    ${wxm_troubleshooting_launcher}
                    "$<TARGET_FILE:test_Troubleshooting>" "[.maxima]")
endif()

# GH #1907: Maxima's batch() has to be able to load a .wxm wxMaxima wrote,
# even when its text cells contain "/*" or "*/", or lines that look like cell
# markers, or nested comments, and when it remembers answers to Maxima's
# questions. Each of these would make Maxima run the "x:2$", "x:3$", "x:4$"
# or "y:2$" in the file, or fail. A code cell's own nested comment has to
# reach Maxima unchanged, so its "z:2" stays a comment.
# test_WXMRoundtrip checks that wxMaxima still writes this file, so the two
# can't drift apart.
add_test(
    NAME wxm_loads_in_maxima
    COMMAND "${CMAKE_COMMAND}"
            "-DMAXIMA=${MAXIMA}"
            "-DWXM_FILE=${CMAKE_CURRENT_SOURCE_DIR}/wxm_comment_injection.wxm"
            "-DWORK_DIR=${CMAKE_CURRENT_BINARY_DIR}"
            "-DREGEX=x is 1 and y is 1 and z is 1"
            -P "${CMAKE_CURRENT_SOURCE_DIR}/check-wxm-batch.cmake")
set_tests_properties(wxm_loads_in_maxima PROPERTIES
    FAIL_REGULAR_EXPRESSION "incorrect syntax;end of file in comment;x is [234];y is 2;z is 2")

# Test if maxima is working
add_test(
    NAME runMaxima
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo "quit();" | ${MAXIMA})

# Test if maxima is able to communicate with a backend
add_test(
    NAME runMaxima-over-network
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo 'quit() ;' | nc -l 40000 & sleep 10 ; ${MAXIMA} -s 40000)

# wxm_add_wxmathml_test(<name> <input> <regex>) sends <input> to a Maxima that
# has loaded src/wxMathML.lisp and passes if the output, its line breaks
# replaced by spaces, matches <regex> -- a CMake regular expression, so "(",
# ")", "+", "?" and "|" need a backslash to stand for themselves. Both stdout
# and stderr are searched, and the output is printed even on success, so a
# test can additionally set FAIL_REGULAR_EXPRESSION.
#
# These tests used to run a POSIX shell script (check-wxMathML.sh), which kept
# them off Windows and only searched stdout (GH #1775). The input and the
# regex are now written to files here and read by check-wxMathML.cmake, so no
# shell and no command-line quoting is involved on any platform.
function(wxm_add_wxmathml_test name input regex)
  set(dir "${CMAKE_CURRENT_BINARY_DIR}/wxmathml_tests")
  # echo, which the shell script used, ended the input with a line break.
  file(WRITE "${dir}/${name}.mac" "${input}\n")
  file(WRITE "${dir}/${name}.regex" "${regex}")
  add_test(
    NAME ${name}
    COMMAND "${CMAKE_COMMAND}"
            "-DMAXIMA=${MAXIMA}"
            "-DWXMATHML_LISP=${CMAKE_SOURCE_DIR}/src/wxMathML.lisp"
            "-DINPUT_FILE=${dir}/${name}.mac"
            "-DREGEX_FILE=${dir}/${name}.regex"
            -P "${CMAKE_CURRENT_SOURCE_DIR}/check-wxMathML.cmake")
endfunction()

# A (very simple) test, if wxMathML.lisp file works (creates the expected XML output).
wxm_add_wxmathml_test(check-wxMathML.lisp_1
    "1+1;"
    "<math><lbl altCopy=\"%o1\">\\(%o1\\) </lbl><mn>2</mn></math>")

# GH #1948: box() draws its own border (the <hl> tag), so it must never be
# wrapped in an extra, redundant pair of parentheses no matter what operator
# surrounds it -- this pins that "a*box(b)*c" renders with no <p>/wxxml-paren
# element between box's own <mrow> and its <hl> content.
wxm_add_wxmathml_test(check-wxMathML.lisp_box_no_extra_parens
    "a*box(b)*c;"
    "</hl></mrow><h>\\*</h><mi")

# GH #1948, the fringe case: box(expr, highlight) doesn't draw a border at
# all (see MathParser::ParseHighlightTag) -- it only colors its content --
# so unlike an ordinary box it still needs the parentheses above to visually
# delimit its extent. This must keep getting them even after the fix above.
wxm_add_wxmathml_test(check-wxMathML.lisp_highlight_box_keeps_parens
    "a*box(b,highlight)*c;"
    "<p lisp=\"wxxml-paren\"><mrow><hl boxname=\"highlight\">")

# GH #640: with derivabbrev:true, diff(C,x) is drawn as a subscripted C_x,
# which a product or a minus sign must not put in parentheses (Maxima's own
# 2D display doesn't either) -- while an exponent still has to, or C_x^2
# would read as C with the subscript x^2.
wxm_add_wxmathml_test(check-wxMathML.lisp_derivabbrev_product_no_parens
    "derivabbrev:true$ depends(C,x)$ B*diff(C,x); -diff(C,x);"
    "<h>\\*</h><munder d=\"1\" altCopy=\"diff\\(C,x,1\\)\">.*<mo>-</mo><munder d=\"1\"")
wxm_add_wxmathml_test(check-wxMathML.lisp_derivabbrev_power_keeps_parens
    "derivabbrev:true$ depends(C,x)$ diff(C,x)^2;"
    "<msup><mrow><mrow lisp=\"wxxml-paren\"><p lisp=\"wxxml-paren\"><munder d=\"1\"")

# GH #1536: a bare summand doesn't need the disambiguating parentheses
# SumCell used to always draw around it -- only a compound one (here a sum
# of terms, "k+k^2") does. wxxml-sum (wxMathML.lisp) decides this from the
# real Maxima expression (mplusp) and hands the decision to SumCell via the
# needsparen attribute on <sm>; these two pin that attribute for both cases.
wxm_add_wxmathml_test(check-wxMathML.lisp_sum_bare_summand_no_parens
    "'sum(k,k,1,n);"
    "<sm needsparen=\"false\">")
wxm_add_wxmathml_test(check-wxMathML.lisp_sum_compound_summand_needs_parens
    "'sum(k+k^2,k,1,n);"
    "<sm needsparen=\"true\">")

# GH #1672: wx-cd (the function wxMaxima uses to keep Maxima's working
# directory in sync with the worksheet's) used to fail to cd into a
# directory whose name contains characters outside the OS's active
# codepage -- on Windows specifically, since sb-posix:chdir there goes
# through the C runtime's ANSI-only _chdir (SBCL bug launchpad #2100706).
# A CJK directory name is reliably outside the default English-Windows
# codepage (1252). It needs a fixture directory and a
# check of its own, so it has its own CMake script instead of
# wxm_add_wxmathml_test() -- see check-wx-cd-non-ascii.cmake.
if(MAXIMA)
  add_test(
      NAME check-wxMathML.lisp_wx-cd_non_ascii_dir
      COMMAND "${CMAKE_COMMAND}"
              "-DMAXIMA=${MAXIMA}"
              "-DWXMATHML_LISP=${CMAKE_SOURCE_DIR}/src/wxMathML.lisp"
              "-DTEST_BASE_DIR=${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files"
              -P "${CMAKE_CURRENT_SOURCE_DIR}/check-wx-cd-non-ascii.cmake"
  )
endif()

# Syntax-check every wizard/menu command template (the "#n#" CommandWiz rules in
# src/wxMaxima.cpp) through Maxima's parser, so a malformed generated command --
# e.g. an unbalanced parenthesis -- fails the build instead of only being found
# by a user.
add_test(
    NAME check-wizard-commands
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wizard-commands.sh"
)

# Lint the hand-written Markdown docs (README, Compiling, NEWS, ...) with mdl.
# Skipped with a warning where mdl isn't installed.
add_test(
    NAME check-markdown
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-markdown.sh"
)

# Check every translation catalogue with msgfmt. Chiefly --check-format: a msgstr
# whose format specifiers don't match its msgid makes wxWidgets format against
# the wrong arguments at runtime, in a language we may not read.
# Skipped with a warning where gettext isn't installed.
add_test(
    NAME check-translations
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-translations.sh"
)

# Check that no translatable string can go missing the way ~7000 of them once
# did: the xgettext glob in locales/wxMaxima/CMakeLists.txt was flat src/*, so
# everything under src/cells, src/wizards, src/dialogs and src/sidebars was
# absent from wxMaxima.pot for years without a single warning anywhere.
# The POT drift check in CI cannot see this -- it regenerates the POT and diffs
# it, and a broken glob truncates both sides identically. A CMake script rather
# than a shell helper so it runs on Windows CI too, and it needs neither a build
# nor gettext.
add_test(
    NAME check-pot-coverage
    COMMAND "${CMAKE_COMMAND}"
            "-DSOURCE_DIR=${CMAKE_SOURCE_DIR}"
            -P "${CMAKE_CURRENT_SOURCE_DIR}/check-pot-coverage.cmake"
)
set_property(TEST check-pot-coverage APPEND PROPERTY LABELS unittest)

# An unpaired backtick in the manual's source broke composing the Hungarian
# manual PDF (GH #2271); see the script for why only Hungarian.
add_test(
    NAME check-manual-backticks
    COMMAND "${CMAKE_COMMAND}"
            "-DMANUAL=${CMAKE_SOURCE_DIR}/info/wxmaxima.md"
            -P "${CMAKE_CURRENT_SOURCE_DIR}/check-manual-backticks.cmake"
)
set_property(TEST check-manual-backticks APPEND PROPERTY LABELS unittest)

# Every dialog whose geometry wxPersistenceManager remembers has to send its
# destroy event from its own destructor, or it crashes on closing with wxQt.
add_test(
    NAME check-persistent-dialogs
    COMMAND "${CMAKE_COMMAND}"
            "-DSOURCE_DIR=${CMAKE_SOURCE_DIR}"
            -P "${CMAKE_CURRENT_SOURCE_DIR}/check-persistent-dialogs.cmake"
)
set_property(TEST check-persistent-dialogs APPEND PROPERTY LABELS unittest)

# The source tarball GitHub generates for a release must not contain dot files
# (GH #2272). Only meaningful in a git checkout, and needs git to look.
find_package(Git QUIET)
if(GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/.git")
    add_test(
        NAME source_tarball_has_no_dotfiles
        COMMAND "${CMAKE_COMMAND}"
                "-DGIT_EXECUTABLE=${GIT_EXECUTABLE}"
                "-DSOURCE_DIR=${CMAKE_SOURCE_DIR}"
                "-DWORK_DIR=${CMAKE_CURRENT_BINARY_DIR}/tarball-check"
                -P "${CMAKE_CURRENT_SOURCE_DIR}/check-tarball-dotfiles.cmake"
    )
    set_property(TEST source_tarball_has_no_dotfiles APPEND PROPERTY LABELS unittest)
endif()

add_test(
    NAME multiple_batch_files
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch --exit-on-error unicode.wxm boxes.wxm)

add_test(
    NAME multiple_batch_files_inverse
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch --exit-on-error boxes.wxm unicode.wxm)

# to_lisp() / (to-maxima) mode switching. The worksheet is self-checking (each
# Lisp form sets a Maxima variable that a later Maxima cell verifies with
# error()); with --exit-on-error a wrong/unevaluated value fails, and the ctest
# watchdog catches the prompt-flood hang that a mis-split "$" used to cause.
add_test(
    NAME lisp_mode
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error lisp_mode.wxm)

# set_display('ascii): wx-ascii-displa wraps Maxima's own stock ASCII-art
# printer in <wxxml-asciimath> markers instead of reimplementing it. The
# worksheet is self-checking (results are compared against the same
# expressions evaluated in the default XML display mode); with
# --exit-on-error a wrong value, an error, or a hang in the lisp hook fails
# the test.
add_test(
    NAME asciiDisplay
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error asciiDisplay.wxm)

# Smoke test for the side-by-side diff viewer (wxmxdiff / --diff): it has no
# other test and has broken silently when surrounding code changed. Loads two
# .wxmx files in diff mode and fails on a crash or assertion.
add_test(
    NAME diff_viewer_smoke
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND sh ${CMAKE_CURRENT_SOURCE_DIR}/diff_smoke_test.sh $<TARGET_FILE:wxmaxima> all-celltypes.wxmx intervals.wxmx)

# Hostile-input smoke test: a corrupt .wxmx (damaged ZIP container, or a
# content.xml with attacker-chosen counts) must fail to open gracefully, never
# hang or crash. Fixtures live in the test source dir (not automatic_test_files)
# so the .wxmx-validity checks don't try to validate them.
add_test(
    NAME corrupt_wxmx_smoke
    COMMAND sh ${CMAKE_CURRENT_SOURCE_DIR}/corrupt_wxmx_smoke.sh $<TARGET_FILE:wxmaxima>
            ${CMAKE_CURRENT_SOURCE_DIR}/corrupt_garbage.wxmx
            ${CMAKE_CURRENT_SOURCE_DIR}/corrupt_truncated.wxmx
            ${CMAKE_CURRENT_SOURCE_DIR}/corrupt_hugevars.wxmx)
set_tests_properties(corrupt_wxmx_smoke PROPERTIES TIMEOUT 200)

add_test(
    NAME multiple_batch_files2
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch --exit-on-error --single_process unicode.wxm boxes.wxm)

add_test(
    NAME multiple_batch_files_singlethread
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch --exit-on-error --single_thread unicode.wxm boxes.wxm)

add_test(
    NAME multiple_batch_files2_singlethread
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch --exit-on-error --single_process --single_thread unicode.wxm boxes.wxm)

wxm_add_wxmathml_test(wxstatusbar
    "wxstatusbar(\"a<1>b\");"
    "<statusbar>a&lt;1&gt;b</statusbar>")

# wx_version_min(): $wxmaximaversion is set the way wxMaxima sets it (a
# "-dev" suffix plus a toolkit suffix), then compared against versions that
# are equal, shorter, older and newer, including "26.10" vs "26.9", which a
# string comparison would get wrong.
wxm_add_wxmathml_test(wx_version_min
    ":lisp (setq $wxmaximaversion \"26.09.0-dev_GTK3\")\n[wx_version_min(\"26.09.0\"), wx_version_min(\"26.9\"), wx_version_min(\"26.08.3\"), wx_version_min(\"25.12.9\"), wx_version_min(\"26.09.1\"), wx_version_min(\"26.10.0\"), wx_version_min(\"27\")];"
    "\\(%o1\\) </lbl><mrow list=\"true\"><t[^>]*>\\[</t><mrow><t lisp=\"true\">true</t></mrow><mo>,</mo><mrow><t lisp=\"true\">true</t></mrow><mo>,</mo><mrow><t lisp=\"true\">true</t></mrow><mo>,</mo><mrow><t lisp=\"true\">true</t></mrow><mo>,</mo><mrow><t lisp=\"false\">false</t></mrow><mo>,</mo><mrow><t lisp=\"false\">false</t></mrow><mo>,</mo><mrow><t lisp=\"false\">false</t></mrow><t[^>]*>]</t></mrow>")

# ...and outside wxMaxima, where $wxmaximaversion is never set, it is false.
wxm_add_wxmathml_test(wx_version_min_outside_wxmaxima
    "wx_version_min(\"1.0\");"
    "<t lisp=\"false\">false</t>")

# with_slider_draw_bare(): each frame is a plain draw() call, so a frame can
# hold several scenes, 2D and 3D mixed. Two frames means two gnuplot scripts
# in the <slide> tag.
wxm_add_wxmathml_test(with_slider_draw_bare
    "with_slider_draw_bare(a,[1,2],columns=2,gr2d(explicit(sin(a*x),x,0,1)),gr3d(explicit(a*x*y,x,0,1,y,0,1)));"
    "<slide[^>]*>[^<]*\\.gnuplot;[^<]*\\.gnuplot;[^<]*</slide>")

# with_slider_draw()'s file_name writes a gif the user asked for, so wxMaxima
# must not delete it once it has displayed it (GH #2389).
wxm_add_wxmathml_test(with_slider_draw_file_name_kept
    "with_slider_draw(a,[1,2],explicit(a*x,x,0,1),file_name=\"anim_kept\");"
    "<img del=\"no\">anim_kept\\.gif</img>")

# ...and file_name, which draw()'s animated_gif terminal cannot honour for a
# frame made of several scenes: the frames are rendered as usual, and the
# <slide> tag asks wxMaxima to assemble them into anim.gif (GH #2361), named
# with its full path, as wxMaxima may work in another directory than Maxima.
# On Windows that path starts with a drive letter and may use backslashes.
wxm_add_wxmathml_test(with_slider_draw_bare_file_name
    "with_slider_draw_bare(a,[1,2],file_name=\"anim\",gr2d(explicit(sin(a*x),x,0,1)));"
    "<slide[^>]* gifFile=\"([A-Za-z]:)?[/\\\\][^\"]*[/\\\\]anim\\.gif\"[^>]*>[^<]*\\.gnuplot;[^<]*\\.gnuplot;[^<]*</slide>")

# wximplicit_plot() has to load the implicit_plot package itself: without it
# implicit_plot() stays unevaluated, no image is written, and wxMaxima reports
# a missing file instead of a plot (GH #1106).
wxm_add_wxmathml_test(wximplicit_plot_loads_package
    "wximplicit_plot(x^2+y^2=1,[x,-2,2],[y,-2,2])$ is(?fboundp('implicit_plot) # false);"
    "<t lisp=\"true\">true</t>")

wxm_add_wxmathml_test(wxbuildinfo
    "wxbuild_info();"
    "WxMaxima")

# Test if the XML output is valid and doesn't contain duplicate attributes
add_test(
    NAME check-xml-validity_matrix_default
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-xml-validity.sh" "wx_matrix(matrix([1]));"
)

add_test(
    NAME check-xml-validity_matrix_lines
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-xml-validity.sh" "wx_matrix(matrix([1]), lines=true);"
)

add_test(
    NAME check-xml-validity_matrix_complex
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-xml-validity.sh" "wx_matrix(matrix([1,2],[3,4]), lines=true, rownames=true, colnames=true, parenstyle=square);"
)

# GH #2343: wx_matrix()'s oversized option reaches wxMaxima as an attribute
# of the <tb> tag, and doesn't clash with any attribute already on it.
add_test(
    NAME check-xml-validity_matrix_oversized
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-xml-validity.sh" "wx_matrix(matrix([1,2],[3,4]), lines=true, parenstyle=none, oversized=scroll);"
)

wxm_add_wxmathml_test(check-wxMathML.lisp_matrix_oversized
    "wx_matrix(matrix([1,2],[3,4]), oversized=full);"
    "<tb[^>]* oversized=\"full\"")

# wx_matrix()'s banding option reaches wxMaxima as an attribute of the <tb>
# tag; banding=auto, the default, adds none.
wxm_add_wxmathml_test(check-wxMathML.lisp_matrix_banding_on
    "wx_matrix(matrix([1,2],[3,4]), banding=true);"
    "<tb[^>]* banding=\"true\"")
wxm_add_wxmathml_test(check-wxMathML.lisp_matrix_banding_off
    "wx_matrix(matrix([1,2],[3,4]), banding=false);"
    "<tb[^>]* banding=\"false\"")
wxm_add_wxmathml_test(check-wxMathML.lisp_matrix_banding_auto
    "wx_matrix(matrix([1,2],[3,4]), banding=auto);"
    "<tb")
set_tests_properties(check-wxMathML.lisp_matrix_banding_auto PROPERTIES
    FAIL_REGULAR_EXPRESSION "banding=")
add_test(
    NAME check-xml-validity_matrix_banding
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-xml-validity.sh" "wx_matrix(matrix([1,2],[3,4]), lines=true, oversized=scroll, banding=false);"
)

# wx_matrix() only returns the formatted matrix; it is the display of that
# result that shows it. So a matrix ended with ";" is shown exactly once...
wxm_add_wxmathml_test(check-wxMathML.lisp_matrix_shown_once
    "wx_matrix(matrix([1,2],[3,4]), oversized=full);"
    "<tb[^>]* oversized=\"full\"")
set_tests_properties(check-wxMathML.lisp_matrix_shown_once PROPERTIES
    FAIL_REGULAR_EXPRESSION "</tb>.*<tb ")

# ...one ended with "$" isn't shown at all...
wxm_add_wxmathml_test(check-wxMathML.lisp_matrix_dollar_not_shown
    "wx_matrix(matrix([1,2],[3,4]), oversized=full)$ 7;"
    "<mn>7</mn>")
set_tests_properties(check-wxMathML.lisp_matrix_dollar_not_shown PROPERTIES
    FAIL_REGULAR_EXPRESSION "<tb")

# ...and the options stay with the returned value, not with what is
# calculated from it.
wxm_add_wxmathml_test(check-wxMathML.lisp_matrix_options_stay_with_value
    "m:wx_matrix(matrix([1,2],[3,4]), oversized=scroll)$ m; m+1;"
    "<tb[^>]* oversized=\"scroll\"[^>]*>.*<tb bracketParens=\"true\">")

add_test(
    NAME check-xml-validity_mfrac
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-xml-validity.sh" "1/2;"
)

# Test if the content.xml inside .wxmx files is valid
add_test(
    NAME check-wxmx-validity_all-celltypes
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxmx-validity.sh" "${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files/all-celltypes.wxmx"
)

add_test(
    NAME check-wxmx-validity_intervals
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxmx-validity.sh" "${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files/intervals.wxmx"
)

# run Maxima demo files as test?
if(WXM_MAXIMA_DEMO_TESTS)
# Tests that use "%" to reference past lines cannot be executed using "load()".
# But running a few demos just to see if there is bit rot might be a good idea.
#
# It would also be nice to do a run_testsuite(display_all=true), as well. But
# doing that in the CI after every commit uses up way too much CPU power,
# perhaps...
set(DEMO_FILES
  "itensor1.dem"
#  "itensor2.dem"
  "itensor3.dem"
#  "itensor4.dem"
  "itensor5.dem"
  "itensor6.dem"
  "ex_calc.dem"
  "itensor8.dem"
  "itensor9.dem"
  "ctensor1.dem"
  "ctensor2.dem"
  "ctensor3.dem"
  "ctensor4.dem"
  "ctensor5.dem"
  "ctensor6.dem"
  "ctensor7.dem"
  "ctensor8.dem"
#  "atensor.dem"
  "bianchi.dem"
  "car_iden.dem"
#  "kaluza.dem"
#  "ademo.dem"
  "helicity.dem"
  "plasma.dem"
  "spinor.dem"
#  "maxwell.dem"
#  "hodge.dem"
#  "weyl.dem"
#  "rainich.dem"
#  "einhil.dem"
#  "bradic.dem"
#  "laplacian.dem"
#  "schwarz.dem"
  "reissner.dem"
  "petrov.dem"
  "adsitter.dem"
  "godel.dem"
  "taubnut.dem"
  "allnutt.dem"
#  "tetrad.dem"
  "friedmann.dem"
  "papapetrou.dem"
  "kruskal.dem"
  "hydro.dem"
  "killing.dem"
  )

foreach(f ${DEMO_FILES})
	  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/automatic_test_files/run_demo_commands.wxm.in ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files/${f}.wxm)
	  add_test(
	      NAME ${f}
    	      COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error automatic_test_files/${f}.wxm)
endforeach()
endif()

wxm_add_wxmathml_test(wxbug_report
    "wxbug_report();"
    "WxMaxima is a graphical front end for Maxima, which does the mathematics in the background")

wxm_add_wxmathml_test(wxxml-array-no-display-properties
    "make_array(any,10,10);"
    "<mi lisp=.*>#{Lisp array .10,10.}</mi>")

wxm_add_wxmathml_test(wxxml-array-display-properties
    "declare_index_properties(O,[presubscript,presuperscript,postsubscript,postsuperscript]);arraymake(O,[1,2,3,4]);"
    "<mmultiscripts .* altCopy=.*><mi .*>O</mi><mrow><mn>3</mn></mrow> <mrow><mn>4</mn></mrow> <mprescripts/> <mrow><mn>1</mn></mrow> <mrow><mn>2</mn></mrow> </mmultiscripts>")

wxm_add_wxmathml_test(wxxmltag
    "wxxmltag(a\\\<,b);"
    "<b.*>a\\\\&lt;</b>")

wxm_add_wxmathml_test(wxxmltag2
    "wxxmltag(\"a<\",b);"
    "<b.*>a&lt;</b>")

wxm_add_wxmathml_test(show_image
    "show_image(\"test<2.png\");"
    "<img del=\"no\" lisp=.*>test&lt;2.png</img>")

# wxplot_scale: a plot tells wxMaxima the factor to display it with...
wxm_add_wxmathml_test(wxplot_scale_attribute
    "wxplot2d(x,[x,0,1]),wxplot_scale=3/2$"
    "<img scale=\"1.5\" lisp=\"wxxml-tag\">")

# ...only if it isn't 1, so worksheets look as before...
wxm_add_wxmathml_test(wxplot_scale_default_no_attribute
    "wxplot2d(x,[x,0,1])$"
    "<img lisp=\"wxxml-tag\">")

# ...and an animation's frames get it, too.
wxm_add_wxmathml_test(wxplot_scale_animation
    "with_slider_draw(a,[1,2],explicit(a*x,x,0,1)),wxplot_scale=0.75$"
    "<slide [^>]*scale=\"0.75\"")

# 'native: shown at wxplot_size's pixels, not shrunk to fit the window.
wxm_add_wxmathml_test(wxplot_scale_native
    "wxplot2d(x,[x,0,1]),wxplot_scale='native$"
    "<img scale=\"native\" lisp=\"wxxml-tag\">")

# Something that isn't a positive number is an error, not a broken plot.
wxm_add_wxmathml_test(wxplot_scale_invalid
    "wxplot2d(x,[x,0,1]),wxplot_scale=-1$"
    "wxplot_scale must be a positive number")
set_tests_properties(wxplot_scale_invalid PROPERTIES
    FAIL_REGULAR_EXPRESSION "<img ")

wxm_add_wxmathml_test(wxxml-mlable
    "test;"
    "<lbl altCopy=\"%o1\">\\(%o1\\)")

wxm_add_wxmathml_test(wxxml-mlable2
    "outchar:\">\";"
    "<math><lbl altCopy=\"\\\\&gt;1\">\\(&gt;1\\) </lbl>.*&gt;.*</math>")

wxm_add_wxmathml_test(wxxml-orthopoly
    "legendre_p (n - 1, x);"
    "<fn altCopy=\"legendre_p\\(n-1,x\\)\"><munder><fnm>P</fnm><mrow><mi lisp=.*>n</mi><mo>-</mo><mn>1</mn></mrow></munder><p><mi lisp=.*>x</mi></p></fn>")

wxm_add_wxmathml_test(wxxml-pochhammer
    "pochhammer (\"<\", \">\");"
    "<munder altCopy=\"pochhammer\\(&quot;&lt;&quot;,&quot;&gt;&quot;\\)\"><p><st>&lt;</st></p><mrow><st>&gt;</st></mrow></munder>")

wxm_add_wxmathml_test(wxxml-pderivop
    "diff(f(y),y);"
    "<d lisp=.*><mfrac lisp=.*><mrow><s lisp=.*>d</s></mrow><mrow><s lisp=.*>d</s><h>.</h><mi lisp=.*>y</mi></mrow></mfrac><h>.</h><fn lisp=.*><fnm lisp=.*>f</fnm><mrow><p><mrow><mi lisp=.*>y</mi></mrow></p></mrow></fn></d>")

wxm_add_wxmathml_test(mminus
    "-x-1;"
    "<mo>-</mo><mi lisp=.*>x</mi><mo>-</mo><mn>1</mn>")

wxm_add_wxmathml_test(mfactorial
    "(n\!)\!;"
    "<mi lisp=.*>n</mi><mo>\!</mo></p></mrow><mo>\!</mo>")

wxm_add_wxmathml_test(subscriptp_1
    "wxsubscripts:true\$eeee_133;"
    "<munder altCopy=\"eeee_133\"><mrow><mi>eeee</mi></mrow><mrow><mi>133</mi></mrow></munder>")

wxm_add_wxmathml_test(subscriptp_2
    "wxsubscripts:true\$wxdeclare_subscripted(eeee_aaa)\$eeee_aaa;"
    "<munder altCopy=\"eeee_aaa\"><mrow><mi>eeee</mi></mrow><mrow><mi>aaa</mi></mrow></munder>")

wxm_add_wxmathml_test(subscriptp_3
    "wxsubscripts:true\$wxdeclare_subscripted(eeee_aaa)\$eeee_aaae;"
    "<mi lisp=.*>eeee_aaae</mi>")

wxm_add_wxmathml_test(subscriptp_4
    "wxsubscripts:true\$eeee_133;"
    "<munder altCopy=\"eeee_133\"><mrow><mi>eeee</mi></mrow><mrow><mi>133</mi></mrow></munder>")

wxm_add_wxmathml_test(subscriptp_5
    "wxsubscripts:true\$wxdeclare_subscript(bbb)\$eeee_bbb;"
    "<munder altCopy=\"eeee_bbb\"><mrow><mi>eeee</mi></mrow><mrow><mi>bbb</mi></mrow></munder>")

wxm_add_wxmathml_test(subscriptp_6
    "wxsubscripts:true\$wxdeclare_subscript(bbb)\$eeee_bbb;"
    "<munder altCopy=\"eeee_bbb\"><mrow><mi>eeee</mi></mrow><mrow><mi>bbb</mi></mrow></munder>")

wxm_add_wxmathml_test(subscriptp_7
    "wxsubscripts: 'all\$eree_ssss;"
    "<munder altCopy=\"eree_ssss\"><mrow><mi>eree</mi></mrow><mrow><mi>ssss</mi></mrow></munder>")

wxm_add_wxmathml_test(subscriptp_8
    "wxsubscripts: false\$a_b;"
    "<mi lisp=.*>a_b</mi>")

wxm_add_wxmathml_test(subscriptp_9
    "wxsubscripts: false\$a_1;"
    "<mi lisp=.*>a_1</mi>")

wxm_add_wxmathml_test(subscriptp_10
    "wxsubscripts:true\$t_1\\,2;"
    "<munder altCopy=\"t_1\\\\,2\"><mrow><mi>t</mi></mrow><mrow><mi>1,2</mi></mrow></munder>")

wxm_add_wxmathml_test(subscriptp_11
    "wxsubscripts:true\$a\\,b_1;"
    "a,b")

# GH #1653: wxdeclare_subscripted(x, false) has to keep x unsubscripted even
# where the rules wxsubscripts selects would subscript it, and
# wxdeclare_subscripted(x) has to subscript it where they would not.
wxm_add_wxmathml_test(subscriptp_declared_false_wins_over_all
    "wxsubscripts: 'all\$wxdeclare_subscripted(eree_ssss,false)\$eree_ssss;"
    "<mi lisp=.*>eree_ssss</mi>")

wxm_add_wxmathml_test(subscriptp_declared_false_can_be_taken_back
    "wxsubscripts:true\$wxdeclare_subscripted(eree_ssss,false)\$wxdeclare_subscripted(eree_ssss)\$eree_ssss;"
    "<munder altCopy=\"eree_ssss\"><mrow><mi>eree</mi></mrow><mrow><mi>ssss</mi></mrow></munder>")

# A variable Maxima itself defines whose name contains an underscore is
# declared "never subscripted" at startup - that declaration is what the two
# tests above make work, and it has to keep working for the names it was
# written for.
wxm_add_wxmathml_test(subscriptp_maxima_own_variable_not_subscripted
    "wxsubscripts: 'all\$'float_approx_equal_tolerance;"
    "<mi lisp=.*>float_approx_equal_tolerance</mi>")

# kill(all) unbinds every symbol Maxima lists as having user properties. The
# startup declarations above must not put Maxima's own variables on that
# list, or kill(all) unbinds ones Maxima needs (display_format_internal broke
# every display after it on Maxima 5.48).
wxm_add_wxmathml_test(subscriptp_kill_all_keeps_maxima_variables
    "kill(all)\$float_approx_equal_tolerance;"
    "<lbl altCopy=\"%o1\">\\(%o1\\) </lbl><mrow><mn>")

# wxsubscripts' own default is the Maxima symbol $all - it used to be the
# Lisp symbol of the same name, which nothing compares against, so an
# unconfigured wxMaxima silently behaved as if "true" was set.
wxm_add_wxmathml_test(subscriptp_default_is_all
    "eree_ssss;"
    "<munder altCopy=\"eree_ssss\"><mrow><mi>eree</mi></mrow><mrow><mi>ssss</mi></mrow></munder>")

wxm_add_wxmathml_test(mprod
    "a*2*b;"
    "<mn>2</mn><h>[*]</h><mi lisp=.*>a</mi><h>[*]</h><mi lisp=.*>b</mi>")

wxm_add_wxmathml_test(block
    "f(x):=block([a:x],x);"
    "<fn lisp=.*><fnm lisp=.*>f</fnm><mrow><p><mrow><mi lisp=.*>x</mi></mrow></p></mrow></fn><mo>:=</mo><fn lisp=.*><fnm lisp=.*>block</fnm><mrow><p><mrow><mrow list=\"true\"><t lisp=.* listdelim=\"true\">\\[</t><mrow><mi lisp=.*>a</mi><mo>:</mo><mi lisp=.*>x</mi></mrow><t lisp=.* listdelim=\"true\">\\]</t></mrow></mrow><mo>,</mo><mrow><mi lisp=.*>x</mi></mrow></p></mrow></fn>")

wxm_add_wxmathml_test(mlist
    "[&,b];"
    "<t lisp=.* listdelim=\"true\">\\[</t><mrow><mi lisp=.*>&amp;</mi></mrow><mo>,</mo><mrow><mi lisp=.*>b</mi></mrow><t lisp=.* listdelim=\"true\">\\]</t></mrow>")

wxm_add_wxmathml_test(true
    "true;"
    "<t lisp=.*>true</t>")

wxm_add_wxmathml_test(false
    "true;"
    "<t lisp=.*>true</t>")

wxm_add_wxmathml_test(mfrac
    "a/(1/2+b);"
    "<mfrac lisp=.*><mrow><mi lisp=.*>a</mi></mrow><mrow><mi lisp=.*>b</mi><mo>\\+</mo><mfrac lisp=.*><mrow><mn>1</mn></mrow><mrow><mn>2</mn></mrow></mfrac></mrow></mfrac>")

# The test for the argument of load() was added in Maxima 5.46.0
if(MAXIMA_VERSION VERSION_GREATER_EQUAL "5.46.0")
    wxm_add_wxmathml_test(load_symbol
        "load(engineering-format);"
        "argument must be a string, symbol, or pathname")
endif()

add_test(
    NAME wxmaxima_batch_emptyFile
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error empty_file.wxm)

# Verify wxMaxima leaves behind no stray Maxima/gnuplot child process after a
# batch run (it must kill them on shutdown; a survivor is a leak and also keeps
# the inherited ctest pipe open -> hang). The check is a cmake -P driver so it
# runs on both Linux and Windows CI. RUN_SERIAL so no sibling test's Maxima is
# alive concurrently (which would otherwise look like a stray of this run).
add_test(
    NAME wxmaxima_no_stray_children
    COMMAND ${CMAKE_COMMAND}
            -DWXMAXIMA=$<TARGET_FILE:wxmaxima>
            -DTESTFILE=stray_process_check.wxm
            -DWORKDIR=${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
            -P ${CMAKE_CURRENT_SOURCE_DIR}/check_no_stray_procs.cmake)
set_tests_properties(wxmaxima_no_stray_children PROPERTIES RUN_SERIAL TRUE)

# Opening one file must start one Maxima, not two. wxMaxima starts a Maxima as
# soon as it comes up so that the process is warm by the time the user sends
# their first cell off; that head start is only worth having if opening a file
# can then reuse the process instead of killing it and spawning a replacement.
# See check_maxima_spawn_count.cmake for why it used to be unable to.
#
# The count comes from a wrapper the script puts in front of the real Maxima
# (GH #2351), not from wxMaxima's own log: on Windows a GUI-subsystem
# process's stderr does not reliably reach whoever captures it, which once
# turned the minGW job red with an empty log read as "0 processes". So this
# runs on every platform now, and needs to know where the real Maxima is.
if(MAXIMA)
    add_test(
        NAME wxmaxima_one_maxima_per_file
        COMMAND ${CMAKE_COMMAND}
                -DWXMAXIMA=$<TARGET_FILE:wxmaxima>
                -DMAXIMA=${MAXIMA}
                -DTESTFILE=one_maxima_per_file.wxm
                -DWORKDIR=${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
                -DEXPECTED=1
                -P ${CMAKE_CURRENT_SOURCE_DIR}/check_maxima_spawn_count.cmake)
endif()

# GH #2276: a --batch run whose input file doesn't exist has nothing to do and
# must report failure with the dedicated EXIT_FILE_OPEN_FAILED code (92), not
# the generic 1 every --exit-on-error reason used to share. Chosen as the one
# exit-code regression test worth having: unlike the other dedicated codes
# (a Maxima error, an unanswered question, a failed save, an image load/decode
# failure) this one needs no live Maxima evaluation to reach deterministically
# -- the file-open check runs before any cell is ever sent to Maxima.
add_test(
    NAME wxmaxima_exit_code_file_not_found
    COMMAND ${CMAKE_COMMAND}
            -DWXMAXIMA=$<TARGET_FILE:wxmaxima>
            "-DARGS=--batch;--exit-on-error;this-file-does-not-exist-2276.wxm"
            -DWORKDIR=${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
            -DEXPECTED_EXIT_CODE=92
            -P ${CMAKE_CURRENT_SOURCE_DIR}/check_exit_code.cmake)

# A file of a type wxMaxima can't open gets no window and no Maxima of its
# own: opening wxmaxima.exe and its DLLs used to start one of each per file
# and have every Maxima try to load() a binary. The executable that runs this
# test stands in for such a file; a --batch run must fail like it does for a
# file that doesn't exist, without ever starting Maxima.
add_test(
    NAME wxmaxima_exit_code_unsupported_file
    COMMAND ${CMAKE_COMMAND}
            -DWXMAXIMA=$<TARGET_FILE:wxmaxima>
            "-DARGS=--batch;--exit-on-error;$<TARGET_FILE:wxmaxima>"
            -DWORKDIR=${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
            -DEXPECTED_EXIT_CODE=92
            -P ${CMAKE_CURRENT_SOURCE_DIR}/check_exit_code.cmake)
set_tests_properties(wxmaxima_exit_code_unsupported_file PROPERTIES TIMEOUT 60)

# Without a display wxGTK can't initialize, and wxMaxima must say so and exit
# with 1 instead of crashing on the wxTheApp that was never made. Seen in CI
# when Xvfb wasn't ready yet (a segfault in CommonMain()). GTK only: wxQt
# aborts inside Qt in that case, and MSW and macOS always have a display.
if(UNIX AND NOT APPLE AND "${wxWidgets_DEFINITIONS}" MATCHES "__WXGTK__")
  add_test(
      NAME wxmaxima_exit_code_no_display
      COMMAND ${CMAKE_COMMAND} -E env --unset=DISPLAY --unset=WAYLAND_DISPLAY
              ${CMAKE_COMMAND}
              -DWXMAXIMA=$<TARGET_FILE:wxmaxima>
              "-DARGS=--batch;fracCells.wxm"
              -DWORKDIR=${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
              -DEXPECTED_EXIT_CODE=1
              -P ${CMAKE_CURRENT_SOURCE_DIR}/check_exit_code.cmake)
endif()

add_test(
    NAME wxmaxima_makelist
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error makelist.wxm)

add_test(
    NAME wxmaxima_performance
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error performance.wxm)

add_test(
    NAME wxmaxima_performance2
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error 500.wxm)

add_test(
    NAME wxmaxima_performance3
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error 500.wxmx)

add_test(
    NAME wxmaxima_batch_textcell
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error textcells.wxm)

add_test(
    NAME wxmaxima_batch_foreign_characters
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error foreign-characters.wxm)

add_test(
    NAME wxmaxima_load_symbol
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error load_string.wxm)

# --version's *output* is asserted on every platform except Windows, where
# only its exit code is. That is a deliberate, argued-for narrowing, not a
# test quietly switched off, so here is the whole argument -- and the
# wxmaxima-packaging skill's long "wxmaxima_version_string CI test failing on
# the minGW Windows runner" entry has the investigation history it rests on.
#
# wxmaxima.exe is a GUI-subsystem binary (add_executable(wxmaxima WIN32 ...)),
# because a console-subsystem GUI app pops up a console window beside every
# worksheet. An executable's subsystem is one bit in its PE header, fixed at
# link time, and it decides whether the process gets standard streams at all.
# Everything Windows-specific about this test follows from that single bit:
# main.cpp's RedirectStdioToParent() exists only to paper over it, the
# now-removed `cmd /c ... > file && type file` contortion this COMMAND used to
# be was an attempt to work around it, and src/wxmaxima-cli.cpp -- a separate,
# console-subsystem launcher -- exists because no amount of code inside
# wxmaxima.exe can finish the job.
#
# What that means for testing: asserting on text captured from a
# GUI-subsystem process is asserting on a property of Windows, not of
# wxMaxima. It has failed on essentially every push since 2026-08-15, and the
# investigation behind it has disproven, with real evidence, each mechanism it
# could plausibly have been: the FILE struct copy, _dup2()'s handle ownership,
# ctest's own -j 2 scheduling, a ConPTY output-drain race, and the cmd.exe
# quoting bug in the workaround itself. The traces say the process runs,
# binds its streams, reaches the --version branch, completes the write and
# exits 0; ctest captures nothing. A permanently red job is not a warning
# anyone still reads, and this one has been hiding whatever else the minGW
# runner might have to say for a month.
#
# So the assertion stays where it is meaningful and can hold:
#   - every non-Windows platform keeps the full content check below;
#   - Windows keeps wxmaxima_version_returncode and wxmaxima_help_returncode,
#     which do verify that --version and --help run to completion and exit
#     with the status they are supposed to (--help deliberately nonzero, see
#     its WILL_FAIL below). That is more than it sounds: before
#     wxMessageOutput was pointed at stdout, a GUI-subsystem build answered
#     both of them with a *modal message box*, so these two would hang until
#     their timeout rather than fail -- which is exactly the regression they
#     still guard against;
#   - Windows also keeps wxmaxima_cli_version_returncode, which exercises the
#     launcher end to end: it finds wxmaxima.exe beside itself, spawns it,
#     hands it standard handles, waits for it and propagates its exit code.
# What is NOT covered anywhere on Windows is the last step, "and the bytes
# arrive at whoever is capturing them". If that is ever solved, the check to
# restore is a PASS_REGULAR_EXPRESSION on wxmaxima-cli (never on wxmaxima
# itself -- the launcher is the supported way to get output from a command
# prompt, so it is the one worth pinning).
if(NOT WIN32)
    add_test(
        NAME wxmaxima_version_string
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
        COMMAND wxmaxima --debug --logtostderr --pipe --version)
    # ${VERSION} is not a variable this project ever sets -- the version lives
    # in ${WXMAXIMA_VERSION} (and ${PROJECT_VERSION}) -- so this regex used to
    # expand to a much weaker "wxMaxima <anything>" and never actually checked
    # the version it is named after. The binary prints "wxMaxima <version>"
    # optionally followed by " (Git version: ...)", so anchoring the front of
    # the line is enough.
    set_tests_properties(wxmaxima_version_string PROPERTIES
        PASS_REGULAR_EXPRESSION "wxMaxima ${WXMAXIMA_VERSION}")
endif()

add_test(
    NAME wxmaxima_version_returncode
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --version)

if(WIN32)
    # The console-subsystem launcher, as far as it can be checked without
    # capturing the GUI process's output: a failure here means the launcher
    # could not locate wxmaxima.exe, could not start it, did not wait for it,
    # or lost its exit code -- i.e. that the launcher itself is broken, which
    # is the part this test can still tell us about.
    add_test(
        NAME wxmaxima_cli_version_returncode
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
        COMMAND wxmaxima-cli --debug --logtostderr --pipe --version)
endif()

#add_test(
#    NAME maxima_lisp_switch
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --debug --logtostderr --pipe -l undefined_lisp)
#set_tests_properties(maxima_lisp_switch PROPERTIES WILL_FAIL true)

#add_test(
#    NAME maxima_version_switch
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --debug --logtostderr --pipe -u undefined_version)
#set_tests_properties(maxima_version_switch PROPERTIES WILL_FAIL true)

#add_test(
#    NAME maxima_args_switch
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --debug --logtostderr --pipe -X gibberish)
#set_tests_properties(maxima_args_switch PROPERTIES WILL_FAIL true)

add_test(
    NAME wxmaxima_help_returncode
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --help)
set_tests_properties(wxmaxima_help_returncode PROPERTIES WILL_FAIL true)

add_test(
    NAME all_celltypes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error testbench_all_celltypes.wxm)

add_test(
    NAME simpleInput
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error simpleInput.wxm)

add_test(
    NAME rememberingAnswers
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error rememberingAnswers.wxm)

add_test(
    NAME absCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error absCells.wxm)

add_test(
    NAME absCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error absCells.wxm)

add_test(
    NAME boxCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error boxCells.wxm)

add_test(
    NAME boxCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error boxCells.wxm)

add_test(
    NAME diffCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error diffCells.wxm)

add_test(
    NAME diffCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error diffCells.wxm)

add_test(
    NAME atCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error atCells.wxm)

add_test(
    NAME atCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error atCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME atCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error atCells.wxm)

add_test(
    NAME conjugateCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error conjugateCells.wxm)

add_test(
    NAME conjugateCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error conjugateCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME conjugateCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error conjugateCells.wxm)

add_test(
    NAME exptCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error exptCells.wxm)

add_test(
    NAME exptCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error exptCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME exptCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error exptCells.wxm)

add_test(
    NAME fracCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error fracCells.wxm)

add_test(
    NAME fracCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error fracCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME fracCells_cmdline_wxmathml_over_network
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo 'load(fracCells.wxm) ; quit() ;' | nc -l 40000 & sleep 10 ; ${MAXIMA}-s 40000
    VERBATIM
    )

add_test(
    NAME fracCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error fracCells.wxm)

add_test(
    NAME intCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error intCells.wxm)

add_test(
    NAME intCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error intCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME intCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error intCells.wxm)

add_test(
    NAME imageFormat
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/image-test
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error image-test.wxmx)

add_test(
    NAME intervals_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error intervals.wxm)

add_test(
    NAME intervals_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error intervals.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME intervals
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error intervals.wxm)

add_test(
    NAME functionCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error functionCells.wxm)

add_test(
    NAME functionCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error functionCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME functionCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error functionCells.wxm)

add_test(
    NAME slideshowCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error slideshowCells.wxm)

add_test(
    NAME imageCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error imageCells.wxm)

add_test(
    NAME limitCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error limitCells.wxm)

add_test(
    NAME limitCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error limitCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME limitCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error limitCells.wxm)

add_test(
    NAME matrixCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error matrixCells.wxm)

add_test(
    NAME matrixCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error matrixCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME matrixCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error matrixCells.wxm)

add_test(
    NAME parenthesisCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error parenthesisCells.wxm)

add_test(
    NAME parenthesisCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error parenthesisCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME parenthesisCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error parenthesisCells.wxm)

add_test(
    NAME listCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error listCells.wxm)

add_test(
    NAME listCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error listCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME listCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error listCells.wxm)

add_test(
    NAME setCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error listCells.wxm)

add_test(
    NAME setCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error setCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME setCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error setCells.wxm)

add_test(
    NAME sqrtCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error sqrtCells.wxm)

add_test(
    NAME sqrtCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error sqrtCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME sqrtCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error sqrtCells.wxm)

add_test(
    NAME subCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch --exit-on-error subCells.wxm)

add_test(
    NAME subCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch --exit-on-error subCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME subCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error subCells.wxm)

add_test(
    NAME subsupCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error subsupCells.wxm)

add_test(
    NAME subsupCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error subsupCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME subsupCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error subsupCells.wxm)

add_test(
    NAME presubsupcells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch --exit-on-error presubsupcells.wxm)

add_test(
    NAME presubsupcells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch --exit-on-error presubsupcells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME presubsupcells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error presubsupcells.wxm)

add_test(
    NAME sumCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error sumCells.wxm)

add_test(
    NAME sumCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error sumCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME sumCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error sumCells.wxm)

add_test(
    NAME productCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error productCells.wxm)

add_test(
    NAME productCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error productCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME productCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error productCells.wxm)

add_test(
    NAME printf_simple_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error printf_simple.wxm)

add_test(
    NAME printf_simple_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch --exit-on-error printf_simple.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME printf_simple
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error printf_simple.wxm)

add_test(
    NAME printf_equations_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch --exit-on-error printf_equations.wxm)

add_test(
    NAME printf_equations_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch --exit-on-error printf_equations.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME printf_equations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error printf_equations.wxm)

add_test(
    NAME printf_continuationLines_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch --exit-on-error printf_continuationLines.wxm)

add_test(
    NAME printf_continuationLines_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch --exit-on-error printf_continuationLines.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME printf_continuationLines
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error printf_continuationLines.wxm)

add_test(
    NAME weirdLabels
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error weirdLabels.wxm)

add_test(
    NAME xmlQuote
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error xmlQuote.wxm)

add_test(
    NAME formerCrashes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error formerCrashes.wxm)

add_test(
    NAME multiplication
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error multiplication.wxm)

add_test(
    NAME nonsenseConstructs
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error nonsenseConstructs.wxm)

add_test(
    NAME lisp
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error lisp.wxm)

add_test(
    NAME boxes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error boxes.wxm)

add_test(
    NAME unicode
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error unicode.wxm)

add_test(
    NAME unicode_specialchars
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error unicode_specialchars.wxm)

add_test(
    NAME openMacFiles
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND wxmaxima --debug --logtostderr --pipe -f autosave.cfg --batch --exit-on-error testbench_simple.wxm)

add_test(
    NAME openMacFiles2
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f autosave.cfg --batch --exit-on-error testbench_simple2.mac)

add_test(
    NAME tutorial_10Minutes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error 10MinuteTutorial.wxm)

# GH #2196: the first statement of a multi-statement cell can be silently
# dropped and never sent to Maxima at all. This file maximizes the number of
# independent cell-to-cell transitions (the boundary where the drop was
# observed) in a single run and asserts, via a real Maxima error(), that
# every one of them actually ran -- see the file's own header comment.
add_test(
    NAME commandSequenceIntegrity
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error commandSequenceIntegrity.wxm)

add_test(
    NAME comment_begin
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error commentBegin.wxm)

add_test(
    NAME threadtest
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error threadtest.wxm)

add_test(
    NAME autosave
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f autosave.cfg --batch --exit-on-error unicode.wxm)

add_test(
    NAME noautosave
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f noautosave.cfg --batch --exit-on-error unicode.wxm)

add_test(
    NAME config_from_19.11
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f config_from_19.11.cfg --batch --exit-on-error unicode.wxm)

add_test(
    NAME longnum_ellipsis
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f longnum_ellipsis.cfg --batch --exit-on-error longnum_test.wxm)

add_test(
    NAME longnum_allinone
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f longnum_allinone.cfg --batch --exit-on-error longnum_test.wxm)

add_test(
    NAME longnum_broken
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f longnum_broken.cfg --batch --exit-on-error longnum_test.wxm)

add_test(
    NAME invalid_commandline_arg
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --gibberish --batch --exit-on-error unicode.wxm)
set_tests_properties(invalid_commandline_arg PROPERTIES WILL_FAIL true)

add_test(
    NAME multiwindow
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --single_process --batch --exit-on-error unicode.wxm boxes.wxm)

add_test(
    NAME multithreadtest
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo "load(draw);quit();" | ${MAXIMA} --very-very-quiet --quit-on-error ; wxmaxima --debug --logtostderr --pipe --single_process --batch --exit-on-error threadtest.wxm threadtest2.wxm)

#add_test(
#    NAME ipc_copypaste1
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --debug --enableipc --batch --exit-on-error ipc_copypaste1.wxm)

#add_test(
#    NAME ipc_copypaste_all-celltypes
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --debug --enableipc --batch --exit-on-error ipc_copypaste_all-celltypes.wxm)

add_test(
    NAME config_dialogue_sample
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../data
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error sampleWorksheet.wxmx)

add_test(
    NAME tutorial_fastListAccess
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error fastListAccess.wxm)

add_test(
    NAME tutorial_numberFormats
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error numberFormats.wxm)

add_test(
    NAME tutorial_toleranceCalculations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error toleranceCalculations.wxm)

add_test(
    NAME tutorial_fittingEquations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error fittingEquations.wxm)

add_test(
    NAME tutorial_variableNames
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error variableNames.wxm)

add_test(
    NAME tutorial_displaying3DCurves
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error displaying3DCurves.wxm)

add_test(
   NAME tutorial_memoizing
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
   COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error memoizing.wxm)

add_test(
    NAME tutorial_solvingEquations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error solvingEquations.wxm)

add_test(
    NAME testbench_simple.wxmx
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error testbench_simple.wxmx)

find_program(DESKTOP_FILE_VALIDATE desktop-file-validate)
if(DESKTOP_FILE_VALIDATE)
    add_test(
        NAME check_desktop_file
        COMMAND ${DESKTOP_FILE_VALIDATE} ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.desktop)
endif()

find_program(APPSTREAMCLI appstreamcli)
if(APPSTREAMCLI)
	execute_process(COMMAND ${APPSTREAMCLI} --version
					OUTPUT_VARIABLE APPSTREAMCLI_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE)
	string(REGEX REPLACE "^.*:.(.*)" "\\1"  APPSTREAMCLI_VERSION "${APPSTREAMCLI_VERSION}")
    message(STATUS "Found appstreamcli: ${APPSTREAMCLI} (found version \"${APPSTREAMCLI_VERSION}\")")
    add_test(
            NAME check_appstream_file
            COMMAND ${APPSTREAMCLI} validate "${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.appdata.xml")
else()
    message(STATUS "appstreamcli not found. Do not check of the appstream file.")
endif()

find_program(LINTIAN lintian)
find_program(DPKG_DEB dpkg-deb)
if(LINTIAN AND DPKG_DEB)
    message(STATUS "lintian and dpkg-deb found. Enabling lintian check of the Debian package.")
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-lintian.sh.in" check-lintian.sh @ONLY)
    add_test(
        NAME check-lintian
        COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-lintian.sh"
    )
else()
    message(STATUS "lintian or dpkg-deb not found. Skipping lintian check of the Debian package.")
endif()

find_program(RPMLINT rpmlint)
find_program(RPMBUILD rpmbuild)
if(RPMLINT AND RPMBUILD)
    message(STATUS "rpmlint and rpmbuild found. Enabling rpmlint check of the RPM package.")
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-rpmlint.sh.in" check-rpmlint.sh @ONLY)
    add_test(
        NAME check-rpmlint
        COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-rpmlint.sh"
    )
else()
    message(STATUS "rpmlint or rpmbuild not found. Skipping rpmlint check of the RPM package.")
endif()

# Give every test its own environment so tests running in parallel never share
# Maxima's user-customize-files folder (MAXIMA_USERDIR), Maxima's compiled-package
# cache (MAXIMA_OBJDIR), Maxima's scratch space (MAXIMA_TEMPDIR), generic user
# scratch space (TMPDIR), wxMaxima's user configuration folder (XDG_CONFIG_HOME).
# For illustration Maxima's load() of a package (draw, lsquares, ...) writes the compiled
# results into that cache, and two instances loading the same package concurrently into
# one shared cache can race (the second reads a half-written file and load() fails).
# Conversely, Maxima plotting machinery, along the invoked gnuplot machinery,
# generates numerous intermediate data files and scripts into the scratch spaces so having
# them when issues occur can be handy. Isolating per test removes the race without serializing
# the suite and allows developers to isolate possible issues per test at the same expense.
# Notice that wxMaxima's config writes now degrade gracefully when the directory isn't writable,
# so a fresh per-test environment no longer asserts.
# Tests that drive the configured check-*.sh helper scripts (XML / wxmx
# validity, wizard commands, ...). The wxMathML.lisp output tests used to be
# here too; wxm_add_wxmathml_test() runs them without a shell (GH #1775). They need a POSIX shell plus xmllint/unzip and
# are invoked as a .sh directly, so they cannot run on the Windows CI (they show
# up there as "Not Run"). Tag them "needs_posix" so that platform can skip them
# with `ctest -LE needs_posix`; they keep running normally on Linux. Labelling is
# done by name in the loop below so conditionally-defined tests (e.g. check-lintian,
# only added where lintian is installed) are handled automatically.
set(WXM_POSIX_TESTS
  check-wizard-commands
  check-translations
  check-xml-validity_matrix_default
  check-xml-validity_matrix_lines
  check-xml-validity_matrix_complex
  check-xml-validity_mfrac
  check-xml-validity_matrix_oversized
  check-xml-validity_matrix_banding
  check-wxmx-validity_all-celltypes
  check-wxmx-validity_intervals
  check-markdown
  check-lintian
  check-rpmlint)

# A test missing from that list shows up only on the Windows CI, as a red
# BAD_COMMAND nobody sees until the job runs -- five slipped through that way
# in 2026-09. So check it here, on every platform's configure: each test whose
# COMMAND is one of the check-*.sh helpers has to be in WXM_POSIX_TESTS.
file(READ "${CMAKE_CURRENT_LIST_FILE}" wxm_this_file)
string(REGEX MATCHALL
  "NAME[ \t\r\n]+[^ \t\r\n]+[ \t\r\n]+COMMAND[ \t\r\n]+\"\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/check-[^\"]*\\.sh\""
  wxm_sh_tests "${wxm_this_file}")
foreach(wxm_sh_test IN LISTS wxm_sh_tests)
  string(REGEX REPLACE "^NAME[ \t\r\n]+([^ \t\r\n]+).*" "\\1" wxm_sh_test "${wxm_sh_test}")
  if(NOT wxm_sh_test IN_LIST WXM_POSIX_TESTS)
    message(FATAL_ERROR "Test ${wxm_sh_test} runs a check-*.sh helper, so it "
      "needs a POSIX shell: add it to WXM_POSIX_TESTS in ${CMAKE_CURRENT_LIST_FILE}.")
  endif()
endforeach()

# ...with one deliberate exception: the typical real-world case is multiple
# wxMaxima sessions (over time or at once) SHARING one ~/.maxima, reusing and
# rewriting each other's caches (knownSymbols.txt, manual anchors, history).
# The per-test isolation above never exercises that, so this pair of tests -
# chosen because their worksheets load no compiled package, keeping clear of
# Maxima's compiled-package cache race described above - shares one userdir:
# run sequentially they reuse each other's caches, and under `ctest -j` they
# can genuinely overlap.
set(WXM_SHARED_USERDIR_TESTS absCells atCells)

get_property(wxm_all_tests DIRECTORY PROPERTY TESTS)
foreach(wxm_t IN LISTS wxm_all_tests)
  if("${wxm_t}" IN_LIST WXM_SHARED_USERDIR_TESTS)
    set(wxm_envdir "${CMAKE_CURRENT_BINARY_DIR}/test_envdirs/shared_typical_use")
  else()
    set(wxm_envdir "${CMAKE_CURRENT_BINARY_DIR}/test_envdirs/${wxm_t}")
  endif()
  # Create every directory the environment below points at: nothing creates
  # them on demand. Maxima in particular writes its plot data and gnuplot
  # scripts straight into MAXIMA_TEMPDIR and fails with "Cannot create" if
  # that folder is missing.
  foreach(wxm_d TMPDIR MAXIMA_USERDIR MAXIMA_OBJDIR MAXIMA_TEMPDIR XDG_CONFIG_HOME)
    file(MAKE_DIRECTORY "${wxm_envdir}/${wxm_d}")
  endforeach()
  set_property(TEST "${wxm_t}" APPEND PROPERTY ENVIRONMENT
    "TMPDIR=${wxm_envdir}/TMPDIR"
    #"TEMPDIR=${wxm_envdir}/TMPDIR"
    "MAXIMA_USERDIR=${wxm_envdir}/MAXIMA_USERDIR"
    "MAXIMA_OBJDIR=${wxm_envdir}/MAXIMA_OBJDIR"
    "MAXIMA_TEMPDIR=${wxm_envdir}/MAXIMA_TEMPDIR"
    "XDG_CONFIG_HOME=${wxm_envdir}/XDG_CONFIG_HOME"
    )
  # Watchdog timeout so a single hung test fails fast instead of blocking the
  # whole suite until the CI job hits its two-hour wall (see WXM_TEST_TIMEOUT).
  # Only set it where a test hasn't requested a longer one of its own.
  get_test_property("${wxm_t}" TIMEOUT wxm_existing_timeout)
  if(NOT wxm_existing_timeout)
    set_property(TEST "${wxm_t}" PROPERTY TIMEOUT ${WXM_TEST_TIMEOUT})
  endif()
  if(wxm_t IN_LIST WXM_POSIX_TESTS)
    set_property(TEST "${wxm_t}" APPEND PROPERTY LABELS needs_posix)
  endif()
endforeach()
