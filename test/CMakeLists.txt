# -*- mode: CMake; cmake-tab-width: 4; -*-

# Compile a list of test files
file(GLOB IMAGE_FILES automatic_test_files/*.png automatic_test_files/*.gif)
file(GLOB TEST_FILES ${IMAGE_FILES} automatic_test_files/*.wxmx automatic_test_files/*.wxm automatic_test_files/*.mac automatic_test_files/*.cfg)

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
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-wxMathML.sh.in" check-wxMathML.sh)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-xml-validity.sh.in" check-xml-validity.sh)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-wxmx-validity.sh.in" check-wxmx-validity.sh)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-wizard-commands.sh.in" check-wizard-commands.sh)

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

# 2. Try Weston (Wayland)
if command -v weston >/dev/null 2>&1; then
    export GDK_BACKEND=wayland
    echo \"Attempting headless Weston...\" >&2
    # Use exec to ensure signals from ctest reach the process and it's dead on timeout
    exec weston --backend=headless -- \"$@\"
fi

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
    testbench_simple.wxmx
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_WRITE OWNER_READ
)

file(
    COPY
    testbench_simple.mac
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


set(CTEST_TEST_TIMEOUT 120)

if(WXM_UNIT_TESTS)
    add_subdirectory(unit_tests)
endif()

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

# A (very simple) test, if wxMathML.lisp file works (creates the expected XML output).
add_test(
    NAME check-wxMathML.lisp_1
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "1+1;" "<math><lbl altCopy=\"%o1\">(%o1) </lbl><mn>2</mn></math>"
)

# Syntax-check every wizard/menu command template (the "#n#" CommandWiz rules in
# src/wxMaxima.cpp) through Maxima's parser, so a malformed generated command --
# e.g. an unbalanced parenthesis -- fails the build instead of only being found
# by a user.
add_test(
    NAME check-wizard-commands
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wizard-commands.sh"
)

add_test(
    NAME multiple_batch_files
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch --exit-on-error unicode.wxm boxes.wxm)

add_test(
    NAME multiple_batch_files_inverse
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch --exit-on-error boxes.wxm unicode.wxm)

# Smoke test for the side-by-side diff viewer (wxmxdiff / --diff): it has no
# other test and has broken silently when surrounding code changed. Loads two
# .wxmx files in diff mode and fails on a crash or assertion.
add_test(
    NAME diff_viewer_smoke
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND sh ${CMAKE_CURRENT_SOURCE_DIR}/diff_smoke_test.sh $<TARGET_FILE:wxmaxima> all-celltypes.wxmx intervals.wxmx)

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

add_test(
    NAME wxstatusbar
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxstatusbar(\"a<1>b\");" "<statusbar>a&lt;1&gt;b</statusbar>"
)

add_test(
    NAME wxbuildinfo
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxbuild_info();" "WxMaxima"
)

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

add_test(
    NAME wxbug_report
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxbug_report();" "WxMaxima is a graphical front end for Maxima, which does the mathematics in the background"
)

add_test(
    NAME wxxml-array-no-display-properties
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "make_array(any,10,10);" "<mi lisp=.*>#{Lisp array .10,10.}</mi>"
)

add_test(
    NAME wxxml-array-display-properties
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "declare_index_properties(O,[presubscript,presuperscript,postsubscript,postsuperscript]);arraymake(O,[1,2,3,4]);" "<mmultiscripts .* altCopy=.*><mi .*>O</mi><mrow><mn>3</mn></mrow> <mrow><mn>4</mn></mrow> <mprescripts/> <mrow><mn>1</mn></mrow> <mrow><mn>2</mn></mrow> </mmultiscripts>"
)

add_test(
    NAME wxxmltag
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxxmltag(a\\\<,b);" "<b.*>a\\\\&lt;</b>"
)

add_test(
    NAME wxxmltag2
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxxmltag(\"a<\",b);" "<b.*>a&lt;</b>"
)

add_test(
    NAME show_image
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "show_image(\"test<2.png\");" "<img del=\"no\" lisp=.*>test&lt;2.png</img>"
)

add_test(
    NAME wxxml-mlable
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "test;" "<lbl altCopy=\"%o1\">(%o1)"
)

add_test(
    NAME wxxml-mlable2
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "outchar:\">\";" "<math><lbl altCopy=\"\\\\&gt;1\">(&gt;1) </lbl>.*&gt;.*</math>"
)

add_test(
    NAME wxxml-orthopoly
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "legendre_p (n - 1, x);" "<fn altCopy=\"legendre_p(n-1,x)\"><munder><fnm>P</fnm><mrow><mi lisp=.*>n</mi><mo>-</mo><mn>1</mn></mrow></munder><p><mi lisp=.*>x</mi></p></fn>"
)

add_test(
    NAME wxxml-pochhammer
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "pochhammer (\"<\", \">\");" "<munder altCopy=\"pochhammer(&quot;&lt;&quot;,&quot;&gt;&quot;)\"><p><st>&lt;</st></p><mrow><st>&gt;</st></mrow></munder>"
)

add_test(
    NAME wxxml-pderivop
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "diff(f(y),y);" "<d lisp=.*><mfrac lisp=.*><mrow><s lisp=.*>d</s></mrow><mrow><s lisp=.*>d</s><h>.</h><mi lisp=.*>y</mi></mrow></mfrac><h>.</h><fn lisp=.*><fnm lisp=.*>f</fnm><mrow><p><mrow><mi lisp=.*>y</mi></mrow></p></mrow></fn></d>"
)

add_test(
    NAME mminus
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "-x-1;" "<mo>-</mo><mi lisp=.*>x</mi><mo>-</mo><mn>1</mn>"
)

add_test(
    NAME mfactorial
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "(n\!)\!;" "<mi lisp=.*>n</mi><mo>\!</mo></p></mrow><mo>\!</mo>"
)

add_test(
    NAME subscriptp_1
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts:true\$eeee_133;" "<munder altCopy=\"eeee_133\"><mrow><mi>eeee</mi></mrow><mrow><mi>133</mi></mrow></munder>"
)

add_test(
    NAME subscriptp_2
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts:true\$wxdeclare_subscripted(eeee_aaa)\$eeee_aaa;" "<munder altCopy=\"eeee_aaa\"><mrow><mi>eeee</mi></mrow><mrow><mi>aaa</mi></mrow></munder>"
)

add_test(
    NAME subscriptp_3
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts:true\$wxdeclare_subscripted(eeee_aaa)\$eeee_aaae;" "<mi lisp=.*>eeee_aaae</mi>"
)

add_test(
    NAME subscriptp_4
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts:true\$eeee_133;" "<munder altCopy=\"eeee_133\"><mrow><mi>eeee</mi></mrow><mrow><mi>133</mi></mrow></munder>"
)

add_test(
    NAME subscriptp_5
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts:true\$wxdeclare_subscript(bbb)\$eeee_bbb;" "<munder altCopy=\"eeee_bbb\"><mrow><mi>eeee</mi></mrow><mrow><mi>bbb</mi></mrow></munder>"
)

add_test(
    NAME subscriptp_6
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts:true\$wxdeclare_subscript(bbb)\$eeee_bbb;" "<munder altCopy=\"eeee_bbb\"><mrow><mi>eeee</mi></mrow><mrow><mi>bbb</mi></mrow></munder>"
)

add_test(
    NAME subscriptp_7
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts: 'all\$eree_ssss;" "<munder altCopy=\"eree_ssss\"><mrow><mi>eree</mi></mrow><mrow><mi>ssss</mi></mrow></munder>"
)

add_test(
    NAME subscriptp_8
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts: false\$a_b;" "<mi lisp=.*>a_b</mi>"
)

add_test(
    NAME subscriptp_9
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts: false\$a_1;" "<mi lisp=.*>a_1</mi>"
)

add_test(
    NAME subscriptp_10
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts:true\$t_1\\,2;" "<munder altCopy=\"t_1\\\\,2\"><mrow><mi>t</mi></mrow><mrow><mi>1,2</mi></mrow></munder>"
)

add_test(
    NAME subscriptp_11
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxsubscripts:true\$a\\,b_1;" "a,b"
)

add_test(
    NAME mprod
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "a*2*b;" "<mn>2</mn><h>[*]</h><mi lisp=.*>a</mi><h>[*]</h><mi lisp=.*>b</mi>"
)

add_test(
    NAME block
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "f(x):=block([a:x],x);" "<fn lisp=.*><fnm lisp=.*>f</fnm><mrow><p><mrow><mi lisp=.*>x</mi></mrow></p></mrow></fn><mo>:=</mo><fn lisp=.*><fnm lisp=.*>block</fnm><mrow><p><mrow><mrow list=\"true\"><t lisp=.* listdelim=\"true\">\\[</t><mrow><mi lisp=.*>a</mi><mo>:</mo><mi lisp=.*>x</mi></mrow><t lisp=.* listdelim=\"true\">\\]</t></mrow></mrow><mo>,</mo><mrow><mi lisp=.*>x</mi></mrow></p></mrow></fn>"
)

add_test(
    NAME mlist
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "[&,b];" "<t lisp=.* listdelim=\"true\">\\[</t><mrow><mi lisp=.*>&amp;</mi></mrow><mo>,</mo><mrow><mi lisp=.*>b</mi></mrow><t lisp=.* listdelim=\"true\">\\]</t></mrow>"
)

add_test(
    NAME true
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "true;" "<t lisp=.*>true</t>"
)

add_test(
    NAME false
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "true;" "<t lisp=.*>true</t>"
)

add_test(
    NAME mfrac
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "a/(1/2+b);" "<mfrac lisp=.*><mrow><mi lisp=.*>a</mi></mrow><mrow><mi lisp=.*>b</mi><mo>+</mo><mfrac lisp=.*><mrow><mn>1</mn></mrow><mrow><mn>2</mn></mrow></mfrac></mrow></mfrac>"
)

# The test for the argument of load() was added in Maxima 5.46.0
if(MAXIMA_VERSION VERSION_GREATER_EQUAL "5.46.0")
    add_test(
        NAME load_symbol
        COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "load(engineering-format);" "argument must be a string, symbol, or pathname")
endif()

add_test(
    NAME wxmaxima_batch_emptyFile
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch --exit-on-error empty_file.wxm)

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

add_test(
    NAME wxmaxima_version_string
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --version)
set_tests_properties(wxmaxima_version_string PROPERTIES  PASS_REGULAR_EXPRESSION "wxMaxima ${VERSION}.*")

add_test(
    NAME wxmaxima_version_returncode
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --version)

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
    COMMAND wxmaxima --debug --logtostderr --pipe -f autosave.cfg --batch --exit-on-error testbench_simple.mac)

add_test(
    NAME openMacFiles2
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f autosave.cfg --batch --exit-on-error testbench_simple2.mac)

add_test(
    NAME tutorial_10Minutes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch --exit-on-error 10MinuteTutorial.wxm)

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
    COMMAND wxmaxima --debug --logtostderr --pipe --single_process --batch --exit-on-error threadtest.wxm threadtest2.wxm)

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

# Give every test its own MAXIMA_USERDIR so tests running in parallel never share
# Maxima's compiled-package cache: Maxima's load() of a compiled package (draw,
# lsquares, ...) writes the compiled result into that cache, and two instances
# loading the same package concurrently into one shared cache can race (the second
# reads a half-written file and load() fails). Isolating per test removes the race
# without serializing the suite. (wxMaxima's config writes now degrade gracefully
# when the dir isn't writable, so a fresh per-test MAXIMA_USERDIR no longer
# asserts.)
get_property(_wxm_all_tests DIRECTORY PROPERTY TESTS)
foreach(_wxm_t IN LISTS _wxm_all_tests)
  set(_wxm_userdir "${CMAKE_CURRENT_BINARY_DIR}/test_userdirs/${_wxm_t}")
  file(MAKE_DIRECTORY "${_wxm_userdir}")
  set_property(TEST "${_wxm_t}" APPEND PROPERTY ENVIRONMENT
    "MAXIMA_USERDIR=${_wxm_userdir}")
endforeach()



