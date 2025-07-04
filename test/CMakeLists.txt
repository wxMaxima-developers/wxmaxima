# -*- mode: CMake; cmake-tab-width: 4; -*-

# Compile a list of test files
file(GLOB TEST_FILES automatic_test_files/*.png automatic_test_files/*.wxmx automatic_test_files/*.wxm automatic_test_files/*.mac automatic_test_files/*.cfg testbench_simple.wxmx automatic_test_files/*.gif)

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

# Copy the .wxm files to the binary dir so they can be used as test case without
# being afraid that we change the test cases somehow.
file(
    COPY
    ${TEST_FILES}
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
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

add_test(
    NAME multiple_batch_files
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch unicode.wxm boxes.wxm)

add_test(
    NAME multiple_batch_files_inverse
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch boxes.wxm unicode.wxm)

add_test(
    NAME multiple_batch_files2
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch --single_process unicode.wxm boxes.wxm)

add_test(
    NAME multiple_batch_files_singlethread
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch --single_thread unicode.wxm boxes.wxm)

add_test(
    NAME multiple_batch_files2_singlethread
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --batch --single_process --single_thread unicode.wxm boxes.wxm)

add_test(
    NAME wxstatusbar
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxstatusbar(\"a<1>b\");" "<statusbar>a&lt;1&gt;b</statusbar>"
)

add_test(
    NAME wxbuildinfo
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxbuild_info();" "WxMaxima"
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
    	      COMMAND wxmaxima --debug --logtostderr --pipe --batch automatic_test_files/${f}.wxm)
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
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "outchar:\">\";" "<math><lbl altCopy=\"\\\\&gt;1\">(&gt;1) </lbl><st>&gt;</st></math>"
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
    COMMAND wxmaxima --debug --logtostderr --pipe --batch empty_file.wxm)

add_test(
    NAME wxmaxima_performance
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch performance.wxm)

add_test(
    NAME wxmaxima_batch_textcell
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch textcells.wxm)

add_test(
    NAME wxmaxima_batch_foreign_characters
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch foreign-characters.wxm)

add_test(
    NAME wxmaxima_load_symbol
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch load_string.wxm)

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
    COMMAND wxmaxima --debug --logtostderr --pipe --batch testbench_all_celltypes.wxm)

add_test(
    NAME simpleInput
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch simpleInput.wxm)

add_test(
    NAME rememberingAnswers
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch rememberingAnswers.wxm)

add_test(
    NAME absCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch absCells.wxm)

add_test(
    NAME absCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch absCells.wxm)

add_test(
    NAME boxCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch boxCells.wxm)

add_test(
    NAME boxCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch boxCells.wxm)

add_test(
    NAME diffCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch diffCells.wxm)

add_test(
    NAME diffCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch diffCells.wxm)

add_test(
    NAME atCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch atCells.wxm)

add_test(
    NAME atCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch atCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME atCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch atCells.wxm)

add_test(
    NAME conjugateCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch conjugateCells.wxm)

add_test(
    NAME conjugateCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch conjugateCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME conjugateCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch conjugateCells.wxm)

add_test(
    NAME exptCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch exptCells.wxm)

add_test(
    NAME exptCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch exptCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME exptCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch exptCells.wxm)

add_test(
    NAME fracCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch fracCells.wxm)

add_test(
    NAME fracCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch fracCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME fracCells_cmdline_wxmathml_over_network
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo 'load(fracCells.wxm) ; quit() ;' | nc -l 40000 & sleep 10 ; ${MAXIMA}-s 40000
    VERBATIM
    )

add_test(
    NAME fracCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch fracCells.wxm)

add_test(
    NAME intCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch intCells.wxm)

add_test(
    NAME intCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch intCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME intCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch intCells.wxm)

add_test(
    NAME intervals_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch intervals.wxm)

add_test(
    NAME intervals_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch intervals.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME intervals
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch intervals.wxm)

add_test(
    NAME functionCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch functionCells.wxm)

add_test(
    NAME functionCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch functionCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME functionCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch functionCells.wxm)

add_test(
    NAME slideshowCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch slideshowCells.wxm)

add_test(
    NAME imageCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch imageCells.wxm)

add_test(
    NAME limitCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch limitCells.wxm)

add_test(
    NAME limitCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch limitCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME limitCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch limitCells.wxm)

add_test(
    NAME matrixCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch matrixCells.wxm)

add_test(
    NAME matrixCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch matrixCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME matrixCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch matrixCells.wxm)

add_test(
    NAME parenthesisCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch parenthesisCells.wxm)

add_test(
    NAME parenthesisCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch parenthesisCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME parenthesisCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch parenthesisCells.wxm)

add_test(
    NAME listCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch listCells.wxm)

add_test(
    NAME listCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch listCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME listCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch listCells.wxm)

add_test(
    NAME setCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch listCells.wxm)

add_test(
    NAME setCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch setCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME setCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch setCells.wxm)

add_test(
    NAME sqrtCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch sqrtCells.wxm)

add_test(
    NAME sqrtCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch sqrtCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME sqrtCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch sqrtCells.wxm)

add_test(
    NAME subCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch subCells.wxm)

add_test(
    NAME subCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch subCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME subCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch subCells.wxm)

add_test(
    NAME subsupCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch subsupCells.wxm)

add_test(
    NAME subsupCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch subsupCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME subsupCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch subsupCells.wxm)

add_test(
    NAME presubsupcells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch presubsupcells.wxm)

add_test(
    NAME presubsupcells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch presubsupcells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME presubsupcells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch presubsupcells.wxm)

add_test(
    NAME sumCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch sumCells.wxm)

add_test(
    NAME sumCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch sumCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME sumCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch sumCells.wxm)

add_test(
    NAME productCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch productCells.wxm)

add_test(
    NAME productCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch productCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME productCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch productCells.wxm)

add_test(
    NAME printf_simple_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch printf_simple.wxm)

add_test(
    NAME printf_simple_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch printf_simple.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME printf_simple
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch printf_simple.wxm)

add_test(
    NAME printf_equations_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch printf_equations.wxm)

add_test(
    NAME printf_equations_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch printf_equations.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME printf_equations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch printf_equations.wxm)

add_test(
    NAME printf_continuationLines_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch printf_continuationLines.wxm)

add_test(
    NAME printf_continuationLines_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch printf_continuationLines.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME printf_continuationLines
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch printf_continuationLines.wxm)

add_test(
    NAME weirdLabels
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch weirdLabels.wxm)

add_test(
    NAME xmlQuote
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch xmlQuote.wxm)

add_test(
    NAME formerCrashes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch formerCrashes.wxm)

add_test(
    NAME multiplication
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch multiplication.wxm)

add_test(
    NAME nonsenseConstructs
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch nonsenseConstructs.wxm)

add_test(
    NAME lisp
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch lisp.wxm)

add_test(
    NAME boxes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch boxes.wxm)

add_test(
    NAME unicode
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch unicode.wxm)

add_test(
    NAME unicode_specialchars
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch unicode_specialchars.wxm)

add_test(
    NAME openMacFiles
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f autosave.cfg --batch testbench_simple.mac)

add_test(
    NAME openMacFiles2
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f autosave.cfg --batch testbench_simple2.mac)

add_test(
    NAME tutorial_10Minutes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch 10MinuteTutorial.wxm)

add_test(
    NAME comment_begin
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch commentBegin.wxm)

add_test(
    NAME threadtest
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --batch threadtest.wxm)

add_test(
    NAME autosave
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f autosave.cfg --batch unicode.wxm)

add_test(
    NAME noautosave
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f noautosave.cfg --batch unicode.wxm)

add_test(
    NAME config_from_19.11
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe -f config_from_19.11.cfg --batch unicode.wxm)

add_test(
    NAME invalid_commandline_arg
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --gibberish --batch unicode.wxm)
set_tests_properties(invalid_commandline_arg PROPERTIES WILL_FAIL true)

add_test(
    NAME multiwindow
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --single_process --batch unicode.wxm boxes.wxm)

add_test(
    NAME multithreadtest
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe --single_process --batch threadtest.wxm threadtest2.wxm)

#add_test(
#    NAME ipc_copypaste1
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --debug --enableipc --batch ipc_copypaste1.wxm)

#add_test(
#    NAME ipc_copypaste_all-celltypes
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --debug --enableipc --batch ipc_copypaste_all-celltypes.wxm)

add_test(
    NAME config_dialogue_sample
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../data
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch sampleWorksheet.wxmx)

add_test(
    NAME tutorial_fastListAccess
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch fastListAccess.wxm)

add_test(
    NAME tutorial_numberFormats
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch numberFormats.wxm)

add_test(
    NAME tutorial_toleranceCalculations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch toleranceCalculations.wxm)

add_test(
    NAME tutorial_fittingEquations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch fittingEquations.wxm)

add_test(
    NAME tutorial_variableNames
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch variableNames.wxm)

add_test(
    NAME tutorial_displaying3DCurves
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch displaying3DCurves.wxm)

add_test(
   NAME tutorial_memoizing
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
   COMMAND wxmaxima --debug --logtostderr --pipe  --batch memoizing.wxm)

add_test(
    NAME tutorial_solvingEquations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch solvingEquations.wxm)

add_test(
    NAME testbench_simple.wxmx
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --debug --logtostderr --pipe  --batch testbench_simple.wxmx)

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

