AM_CXXFLAGS = -DPREFIX=\"$(prefix)\"

bin_PROGRAMS = wxmaxima

wxmaxima_SOURCES = \
	CommandLine.cpp    CommandLine.h    \
	Config.cpp         Config.h         \
	DragNDrop.cpp      DragNDrop.h      \
	main.cpp           Version.h        \
	TextInput.cpp      TextInput.h      \
	wxMaxima.cpp       wxMaxima.h       \
	wxMaximaFrame.cpp  wxMaximaFrame.h  \
	SubstituteWiz.cpp  SubstituteWiz.h  \
	IntegrateWiz.cpp   IntegrateWiz.h   \
	LimitWiz.cpp       LimitWiz.h       \
	Plot2dWiz.cpp      Plot2dWiz.h      \
	SeriesWiz.cpp      SeriesWiz.h      \
	SumWiz.cpp         SumWiz.h         \
	Plot3dWiz.cpp      Plot3dWiz.h      \
	Gen1Wiz.cpp        Gen1Wiz.h        \
	Gen2Wiz.cpp        Gen2Wiz.h        \
	Gen3Wiz.cpp        Gen3Wiz.h        \
	Gen4Wiz.cpp        Gen4Wiz.h        \
	IC1Wiz.cpp         IC1Wiz.h         \
	IC2Wiz.cpp         IC2Wiz.h         \
	BC2Wiz.cpp         BC2Wiz.h         \
	SystemWiz.cpp      SystemWiz.h      \
	BTextCtrl.cpp      BTextCtrl.h      \
	TextCtrl.cpp       TextCtrl.h       \
	MatWiz.cpp         MatWiz.h         \
	ExptCell.cpp       ExptCell.h       \
	FracCell.cpp       FracCell.h       \
	SqrtCell.cpp       SqrtCell.h       \
	MatrCell.cpp       MatrCell.h       \
	MathCell.cpp       MathCell.h       \
	SubCell.cpp        SubCell.h        \
	IntCell.cpp        IntCell.h        \
	TextCell.cpp       TextCell.h       \
	LimitCell.cpp      LimitCell.h      \
	ParenCell.cpp      ParenCell.h      \
	SumCell.cpp        SumCell.h        \
	AbsCell.cpp        AbsCell.h        \
	AtCell.cpp         AtCell.h         \
	DiffCell.cpp       DiffCell.h       \
	FunCell.cpp        FunCell.h        \
	MathCtrl.cpp       MathCtrl.h       \
	CellParser.cpp     CellParser.h     \
	MathParser.cpp     MathParser.h     \
	MathPrintout.cpp   MathPrintout.h   \
	TextStyle.h

wxmaxima_LDFLAGS =
wxmaxima_LDADD = $(RC_OBJ) $(WX_LIBS)

wxmaxima_DEPENDENCIES = $(RC_OBJ)
EXTRA_wxmaxima_SOURCES = Resources.rc

EXTRA_DIST = maximaicon.xpm maximaicon.ico docs.zip \
             intro.zip tips.txt wxmathml.lisp wxmathml-cvs.lisp

wxmaximadir = ${datadir}/wxMaxima

wxmaxima_DATA = docs.zip intro.zip tips.txt wxmathml.lisp wxmathml-cvs.lisp

Resources.o :
	windres --include-dir $(WX_RC_PATH) Resources.rc -o Resources.o
