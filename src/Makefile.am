AM_CXXFLAGS = -DPREFIX=\"$(prefix)\"

if CHM
AM_CXXFLAGS += -DCHM=true
endif

bin_PROGRAMS = wxmaxima

wxmaxima_SOURCES = \
	ConfigDialogue.cpp         ConfigDialogue.h         \
	main.cpp                            \
	wxMaxima.cpp       wxMaxima.h       \
	FindReplaceDialog.cpp       FindReplaceDialog.h       \
	FindReplacePane.cpp       FindReplacePane.h       \
	wxMaximaFrame.cpp  wxMaximaFrame.h  \
	SubstituteWiz.cpp  SubstituteWiz.h  \
	IntegrateWiz.cpp   IntegrateWiz.h   \
	LimitWiz.cpp       LimitWiz.h       \
	Plot2dWiz.cpp      Plot2dWiz.h      \
	SeriesWiz.cpp      SeriesWiz.h      \
	SumWiz.cpp         SumWiz.h         \
	Plot3dWiz.cpp      Plot3dWiz.h      \
	Dirstructure.cpp   Dirstructure.h   \
	Gen1Wiz.cpp        Gen1Wiz.h        \
	Gen2Wiz.cpp        Gen2Wiz.h        \
	Gen3Wiz.cpp        Gen3Wiz.h        \
	Gen4Wiz.cpp        Gen4Wiz.h        \
	BC2Wiz.cpp         BC2Wiz.h         \
	SystemWiz.cpp      SystemWiz.h      \
	BTextCtrl.cpp      BTextCtrl.h      \
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
	SVGout.cpp         SVGout.h         \
	AbsCell.cpp        AbsCell.h        \
	ConjugateCell.cpp  ConjugateCell.h  \
	AtCell.cpp         AtCell.h         \
	DiffCell.cpp       DiffCell.h       \
	FunCell.cpp        FunCell.h        \
	MathCtrl.cpp       MathCtrl.h       \
	Configuration.cpp     Configuration.h     \
	MathParser.cpp     MathParser.h     \
	MathPrintout.cpp   MathPrintout.h   \
	Notification.cpp   Notification.h   \
	Bitmap.cpp         Bitmap.h         \
	MyTipProvider.cpp  MyTipProvider.h  \
	EditorCell.cpp     EditorCell.h     \
	ImgCell.cpp        ImgCell.h        \
	Image.cpp          Image.h          \
	SubSupCell.cpp     SubSupCell.h     \
	SlideShowCell.cpp  SlideShowCell.h  \
	StatusBar.cpp      StatusBar.h      \
	GroupCell.cpp      GroupCell.h      \
	EvaluationQueue.cpp   EvaluationQueue.h   \
	AutocompletePopup.cpp AutocompletePopup.h \
	ContentAssistantPopup.cpp ContentAssistantPopup.h \
	MarkDown.cpp       MarkDown.h       \
	ToolBar.cpp        ToolBar.h        \
	History.cpp        History.h        \
	TableOfContents.cpp      TableOfContents.h      \
	XmlInspector.cpp   XmlInspector.h   \
	Autocomplete.cpp   Autocomplete.h   \
	PlotFormatWiz.cpp  PlotFormatWiz.h  \
	TextStyle.h

wxmaxima_LDADD = $(RC_OBJ) $(WX_LIBS)

wxmaxima_DEPENDENCIES = $(RC_OBJ)
EXTRA_wxmaxima_SOURCES = Resources.rc

Resources.o :
	$(WINDRES) $(WX_RC_PATH) --include-dir $(srcdir)/../data --include-dir $(srcdir)/../art $(srcdir)/Resources.rc -o Resources.o
