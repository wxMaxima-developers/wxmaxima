// Stubs for the main.cpp symbols referenced by the rest of the app but not
// needed by the GroupCell layout unit test. main.cpp is excluded from the
// wxmTestApp object library because it holds the application's real main()/
// app-bootstrap; the test provides its own minimal main().
//
// NewWindow() opens a GUI window; the layout test never triggers it, so a no-op
// definition is enough to satisfy the linker.
#include "wxMaxima.h"

void MyApp::NewWindow(const wxString &, bool, bool, unsigned char *, std::size_t) {}
