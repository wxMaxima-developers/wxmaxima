// Stub for the one main.cpp symbol referenced by the rest of the app but not
// needed for parse-only fuzzing (main.cpp is excluded - it holds libFuzzer's
// rival main()). NewWindow opens a GUI window; a no-op is correct here.
#include "wxMaxima.h"
void MyApp::NewWindow(const wxString &, bool, bool, unsigned char *, std::size_t) {}
