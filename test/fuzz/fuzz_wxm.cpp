// Coverage-guided libFuzzer harness for wxMaxima's .wxm parser
// (Format::TreeFromWXM). Parse-only: builds the cell tree and destroys it, so a
// minimal headless wxWidgets init + memory DC is enough (no layout/real fonts).
#include "WXMformat.h"
#include "Configuration.h"
#include "Worksheet.h"
#include <wx/app.h>
#include <wx/frame.h>
#include <wx/dcmemory.h>
#include <wx/bitmap.h>
#include <wx/image.h>
#include <wx/string.h>
#include <wx/tokenzr.h>
#include <vector>
#include <cstdint>
#include <cstddef>

// wxGTK routes font/DC operations through GTK, which needs an X display, so we
// do a full GUI init (run the fuzzer under Xvfb). A trivial wxApp is enough.
class FuzzApp : public wxApp { public: bool OnInit() override { return true; } };

namespace {
wxMemoryDC *g_dc = nullptr;
wxBitmap   *g_bmp = nullptr;
Configuration *g_cfg = nullptr;
}

extern "C" int LLVMFuzzerInitialize(int *argc, char ***argv) {
  wxApp::SetInstance(new FuzzApp());
  wxEntryStart(*argc, *argv);     // GUI init (connects to $DISPLAY)
  wxTheApp->CallOnInit();
  wxInitAllImageHandlers();
  g_bmp = new wxBitmap(64, 64);
  g_dc  = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);     // gives the DC valid font metrics
  g_cfg = new Configuration(g_dc);
  // Cells reach their CellPointers via Configuration->GetWorkSheet(), so a real
  // (hidden) Worksheet must back the Configuration or GroupCell construction
  // asserts. A frame parents it.
  auto *frame = new wxFrame(nullptr, wxID_ANY, wxS("fuzz"));
  auto *ws = new Worksheet(frame, wxID_ANY, g_cfg);
  g_cfg->SetWorkSheet(ws);
  return 0;
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  // Interpret the fuzz bytes as the text content of a .wxm file.
  wxString text(reinterpret_cast<const char *>(data), wxConvUTF8, size);
  if (text.IsEmpty() && size)     // non-UTF8: fall back to latin1 so we still parse
    text = wxString(reinterpret_cast<const char *>(data), wxConvISO8859_1, size);

  std::vector<wxString> lines;
  wxStringTokenizer tok(text, wxS("\n"), wxTOKEN_RET_EMPTY_ALL);
  while (tok.HasMoreTokens())
    lines.push_back(tok.GetNextToken());

  auto tree = Format::TreeFromWXM(lines, g_cfg);   // tree destroyed at scope end
  return 0;
}
