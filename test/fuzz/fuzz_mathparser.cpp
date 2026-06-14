// Coverage-guided libFuzzer harness for wxMaxima's MathParser — the parser for
// the MathML-like XML inside .wxmx files, i.e. Maxima's rich 2D output
// (fractions, matrices, integrals, sub/superscripts, ...). This exercises the
// 2D math cell construction that the .wxm parser never touches.
#include "fuzz_init.h"
#include "MathParser.h"
#include "Configuration.h"
#include "Worksheet.h"
#include "cells/GroupCell.h"
#include <wx/app.h>
#include <wx/frame.h>
#include <wx/dcmemory.h>
#include <wx/bitmap.h>
#include <wx/image.h>
#include <wx/xml/xml.h>
#include <wx/mstream.h>
#include <wx/log.h>
#include <cstdint>
#include <cstddef>

class FuzzApp : public wxApp { public: bool OnInit() override { return true; } };

namespace {
wxMemoryDC *g_dc = nullptr;
wxBitmap   *g_bmp = nullptr;
Configuration *g_cfg = nullptr;
GroupCell  *g_group = nullptr;
}

extern "C" int LLVMFuzzerInitialize(int *argc, char ***argv) {
  EnsureDisplay();
  wxApp::SetInstance(new FuzzApp());
  wxEntryStart(*argc, *argv);
  wxTheApp->CallOnInit();
  wxInitAllImageHandlers();
  wxLog::EnableLogging(false);     // silence wxXml parse-error spam
  g_bmp = new wxBitmap(64, 64);
  g_dc  = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  auto *frame = new wxFrame(nullptr, wxID_ANY, wxS("fuzz"));
  auto *ws = new Worksheet(frame, wxID_ANY, g_cfg);
  g_cfg->SetWorkSheet(ws);
  g_group = new GroupCell(g_cfg, GC_TYPE_CODE);   // group context for parsed cells
  return 0;
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  wxMemoryInputStream stream(data, size);
  wxXmlDocument doc;
  {
    wxLogNull noLog;
    if (!doc.Load(stream) || !doc.GetRoot())
      return 0;                    // not well-formed XML: nothing for MathParser
  }
  MathParser mp(g_cfg);
  mp.SetGroup(g_group);
  auto cell = mp.ParseTag(doc.GetRoot(), true, 0);   // build the 2D math cells
  return 0;
}
