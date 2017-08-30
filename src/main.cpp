// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2015-2017 Gunter Königsmann <wxMaxima@physikbuch.de>
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

/*!\mainpage Documentation for the code of wxMaxima

\section whatiswxmaxima What is wxMaxima?
wxMaxima is a full-featured graphical frontend for maxima, a full-featured
computer algebra system that will do numeric calculations, if one wants to.
But it is specialized in manipulating and solving symbolic equations.

\image html wxMaxima.gif "A random screenshot of wxMaxima"

\section Wheretostart Where to start reading the code?
The main starting points would be:
 - For everything that happens before the main window is displayed: main.cpp
 - For things having to do with the worksheet: MathCtrl
 - For things having to do with how text the user input is displayed: EditorCell
 - For the items surrounding the work sheet: wxMaximaFrame
 - For the general program logic and communication with maxima: wxMaxima.cpp
 - For how 2d math items are handled: MathCell and all cells derived from it
 - For how math is rendered as bitmap: Bitmap
 - For the configuration dialogue: ConfigDialogue
 - For how .wxmx files and output from maxima are read (They are in the same
   format): MathParser
 - For how images from plots or images that are included by the users are 
   handled: Image (for the code handling the images), ImageCell (for still
   images) and SlideshowCell (for animations).
 - For printing out data: MathPrintout

\section GeneralPrinciple The general principle
The main things that are important to know are:
 - Maxima is a command-line program that can use a network connection in order
   to communicate with a frontend.
 - In data/wxmathml.lisp wxMaxima teaches maxima to talk in a XML dialect:
   Maxima's normal output format is human-readable. But it can be tricked into 
   containing strings that look like input or output labels and it doesn't 
   guarantee that there is a way to transform output from maxima into a valid input
   again that has exactly the same meaning.
 - The worksheet is defined in the class MathCtrl.
 - It is organized in GroupCells that each can contain a cell containing a list of
   cells containing a label and the user input and a list of cells containing the
   output label and 2d math from maxima.
 - All cells contain pointers so they can be used as a part of an double-linked list
   for the logical order they appear in and a second double-linked list that tells 
   which cell to draw next. The latter is needed for handling the fact that some
   things (like fractions) can be displayed as 2D maths and in a more linear way.
 - For every mathematical function, image or piece of text a Cell there is a specialized 
   MathCell type that "knows" how to draw it, how to convert it to a string or how to 
   convert it to Mathml, OOML, RTF or any other data format wxMaxima supports.
   There is, for example, an AbsCell for the <code>abs()</code> command, an IntCell 
   representing <code>integrate</code>, a ParenCell for parenthesis and a FunCell for 
   all the functions no special handling is needed for.
 - Configuration is something like a central object keeping the configuration needed for 
   displaying cells.

\section CodingStyle The Coding Style
Keeping the code more or less homogenous increases the readability. In
order to archieve that wxMaxima uses a few naming rules:
 - The names of member variables are prefixed with "m_" for "member".
 - The names of member functions (aka methods) are written in CamelCase.
 - The names of enums do not really matter as they are rarely used and if
   they are they are used in context where it is opvious that they name an
   enum type so there aren't any rules for the names of enums right now.
There are a few other coding-style rules, as well:
 - A "<code>=</code>" as an assignment operator is preceded and followed by
   a space character.
 - And the code is documented in doxygen, which basically means to start every comment
   that describes the function, object, method or variable that follows the comment
   with a "!" so Doxygen knows that this comment is meant to end up in the 
   documentation.
 */

/*!\file
  The file that defines the starting point of the program
 */

#include <wx/wx.h>
#include <wx/tipdlg.h>
#include <wx/config.h>
#include <wx/intl.h>
#include <wx/fs_zip.h>
#include <wx/image.h>

#include <wx/cmdline.h>
#include <wx/fileconf.h>
#include "Dirstructure.h"
#include <iostream>

#include "wxMaxima.h"
#include "Setup.h"

// On wxGTK2 we support printing only if wxWidgets is compiled with gnome_print.
// We have to force gnome_print support to be linked in static builds of wxMaxima.

#if defined wxUSE_LIBGNOMEPRINT
#if wxUSE_LIBGNOMEPRINT
#include "wx/html/forcelnk.h"
FORCE_LINK(gnome_print)
#endif
#endif


IMPLEMENT_APP(MyApp)

void MyApp::Cleanup_Static()
{
  if (m_frame)
    m_frame->CleanUp();
}

bool MyApp::OnInit()
{
  m_frame = NULL;
//  atexit(Cleanup_Static);
  int lang = wxLANGUAGE_UNKNOWN;

  bool batchmode = false;

  wxCmdLineParser cmdLineParser(argc, argv);

  static const wxCmdLineEntryDesc cmdLineDesc[] =
          {
                  {wxCMD_LINE_SWITCH, "v", "version", "Output the version info"},
                  /* Usually wxCMD_LINE_OPTION_HELP is used with the following option, but that displays a message
                   * using a own window and we want the message on the command line. If a user enters a command
                   * line option, he expects probably a answer just on the command line... */
                  {wxCMD_LINE_SWITCH, "h", "help", "show this help message", wxCMD_LINE_VAL_NONE},
                  {wxCMD_LINE_OPTION, "o", "open", "open a file"},
                  {wxCMD_LINE_SWITCH, "b", "batch",
                   "run the file and exit afterwards. Halts on questions and stops on errors."},
                  { wxCMD_LINE_OPTION, "f", "ini", "use a specific configuration file" },
                  {wxCMD_LINE_PARAM, NULL, NULL, "input file", wxCMD_LINE_VAL_STRING, wxCMD_LINE_PARAM_OPTIONAL},
                  {wxCMD_LINE_NONE}
          };

  cmdLineParser.SetDesc(cmdLineDesc);
  cmdLineParser.Parse();
  wxString ini, file;
  // Attention: The config file is changed by wxMaximaFrame::wxMaximaFrame::ReReadConfig
  if (cmdLineParser.Found(wxT("f"),&ini))
  {
    wxConfig::Set(new wxFileConfig(ini));
    m_configFileName = ini;
  }
  else
    wxConfig::Set(new wxConfig(wxT("wxMaxima")));

  wxImage::AddHandler(new wxPNGHandler);
  wxImage::AddHandler(new wxXPMHandler);
  wxImage::AddHandler(new wxJPEGHandler);

  wxFileSystem::AddHandler(new wxZipFSHandler);

  Dirstructure dirstructure;

  wxConfigBase *config = wxConfig::Get();
  lang = wxLocale::GetSystemLanguage();
  config->Read(wxT("language"), &lang);

  // If wxWidgets or wxMaxima doesn't provide a translation for the current language
  // we don't want this to throw an error => Mute errors for the next command.
  {
    wxLogNull disableErrors;
    m_locale.Init(lang);
  }

#if defined (__WXMSW__)
  wxSetEnv(wxT("LANG"), m_locale.GetName());
  if (!wxGetEnv(wxT("BUILD_DIR"), NULL))
  {
    wxString dir = wxPathOnly(wxStandardPaths::GetExecutablePath());
    if(dir != wxEmptyString)
      wxSetWorkingDirectory(wxPathOnly(wxStandardPaths::GetExecutablePath()));
  }
    
    Dirstructure dirstruct;
    wxString fontPrefix = dirstruct.FontDir() + wxT("/");

  /* Add private jsMath fonts, if they exist */ 
    if (wxFileExists(fontPrefix + wxT(CMEX10) + wxT(".ttf"))) AddFontResource(fontPrefix + wxT(CMEX10) + wxT(".ttf"));
  if (wxFileExists(fontPrefix + wxT(CMSY10) + wxT(".ttf"))) AddFontResource(fontPrefix + wxT(CMSY10) + wxT(".ttf"));
  if (wxFileExists(fontPrefix + wxT(CMR10) + wxT(".ttf")))  AddFontResource(fontPrefix + wxT(CMR10) + wxT(".ttf"));
  if (wxFileExists(fontPrefix + wxT(CMMI10) + wxT(".ttf"))) AddFontResource(fontPrefix + wxT(CMMI10) + wxT(".ttf"));
  if (wxFileExists(fontPrefix + wxT(CMTI10) + wxT(".ttf"))) AddFontResource(fontPrefix + wxT(CMTI10) + wxT(".ttf"));

  /* Add private Libertine fonts, if they exist */
  if (wxFileExists(fontPrefix + wxT(LIBERTINE1))) 
	  AddFontResource(fontPrefix + wxT(LIBERTINE1));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE2))) AddFontResource(fontPrefix + wxT(LIBERTINE2));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE3))) AddFontResource(fontPrefix + wxT(LIBERTINE3));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE4))) AddFontResource(fontPrefix + wxT(LIBERTINE4));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE5))) AddFontResource(fontPrefix + wxT(LIBERTINE5));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE6))) AddFontResource(fontPrefix + wxT(LIBERTINE6));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE7))) AddFontResource(fontPrefix + wxT(LIBERTINE7));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE8))) AddFontResource(fontPrefix + wxT(LIBERTINE8));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE9))) AddFontResource(fontPrefix + wxT(LIBERTINE9));
#endif

  m_locale.AddCatalogLookupPathPrefix(dirstructure.LocaleDir());
  m_locale.AddCatalogLookupPathPrefix(dirstructure.LocaleDir()+wxT("/wxwin"));
  m_locale.AddCatalogLookupPathPrefix(wxT("/usr/share/locale"));
  m_locale.AddCatalogLookupPathPrefix(wxT("/usr/local/share/locale"));
  m_locale.AddCatalog(wxT("wxMaxima"));
  m_locale.AddCatalog(wxT("wxMaxima-wxstd"));

#if defined __WXMAC__
  wxString path;
  wxGetEnv(wxT("PATH"), &path);
  wxSetEnv(wxT("PATH"), path << wxT(":/usr/local/bin"));

  wxApp::SetExitOnFrameDelete(false);
  wxMenuBar *menuBar = new wxMenuBar;
  wxMenu *fileMenu = new wxMenu;
  fileMenu->Append(wxMaxima::mac_newId, _("&New\tCtrl+N"));
  fileMenu->Append(wxMaxima::mac_openId, _("&Open\tCtrl+O"));
  menuBar->Append(fileMenu, _("File"));
  wxMenuBar::MacSetCommonMenuBar(menuBar);

  Connect(wxMaxima::mac_newId, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(MyApp::OnFileMenu));
  Connect(wxMaxima::mac_openId, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(MyApp::OnFileMenu));
  Connect(wxID_EXIT, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(MyApp::OnFileMenu));
#endif

  if (cmdLineParser.Found(wxT("v")))
  {
    std::cout << "wxMaxima ";
    std::cout << GITVERSION;
    std::cout << "\n";
    wxExit();
  }
  if (cmdLineParser.Found(wxT("h")))
  {
    std::cout << "A feature-rich graphical user interface for the computer algebra system maxima\n";
    std::cout << cmdLineParser.GetUsageString();
    wxExit();
  }

  if (cmdLineParser.Found(wxT("b")))
  {
    batchmode = true;
  }

  if (cmdLineParser.Found(wxT("o"), &file))
  {
    wxFileName FileName = file;
    FileName.MakeAbsolute();
    wxString CanonicalFilename = FileName.GetFullPath();
    NewWindow(wxString(CanonicalFilename), batchmode);
    return true;
  }
  else
  {
    if (cmdLineParser.GetParamCount() > 0)
    {
      wxFileName FileName = cmdLineParser.GetParam();
      FileName.MakeAbsolute();

      wxString CanonicalFilename = FileName.GetFullPath();
      NewWindow(CanonicalFilename, batchmode);
    }
    else
      NewWindow();
  }
  return true;
}

#if defined (__WXMSW__)
int MyApp::OnExit()
{
  Dirstructure dirstruct;
  wxString fontPrefix = dirstruct.FontDir() + wxT("/");
  if (wxFileExists(fontPrefix + CMEX10)) RemoveFontResource(fontPrefix + wxT(CMEX10));
  if (wxFileExists(fontPrefix + CMSY10)) RemoveFontResource(fontPrefix + wxT(CMSY10));
  if (wxFileExists(fontPrefix + CMR10))  RemoveFontResource(fontPrefix + wxT(CMR10));
  if (wxFileExists(fontPrefix + CMMI10)) RemoveFontResource(fontPrefix + wxT(CMMI10));
  if (wxFileExists(fontPrefix + CMTI10)) RemoveFontResource(fontPrefix + wxT(CMTI10));

  if (wxFileExists(fontPrefix + LIBERTINE1)) RemoveFontResource(fontPrefix + wxT(LIBERTINE1));
  if (wxFileExists(fontPrefix + LIBERTINE2)) RemoveFontResource(fontPrefix + wxT(LIBERTINE2));
  if (wxFileExists(fontPrefix + LIBERTINE3)) RemoveFontResource(fontPrefix + wxT(LIBERTINE3));
  if (wxFileExists(fontPrefix + LIBERTINE4)) RemoveFontResource(fontPrefix + wxT(LIBERTINE4));
  if (wxFileExists(fontPrefix + LIBERTINE5)) RemoveFontResource(fontPrefix + wxT(LIBERTINE5));
  if (wxFileExists(fontPrefix + LIBERTINE6)) RemoveFontResource(fontPrefix + wxT(LIBERTINE6));
  if (wxFileExists(fontPrefix + LIBERTINE7)) RemoveFontResource(fontPrefix + wxT(LIBERTINE7));
  if (wxFileExists(fontPrefix + LIBERTINE8)) RemoveFontResource(fontPrefix + wxT(LIBERTINE8));
  if (wxFileExists(fontPrefix + LIBERTINE9)) RemoveFontResource(fontPrefix + wxT(LIBERTINE9));

  return true;
}
#endif

int window_counter = 0;

void MyApp::NewWindow(wxString file, bool batchmode)
{
  int x = 40, y = 40, h = 650, w = 950, m = 0;
  int rs = 0;
  int display_width = 1024, display_height = 768;
  bool have_pos;

  wxConfig *config = (wxConfig *) wxConfig::Get();

  wxDisplaySize(&display_width, &display_height);

  have_pos = config->Read(wxT("pos-x"), &x);
  config->Read(wxT("pos-y"), &y);
  config->Read(wxT("pos-h"), &h);
  config->Read(wxT("pos-w"), &w);
  config->Read(wxT("pos-max"), &m);
  config->Read(wxT("pos-restore"), &rs);

  if (rs == 0)
    have_pos = false;
  if (!have_pos || m == 1 || x > display_width || y > display_height || x < 0 || y < 0)
  {
    x = 40;
    y = 40;
    h = 650;
    w = 950;
  }

  x += topLevelWindows.GetCount() * 20;
  y += topLevelWindows.GetCount() * 20;

  m_frame = new wxMaxima((wxFrame *) NULL, -1, _("wxMaxima"), m_configFileName,
                         wxPoint(x, y), wxSize(w, h));

  if (m == 1)
    m_frame->Maximize(true);

  if (file.Length() > 0 && wxFileExists(file))
  {
    m_frame->SetOpenFile(file);
  }

  m_frame->SetBatchMode(batchmode);
  topLevelWindows.Append(m_frame);
  if (topLevelWindows.GetCount() > 1)
    m_frame->SetTitle(wxString::Format(_("untitled %d"), ++window_counter));

  SetTopWindow(m_frame);
  m_frame->Show(true);
  m_frame->InitSession();
  m_frame->ShowTip(false);
}

void MyApp::OnFileMenu(wxCommandEvent &ev)
{
  switch (ev.GetId())
  {
    case wxMaximaFrame::menu_new_id:
    case ToolBar::tb_new:
    case wxMaxima::mac_newId:

      // Mac computers insist that all instances of a new application need to share
      // the same process. On all other OSes we create a separate process for each
      // window: This way if one instance of wxMaxima crashes all the other instances
      // are still alive.
#if defined __WXMAC__
      NewWindow();
#else
      wxExecute(wxT("\"")+wxStandardPaths::Get().GetExecutablePath()+wxT("\""));
#endif
      break;
    case wxMaxima::mac_openId:
    {
      wxString file = wxFileSelector(_("Open"), wxEmptyString,
                                     wxEmptyString, wxEmptyString,
                                     _("wxMaxima document (*.wxm, *.wxmx)|*.wxm;*.wxmx"),
                                     wxFD_OPEN);
      if (file.Length() > 0)
        NewWindow(file);
    }
      break;
    case wxID_EXIT:
    {
      bool quit = true;
      wxWindowList::compatibility_iterator node = topLevelWindows.GetFirst();
      while (node)
      {
        wxWindow *frame = node->GetData();
        node = node->GetNext();
        frame->Raise();
        if (!frame->Close())
        {
          quit = false;
          break;
        }
      }
      if (quit)
        wxExit();
    }
      break;
  }
}

void MyApp::MacNewFile()
{
  wxWindow *frame = GetTopWindow();
  if (frame == NULL)
    NewWindow();
}

void MyApp::MacOpenFile(const wxString &file)
{
  NewWindow(file);
}

BEGIN_EVENT_TABLE(MyApp, wxApp)
  EVT_MENU(wxMaximaFrame::menu_new_id, MyApp::OnFileMenu)
  EVT_TOOL(ToolBar::tb_new, MyApp::OnFileMenu)
END_EVENT_TABLE()
