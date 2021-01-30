// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2006-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2019 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//

//
// This class exists because the wxFileTipProvider does not translate tips.
// This is a bug in wxWidgets. This class can be removed after this bug is fixed
// and is common in wxWidgets libraries distributed all over.
//
//  SPDX-License-Identifier: GPL-2.0+

#include "TipOfTheDay.h"
#include <wx/config.h>
#include <wx/display.h>
#include <wx/persist.h>
#include <wx/persist/toplevel.h>
#include "SvgBitmap.h"
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/txtstrm.h>
#include "Image.h"
#include <cstdlib>

#define ICON_SCALE (0.35)

TipOfTheDay::TipOfTheDay(wxWindow *parent)
  : wxDialog(parent,-1,_("Tip of the day"))
{
  m_tips.Add(
    _("To start using wxMaxima right away, start typing your command. An input cell should appear. Then press Shift-Enter to evaluate your command.")
    );
  m_tips.Add(
    _("By default, Shift-Enter is used to evaluate commands, while Enter is used for multiline input. This behaviour can be changed in 'Edit->Configure' dialog by checking 'Enter evaluates cells'. This switches the roles of these two key commands.")
    );
  m_tips.Add(
    _("Maxima uses ':' to assign values to a variable ('a : 3;'), not '=' as most programming languages do.\nThis allows, that a mathematical equation can be assigned to a variable, e.g.\n eq1:3*x=4;\nwhich may then be solved for x with\nsolve(eq1,x);")
    );
  m_tips.Add(
    _("Maxima uses ':=' to define functions, e.g.:\nf(x) := x^2;")
    );
  m_tips.Add(
    _("You can access the last output using the variable '%'. You can access the output of previous commands using variables '%on' where n is the number of output.")
    );
  m_tips.Add(
    _("Maxima supports three types of numbers: exact fractions (which can be generated for example by typing 1/10), IEEE floating-point numbers (0.2) and arbitrary precision big floats (1b-1). Note that, owing to their nature as binary (not decimal) numbers, there is for example no way to generate an IEEE floating-point number that exactly equals 0.1. If floating-point numbers are used instead of fractions, Maxima will therefore sometimes have to introduce a (very small) error and use things like 3602879701896397/36028797018963968 for 0.1.")
    );
  m_tips.Add(
    _("If you type an operator (one of +*/^=,) as the first symbol in an input cell, % will be automatically inserted before the operator, as on a graphing calculator. You can disable this feature from the 'Edit->Configure' dialog.")
    );
  m_tips.Add(
    _("You can insert different types of 'cells' in wxMaxima documents using the 'Cell' menu. Note that only 'input cells' can be evaluated, while others are used for commenting and structuring your calculations.")
    );
  m_tips.Add(
    _("A new document format has been introduced in wxMaxima 0.8.2 that saves not only your input and text commentaries, but also the outputs of your calculations. When saving your document, select 'Whole document (*.wxmx)' format.")
    );
  m_tips.Add(
    _("Title, section and subsection cells can be folded to hide their contents. To fold or unfold, click in the square next to the cell. If you shift-click, all sublevels of that cell will also fold/unfold.")
    );
  m_tips.Add(
    _("You can hide output part of cells by clicking in the triangle on the left side of cells. This works on text cells also.")
    );
  m_tips.Add(
    _("There are many resources about Maxima and wxMaxima on the internet. Visit https://wxMaxima-developers.github.io/wxmaxima/help.html for more information and to find tutorials on using wxMaxima and Maxima.")
    );
  m_tips.Add(
    _("You can get help on a Maxima function by selecting or clicking on the function name and pressing F1. wxMaxima will search the help index for the selection or the word under the cursor.")
    );
  m_tips.Add(
    _("A 'horizontal cursor' was introduced in wxMaxima 0.8.0. It looks like a horizontal line between cells. It indicates where a new cell will appear if you type or paste text or execute a menu command.")
    );
  m_tips.Add(
    _("Horizontal cursor works like a normal cursor, but it operates on cells: press up or down arrow to move it, holding down Shift while moving will select cells, pressing backspace or delete twice will delete a cell next to it.")
    );
  m_tips.Add(
    _("Selecting a part of an output and right-clicking on the selection will bring up a menu with convenient functions that will operate on the selection.")
    );
  m_tips.Add(
    _("You can select multiple cells either with a mouse - click'n'drag from between cells or from a cell bracket on the left - or with a keyboard - hold down Shift while moving the horizontal cursor - and then operate on selection. This comes in handy when you want to delete or evaluate multiple cells.")
    );
  m_tips.Add(
    _("You can evaluate your whole document by using 'Cell->Evaluate All Cells' menu command or the appropriate key shortcut. The cells will be evaluated in the order they appear in the document.")
    );
  m_tips.Add(
    _("If your calculation is taking too long to evaluate, you can try 'Maxima->Interrupt' or 'Maxima->Restart Maxima' menu commands.")
    );
  m_tips.Add(
    _("To plot in polar coordinates, select 'set polar' in the Options entry for Plot2d dialog. You can also plot in spherical and cylindrical coordinates in 3D.")
    );
  m_tips.Add(
    _("wxMaxima dialogs set default values for inputs entries, one of which is '%'. If you have made a selection in your document, the selection will be used instead of '%'.")
    );
  m_tips.Add(
    _("To put parentheses around an expression, select it, and press '(' or ')' depending on where you want the cursor to appear afterwards.")
    );
  m_tips.Add(
    _("When applying functions with one argument from menus, the default argument is '%'. To apply the function to some other value, select it in the document before executing a menu command.")
    );
  m_tips.Add(
    _("To save the size and position of wxMaxima windows between session, use 'Edit->Configure' dialog.")
    );
  m_tips.Add(
    _("Since wxMaxima 0.8.2 you can also insert images into your documents. Use 'Cell->Insert Image...' menu command.")
    );
  m_tips.Add(
    _("Besides the global undo functionality that is active when the cursor is between cells, wxMaxima has a per-cell undo function that is active if the cursor is inside a cell. Pressing Ctrl+Z inside a cell can therefore been used for a fine-pitch undo that doesn't affect latter changes made in other cells.")
    );
  m_tips.Add(
    _("Pressing Ctrl+Space or Ctrl+Tab starts an autocomplete function that can not only complete all functions that are integrated into the Maxima core and their parameters: It also knows about parameters from currently loaded packages and from functions that are defined in the current file.")
    );
  m_tips.Add(
    _("It is possible to define reusable Maxima libraries with wxMaxima that can be later loaded by using the load() function. All that has to be done is to export a file in the .mac or to save it in the .wxm format. Note, though, that a few special characters like the \"not equal\" symbol for \"#\" are handled by wxMaxima, not by Maxima and therefore cannot be recognized on load().")
    );
  m_tips.Add(
    _("wxMaxima can be made to execute commands at every start-up by placing them in a text file with the name wxmaxima.rc in the user directory. This directory can be found by typing maxima_userdir;")
    );
  m_tips.Add(
    _("Libraries can be accessed by any wxMaxima process regardless of which directory it runs in, if they are placed in the user directory. This directory can be found by typing maxima_userdir;")
    );
  m_tips.Add(
    _("The load() command of Maxima versions >5.38 is able to load .wxm files as library of Maxima's commands.")
    );
  m_tips.Add(
    _("A plot to be embedded into the work sheet by preceding its name with a \"wx\". \"draw\" can be replaced by \"wxdraw\", plot by \"wxplot\" etc.")
    );
  m_tips.Add(
    _("wxMaxima provides the \"table_form\" command that shows lists in an alternative form. One example would be:\n\ntable_form([1,2,3,4,5]);")
    );
  m_tips.Add(
    _("In order to visualize how a parameter affects an equation wxMaxima provides commands that start with \"with_slider_\" and that create animations. One example would be:\n\n    with_slider_draw(\n        a,[-16,-9,-4,-2,0,1,2,3,4,5,6,7],\n        title=concat(\"a=\",a),\n        grid=true,\n        explicit(\n            x^2-a*x,\n            x,-10,10\n        )\n    );")
    );
  m_tips.Add(
    _("If the \"Autosave\" functionality is enabled in the configuration dialogue wxMaxima acts like mobile apps that backup the file every few minutes and save it on closing.")
    );
  m_tips.Add(
    _("It is possible to add custom symbols to the \"symbols\" sidebar by copying them into the respective field in the configuration dialogue.")
    );
  m_tips.Add(
    _("Maxima's strengths are manipulating equations and in symbolic calculations. It therefore makes sense to use functions (as opposed to equations with labels) sparingly and to keep the actual values of variables in a list, instead of directly assigning them values. An example session that does do so would be:\n\n/* We keep the actual values in a list so we can use them later on */\n    Values:[a=10,c=100];\n    Pyth:a^2+b^2=c^2;\n    solve(%,b);\n    result:%[2];\n    at(result,Values);\n    float(%);")
    );
  m_tips.Add(
    _("Equations have several advantages over functions. For example they can be manipulated with factor(), expand() and similar functions. They can easily be introduced one into another. Also they are always printed out as 2D maths.")
    );
  m_tips.Add(
    _("Maxima's \"at\" function allows to access to arbitrary variables in a list of results:\n\n    g1:a*x+y=0;\n    g2:b*y+x*x=1;\n    solve([g1,g2],[a,b]);\n    %[1];\n    result_b:b=at(b,%);")
    );
  m_tips.Add(
    _("The \"at\" function allows to introduce one equation into another:\n\n    ohm:U=R*I;\n    r_parallel:R=R_1*R_2/(R_1+R_2);\n    result:at(ohm,r_parallel);")
    );
  m_tips.Add(
    _("The rhs() (\"right hand side\") command allows to retrieve the result of an equation in exactly the format a function would have:\n\n    Values:[\n        /* m=1.2 tons */\n        m=1.2*10^3,\n        /* 100 km/h*/\n        v=100*10^3/(60*60)\n    ];\n    Energy:W=1/2*m*v^2;\n    at(Energy,Values);\n    W_mech:rhs(%);")
    );
  m_tips.Add(
    _("In text cells bullet lists can be created by beginning a line with \" * \". The number of spaces in front of the \"*\" determines the indentation level; Indentation can be continued in the next line by indenting the line using spaces.")
    );
  m_tips.Add(
    _("Text cells can hold citations that as marked as such by beginning a line with \" > \". The number of spaces in front of the \">\" determines the indentation level, as it does with bullet lists.")
    );
  m_tips.Add(
    _("The key combination Shift+Space results in a non-breakable space.")
    );
  m_tips.Add(
    _("wxMaxima needs more translators. We do not speak every language. Help us to translate wxMaxima and the manual to your language. Join the wxMaxima development at: https://github.com/wxMaxima-developers/wxmaxima")
    );
  m_tips.Add(
    _("wxMaxima needs more developers. It is an open source project where you can contribute. Join the wxMaxima development at: https://github.com/wxMaxima-developers/wxmaxima")
    );
  m_tips.Add(
    _("You found a bug in wxMaxima? Please report it at https://github.com/wxMaxima-developers/wxmaxima/issues\nA mathematical bug is maybe a bug in Maxima (the backend), which can be reported at https://sourceforge.net/p/maxima/bugs/\nPlease check, before, if the bug was not already reported.")
    );
  m_tips.Add(
    _("You got a idea for an improvement of wxMaxima? Great! Please submit your idea at https://github.com/wxMaxima-developers/wxmaxima/issues. ")
    );
  m_tips.Add(
    _("wxMaxima is programmed in C++ using the wxWidgets toolkit and uses CMake as build system. You can contribute to wxMaxima, if you have some knowledge with this environment.")
    );

  m_num = 0;
  wxConfigBase *config = wxConfig::Get();
  config->Read(wxT("tipNum"), &m_num);
  if(m_num < 0)
    m_num = m_tips.GetCount()-1;
  if((unsigned)m_num >=m_tips.GetCount())
    m_num = 0;

  SetName("TipOfTheDay");
  SetTitle(_("Tip of the day"));
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *hbox = new wxBoxSizer(wxHORIZONTAL);
  wxButton *backButton = new wxButton(this,-1);
  backButton->SetBitmap(
    GetImage(
      media_playback_start_reverse_svg_gz,media_playback_start_reverse_svg_gz_len
      )
    );
  backButton->Connect(
    wxEVT_BUTTON,
    wxCommandEventHandler(TipOfTheDay::OnPreviousButton),
    NULL, this
    );
  hbox->Add(backButton,wxSizerFlags().Expand());
  hbox->Add(new wxStaticText(this, -1,_("Did you know?"),
                             wxDefaultPosition,wxDefaultSize,
                             wxALIGN_CENTRE_HORIZONTAL),
            wxSizerFlags().Proportion(10).Center());
  wxButton *forwardButton = new wxButton(this,-1);
  forwardButton->SetBitmap(
    GetImage(
      media_playback_start_svg_gz,media_playback_start_svg_gz_len
      )
    );
  forwardButton->Connect(
    wxEVT_BUTTON,
    wxCommandEventHandler(TipOfTheDay::OnNextButton),
    NULL, this
    );
  hbox->Add(forwardButton,wxSizerFlags().Expand());
  vbox->Add(hbox, wxSizerFlags().Expand());

  m_tip = new wxTextCtrl(this,-1,m_tips[m_num],
                         wxDefaultPosition,wxDefaultSize,
                         wxTE_READONLY | wxTE_MULTILINE
    );
  vbox->Add(m_tip, wxSizerFlags().Expand().Proportion(10));

  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);

  m_showAtStartup = new wxCheckBox(this, -1, _("Show tips at Startup"));
  bool showTip = true;
  config->Read(wxT("ShowTips"), &showTip);
  m_showAtStartup->SetValue(showTip);
  buttonSizer->Add(m_showAtStartup, wxSizerFlags().Expand());
  buttonSizer->AddStretchSpacer(5);

  wxButton *okButton = new wxButton(this, wxID_OK, _("OK"));
  okButton->Connect(
    wxEVT_BUTTON,
    wxCommandEventHandler(TipOfTheDay::OnOkButton),
    NULL, this
    );

  buttonSizer->Add(okButton);
  okButton->SetDefault();
  vbox->Add(buttonSizer, wxSizerFlags().Expand().Proportion(1));

  wxPersistenceManager::Get().RegisterAndRestore(this);
  SetSizerAndFit(vbox);
  config->Write(wxT("tipNum"), m_num + 1);
}

TipOfTheDay::~TipOfTheDay()
{
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxT("ShowTips"), m_showAtStartup->GetValue());
  config->Write(wxT("tipNum"), m_num + 1);
}


wxImage TipOfTheDay::GetImage(unsigned char *data, size_t len)
{
  int ppi;
#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;

  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    ppi = 72;
  else
    ppi = wxDisplay(display_idx).GetPPI().x;
#else
  ppi = wxGetDisplayPPI().x;
#endif
  if(ppi < 10)
    ppi = wxGetDisplayPPI().x;
  if(ppi <= 10)
    ppi = 72;

  int targetSize = wxMax(ppi,75) * ICON_SCALE;

  int sizeA = 128 << 4;
  while(sizeA * 3 / 2 > targetSize && sizeA >= 32) {
    sizeA >>= 1;
  };

  int sizeB = 192 << 4;
  while(sizeB * 4 / 3 > targetSize && sizeB >= 32) {
    sizeB >>= 1;
  }

  if(std::abs(targetSize - sizeA) < std::abs(targetSize - sizeB)) {
    targetSize = sizeA;
  } else {
    targetSize = sizeB;
  }

  wxImage img = SvgBitmap(data, len, targetSize, targetSize).ConvertToImage();

#if defined __WXMSW__
#if wxCHECK_VERSION(3, 1, 1)
  // MSW is notorious for having problems with transparent black pixels.
  // Let's see if we can avoid these problems by converting the alpha
  // channel to a mask even if that means we cannot antialias a transparent
  // and a colored pixel to a half-transparent one any more.
  img.ConvertAlphaToMask();
#endif
#endif

  img.Rescale(targetSize, targetSize, wxIMAGE_QUALITY_HIGH);
  return img;
}

void TipOfTheDay::OnNextButton(wxCommandEvent &WXUNUSED(dummy))
{
  m_num++;
  if((unsigned)m_num >=m_tips.GetCount())
    m_num = 0;
  m_tip->SetValue(m_tips[m_num]);
}

void TipOfTheDay::OnPreviousButton(wxCommandEvent &WXUNUSED(dummy))
{
  m_num--;
  if(m_num < 0)
    m_num = m_tips.GetCount()-1;
  m_tip->SetValue(m_tips[m_num]);
}

void TipOfTheDay::OnOkButton(wxCommandEvent &WXUNUSED(dummy))
{
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxT("ShowTips"), m_showAtStartup->GetValue());
  config->Write(wxT("tipNum"), m_num + 1);
  Destroy();
}
