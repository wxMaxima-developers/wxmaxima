// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2011-2011 cw.ahbong <cw.ahbong@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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
//  SPDX-License-Identifier: GPL-2.0+

#include <utility>
#include <algorithm>
#include <memory>
#include <cstdlib>
#include <vector>
#include "WXMXformat.h"
#include "CellPointers.h"
#include "ErrorRedirector.h"
#include "cells/CellList.h"
#include "cells/ImgCell.h"
#include <wx/debug.h>
#include <wx/textbuf.h>
#include <wx/txtstrm.h>
#include <wx/tokenzr.h>
#include <wx/xml/xml.h>
#include <wx/mstream.h>
#include <wx/zipstrm.h>
#include <wx/clipbrd.h>
namespace Format {

  /*
  Save the data as wxmx file

  First saves the data to a backup file ending in .wxmx~ so if anything goes
  horribly wrong in this step all that is lost is the data that was input
  since the last save. Then the original .wxmx file is replaced in a
  (hopefully) atomic operation.
*/
  bool ExportToWXMX(GroupCell *cells, const wxString &file,
                    Configuration *configuration, CellPointers *cellPointers,
                    const std::vector<wxString> &variables, const GroupCell * const cursorCell) {
  // Show a busy cursor as long as we export a file.
  wxBusyCursor crs;
  // Clear the list of files we need to embed
  configuration->ClearFilesToSave();
  // Don't update the worksheet whilst exporting
  //  wxWindowUpdateLocker noUpdates(this);
  wxLogMessage(_("Starting to save the worksheet as .wxmx"));
  // delete temp file if it already exists
  wxString backupfile = file + wxS("~");
  if (wxFileExists(backupfile)) {
    if (!wxRemoveFile(backupfile))
      return false;
  }
  {
    wxFFileOutputStream out(backupfile);
    if (!out.IsOk())
      return false;
    {
      wxZipOutputStream zip(out);
      if (!zip.IsOk())
        return false;
      {
        wxLogMessage(_("Created a .zip archive (the .wxmx file technically is a .zip file)"));
        wxTextOutputStream output(zip);

        /* The first zip entry is a file named "mimetype": This makes sure that
           the mimetype is always stored at the same position in the file. This
           is common practice. One example from an ePub file:

           00000000  50 4b 03 04 14 00 00 08  00 00 cd bd 0a 43 6f 61
           |PK...........Coa| 00000010  ab 2c 14 00 00 00 14 00  00 00 08 00 00
           00 6d 69  |.,............mi| 00000020  6d 65 74 79 70 65 61 70  70 6c
           69 63 61 74 69 6f  |metypeapplicatio| 00000030  6e 2f 65 70 75 62 2b
           7a  69 70 50 4b 03 04 14 00  |n/epub+zipPK....|

        */

        // Make sure that the mime type is stored as plain text.
        //
        // We will keep that setting for the rest of the file for the following
        // reasons:
        //  - Compression of the .zip file won't improve compression of the
        //  embedded .png images
        //  - The text part of the file is too small to justify compression
        //  - not compressing the text part of the file allows version control
        //  systems to
        //    determine which lines have changed and to track differences
        //    between file versions efficiently (in a compressed text virtually
        //    every byte might change when one byte at the start of the
        //    uncompressed original is)
        //  - and if anything crashes in a bad way chances are high that the
        //  uncompressed
        //    contents of the .wxmx file can be rescued using a text editor.
        //  Who would - under these circumstances - care about a kilobyte?
        zip.SetLevel(0);
        zip.PutNextEntry(wxS("mimetype"));
        output << wxS("text/x-wxmathml");
        zip.CloseEntry();
        wxLogMessage(_("Wrote the mimetype info"));
        zip.PutNextEntry(wxS("format.txt"));
        output << wxS(
                      "\n\nThis file contains a wxMaxima session in the .wxmx format.\n"
                      ".wxmx files are .xml-based files contained in a .zip container "
                      "like .odt\n"
                      "or .docx files. After changing their name to end in .zip the .xml "
                      "and\n"
                      "eventual bitmap files inside them can be extracted using any .zip "
                      "file\n"
                      "viewer.\n"
                      "The reason why part of a .wxmx file still might still seem to "
                      "make sense in a\n"
                      "ordinary text viewer is that the text portion of .wxmx by "
                      "default\n"
                      "isn't compressed: The text is typically small and compressing it "
                      "would\n"
                      "mean that changing a single character would (with a high "
                      "probability) change\n"
                      "big parts of the  whole contents of the compressed .zip archive.\n"
                      "Even if version control tools like git and svn that remember all "
                      "changes\n"
                      "that were ever made to a file can handle binary files compression "
                      "would\n"
                      "make the changed part of the file bigger and therefore seriously "
                      "reduce\n"
                      "the efficiency of version control\n\n"
                      "wxMaxima can be downloaded from "
                      "https://github.com/wxMaxima-developers/wxmaxima.\n"
                      "It also is part of the windows installer for maxima\n"
                      "(https://wxmaxima-developers.github.io/wxmaxima/).\n\n"
                      "If a .wxmx file is broken but the content.xml portion of the file "
                      "can still be\n"
                      "viewed using a text editor just save the xml's text as "
                      "\"content.xml\"\n"
                      "and try to open it using a recent version of wxMaxima.\n"
                      "If it is valid XML (the XML header is intact, all opened tags are "
                      "closed again,\n"
                      "the text is saved with the text encoding \"UTF8 without BOM\" and "
                      "the few\n"
                      "special characters XML requires this for are properly escaped)\n"
                      "chances are high that wxMaxima will be able to recover all code "
                      "and text\n"
                      "from the XML file.\n\n");
        zip.CloseEntry();

        // next zip entry is "content.xml", xml of cells

        zip.PutNextEntry(wxS("content.xml"));
        wxString xmlText;

        xmlText << wxS("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        xmlText << wxS("\n<!--   Created using wxMaxima ") << wxS(WXMAXIMA_VERSION)
                << wxS("   -->");
        xmlText << wxS(
                       "\n<!--https://wxMaxima-developers.github.io/wxmaxima/-->\n");

        // write document
        xmlText << wxS("\n<wxMaximaDocument version=\"");
        xmlText << DOCUMENT_VERSION_MAJOR << wxS(".");
        xmlText << DOCUMENT_VERSION_MINOR << wxS("\" zoom=\"");
        xmlText << int(100.0 * configuration->GetZoomFactor()) << wxS("\"");

        std::size_t ActiveCellNumber = 0;

        // We want to save the information that the cursor is in the nth cell.
        // Count the cells until then.
        bool found = false;
        if (cells)
          for (const GroupCell &tmp : OnList(cells)) {
            if (&tmp == cursorCell) {
              found = true;
              break;
            }
            ActiveCellNumber++;
          }

        // Paranoia: Test if we did find the cursor
        if (cells && found)
          // If we know where the cursor was we save this piece of information.
          // If not we omit it.
          xmlText << wxString::Format(wxS(" activecell=\"%li\""),
                                      static_cast<long>(ActiveCellNumber));

        // Save the variables list for the "variables" sidepane.
        if (variables.size() > 1) {
          std::size_t varcount = variables.size() - 1;
          xmlText += wxString::Format(" variables_num=\"%li\"", static_cast<long>(varcount));
          for (std::size_t i = 0; i < variables.size(); i++)
            xmlText +=
              wxString::Format(" variables_%li=\"%s\"", static_cast<long>(i),
                               Cell::XMLescape(variables.at(i)).utf8_str());
        }

        xmlText << ">\n";

        // Reset image counter
        cellPointers->WXMXResetCounter();

        if (cells)
          xmlText += cells->ListToXML();

        // Delete all but one control character from the string: there should be
        // no way for them to enter this string, anyway. But sometimes they
        // still do...
        for (wxString::const_iterator it = xmlText.begin(); it != xmlText.end();
             ++it) {
          wxChar c = *it;
          if ((c < wxS('\t')) || ((c > wxS('\n')) && (c < wxS(' '))) ||
              (c == wxChar(static_cast<char>(0x7F)))) {
            // *it = wxS(' ');
          }
        }

        xmlText += wxS("\n</wxMaximaDocument>");
        wxLogMessage(_("Generated the XML representation of the document"));

        {
          // Let wxWidgets test if the document can be read again by the XML
          // parser before the user finds out the hard way.
          {
            wxXmlDocument doc;
            {
              wxMemoryOutputStream ostream;
              wxTextOutputStream txtstrm(ostream);
              txtstrm.WriteString(xmlText);
              wxMemoryInputStream istream(ostream);
              wxLogNull suppressErrorMessages;
              doc.Load(istream);
            }

            // If we fail to load the document we abort the safe process as it
            // will only destroy data. But we can still put the erroneous data
            // into the clipboard for debugging purposes.
            if (!doc.IsOk()) {
              if (wxTheClipboard->Open()) {
                wxDataObjectComposite *data = new wxDataObjectComposite;
                data->Add(new wxTextDataObject(xmlText));
                wxTheClipboard->SetData(data);
                LoggingMessageDialog dialog(NULL, _("Produced invalid XML. The erroneous XML data has "
                                                    "therefore not been saved but has been put on the "
                                                    "clipboard in order to allow to debug it."),
                                            _("Error"), wxCENTER | wxOK);
                dialog.ShowModal();
                wxTheClipboard->Close();
              }

              return false;
            }
          }
          wxLogMessage(_("Validated that the XML representation of the document actually is valid XML"));

          // wxWidgets could pretty-print the XML document now. But as no-one
          // will look at it, anyway, there might be no good reason to do so.
          output << xmlText;
          zip.CloseEntry();
          wxLogMessage(_("Wrote the XML representation of the document to the zip archive"));

          // Move all files we have stored in memory during saving to zip file
          zip.SetLevel(0);
          for (const auto &fil: configuration->GetFilesToSave())
            {
              zip.PutNextEntry(fil.FileName());
              zip.Write(fil.Data().GetData(), fil.Data().GetDataLen());
              zip.CloseEntry();
            }
          wxLogMessage(_("Wrote all image and gnuplot files to the zip archive"));
        }
      }
      if (!zip.Close())
        {
          LoggingMessageDialog dialog(NULL, _("Could not write the file's contents during saving => aborting."),
                                      _("Error"), wxCENTER | wxOK);
          dialog.ShowModal();
          return false;
        }
    }
    if (!out.Close())
      {
        LoggingMessageDialog dialog(NULL, _("Could not create the backup file during saving => aborting."),
                                    _("Error"), wxCENTER | wxOK);
        dialog.ShowModal();
        return false;
      }
  }
  wxLogMessage(_("Closed the zip archive"));
  // If all data is saved now we can overwrite the actual save file.
  // We will try to do so a few times if we suspect a MSW virus scanner or
  // similar temporarily hindering us from doing so.

  // The following line is paranoia as closing (and thus writing) the file has
  // succeeded.
  if (!wxFileExists(backupfile))
    {
      LoggingMessageDialog dialog(NULL, _("Saving succeeded, but the resulting files has disappeared ?!?."),
                                  _("Error"), wxCENTER | wxOK);
      dialog.ShowModal();
      return false;
    }

  // Now we try to open the file in order to see if saving hasn't failed
  // without returning an error - which can apparently happen on MSW.
  {
    wxFileInputStream wxmxFile(file + "~");
    wxZipInputStream wxmxContents(wxmxFile);
    wxZipEntry *contentsEntry = NULL;
    while(!wxmxContents.Eof())
      {
        contentsEntry = wxmxContents.GetNextEntry();
        if((!contentsEntry) || (contentsEntry->GetName() == wxS("content.xml")))
          break;
      }
    // Did we succeed in opening the file?
    if (!contentsEntry) {
      LoggingMessageDialog dialog(NULL, _(wxS("Saving succeeded, but the file could not be read "
                                              "again \u21D2 Not replacing the old saved file.")),
                                  _("Error"), wxCENTER | wxOK);
      dialog.ShowModal();
      return false;
    }
  }
  wxLogMessage(_("Verified that we are able to read the whole .zip archive we produced."));

  {
    bool done;
    SuppressErrorDialogs suppressor;
    done = wxRenameFile(backupfile, file, true);
    if (!done) {
      // We might have failed to move the file because an over-eager virus
      // scanner wants to scan it and a design decision of a filesystem driver
      // might hinder us from moving it during this action => Wait for a second
      // and retry.
      wxSleep(1);
      done = wxRenameFile(backupfile, file, true);
    }
    if (!done) {
      // We might have failed to move the file because an over-eager virus
      // scanner wants to scan it and a design decision of a filesystem driver
      // might hinder us from moving it during this action => Wait for a second
      // and retry.
      wxSleep(1);
      done = wxRenameFile(backupfile, file, true);
    }
    if (!done) {
      wxSleep(1);
      if (!wxRenameFile(backupfile, file, true))
        {
          LoggingMessageDialog dialog(NULL, _(wxS("Creating a backup file succeeded, but could not move the .wxmx file to the intended location.")),
                                      _("Error"), wxCENTER | wxOK);
          dialog.ShowModal();
          return false;
        }
    }
    wxLogMessage(_("wxmx file saved"));
  }
  return true;
}


} // namespace Format
