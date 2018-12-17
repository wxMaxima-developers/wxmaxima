
#include <LogPane.h>

LogPane::LogPane(wxWindow *parent, wxWindowID id) : wxPanel(parent, id)
{
  wxBoxSizer *vbox  = new wxBoxSizer(wxVERTICAL);

  wxTextCtrl *textCtrl = new wxTextCtrl(this, -1, wxEmptyString, wxDefaultPosition,
					wxDefaultSize,
					wxTE_MULTILINE | wxTE_READONLY | wxHSCROLL);

  vbox->Add(textCtrl, wxSizerFlags().Expand().Proportion(10));

  SetSizerAndFit(vbox);
  wxLog::SetActiveTarget(m_logPanelTarget = new wxLogTextCtrl(textCtrl));
  m_errorRedirector = new ErrorRedirector(new wxLogGui());

  // m_logPanelTarget->SetRepetitionCounting();
  // m_logPanelTarget->DisableTimestamp();
  SetMinSize(wxSize(wxSystemSettings::GetMetric ( wxSYS_SCREEN_X )/10,
                    wxSystemSettings::GetMetric ( wxSYS_SCREEN_Y )/10));
}

LogPane::~LogPane()
{
  // m_logPanelTarget is automatically destroyed in this step..
  wxDELETE(m_errorRedirector);
  wxLog::SetActiveTarget(new wxLogGui());
  m_errorRedirector = NULL;
  m_logPanelTarget = NULL;
}
