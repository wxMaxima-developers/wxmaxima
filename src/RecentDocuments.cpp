#include "RecentDocuments.h"

RecentDocuments::RecentDocuments(wxString documentType)
{
  m_documentType = documentType;
}

void RecentDocuments::Load()
{
  wxConfigBase *config = wxConfig::Get();
  wxString autoSaveFiles;

  // Read the old list from the config
  config->Read(m_documentType,&autoSaveFiles);
  wxStringTokenizer files(autoSaveFiles, wxT(";"));
  
  while(files.HasMoreTokens())
    {
      wxString filename = files.GetNextToken();
      if(filename != wxEmptyString)
	{
	  if(wxFileExists(filename))
	    {
	      m_listOfFiles.push_back(filename);
	    }
	}
    }
  m_listOfFiles.unique();

  Save();
}

void RecentDocuments::Save()
{
  wxConfigBase *config = wxConfig::Get();
  wxString autoSaveFiles;
  for(std::list<wxString>::iterator it = m_listOfFiles.begin(); it != m_listOfFiles.end();++it)
    {
      autoSaveFiles += *it + wxString(wxT(";"));
    }
  config->Write(m_documentType, autoSaveFiles);
}

void RecentDocuments::AddDocument(wxString filename)
{
  m_listOfFiles.push_front(filename);

  long recentItems;
  wxConfig::Get()->Read(wxT("recentItems"), &recentItems);
  if (recentItems < 5) recentItems = 5;
  if (recentItems > 30) recentItems = 30;
  while(m_listOfFiles.size() > recentItems)
    m_listOfFiles.pop_back();
  Save();
}

