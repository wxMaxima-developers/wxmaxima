#include "LoggingMessageDialog.h"

int LoggingMessageBox( 	const wxString &  	message,
				const wxString &  	caption,
				int  	style,
				wxWindow *  	parent,
				int  	x,
				int  	y  
				)
{
  wxLogMessage(message);
  return wxMessageBox(message, caption, style, parent, x, y);
}
