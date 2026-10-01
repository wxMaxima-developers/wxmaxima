; Lets the Windows installer run without administrator rights (GH #2298).
;
; This file is spliced into CPack's own NSIS template via CPACK_NSIS_DEFINES
; (see src/CMakeLists.txt), which CPack places right after the template's
; hard-coded "RequestExecutionLevel admin". That is why it can override it:
; NSIS keeps the last value an installer attribute is given.
;
; With "highest" the installer behaves like this:
;  - A user who can become administrator gets the usual UAC prompt and an
;    all-users install into Program Files, exactly as before.
;  - A standard user gets no UAC prompt at all. CPack's template already
;    handles that case ("JustMe": shortcuts, uninstall entry and settings go
;    to the user's own part of the registry and Start Menu); it only picks
;    a poor default folder for it, the user's Documents folder, which is
;    often synced to the cloud. The function below moves that default to
;    %LOCALAPPDATA%\Programs, where Windows expects per-user programs.
;
; wxMaxima registers its file associations itself, per user (HKCU, see
; wxMaxima.cpp), so a per-user install loses nothing.

RequestExecutionLevel highest

!define MUI_CUSTOMFUNCTION_GUIINIT wxmPerUserInstallDir

; Runs after the template's .onInit has decided between "AllUsers" and
; "JustMe". $SV_ALLUSERS and $IS_DEFAULT_INSTALLDIR are the template's own
; variables, declared above the point this file is included. Only the
; default folder is changed: a folder the user typed (/D=...) is left alone.
;
; Limitation: NSIS skips .onGUIInit in a silent (/S) install, so a silent
; per-user install still defaults to the Documents folder; pass /D=... there.
Function wxmPerUserInstallDir
  StrCmp $SV_ALLUSERS "JustMe" 0 wxmPerUserDone
  StrCmp $IS_DEFAULT_INSTALLDIR "1" 0 wxmPerUserDone
  ; $INSTDIR is "$DOCUMENTS\<install directory>" here: keep the last part.
  StrLen $0 "$DOCUMENTS\"
  StrCpy $0 $INSTDIR "" $0
  StrCpy $INSTDIR "$LOCALAPPDATA\Programs\$0"
wxmPerUserDone:
FunctionEnd
