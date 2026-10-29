# Tells whoever builds wxMaxima which optional features this build will have,
# and for each one that is missing, why and how to get it.
#
# Several features silently depend on how wxWidgets itself was compiled
# (wxUSE_* switches in its setup.h), not just on whether wxMaxima found a
# library: a packager otherwise only learns from a bug report that their
# wxMaxima has no help sidebar, no AI sidebar or no scalable plots. The
# runtime counterparts of these checks are wxm::HelpBrowserDiagnosis(),
# AiProvider::SecretStoreAvailable() and SvgBitmap::TextRenderingDiagnosis();
# keep the wording here in step with theirs.
#
# Usage: include() this file once wxWidgets has been found, call
# wxm_feature_summary() at the very end of configuring, so that its output is
# the last thing the packager sees. Values that are only known inside a
# subdirectory are handed over as global properties:
#   WXM_FRIBIDI_VERSION    libfribidi's version, if it is used
#   WXM_WEBVIEW2_LOADER    the WebView2Loader.dll that is shipped (Windows)

include(CheckCXXSourceCompiles)

# Sets <result> to TRUE if the preprocessor condition <condition> holds once
# wxWidgets' own headers (and thereby its setup.h) are included.
#
# The test is re-run on every configure rather than cached: the result depends
# on which wxWidgets was found, which can change between runs without any of
# our cache variables changing.
function(wxm_wx_setup_check result condition)
  set(CMAKE_REQUIRED_QUIET ON)
  # Compile only: nothing is linked, so MSVC's auto-linking pragmas or a
  # missing library path can't make a condition look false.
  set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
  set(CMAKE_REQUIRED_INCLUDES ${wxWidgets_INCLUDE_DIRS})
  set(CMAKE_REQUIRED_DEFINITIONS "")
  foreach(def ${wxWidgets_DEFINITIONS})
    list(APPEND CMAKE_REQUIRED_DEFINITIONS "-D${def}")
  endforeach()
  string(REPLACE ";" " " CMAKE_REQUIRED_FLAGS "${wxWidgets_CXX_FLAGS}")
  unset(WXM_WX_SETUP_CHECK CACHE)
  check_cxx_source_compiles("
#include <wx/defs.h>
#if !(${condition})
#error condition not met
#endif
int main() { return 0; }
" WXM_WX_SETUP_CHECK)
  if(WXM_WX_SETUP_CHECK)
    set(${result} TRUE PARENT_SCOPE)
  else()
    set(${result} FALSE PARENT_SCOPE)
  endif()
  unset(WXM_WX_SETUP_CHECK CACHE)
endfunction()

# Prints one feature: its name, whether it is available and, indented below,
# the explanation lines given as further arguments.
function(wxm_feature_line name state)
  string(LENGTH "${name}" len)
  math(EXPR pad "40 - ${len}")
  if(pad LESS 1)
    set(pad 1)
  endif()
  string(REPEAT " " ${pad} spaces)
  message(STATUS "  ${name}:${spaces}${state}")
  foreach(line ${ARGN})
    message(STATUS "      ${line}")
  endforeach()
endfunction()

function(wxm_feature_summary)
  set(wxver "${wxWidgets_VERSION_STRING}")

  # If wxWidgets' headers can't be compiled against with the flags we have,
  # every wxUSE_* check below would report "no" for the wrong reason.
  wxm_wx_setup_check(wx_headers_usable "1")

  message(STATUS "")
  message(STATUS "Optional features of this wxMaxima build:")
  if(NOT wx_headers_usable)
    message(STATUS "  (Could not compile against wxWidgets' headers to read its setup.h,"
      " so what depends on how wxWidgets was built is unknown.)")
  endif()

  # --- The help sidebar ------------------------------------------------------
  set(help_name "Help sidebar (integrated manual)")
  if(WXM_DISABLE_WEBVIEW)
    if(WXM_WEBVIEW_AUTODISABLED)
      wxm_feature_line("${help_name}" "no"
        "wxWidgets' webview library was not found or is unusable."
        "Install it (e.g. libwxgtk-webview3.2-dev) and configure again. wxWidgets"
        "itself needs WebKit2GTK (Linux) or the WebView2 SDK (Windows) for it."
        "The manual opens in an external web browser instead.")
    else()
      wxm_feature_line("${help_name}" "no"
        "Disabled with WXM_DISABLE_WEBVIEW=ON. Drop that option to enable it."
        "The manual opens in an external web browser instead.")
    endif()
  elseif(NOT wx_headers_usable)
    wxm_feature_line("${help_name}" "yes (wxWebView, backend unknown)")
  elseif(WIN32)
    # Configuration::OfferInternalHelpBrowser() only offers the Edge backend:
    # the old Internet Explorer one can't display the manual properly.
    wxm_wx_setup_check(edge "defined(wxUSE_WEBVIEW_EDGE) && wxUSE_WEBVIEW_EDGE")
    wxm_wx_setup_check(edge_static
      "defined(wxUSE_WEBVIEW_EDGE_STATIC) && wxUSE_WEBVIEW_EDGE_STATIC")
    get_property(loader GLOBAL PROPERTY WXM_WEBVIEW2_LOADER)
    if(NOT edge)
      wxm_feature_line("${help_name}" "no"
        "wxWidgets was built without the Edge (WebView2) backend (wxUSE_WEBVIEW_EDGE=0)."
        "Rebuild wxWidgets with the WebView2 SDK and wxUSE_WEBVIEW_EDGE=1.")
    elseif(edge_static)
      wxm_feature_line("${help_name}" "yes (Edge/WebView2, loader linked statically)"
        "Needs the WebView2 runtime on the user's machine.")
    elseif(loader)
      wxm_feature_line("${help_name}" "yes (Edge/WebView2)"
        "Ships ${loader}."
        "Needs the WebView2 runtime on the user's machine.")
    else()
      wxm_feature_line("${help_name}" "only if WebView2Loader.dll is put next to wxmaxima.exe"
        "WebView2Loader.dll was not found next to wxWidgets."
        "Set WXM_WEBVIEW2_LOADER to the DLL (NuGet package Microsoft.Web.WebView2)"
        "to ship it. Also needs the WebView2 runtime on the user's machine.")
    endif()
  else()
    wxm_wx_setup_check(webkit2 "defined(wxUSE_WEBVIEW_WEBKIT2) && wxUSE_WEBVIEW_WEBKIT2")
    wxm_wx_setup_check(webkit "defined(wxUSE_WEBVIEW_WEBKIT) && wxUSE_WEBVIEW_WEBKIT")
    if(webkit2)
      wxm_feature_line("${help_name}" "yes (wxWebView, WebKit2GTK backend)")
    elseif(webkit)
      wxm_feature_line("${help_name}" "yes (wxWebView, WebKit backend)")
    else()
      wxm_feature_line("${help_name}" "probably not"
        "wxWidgets' webview library exists, but wxWidgets was built without a"
        "WebKit backend (wxUSE_WEBVIEW_WEBKIT2/wxUSE_WEBVIEW_WEBKIT are 0)."
        "Rebuild wxWidgets with WebKit2GTK (libwebkit2gtk-4.1-dev) installed.")
    endif()
  endif()

  # --- The AI chat sidebar ---------------------------------------------------
  set(ai_name "AI chat sidebar")
  if(NOT WXM_AI_TOOLS_REQUESTED)
    wxm_feature_line("${ai_name}" "no"
      "Disabled with WXM_USE_AI_TOOLS=OFF. Set it to ON to enable it.")
  elseif(NOT WXM_USE_AI_TOOLS)
    wxm_feature_line("${ai_name}" "no"
      "wxWidgets ${wxver} is older than ${WXM_AI_TOOLS_MIN_WXWIDGETS_VERSION}, the first version"
      "it works reliably with. Build against wxWidgets >= ${WXM_AI_TOOLS_MIN_WXWIDGETS_VERSION}.")
  elseif(NOT wx_headers_usable)
    wxm_feature_line("${ai_name}" "yes, if wxWidgets has a secret store")
  else()
    # API keys are kept only in the OS' secret store; without one the sidebar
    # is never shown (AiProvider::SecretStoreAvailable()).
    wxm_wx_setup_check(secretstore "defined(wxUSE_SECRETSTORE) && wxUSE_SECRETSTORE")
    if(secretstore)
      if(APPLE OR WIN32)
        wxm_feature_line("${ai_name}" "yes")
      else()
        wxm_feature_line("${ai_name}" "yes"
          "Shown only where a secret service (GNOME Keyring, KWallet, ...) is running.")
      endif()
    else()
      if(APPLE OR WIN32)
        set(how "Rebuild wxWidgets with wxUSE_SECRETSTORE=1.")
      else()
        set(how "Rebuild wxWidgets with libsecret-1-dev installed (wxUSE_SECRETSTORE=1).")
      endif()
      wxm_feature_line("${ai_name}" "no"
        "wxWidgets was built without wxSecretStore (wxUSE_SECRETSTORE=0), and API keys"
        "are only ever stored there, so the sidebar is never shown."
        "${how}")
    endif()
  endif()

  # --- Scalable (SVG) plots --------------------------------------------------
  # Plots are only made as SVG if the renderer draws text, see
  # SvgBitmap::RendersText(). wxBitmapBundle::FromSVG() uses NanoSVG, which
  # draws no text, whenever it is compiled in -- even next to LunaSVG.
  set(svg_name "Scalable (SVG) plots")
  set(svg_how "Needs a wxWidgets >= 3.3.2 built with wxUSE_LUNASVG=1 and wxUSE_NANOSVG=0.")
  if(NOT wx_headers_usable)
    wxm_feature_line("${svg_name}" "unknown")
  else()
    wxm_wx_setup_check(nanosvg "defined(wxUSE_NANOSVG) && wxUSE_NANOSVG")
    wxm_wx_setup_check(lunasvg "defined(wxUSE_LUNASVG) && wxUSE_LUNASVG")
    if(nanosvg AND lunasvg)
      wxm_feature_line("${svg_name}" "no (plots are PNG)"
        "wxWidgets was built with both NanoSVG and LunaSVG, and NanoSVG, which"
        "draws no text, wins. Rebuild wxWidgets with wxUSE_NANOSVG=0.")
    elseif(nanosvg)
      wxm_feature_line("${svg_name}" "no (plots are PNG)"
        "wxWidgets renders SVG with NanoSVG, which draws no text."
        "${svg_how}")
    elseif(lunasvg)
      if(APPLE OR WIN32)
        set(font "Arial")
      else()
        set(font "DejaVu Sans")
      endif()
      wxm_feature_line("${svg_name}" "yes (LunaSVG)"
        "Needs ${font} in the system font directories at run time.")
    else()
      wxm_feature_line("${svg_name}" "no (plots are PNG)"
        "wxWidgets was built without an SVG renderer."
        "${svg_how}")
    endif()
  endif()

  # --- Bidirectional text ----------------------------------------------------
  set(bidi_name "Accurate right-to-left text editing")
  get_property(fribidi GLOBAL PROPERTY WXM_FRIBIDI_VERSION)
  if(fribidi)
    wxm_feature_line("${bidi_name}" "yes (libfribidi ${fribidi})")
  elseif(NOT WXM_USE_FRIBIDI)
    wxm_feature_line("${bidi_name}" "no"
      "Disabled with WXM_USE_FRIBIDI=OFF. Set it to ON to enable it.")
  else()
    wxm_feature_line("${bidi_name}" "no"
      "libfribidi was not found. Install libfribidi-dev (or your distribution's"
      "equivalent) and configure again.")
  endif()

  message(STATUS "In wxMaxima, Help > Troubleshooting shows what is usable at run time.")
  message(STATUS "")
endfunction()
