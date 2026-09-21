---
name: wxmaxima-ai-tools
description: How wxMaxima's MCP server and AI chat sidebar work - the read-only tool surface over the worksheet, the provider abstraction and its four-plus backends, API keys in the OS secret store, the status icon and connection monitor, and the traps each of them has already hit. Use when touching src/mcp/, src/ai/, AiChatSidebar, AiConnectionMonitor, the AI Chat options tab, or the WXM_USE_AI_TOOLS build option.
---

# The MCP server and the AI chat sidebar

Two related features that let an AI look at a worksheet: an **MCP server**, for
an external AI tool, and an **AI chat sidebar**, for when the AI tool is
wxMaxima's own UI. They share `McpTools` - the actual worksheet-reading logic -
so a change to what an AI can see usually belongs there rather than in either
front end.

Both are gated at compile time by `WXM_USE_AI_TOOLS`, and the sidebar is gated
again at runtime by whether an OS secret store is actually reachable.

**The single most important rule here: every tool is read-only except two
explicitly approved exceptions** (`watch_variable`/`unwatch_variable`). See the
last section for why a write- or evaluate-capable tool is a separate decision
and not a natural next step.

> **Provenance.** The entries below were moved here verbatim from
> `AGENTS.md`, which had grown past 4800 lines. Where one of them says
> "this file" about a *document* (as opposed to the C++ source it is
> discussing), it means `AGENTS.md` -- check there, and in the sibling
> skills, for anything it cross-references that is not here.

## The MCP server

- **MCP server (`src/mcp/McpServer.{h,cpp}`, `src/mcp/McpTools.{h,cpp}`,
  2026-09) -- lets an external AI tool read the current worksheet as
  context.** User request: "they say that there is a de facto standard on
  how to create an AI sidebar that allows to talk to an AI and gives it
  access to a worksheet" -- identified as MCP (Model Context Protocol).
  Scoped down with the maintainer to a first, self-contained PR before any
  in-app chat sidebar: an MCP server only, read-only except for two
  explicitly-approved exceptions (see below), no write/insert/edit/evaluate
  capability of any kind.
  - **Split for testability**: `McpTools` is the actual worksheet-reading
    logic (`ListCells`/`ReadCell`/`ReadWorksheet`/`ReadToc`/`ReadSection`/
    `ReadVariables`/`WatchVariable`/`UnwatchVariable`), with zero networking
    or JSON-RPC framing -- takes a `Worksheet*`/`Variablespane*`, returns
    `nlohmann::json`, throws `McpToolError` for a bad tool name/arguments.
    `McpServer` is purely the HTTP+JSON-RPC transport around it. This mirrors
    why `TableOfContents`'s own structure-walking isn't reused as-is: its
    public surface (`GetCell(displayedIndex)`, filtered by the TOC's own
    search box/depth setting) is display-oriented, not a clean data API, so
    `McpTools::ReadToc()` just re-walks `OnList(tree)` filtering
    `GroupCell::IsHeading()` directly instead -- the same primitive
    `TableOfContents::UpdateStruct()` itself uses.
  - **Why "read-only except two things" and not stricter**: the maintainer
    explicitly approved `watch_variable`/`unwatch_variable` (adding/removing
    a name from the Variables sidebar's watchlist) as safe despite being
    nominal writes -- they only change what that sidebar happens to be
    displaying, the same as a user typing a name into it by hand; they touch
    no worksheet content and cannot insert, edit or evaluate anything. Every
    other tool cannot mutate anything even in principle, since all they do
    is turn existing worksheet/cell state into text.
  - **`read_section`, added after the maintainer's own suggestion** ("AIs
    often [read] the table of contents [of] big documents ... perhaps
    another useful command would be read_section"): reads one whole
    section's text by its heading cell's UUID (from `read_toc`), so an AI
    can navigate a large worksheet via TOC -> one section at a time instead
    of pulling `read_worksheet`'s entire (size-capped) text or walking
    `list_cells` one UUID at a time. Its "how far does this section extend"
    walk is the exact same loop `GroupCell::Fold()` already uses (stop at
    the first following cell whose type equals, or is a higher heading
    level than, the section's own -- `GroupCell::IsLesserGCType()`, already
    public) -- reimplemented read-only rather than calling `Fold()` itself,
    since folding actually tears the range out of the tree
    (`CellList::TearOut`) and this must never touch the tree at all.
  - **Cell identity**: `Cell::GetUUID()`/`GenerateUUID()` (lazy -- empty
    until first needed, see `Cell.h`) is the only existing stable identifier
    a cell has; every tool that returns cells generates one on the fly for
    any cell that doesn't have one yet. This is a real, if minor, side
    effect worth knowing about: once an MCP tool has looked at a cell, that
    cell's UUID is no longer empty and will be written out on the next
    save -- same as opening the XML inspector or diff view already does
    elsewhere in this codebase, not something unique to this feature.
  - **`GroupCell::GetOutput()` deliberately skips the first cell in
    `m_output` -- that's the answer label ("(%o1)"), not part of the actual
    output text**, per its own doc comment (`GetLabel()` returns that first
    cell instead). `McpTools::OutputText()` relies on this to return clean
    output text with no label prefix -- confirmed both live (a real
    `--eval`-loaded worksheet's `read_cell` showed plain output text, no
    "(%o1)" noise) and by a test-fixture bug this exact behavior caused
    while writing `test_McpTools.cpp`: building a cell's test output as a
    single `TextCell` via `SetOutput()` alone left `GetOutput()` returning
    `nullptr` (nothing follows a lone cell), because a *real* Maxima
    response's output always starts with that label cell first -- the test
    fixture had to chain a label cell then the real content via
    `SetOutput()` followed by `AppendOutput()`, mirroring that real shape,
    before `GetOutput()` returned anything.
  - **Transport (`McpServer`)**: MCP's "Streamable HTTP", the read-only
    subset of it that needs no server-initiated push -- one HTTP endpoint
    (`POST /mcp`) answering one JSON-RPC 2.0 request per connection, closing
    afterwards (`Connection: close`, no keep-alive, no chunked encoding); a
    `GET /mcp` gets a plain 405, which the spec allows for a server with no
    SSE stream to offer. No `Mcp-Session-Id` bookkeeping either -- optional
    for the server to assign per spec, and every tool call here is
    independently answerable from live worksheet state with no session to
    track. Implemented directly on `wxSocketServer`/`wxSocketBase`
    (event-driven, `Notify()`-based, the same primitive `Maxima.cpp` already
    uses for the Maxima<->wxMaxima protocol -- just event-driven here
    instead of on `Maxima.cpp`'s dedicated worker thread) rather than
    pulling in an HTTP library: the request shape needed is tiny (one
    method, one path, a couple of headers, a Content-Length-delimited
    body), so a general-purpose HTTP server buys nothing. Runs entirely on
    the GUI thread's own event loop for exactly this reason -- it can call
    directly into `Worksheet`/`GroupCell`/`Variablespane` with zero
    marshaling, since none of them are thread-safe. Never make this
    transport threaded without adding the `CallAfter()`-based marshaling
    every cross-thread worksheet touch elsewhere in this codebase needs.
  - **JSON**: vendored `nlohmann/json.hpp` (single header, MIT, v3.11.3,
    `src/vendor/nlohmann/`) rather than a hand-rolled parser -- correctness
    matters here since real MCP clients need to interoperate with this, and
    a hand-rolled JSON parser is exactly the kind of "looks fine until an
    edge case" component not worth re-deriving for this. Wired in via
    `include_directories(SYSTEM ".../src/vendor")` in `src/CMakeLists.txt`
    (so its own warnings don't hit `-Werror` builds, the same reasoning
    `privateNanoSVG.cmake`'s pragma-based warning guard exists for, just via
    CMake's own mechanism since there's no symbol collision to rename around
    the way nanoSVG's vendoring needs) -- **this include path is
    directory-scoped and does not reach `test/unit_tests/`** (a sibling
    directory of `src/`, not a descendant -- the exact same trap
    `WXM_USE_FRIBIDI`'s include wiring already documents elsewhere in this
    file), so `test/unit_tests/CMakeLists.txt` needed the identical
    `include_directories(SYSTEM ...)` line added independently, or
    `test_McpTools`'s very first build failed with a plain "no such file"
    on `<nlohmann/json.hpp>` despite `wxmaxima` itself building cleanly.
  - **Safety**: binds only to `127.0.0.1` (`wxIPV4address::LocalHost()`),
    double-checks each accepted connection's peer address is loopback too
    (defense in depth beyond the bind itself), and validates the `Origin`
    header against localhost/127.0.0.1 when a client sends one (most
    non-browser MCP clients don't; browsers always do) -- this is
    specifically the MCP spec's own called-out DNS-rebinding mitigation: a
    malicious web page resolving an attacker-controlled hostname to
    127.0.0.1 so a victim's browser fetch reaches this server looking
    same-origin. Off by default (`Configuration::McpServerEnabled()`,
    opt-in via Options).
  - **wxSocketBase::Destroy(), not `delete`/a default-deleter
    `unique_ptr`, for every client connection** -- connections are always
    torn down (both the normal "request handled, close it" path and the
    `wxSOCKET_LOST` path) from inside that same socket's own `wxEVT_SOCKET`
    event handler call stack, which is exactly the situation wx's own docs
    say `Destroy()` (defers actual destruction rather than deleting
    mid-callback) exists for. `Connection` therefore holds a raw
    `wxSocketBase*`, not an owning smart pointer, and every removal site
    calls `->Destroy()` explicitly before erasing the connection from
    `McpServer::m_connections`. The listening `wxSocketServer` itself is a
    plain `std::unique_ptr` since it's only ever destroyed from
    `ReconcileWithConfig()`/`~McpServer()`, neither of which nests inside
    the server socket's own accept-event handler.
  - **Verification**: `test/unit_tests/test_McpTools.cpp` pins `McpTools`
    against a real, headless `Worksheet`/`Variablespane` (no live Maxima, no
    sockets -- the same `InsertGroupCells`/`SetOutput` pattern
    `test_WorksheetFind.cpp`/`test_AnonymizeCodeCells.cpp` already use) --
    cell listing/reading, TOC extraction, section boundaries (including a
    nested-subsection case), and the full watch/read/unwatch variable round
    trip, plus the "unknown tool name/UUID/missing argument throws
    `McpToolError`" error paths. The transport itself (`McpServer`) was
    verified live instead, since sockets/HTTP framing don't fit that
    fixture style: a real `-e`-loaded worksheet in a live Xvfb session,
    `curl`-ing every tool (`initialize`, `tools/list`, `tools/call` for all
    eight tools, including the full watch/read/unwatch flow against a real
    connected Maxima) plus the transport-level edge cases (`GET` -> 405, a
    non-localhost `Origin` -> 403, a localhost `Origin` -> 200, an unknown
    path -> 404, a JSON-RPC notification -> empty 202) -- then confirmed a
    clean shutdown (`SIGTERM`) leaves no lingering `wxmaxima`/`maxima`
    process and the port genuinely closes. Also confirmed the whole
    `ctest` suite's non-batch/non-live-Maxima tests still pass and the
    full tree rebuilds with zero new warnings.

## The AI chat sidebar

- **AI chat sidebar (`src/ai/AiProvider.{h,cpp}`, `src/sidebars/AiChatSidebar.{h,cpp}`)
  -- a follow-up to the MCP server above, for the case where the "AI tool"
  is wxMaxima's own UI rather than an external one.** A docked sidebar
  (View -> Sidebars -> AI Chat) that chats with Anthropic, OpenAI, Google
  Gemini or Qwen (via DashScope's OpenAI-compatible endpoint) using a
  pasted API key -- no OAuth, since none of these four offer a legitimate
  third-party OAuth flow for a desktop app to use. v1 is deliberately
  read-only and has no tool-calling: `AiChatSidebar::BuildContextSnapshot()`
  sends one plain-text snapshot of the worksheet (via the *same* `McpTools`
  the MCP server already uses, `McpTools::ReadWorksheet()`) as a system-
  style context message, truncated to `MAX_CONTEXT_LENGTH` (8000 chars);
  the model can discuss it but has no way to act on the worksheet, since
  wiring up real tool-calling (there are 8 tools now, MCP's `tools/call`
  can already run them) is real design work of its own -- a natural
  follow-up, not attempted here.
  - **Provider abstraction (`AiProvider.h`):** one small `AiProvider` base
    class per provider family, each implementing four things that differ
    per API -- `RequestUrl()`, `AuthHeaders()`, `BuildRequestBody()`,
    `ParseReply()` -- all pure string/JSON logic with no networking of its
    own, which is what makes `test/unit_tests/test_AiProvider.cpp` able to
    pin all four providers' request/response shapes with zero live network
    access (`#include "ai/AiProvider.cpp"` directly, same lightweight
    pattern as `test_MaximaProtocol.cpp`). OpenAI and Qwen share one
    `OpenAiCompatibleProvider` implementation (DashScope's compatible-mode
    endpoint is byte-for-byte the same `chat/completions` shape, only the
    URL differs) -- confirmed via `test_AiProvider.cpp`'s "Qwen, via
    DashScope's OpenAI-compatible endpoint" scenario, not assumed from the
    provider's marketing docs.
  - **Lifetime safety across the async boundary:** `MakeAiProvider()`
    returns a `std::shared_ptr<AiProvider>`, not `unique_ptr`, and the
    static `AiProvider::SendChat(std::shared_ptr<const AiProvider> self,
    ...)` takes that shared ownership explicitly and captures it by value
    into the completion lambda. This is load-bearing, not defensive
    overkill: `AiChatSidebar::m_provider` can be replaced mid-flight (the
    user opens Options and changes provider/model while a request is still
    in the air), and without the shared_ptr the callback's `self->Name()`/
    `self->ParseReply()` calls would use a dangling pointer to whatever the
    sidebar used to point at. `wxWebRequest` itself follows the exact same
    "local value, never stored anywhere else, goes out of scope right
    after `.Start()`" shape as the codebase's one prior use
    (`wxMaxima::CheckForUpdates()`) -- confirmed this is fine because
    `wxWebRequest` is a ref-counted handle wxWebSession itself keeps alive
    while the request is in flight, not something this code has to keep
    alive itself. The completion lambda is deliberately never `Unbind()`'d
    for the same reason `CheckForUpdates()`'s never is: wx's functor-based
    `Bind()` has no reliable identity to `Unbind()` a lambda by, and one
    small permanently-bound no-op-after-firing lambda per chat turn is not
    a real leak at realistic chat volumes. The request's id must be a real,
    unique one from `wxWindow::NewControlId()` (stored in a
    `wxWindowIDRef`, which releases it back to wx's finite id pool once
    nothing references it, unlike a plain `int`) -- with the default
    `wxID_ANY` every request would share one id and every past request's
    stale lambda would refire on every new request's completion.
  - **`wxWebRequest::State_Unauthorized` is not optional to handle -- an
    invalid API key hangs the chat forever, silently, if you only handle
    `State_Completed`/`State_Failed`/`State_Cancelled` (confirmed live,
    not guessed).** The natural-looking implementation handles exactly
    those three states and falls through to `default: break;` for
    anything else. Live-testing against the real Anthropic API with a
    deliberately wrong key (`sk-ant-fake-test-key-1234` -- this sandbox's
    `no_proxy` list happens to include `api.anthropic.com`, so direct
    outbound HTTPS to it works here without a real key or a full model
    call) reproduced a genuine hang: the sidebar sat on "Waiting for
    Anthropic..." with Send disabled for 7+ minutes and never recovered.
    Root-caused with `/proc/<pid>/fd` + `/proc/<pid>/net/tcp` (this
    sandbox has no `tcpdump`): a real ESTABLISHED TCP connection to
    `api.anthropic.com`'s actual IP was sitting open and idle (0 bytes in
    either queue), and a raw `curl` to the same endpoint with the same fake
    key returned instantly with a normal HTTP 401 -- so the network path
    itself was never the problem. Confirmed with temporary `wxLogMessage`
    tracing in the event handler (removed before committing) that
    `wxWebRequestEvent::GetState()` reports `State_Unauthorized` (value
    `1`) for this response, not `State_Completed` with `status==401` --
    wx's web-request backend intercepts any 401 (confirmed via a raw
    `curl -i` that Anthropic's actual 401 response carries no
    `WWW-Authenticate` header at all, so this isn't about a real HTTP-auth
    challenge, just the status code alone) and diverts it to this separate
    state, meant for the case where the app might retry with different
    credentials via `wxWebAuthChallenge::SetCredentials()`. None of these
    four providers use real HTTP Basic/Digest auth -- they authenticate via
    a plain header (`x-api-key`/`Authorization: Bearer`/`x-goog-api-key`)
    -- so there is no challenge this app could ever answer, and left
    unhandled the request simply never produces another event on its own.
    Fixed by adding an explicit `case wxWebRequest::State_Unauthorized`
    that reads `evt.GetResponse()` (still valid here -- the server's full
    401 response already arrived before wx reinterpreted it), reports it
    through the same `callback(false, ...)` path as a non-2xx
    `State_Completed`, and calls `.Cancel()` on a copy of
    `evt.GetRequest()` (a ref-counted handle, so the copy cancels the same
    underlying request) so the connection doesn't dangle. `Cancel()` itself
    raises a *second* event (`State_Cancelled`) for the same request id,
    so a `std::shared_ptr<bool> done` guard (captured by the lambda,
    checked at the top and set before every `callback()` call) stops that
    second event from invoking `callback` twice for one chat turn -- caught
    by reasoning through the control flow before it could ship as a
    double-`AppendToHistory()` bug, not found live. Re-verified live after
    the fix with the identical fake-key setup: the sidebar's history now
    shows the real `{"type":"error","error":{"type":"authentication_error",
    "message":"API key is invalid."},...}` body from Anthropic, the status
    line returns to "Chatting with Anthropic", and Send/Clear both
    re-enable -- confirming the fix, not just the absence of a hang.
  - **Layout: this sidebar docks into a narrow shared column (e.g. next to
    Table of Contents), and any horizontal-sibling layout inside it risks
    the same "one control crowds another down to an invisible sliver"
    failure.** First cut put the input box beside a button column, and
    separately tried two buttons side by side below a full-width input --
    both silently rendered one of the two controls at a few pixels wide,
    invisible in normal use. Confirmed live (not guessed) by temporarily
    setting each competing control's background to a distinct debug colour
    and screenshotting in Xvfb: the "input" area was entirely one colour
    with zero of the other visible anywhere. Fixed by abandoning every
    horizontal pairing and stacking status/history/input/Send/Clear as
    five independent children of one vertical `wxBoxSizer`, each full
    width -- no two controls ever compete for the same row's width.
  - **Verification recap:** `test_AiProvider` (54 assertions across all
    four providers' request/response/error shapes, no live network) plus
    the live Xvfb round trip above through the real Anthropic API endpoint
    (this sandbox's `no_proxy` list permits it) covering the layout fix,
    the full send/receive/error cycle, and the `State_Unauthorized` hang
    and its fix. Not verified here: a real, valid API key's successful
    reply for any of the four providers (this sandbox has no real
    credentials for any of them, and the user's own account is Anthropic's
    -- theirs to try first), and OpenAI/Google/Qwen's actual live endpoints
    at all (only Anthropic was reachable from this sandbox; the other
    three were checked only against their documented request/response
    shapes in `test_AiProvider.cpp`, not against a live server).
  - **Follow-up (2026-09-06): the worksheet context carried no notion of
    "where the user is" or "which cell errored" at all** -- raised by the
    user directly ("if a user asks the AI about the 'current' cell or the
    cell above the cursor that output an error message... we provide
    little to no info that allows the AI to navigate/find out where in
    the worksheet the user currently is"), and confirmed exactly right by
    reading `McpTools::ReadWorksheet()`/`CellSummary()`: neither carried
    any cursor or error information, only cell type/input/output text.
    Fixed by adding `McpTools::CurrentCell()` (thin wrapper over
    `Worksheet::GetHCaret()`, which already resolves "where the user
    is" -- active editor, h-caret, selection, or a last-resort fallback --
    for the worksheet's own insertion-point logic, so this reuses an
    existing, already-correct notion rather than inventing a second one)
    and `McpTools::HasError()` (a thin wrapper over the existing
    `DocumentCellPointers::ErrorList::Contains()`, the same mechanism
    `Worksheet::ScrollToError()` already relies on -- see the GH #1952
    entry above). Both are surfaced two ways: as `is_current`/`has_error`
    booleans on `list_cells`/`read_cell`'s JSON, and -- since the AI chat
    sidebar sends only `read_worksheet`'s plain-text dump, with no
    tool-calling to fall back on -- as inline markers
    (`"(CURRENT CELL -- the user's cursor is here)"`/`"(THIS CELL HAS AN
    ERROR)"`) directly in that text, extracted into a new shared
    `McpTools::CellText()` helper so `ReadWorksheet()` and `ReadSection()`
    can't drift apart on this (an actual near-miss: the first pass only
    patched `ReadWorksheet()`, and `ReadSection()`'s own near-identical
    per-cell loop would have silently kept lacking both markers).
    **A second, related bug found while fixing the first**:
    `AiChatSidebar::BuildContextSnapshot()`'s truncation for
    `MAX_CONTEXT_LENGTH` (8000 chars) took `text.Left(...)` -- for any
    worksheet long enough to actually need truncating, this silently cuts
    off content from the *end*, which is exactly where the new
    "(CURRENT CELL...)" marker is most likely to sit in the first place
    (a worksheet only needs truncating once it's already long, and a user
    is far more likely to be asking about their current cell in a long
    worksheet than a short one that never gets truncated at all) --
    quietly defeating the very feature just added, for precisely the
    worksheets where it matters most. Fixed by locating the marker first
    and centering the kept window on it (falling back to the original
    from-the-start behavior when there is no marker to center on, e.g. no
    real cursor position could be determined). Verified live, not just by
    reasoning about the arithmetic: loaded a real ~15KB synthetic
    worksheet (60 filler cells plus one distinctively-named final cell) in
    a live Xvfb session, moved the cursor to the very last cell
    (Ctrl+End), and confirmed via temporary logging (removed before
    committing, same discipline as the `State_Unauthorized` investigation
    above) that the ~8KB snapshot actually sent still contained both the
    `(CURRENT CELL` marker and the final cell's distinctive text -- on
    unfixed code this exact scenario reproduces the bug (the marker sits
    at the very end of a >8000-character document, so a plain `Left()`
    truncation cuts it every time, deterministically, not intermittently).
  - **Follow-up (2026-09-06): a variable's value can be empty just because
    Maxima hasn't answered yet, not because it's undefined -- and nothing
    told a tool-calling AI that.** `Variablespane::GetWatchedValues()`
    returns whatever the grid's value column currently holds, which starts
    empty the instant a variable is watched and only updates once Maxima's
    asynchronous response for that query actually arrives -- there is no
    "pending" state distinct from "empty," so `read_variables` right after
    `watch_variable` (a completely natural sequence for a tool-calling AI)
    can read back an empty value and have no way to tell "not answered
    yet" apart from "genuinely undefined." Fixed by adding
    `McpTools::MaximaIsBusy()` (`Worksheet::GetWorkingGroup(false) !=
    nullptr` -- something is actively being evaluated right now -- or a
    non-empty `Worksheet::GetEvaluationQueue()` -- more work is queued
    behind it) and surfacing it as `read_variables`' own `maxima_busy`
    field, with both tools' descriptions in `ListTools()` updated to spell
    out the implication (an empty/unchanged value while `maxima_busy` is
    true may just mean "not answered yet").
  - **Follow-up (2026-09-06): `read_cell` had no cap on a single cell's
    output at all, and `read_worksheet`/`read_section` only capped the
    *aggregate* text, not each cell's own contribution to it** -- raised
    by the user directly ("if a cell produced way too much output... do
    we provide the AI with methods to only read the input/only read the
    beginning or only read the end of the output?"), and confirmed exactly
    right by reading `OutputText()`/`CapLength()`: a single pathological
    cell (a huge matrix, a long list, ...) could return an unbounded
    response via `read_cell`, or silently crowd out every other cell's
    info in a `read_worksheet`/`read_section` response (worse: since that
    aggregate cap truncates from the *start*, one huge cell early in the
    document could crowd out not just later cells' content but the
    `(CURRENT CELL...)`/`(THIS CELL HAS AN ERROR)` markers just added
    above too, the same "truncate from the wrong end" bug shape as the AI
    chat sidebar's own truncation, independently). Fixed with a new shared
    `McpTools::TruncateText(text, maxLen, fromEnd, wasTruncated)` used two
    ways: (1) `read_cell` gained optional `output_length`/`output_from_end`
    arguments (defaulting to the full `MAX_TEXT_LENGTH` hard cap, i.e. no
    real-world limit for any normal cell, but never unbounded) plus an
    `output_truncated` result field, so an AI can ask for just a preview or
    specifically the tail (e.g. "did this converge") instead of the whole
    thing; (2) `ReadWorksheet()`/`ReadSection()` now cap *each* cell's own
    output to a smaller `OUTPUT_PREVIEW_LENGTH` (2000 chars) before
    concatenating, with an inline note pointing at `read_cell` when a
    cell's own output was individually truncated -- extracted into the
    same `CellText()` helper the current-cell/error markers already use,
    so the two follow-ups share one place to get right rather than two.
    **A real nlohmann::json gotcha hit while writing this, worth keeping in
    mind for any future tool argument parsing here**: `is_number_unsigned()`
    only returns true for a value that was *already* typed unsigned (e.g.
    text-parsed JSON with no leading minus sign) -- the identical positive
    value built from a plain C++ `int` literal, as any hand-constructed
    `json` object (including every test in `test_McpTools.cpp`) does, is
    classified `number_integer` (signed) instead despite being positive,
    and would have silently failed the check. Used `is_number_integer()`
    instead, which covers both representations, and simply ignores a
    negative value rather than rejecting the whole call.
  - **Follow-up (2026-09-06): "would it be possible to provide it with a
    'login' button or, if not, an info on how to obtain such an API key?"**
    A real "log in" button isn't possible -- same reasoning as the sidebar's
    original design (see the AI chat sidebar entry above): none of these
    four providers offer a legitimate third-party OAuth flow a desktop app
    could use, so there is no automated way to obtain a key. Added the next
    best thing instead: `AiProviderApiKeyUrl(AiProviderKind)` (alongside
    the existing `AiProviderKindName()`/`AiProviderDefaultModel()`) returns
    each provider's own API-key page, shown as a `wxHyperlinkCtrl` under
    that provider's key field in Options -> AI Chat. Also added an "Open
    Options..." button to the sidebar itself, shown only while no provider
    is configured (`ReloadProviderFromConfig()` toggles it), so a user who
    opens the sidebar cold has one click to the exact place that both
    explains the settings and links to where to get a key -- re-posting
    `wxEVT_MENU`/`wxID_PREFERENCES` to `GetParent()`'s event handler rather
    than constructing `ConfigDialogue` itself, reusing
    `MaximaCommandMenus.cpp`'s existing handling (re-reading the config
    file first, applying settings on OK, ...) instead of duplicating any of
    it -- the same "re-post the menu event, don't reimplement the handler"
    idiom `TrayIcon::OnInterrupt()`/`OnExit()` already use elsewhere.
    **A real bug caught by looking at the live screenshot, not by reading
    the code:** the four links were built from one shared format string,
    `wxString::Format(_("Get an %s API key..."), AiProviderKindName(kind))`
    -- grammatically fine for "Anthropic (Claude)"/"OpenAI" (both take
    "an"), but wrong for "Google (Gemini)"/"Qwen (Alibaba)" (both need
    "a"), rendering as the actually-shipped-then-caught "Get an Google
    (Gemini) API key..." Fixed by rewording to "Get an API key for %s...",
    which sidesteps the a/an agreement entirely rather than trying to track
    which of the four provider names needs which article.
  - **Follow-up (2026-09-06): "do we need to hardcode the model names? they
    tend to change over time"** -- a fair challenge, and the user's own
    follow-up ("model auto detection feels like hunting a mechanism that
    catches outdated strings with a mechanism that can get outdated") is
    exactly why this wasn't turned into a live "fetch the model list from
    each provider's API" feature: that mechanism would itself need
    maintaining against four APIs just to answer a question a plain link
    to each provider's own docs already answers, permanently, for free.
    Two changes instead: (1) `AiProviderDefaultModel()`'s Anthropic entry
    now uses `claude-3-5-sonnet-latest`, that provider's own "rolling"
    alias, instead of the dated snapshot `claude-3-5-sonnet-20241022` it
    used to be -- OpenAI/Google/Qwen's defaults (`gpt-4o-mini`/
    `gemini-1.5-flash`/`qwen-plus`) were already un-dated in this same
    sense, so only Anthropic's needed changing; this only pushes the
    staleness problem up one level (from "this exact snapshot got retired"
    to "this whole model line got superseded"), which a plain string
    constant genuinely cannot solve by itself. (2) A new
    `AiProviderModelListUrl()`, shown as a "See current models for %s..."
    link next to each provider's Model field in Options, mirroring
    `AiProviderApiKeyUrl()`'s own link added just above.
    **A real, independent bug found live while verifying this, not by
    reading the code**: after changing Anthropic's default in
    `AiProvider.cpp`, Options kept showing the *old* `-20241022` value
    regardless -- `AiProviderDefaultModel()` turned out to not be called
    from anywhere at all; `Configuration::ResetAllToDefaults()` had its
    *own*, completely separate hardcoded copy of all four model strings
    (`m_aiModelAnthropic = wxS("claude-3-5-sonnet-20241022")` et al.),
    silently disconnected from the function whose entire purpose is to be
    the one place these live. Exactly the class of bug the user's original
    question was worried about, already present in the code before this
    session touched it, just latent until an actual edit exposed it (a
    duplicated constant can drift silently for a long time if nothing
    ever changes the value in only one of its two copies). Fixed by
    having `ResetAllToDefaults()` call `AiProviderDefaultModel()` for all
    four, removing the second copy entirely; re-verified live in Xvfb
    that Options now genuinely shows `claude-3-5-sonnet-latest`.
  - **Follow-up (2026-09-06): a new `search_cells` MCP tool, so an AI can
    find a cell instead of reading the whole worksheet.** Raised by the
    user directly ("would the AI profit from some regex search for input/
    output?") -- a fair gap, since `list_cells`/`read_worksheet` were the
    only way to find "which cell mentions X" and both mean reading every
    cell yourself, exactly the friction the current-cell/error markers
    above already exist to reduce. Added `McpTools::SearchCells()`: a
    `pattern` argument, a plain case-insensitive substring by default, or
    a POSIX extended regular expression with `"regex": true` (`wxRegEx`,
    the same engine `RegexSearch`/`FindReplacePane` already use elsewhere
    in this codebase -- reused directly rather than adding a second regex
    dependency); optional `case_sensitive` and `scope` ("input"/"output"/
    "both", default "both") to narrow a search. Returns each matching
    cell's usual summary (uuid/is_current/has_error, via the existing
    `CellSummary()`) plus `matched_in` and a short `match_snippet` (40
    characters of context on each side of the match, with "..." markers
    where it was cut) -- enough to judge relevance without dumping a
    potentially huge cell's entire text for every hit. Capped to
    `MAX_SEARCH_MATCHES` (50) with a `truncated` flag, the same shape as
    every other capped response in this file, so a pathological pattern
    matching most of a huge worksheet can't turn into an unbounded reply.
    An invalid regex (checked via `wxRegEx::IsValid()` right after
    construction) throws `McpToolError` rather than matching nothing
    silently or crashing. The whole search runs under a `wxLogNull` guard
    -- a bad pattern or a match attempt could otherwise trigger a
    `wxLogXXX` call that, per this file's own "not reliably visible"
    entry, could in the worst case surface as an unwanted modal `wxLogGui`
    popup from what is, from the caller's perspective, an ordinary
    JSON-RPC error response. Verified both ways: `test_McpTools.cpp` gained
    a new SCENARIO (substring/regex/case-sensitivity/scope/invalid-pattern/
    the 50-match cap, 18 new assertions, 86 total in the file) against a
    real headless `Worksheet`, and the transport itself was re-checked live
    in Xvfb via `curl` against a real running MCP server (`tools/list`
    includes it; a plain-substring call, a case-insensitive call, and an
    invalid-regex call all matched expectations) -- catching, in the
    process, an unrelated test-harness mistake of my own (a `.wxm` file
    written for the live check without the required `/* [wxMaxima batch
    file version 1] ... */` header line fails to load with a completely
    silent-to-the-MCP-caller empty worksheet, `Format::ParseWXMFile()`'s
    own recognized-header check rejecting it before a single cell is
    parsed -- not a McpTools bug, just a reminder that a `.wxm` fixture
    needs that exact header, see the existing fixtures under
    `test/automatic_test_files/` for the correct shape).
  - **Follow-up (2026-09-08): AI Chat and Accessibility tabs had no icon of
    their own, and the Anthropic "Get an API key" link sends a subscriber
    to a "buy credits" page -- raised directly by the user.** Both
    `ConfigDialogue.cpp`'s `AddPage()` calls for these two tabs had reused
    tab index 4 (`wxmaximaART_CONFIG_OPTIONS`, a generic gear/wrench glyph)
    with an explicit "no dedicated one exists for this tab" comment -- fixed
    by adding two new hand-drawn SVG icons (`art/config/accessibility.svg`,
    a white stick figure with arms/legs spread inside a blue circle, the
    same motif as GNOME's `preferences-desktop-accessibility`; `art/config/
    ai-chat.svg`, a blue chat bubble with an orange four-point sparkle) and
    wiring them the same way every other `art/config/*.svg.gz` icon already
    is: gzip the plain SVG, add the base name to `art/config/
    CMakeLists.txt`'s `IMAGE_FILES` (bin2h's `string(MAKE_C_IDENTIFIER
    ...)` step sanitizes the hyphen in `ai-chat` to `AI_CHAT_SVG_GZ`
    automatically -- no special-casing needed, same as the pre-existing
    hyphenated `edit-copy-confdialogue`), a new `wxmaximaART_CONFIG_*` art
    ID in `wxMaximaArtProvider.h`/`.cpp`, and two new entries appended to
    *both* of `ConfigDialogue.cpp`'s parallel image-list branches (the
    `wxCHECK_VERSION(3, 1, 6)` `wxBitmapBundle` one and the older
    `wxImageList` fallback) -- missing either branch would silently break
    only pre-3.1.6 wx or only 3.1.6+, so both need touching together.
    Verified live in Xvfb: the AI Chat tab shows the new sparkle-bubble
    icon correctly (screenshotted, matches the standalone-rendered PNG
    pixel-for-pixel in shape); the Accessibility tab's icon could only be
    confirmed via the same standalone SVG render, not live, since this
    sandbox's wxWidgets build has `wxUSE_ACCESSIBILITY` off (the tab is
    `#if wxUSE_ACCESSIBILITY`-gated and never appears in this environment
    at all -- consistent with the sandbox limitation this file's Key
    Subsystems section doesn't otherwise document per-tab, just worth
    knowing if a future session can't find this tab locally either).
    **The second half of the request -- "does a subscription need a
    separate link" -- turned out to have a firm, current (2026) answer,
    not a wrong-link bug**: researched directly (this postdates training
    data, so worth citing rather than assuming) that Anthropic's OAuth
    token for a Claude Pro/Max subscription (`claude setup-token`,
    `sk-ant-oat01-...`) is *rejected* by the Messages API this sidebar
    calls -- it only authenticates against Claude Code/claude.ai -- and
    Anthropic has explicitly banned third-party use of subscription auth
    outside those two surfaces since January 2026. So there is no
    "subscription" link this sidebar could offer instead; a console.
    anthropic.com API key, billed per-token and separately from any chat
    subscription, is the only way this feature (or any raw-Messages-API
    integration) can authenticate, for every account regardless of
    subscription status. This matches the same "no legitimate third-party
    OAuth flow" reasoning the AI chat sidebar's own top-level entry already
    gives for all four providers -- confirmed still true, not just assumed
    unchanged. Fixed by adding a short, upfront note to the AI Chat
    Options panel (right under the existing intro paragraph, not per-
    provider) stating plainly that an API key is billed separately from a
    Claude Pro/Max, ChatGPT Plus or Gemini Advanced plan, so hitting a
    "buy credits" page isn't mistaken for a broken/outdated link. Verified
    live in Xvfb that the note renders under the intro text on the AI Chat
    tab.
  - **Follow-up (2026-09-08): API keys moved from plain-text wxConfig into
    the OS secret store, custom (non-built-in) providers, and a
    dropdown-based Options redesign -- raised directly by the user
    ("are the API keys stored in wxWidgets secret storage thingy?" plus a
    request to stop showing all four providers' fields stacked at once and
    to let a "not completely exotic" auth method be picked for a
    user-added provider).**
    1. *Secret storage.* The four keys were, until this point, plain
       `wxString` fields going through `Configuration`'s generic scalar-
       settings table -- clear text in the same file as `texPreamble`/
       `mathJaxURL`/everything else, confirmed by grepping that table
       directly, not assumed. Replaced with `AiProvider::SaveApiKey()`/
       `LoadApiKey()`/`DeleteApiKey()` (`wxSecretStore`, service name
       `"wxMaxima/AI/" + <built-in kind name or "custom:"+id>`), and
       `Configuration::AiApiKeyAnthropic()`/etc. kept their exact names and
       signatures but now just forward to these -- every existing call
       site (`ConfigDialogue.cpp`, `AiChatSidebar.cpp`) needed zero changes
       for the four built-ins. The user explicitly chose (over a
       plaintext-fallback option offered first) to **hide the whole AI
       Chat feature outright** -- tab, sidebar, and its View -> Sidebars
       menu entry -- rather than ever fall back to plain-text storage on a
       system with no working secret store; see
       `AiProvider::SecretStoreAvailable()` (`wxUSE_SECRETSTORE` at compile
       time AND `wxSecretStore::GetDefault().IsOk()` at runtime -- a
       compiled-in build can still have no real keyring service reachable,
       e.g. no gnome-keyring/kwallet D-Bus session) and every call site
       that guards on it (`ConfigDialogue::CreateAiChatPanel()`'s
       `AddPage()` call and its `SetCheckboxValues()`/save-on-OK
       counterparts, `wxMaximaFrame`'s sidebar construction and its
       `AppendCheckItem()` call). **This sandbox's own prebuilt
       `libwxgtk3.2-dev` has `wxUSE_SECRETSTORE 0`** (confirmed by grepping
       its installed `setup.h` directly -- `libsecret-1-0` the runtime lib
       is present, but not `libsecret-1-dev`, the headers wx's own build
       needed), so the real `wxSecretStore`-calling branch could not be
       exercised here at all -- same category as this file's other
       version/backend-gated code (wx 3.3 dark mode, `wxUSE_ACCESSIBILITY`)
       verified by careful reading against the real `wx/secretstore.h`
       header (present regardless of the flag, read directly to confirm
       `Save(service, username, wxSecretValue)`/`Load(service, username&,
       password&)`/`Delete(service)`'s exact signatures) rather than
       compiled. The *hidden* path, conversely, is exactly what this
       sandbox exercises by construction, and was verified live in Xvfb:
       a fresh profile shows no "AI Chat" entry anywhere in View ->
       Sidebars, and Options has no "AI Chat" tab/icon at all (confirmed
       against the same screenshot used to verify the new Accessibility/
       AI Chat tab icons in the entry above -- "Startup commands" ->
       "Printout settings" -> "Revert all to defaults" run consecutively,
       nothing between them).
       **Migration**: `Configuration::ReadConfig()` gained a one-time
       step, gated on `SecretStoreAvailable()`, that reads any of the four
       old plain-text keys still present, moves each non-empty one into
       the secret store, and calls `config->DeleteEntry()` immediately
       (not just clearing the in-memory field) so the plain-text copy
       doesn't linger on disk until some unrelated setting change happens
       to trigger a save. Deliberately does *nothing* (leaves the old
       plain-text value exactly where it is) when no secret store is
       available -- the feature is hidden either way in that case, and
       there is nowhere safer to move the value to yet; better to leave it
       dormant-but-recoverable than destroy it. This exact migration path
       could not be exercised live either, for the same `wxUSE_SECRETSTORE
       0` reason -- verified by reading the code path against the real
       `wxSecretStore` API and by confirming (live, this sandbox) that
       with no legacy key ever present, `ReadConfig()` still runs cleanly
       start to finish with no crash.
    2. *Custom providers.* `AiProviderKind` gained a fifth value, `Custom`
       (existing values are untouched, so no migration needed for the
       existing `aiChatProvider` int); a new `AiProviderShape` enum
       (`Anthropic`/`OpenAiCompatible`/`Google`) captures the one thing
       that actually varies across a hand-added endpoint's wire format --
       the request/response JSON shape, which also implies its auth
       header (`x-api-key`/`Authorization: Bearer`/`x-goog-api-key`
       respectively; none of the three needed a separate, freeform "custom
       auth" option, since none of the three headers are exotic enough to
       need one, per the user's own framing). `MakeAiProviderForShape()`
       reuses the exact same `AnthropicProvider`/`OpenAiCompatibleProvider`/
       `GoogleProvider` classes a built-in provider already uses, just
       pointed at an arbitrary URL/model/display-name instead of one of
       the four hardcoded built-ins -- `Kind()` on the result is always
       `Custom` (each of the three provider classes gained a `kind`
       constructor parameter it stores and returns, defaulting to that
       class's own natural built-in kind, rather than hardcoding the
       return value the way they used to), and `Name()` returns the display
       name override (`AiProvider::SetDisplayName()`) instead of falling
       through to `AiProviderKindName(Kind())`, since `Custom` alone
       carries no name of its own. Everything a custom provider needs
       beyond its API key (name/shape/baseUrl/model) is persisted as one
       JSON array in `Configuration::AiCustomProvidersJson()`
       (`AiCustomProviderConfig`/`ParseAiCustomProviders()`/
       `SerializeAiCustomProviders()`, using the already-vendored
       `nlohmann::json` the same way `McpTools`/`AiProvider.cpp`'s own
       request bodies already do) -- deliberately *not* a second ad hoc
       string-blob format, and deliberately tolerant of a missing/
       malformed value (empty list, not a thrown exception or a crash),
       since this is exactly the kind of field a hand-edited or corrupted
       config file could plausibly break. Which custom entry (if any) is
       currently active is `Configuration::AiActiveCustomProviderId()`,
       a separate string field checked only when `AiChatProvider() ==
       (int)AiProviderKind::Custom`.
    3. *Options redesign.* `CreateAiChatPanel()` used to build and always
       show all four providers' `wxStaticBoxSizer`s stacked vertically
       (the user's own "somewhat repetitive" framing). Replaced with one
       reusable detail box (key/model always shown; a request-URL field
       and an API-style dropdown shown *only* for a Custom entry, since a
       built-in's URL/shape are implied by its kind and never editable)
       that `LoadAiProviderRecordIntoUi()` repopulates every time the
       "Active provider" `wxChoice` selection changes, via
       `OnAiProviderChoice()`. The choice's own item list is the four
       built-ins (fixed order, matching their `AiProviderKind` values) plus
       every custom entry, plus a trailing "Add custom provider..." item
       that is never itself a persistent selection -- picking it pops
       `AddCustomAiProviderDialog()` (a small ad hoc `wxDialog`: name/
       shape/URL/model fields, an `wxEVT_UPDATE_UI`-driven OK button that's
       disabled until name and URL are both non-empty) and either commits
       a new entry and selects it, or reverts the choice to whatever was
       selected before if cancelled. **Switching the active selection
       needed an explicit "flush the outgoing selection's on-screen edits
       first" step** (`StashAiProviderUiIntoRecord()`, called both before
       loading a different record into the shared controls and before the
       final save-on-OK) -- without it, typing a key/model for provider A,
       switching to provider B, then clicking OK would silently discard
       whatever was typed for A, since only one physical set of controls
       ever exists now and switching would otherwise just overwrite it
       with B's values with no intermediate write-back. This mirrors the
       exact same class of bug this file's other stateful-editor entries
       warn about (a single shared UI surface standing in for N pieces of
       backing state needs an explicit save-before-switch step; the
       previous four-boxes-always-visible design never had this problem
       *because* nothing was ever hidden/swapped). All 45 existing unit
       tests plus two new `test_AiProvider.cpp` scenarios (
       `MakeAiProviderForShape()` against all three shapes, confirming
       each one's real request/response behavior is preserved verbatim at
       an arbitrary URL; `ParseAiCustomProviders()`/
       `SerializeAiCustomProviders()` round-tripping, including the empty-
       and malformed-JSON degrade-gracefully cases) pass; the dropdown/
       detail-box UI itself was verified live in Xvfb for the *pre-hidden*
       state in the prior follow-up's own screenshots -- the redesigned
       version could not be re-screenshotted in this same sandbox session
       for the obvious reason (the tab doesn't exist here at all once
       `wxUSE_SECRETSTORE` is confirmed off), so this specific UI's actual
       on-screen behavior (choice switching, the Add-custom dialog, the
       Remove-custom button) is verified by code reading and the unit
       tests above, not by a live screenshot -- worth a real interactive
       check in an environment with a working secret store if this is
       revisited.
  - **Follow-up (2026-09-08): quick-fill presets for well-known local AI
    servers (Ollama, LM Studio, llama.cpp server), plus the security
    question that came with the idea.** The user asked directly whether a
    local server should even be a *built-in* `AiProviderKind` (a fifth
    choice alongside Anthropic/OpenAI/Google/Qwen), and, separately,
    whether a malicious website could set up its own fake local server and
    remote-control a running wxMaxima. The second question has a clean
    answer that shaped the first: **a web page cannot open a listening
    socket at all** -- browsers expose no raw-socket/server API to page
    JavaScript, so "a website sets up its own fake local server" isn't a
    mechanism that exists; the real (much narrower) risk is a *different
    already-running local process* squatting on the same port a real local
    LLM server would use (e.g. something else bound to `:11434`) before the
    user starts Ollama -- and even then, the blast radius is already bounded
    by this sidebar's own documented design: v1 is read-only with no
    tool-calling (see the "AI chat sidebar" entry's own opening paragraph),
    so a hostile response can only inject text into the chat transcript, not
    touch the worksheet or execute anything -- the same reasoning that
    already bounds a hostile *real* provider's response. This is also why a
    genuine built-in `AiProviderKind` for "local server" doesn't pull its
    weight: unlike the four real, name-brand providers, there's no single
    fixed URL/auth-header pair to hardcode -- Ollama/LM Studio/llama.cpp
    server each pick their own port and path, and a user can point any of
    them at a nonstandard address anyway -- so the entry actually needed
    something orthogonal to `AiProviderKind`, not another value in it.
    Added `AiLocalServerPreset` (`name`/`baseUrl`/`model`) and
    `AiKnownLocalServerPresets()` (`src/ai/AiProvider.h`/`.cpp`) -- a short,
    hand-picked, non-exhaustive list (Ollama's OpenAI-compatible endpoint at
    `http://localhost:11434/v1/chat/completions`, note the `/v1/` prefix:
    Ollama's *native* `/api/chat` endpoint uses a different, non-OpenAI
    wire shape that `OpenAiCompatibleProvider` doesn't speak; LM Studio's
    built-in server at `:1234`; `llama.cpp`'s `llama-server` at `:8080` --
    all three are OpenAI-compatible by construction) -- and a "Quick fill:"
    `wxChoice` at the top of `ConfigDialogue::AddCustomAiProviderDialog()`'s
    grid (`src/dialogs/ConfigDialogue.cpp`). Picking a preset just
    `ChangeValue()`s the dialog's existing Name/URL/Model fields (and forces
    the API-style choice to "OpenAI-compatible") -- a one-time convenience,
    not a new persisted concept: every field stays independently editable
    afterward, nothing distinguishes a preset-filled custom provider from a
    hand-typed one once saved, and picking "(Custom)" back leaves whatever
    is already typed untouched. `ChangeValue()`, not `SetValue()`, is used
    deliberately -- a prefill shouldn't fire a spurious `wxEVT_TEXT` the
    way `SetValue()` would, even though nothing in this particular dialog
    currently listens for one; picked for correctness against future
    changes, not because it fixed an observed bug here.
    `test_AiProvider.cpp` gained a new SCENARIO asserting every
    preset is fully filled in (no empty name/URL/model) and that each one's
    URL survives unchanged through `MakeAiProviderForShape(OpenAiCompatible,
    ...)` -- 116 assertions in 8 test cases total now, all passing.
    Same sandbox limitation as the rest of this feature applies here too:
    the actual "Quick fill" dropdown cannot be screenshotted in this sandbox
    (`wxUSE_SECRETSTORE` is off here, so the whole AI Chat tab stays hidden
    per its own gating) -- verified by code reading plus the unit test
    above, not a live screenshot.
  - **Follow-up (2026-09-11): "can you implement a GitHub Copilot
    connector?" -- researched directly with the user rather than just
    coding it, since "GitHub Copilot" turned out to mean two genuinely
    different things with very different risk profiles.** GitHub Copilot
    Chat (the assistant bundled with a Copilot subscription) has **no
    official third-party API at all**. The way every community tool that
    reaches it (copilot.vim-style plugins, "copilot-api" proxies) actually
    works: perform the GitHub OAuth device flow using *another product's*
    client id (typically an approved editor's, e.g. VS Code's -- GitHub
    only allowlists Copilot-token exchange for client ids it has
    specifically approved, so a freshly-registered wxMaxima OAuth app would
    authenticate fine via the device flow but be rejected at the next
    step), then exchange the resulting token at an undocumented internal
    endpoint (`api.github.com/copilot_internal/v2/token`) for a short-lived
    Copilot token, then call another undocumented endpoint
    (`api.githubcopilot.com/chat/completions`) while sending headers that
    make the request look like it came from that editor. That is not "no
    legitimate OAuth flow for a desktop app" (the situation already
    documented for Anthropic/OpenAI/Google/Qwen/GitHub Models, all of
    which still have a real pasted-API-key path) -- it is impersonating an
    authorized client's identity to reach an API GitHub has not opened to
    third parties, which risks the account being flagged or suspended
    under GitHub's Copilot terms. Presented to the user as an explicit
    choice with this risk spelled out; the user's first answer was "both,"
    but on seeing the mechanism restated even more concretely (a borrowed
    client id, not wxMaxima's own) they reconsidered mid-implementation
    ("If that feature risks getting our users banned perhaps we should
    only support the personal chat...") and the Copilot Chat half was
    dropped entirely -- no OAuth device-flow code, no borrowed client id,
    no new Custom-provider auth scheme for it exists anywhere in this
    codebase. Don't re-add it without a new, explicit request; if one comes
    in, this reasoning (and the specific "GitHub gates the token-exchange
    endpoint to an allowlist, not just any authenticated OAuth app" fact)
    is the thing to re-derive from, not guess at again.
    **What *was* added: GitHub Models**, a genuinely different, official
    GitHub product -- an OpenAI-compatible chat-completions endpoint
    (`https://models.github.ai/inference/chat/completions`) authenticated
    with a plain GitHub personal access token via a standard
    `Authorization: Bearer` header, publicly documented at
    <https://docs.github.com/en/github-models>, not a workaround of
    anything. This is the sixth `AiProviderKind` (`GitHubModels = 6`,
    `src/ai/AiProvider.h`) and needed **zero new provider logic**: its
    request/response shape and auth header are byte-for-byte what
    `OpenAiCompatibleProvider` already implements for OpenAI/Qwen, so
    `MakeAiProvider(AiProviderKind::GitHubModels, ...)` just constructs one
    pointed at GitHub's own URL (`AiProvider.cpp`) -- the same "OpenAI-
    compatible shape, different base URL/auth secret" pattern Qwen already
    established. Wired through the same places every built-in kind needs:
    `Configuration::AiApiKeyGitHubModels()`/`AiModelGitHubModels()` (new
    accessors, same secret-store-backed pattern as the other four),
    `ConfigDialogue.cpp`'s `fixedKinds[]` array and both of its per-kind
    switch statements (populate-on-open, apply-on-OK), and
    `AiChatSidebar::ReloadProviderFromConfig()`'s switch -- all three of
    these switches are the actual places that need a new `case` per
    built-in kind; `RebuildAiProviderChoice()`/`LoadAiProviderRecordIntoUi()`
    and the rest of the Options UI are already fully generic over
    `m_aiProviderRecords`, so nothing there needed touching, and it's worth
    checking that dynamism holds before assuming a new provider needs UI
    changes beyond the three switches. `AiProviderDefaultModel()` uses
    `"openai/gpt-4o-mini"` -- GitHub Models' catalog names entries
    `<publisher>/<model>`, and that specific one is on the free tier, unlike
    some of the catalog's larger models which need a paid Models plan.
    Verified with a new `test_AiProvider.cpp` SCENARIO mirroring the
    existing OpenAI/Qwen one (request URL, Bearer header, OpenAI-compatible
    body/reply shape) -- not verified live (no real GitHub PAT in this
    sandbox), same caveat this file's other AI-provider entries already
    carry for OpenAI/Google/Qwen.
  - **Follow-up (2026-09-09): a new `evaluation_status` MCP tool -- "if
    Maxima is evaluating, what cell it works on, what command within that
    cell and for how long this command already is being evaluated,"
    raised directly by the maintainer.** Two of the three pieces already
    existed and just needed exposing; the third (elapsed time) needed new
    state, since nothing in this codebase timed a single in-flight command
    before this. Researched via a dedicated Explore agent across
    `Worksheet`, `EvaluationQueue`, `MaximaEvaluator`, `StatusBar` and
    `GroupCell` before writing anything, specifically to avoid re-deriving
    "does timing already exist somewhere" from scratch:
    - **"Is Maxima evaluating" / "which cell"**: `Worksheet::
      GetWorkingGroup(false)` (no fallback) already returns exactly the
      cell being worked on right now, `nullptr` the instant nothing is in
      flight -- the same distinction `McpTools::MaximaIsBusy()` already
      relies on. No new state needed.
    - **"Which command within that cell"**: `EvaluationQueue` already
      tracks this at the single-statement level, not just per-cell --
      commands are tokenized lazily, one at a time (`m_commands` holds 0
      or 1 entries, see its own doc comment: this is deliberate, since a
      cell's lisp/maxima mode split can only be known once the previous
      command's prompt arrives). `EvaluationQueue::GetCommand()` returns
      the exact statement text currently in flight;
      `EvaluationQueue::GetIndex()` its character offset within the
      cell's input. Both already existed, unused by anything outside
      `MaximaEvaluator::TriggerEvaluation()` itself.
    - **"For how long"**: genuinely new. Added `wxStopWatch
      m_commandStopwatch` + `bool m_commandTimerRunning` to
      `EvaluationQueue` (`src/EvaluationQueue.h`), with
      `MarkCommandSent()` (starts/restarts it) and
      `GetCommandElapsedMilliseconds()` (returns -1 when nothing is
      currently timed). **Deliberately started at the moment the command
      is actually written to the socket, not when it's tokenized**:
      `EvaluationQueue::AddTokens()`/`ProduceNextCommand()` can produce a
      command that then sits briefly unsent (`MaximaEvaluator::
      TriggerEvaluation()` still has to validate its parenthesis balance
      in the *current* lisp/maxima mode before deciding to send it at
      all -- see that function's own comment on why this check has to
      happen per-command, not per-cell) -- timing from tokenization would
      report time Maxima was never actually working. `MarkCommandSent()`
      is called from `TriggerEvaluation()` immediately after the real
      `SendMaxima(text, true)` call that dispatches it
      (`MaximaEvaluator.cpp`, right where `m_wxMaxima.m_maximaBusy = true`
      is also set a line later). The timer is stopped again (`
      m_commandTimerRunning = false`) at the top of `RemoveFirst()`,
      before that command is erased -- and in `Clear()`, for the abort/
      restart paths. Confirmed by tracing every call site that can end a
      command's lifetime (`RemoveFirst()`'s own erase, `Clear()`) that
      none can leave a stale "still timing" flag set once the command it
      was timing is gone.
    - **New tool**: `McpTools::EvaluationStatus()` (`src/mcp/McpTools.{h,
      cpp}`) returns `{"evaluating": bool}` alone when idle, or adds
      `cell_uuid`/`is_current`/`has_error` (same fields `CellSummary()`
      already surfaces elsewhere, built inline here rather than through
      `CellSummary()` itself since that needs a document-order `index`
      this tool has no reason to compute), `command` (the exact in-flight
      statement text), `command_index_in_cell`, `elapsed_ms` (omitted
      rather than reported as a bogus value in the -- believed impossible
      -- case `GetCommandElapsedMilliseconds()` returns -1 while
      `evaluating` is true), `commands_left_in_cell` (existing
      `EvaluationQueue::CommandsLeftInCell()`, already a "best-effort
      hint" per its own doc comment, since lazy tokenization means the
      exact count isn't knowable ahead of time) and `queue_length`
      (`EvaluationQueue::Size()`, counting the currently-evaluating cell
      too -- "how much work is left," not "how much is waiting behind the
      current one"). **Queued-but-not-yet-sent is deliberately reported as
      `evaluating: false`**: a cell freshly handed to `AddToQueue()` gets
      tokenized (`m_commands` non-empty) before `TriggerEvaluation()` ever
      calls `MarkCommandSent()` on it, and calling `GetWorkingGroup(false)`
      at that point still correctly returns `nullptr` -- so "a command
      exists in the queue" and "a command has actually been dispatched"
      stay distinguishable, matching the tool's own framing (deliberately
      *not* reusing `MaximaIsBusy()`'s broader "busy OR queued" definition
      here, since the maintainer's own wording asked specifically about
      *evaluating*).
    - **Verification**: `test_McpTools.cpp` gained a new SCENARIO with
      three GIVENs -- nothing queued (evaluating=false, no per-command
      fields present at all, checked with `CHECK_FALSE(status.contains(...))`
      rather than assuming a default), a cell actually
      dispatched via the real `AddToQueue()` + `MarkCommandSent()` +
      `SetWorkingGroup()` sequence (evaluating=true, correct uuid/command
      text/non-negative elapsed_ms), and a cell merely queued via
      `AddToQueue()` with neither `MarkCommandSent()` nor
      `SetWorkingGroup()` called (evaluating stays false despite
      `queue_length` being 1) -- this last case is exactly the "tokenized
      but not dispatched" distinction above, pinned as its own scenario so
      a future change can't collapse it back into "queued counts as
      evaluating" without a test failing. 101 assertions in 7 test cases
      now, all passing. Not verified live (no live Maxima connection in
      this sandbox for this specific tool -- unlike `search_cells`'s own
      live-`curl` verification, actually timing a real multi-second Maxima
      computation and reading `elapsed_ms` back through a live MCP
      `tools/call` would need a genuinely slow real computation to be
      convincing, which wasn't attempted this pass); the full existing
      ctest suite's non-batch/non-live-Maxima tests were re-run and a
      clean full rebuild was confirmed to produce zero new warnings.
  - **Follow-up (2026-09-09): `watch_variable` gave no hint that Maxima was
    busy, forcing an extra round trip just to learn that -- raised
    directly by the maintainer: "if the AI tries to query a variable and
    Maxima is busy which prevents it from receiving a result, is the AI
    informed about the reason?"** Answer at the time: only partially, and
    not from the tool an AI would call first. `WatchVariable()`'s own
    response was just `{"ok": true, "name": "..."}` -- the busy state only
    surfaced if the AI *separately* called `read_variables` afterward
    (which already reports `maxima_busy`), a round trip the tool's own
    description already told it to make ("If maxima_busy is true right
    after watch_variable, wait and call read_variables again") but that a
    tool-calling AI could easily skip, reading back an empty value and
    concluding the variable is undefined instead of "not answered yet."
    Fixed by adding `maxima_busy` (via the existing `MaximaIsBusy()`)
    directly to `WatchVariable()`'s own result -- the same information,
    just available one call earlier, no new mechanism needed since
    nothing about *whether* Maxima is busy depends on the watch request
    itself. Also cross-referenced the brand-new `evaluation_status` tool
    (added earlier this same session) from both `read_variables`' and
    `watch_variable`'s `ListTools()` descriptions, since `maxima_busy`
    alone only ever answered "is something blocking this," never the
    *reason* the maintainer's question was actually asking about (which
    cell, which command, how long) -- `evaluation_status` is precisely
    the tool built to answer that, but until this fix nothing pointed an
    AI at it from here. `test_McpTools.cpp` gained a new WHEN case pinning
    the true branch (`SetWorkingGroup()` set before calling
    `WatchVariable()`, checking its response reports `maxima_busy: true`
    directly) alongside the existing idle-case WHEN, which was extended to
    also check `maxima_busy == false` -- 104 assertions in 7 test cases
    now, all passing; full 47-test ctest suite re-run clean.
  - **Follow-up (2026-09-09): a new `maxima_connected` field on
    `read_variables`/`watch_variable`/`evaluation_status` -- raised
    directly by the maintainer as a follow-up to the `maxima_busy` fix
    above: "if Maxima isn't running at all and that causes the variable
    query to fail, is the AI informed about that?"** Researched via a
    dedicated Explore agent before answering (across `Worksheet`,
    `Variablespane`, `MaximaProcessManager`, `Maxima` and `StatusBar`)
    rather than guessing: the answer was **no**. `MaximaIsBusy()` (and
    `EvaluationStatus()`'s `evaluating`) only ever read `Worksheet::
    GetWorkingGroup()`/`GetEvaluationQueue()` -- both describe *evaluation-
    queue content*, not process/socket state -- so when Maxima was never
    started, crashed, or was killed, they report exactly the same "false"/
    "nothing queued" as a genuinely idle, fully-answered Maxima. The real
    connection state (`Maxima::IsConnected()`, `m_socket->IsConnected()`)
    lives on `wxMaxima::m_client`, completely unreachable from a bare
    `Worksheet*`/`Variablespane*` -- the only two things `McpTools` ever
    holds (confirmed: no connection concept exists anywhere on `Worksheet`/
    `Variablespane` themselves).
    **Plumbing chosen**: a `std::function<bool()>` callback, not a new
    constructor parameter or a raw `MaximaProcessManager*`/`wxMaxima*`
    pointer threaded down -- a raw pointer would force every existing
    `test_McpTools.cpp` fixture (none of which build a live Maxima
    connection, or even a full `wxMaxima` app object) to either construct
    one or special-case a null check, where a callback left unset simply
    defaults to "assume connected" (the correct default: nothing indicates
    trouble, so don't report false alarm). `McpTools::SetConnectionCheck()`
    stores it; `IsMaximaConnected()` is `!m_isMaximaConnected ||
    m_isMaximaConnected()`. `McpServer::SetConnectionCheck()` forwards to
    it. Wired up once, in `wxMaxima`'s own constructor (`wxMaxima.cpp`,
    right after `StatusMaximaBusy(StatusBar::MaximaStatus::disconnected)`):
    `m_mcpServer->SetConnectionCheck([this] { return m_client &&
    m_client->IsConnected(); });` -- safe to register this early even
    though `m_client` is still null at that exact point in the constructor,
    since the lambda captures `this` and re-reads `m_client` fresh on every
    future call, not at bind time; `m_mcpServer` itself (constructed inside
    `wxMaximaFrame`'s constructor, a base-class step that runs *before*
    `wxMaxima`'s own body) is reachable here because it's declared
    `protected`, not `private`, on `wxMaximaFrame` -- no virtual-dispatch
    trick or deferred-`this`-cast needed, unlike what would have been
    required had it been private.
    **A real, self-inflicted compile break caught immediately, not shipped**:
    the first draft's new Doxygen comment on `McpTools::SetConnectionCheck()`
    literally wrote out "a Worksheet*/Variablespane* pointer" -- the `*/`
    inside that phrase closed the `/*! ... */` block comment early, so
    every subsequent line (the setter itself, `IsMaximaConnected()`, the
    private member) silently fell *outside* the comment and became raw,
    malformed declarations, breaking every translation unit that includes
    `McpTools.h` (i.e. nearly the whole `wxMaximaFrame.h` include chain)
    with a wall of "missing terminating '" / "does not name a type" errors
    that look nothing like their real cause at first glance. Fixed by
    rewording to avoid embedding a literal `*/` inside a block comment at
    all ("a Worksheet/Variablespane pointer") -- worth remembering
    generally: any doc comment mentioning two pointer types back to back
    with a slash between them (`Foo*/Bar*`) is one keystroke away from
    silently truncating the comment it's written inside.
    **Deliberately reported from all three tools, not just
    `read_variables`**: `watch_variable` gets it for the same reason it
    already got `maxima_busy` in the follow-up just above (save the AI a
    round trip it might not think to make); `evaluation_status` gets it
    too since `evaluating: false` has exactly the same "idle vs. not
    running" ambiguity `maxima_busy` does, and the whole point of that
    tool is to answer detailed "why" questions this ambiguity would
    otherwise leave unanswered. All three `ListTools()` descriptions were
    updated to explain the distinction explicitly, not just add the field
    silently.
    **Verification**: a new dedicated SCENARIO in `test_McpTools.cpp`
    calls `tools.SetConnectionCheck([] { return false; })` once and checks
    all three tools (`EvaluationStatus()`, `ReadVariables()`,
    `WatchVariable()`) report `maxima_connected: false` alongside their
    existing busy/evaluating fields staying `false` too -- pinning that
    the two are independent signals, not one implying the other. The
    pre-existing "nothing queued" `EvaluationStatus()` scenario also now
    checks `maxima_connected == true` (the un-configured default) so a
    future regression that silently flips that default would be caught.
    112 assertions in 8 test cases now, all passing; full 47-test ctest
    suite and a full clean rebuild (zero new warnings) re-confirmed
    afterward. Not verified live end-to-end (no real Maxima
    connect/disconnect cycle driven through a live MCP `tools/call` in
    this pass) -- the callback wiring itself was checked by reading the
    exact construction-order/`protected`-visibility facts above, not by
    running the real app with Maxima killed mid-session.
  - **Follow-up (2026-09-08): three real layout/API-misuse bugs in the
    redesigned Options -> AI Chat tab, all invisible in this sandbox and
    only caught because the maintainer ran a real build with a stricter
    wxWidgets (their own experimental GTK4 port) that actually asserts on
    them.** The maintainer asked directly "is that us or my experimental
    wxWidgets version?" -- worth restating the answer here since it's the
    reusable lesson: **all three were wxMaxima's own bugs**, not anything
    specific to that fork. wxWidgets' assertions exist precisely to catch
    API misuse that an unasserted (or `NDEBUG`) build silently tolerates --
    this sandbox's prebuilt `libwxgtk3.2-dev` apparently doesn't hit either
    assert at all (or has assertions compiled out), which is exactly why
    the earlier follow-ups in this section could only claim "verified by
    code reading," never a live screenshot of this tab. A stricter build
    surfacing a real bug the moment the tab is actually opened is the
    system working as intended, not a fork-specific false positive.
    1. `wxFlexGridSizer(2, 2, 5, 5)` -- interpreted as the (rows, cols,
       vgap, hgap) overload, so *rows* was hardcoded to 2 -- appeared twice
       (the shared provider-detail box in `CreateAiChatPanel()`, and
       `AddCustomAiProviderDialog()`), each adding 4-5 rows x 2 columns of
       items. Every other `wxFlexGridSizer(N, 2, 5, 5)` call already in
       this file (search it -- `10, 2, 5, 5`, `9, 2, 5, 5`, `20, 2, 5, 5`,
       ...) correctly passes the real row count as the first argument;
       these two didn't, presumably copy-pasted before the actual row
       count was known and never updated as rows were added. Since a
       `wxFlexGridSizer` with *both* rows and cols fixed caps its total
       item count at `rows*cols` (`wxGridSizer::DoInsert()`'s own
       contract), the 5th item added (the second row's label) tripped
       `assert "Assert failure" failed in DoInsert(): too many items
       (5 > 2*2) in grid sizer` -- deterministically, on every single open
       of that tab, not a race or a GTK4-specific quirk. Fixed by setting
       each grid's row count to its actual number of rows (4 and 5
       respectively), matching the established convention.
    2. `m_aiApiKeyLink`/`m_aiModelListLink` were each constructed as
       `new wxHyperlinkCtrl(parent, id, wxEmptyString, wxEmptyString)` --
       both label and URL empty, since the real values aren't known until
       `LoadAiProviderRecordIntoUi()` runs moments later and the control
       has to exist before that to be added to the sizer. wxWidgets'
       `wxHyperlinkCtrlBase::CheckParams()` asserts `!url.empty() ||
       !label.empty()` -- constructing with both empty is invalid
       regardless of platform; this sandbox's wx build simply doesn't
       enforce it. Fixed by giving each a throwaway single-space
       placeholder label (satisfies the assert; never actually seen) and
       calling `Show(false)` immediately at construction, matching what
       `LoadAiProviderRecordIntoUi()` already does once a real URL is
       known (`m_aiApiKeyLink->Show(!keyUrl.IsEmpty())` etc.) -- the
       control was already meant to start hidden, it just wasn't
       constructed in a way that survived a strict build long enough to
       reach that point.
    3. The "Active provider" `wxChoice` (`m_aiChatProviderChoice`) rendered
       as a near-invisible sliver instead of spanning the tab's width --
       reported directly by the maintainer from the same live run. Root
       cause: the choice is constructed empty (`RebuildAiProviderChoice()`
       populates it a moment later, once the four built-ins plus any
       custom entries are known), and neither its own sizer item nor the
       horizontal `providerBox` row it sits in carried an `Expand()`/
       stretch factor -- so both defaulted to their natural size, which for
       a not-yet-populated `wxChoice` is essentially zero. This is the
       same "one control crowds/shrinks to an invisible sliver" shape the
       AI chat *sidebar*'s own layout entry earlier in this section
       already warns about, just in a different dialog. Fixed by adding
       `wxSizerFlags(1).Expand()` to the choice's own `Add()` call and
       `wxSizerFlags().Expand()` to `providerBox`'s -- both are required
       together (a proportion-1 item in an unexpanded row still only gets
       that row's own natural width; an expanded row with no stretch
       factor on its child still leaves the child at its natural size
       inside the extra space).
    None of these three could be verified live in this sandbox for the
    same `wxUSE_SECRETSTORE=0` reason the rest of this feature couldn't
    (the tab never appears at all here) -- fixed by reading the exact
    wxWidgets contracts each violated (`wxGridSizer::DoInsert()`'s
    `rows*cols` cap, `wxHyperlinkCtrlBase::CheckParams()`'s assert, plain
    sizer-proportion semantics) rather than by reproducing them, and
    rebuilt clean with the existing unit tests (`test_AiProvider`,
    `test_ConfigRoundtrip`, `test_StyleConfigRoundtrip`) still passing
    unchanged -- none of them touch this tab's actual widget construction.
    Confirming the fix's actual on-screen effect (grid opens without
    asserting, both links stay hidden until populated, the dropdown spans
    the tab) needs the maintainer's own build to re-check.
  - **Follow-up (2026-09-11): a `WXM_USE_AI_TOOLS` CMake option landed
    (two commits by Wolfgang Dautermann/the maintainer) meaning to make the
    whole AI Chat feature optional at compile time -- but the plumbing had
    two independent bugs that together silently compiled the sidebar out
    of *every* build, regardless of the option's value, confirmed live
    with `nm` on a fresh build (`grep -c AiChatSidebar` -> 0) before this
    fix and non-zero after.**
    1. **`WXM_USE_AI_TOOLS` was never passed to the C++ preprocessor at
       all** -- no `add_compile_definitions`/`target_compile_definitions`
       anywhere. Every `#ifdef WXM_USE_AI_TOOLS` guard added to
       `Configuration.cpp`, `ConfigDialogue.cpp`/`.h`, `wxMaxima.cpp` and
       `wxMaximaFrame.h`/`.cpp` was therefore testing an *undefined*
       macro, which the preprocessor always reads as `0` -- permanently
       compiling out `Configuration`'s AI API key accessors,
       `wxMaximaFrame::m_aiChatSidebar`, and the whole Options "AI Chat"
       tab, no matter what the CMake option was set to. Fixed the same way
       `USE_FRIBIDI`/`USE_WEBVIEW`/`USE_QA` already are: added
       `#cmakedefine WXM_USE_AI_TOOLS` to `src/BuildConfig.h.cin` (picks up
       the CMake option of the same name automatically, no extra
       `set()` needed) and changed every `#if(WXM_USE_AI_TOOLS)` to
       `#ifdef WXM_USE_AI_TOOLS` to match `#cmakedefine`'s "defined or
       not," no-value semantics (the same reason `Bidi.cpp` uses `#ifdef
       USE_FRIBIDI`, never `#if USE_FRIBIDI`) -- every affected file
       already transitively includes `BuildConfig.h` via `precomp.h`
       (included as an ordinary header everywhere, not just as an actual
       PCH -- `WXM_ENABLE_PRECOMPILED_HEADERS` defaults off), so no new
       `#include` was needed anywhere.
    2. **`src/CMakeLists.txt` never actually compiled `AiChatSidebar.cpp`
       into `wxmaxima`, regardless of the option.** It had been removed
       from `SIDEBAR_SOURCE_FILES` and instead added via `if(WXM_USE_AI_TOOLS)
       list(APPEND SOURCE_FILES AiChatSidebar.cpp) endif()` -- but at that
       point in the file `SOURCE_FILES` doesn't exist yet (it's `set()`
       ~45 lines later, which wholesale overwrites whatever this line
       produced), and even the filename itself was wrong (missing the
       `sidebars/` prefix `list(TRANSFORM SIDEBAR_SOURCE_FILES PREPEND
       sidebars/)` applies to everything else in that list, two lines
       below the broken `append`). Fixed by moving the `if(WXM_USE_AI_TOOLS)`
       block to append to `SIDEBAR_SOURCE_FILES` (the correct list)
       *before* that `PREPEND sidebars/` transform runs, so the new entry
       gets the same path-prefixing treatment as its siblings.
    3. **The exact same mistake a third time, harmlessly, in two more
       places** -- `#if(WXM_USE_AI_TOOLS)` / `#endif` used as if it were a
       preprocessor guard inside `src/CMakeLists.txt` (around
       `AI_SOURCE_FILES`) and `test/unit_tests/CMakeLists.txt` (around
       `test_AiProvider`'s `add_executable`) -- but `#` is CMake's comment
       character, so both were just comments, and the code between them
       ran completely unconditionally either way. Harmless in the first
       spot (`AI_SOURCE_FILES`'s actual inclusion into `SOURCE_FILES` is
       separately, correctly gated by a real CMake `if()` a few dozen
       lines later) but meant `test_AiProvider` was never actually
       disabled by `WXM_USE_AI_TOOLS=OFF`, contrary to that commit's own
       message ("Disable AI test when AI is disabled..."). Fixed by
       deleting the two misleading fake-comment guards around
       `AI_SOURCE_FILES` (redundant with the real gating downstream) and
       turning `test_AiProvider`'s into a genuine CMake `if(WXM_USE_AI_TOOLS)
       ... endif()`.
    **Verified both directions, not just one**: a fresh configure+build
    with the option at its new default (`ON`, per the maintainer's own
    "Enable the AI sidebar by default" commit) now genuinely produces a
    binary containing `AiChatSidebar`/`Configuration::AiApiKeyAnthropic()`
    symbols (confirmed via `nm`, both present, both absent before this
    fix); a second fresh configure+build with `-DWXM_USE_AI_TOOLS=OFF`
    still compiles and links cleanly end to end with neither symbol
    present -- confirming the option now genuinely controls the feature
    in both directions, not just re-enabling it. Not yet verified live in
    Xvfb (the Options "AI Chat" tab actually opening, the sidebar actually
    appearing in View -> Sidebars) -- that's the natural next check before
    building anything new on top of this.
  - **Follow-up (2026-09-14): "network error" from both a local Ollama
    server and Anthropic -- the maintainer's own live report, and the one
    time this feature's root cause was found by reading the user's actual
    `~/.config/wxMaxima.conf` rather than by reasoning about the code.**
    The saved entry was
    `{"baseUrl":"127.0.0.1:11434","model":"llama3.2","name":"Ollama","shape":"anthropic"}`
    -- three separate bugs visible in one line, two of them wxMaxima's own:
    1. **`shape` was `anthropic` for what the user had set up as an
       OpenAI-compatible local server.** `ConfigDialogue::
       LoadAiProviderRecordIntoUi()` populated the "API style" `wxChoice`
       with `SetSelection(static_cast<int>(rec.shape))`, but that dropdown
       deliberately lists `OpenAiCompatible` *first* (it is the
       overwhelmingly common choice, and what every local-server preset
       uses) while `AiProviderShape` declares `Anthropic` first --
       so `OpenAiCompatible` (enum value 1) selected item 1,
       "Anthropic (Messages API)". `StashAiProviderUiIntoRecord()`'s own
       index->enum `switch` was correct, so the mismatch was silent *and*
       self-propagating: the picker showed the wrong style, and the next
       save-before-switch write-back stored what the picker showed.
       Deterministic on every single add, since
       `AddCustomAiProviderDialog()` ends with `RebuildAiProviderChoice()`
       -> `LoadAiProviderRecordIntoUi()`. Fixed with
       `AiProviderShapeToChoiceIndex()`/`AiProviderShapeFromChoiceIndex()`
       (`AiProvider.h`/`.cpp`) as the single source of truth, used by all
       three sites; both `shapeChoices.Add(...)` blocks carry a comment
       saying their order is what those two encode. **Never cast between
       this enum and a selection index directly.**
    2. **`baseUrl` was a bare `127.0.0.1:11434` -- no scheme, no path**,
       which is exactly what someone sets up a local server by typing, and
       is not a URL at all: libcurl reads everything before the first colon
       as the scheme, so this surfaced to the user as an unexplained
       network failure with nothing pointing at the URL. This is also what
       the maintainer's own hypothesis ("the custom AI provider lacks a
       setting that allows to change https to http") was really about --
       there is no such setting needed, the field has always been free text
       and every preset already fills in a full `http://` URL; the missing
       piece was that nothing said a scheme was required. Added
       `AiProviderRequestUrlProblem()`: empty if the URL is usable,
       otherwise a sentence naming the problem. Wired into
       `AddCustomAiProviderDialog()` (an inline `WrappingStaticText` plus
       the OK button's existing `wxEVT_UPDATE_UI` enable check -- silent
       while the field is still empty, so it nags only once something
       unusable has actually been typed) and into
       `AiChatSidebar::ReloadProviderFromConfig()`, which must check it too
       since an entry saved before this existed still carries a bad URL.
       **Deliberately does not guess a missing scheme**: prepending
       `http://` to a remote host would put the user's API key on the wire
       in clear text, and prepending `https://` to a plain-HTTP local
       server just swaps one confusing failure for another.
    3. **The stored API key for that entry was one character long** -- the
       user working around `ReloadProviderFromConfig()`'s
       `if (!apiKey.IsEmpty())` gate, which refused to build a custom
       provider without a key and reported the perfectly-configured local
       server as "No AI provider configured." A local server has no third
       party to authenticate to; the gate now checks the *URL* instead
       (the thing a custom entry genuinely cannot work without), and all
       three provider shapes omit their credential header entirely when
       the key is empty rather than sending an `Authorization` header whose
       value is a bare "Bearer" with nothing after it.
    **Separately, and the most broadly useful fix here:
    `SendChat()`'s `State_Failed` branch threw away
    `wxWebRequestEvent::GetErrorDescription()`**, reporting every single
    transport failure as "Could not reach %s (network error)." That string
    is why neither of the maintainer's two failures could be diagnosed
    from the app at all. The backend already distinguishes them
    perfectly -- confirmed live with a standalone wxWebRequest harness
    against this same wxWidgets 3.3.4/libcurl-gnutls build, which returned
    `"Could not resolve hostname"`, `"Could not connect to server"` and
    `"Failure when receiving data from the peer"` for three different
    causes, all of which the app was collapsing into one useless sentence.
    Now passed through verbatim, to both the chat history and the AI
    Connection Monitor. **The maintainer's Anthropic failure is still
    undiagnosed as of this entry** -- their key is present and well-formed
    (108 chars, `sk-ant-` prefix, so the provider really is built and the
    request really is sent), the machine has no system proxy and has valid
    CA certificates, and this sandbox's own egress proxy makes any direct
    HTTPS test from here worthless (plain `curl` reaches
    api.anthropic.com fine, the wx curl backend does not -- a proxy-auth
    artifact of the sandbox, reproduced and confirmed as such by removing
    the proxy env vars and watching the error change). The new message is
    what will identify it; don't guess at it further without that text.
    **Verified live** in a real Xvfb session driving the actual app with a
    throwaway `HOME` (the maintainer's own config left byte-identical,
    checked before and after): a keyless OpenAI-compatible entry pointed at
    a local test server now reports "Chatting with Ollama", enables Send,
    sends a correct OpenAI-shaped body with *no* auth header, and shows the
    reply -- and the maintainer's exact broken `127.0.0.1:11434` entry now
    shows "This provider's settings need fixing." with the full
    explanation in the chat history, without any network attempt at all.
    **GTK4 note for any future live check here**: wxWidgets 3.3.4/GTK4 on
    plain Xvfb never maps its window (it sits in a dmabuf/DRM path,
    `/proc/<pid>/fd` full of `syncobj_file` entries, screenshot solid
    black, zero children on the root window). `GSK_RENDERER=cairo
    GDK_BACKEND=x11 LIBGL_ALWAYS_SOFTWARE=1` fixes it completely -- worth
    reaching for before concluding the app failed to start. Also: this
    machine's wx build has `wxUSE_SECRETSTORE=1` and a working keyring, so
    unlike the older sessions this file documents, the AI Chat tab and
    sidebar *are* reachable here. And `pkill -f "src/wxmaxima"` kills the
    agent's own shell (its command line contains the pattern) -- match on
    `ps -eo pid,comm` instead.
  - **Follow-up (2026-09-21): HTTP Basic authentication for custom
    providers, and the one-credential-slot decision that shapes it.**
    Raised directly by the maintainer: "Ollama's documentation recommends
    to put ollama behind an authentication - and it looks like that being
    a password-and-username based one. Do we support that case?" The
    answer was **no** -- every provider shape authenticated through its
    own single header (`x-api-key`/`Authorization: Bearer`/
    `x-goog-api-key`) built from one stored secret, and nothing anywhere
    in this codebase had a notion of a *username*. A local server put
    behind an authenticating reverse proxy (nginx/caddy `basic_auth`,
    which is what Ollama's own docs steer people to) therefore could not
    be reached at all: wxMaxima would send its Bearer header, the proxy
    would answer 401, and -- per the `State_Unauthorized` entry further up
    this section -- that 401 does not even arrive as a normal completion.
    - **The design decision worth knowing before extending this: there is
      exactly ONE credential slot per provider, and a non-empty username
      reinterprets it.** `AiCustomProviderConfig` gained a plain
      `username` field (persisted in the same JSON array as
      `name`/`shape`/`baseUrl`/`model`, parsed with
      `entry.value("username", std::string())` so an entry written before
      this existed still loads), but the *password* is the already-
      existing secret-store entry -- the same `wxSecretStore` slot the API
      key uses, not a second one. `AiProvider::UsesBasicAuth()` is simply
      `!m_basicAuthUser.IsEmpty()`, and when it is true all three
      `AuthHeaders()` overrides emit `Authorization: Basic
      base64(user:secret)` *instead of* their own provider header
      (Anthropic keeps `anthropic-version`, which is a protocol version
      marker, not a credential). **What this covers is the real reported
      case** -- a local model server with no upstream API key of its own,
      sitting behind a proxy that wants a password. **What it deliberately
      does not cover** is a keyed cloud provider behind a Basic-auth
      proxy, which needs *two* independent secrets at once (the proxy's
      password and the provider's own key) and therefore a second secret-
      store slot plus a second UI field. Do not "fix" that by squeezing
      both into one field; it is a real feature with real UI cost, and
      nobody has asked for it.
    - **The Options UI renames its own field rather than growing a mode
      switch.** `ConfigDialogue::UpdateAiCredentialLabel()` flips the key
      row's label between "API key:" and "Password:" from a plain
      `wxEVT_TEXT` handler on the username box, so what the one secret
      slot currently *means* is visible without a checkbox or a radio
      group that would then also need persisting. The username row is
      shown only for a Custom provider, next to the request-URL and
      API-style controls that are already Custom-only -- a built-in
      provider's auth shape is implied by its kind and is not editable.
    - **The grid's row count had to be bumped from 4 to 5**
      (`wxFlexGridSizer(5, 2, 5, 5)`) in the same edit that added the row.
      This is not cosmetic: a `wxFlexGridSizer` with both rows and cols
      fixed hard-caps its item count at `rows*cols`, so adding a row
      without bumping the count asserts on the first item past the cap --
      exactly the bug this section's own 2026-09-08 entry already
      documents hitting twice. Any future row added here needs the same
      bump, and this sandbox will not catch it (`wxUSE_SECRETSTORE` is 0
      here, so the tab never opens).
    - **Verification**: `test_AiProvider.cpp` gained two SCENARIOs -- Basic
      auth across all three provider shapes (asserting the exact base64
      literals, cross-checked independently rather than read back out of
      the implementation; that an empty username leaves every existing
      header byte-for-byte unchanged; and that a colon inside the password
      is transmitted correctly, since RFC 7617 splits on the *first* colon
      only) and a username config round trip including an entry with no
      `username` key at all. 198 assertions in 14 test cases, all passing,
      plus the full 48-test unit suite and a clean `-Wall -Wextra
      -Wpedantic` build with `WXM_USE_AI_TOOLS` both ON and OFF. Not
      verified live, for this section's usual `wxUSE_SECRETSTORE=0`
      reason. **A trap worth repeating from the GitHub Models entry**:
      adding a field to `AiCustomProviderConfig` breaks every existing
      aggregate initializer under `-Wmissing-field-initializers`, which
      the `-Werror` CI jobs turn into a hard failure while a plain local
      build stays silent -- three initializers in `test_AiProvider.cpp`
      needed a trailing `wxS("")` and were caught locally only because
      that warning was enabled deliberately.
  - **Follow-up (2026-09-11): a third status bar icon for the AI Chat
    sidebar, mirroring the existing Maxima/network status icons.** Raised
    directly by the maintainer: "on the bottom right there are two spaces
    for symbols... could we add a third one so if the AI connection is
    active the leftmost of the 3 spaces could show an AI symbol... if the
    connection isn't active I would leave that space empty... single click
    might put the ai panel into the foreground... double-click might lead
    to an AI connection monitor sidebar like the XML monitor does." Neither
    the AI Chat sidebar nor the MCP server has a real persistent "session"
    to reflect (each Send is one stateless HTTP request/response, and
    `McpServer` deliberately never implements MCP's own `Mcp-Session-Id`
    concept -- see that section's own "Transport" note above), so "is the
    AI connection active" was redefined, with the maintainer's explicit
    sign-off ("Perhaps showing all those states in the icon makes sense =>
    let's implement that"), as a small state machine: `StatusBar::AiStatus`
    is `None` (no provider configured -- icon hidden entirely), `Active`
    (configured, last request -- if any -- succeeded), `Busy` (a request is
    currently in flight), or `Error` (the last request failed, detail in
    the tooltip).
    - **`StatusBar` itself stays free of any `AiProvider`/`WXM_USE_AI_TOOLS`
      dependency.** The constructor takes a plain `bool aiChatAvailable`
      (whether to reserve a 4th status bar field at all) rather than
      including `ai/AiProvider.h` or checking the macro itself --
      `wxMaximaFrame` computes that bool via
      `AiProvider::SecretStoreAvailable()` inside its own already-`#ifdef
      WXM_USE_AI_TOOLS`-guarded code and passes it in as a plain bool. This
      mirrors `GetTrayIconBitmap()`'s own reasoning (added for `TrayIcon`,
      GH #2286) for why `StatusBar` shouldn't grow feature-specific
      dependencies: it's constructed early, from `wxMaximaFrame`'s own
      constructor, and every other status/tray-icon consumer already goes
      through plain bitmaps/enums, not the features that produce them.
    - **Reusing `art/config/ai-chat.svg`'s exact motif for the icon hit an
      immediate CMake target-name collision**: `art/config/CMakeLists.txt`'s
      own bin2h loop already creates a target literally named
      `build_ai-chat.h` for the Options-tab icon of the same name -- CMake
      target names are global across the whole project, not per-directory,
      so `art/statusbar/ai-chat.svg.gz` reusing that exact basename failed
      configure with "another target with the same name already exists."
      Fixed by naming the three status-bar variants `ai-active`/`ai-busy`/
      `ai-error` instead (distinct from `art/config`'s `ai-chat`/
      `ai-chat-error`, which serve a different UI surface and were kept
      as-is) -- worth remembering for any future icon added under
      `art/statusbar/` that's inspired by an existing `art/config/` (or any
      other art directory's) file: check for a basename collision first,
      since nothing catches it until CMake's configure step actually runs.
    - **Tracking "what to show after a bitmap reload" needed its own
      logical-state member, not a bitmap-object comparison.**
      `StatusBar::UpdateBitmaps()` only runs on a genuine PPI change and
      reloads every bitmap from scratch (including the three new AI ones);
      the first draft tried to detect "was the icon currently showing the
      error bitmap" by comparing `m_aiStatus->GetBitmap() ==
      m_bitmap_ai_error` -- which is always false right after reloading,
      since `m_bitmap_ai_error` was simultaneously reassigned to a freshly
      rasterized (differently-backed) `wxBitmap` object a few lines above
      the comparison. Fixed the same way `m_oldNetworkState` already tracks
      `NetworkStatus()`'s own logical state independent of whatever bitmap
      object happens to be currently displayed: added `m_aiStatusState`/
      `m_aiStatusDetail`, updated on every `UpdateAiStatus()` call, and
      `UpdateBitmaps()` re-applies them (calling `UpdateAiStatus()` again
      with the freshly loaded bitmaps) after a PPI change instead of trying
      to infer the previous state from a bitmap comparison.
    - **The "Busy" state was a deliberate addition beyond the maintainer's
      original 3-space request**, made after the maintainer's own follow-up
      mid-session: "if the AI thought for a long time we should act like
      when Maxima thought for a long time and inform the user." Maxima's
      own `StatusBar::UpdateStatusMaximaBusy()` shows an immediate
      "calculating" icon+tooltip the instant it starts working, with no
      fixed threshold -- `AiChatSidebar::SetBusy(bool)` now mirrors exactly
      that (calls `UpdateAiStatusIcon()`, which reports `AiStatus::Busy`
      whenever `m_requestInFlight` is true) rather than only signaling busy
      after some delay. A *second*, smaller escalation was added on top for
      the "long time" half of the request specifically: a one-shot
      `wxTimer` (`LONG_WAIT_MS`, 10 seconds -- no existing precedent value
      to reuse, since Maxima's own status text/tooltip never escalates by
      elapsed time either, unlike `transferring`'s dynamic byte count) that,
      if the request is still in flight when it fires, updates both the
      sidebar's own status text and the icon's tooltip to say the request
      is "taking longer than usual" -- purely a wording change, not a new
      status; `AiStatus::Busy` covers both the just-started and the
      long-elapsed case, distinguished only by tooltip text (via
      `UpdateAiStatus()`'s existing `detail` parameter, the same mechanism
      `AiStatus::Error` already uses for its own detail text). The timer is
      started in `SetBusy(true)` and explicitly `.Stop()`'d in
      `SetBusy(false)`, so a request that finishes before 10 seconds never
      fires it at all.
    - **`AiConnectionMonitor` (`src/sidebars/AiConnectionMonitor.{h,cpp}`)
      mirrors `XmlInspector`'s shape** (a read-only `wxRichTextCtrl`-derived
      sidebar with colored section headers) but deliberately skips
      `XmlInspector`'s idle-driven `UpdateContents()`/batching entirely: one
      AI chat turn is a single user-paced Send click, not a flood of small
      socket reads, so `Add_Request()`/`Add_Response()` write directly and
      immediately rather than deferring. **No redaction of the displayed
      traffic is needed** -- confirmed (this session and the original
      `AiChatSidebar` follow-up both independently grepped for
      `m_apiKey`) that every provider's API key is used only inside its own
      `AuthHeaders()` return value, a request *header*, never inside
      `BuildRequestBody()`'s JSON body -- so the plain request/response text
      shown here never contains it.
    - **`AiProvider::SendChat()` gained two new optional trailing
      parameters**, `onRequest`/`onResponse` (both default `nullptr`,
      so the existing `test_AiProvider.cpp` call sites and the function's
      own contract are unaffected), invoked once each right before the
      request is sent and at every one of `SendChat()`'s existing terminal
      states (`State_Completed` both 2xx and non-2xx, `State_Unauthorized`,
      `State_Failed`, `State_Cancelled`, the `!request.IsOk()` early return,
      and the `!wxUSE_WEBREQUEST` compile-time fallback) -- deliberately
      exhaustive, mirroring every existing `callback(...)` call site 1:1,
      so the connection monitor can never silently miss a terminal state
      `callback` itself already handles.
    - **Verification**: `test_AiProvider` (116 assertions, unchanged --
      the new parameters default to `nullptr` and aren't exercised, same as
      `SendChat()` itself already wasn't per that file's own note) plus a
      full rebuild in both `-DWXM_USE_AI_TOOLS=ON` (confirmed via `nm`:
      `AiConnectionMonitor`/`AiStatusClick`/`UpdateAiStatus` all present,
      full link succeeds) and `=OFF` (confirmed via `nm`: zero occurrences
      of `AiConnectionMonitor`/`AiChatSidebar`/`AiStatusClick`, full link
      still succeeds) directions -- the same both-directions discipline the
      `WXM_USE_AI_TOOLS` plumbing fix above established. Not yet verified
      live in Xvfb (the icon's actual on-screen appearance/click behavior,
      the monitor sidebar's real traffic display) -- worth doing before
      extending this further.

## Sidebar visibility tools (the third, fourth and fifth writes)

- **`list_sidebars`/`show_sidebar`/`hide_sidebar` (2026-09-21)** -- requested
  by the maintainer (2026-09-09) as "for the next branch and PR": let an AI
  query which sidebars are currently visible, and show/hide them.
  - **Why these count as safe writes.** The backlog entry that carried this
    request flagged one thing to settle before writing code: whether toggling
    a sidebar belongs in the same category as `watch_variable`/
    `unwatch_variable` or in the category the "no write/evaluate tools" rule
    exists to keep out. It is the former, and the test is the one that
    section already states -- what is the blast radius if the model gets it
    wrong? Here it is "a pane appeared or disappeared": visible to the user
    the instant it happens, undone with one click, and unable to reach
    worksheet content, insert or edit a cell, or make Maxima evaluate
    anything. That is strictly *less* reach than `watch_variable`, which at
    least causes Maxima to be sent a query. Note this is a judgement about
    *this* kind of write, not a general loosening: an `insert_cell` or
    `evaluate_cell` is still the separate, much bigger decision the next
    section describes.
  - **Plumbing: a callback pair, for the same reason `maxima_connected`
    needed one.** `McpTools` holds only a `Worksheet` and a `Variablespane`;
    which panes exist and which are shown lives on `wxMaximaFrame`
    (`m_sidebarNames`/`m_sidebarCaption` plus its `wxAuiManager`), which
    neither of those can reach. `McpTools::SetSidebarAccess()` takes both
    halves -- enumerate, and set-visible -- in **one** call on purpose: a
    build wired for one but not the other would advertise half-working tools,
    and nothing at the tool layer could tell that apart from a genuine
    failure. Unset (the default, and what every test that doesn't care about
    sidebars does) makes all three tools throw `McpToolError` saying so,
    rather than returning an empty list that reads like "this wxMaxima has no
    sidebars." Wired once, in `wxMaximaFrame`'s own constructor right after
    `m_mcpServer` is created -- **not** in `wxMaxima`'s constructor where
    `SetConnectionCheck()` goes, because `m_sidebarNames`/`m_sidebarCaption`
    are *private* to `wxMaximaFrame` and a derived class cannot read them.
    Safe to wire there even though a good half of the panes (the symbol
    sidebars, wizard, find, help, the AI ones) are only registered further
    down that same constructor: both lambdas re-read the map on every call,
    so they report whatever exists when a request actually arrives.
  - **The identifier must be the untranslated name, and the caption must not
    be usable as one.** `m_sidebarNames` holds the stable config key
    (`"structure"`, `"variables"`, ...); `m_sidebarCaption` holds the
    `_()`-wrapped text the user sees (`"Table of Contents"`). `list_sidebars`
    reports both, but only the name is accepted back -- an AI that keyed off
    the caption would break the moment wxMaxima runs in any language but
    English. The error message for an unknown name lists the known names and
    says explicitly that captions are not names, since "I passed what
    list_sidebars showed me" is the obvious way to get this wrong.
  - **Two failure modes that had to be caught at the tool layer, because the
    frame's own API swallows both.** (1) `wxMaximaFrame::ShowPane()` silently
    does nothing for a name it doesn't know -- so handing it an unchecked
    name would have us report success for a typo, or for a pane this build
    was compiled without. The tools look the name up first and throw if it
    isn't there. (2) `ShowPane()` *asserts* if asked to hide the worksheet
    (it is wxAUI's centre pane; see the centre-pane entry in `AGENTS.md`), so
    `hide_sidebar` refuses `console` before reaching it -- an assert firing
    inside what is, to the caller, an ordinary tool call is not an acceptable
    way to report "you can't do that". `list_sidebars` reports this up front
    as `can_hide: false` so it never has to be discovered by failing.
  - **`McpSidebarList()` sorts by name.** `m_sidebarNames` is an
    `unordered_map`, so without the sort the same wxMaxima lists its sidebars
    in a different order from one run to the next.
  - **The result reads visibility back rather than echoing the request**, so
    a pane that declined to change shows up as unchanged instead of being
    reported as done.
  - **Verification.** `test_McpTools.cpp` gained two SCENARIOs (146
    assertions in 10 test cases, all passing): the tool layer against a
    stand-in for the frame -- a real `wxMaximaFrame` is far too heavy to
    build in a unit test, and the callback design is exactly what makes one
    unnecessary -- covering listing, show, hide, show-when-already-shown,
    the `console` refusal, an unknown name, a caption used as a name, a
    missing argument, the unset-access case, and `CallTool()` dispatch.
    Since that stand-in is by definition not the real frame, the frame half
    was verified **live** instead, which is what actually proves
    `McpSidebarList()`/`McpSetSidebarVisible()` work against a real
    `wxAuiManager`: a real Xvfb session with `mcpServerEnabled=1`, `curl`-ing
    all three tools -- `list_sidebars` returned all 17 real panes sorted,
    with `console` correctly `can_hide: false`; `show_sidebar variables` and
    `hide_sidebar structure` both took effect and were confirmed by a
    screenshot (Variables docked at the bottom, Table of Contents gone, every
    other pane untouched) as well as by reading `list_sidebars` back; and
    hiding `console`, an unknown name and a caption-as-name each returned a
    JSON-RPC error with the intended message.
  - **A test bug worth remembering, caught only because the assertion
    failed:** `for (const auto &t : tools.ListTools()["tools"])` iterates a
    json that has *already been destroyed*. The temporary `ListTools()`
    returns is not lifetime-extended, because `operator[]` yields a reference
    *into* it rather than the temporary itself, and only the latter would be
    extended before C++23. The symptom was three assertions failing as though
    the tools were never registered, with the source plainly showing that
    they were -- which sends you looking at the build, not the test. Bind the
    call's result to a named local first. A sweep found no other instance of
    this shape in the tree.

## Deliberately not implemented: a write/evaluate-capable MCP tool

- **Not implemented, and shouldn't be without a separate decision: a
  write/evaluate-capable MCP tool.** Raised and discussed directly with
  the user (2026-09-06): unlike `watch_variable`/`unwatch_variable` (see
  this section's own "why 'read-only except two things' and not
  stricter" note above), an `evaluate_cell`-style tool is not a bounded
  side effect -- Maxima has no sandbox at all around it (it can `system()`
  out to the shell, read/write arbitrary files the user can access, ...),
  so letting an AI evaluate a cell is, in effect, letting it run arbitrary
  code on the user's machine. Per-call user consent does not bound that
  blast radius the way it does for a sidebar-display change. If this is
  picked up later: a consent-gated `insert_cell` that only inserts text
  *without* auto-evaluating it (the user still has to press Enter/
  Shift-Enter themselves) is a meaningfully safer middle ground than a
  tool that evaluates anything -- treat a real `evaluate_cell` as a
  separate, much bigger decision that needs either genuine sandboxing
  around the Maxima process or explicit human-in-the-loop confirmation
  on every single call, not a one-time "make this permanent" toggle.
