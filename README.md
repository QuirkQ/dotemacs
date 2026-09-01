# Quint's Emacs configuration

Emacs 31.1 on macOS (Apple Silicon), managed with straight.el + use-package.
Caps Lock is a Hyper key via Karabiner-Elements. Ruby intelligence comes from
eglot + ruby-lsp, launched under mise.

The configuration lives at `~/.emacs.d`. `~/.emacs` is only ever read as an
init *file*; Emacs never reads it as a directory, so a clone at that path would
never load.

## Layout

| Path | Contents |
|------|----------|
| `early-init.el` | `.env` loading, GC threshold, backup/auto-save off, `explicit-shell-file-name` |
| `init.el` | everything else |
| `lisp/my-hyper.el` | the Caps-Lock Hyper key layer; a pure keymap that requires nothing, so it loads under `emacs -Q --batch` |
| `lisp/my-op.el` | the 1Password reader — one asynchronous `op` call per session for every secret; depends on no package, so it loads under `emacs -Q --batch` |
| `bin/mise-ruby-lsp` | ruby-lsp launcher for eglot (mise Ruby + the JFrog Bundler credential) |
| `test/check-config.sh` | offline static checks |
| `test/check-runtime.sh` | launches the real Emacs with a Dock-like environment and asserts what only exists after init |
| `.env.example` | template for `.env`, which is gitignored |

## Requirements

Before the first launch:

- **Emacs 31.1** — `brew tap d12frosted/emacs-plus && brew install --cask emacs-plus-app`.
  Do not keep an `emacs-plus@N` *formula* installed alongside the cask; both provide
  `emacs` and `emacsclient` in `$(brew --prefix)/bin` and the cask states the
  combination is unsupported.
- **`brew install mise`** — per-project runtimes.
- **Karabiner-Elements**, with the Caps-Lock rule described below. Without it the
  whole Hyper layer is unreachable.
- **1Password CLI** — the `1password-cli` cask, at `/opt/homebrew/bin/op`, with
  desktop-app integration enabled. Needed for Ruby projects whose gems come from
  `nedap.jfrog.io`, and for Forge. See [Secrets](#secrets).
- **A GitHub SSH key** — `straight-vc-git-default-protocol` is `ssh`, so every
  package clone goes over SSH.
- **`git config --global github.user QuirkQ`** — ghub reads the GitHub username
  from Git, not from Emacs, and signals when it is missing. Not a secret; the
  token that goes with it comes from 1Password.
- **JetBrains Mono Nerd Font** — `brew install --cask font-jetbrains-mono-nerd-font`.
- **`.env`** — copy `.env.example` and fill in `OPENROUTER_API_KEY` if aidermacs
  will be used. `early-init.el` loads it; the file is gitignored.

The first launch is slow and needs the network. straight clones and
native-compiles every package, and because `~/.emacs.d/tree-sitter` does not
exist yet, `init.el` installs all twenty tree-sitter grammars synchronously
before the rest of startup continues. Both happen once.

`init.el` starts a server unless one is already running, so `emacsclient` works.

## The Hyper key

Karabiner-Elements maps `caps_lock` to `left_shift` + `left_control` +
`left_option` + `left_command`, with `to_if_alone` → `escape`. Held, Caps Lock is
Hyper; tapped, it is ESC.

With `ns-command-modifier` at its default of `super`, Emacs sees that chord as
`C-M-S-s-`.

### Why every key is bound several times

Shift is part of the chord, and which spelling macOS actually delivers cannot be
predicted per key. It depends on whether Shift reaches the application, whether
the keyboard layout substitutes the shifted glyph (`.`→`>`, `1`→`!`, `;`→`:`, …),
and whether Emacs' shift translation drops the `S-` bit — which it does for keys
that are really ASCII characters, so Hyper-RET arrives as `C-M-s-<return>`, not
`C-M-S-s-<return>`.

Rather than guess, `my-hyper-set` binds the whole cross-product of
`{C-M-S-s-, C-M-s-}` against `{the key, its Shift-folded spelling}`, keeping only
spellings that satisfy `key-valid-p`. The 55 chords below therefore expand to 210
global key strings naming 54 distinct commands (`xref-find-definitions` is on two
chords). The redundant bindings are harmless: they all name the same command.

If a key ever misbehaves, `C-h k` reports the spelling Emacs actually saw.

Two consequences of folding Shift into the chord:

- **Case cannot be distinguished at the chord position.** Hyper-r and Hyper-R are
  one event, as are Hyper-/ and Hyper-?. Keys *after* the chord are typed
  unmodified and do distinguish case, which is why `a r` and `a R` can coexist.
- **The shifted pairs are one key each**: `1`/`!`, `2`/`@`, `3`/`#`, `0`/`)`,
  `=`/`+`, `;`/`:`, `/`/`?`, `,`/`<`, `.`/`>`.

### Project and file navigation

| Key | Command | Description |
|-----|---------|-------------|
| `Hyper-p` | `project-find-file` | Find file in the current project |
| `Hyper-t` | `treemacs` | Toggle the file tree sidebar |
| `Hyper-b` | `ivy-switch-buffer` | Switch buffer |
| `Hyper-k` | `kill-current-buffer` | Close the current buffer |
| `Hyper-w` | `save-buffer` | Save |
| `Hyper-←` | `previous-buffer` | Previous buffer |
| `Hyper-→` | `next-buffer` | Next buffer |
| `Hyper-d` | `counsel-git` | Find a git-tracked file |
| `Hyper-f` | `counsel-git-grep` | Search the git repository |

### Git

| Key | Command | Description |
|-----|---------|-------------|
| `Hyper-g s` | `magit-status` | Magit status |
| `Hyper-g c` | `magit-commit` | Commit |
| `Hyper-g p` | `magit-push` | Push |
| `Hyper-g l` | `magit-log-all` | Log |
| `Hyper-g b` | `magit-blame` | Blame the current file |
| `Hyper-g f` | `magit-pull` | Pull |

### AI assistance

| Key | Command | Description |
|-----|---------|-------------|
| `Hyper-a g` | `aidermacs-run` | Start Aider in the project |
| `Hyper-a s` | `aidermacs-question-code` | Ask about the selected region |
| `Hyper-a f` | `aidermacs-add-file` | Add a file to the context |
| `Hyper-a b` | `aidermacs-add-current-file` | Add the current file to the context |
| `Hyper-a r` | `aidermacs-drop-current-file` | Drop the current file |
| `Hyper-a R` | `aidermacs-drop-all-files` | Drop every file |
| `Hyper-a k` | `aidermacs-exit` | Quit the session |

### Development tools

The `c` prefix; this is also where the old `<f19> R` Ruby group ended up.

| Key | Command | Description |
|-----|---------|-------------|
| `Hyper-c c` | `compile` | Compile |
| `Hyper-c r` | `my/ruby-run-tests` | Run this project's Ruby test suite |
| `Hyper-c R` | `my/rails-console` | Rails console in a ghostel terminal |
| `Hyper-c f` | `my/ruby-format-buffer` | Format via the language server |
| `Hyper-c l` | `flycheck-list-errors` | List diagnostics |
| `Hyper-c n` | `xref-find-references` | Find references |
| `Hyper-c h` | `eldoc-doc-buffer` | Documentation for the symbol at point |
| `Hyper-c a` | `eglot-code-actions` | Code actions |
| `Hyper-c d` | `docker` | Docker interface |
| `Hyper-c t` | `ghostel` | Terminal |

### LSP navigation

| Key | Command | Description |
|-----|---------|-------------|
| `Hyper-.` | `xref-find-definitions` | Jump to definition |
| `Hyper-,` | `xref-go-back` | Jump back |
| `Hyper-RET` | `xref-find-definitions` | Jump to definition (keyboard "click") |

### Windows

| Key | Command | Description |
|-----|---------|-------------|
| `Hyper-o` | `other-window` | Other window |
| `Hyper-1` | `delete-other-windows` | Fill the frame with this window |
| `Hyper-2` | `split-window-below` | Split below |
| `Hyper-3` | `split-window-right` | Split right |
| `Hyper-0` | `delete-window` | Close this window |
| `Hyper-=` | `balance-windows` | Balance sizes |

### Macros

| Key | Command | Description |
|-----|---------|-------------|
| `Hyper-r` | `kmacro-start-macro` | Start recording |
| `Hyper-e` | `kmacro-end-macro` | Stop recording |
| `Hyper-SPC` | `kmacro-end-or-call-macro` | Finish or replay |
| `Hyper-m` | `kmacro-name-last-macro` | Name the last macro |

### Quick actions

| Key | Command | Description |
|-----|---------|-------------|
| `Hyper-;` | `comment-or-uncomment-region` | Toggle comments |
| `Hyper-u` | `undo` | Undo |
| `Hyper-/` | `swiper` | Search this buffer |
| `Hyper-i` | `imenu` | Jump to a definition in this buffer |
| `Hyper-j` | `avy-goto-char` | Jump to a character |
| `Hyper-l` | `goto-line` | Go to line |
| `Hyper-x` | `execute-extended-command` | M-x |
| `Hyper-q` | `keyboard-quit` | Cancel |
| `Hyper-h` | `which-key-show-top-level` | Show the available bindings |
| `Hyper-ESC` | `keyboard-escape-quit` | Escape everything |

### Where this differs from the old F19 layout

The layer is otherwise a 1:1 port of the `<f19>` layout it replaced. The
exceptions:

- **The Ruby group moved from `R` to the `c` prefix.** Hyper-R cannot be told
  apart from Hyper-r, which is `kmacro-start-macro`.
- **which-key help moved from `?` to `h`.** `?` is the same physical key as `/`,
  which is swiper.
- **`<f19> <f19>` → `execute-extended-command` could not be ported.** Hyper is a
  modifier, not a key: there is no standalone Hyper key event to double-tap.
  `Hyper-x` is still `execute-extended-command`.
- **robe's `<f19> R r` / `R d` / `R s` are gone** along with robe itself. Jump to
  definition is `Hyper-.`; documentation is `Hyper-c h`.
- **`Hyper-3` collides with macOS.** ⌃⌥⇧⌘3 copies a screenshot to the clipboard.
  Turn it off under System Settings → Keyboard → Keyboard Shortcuts → Screenshots
  to get `split-window-right`.

### Shared with Zed

`Hyper-.`, `Hyper-,` and `Hyper-RET` are deliberately the same physical keys as
in `~/.config/zed/keymap.json`, so jump-to-definition and jump-back carry between
the two editors. `Hyper-SPC` does not: here it is `kmacro-end-or-call-macro`,
where Zed uses that chord for `editor::AcceptEditPrediction`.

## Secrets

`lisp/my-op.el` is the only thing here that talks to 1Password. It reads **every**
secret the configuration needs in a single `op inject`, at most once per Emacs
session:

```elisp
(my-op-get-async 'jfrog-token #'f)  ; never blocks; f is called with the value
(my-op-get 'jfrog-token)            ; blocks; Bundler credential for nedap.jfrog.io
(my-op-get 'github-token)           ; blocks; the PAT Forge authenticates with
(my-op-refresh)                     ; forget them and read again
```

One call, not one per secret, for the same reason
`~/.config/zsh/functions/tokens` does it that way: a GUI Emacs has no shell
session, so there is no `op` session token to reuse and *every* invocation can
raise its own biometric prompt. Both read from the same account and vault as
that shell function, so the two stay in step.

### Why the read is asynchronous

`op` does not answer until a human has answered a Touch ID prompt, and the first
caller of the session is a mode hook — opening a `.rb` file. Read with
`call-process` from there, that wait held Emacs's only Lisp thread for its whole
duration: no redisplay, so the frame went blank the moment the 1Password sheet
uncovered it, and no keyboard input either, so `C-g` could not get in. Opening a
Ruby file could only be escaped by force-quitting Emacs.

So `my-op-get-async` is the entry point for anywhere that wait would be
unattended, and it is what `ruby-ts-mode-hook` uses. `my-op-get` still blocks and
is for the callers that cannot defer — `auth-source` is synchronous by contract,
so the Forge path is one — but it waits inside `accept-process-output`, which
keeps servicing input, so `C-g` works and the frame is not left unpainted.

The cache has four states — never asked, *in flight*, the secrets, and *asked and
failed*. `in-flight` is what keeps one prompt per session true now that the read
is asynchronous: everyone who asks while `op` is out queues behind the process
already running instead of starting — and prompting for — one of their own. That
holds for a blocking caller arriving mid-flight too. `failed` is what stops a
locked 1Password from sending every Ruby buffer and every Forge request back to
`op`, which is the prompt storm the whole file exists to prevent.

Failure is never fatal and never signals: the first call can come from a mode
hook, where a signal would take `eglot-ensure` down with it. A missing CLI, a
locked vault or a renamed field each report a message and yield nil, and the
callers carry on without the credential. `M-x my-op-refresh` retries — for after
a token rotates, or after unlocking 1Password following a failed read.

An empty field is reported but only nils out its own key. A broken GitHub item
must not cost the Ruby setup its Bundler credential.

Only `op://` paths are in this repository. The values they resolve to must never
reach a file, a log, a commit or `message`.

`test/op-assertions.el` drives all of this against a stub CLI in a temp
directory, so the checks stay offline and cannot raise a prompt.

## Forge

[Forge](https://github.com/magit/forge) is loaded `:after magit`, so it comes up
with the first `C-x g` and adds its issue and pull-request sections and its own
keys inside Magit. It keeps a cache at `forge-database.sqlite`, which is
gitignored; Emacs 31's built-in SQLite means emacsql needs nothing compiled.

Forge holds no token of its own — ghub looks one up through `auth-source`,
keyed on the API host and an ident of `<github.user>^forge`. So the token
arrives by answering that query rather than by an `~/.authinfo` entry, and it
stays in 1Password and in memory:

- `my/github-auth-source-search` answers for `api.github.com` and `github.com`
  with `(my-op-get 'github-token)`, and returns nil for everything else.
- `my/github-auth-source-backend` wraps it as an `auth-source` backend, hooked
  onto `auth-source-backend-parser-functions` and put at the front of
  `auth-sources`.

The backend has to do its own host matching. `auth-source-search` does not
re-check a backend's results against the spec it was given, so a search function
that answered indiscriminately would hand the GitHub token to the next caller
that asked auth-source for an SMTP password. A wildcard host (`t`) deliberately
does not match either.

When 1Password comes up empty the search returns nothing rather than an empty
credential, and auth-source falls through to the netrc backends behind it.

## Ruby

### ruby-lsp under mise

eglot (built-in) is hooked into `ruby-ts-mode` and `ruby-mode`, and the server
registered for both is `bin/mise-ruby-lsp`. ruby-lsp is the only Ruby language
server, matching the Zed config's
`"language_servers": ["ruby-lsp", "!solargraph", …]`. robe is gone, along with
`Gemfile.robe`.

The shim exists for two things Emacs cannot do for itself:

1. **ruby-lsp has to run under the project's mise Ruby**, with the matching
   `GEM_HOME`/`GEM_PATH`, so the shim ends in `mise exec -- ruby-lsp`. This is
   the launch strategy ruby-lsp documents for version managers.
2. **Bundler needs the Artifactory credential** to resolve gems from
   `nedap.jfrog.io`, and it lives in 1Password. A GUI-launched Emacs has no shell
   environment — `~/.config/zsh` never runs — so nothing has set it.

Tool paths in the shim are absolute (`/opt/homebrew/bin/op`,
`/opt/homebrew/bin/mise`) for the same reason. It mirrors
`~/.local/bin/zed-ruby-lsp`; Zed's `lsp.ruby-lsp.binary.path` can be repointed at
this file to keep a single shim under version control, and currently still points
at the copy in `~/.local/bin`.

### The JFrog token

`init.el` supplies the token, not the shim. The read itself belongs to
[`lisp/my-op.el`](#secrets); what is Ruby-specific is *when* it is asked for and
*how* it gets to the server.

It is asked for only from a buffer whose project references `nedap.jfrog.io` in
its `Gemfile` or `Gemfile.lock`. Projects that do not use the private source
never reach 1Password at all. The value is then consed onto the buffer-local
`process-environment` as `BUNDLE_NEDAP__JFROG__IO`, which the language server
inherits when eglot connects.

Doing it here rather than in the shim matters because eglot launches the shim
once per language server — one per project, one after every server restart, one
after every Emacs restart — and each of those launches could otherwise raise its
own biometric prompt.

**It is the server start that waits for 1Password, not the editor.**
`ruby-ts-mode-hook` runs `my/ruby-start-lsp`, which asks through
[`my-op-get-async`](#why-the-read-is-asynchronous) and returns immediately;
`eglot-ensure` is called from the callback, once the token is in hand. Where no
credential is needed — most projects, and every buffer after the session's one
read — it is the same immediate `eglot-ensure` as before. This is why
`ruby-ts-mode` and `ruby-mode` are *not* in eglot's own `:hook`.

Consed onto `process-environment`, never assigned over it, so mise's Ruby stays
on the buffer's `PATH` whichever of the two got there first. Arriving after mise
is fine: `mise--update` runs once per file buffer, at
`after-change-major-mode-hook`.

A failed read is not fatal. It reports a message and starts ruby-lsp anyway,
which is still useful in projects whose Artifactory gems are already installed.
Buffers already carrying an old value pick up a refreshed one when their language
server next restarts.

### Diagnostics

Flycheck, not flymake: `eglot-stay-out-of` is `'(flymake)`, so there is exactly
one source of squiggles and standardrb does not run twice over the same buffer.

The checker is chosen per project by reading `Gemfile.lock` and `Gemfile` —
`ruby-standard` when the `standard` gem is there, `ruby-rubocop` otherwise. With
neither file present it falls back to whatever `standardrb` is on `exec-path`.

The command runs through `flycheck-command-wrapper-function`, which prefixes the
whole argv with `mise x --`, plus `bundle exec` when the project has a Gemfile.
Flycheck has no per-checker argument list, so this is the supported hook.
`flycheck-executable-find` is additionally set buffer-locally to `identity`: that
hands the bare name straight through, so the argv reads
`mise x -- bundle exec standardrb …` and resolution happens inside the bundle.
Without it, flycheck's `:enabled` predicate resolves the name over `exec-path`
and never sees the wrapper — silently disabling the checker when the linter lives
only in the bundle, and putting the absolute *global* binstub under `bundle exec`
when it does not.

`C-c ! v` (`flycheck-verify-setup`) shows which checker was selected.

Removing `flymake` from `eglot-stay-out-of` swaps this for ruby-lsp's own
diagnostics.

### Formatting

eglot's, on save: `eglot-format-buffer` on a buffer-local `before-save-hook`.
ruby-lsp picks StandardRB or RuboCop from the project itself. This mirrors Zed's
`"format_on_save": "on"` with `"formatter": "language_server"` for Ruby.

The hook is taken back off when the server goes away — `eglot-managed-mode-hook`
fires in both directions, and without that check every save after a shutdown
printed "No current JSON-RPC connection".

### Ruby commands

`Hyper-c r` (`my/ruby-run-tests`) picks the runner from what the project actually
has: a `.rspec` file, the `rspec` gem, or a `spec/` directory means RSpec;
otherwise a `test/` directory plus `bin/rails` means `rails test`, and `test/`
plus a `Rakefile` means `rake test`. The command is prefixed with `mise x --`,
plus `bundle exec` when there is a Gemfile.

`Hyper-c R` (`my/rails-console`) runs `mise x -- bundle exec rails console`
through `ghostel-exec`, and errors out when the project has no `bin/rails`.
`ghostel-exec` takes the command as distinct argv entries — nothing is handed to
a shell to re-parse — and is documented as skipping the macOS `login(1)` wrap
that `ghostel-macos-login-shell` applies to interactive shells. The buffer is
displayed before the process starts, because `ghostel-exec` sizes the pty from
the window the buffer is already in and falls back to 80×24 otherwise.

The project root for all of these comes from `locate-dominating-file` on
`Gemfile`, `Gemfile.lock`, then `.git` — not `vc-root-dir`, whose state is
populated from `find-file-hook` and is therefore still nil when a major-mode hook
runs.

## Terminal

The terminal is [ghostel](https://github.com/dakra/ghostel), which runs
libghostty-vt — the VT engine from Ghostty — as a native module. It replaced
vterm.

The native module is a **prebuilt binary fetched from GitHub releases** on the
first `M-x ghostel`. `ghostel-module-auto-install` is `download`, so it fetches
rather than prompting. Nothing is compiled: no cmake, no libtool, no vendored
libvterm, and none of the Apple-Silicon `libvterm.dylib` archaeology vterm
needed. `ghostel-module-compile` builds from source instead if you ever want
that, and needs a Zig toolchain.

The straight recipe carries an explicit `:files` list mirroring MELPA's. The
Lisp lives in `lisp/`, which straight's default directive already covers, but
`etc/` holds the bundled `xterm-ghostty` terminfo and `src/` + `vendor/` +
`build.zig*` are what a local module build would need.

`ghostel-shell` is `/bin/zsh`. On macOS ghostel additionally wraps it through
`login(1)` — see `ghostel-macos-login-shell`, on by default — so `~/.zprofile`
is sourced the way Terminal.app and Ghostty do it. **This is a behaviour change
from vterm**, whose shell was interactive but not a login shell: anything in
`~/.zprofile` now runs that previously did not.

An interactive zsh runs `mise activate zsh`, so a ghostel shell already has the
project's runtimes. Emacs' own subprocesses do not go through zsh, which is why
`compile` and flycheck prefix `mise x --`.

### Why ghostel and not vterm

vterm could not draw a full-screen TUI cleanly at any setting, and the reason is
structural rather than a matter of tuning. libvterm reports damaged cells with
no notion of where a frame begins or ends, so `vterm-timer-delay` — which
coalesces every damage report into one `run-with-timer` and drops the rest until
it fires — was the only lever available, and every value on it traded torn
frames against dropped ones. Measured here at 290×77 against a pty repainting
its whole viewport 30 times a second, counting the redraws that survived
coalescing:

| `vterm-timer-delay` | frames drawn / 90 sent | effective |
| --- | --- | --- |
| `0.1` (stock) | 32 | 9 fps |
| `0.05` | 48 | 13 fps |
| `0.033` | 92 | 25 fps |
| `0.02` | 92 | 26 fps |
| `nil` | 2094 | 544 fps |

There is no good row. The stock 0.1 drops two frames in three; `nil` draws 90
frames as 2094 partial redraws, which is what a half-drawn frame actually is.
0.033 was the knee and still tore.

libghostty-vt implements **synchronized output** (DEC mode 2026): the
application brackets each frame, and the terminal withholds the redraw until the
frame is complete. Claude Code uses it. So the frame budget stops being a
setting to tune — which is why none of the measurements above carried over, and
why `ghostel-timer-delay` is left alone. Upstream's default is already 0.033,
the same knee, and `ghostel-adaptive-fps` varies it under load.

Two further things deleted configuration outright:

- **`TERM` is `xterm-ghostty` with terminfo bundled in `etc/`**, so a TUI
  queries the terminal's real capabilities instead of assuming a 1990s xterm.
- **The wheel is bound natively** (`ghostel.el:1072`) at
  `emulation-mode-map-alists` priority, and copy mode exits itself. vterm bound
  no wheel events at all, so the trackpad fell through to `mwheel-scroll` and
  the next redraw called `vterm-reset-cursor-point` and yanked you back to the
  prompt. Making that stick took ~60 lines: enter copy mode on wheel-up, leave
  on wheel-down, remap `self-insert-command` so typing resumed, and bind every
  double- and triple- spelling a fast trackpad flick produces. All gone, and so
  is the caveat that came with it — vterm's copy mode called `vterm-send-stop`,
  XOFF on the pty, so scrolling back actually throttled the application.

### What is still configured, and why

Only the parts that are Emacs' own display layer rather than the VT engine, so
they outlived the move:

- **`nobreak-char-display` is nil in ghostel buffers.** Claude Code pads its UI
  with U+00A0, and the stock `nobreak-space` face is
  `(:inherit escape-glyph :underline t)` — so every pad character rendered as a
  cyan underlined cell, appearing as a stray "underscore" after the `❯` prompt.
  Highlighting non-break space is a prose-editing aid; a terminal must draw
  exactly the glyphs the application sent.
- **`bidi-paragraph-direction` is pinned** to `left-to-right` with
  `bidi-inhibit-bpa`, so redisplay skips the bidirectional algorithm on
  290-column lines it re-runs every frame. ghostel sets `truncate-lines` and the
  scroll margins itself, but not these.
- **`gc-cons-threshold` settles at 32MB, not Emacs' stock 800KB.** A terminal
  conses a fresh propertized string per line per redraw. At 800KB a
  full-viewport repaint blew the budget several times a second, and collections
  in this config measured ~26ms each — every one a visible hitch.
- **company and emojify are switched off in `ghostel-mode`.** Both are
  *globalized* minor modes that turn themselves on from
  `after-change-major-mode-hook`, which `run-mode-hooks` runs **after**
  `ghostel-mode-hook` — so disabling them in a mode hook gets silently undone,
  and `company-global-modes` / `emojify-inhibit-major-modes` are the only
  opt-outs that stick. company matters more here than it did under vterm:
  ghostel forwards foreign buffer insertions to the pty from
  `after-change-functions`, so a completion that edits the buffer is a
  completion typed at the shell.
- **`read-process-output-max` is 1MB** — fewer `read(2)` calls when a command
  dumps a lot of text. It was never what stopped partial frames; under vterm,
  64KB and 1MB reads delivered identical frame counts.

**font-lock is no longer load-bearing.** Under vterm it was: vterm's module
wrote every colour into a `font-lock-face` property, which reaches redisplay
only through the `(face font-lock-face)` entry font-lock pushes onto
`char-property-alias-alist`, so excluding `vterm-mode` from
`font-lock-global-modes` looked free and instead dropped every colour in the
terminal. ghostel writes the real `face` property, turns font-lock off in
`ghostel-mode` itself, and points `font-lock-unfontify-region-function` at
`#'ignore` so a config that forces font-lock back on cannot strip the per-cell
faces on redraw.

### Muscle-memory aliases

vterm and multi-vterm are gone, but the old `M-x` names still work. Everything
routes through a `my/term*` layer, so swapping the backend again means editing
those aliases and nothing else.

| Old name | Now runs | Note |
| --- | --- | --- |
| `vterm` | `my/term` → `ghostel` | |
| `vterm-other-window` | `my/term-new` | |
| `multi-vterm` | `my/term-new` | `multi-vterm` always made a **new** numbered terminal; plain `ghostel` reuses the existing one and only creates on a prefix arg, so this is a wrapper rather than a straight alias |
| `multi-vterm-next` / `-prev` | `ghostel-next` / `ghostel-previous` | |
| `multi-vterm-dedicated-toggle` | `my/term-dedicated-toggle` | ghostel has no dedicated-window concept, so this is a side window at the bottom, 30 lines — the height `multi-vterm-dedicated-window-height` used to ask for |

`my/term-project` (`ghostel-project`) and `my/term-list`
(`ghostel-list-buffers`) have no vterm predecessor.

Inside a ghostel buffer, `C-c C-t` is copy mode from `ghostel-mode-map`, which
outranks the global `C-c C-t` — the same arrangement `vterm-copy-mode` had with
`multi-vterm`.

## Fonts, icons and emoji

The default face, `fixed-pitch` and `nerd-icons-font-family` all resolve through
a candidate list — `JetBrainsMono Nerd Font Mono`, then `JetBrainsMono NFM`, then
`Menlo` — taking the first that `find-font` resolves. Nerd Fonts set family name
(name ID 1) to `JetBrainsMono NFM` and typographic family (name ID 16) to
`JetBrainsMono Nerd Font Mono`, and Core Text is inconsistent about which one it
reports; a wrong literal falls back to Menlo silently, at which point every
nerd-icons glyph renders as a hex box.

nerd-icons upstream defaults to `Symbols Nerd Font Mono`, which is not installed
here. The JetBrainsMono Nerd Font patch carries the same Private Use Area glyphs,
so icons render inline in any buffer, not only where nerd-icons sets its own face.

Emoji are drawn natively: a fontset rule prepends `Apple Color Emoji` for the
`emoji` script. `emojify` runs in `unicode` style purely to rewrite shortcodes
(`:wink:` → the codepoint) — its `image` default needs an asset set downloaded
into `~/.emacs.d/emojis`, which does not exist, so nothing rendered.

## Packages

- **Completion and search** — ivy, counsel, swiper; company with company-box.
- **Git** — magit, forge, transient, ibuffer-vc, treemacs-magit.
- **Project and files** — built-in `project`, treemacs with the nerd-icons theme,
  dashboard, page-break-lines.
- **Environment** — mise.el. `global-mise-mode` hooks `find-file` and friends
  itself, setting `process-environment` and `exec-path` per buffer from
  `mise env`; that covers every language, so there is nothing Ruby-specific to
  configure.
- **Language servers** — eglot and xref, both built-in.
- **Diagnostics** — flycheck.
- **Terminal and containers** — ghostel, docker.el.
- **AI** — aidermacs on the comint backend, routed through OpenRouter.
  aidermacs knows exactly two backends, comint and vterm, and there is no
  ghostel one, so removing vterm settled this. No real loss: the variable is
  `aidermacs-backend`, the misspelt `aidermacs-terminal-backend` never took
  effect, and aider had silently run on comint all along. If its output ever
  renders badly, `ghostel-comint-global-mode` gives comint buffers ghostel's VT
  rendering.
- **Theme** — doom-moonlight with doom-modeline, which shows the Ruby version.
- **Built-ins used as built-ins** (`:ensure nil :straight nil`) — eglot,
  which-key, editorconfig, treesit, markdown-ts-mode, ibuffer, delsel, server.
  which-key and editorconfig moved into core and no longer come from ELPA.

Three pins worth knowing about:

- `straight-built-in-pseudo-packages` includes `project` and `xref`. The package
  that forced this was multi-vterm, which declared `(project "0.3.0")` as a
  dependency and so made straight clone the GNU ELPA `project` and `xref` on top
  of Emacs 31's own; those then loaded *after* the built-ins were already in
  memory, leaving two versions live at once. Both are the backbone of the Ruby
  setup. multi-vterm is gone, but the pin stays — any package declaring a
  `project` dependency would reintroduce it.
- `markdown-ts-mode` ships with Emacs 31 but upstream deliberately withholds the
  autoload cookie, so the symbol is void until the file is required. With a bare
  `auto-mode-alist` entry every `.md` buffer died on "Ignoring unknown mode" and
  fell back to fundamental-mode; use-package's `:mode` emits the missing autoload.
- `csv-mode` is *not* built in, despite an old comment here claiming so. It is
  pinned to the GNU ELPA git mirror (`emacs-straight/csv-mode`) so straight never
  has to clone its recipe repositories to look this one up.

`exec-path` gets `/opt/homebrew/bin`, `~/.docker/bin` and `~/.local/bin` added
explicitly. Emacs 31 derives the default from the PATH a GUI launch inherits,
which never went through zsh.

## Other keybindings

| Key | Command |
|-----|---------|
| `C-s` | `swiper` |
| `M-x` | `counsel-M-x` |
| `C-c f` / `C-c k` | `counsel-find-file` / `counsel-ag` |
| `<f1> f` / `<f1> v` / `<f1> l` | `counsel-describe-function` / `-variable` / `counsel-find-library` |
| `<f2> i` / `<f2> u` | `counsel-info-lookup-symbol` / `counsel-unicode-char` |
| `C-x g` | `magit-status` |
| `C-x C-b` | `ibuffer` |
| `<f12>` | `treemacs` |
| `M-<up>` / `M-<down>` | `move-text-up` / `move-text-down` |
| `<C-tab>` / `<C-S-tab>` | `iflipb-next-buffer` / `iflipb-previous-buffer` |
| `M-o` | `other-window` |
| `M-/` | `comment-or-uncomment-region` |
| `<escape>` | `keyboard-escape-quit` |
| `C-c d` | `docker` |
| `C-c t` | `ghostel` |
| `C-c C-t` | `my/term-new` (a new terminal, as `multi-vterm` did) |
| `C-c m t` / `C-c m n` / `C-c m p` | `my/term-dedicated-toggle` / `my/term-next` / `my/term-previous` |
| `C-c a a s f b r R k` | the same aidermacs commands as `Hyper-a` |
| `C-c ! l n p c v` | `flycheck-list-errors` / `next-error` / `previous-error` / `buffer` / `verify-setup` |

Editor defaults: line numbers everywhere except org, term, shell, ibuffer,
treemacs and ghostel buffers; `delete-selection-mode`; trailing whitespace stripped
on save; no menu, tool or scroll bar; visible bell; UTF-8 preferred.

## Checks

```
./test/check-config.sh
EMACS=/path/to/Emacs ./test/check-config.sh
```

An offline static gate: it syntax-checks the shims in `bin/`, byte-compiles
`early-init.el`, `init.el`, `lisp/my-hyper.el` and `lisp/my-op.el` looking for
blocking warnings, loads the Hyper layer in batch to assert its bindings
resolve, and runs `test/op-assertions.el` against a stub 1Password CLI. It
never starts a server, fetches a package, writes into `straight/` or
`eln-cache/`, or touches the real `op`. It currently passes.

```
./test/check-runtime.sh
EMACS=/path/to/Emacs ./test/check-runtime.sh
```

The other half: it actually launches Emacs, because some things do not exist
until init.el has run and every package has loaded — the order of
`after-change-major-mode-hook` above all. Two files:

- `test/modeline-env-assertions.el` opens a real Ruby buffer and asserts that
  the modeline's version indicator measures the interpreter *that buffer* would
  run rather than one resolved before mise set the buffer's `exec-path`.
- `test/ruby-lsp-assertions.el` points `my-op-executable` at a stub `op` that
  sleeps for five seconds, opens a Ruby file in a project that needs the JFrog
  credential, and asserts that `find-file` returns *while the read is still in
  flight* — the reader's own `in-flight` state says so, which is a fact about
  the run and not about the clock. Then that ruby-lsp is started afterwards,
  from the callback, with the token on the buffer's `process-environment`.
  Against the old synchronous reader `find-file` took the full five seconds,
  and against the real `op` that wait was however long the user took to answer
  a Touch ID prompt — with the frame blank and `C-g` dead for the duration.

Emacs is launched through `env -i` with a bare `PATH`, the way the Dock and
Spotlight launch it, and that is the point of the whole file. An interactive
zsh has already run `mise activate`, so a test that inherits your shell's
`PATH` carries a mise interpreter in from outside and cannot see this class of
bug at all — which is exactly how the modeline came to report Ruby 2.6.10 for
months while looking correct every time it was checked from a terminal.

Unlike `check-config.sh` this one is not side-effect free: it loads the real
configuration, so straight.el may rebuild a package. It fetches nothing and
writes nothing outside this repository, and it gets a `TMPDIR` of its own so
the `server-start` in init.el cannot collide with a running session's socket.
It currently passes.

## Known gaps

- `.kt` and `.kts` have no major mode. The kotlin grammar is in
  `treesit-language-source-alist` and gets installed, but `kotlin-ts-mode` is not
  part of Emacs 31 and no package here supplies it.
- `aggressive-indent` is installed but not hooked to any mode, so nothing enables
  it.
- `.erb` is mapped to `ruby-ts-mode`; the ruby grammar has no notion of the
  surrounding template markup.
