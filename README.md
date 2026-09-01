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

- **Emacs 31.1** — `brew tap d12frosted/emacs-plus && brew install --cask emacs-plus-app`.
  Do not keep an `emacs-plus@N` *formula* installed alongside the cask; both
  provide `emacs` in `$(brew --prefix)/bin` and the combination is unsupported.
- **`brew install mise`** — per-project runtimes.
- **Karabiner-Elements**, with the Caps-Lock rule described below. Without it the
  whole Hyper layer is unreachable.
- **1Password CLI** — the `1password-cli` cask, at `/opt/homebrew/bin/op`, with
  desktop-app integration enabled. Needed for Ruby projects whose gems come from
  `nedap.jfrog.io`, and for Forge. See [Secrets](#secrets).
- **A GitHub SSH key** — `straight-vc-git-default-protocol` is `ssh`, so every
  package clone goes over SSH.
- **`git config --global github.user QuirkQ`** — ghub reads the GitHub username
  from Git, not from Emacs. Not a secret; the token comes from 1Password.
- **JetBrains Mono Nerd Font** — `brew install --cask font-jetbrains-mono-nerd-font`.
- **`.env`** — copy `.env.example` and fill it in. `OPENROUTER_API_KEY` is for
  aidermacs; `OP_ACCOUNT`, `OP_VAULT` and the `OP_<KEY>_ITEM` pairs tell the
  1Password reader and the shim in `bin/` where the secrets live. `early-init.el`
  loads it; the file is gitignored.

The first launch is slow and needs the network: straight clones and
native-compiles every package, and `init.el` installs all twenty tree-sitter
grammars synchronously (twenty languages, twenty-one with `markdown-inline`). Both happen once. `init.el` starts a server unless one
is already running, so `emacsclient` works.

## The Hyper key

Karabiner-Elements maps `caps_lock` to `left_shift` + `left_control` +
`left_option` + `left_command`, with `to_if_alone` → `escape`. Held, Caps Lock is
Hyper; tapped, it is ESC. With `ns-command-modifier` at its default of `super`,
Emacs sees that chord as `C-M-S-s-`.

Shift is part of the chord, and which spelling macOS delivers cannot be
predicted per key (shifted-glyph substitution, Emacs' shift translation dropping
the `S-` bit, …). Rather than guess, `my-hyper-set` binds the whole
cross-product of `{C-M-S-s-, C-M-s-}` against `{the key, its Shift-folded
spelling}`, keeping only spellings that satisfy `key-valid-p`. The redundant
bindings are harmless. If a key ever misbehaves, `C-h k` reports the spelling
Emacs actually saw.

Two consequences of folding Shift into the chord:

- **Case cannot be distinguished at the chord position.** Hyper-r and Hyper-R
  are one event. Keys *after* the chord are typed unmodified and do distinguish
  case, which is why `a r` and `a R` can coexist.
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

### Shared with Zed

`Hyper-.`, `Hyper-,` and `Hyper-RET` are deliberately the same physical keys as
in `~/.config/zed/keymap.json`, so jump-to-definition and jump-back carry
between the two editors. `Hyper-SPC` does not: here it is
`kmacro-end-or-call-macro`, where Zed uses that chord for
`editor::AcceptEditPrediction`.

`Hyper-3` collides with macOS: ⌃⌥⇧⌘3 copies a screenshot to the clipboard. Turn
it off under System Settings → Keyboard → Keyboard Shortcuts → Screenshots to
get `split-window-right`.

## Secrets

`lisp/my-op.el` is the only thing here that talks to 1Password. Where the
secrets live — the account, the vault and one item path per key — comes from
`OP_ACCOUNT`, `OP_VAULT` and an `OP_<KEY>_ITEM` per key, which
`my/load-dotenv` in `early-init.el` populates from the gitignored `.env` next
to `init.el`. `.env.example` documents the shape. The shim in `bin/` sources
the same file itself, so Emacs and the standalone launcher agree.

```elisp
(my-op-get-async 'jfrog-token #'f)  ; never blocks; f is called with the value
(my-op-get 'jfrog-token)            ; blocks; the Bundler credential key
(my-op-get 'github-token)           ; blocks; the PAT Forge authenticates with
(my-op-refresh)                     ; forget them and read again
```

All secrets are read in one `op` call per session: a GUI Emacs has no shell
session, so there is no `op` session token to reuse and every invocation can
raise its own biometric prompt. The read is asynchronous because the first
caller is a mode hook and `op` does not answer until a human answers Touch ID;
a synchronous read there froze the frame with `C-g` dead. The cache has four
states — never asked, *in flight*, the secrets, and *asked and failed* — so
there is one prompt per session and a locked 1Password cannot cause a prompt
storm. Failure is never fatal: it reports a message, yields nil, and callers
carry on without the credential. `M-x my-op-refresh` retries.

Only `op://` paths are in this repository. The values they resolve to must
never reach a file, a log, a commit or `message`.

## Forge

[Forge](https://github.com/magit/forge) is loaded `:after magit` and keeps its
cache at `forge-database.sqlite` (gitignored). It holds no token of its own:
ghub looks one up through `auth-source`, and `my/github-auth-source-search`
answers that query for `api.github.com` and `github.com` with
`(my-op-get 'github-token)`. The backend does its own host matching — a search
function that answered indiscriminately would hand the GitHub token to any
caller asking auth-source for a password. When 1Password comes up empty the
search returns nothing and auth-source falls through to netrc.

## Ruby

### ruby-lsp under mise

eglot (built-in) is hooked into `ruby-ts-mode` and `ruby-mode`, and the server
registered for both is `bin/mise-ruby-lsp`. The shim exists for two things
Emacs cannot do for itself: ruby-lsp has to run under the project's mise Ruby
(`mise exec -- ruby-lsp`), and Bundler needs the Artifactory credential for
`nedap.jfrog.io`, which lives in 1Password. Tool paths in the shim are absolute
because a GUI-launched Emacs has no shell environment. It mirrors
`~/.local/bin/zed-ruby-lsp`; Zed's `lsp.ruby-lsp.binary.path` can be repointed
at this file to keep a single shim under version control.

### The JFrog token

`init.el` supplies the token, not the shim. It is asked for only from a buffer
whose project references `nedap.jfrog.io` in its `Gemfile` or `Gemfile.lock`,
then consed onto the buffer-local `process-environment` as
`BUNDLE_NEDAP__JFROG__IO`, which the language server inherits when eglot
connects. `ruby-ts-mode-hook` runs `my/ruby-start-lsp`, which asks through
`my-op-get-async` and returns immediately; `eglot-ensure` is called from the
callback once the token is in hand. A failed read reports a message and starts
ruby-lsp anyway, which is still useful when the Artifactory gems are already
installed.

### Diagnostics

Flycheck, not flymake: `eglot-stay-out-of` is `'(flymake)`, so there is exactly
one source of squiggles. The checker is chosen per project from `Gemfile.lock`
and `Gemfile` — `ruby-standard` when the `standard` gem is there,
`ruby-rubocop` otherwise. The command runs through
`flycheck-command-wrapper-function`, which prefixes the argv with `mise x --`,
plus `bundle exec` when the project has a Gemfile. `C-c ! v`
(`flycheck-verify-setup`) shows which checker was selected. Removing `flymake`
from `eglot-stay-out-of` swaps this for ruby-lsp's own diagnostics.

### Formatting

eglot's, on save: `eglot-format-buffer` on a buffer-local `before-save-hook`.
ruby-lsp picks StandardRB or RuboCop from the project itself. The hook is taken
back off when the server goes away, so saves after a shutdown don't print "No
current JSON-RPC connection".

### Ruby commands

`Hyper-c r` (`my/ruby-run-tests`) picks the runner from what the project has:
`.rspec`, the `rspec` gem or a `spec/` directory means RSpec; `test/` plus
`bin/rails` means `rails test`; `test/` plus a `Rakefile` means `rake test`.
The command is prefixed with `mise x --`, plus `bundle exec` when there is a
Gemfile.

`Hyper-c R` (`my/rails-console`) runs `mise x -- bundle exec rails console`
through `ghostel-exec`, and errors out when the project has no `bin/rails`.

The project root for all of these comes from `locate-dominating-file` on
`Gemfile`, `Gemfile.lock`, then `.git` — not `vc-root-dir`, which is still nil
when a major-mode hook runs.

## Terminal

The terminal is [ghostel](https://github.com/dakra/ghostel), which runs
libghostty-vt — the VT engine from Ghostty — as a native module. It replaced
vterm, which could not draw a full-screen TUI cleanly: libvterm has no
synchronized output, so every frame-coalescing setting traded torn frames
against dropped ones. libghostty-vt implements synchronized output (DEC mode
2026), so `ghostel-timer-delay` is left at upstream's default.

The native module is a prebuilt binary fetched from GitHub releases on the
first `M-x ghostel` (`ghostel-module-auto-install` is `download`); nothing is
compiled. `ghostel-module-compile` builds from source instead and needs a Zig
toolchain. The straight recipe carries an explicit `:files` list mirroring
MELPA's: `lisp/` for the Lisp, `etc/` for the bundled `xterm-ghostty` terminfo,
`src/` + `vendor/` + `build.zig*` for a local module build.

`ghostel-shell` is `/bin/zsh`, wrapped through `login(1)` on macOS
(`ghostel-macos-login-shell`, on by default), so `~/.zprofile` is sourced the
way Terminal.app does it — a behaviour change from vterm. An interactive zsh
runs `mise activate zsh`, so a ghostel shell already has the project's
runtimes; Emacs' own subprocesses do not go through zsh, which is why `compile`
and flycheck prefix `mise x --`.

Notable display settings in ghostel buffers: `nobreak-char-display` nil (Claude
Code pads its UI with U+00A0, which otherwise renders as cyan underlined
cells), `bidi-paragraph-direction` pinned with `bidi-inhibit-bpa` (skips the
bidi algorithm on 290-column lines), `gc-cons-threshold` at 32MB (a terminal
conses a propertized string per line per redraw), and company + emojify
switched off via `company-global-modes` / `emojify-inhibit-major-modes` (mode
hooks get silently undone by those globalized modes; and ghostel forwards
foreign buffer insertions to the pty, so a completion is a completion typed at
the shell).

### Muscle-memory aliases

vterm and multi-vterm are gone, but the old `M-x` names still work. Everything
routes through a `my/term*` layer, so swapping the backend again means editing
those aliases and nothing else.

| Old name | Now runs | Note |
| --- | --- | --- |
| `vterm` | `my/term` → `ghostel` | |
| `vterm-other-window` | `my/term-new` | |
| `multi-vterm` | `my/term-new` | `multi-vterm` always made a **new** numbered terminal; plain `ghostel` reuses the existing one and only creates on a prefix arg |
| `multi-vterm-next` / `-prev` | `ghostel-next` / `ghostel-previous` | |
| `multi-vterm-dedicated-toggle` | `my/term-dedicated-toggle` | ghostel has no dedicated-window concept, so this is a side window at the bottom, 30 lines |

`my/term-project` (`ghostel-project`) and `my/term-list`
(`ghostel-list-buffers`) have no vterm predecessor. Inside a ghostel buffer,
`C-c C-t` is copy mode from `ghostel-mode-map`, which outranks the global
`C-c C-t`.

### Which keys Emacs still gets

`ghostel-keymap-exceptions` is the list of keys semi-char mode — the default
input mode — leaves alone; everything else goes to the pty. Upstream's default
is `C-c C-x C-u C-h M-x M-: C-\`, and this config appends **`M-o`**, so
`other-window` works from inside a terminal. It is set through the custom
machinery because the defcustom's `:set` is what rebuilds the keymap. The cost
is that the excepted key no longer reaches the application; char mode
(`C-c M-d`) is the escape hatch.

## Fonts, icons and emoji

The default face, `fixed-pitch` and `nerd-icons-font-family` all resolve
through a candidate list — `JetBrainsMono Nerd Font Mono`, then
`JetBrainsMono NFM`, then `Menlo` — taking the first that `find-font` resolves.
Nerd Fonts set the family name and typographic family to different strings and
Core Text is inconsistent about which one it reports; a wrong literal falls
back to Menlo silently and every nerd-icons glyph renders as a hex box.

Emoji are drawn natively: a fontset rule prepends `Apple Color Emoji` for the
`emoji` script. `emojify` runs in `unicode` style purely to rewrite shortcodes
(`:wink:` → the codepoint).

## Packages

- **Completion and search** — ivy, counsel, swiper; company with company-box.
- **Git** — magit, forge, transient, ibuffer-vc, treemacs-magit.
- **Project and files** — built-in `project`, treemacs with the nerd-icons
  theme, dashboard, page-break-lines.
- **Environment** — mise.el. `global-mise-mode` sets `process-environment` and
  `exec-path` per buffer from `mise env`; that covers every language.
- **Language servers** — eglot and xref, both built-in.
- **Diagnostics** — flycheck.
- **Terminal and containers** — ghostel, docker.el.
- **AI** — aidermacs on the comint backend, routed through OpenRouter.
  aidermacs knows exactly two backends, comint and vterm, and there is no
  ghostel one. If its output ever renders badly,
  `ghostel-comint-global-mode` gives comint buffers ghostel's VT rendering.
- **Theme** — doom-moonlight with doom-modeline, which shows the Ruby version.
- **Built-ins used as built-ins** (`:ensure nil :straight nil`) — eglot,
  which-key, editorconfig, treesit, markdown-ts-mode, ibuffer, delsel, server.

Three pins worth knowing about:

- `straight-built-in-pseudo-packages` includes `project` and `xref`, so no
  package can make straight clone the ELPA versions on top of Emacs 31's own.
- `markdown-ts-mode` ships with Emacs 31 but upstream withholds the autoload
  cookie; use-package's `:mode` emits the missing autoload.
- `csv-mode` is *not* built in. It is pinned to the GNU ELPA git mirror
  (`emacs-straight/csv-mode`) so straight never has to clone its recipe
  repositories to look it up.

`exec-path` gets `/opt/homebrew/bin`, `~/.docker/bin` and `~/.local/bin` added
explicitly, because a GUI launch's PATH never went through zsh.

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
treemacs and ghostel buffers; `delete-selection-mode`; trailing whitespace
stripped on save; no menu, tool or scroll bar; visible bell; UTF-8 preferred.

## Checks

```
./test/check-config.sh
EMACS=/path/to/Emacs ./test/check-config.sh
```

An offline static gate: syntax-checks the shims in `bin/`, byte-compiles
`early-init.el`, `init.el`, `lisp/my-hyper.el` and `lisp/my-op.el` looking for
blocking warnings, loads the Hyper layer in batch to assert its bindings
resolve, and runs `test/op-assertions.el` against a stub 1Password CLI. It
never starts a server, fetches a package, or touches the real `op`.

```
./test/check-runtime.sh
EMACS=/path/to/Emacs ./test/check-runtime.sh
```

Launches the real Emacs, because some things do not exist until init.el has run
and every package has loaded. Emacs is launched through `env -i` with a bare
`PATH`, the way the Dock and Spotlight launch it — an interactive zsh has
already run `mise activate`, so a test that inherits your shell's `PATH` cannot
see this class of bug at all. It loads the real configuration, so straight.el
may rebuild a package; it fetches nothing and gets a `TMPDIR` of its own so
`server-start` cannot collide with a running session's socket.

## Known gaps

- `.kt` and `.kts` have no major mode. The kotlin grammar is in
  `treesit-language-source-alist` and gets installed, but `kotlin-ts-mode` is
  not part of Emacs 31 and no package here supplies it.
- `aggressive-indent` is installed but not hooked to any mode, so nothing
  enables it.
- `.erb` is mapped to `ruby-ts-mode`; the ruby grammar has no notion of the
  surrounding template markup.
