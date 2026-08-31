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
| `bin/mise-ruby-lsp` | ruby-lsp launcher for eglot (mise Ruby + the JFrog Bundler credential) |
| `test/check-config.sh` | offline static checks |
| `.env.example` | template for `.env`, which is gitignored |

## Requirements

Before the first launch:

- **Emacs 31.1** — `brew tap d12frosted/emacs-plus && brew install --cask emacs-plus-app`.
  Do not keep an `emacs-plus@N` *formula* installed alongside the cask; both provide
  `emacs` and `emacsclient` in `$(brew --prefix)/bin` and the cask states the
  combination is unsupported.
- **`brew install cmake libtool`** — for vterm's native module. cmake compiles it;
  libtool supplies `glibtool`, which the vendored libvterm build calls.
- **`brew install mise`** — per-project runtimes.
- **Karabiner-Elements**, with the Caps-Lock rule described below. Without it the
  whole Hyper layer is unreachable.
- **1Password CLI** — the `1password-cli` cask, at `/opt/homebrew/bin/op`, with
  desktop-app integration enabled. Only needed for Ruby projects whose gems come
  from `nedap.jfrog.io`.
- **A GitHub SSH key** — `straight-vc-git-default-protocol` is `ssh`, so every
  package clone goes over SSH.
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
| `Hyper-c R` | `my/rails-console` | Rails console in a vterm |
| `Hyper-c f` | `my/ruby-format-buffer` | Format via the language server |
| `Hyper-c l` | `flycheck-list-errors` | List diagnostics |
| `Hyper-c n` | `xref-find-references` | Find references |
| `Hyper-c h` | `eldoc-doc-buffer` | Documentation for the symbol at point |
| `Hyper-c a` | `eglot-code-actions` | Code actions |
| `Hyper-c d` | `docker` | Docker interface |
| `Hyper-c t` | `vterm` | Terminal |

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

`init.el` reads the token, not the shim. `my/jfrog-token` runs `op read` **at most
once per Emacs session**, and only from a buffer whose project references
`nedap.jfrog.io` in its `Gemfile` or `Gemfile.lock`. Projects that do not use the
private source never reach 1Password at all. The value is consed onto the
buffer-local `process-environment` as `BUNDLE_NEDAP__JFROG__IO`, which the
language server inherits when eglot connects.

Caching it matters because there is no shell session, so there is no `op` session
token to reuse and every `op read` can raise its own biometric prompt — and eglot
launches the shim once per language server: one per project, one after every
server restart, one after every Emacs restart.

`M-x my/jfrog-token-refresh` forgets the cached value and reads it again — for
after the token rotates, or after unlocking 1Password following a failed read.
Buffers already carrying the old value pick up the new one when their language
server next restarts.

A failed read is not fatal. It reports a message and leaves ruby-lsp running,
which is still useful in projects with no Artifactory gems.

Only the `op://` reference is in this repository (`my/jfrog-op-reference` in
`init.el`, `OP_REF` in the shim). The value it resolves to must never reach a
file, a log, a commit or `message`.

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

`Hyper-c R` (`my/rails-console`) runs `mise x -- bundle exec rails console` as the
vterm shell, and errors out when the project has no `bin/rails`.

The project root for all of these comes from `locate-dominating-file` on
`Gemfile`, `Gemfile.lock`, then `.git` — not `vc-root-dir`, whose state is
populated from `find-file-hook` and is therefore still nil when a major-mode hook
runs.

## Terminal

vterm compiles its native module on first load. `vterm-always-compile-module` and
`vterm-module-cmake-args` are set in `:init`, not `:config`, because vterm.el's
module check is a top-level form — in `:config` they would land after the load
and do nothing.

`vterm-module-cmake-args` is `-DUSE_SYSTEM_LIBVTERM=OFF`, building against the
vendored libvterm. Linking the system dylib is the classic Apple-Silicon failure:
a stray x86_64 `/usr/local/lib/libvterm.dylib` gets picked up and the link fails
with "building for macOS-arm64 but attempting to link with file built for
macOS-x86_64".

The build needs **both** cmake and libtool. Without `glibtool` the cmake step
fails while vterm still prints "module succeeded", so the failure is easy to
miss; the tell is that no module file appears.

The module lands as `straight/build/vterm/vterm-module.so`. Emacs finds it even
though `module-file-suffix` is `.dylib`, because `load-suffixes` lists `.so`
first.

An interactive zsh runs `mise activate zsh`, so a vterm shell already has the
project's runtimes. Emacs' own subprocesses do not go through zsh, which is why
`compile` and flycheck prefix `mise x --`.

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
- **Git** — magit, transient, ibuffer-vc, treemacs-magit.
- **Project and files** — built-in `project`, treemacs with the nerd-icons theme,
  dashboard, page-break-lines.
- **Environment** — mise.el. `global-mise-mode` hooks `find-file` and friends
  itself, setting `process-environment` and `exec-path` per buffer from
  `mise env`; that covers every language, so there is nothing Ruby-specific to
  configure.
- **Language servers** — eglot and xref, both built-in.
- **Diagnostics** — flycheck.
- **Terminal and containers** — vterm, multi-vterm, docker.el.
- **AI** — aidermacs on the vterm backend, routed through OpenRouter. The
  variable is `aidermacs-backend`; `aidermacs-terminal-backend` does not exist
  and never took effect, so this had silently stayed on comint.
- **Theme** — doom-moonlight with doom-modeline, which shows the Ruby version.
- **Built-ins used as built-ins** (`:ensure nil :straight nil`) — eglot,
  which-key, editorconfig, treesit, markdown-ts-mode, ibuffer, delsel, server.
  which-key and editorconfig moved into core and no longer come from ELPA.

Three pins worth knowing about:

- `straight-built-in-pseudo-packages` includes `project` and `xref`. multi-vterm
  declares `(project "0.3.0")` as a dependency, which made straight clone the
  GNU ELPA `project` and `xref` on top of Emacs 31's own; those then loaded
  *after* the built-ins were already in memory, leaving two versions live at
  once. Both are the backbone of the Ruby setup.
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
| `C-c t` | `vterm` |
| `C-c C-t` | `multi-vterm` |
| `C-c m t` / `C-c m n` / `C-c m p` | `multi-vterm-dedicated-toggle` / `-next` / `-prev` |
| `C-c a a s f b r R k` | the same aidermacs commands as `Hyper-a` |
| `C-c ! l n p c v` | `flycheck-list-errors` / `next-error` / `previous-error` / `buffer` / `verify-setup` |

Editor defaults: line numbers everywhere except org, term, shell, ibuffer,
treemacs and vterm buffers; `delete-selection-mode`; trailing whitespace stripped
on save; no menu, tool or scroll bar; visible bell; UTF-8 preferred.

## Checks

```
./test/check-config.sh
EMACS=/path/to/Emacs ./test/check-config.sh
```

An offline static gate: it syntax-checks the shims in `bin/`, byte-compiles
`early-init.el`, `init.el` and `lisp/my-hyper.el` looking for blocking warnings,
and loads the Hyper layer in batch to assert its bindings resolve. It never
starts a server, fetches a package, or writes into `straight/` or `eln-cache/`.
It currently passes.

The real test is launching Emacs, which the harness deliberately does not do.

## Known gaps

- `.kt` and `.kts` have no major mode. The kotlin grammar is in
  `treesit-language-source-alist` and gets installed, but `kotlin-ts-mode` is not
  part of Emacs 31 and no package here supplies it.
- `aggressive-indent` is installed but not hooked to any mode, so nothing enables
  it.
- `.erb` is mapped to `ruby-ts-mode`; the ruby grammar has no notion of the
  surrounding template markup.
