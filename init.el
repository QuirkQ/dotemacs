;;; init.el --- Quint's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:

; Quint his fantastic init.el

;;; Code:

(defconst my-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

;; Configuration split out of this file lives in lisp/.
(add-to-list 'load-path (expand-file-name "lisp" my-emacs-dir))

;; The 1Password reader -- see lisp/my-op.el. Required up here rather than
;; beside `my-hyper' at the bottom because code further down this file calls
;; `my-op-get'. Loading it reads nothing; the first `op' call happens when a
;; secret is actually asked for.
(require 'my-op)

;; Nerd Fonts ship two different family strings and Core Text is inconsistent
;; about which one it reports: the JetBrainsMono Mono patch has family name
;; (name ID 1) "JetBrainsMono NFM" but typographic family (name ID 16)
;; "JetBrainsMono Nerd Font Mono". Hardcoding either one risks a silent
;; fallback to Menlo, so try both and let the first that resolves win.
(defconst my-mono-font-candidates
  '("JetBrainsMono Nerd Font Mono" "JetBrainsMono NFM" "Menlo")
  "Monospace families to try, most preferred first.")

(defun my/first-available-font (candidates)
  "Return the first family in CANDIDATES that is installed.
`find-font' needs a display, so this returns nil under -batch and in a
daemon with no frame yet; callers must cope with that."
  (and (display-graphic-p)
       (seq-find (lambda (family)
                   (find-font (font-spec :family family)))
                 candidates)))

(defun my/mono-font ()
  "The monospace family to use, falling back to the preferred name."
  (or (my/first-available-font my-mono-font-candidates)
      (car my-mono-font-candidates)))

;; Treat these as built-in so straight.el never installs the GNU ELPA copies.
;; The package that forced this was multi-vterm, which declared
;; (project "0.3.0") and so made straight clone ELPA `project' and `xref' on
;; top of Emacs 31's own. They then loaded *after* the built-ins were already
;; in memory, producing
;;   Feature `project' loaded from ".../Resources/lisp/progmodes/project.elc"
;;   is now provided by ".../straight/build/project/project.elc"
;; and leaving two versions live at once -- which matters here because eglot
;; and xref are the backbone of the Ruby setup.
;;
;; multi-vterm is gone now (ghostel replaced it and has its own project
;; support), but the pin stays: any package declaring a `project' dependency
;; would reintroduce exactly this. Must precede the bootstrap.
(setq straight-built-in-pseudo-packages
      '(emacs nadvice python image-mode project xref))

;; Initialise straight.el : https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el to use use-package
(straight-use-package 'use-package)

;; Make use-package install packages with straight.el by default
(setq straight-use-package-by-default t)

;; Set the default Git protocol for straight.el
(setq straight-vc-git-default-protocol 'ssh)

;; Control verbosity of use-package; set to 't' for detailed startup info
(setq use-package-verbose nil)

;; Emacs 31 derives the default `exec-path' from the same PATH other
;; programs get. A GUI launch from the Dock or Spotlight never runs zsh, so
;; anything ~/.config/zsh adds is missing; add it back explicitly.
;; mise's own per-project paths are handled by mise.el, below.
(dolist (dir '("/opt/homebrew/bin"
               "~/.docker/bin"
               "~/.local/bin"))
  (let ((path (expand-file-name dir)))
    (when (file-directory-p path)
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat (getenv "PATH") ":" path)))))

;; mise - https://github.com/liuyinz/mise.el
;; `global-mise-mode' hooks find-file and friends itself, setting
;; `process-environment' and `exec-path' per buffer from `mise env'. That
;; covers every language, so there is nothing Ruby-specific to do here.
(use-package mise
  :straight (mise :type git :host github :repo "liuyinz/mise.el")
  :hook (after-init . global-mise-mode)
  :config
  ;; Keep eshell's environment in step when its directory changes.
  (setq mise-update-on-eshell-directory-change t))

;; Start the emacs server when it isn't running
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; nerd-icons : https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons
  :straight (nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el")
  :config
  ;; Upstream defaults to "Symbols Nerd Font Mono", which is not installed
  ;; here. Every icon then renders as a hex box (U+F06EB and friends) in
  ;; treemacs, the modeline and the dashboard. The JetBrainsMono Nerd Font
  ;; patch carries the same Private Use Area glyphs -- verified: all of
  ;; F06EB F178A F19CA F0225 F0626 F015A F002A F02FD are in its cmap.
  (setq nerd-icons-font-family (my/mono-font)))

;; shrink-path : https://github.com/zbelial/shrink-path.el
(use-package shrink-path
  :straight (shrink-path :type git :host github :repo "zbelial/shrink-path.el")
  :ensure t
  :demand t)

;; doom-theme : https://github.com/doomemacs/themes
(use-package doom-themes
  :ensure t
  :straight (doom-themes :type git :host github :repo "doomemacs/themes")
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

;; doom-modeline : https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :ensure t
  :straight (doom-modeline :type git :host github :repo "seagle0128/doom-modeline")
  :hook ((after-init . doom-modeline-mode)
         (ruby-ts-mode . (lambda ()
                           (run-with-idle-timer 0.1 nil #'force-mode-line-update)))
         (ruby-mode . (lambda ()
                        (run-with-idle-timer 0.1 nil #'force-mode-line-update))))
  :config
  ;; Enable environment version display
  (setq doom-modeline-env-version t)
  (setq doom-modeline-env-enable-ruby t)

  ;; Which interpreter the version indicator actually measures.
  ;;
  ;; `doom-modeline-env-setup-ruby' runs from `ruby-ts-mode-hook' and defers
  ;; the real work to the buffer-local `hack-local-variables-hook', while
  ;; `global-mise-mode' turns `mise-mode' on from
  ;; `after-change-major-mode-hook'. `run-mode-hooks' (subr.el) runs those
  ;; three in this order and no other:
  ;;
  ;;   ruby-ts-mode-hook -> (hack-local-variables 'no-mode)
  ;;                     -> after-change-major-mode-hook
  ;;
  ;; and Emacs 31's `normal-mode' reaches `hack-local-variables' only through
  ;; `run-mode-hooks', so there is no second, later pass to correct it. So
  ;; `doom-modeline-env-update-ruby' resolved "ruby" over `exec-path' exactly
  ;; one step before mise replaced it, cached the absolute path it found in
  ;; `doom-modeline-env--command', and never resolved again -- the modeline
  ;; reported the *launch environment's* ruby for the life of the buffer. From
  ;; the Dock or Spotlight that is /usr/bin/ruby, i.e. "Ruby 2.6.10", on a
  ;; machine where every project pins something else. Launched from a terminal
  ;; it looked correct by accident only: an interactive zsh has run
  ;; `mise activate', so the inherited PATH carries mise's *global* ruby --
  ;; which is still not what a project pins (4.0.6 against epoxy's 4.0.5).
  ;;
  ;; Nothing about that is Ruby-specific. `doom-modeline-env-version' turns on
  ;; all six of upstream's indicators, and every one of them is wired the same
  ;; way, so every one of them measures the launch environment.
  ;;
  ;; So take the setup off the mode hooks and run it from
  ;; `after-change-major-mode-hook' instead, APPENDED: `add-hook' prepends and
  ;; mise registers on that same hook, so appending is what puts this after
  ;; `global-mise-mode-enable-in-buffer'. Moved rather than duplicated on
  ;; purpose -- `doom-modeline-env--get' is asynchronous, so leaving the early
  ;; call in place would race two `ruby --version' processes whose filters
  ;; write the same variable, and the wrong one could land last.
  ;;
  ;; File-local variables are still honoured. That is what upstream's
  ;; indirection through `hack-local-variables-hook' buys, and by this point
  ;; in `run-mode-hooks' `hack-local-variables' has already run.
  ;;
  ;; test/modeline-env-assertions.el is the executable spec, and asserts the
  ;; hook order rather than trusting it.
  (defconst my/doom-modeline-env-languages
    '((python python-mode python-ts-mode)
      (ruby   ruby-mode ruby-ts-mode enh-ruby-mode)
      (perl   perl-mode)
      (go     go-mode go-ts-mode)
      (elixir elixir-mode elixir-ts-mode)
      (rust   rust-mode rust-ts-mode))
    "Alist of (LANGUAGE MODE...), one entry per `doom-modeline-def-env'.
Mirrors the `:hooks' of each definition in doom-modeline-env.el. Data
rather than two hand-written lists so the hooks taken away below and the
dispatch in `my/doom-modeline-setup-env' cannot drift apart; a language
upstream adds is one line here.")

  (defun my/doom-modeline-setup-env ()
    "Resolve this buffer's interpreter version for the modeline.
Runs from `after-change-major-mode-hook', which is late enough that mise
has already given the buffer its own `exec-path'."
    (when-let* ((entry (seq-find (lambda (entry) (derived-mode-p (cdr entry)))
                                 my/doom-modeline-env-languages))
                (update (intern-soft (format "doom-modeline-env-update-%s"
                                             (car entry)))))
      (when (fboundp update)
        (funcall update))))

  (dolist (entry my/doom-modeline-env-languages)
    (when-let* ((setup (intern-soft (format "doom-modeline-env-setup-%s"
                                            (car entry)))))
      (dolist (mode (cdr entry))
        (remove-hook (intern (format "%s-hook" mode)) setup))))

  (add-hook 'after-change-major-mode-hook #'my/doom-modeline-setup-env t))

;; treemacs : https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :ensure t
  :defer t
  :straight (treemacs :type git :host github :repo "Alexander-Miller/treemacs")
  :config
  (progn
    (setq treemacs-width 40)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("<f12>"   . treemacs)))

;; treemacs-nerd-icons : https://github.com/rainstormstudio/treemacs-nerd-icons
(use-package treemacs-nerd-icons
  :straight (treemacs-nerd-icons :type git :host github :repo "rainstormstudio/treemacs-nerd-icons")
  :config
  (treemacs-load-theme "nerd-icons"))

;; treemacs-magit : https://github.com/Alexander-Miller/treemacs
(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

;; aggressive-indent-mode : https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :ensure t
  :straight (aggressive-indent :type git :host github :repo "Malabarba/aggressive-indent-mode"))

;; ibuffer : [build-in]
(use-package ibuffer
  :straight nil
  :bind ("C-x C-b" . ibuffer))

;; ibuffer-vc : https://github.com/purcell/ibuffer-vc
(use-package ibuffer-vc
  :ensure t
  :after ibuffer
  :straight (ibuffer-vc :type git :host github :repo "purcell/ibuffer-vc")
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))

;; page-break-lines : https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :ensure t
  :straight (page-break-lines :type git :host github :repo "purcell/page-break-lines"))

;; dashboard : https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :straight (dashboard :type git :host github :repo "emacs-dashboard/emacs-dashboard")
  :config
  (dashboard-setup-startup-hook))

;; delsel : [built-int]
(use-package delsel
  :ensure nil ; It's a built-in package, so no need to ensure its installation.
  :config
  (delete-selection-mode 1))

;; move-text : https://github.com/emacsfodder/move-text
(use-package move-text
  :straight (move-text :type git :host github :repo "emacsfodder/move-text")
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down)))

;; avy : https://github.com/abo-abo/avy  -- bound to Hyper-j
(use-package avy
  :straight (avy :type git :host github :repo "abo-abo/avy"))

;; csv-mode : GNU ELPA -- not built in, despite the old comment claiming so.
;; Pinned to the GNU ELPA git mirror so straight never has to clone its
;; recipe repositories just to look this one up.
(use-package csv-mode
  :straight (csv-mode :type git :host github :repo "emacs-straight/csv-mode")
  :mode "\\.csv\\'")

;; ivy : https://github.com/abo-abo/swiper
(use-package ivy
  :ensure t
  :diminish
  :straight (ivy :type git :host github :repo "abo-abo/swiper")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; swiper : https://github.com/abo-abo/swiper
(use-package swiper
  :ensure t
  :straight (swiper :type git :host github :repo "abo-abo/swiper")
  :bind (("C-s" . swiper)))

;; counsel : https://github.com/abo-abo/swiper
(use-package counsel
  :ensure t
  :straight (counsel :type git :host github :repo "abo-abo/swiper")
  :bind (("M-x" . counsel-M-x)
         ("C-c f" . counsel-find-file)
         ("C-c k" . counsel-ag)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)))

;; magit : https://github.com/magit/transient
(use-package transient
  :straight (transient :type git :host github :repo "magit/transient")
  :ensure t)

;; magit : https://github.com/magit/magit
(use-package magit
  :ensure t
  :straight (magit :type git :host github :repo "magit/magit")
  :bind ("C-x g" . magit-status))

;; forge : https://github.com/magit/forge
;;
;; Forge carries no token of its own. It goes through ghub, which looks one
;; up with `auth-source-search' against the API host and an ident of
;; "<git config github.user>^forge" -- so handing Forge the 1Password token
;; means answering that query.
;;
;; Hence a custom `auth-source' backend and not an ~/.authinfo entry: the
;; token stays in 1Password and in memory and never lands in a file. Emacs
;; supports this outright. `auth-source-backend-parser-functions' turns an
;; entry in `auth-sources' into a backend, and that backend's
;; `:search-function' answers the query.
;;
;; The backend has to do its own matching. `auth-source-search' does not
;; re-check a backend's results against the spec it was given, so a search
;; function that answered indiscriminately would hand the GitHub token to
;; the next caller that asked auth-source for an SMTP password.

(declare-function auth-source-backend "auth-source")

(defconst my/github-auth-hosts '("api.github.com" "github.com")
  "Hosts `my/github-auth-source-search' answers for.
ghub asks for the API host; github.com is here for anything that keys on
the web host instead.")

(defun my/github-auth-host-p (host)
  "Non-nil when HOST, as `auth-source' spells it, names GitHub.
HOST arrives as a string, a list of strings, or t for \"any host\" -- and
t must not match, or a wildcard search would be handed the token."
  (seq-some (lambda (h) (member h my/github-auth-hosts))
            (if (listp host) host (list host))))

(defun my/github-auth-source-search (&rest spec)
  "Answer an `auth-source' query in SPEC for GitHub, from 1Password.

Nil for every other host, and nil when 1Password cannot produce the
token -- a locked vault or a missing CLI then falls through to the rest
of `auth-sources' rather than returning a credential that is empty.
`my-op-get' has already said why on its way out."
  (let ((host (plist-get spec :host)))
    (when (my/github-auth-host-p host)
      (when-let* ((token (my-op-get 'github-token)))
        (list (list :host (if (listp host) (car host) host)
                    :user (plist-get spec :user)
                    ;; A function, as every other backend returns: callers
                    ;; funcall `:secret' rather than reading it.
                    :secret (lambda () token)))))))

(defun my/github-auth-source-backend (entry)
  "Return the 1Password backend when ENTRY in `auth-sources' names it."
  (when (eq entry 'my-op-github)
    (auth-source-backend
     :source "1Password"
     :type 'my-op
     :search-function #'my/github-auth-source-search)))

(use-package forge
  :ensure t
  :straight (forge :type git :host github :repo "magit/forge")
  :after magit
  :init
  ;; Registered when magit loads, which is well ahead of anything ghub does.
  ;; At the front of `auth-sources' so 1Password wins over a stale ~/.authinfo
  ;; line, while still leaving those backends reachable behind it.
  (with-eval-after-load 'auth-source
    (add-hook 'auth-source-backend-parser-functions
              #'my/github-auth-source-backend)
    (add-to-list 'auth-sources 'my-op-github)))

;; emacs-emojify : https://github.com/iqbalansari/emacs-emojify
;; Only for shortcode/ascii substitution (":wink:" -> the codepoint). The
;; rendering itself is native: see the `emoji' fontset rule below.
(use-package emojify
  :ensure t
  :straight (emojify :type git :host github :repo "iqbalansari/emacs-emojify")
  :hook (after-init . global-emojify-mode)
  :config
  ;; Upstream default is `image', which needs an asset set downloaded into
  ;; `emojify-emojis-dir' (~/.emacs.d/emojis). That directory does not exist,
  ;; so nothing rendered. `unicode' substitutes the real codepoint and lets
  ;; Emacs draw it with Apple Color Emoji -- no downloads, no network.
  (setq emojify-display-style 'unicode)

  ;; Never in a terminal. emojify hooks `after-change-functions' and rescans
  ;; the changed region, and a terminal rewrites its whole visible buffer on
  ;; every frame -- so this ran over ~22,000 cells per repaint in a 290x77
  ;; window. It also buys nothing here: with `unicode' the only job left is
  ;; rewriting shortcodes like ":wink:", and rewriting text a shell printed
  ;; is wrong.
  (add-to-list 'emojify-inhibit-major-modes 'ghostel-mode))

;; which-key : [built-in since Emacs 30 -- justbur/emacs-which-key moved into core]
(use-package which-key
  :ensure nil
  :straight nil
  :config
  (which-key-mode))

;; company-mode : https://github.com/company-mode/company-mode
(use-package company
  :demand t
  :ensure t
  :straight (company :type git :host github :repo "company-mode/company-mode")
  :hook ((prog-mode . company-mode))
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)

  ;; `global-company-mode' turns itself on from `after-change-major-mode-hook',
  ;; which `run-mode-hooks' runs *after* `ghostel-mode-hook' -- so disabling it
  ;; buffer-locally in a mode hook gets undone. This list is the only opt-out
  ;; that sticks. It matters because `company-idle-delay' is 0.0 above: in a
  ;; terminal that armed a completion pass on every keystroke of a shell line,
  ;; and company-box builds a child frame to show the result. Worse in ghostel
  ;; than it was in vterm: ghostel forwards foreign insertions to the pty from
  ;; `after-change-functions', so a completion that edits the buffer is a
  ;; completion typed at the shell.
  (setq company-global-modes '(not ghostel-mode))
  :init
  (global-company-mode 1))

;; company-box : https://github.com/sebastiencs/company-box
(use-package company-box
  :straight (company-box :type git :host github :repo "sebastiencs/company-box")
  :hook (company-mode . company-box-mode))

;; eglot : [built-in] -- ruby-lsp is the only Ruby language server, matching
;; the Zed config's  "language_servers": ["ruby-lsp", "!solargraph", ...]
(use-package eglot
  :ensure nil
  :straight nil
  ;; `eglot-code-actions' is NOT autoloaded by Emacs 31 core (only `eglot'
  ;; and `eglot-ensure' are), and lisp/my-hyper.el binds it to Hyper-c a.
  ;; Without this the key is void until something else loads eglot.
  :commands (eglot-code-actions)
  ;; Ruby does NOT call `eglot-ensure' from here. The server may need a
  ;; credential out of 1Password first, and waiting for that on a mode hook
  ;; is what used to freeze Emacs -- `my/ruby-start-lsp', further down, is
  ;; the Ruby entry point and calls `eglot-ensure' itself.
  :hook (eglot-managed-mode . my/eglot-format-on-save)
  :config
  ;; Eglot ships a solargraph entry for Ruby; add-to-list puts ours first.
  ;;
  ;; A *function* contact, not a fixed command line. Eglot calls it from
  ;; `eglot--guess-contact' with the project it is about to connect for
  ;; (eglot.el:1583), in the buffer that triggered the connection and after
  ;; `hack-local-variables' has run -- which is what lets one entry pick a
  ;; different ruby-lsp working directory per project. See
  ;; `my/ruby-lsp-contact' further down.
  (add-to-list 'eglot-server-programs
               '((ruby-ts-mode ruby-mode) . my/ruby-lsp-contact))

  ;; Flycheck owns diagnostics in this configuration (see the flycheck
  ;; block), and ruby-lsp's own linting would run standardrb a second time
  ;; over the same buffer. Keep Eglot's completion, eldoc and xref; drop
  ;; only its Flymake backend. Remove `flymake' from this list to get
  ;; ruby-lsp diagnostics back instead.
  (setq eglot-stay-out-of '(flymake))

  (defun my/eglot-format-on-save ()
    "Format Ruby buffers with the language server on save.
Mirrors \"format_on_save\": \"on\" for Ruby in ~/.config/zed/settings.json.
ruby-lsp picks StandardRB or RuboCop from the project's Gemfile itself.

`eglot-managed-mode-hook' fires in BOTH directions -- `define-minor-mode'
puts a `run-hooks' in the off branch too (eglot.el:2503) -- so this must
check `eglot--managed-mode' and take the hook back off when the server
goes away. Without that, every save after a shutdown or crash called
`eglot-format-buffer' with no connection and printed
\"No current JSON-RPC connection\"; `before-save-hook' runs inside
`with-demoted-errors' (files.el), so the save still happened, but the
error was there on every single save."
    (when (derived-mode-p 'ruby-ts-mode 'ruby-mode)
      (if (bound-and-true-p eglot--managed-mode)
          (add-hook 'before-save-hook #'eglot-format-buffer nil t)
        (remove-hook 'before-save-hook #'eglot-format-buffer t)))))

;; Ruby project introspection, shared by the flycheck setup below and by the
;; Ruby commands at the bottom of this file. Top level on purpose: the
;; commands must not depend on flycheck having been loaded first.

(defun my/ruby-project-root ()
  "Directory of the Ruby project owning the current buffer.

NOT `vc-root-dir'. That goes through `vc-deduce-backend', whose last
resort is `(vc-mode (vc-backend buffer-file-name))', and `vc-mode' is
populated by `vc-refresh-state' from `find-file-hook' -- which runs
*after* every major-mode hook. Called from `ruby-ts-mode-hook' it
therefore returns nil, and the old `(or (vc-root-dir) default-directory)'
silently degraded to the buffer's own directory: for
<proj>/app/models/user.rb the \"root\" was <proj>/app/models/, so no
Gemfile was ever found and `bundle exec' was dropped from the argv.
`locate-dominating-file' needs no such state."
  (let ((start (or (and buffer-file-name
                        (file-name-directory buffer-file-name))
                   default-directory)))
    (expand-file-name
     (or (locate-dominating-file start "Gemfile")
         (locate-dominating-file start "Gemfile.lock")
         (locate-dominating-file start ".git")
         default-directory))))

(defun my/ruby--file-matches-p (file regexp)
  "Non-nil when FILE is a readable regular file matching REGEXP.

Never signals. `file-readable-p' alone is also true of a *directory*
named Gemfile.lock, and `insert-file-contents' then raises `file-error';
these predicates run from `ruby-ts-mode-hook', where a signal aborts the
remainder of the hook -- `eglot-ensure' included."
  (and (file-regular-p file)
       (file-readable-p file)
       (ignore-errors
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (and (re-search-forward regexp nil t) t)))))

(defun my/ruby-gem-in-project-p (root gem)
  "Non-nil when GEM belongs to the Ruby project at ROOT.
Decided by reading Gemfile.lock and Gemfile.  The obvious alternative,
shelling out to `bundle list GEM', is a synchronous subprocess on a mode
hook -- it blocks the first keystroke in every Ruby buffer and pops up
*Shell Command Output*."
  (let ((case-fold-search nil)
        (name (regexp-quote gem)))
    (or (my/ruby--file-matches-p
         (expand-file-name "Gemfile.lock" root)
         ;; Gemfile.lock spells a gem two ways: "    standard (1.54.0)" under
         ;; GEM/specs and a bare "  standard" under DEPENDENCIES. The space
         ;; has to sit INSIDE the group -- with it outside, as in the old
         ;; "^ +standard \\((\\|$\\)", the `$' branch demanded a trailing
         ;; space before end-of-line and could never match anything.
         (concat "^ +" name "\\( (\\|$\\)"))
        (my/ruby--file-matches-p
         (expand-file-name "Gemfile" root)
         (concat "^ *gem +['\"]" name "['\"]")))))

;; The JFrog Artifactory credential.
;;
;; Bundler needs it to resolve gems from nedap.jfrog.io. lisp/my-op.el reads
;; it out of 1Password, at most once per Emacs session and alongside every
;; other secret, so the whole configuration costs one authorization prompt.
;; What stays here is the Ruby half: which projects need the credential, and
;; how it reaches the language server.
;;
;; It goes in through the server's environment rather than through the shim.
;; bin/mise-ruby-lsp runs once per language server -- one per project, plus
;; one after every server restart -- and a GUI Emacs has no shell session for
;; `op' to reuse, so reading it in the shim meant every one of those launches
;; could raise its own biometric prompt: "I need to re-auth with 1Password on
;; opening every ruby file".

(defconst my/jfrog-bundle-variable "BUNDLE_NEDAP__JFROG__IO"
  "Environment variable Bundler reads for nedap.jfrog.io credentials.")

(defconst my/jfrog-source-regexp "nedap\\.jfrog\\.io"
  "Marker of a project that resolves gems from the private source.")

(defun my/ruby-project-uses-jfrog-p (root)
  "Non-nil when the Ruby project at ROOT pulls gems from nedap.jfrog.io.
Most projects do not, and those must never reach 1Password at all.
Reads Gemfile and Gemfile.lock through `my/ruby--file-matches-p', which
tolerates a missing, unreadable or non-regular file for the same reason
as `my/ruby-gem-in-project-p'."
  (or (my/ruby--file-matches-p (expand-file-name "Gemfile" root)
                               my/jfrog-source-regexp)
      (my/ruby--file-matches-p (expand-file-name "Gemfile.lock" root)
                               my/jfrog-source-regexp)))

(defun my/ruby-needs-jfrog-token-p ()
  "Non-nil when this buffer's subprocesses still need the JFrog credential.

Nil when the variable is already set -- by the launch environment, or by
an earlier pass through the same buffer. That matches the shim's own
short-circuit, and keeps a re-run from consing a duplicate."
  (let ((present (getenv my/jfrog-bundle-variable)))
    (and (not (and present (not (string-empty-p present))))
         (my/ruby-project-uses-jfrog-p (my/ruby-project-root)))))

(defun my/ruby-set-jfrog-token (token)
  "Give this buffer's subprocesses TOKEN as the JFrog Bundler credential.

Consed onto `process-environment', never assigned over it. mise.el sets
its own buffer-local value from `after-change-major-mode-hook', and
`mise--merged-env' builds that value by appending mise's variables in
front of whatever it captured once as `mise--init-env'. Assigning here
would drop mise's Ruby off the PATH; consing leaves it alone whichever
of the two ran first.

Arriving after mise is therefore fine, which is what lets the token be
fetched asynchronously: consed on, it sits in front of mise's merged
value, and `mise--update' only ever runs again for a file buffer if the
user calls `mise-update-buffer' or `mise-update-dir' by hand."
  (when (and token (not (string-empty-p token)))
    (setq-local process-environment
                (cons (concat my/jfrog-bundle-variable "=" token)
                      process-environment))))

(defun my/ruby-start-lsp ()
  "Start ruby-lsp for this buffer, with the JFrog credential if it needs one.

Never waits. The credential comes out of 1Password, and `op' does not
answer until a human has answered a Touch ID prompt. Read synchronously
from this hook -- which is what the first version did -- that wait held
Emacs's only Lisp thread for its whole duration: no redisplay, so the
frame went blank the moment the 1Password sheet uncovered it, and no
keyboard input either, so `C-g' could not get in. Opening a .rb file
could then only be escaped by force-quitting Emacs.

So it is the server start that waits, not the editor. Where no
credential is needed -- most projects, and every buffer once the one
read of the session has happened -- this is the same immediate
`eglot-ensure' as before.

`eglot-ensure' never connects on the spot in either path; it arms a
buffer-local `post-command-hook' and connects from there. Reached
through the callback, that means ruby-lsp starts on the next command in
this buffer -- and what the server inherits is the buffer-local
`process-environment' as it stands by then, token included."
  (when (derived-mode-p 'ruby-ts-mode 'ruby-mode)
    (if (not (my/ruby-needs-jfrog-token-p))
        (eglot-ensure)
      (let ((buffer (current-buffer)))
        (my-op-get-async
         'jfrog-token
         (lambda (token)
           ;; A `my-op-get-async' callback promises nothing about the buffer
           ;; it runs in, and this one may well have been killed while the
           ;; prompt was still up.
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (my/ruby-set-jfrog-token token)
               ;; Started even when no token arrived: ruby-lsp is still
               ;; worth having in a project whose private gems are already
               ;; installed, and the shim explains itself on its stderr.
               (eglot-ensure)))))))))

;; Plain `add-hook', not eglot's `:hook': this must be in place whether or
;; not eglot has loaded.
(add-hook 'ruby-ts-mode-hook #'my/ruby-start-lsp)
(add-hook 'ruby-mode-hook #'my/ruby-start-lsp)

;; Which bundle ruby-lsp indexes.
;;
;; In a monorepo the repository root is not necessarily the bundle you want
;; the language server to reason about. epoxy is the case in point: its root
;; Gemfile is `gemspec' plus an `eval_gemfile' of every gems/*/Gemfile and
;; carries no activerecord, while integration/ is a full Rails 8 app that
;; depends on every epoxy gem by `path:'. Rooted at the repository, ruby-lsp
;; resolves `ActiveRecord::Base' in integration/app/models/note.rb to a test
;; stub in gems/fragmentary/fragments/cupido-push/spec/support/prerequisites.rb
;; -- the only `ActiveRecord' the index has -- and never loads ruby-lsp-rails.
;;
;; The lever is the server process's *working directory*, not BUNDLE_GEMFILE.
;; `RubyLsp::SetupBundler' does read `Bundler.default_gemfile', which honours
;; BUNDLE_GEMFILE, but it never gets that far: exe/ruby-lsp wraps the whole
;; composed-bundle setup in `if ENV["BUNDLE_GEMFILE"].nil?' and re-execs under
;; `bundle exec' only from inside that branch. Presetting the variable
;; therefore *skips* bundle composition altogether -- measured against epoxy,
;; that dropped the server out of any bundle at all: 142 "already initialized
;; constant" warnings from duplicate RuboCop activation, the Standard Ruby
;; addon activating but never initializing, and `ActiveRecord::Base' still
;; landing on the spec stub. It makes things worse, not better.
;;
;; ruby-lsp takes its project from `Dir.pwd' in both places that matter --
;; `SetupBundler.new(Dir.pwd)' in exe/ruby-lsp, which is what detects the
;; Rails app and adds ruby-lsp-rails to the composed Gemfile, and
;; `RubyIndexer::Configuration#@workspace_path' -- so pointing the process at
;; integration/ is the whole fix. Eglot's own cwd is `(project-root project)'
;; (eglot.el:1814), which we do not want to move: it is what keeps every
;; buffer under gems/*/lib on one server.
;;
;; The tradeoff is real and worth knowing: with the working directory in
;; integration/, the indexer's workspace glob covers integration/** and the
;; `lib' require path of each path gem -- so gems/*/lib still resolves, but
;; the repository's own lib/ and every gems/*/spec stop being indexed.

(defconst my/ruby-lsp-shim
  (expand-file-name "bin/mise-ruby-lsp" my-emacs-dir)
  "Wrapper that runs ruby-lsp under mise with the JFrog Bundler token set.")

(defconst my/env-executable "/usr/bin/env"
  "Absolute path to env(1), used only for its -C flag.
Not `executable-find': a GUI Emacs starts with no shell PATH. macOS's
env documents -C -- \"usage: env [-0iv] [-C workdir] ...\" -- passes the
whole inherited environment through untouched, and execs its target, so
this buys a working directory at the cost of no extra process and none
of the quoting a `sh -c \"cd ... && exec ...\"' would need.")

(defvar my/ruby-lsp-directory nil
  "Directory ruby-lsp should run in, or nil for the project root.
A string, absolute or relative to the project root. Marked
`safe-local-variable', so a repository's .dir-locals.el can set it
without prompting -- and .dir-locals.el is globally git-ignored here, so
that works in a shared repository without dirtying it.")

(put 'my/ruby-lsp-directory 'safe-local-variable #'stringp)

(defconst my/ruby-lsp-directory-overrides
  '(("epoxy" . "integration"))
  "Alist of (PROJECT-NAME . DIRECTORY) ruby-lsp working directories.
PROJECT-NAME is matched against the last component of the project root,
so no absolute path belongs in here. DIRECTORY is read exactly like
`my/ruby-lsp-directory', which takes precedence over this alist. For
repositories where a .dir-locals.el is unwelcome.")

(defun my/ruby-lsp-directory (root)
  "Absolute directory ruby-lsp should run in for the project at ROOT.

Nil -- meaning \"just use ROOT\" -- unless an override names a directory
that exists and holds a Gemfile. A stale override has to degrade to
normal behaviour: pointing the server at a directory with no bundle
would leave it with no composed bundle and no index worth having, which
is a worse failure than the one the override is there to fix."
  (let* ((name (or my/ruby-lsp-directory
                   (cdr (assoc (file-name-nondirectory
                                (directory-file-name root))
                               my/ruby-lsp-directory-overrides))))
         (dir (and name (file-name-as-directory
                         (expand-file-name name root)))))
    (and dir
         (file-directory-p dir)
         (file-regular-p (expand-file-name "Gemfile" dir))
         dir)))

(defun my/ruby-lsp-contact (&optional _interactive project)
  "Return the command eglot should run to start ruby-lsp for PROJECT.

Eglot funcalls this from `eglot--guess-contact', which runs in the Ruby
buffer from `post-command-hook' by way of `eglot-ensure' -- late enough
that `hack-local-variables' has already applied any .dir-locals.el, so
`my/ruby-lsp-directory' is readable here even though it would not be in
a major-mode hook.

PROJECT is the project eglot is about to root the server at, and
`project-root' is exactly the directory the server would otherwise start
in; env -C then overrides that directory alone, leaving eglot's
workspace root -- and so the set of buffers this one server manages --
untouched.

`project-root' and not `my/ruby-project-root' on purpose, and the
difference matters in exactly the repositories this exists for: every
epoxy sub-gem carries its own Gemfile, so for gems/core/lib/epoxy/boot.rb
`my/ruby-project-root' stops at gems/core/ while eglot roots the server
at the repository. Keying the override off anything but the root eglot
actually uses would leave half a monorepo's buffers unmatched."
  (let* ((root (expand-file-name (if project
                                     (project-root project)
                                   (my/ruby-project-root))))
         (dir (my/ruby-lsp-directory root)))
    (if dir
        (list my/env-executable "-C" (directory-file-name dir)
              my/ruby-lsp-shim)
      (list my/ruby-lsp-shim))))

;; flycheck : https://github.com/flycheck/flycheck
(use-package flycheck
  :ensure t
  :straight (flycheck :type git :host github :repo "flycheck/flycheck")
  :hook ((prog-mode . flycheck-mode)
         (ruby-ts-mode . my/setup-ruby-flycheck)
         (ruby-mode . my/setup-ruby-flycheck))
  :config
  (defun my/ruby-uses-standard-p (root)
    "Non-nil when the Ruby project at ROOT lints with StandardRB.
With neither a Gemfile.lock nor a Gemfile to read, falls back to whatever
`standardrb' happens to be on `exec-path'."
    (cond
     ((my/ruby-gem-in-project-p root "standard") t)
     ((or (file-regular-p (expand-file-name "Gemfile.lock" root))
          (file-regular-p (expand-file-name "Gemfile" root)))
      nil)
     (t (and (executable-find "standardrb") t))))

  (defun my/setup-ruby-flycheck ()
    "Run this project's Ruby linter through mise, and Bundler when present.
Flycheck has no per-checker argument list -- there is no
`flycheck-...-executable-args' -- so prefixing a checker's command is done
with `flycheck-command-wrapper-function', which receives the whole argv."
    (when (derived-mode-p 'ruby-ts-mode 'ruby-mode)
      (let* ((root (my/ruby-project-root))
             (bundled (file-exists-p (expand-file-name "Gemfile" root))))
        (setq-local flycheck-command-wrapper-function
                    (lambda (command)
                      (append '("mise" "x" "--")
                              (and bundled '("bundle" "exec"))
                              command)))
        ;; The wrapper alone is not enough. `flycheck-define-command-checker'
        ;; injects `:enabled (lambda () (and (flycheck-find-checker-executable
        ;; symbol) ...))', which resolves the BARE name over `exec-path' and
        ;; never sees the wrapper. When the linter lives only in the bundle
        ;; that predicate returns nil, the negative is cached per buffer in
        ;; `flycheck--automatically-disabled-checkers', and because
        ;; `flycheck-checkers' is pinned to this one checker while
        ;; `eglot-stay-out-of' has already dropped Flymake, the buffer shows
        ;; zero diagnostics and no error -- only C-c ! v tells you why. When
        ;; it IS found globally it is worse: the argv carries the absolute
        ;; *global* binstub under `bundle exec', which Bundler rejects
        ;; whenever that gem's version differs from Gemfile.lock.
        ;; `identity' hands the bare name straight through, so the argv reads
        ;; "mise x -- bundle exec standardrb ..." and resolution happens
        ;; inside the bundle, where it belongs.
        (setq-local flycheck-executable-find #'identity)
        (setq-local flycheck-checkers
                    (if (my/ruby-uses-standard-p root)
                        '(ruby-standard)
                      '(ruby-rubocop))))))

  :bind (("C-c ! l" . flycheck-list-errors)
         ("C-c ! n" . flycheck-next-error)
         ("C-c ! p" . flycheck-previous-error)
         ("C-c ! c" . flycheck-buffer)
         ("C-c ! v" . flycheck-verify-setup)))

;; Tree-sitter configuration
(use-package treesit
  :ensure nil
  :straight nil
  :config
  ;; Tree-sitter grammar sources
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java . ("https://github.com/tree-sitter/tree-sitter-java"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))
          (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

  (defun my/treesit-install-all-languages ()
    "Install all tree-sitter languages."
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

  (unless (file-exists-p (expand-file-name "tree-sitter" user-emacs-directory))
    (my/treesit-install-all-languages))

  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)  ; Map shell mode to bash
          (shell-mode . bash-ts-mode)  ; Map shell mode to bash
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (css-mode . css-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (go-mode . go-ts-mode)
          (html-mode . html-ts-mode)
          (java-mode . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (markdown-mode . markdown-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (toml-mode . toml-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))

  (add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.bash\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.fish\\'" . bash-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ruby\\'" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-ts-mode))
  ;; .md is owned by the `markdown-ts-mode' declaration below, which also
  ;; has to supply the autoload the built-in library lacks.
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.htm\\'" . html-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.pyi\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-ts-mode))

  ;; Tree-sitter will be enabled automatically for supported modes
  )

;; markdown-ts-mode : [built-in since Emacs 31]
;; The library ships in Resources/lisp/textmodes/, but upstream deliberately
;; withholds the autoload cookie -- it calls the mode experimental and does
;; not enable it by default -- so loaddefs.el only carries
;; `register-definition-prefixes' for it and `markdown-ts-mode' is void until
;; something requires the file. With a bare auto-mode-alist entry every .md
;; buffer therefore died on "Ignoring unknown mode 'markdown-ts-mode'" and
;; fell back to fundamental-mode. `:mode' makes use-package emit the missing
;; autoload alongside the auto-mode-alist entries.
(use-package markdown-ts-mode
  :ensure nil
  :straight nil
  :mode (("\\.md\\'" . markdown-ts-mode)
         ("\\.markdown\\'" . markdown-ts-mode)))

;; docker.el : https://github.com/Silex/docker.el
(use-package docker
  :ensure t
  :straight (docker :type git :host github :repo "Silex/docker.el")
  :bind ("C-c d" . docker))

;; iflipb : https://github.com/jrosdahl/iflipb
(use-package iflipb
  :ensure t
  :straight (iflipb :type git :host github :repo "jrosdahl/iflipb")
  :bind (("<C-tab>" . 'iflipb-next-buffer)
	 ("<C-S-tab>" . 'iflipb-previous-buffer)
	 ("M-o" . 'other-window)))

;; editorconfig : [built-in since Emacs 30]
(use-package editorconfig
  :ensure nil
  :straight nil
  :config
  (editorconfig-mode 1))

;; ghostel : https://github.com/dakra/ghostel
;;
;; Terminal emulator on libghostty-vt, the VT engine from Ghostty. This
;; replaced vterm, which could not draw a full-screen TUI cleanly at any
;; setting: libvterm reports damaged cells with no notion of where a frame
;; begins or ends, so `vterm-timer-delay' was the only lever available and
;; every value on it traded torn frames against dropped ones -- measured here
;; at 290x77, the stock 0.1 landed 9 of 30 frames a second and nil drew 90
;; frames as 2094 partial redraws.
;;
;; libghostty-vt implements synchronized output (DEC mode 2026): the
;; application brackets each frame, and the terminal withholds the redraw
;; until the frame is complete. Claude Code uses it. That is a protocol fix
;; rather than a tuning one, which is why none of the vterm frame-budget
;; measurements carried over -- there is no longer a budget to tune.
;;
;; Two further wins deleted configuration outright:
;;
;;   * TERM is `xterm-ghostty' with terminfo bundled in etc/, so a TUI
;;     queries the terminal's real capabilities instead of assuming a 1990s
;;     xterm and probing.
;;   * The wheel is bound natively (ghostel.el:1072) at
;;     `emulation-mode-map-alists' priority, and copy mode exits itself. The
;;     ~60 lines of hand-rolled wheel scrollback vterm needed -- enter copy
;;     mode on wheel-up, leave on wheel-down, rebind every double- and
;;     triple- spelling because vterm bound no wheel events at all -- are all
;;     gone, and so is the XOFF-the-pty caveat that came with them.
;;
;; The native module is a prebuilt binary fetched from GitHub releases on
;; first use: no cmake, no libtool, no vendored libvterm build, and none of
;; the Apple-Silicon dylib archaeology vterm's :init block existed to avoid.
;;
;; Interactive zsh runs `mise activate zsh' (~/.config/zsh/conf.d/60-tools.zsh),
;; so a ghostel shell already has the project's runtimes -- unlike Emacs' own
;; subprocesses, which is why compile and flycheck prefix `mise x --'.
(use-package ghostel
  :ensure t
  ;; :files mirrors the MELPA recipe. The Lisp is under lisp/, which
  ;; straight's default directive already covers, but etc/ carries the
  ;; bundled terminfo and src/ + vendor/ + build.zig* are what
  ;; `ghostel-module-compile' needs if the prebuilt binary ever has to be
  ;; built locally (that route wants a Zig toolchain).
  :straight (ghostel :type git :host github :repo "dakra/ghostel"
                     :files (:defaults "etc" "src" "vendor"
                                       "build.zig" "build.zig.zon"
                                       "symbols.map"))
  :hook (ghostel-mode . my/ghostel-setup)
  :custom
  ;; Upstream default is `ask', which prompts on the first `M-x ghostel'.
  ;; This is read at the interactive entry point rather than at load time, so
  ;; unlike vterm's module variables it does not have to be in :init.
  (ghostel-module-auto-install 'download)
  ;; The shell vterm ran. On macOS ghostel additionally wraps it through
  ;; login(1) -- see `ghostel-macos-login-shell' -- so ~/.zprofile is sourced
  ;; the way Terminal.app and Ghostty do it. vterm's shell was interactive
  ;; but not a login shell, so this is a behaviour change: anything in
  ;; ~/.zprofile now runs that previously did not.
  (ghostel-shell "/bin/zsh")
  (ghostel-kill-buffer-on-exit t)
  :config
  ;; Deliberately NOT set: `ghostel-timer-delay'. Upstream's default is
  ;; already 0.033 -- the same knee this config measured for vterm -- and
  ;; `ghostel-adaptive-fps' (default t) varies it under load, which beats any
  ;; fixed value. `ghostel-max-scrollback' is left at its 5MB default too;
  ;; vterm's 10000 was a line count and does not translate.

  (defun my/ghostel-setup ()
    "Drop line numbers, use a Nerd Font, and keep terminal glyphs literal."
    (display-line-numbers-mode 0)
    ;; Mono variant deliberately: the proportional patch drifts columns in a
    ;; terminal. Same family as the default face, so widths agree. ghostel
    ;; advises `buffer-face-mode' (ghostel.el:4671) to recompute its cell
    ;; geometry when the font changes, so this is a supported route. The
    ;; alternative -- customizing the `ghostel-default' face -- would have to
    ;; call `my/mono-font' at load time, which is exactly the frameless-daemon
    ;; case that function warns about.
    (setq-local buffer-face-mode-face
                (list :family (my/mono-font) :height 120))
    (buffer-face-mode t)

    ;; Claude Code pads its UI with U+00A0 (the input line is "❯" U+00A0 ...).
    ;; `nobreak-char-display' is t by default and draws those cells in the
    ;; `nobreak-space' face, whose stock spec is
    ;;   (:inherit escape-glyph :underline t)
    ;; -- so every pad character renders as a cyan underlined cell. That is
    ;; the stray "underscore" after the prompt. This one is Emacs' own display
    ;; layer rather than the VT engine, so it outlived the move off vterm:
    ;; highlighting non-break space is a prose-editing aid, and a terminal
    ;; must draw exactly the glyphs the application sent.
    (setq-local nobreak-char-display nil)

    ;; Terminal output is a grid of LTR cells, but Emacs cannot know that and
    ;; runs the bidirectional and paragraph-direction algorithms over every
    ;; line each redisplay -- at 290 columns, on every frame. Pinning the
    ;; direction lets redisplay skip both. ghostel sets `truncate-lines' and
    ;; the scroll margins itself, but not these.
    (setq-local bidi-paragraph-direction 'left-to-right)
    (setq-local bidi-inhibit-bpa t))

  ;; Nothing here disables `font-lock-mode', and nothing should re-enable it.
  ;; vterm needed font-lock ON: its module wrote colours into a
  ;; `font-lock-face' property, which only reaches redisplay through the
  ;; (face font-lock-face) alias font-lock installs. ghostel writes the real
  ;; `face' property, turns font-lock off in `ghostel-mode' itself
  ;; (ghostel.el:4827), and additionally points
  ;; `font-lock-unfontify-region-function' at #'ignore so that a config which
  ;; forces font-lock back on cannot strip the per-cell faces on redraw.

  :bind (("C-c t" . ghostel)))

;; Muscle memory. vterm and multi-vterm are gone; a decade of `M-x vterm' and
;; `M-x multi-vterm' is not, and neither are the C-c m keys.
;;
;; Everything below reaches ghostel through the `my/term*' names, so swapping
;; the backend again means editing these aliases and nothing else -- no
;; keybinding, no muscle-memory shim and no caller has to know.
;;
;; One semantic difference the aliases have to paper over: `multi-vterm'
;; always created a NEW numbered terminal, while plain `ghostel' switches to
;; the existing one and only creates on a prefix argument. `my/term-new' keeps
;; the old behaviour, so the alias does what the fingers expect.
(defalias 'my/term          #'ghostel)
(defalias 'my/term-project  #'ghostel-project)
(defalias 'my/term-next     #'ghostel-next)
(defalias 'my/term-previous #'ghostel-previous)
(defalias 'my/term-list     #'ghostel-list-buffers)

(defun my/term-new ()
  "Open a new terminal, never reusing an existing one.
This is what `multi-vterm' did.  Plain `ghostel' switches to the
terminal that already exists and needs a prefix argument to make
another, so the two are not interchangeable."
  (interactive)
  ;; A non-numeric prefix arg is ghostel's "create a new buffer" spelling.
  (ghostel '(4)))

(defun my/term-dedicated-toggle ()
  "Show or hide a terminal in a short window at the foot of the frame.
Replaces `multi-vterm-dedicated-toggle'.  ghostel has no dedicated-window
concept of its own, so this is an ordinary side window at the same 30 lines
`multi-vterm-dedicated-window-height' used to ask for."
  (interactive)
  (require 'ghostel)
  (let* ((name "*ghostel-dedicated*")
         (buffer (get-buffer name))
         (window (and buffer (get-buffer-window buffer)))
         (action '((display-buffer-in-side-window)
                   (side . bottom)
                   (window-height . 30))))
    (cond
     (window (delete-window window))
     (buffer (pop-to-buffer buffer action))
     ;; `ghostel-create' takes a display ACTION precisely for this, and
     ;; sizes the pty from the window it ends up in.
     (t (ghostel-create name action)))))

;; The old names. `defalias' to an autoloaded command is resolved at call
;; time, so these do not drag ghostel in at startup.
(defalias 'vterm                        #'my/term)
(defalias 'vterm-other-window           #'my/term-new)
(defalias 'multi-vterm                  #'my/term-new)
(defalias 'multi-vterm-next             #'my/term-next)
(defalias 'multi-vterm-prev             #'my/term-previous)
(defalias 'multi-vterm-dedicated-toggle #'my/term-dedicated-toggle)

;; Same keys multi-vterm had. C-c t (plain ghostel) is bound in the
;; use-package block above. Inside a ghostel buffer `C-c C-t' is copy mode
;; from `ghostel-mode-map', which outranks this global binding -- the same
;; arrangement vterm-copy-mode had with multi-vterm.
(keymap-global-set "C-c C-t" #'my/term-new)
(keymap-global-set "C-c m t" #'my/term-dedicated-toggle)
(keymap-global-set "C-c m n" #'my/term-next)
(keymap-global-set "C-c m p" #'my/term-previous)

;; aidermacs: https://github.com/MatthewZMD/aidermacs
(use-package aidermacs
  :straight (aidermacs :type git :host github :repo "MatthewZMD/aidermacs")
  :init
  ;; Route Aider through OpenRouter (OpenAI-compatible). The key comes from
  ;; .env via my/load-dotenv in early-init.el.
  (setenv "OPENAI_API_BASE" "https://openrouter.ai/api/v1")
  (when-let* ((key (getenv "OPENROUTER_API_KEY")))
    (setenv "OPENAI_API_KEY" key))
  :custom
  ;; aidermacs dispatches on this in `aidermacs-backends.el' and knows exactly
  ;; two backends, comint and vterm -- there is no ghostel backend upstream.
  ;; With vterm removed, `vterm' here would fail at `aidermacs-run'.
  ;;
  ;; comint is the honest choice rather than a downgrade: the variable used to
  ;; be misspelt `aidermacs-terminal-backend', was therefore never read, and
  ;; aider ran on comint for the whole life of this config regardless. If
  ;; aider's output ever renders badly, `ghostel-comint-global-mode' gives
  ;; comint buffers ghostel's VT rendering.
  (aidermacs-backend 'comint)
  ;; Upstream default is '("aider-ce" "aider"); keep the fallback order and
  ;; let aidermacs-get-program resolve it against exec-path.
  (aidermacs-program '("aider-ce" "aider"))
  (aidermacs-default-model "openai/gpt-5")
  :bind (("C-c a a" . aidermacs-run)
         ("C-c a s" . aidermacs-question-code)
         ("C-c a f" . aidermacs-add-file)
         ("C-c a b" . aidermacs-add-current-file)
         ("C-c a r" . aidermacs-drop-current-file)
         ("C-c a R" . aidermacs-drop-all-files)
         ("C-c a k" . aidermacs-exit)))

;; Ruby project commands, bound under the Hyper "c" prefix.
(defun my/ruby-format-buffer ()
  "Format the current buffer with its language server."
  (interactive)
  (if (bound-and-true-p eglot--managed-mode)
      (eglot-format-buffer)
    (user-error "No language server for this buffer")))

(defun my/ruby-run-tests ()
  "Run this project's Ruby test suite through mise.

The runner is picked from what the project actually has, not assumed: a
.rspec file or `rspec' in the bundle means RSpec, and otherwise a test/
directory decides between `rails test' and `rake test'. Assuming RSpec
for every Gemfile handed minitest projects `bundle exec rspec' and a
compilation buffer full of failure."
  (interactive)
  (let* ((root (my/ruby-project-root))
         (default-directory root)
         (has (lambda (name) (file-exists-p (expand-file-name name root))))
         (prefix (if (funcall has "Gemfile")
                     "mise x -- bundle exec "
                   "mise x -- "))
         (tests (file-directory-p (expand-file-name "test" root)))
         (command
          (cond
           ((or (funcall has ".rspec")
                (my/ruby-gem-in-project-p root "rspec")
                (file-directory-p (expand-file-name "spec" root)))
            (concat prefix "rspec"))
           ((and tests (funcall has "bin/rails")) (concat prefix "rails test"))
           ((and tests (funcall has "Rakefile")) (concat prefix "rake test"))
           (t (user-error "No RSpec or minitest suite under %s" root)))))
    (compile command)))

(defun my/rails-console ()
  "Open a Rails console for this project in a ghostel terminal.

Uses `ghostel-exec' rather than let-binding `ghostel-shell', for two
reasons.  It takes the command as distinct argv entries, so nothing is
handed to a shell to re-parse.  And the macOS login(1) wrap that
`ghostel-macos-login-shell' applies is documented as covering only the
interactive shell `ghostel' spawns, never `ghostel-exec' -- wrapping a
Rails console in `login -flp' would be wrong.

The buffer is displayed before the process starts because `ghostel-exec'
sizes the pty from the window the buffer is already in, falling back to
80x24 when it is not on screen."
  (interactive)
  (require 'ghostel)
  (let* ((root (my/ruby-project-root))
         (default-directory root))
    (unless (file-exists-p (expand-file-name "bin/rails" root))
      (user-error "Not in a Rails project"))
    (let ((buffer (generate-new-buffer "*rails console*")))
      (with-current-buffer buffer
        (setq default-directory root))
      (pop-to-buffer buffer)
      (ghostel-exec buffer "mise"
                    '("x" "--" "bundle" "exec" "rails" "console")))))

;; My own custom configuration
(use-package emacs
  :hook ((org-mode . (lambda () (display-line-numbers-mode 0)))
         (term-mode . (lambda () (display-line-numbers-mode 0)))
         (shell-mode . (lambda () (display-line-numbers-mode 0)))
         (ibuffer-mode . (lambda () (display-line-numbers-mode 0)))
         (treemacs-mode . (lambda () (display-line-numbers-mode 0)))
         (before-save . delete-trailing-whitespace)
         (after-init . (lambda ()
                         ;; Drop from the 64MB startup threshold, but not all
                         ;; the way to Emacs' stock 800000 (800KB) as this
                         ;; used to. Measured in this config after ordinary
                         ;; use: 76 collections costing 2.007s total, i.e.
                         ;; ~26ms per GC. A terminal conses a fresh
                         ;; propertized string per line per redraw, so a
                         ;; full-viewport TUI repaint blew an 800KB budget
                         ;; several times a second and every one of those was
                         ;; a visible hitch.
                         (setq gc-cons-threshold (* 32 1024 1024)))))
  :config
  (setq inhibit-startup-message t)

  ;; Bigger reads, fewer syscalls on bulk output. This was never what stopped
  ;; partial frames -- under vterm, 64KB and 1MB reads delivered identical
  ;; frame counts, because the redraw throttle was what coalesced mid-frame
  ;; reads. Under ghostel that job belongs to synchronized output, and this
  ;; is left in for what it actually does: fewer read(2) calls when a command
  ;; dumps a lot of text at once.
  (setq read-process-output-max (* 1024 1024))

  ;; Set font to Menlo (clean macOS programming font)
  ;; The Nerd Font patch means Private Use Area icon glyphs render inline in
  ;; any buffer, not only where nerd-icons sets its own face.
  (let ((family (my/mono-font)))
    (set-face-attribute 'default nil
                        :family family
                        :height 120
                        :weight 'normal)
    (set-face-attribute 'fixed-pitch nil
                        :family family
                        :height 120))

  ;; Emacs 31 knows the `emoji' script; macOS ships the colour font. This is
  ;; what actually draws emoji -- emojify only rewrites shortcodes.
  (when (find-font (font-spec :family "Apple Color Emoji"))
    (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji")
                      nil 'prepend))

  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room
  (menu-bar-mode -1)            ; Disable the menu bar

  ;; Enable line numbers
  (column-number-mode)
  (global-display-line-numbers-mode t)

  ;; Always prefer UTF-8
  (prefer-coding-system 'utf-8-unix)
  (setq x-select-request-type
	'(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; Set up the visible bell
  (setq visible-bell t)

  ;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Start in full screen
  ; (add-to-list 'default-frame-alist '(fullscreen . fullscreen))

  (keymap-global-set "M-/" 'comment-or-uncomment-region))

;; The Caps-Lock Hyper key layer -- see lisp/my-hyper.el.
(require 'my-hyper)

(provide 'init)
;;; init.el ends here
