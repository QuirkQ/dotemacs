;;; init.el --- Quint's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:

; Quint his fantastic init.el

;;; Code:

(defconst my-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

;; Configuration split out of this file lives in lisp/.
(add-to-list 'load-path (expand-file-name "lisp" my-emacs-dir))

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
;; multi-vterm declares (project "0.3.0") as a dependency, which made straight
;; clone ELPA `project' and `xref' on top of Emacs 31's own. They then load
;; *after* the built-ins are already in memory, producing
;;   Feature `project' loaded from ".../Resources/lisp/progmodes/project.elc"
;;   is now provided by ".../straight/build/project/project.elc"
;; and leaving two versions live at once -- which matters here because eglot
;; and xref are the backbone of the Ruby setup. Must precede the bootstrap.
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
  (setq doom-modeline-env-enable-ruby t))

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
  (setq emojify-display-style 'unicode))

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
  :hook ((ruby-ts-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         (eglot-managed-mode . my/eglot-format-on-save))
  :config
  (defconst my/ruby-lsp-shim
    (expand-file-name "bin/mise-ruby-lsp" my-emacs-dir)
    "Wrapper that runs ruby-lsp under mise with the JFrog Bundler token set.")

  ;; Eglot ships a solargraph entry for Ruby; add-to-list puts ours first.
  (add-to-list 'eglot-server-programs
               `((ruby-ts-mode ruby-mode) . (,my/ruby-lsp-shim)))

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

;; vterm : https://github.com/akermu/emacs-libvterm
;; Needs cmake and a C toolchain; the module builds on first load.
;; Interactive zsh runs `mise activate zsh' (~/.config/zsh/conf.d/60-tools.zsh),
;; so a vterm shell already has the project's runtimes -- unlike Emacs'
;; own subprocesses, which is why compile and flycheck prefix `mise x --'.
(use-package vterm
  :ensure t
  :straight (vterm :type git :host github :repo "akermu/emacs-libvterm")
  :hook (vterm-mode . my/vterm-setup)
  :init
  ;; These two MUST be set before vterm.el loads. Its module check
  ;; (vterm.el:136) is a top-level form: it either prompts
  ;; "Vterm needs `vterm-module' to work.  Compile it now?" or errors outright.
  ;; In `:config' they land after the load and are useless.
  ;;
  ;; Build against the vendored libvterm. Linking the system dylib is the
  ;; classic Apple-Silicon failure: a stray x86_64 /usr/local/lib/libvterm.dylib
  ;; gets picked up and the link fails with "building for macOS-arm64 but
  ;; attempting to link with file built for macOS-x86_64".
  (setq vterm-always-compile-module t)
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=OFF")
  :config
  (setq vterm-shell "/bin/zsh")
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t)

  (defun my/vterm-setup ()
    "Drop line numbers and use a Nerd Font so shell icons render."
    (display-line-numbers-mode 0)
    ;; Mono variant deliberately: the proportional patch drifts columns in a
    ;; terminal. Same family as the default face, so widths agree.
    (setq-local buffer-face-mode-face
                (list :family (my/mono-font) :height 120))
    (buffer-face-mode t))

  :bind (("C-c t" . vterm)))

;; multi-vterm : https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :ensure t
  :straight (multi-vterm :type git :host github :repo "suonlight/multi-vterm")
  :config
  ;; Set dedicated window for multi-vterm
  (setq multi-vterm-dedicated-window-height 30)
  :bind (("C-c C-t" . multi-vterm)
         ("C-c m t" . multi-vterm-dedicated-toggle)
         ("C-c m n" . multi-vterm-next)
         ("C-c m p" . multi-vterm-prev)))

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
  ;; The variable is `aidermacs-backend', not `aidermacs-terminal-backend' --
  ;; the old name was never read, so this had silently stayed on comint.
  (aidermacs-backend 'vterm)
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

;; Declared so the `let' in `my/rails-console' is a DYNAMIC binding even when
;; the file is byte-compiled before vterm.el has been loaded; under
;; lexical-binding a `let' on a not-yet-special symbol binds lexically and
;; vterm never sees the value. vterm.el owns the real defcustom.
(defvar vterm-shell)

(defun my/rails-console ()
  "Open a Rails console for this project in a vterm.

`vterm-other-window' takes a buffer name or a session index, not a
command (its interactive spec is \"P\"; \"with a string prefix arg,
create a new session with arg as buffer name\"). Passing the command
string opened a plain `vterm-shell' -- /bin/zsh -- in a buffer literally
*named* \"mise x -- bundle exec rails console\", with no error and no
console. The command has to arrive through `vterm-shell'."
  (interactive)
  (require 'vterm)
  (let* ((root (my/ruby-project-root))
         (default-directory root))
    (unless (file-exists-p (expand-file-name "bin/rails" root))
      (user-error "Not in a Rails project"))
    (let ((vterm-shell "mise x -- bundle exec rails console"))
      (vterm-other-window))))

;; My own custom configuration
(use-package emacs
  :hook ((org-mode . (lambda () (display-line-numbers-mode 0)))
         (term-mode . (lambda () (display-line-numbers-mode 0)))
         (shell-mode . (lambda () (display-line-numbers-mode 0)))
         (ibuffer-mode . (lambda () (display-line-numbers-mode 0)))
         (treemacs-mode . (lambda () (display-line-numbers-mode 0)))
         (before-save . delete-trailing-whitespace)
         (after-init . (lambda ()
                         ;; restore after startup
                         (setq gc-cons-threshold 800000))))
  :config
  (setq inhibit-startup-message t)

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
