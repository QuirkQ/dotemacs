;;; init.el --- Quint's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:

; Quint his fantastic init.el

;;; Code:

(defconst my-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

;; Configuration split out of this file lives in lisp/.
(add-to-list 'load-path (expand-file-name "lisp" my-emacs-dir))

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
  :straight (nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el"))

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

;; csv-mode : GNU ELPA -- not built in, despite the old comment claiming so
(use-package csv-mode
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
(use-package emojify
  :ensure t
  :straight (emojify :type git :host github :repo "iqbalansari/emacs-emojify"))

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
ruby-lsp picks StandardRB or RuboCop from the project's Gemfile itself."
    (when (derived-mode-p 'ruby-ts-mode 'ruby-mode)
      (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

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
Decided by reading Gemfile.lock and Gemfile.  The obvious alternative,
shelling out to `bundle list standard', is a synchronous subprocess on a
mode hook -- it blocks the first keystroke in every Ruby buffer and pops
up *Shell Command Output*."
    (let ((lock (expand-file-name "Gemfile.lock" root))
          (gemfile (expand-file-name "Gemfile" root)))
      (cond
       ((file-readable-p lock)
        (with-temp-buffer
          (insert-file-contents lock)
          (goto-char (point-min))
          (and (re-search-forward "^ +standard \\((\\|$\\)" nil t) t)))
       ((file-readable-p gemfile)
        (with-temp-buffer
          (insert-file-contents gemfile)
          (goto-char (point-min))
          (and (re-search-forward "^ *gem +['\"]standard['\"]" nil t) t)))
       (t (and (executable-find "standardrb") t)))))

  (defun my/setup-ruby-flycheck ()
    "Run this project's Ruby linter through mise, and Bundler when present.
Flycheck has no per-checker argument list -- there is no
`flycheck-...-executable-args' -- so prefixing a checker's command is done
with `flycheck-command-wrapper-function', which receives the whole argv."
    (when (derived-mode-p 'ruby-ts-mode 'ruby-mode)
      (let* ((root (or (vc-root-dir) default-directory))
             (bundled (file-exists-p (expand-file-name "Gemfile" root))))
        (setq-local flycheck-command-wrapper-function
                    (lambda (command)
                      (append '("mise" "x" "--")
                              (and bundled '("bundle" "exec"))
                              command)))
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
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-ts-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . markdown-ts-mode))
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
  :config
  (setq vterm-shell "/bin/zsh")
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t)
  ;; Build without asking, and against the vendored libvterm. Linking the
  ;; system dylib is the classic Apple-Silicon failure: a stray x86_64
  ;; /usr/local/lib/libvterm.dylib gets picked up and the link fails with
  ;; "building for macOS-arm64 but attempting to link with file built for
  ;; macOS-x86_64".
  (setq vterm-always-compile-module t)
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=OFF")

  (defun my/vterm-setup ()
    "Drop line numbers and use a Nerd Font so shell icons render."
    (display-line-numbers-mode 0)
    (setq-local buffer-face-mode-face
                (if (find-font (font-spec :family "JetBrainsMono Nerd Font"))
                    '(:family "JetBrainsMono Nerd Font" :height 120)
                  '(:family "Menlo" :height 120)))
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
  "Run the project's Ruby test suite through mise."
  (interactive)
  (let ((default-directory (or (vc-root-dir) default-directory)))
    (compile (if (file-exists-p "Gemfile")
                 "mise x -- bundle exec rspec"
               "mise x -- ruby -I test test/"))))

(defun my/rails-console ()
  "Open a Rails console for this project in a vterm."
  (interactive)
  (let ((default-directory (or (vc-root-dir) default-directory)))
    (if (file-exists-p "bin/rails")
        (vterm-other-window "mise x -- bundle exec rails console")
      (user-error "Not in a Rails project"))))

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
  (set-face-attribute 'default nil
                      :family "Menlo"
                      :height 120
                      :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
                      :family "Menlo"
                      :height 120)

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
