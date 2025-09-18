;;; init.el --- Quint's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:

; Quint his fantastic init.el

;;; Code:

(defconst my-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

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

;; Add Brew to my Emacs PATH
(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
(add-to-list 'exec-path "/opt/homebrew/bin")

;; Add Docker to Emacs PATH
(setenv "PATH" (concat (getenv "PATH") ":/Users/quint.pieters/.docker/bin"))
(add-to-list 'exec-path "/Users/quint.pieters/.docker/bin")

;; mise - https://github.com/liuyinz/mise.el
(use-package mise
  :straight (mise :type git :host github :repo "liuyinz/mise.el")
  :hook ((after-init . global-mise-mode)
         (find-file . my/mise-activate-for-ruby-project)
         (ruby-ts-mode . my/mise-activate-for-ruby-project)
         (ruby-mode . my/mise-activate-for-ruby-project))
  :config
  ;; Automatically activate mise in project directories
  (setq mise-auto-activate t)
  ;; Enable more aggressive mise environment updates
  (setq mise-cache-env t)

  ;; Force mise activation for Ruby files using git project root
  (defun my/mise-activate-for-ruby-project ()
    "Activate mise for the current Ruby file's git project root."
    (when (and buffer-file-name
               (derived-mode-p 'ruby-ts-mode 'ruby-mode))
      (when-let* ((project-root (or (vc-root-dir)
                                    (and (fboundp 'project-root)
                                         (project-root (project-current))))))
        ;; Set default-directory to project root and activate mise
        (let ((default-directory project-root))
          (mise-mode 1))))))

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
  :straight (aggressive-indent :type git :host github :repo "Malabarba/aggressive-indent-mode", :files ("dist" "*.el")))

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
	 ("<f19> d" . counsel-git)
	 ("<f19> f" . counsel-git-grep)
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

;; which-key : https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :straight (which-key :type git :host github :repo "justbur/emacs-which-key")
  :config
  (which-key-mode))

;; company-mode : https://github.com/company-mode/company-mode
(use-package company
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

;; robe : https://github.com/dgutov/robe
(use-package robe
  :ensure t
  :straight (robe :type git :host github :repo "dgutov/robe")
  :after company
  :hook ((ruby-ts-mode . robe-mode)
         (ruby-mode . robe-mode)
         (ruby-ts-mode . my/setup-robe-company)
         (ruby-mode . my/setup-robe-company)
         (ruby-ts-mode . my/robe-start-maybe))
  :config
  ;; Set up company-robe backend for Ruby modes specifically
  (defun my/setup-robe-company ()
    "Set up company-robe backend for the current buffer."
    (when (and (bound-and-true-p company-mode)
               (derived-mode-p 'ruby-ts-mode 'ruby-mode))
      (set (make-local-variable 'company-backends)
           (append '(company-robe) company-backends))))

  ;; Path to our robe-specific Gemfile
  (defvar my/robe-gemfile (expand-file-name "Gemfile.robe" user-emacs-directory)
    "Path to the robe-specific Gemfile for bundler-compose.")

  (defun my/bundler-compose-available-p ()
    "Check if bundler-compose is available."
    (zerop (shell-command "gem exec bundler-compose help >/dev/null 2>&1")))

  (defun my/robe-start-with-compose ()
    "Start robe using bundler-compose if available, otherwise fallback to regular robe-start."
    (interactive)
    (if (and (file-exists-p my/robe-gemfile)
             (my/bundler-compose-available-p))
        (progn
          (message "Starting robe with bundler-compose...")
          ;; Ensure inf-ruby is loaded first
          (require 'inf-ruby nil t)
          ;; Create a custom implementation for bundler-compose
          (let ((original-implementations inf-ruby-implementations))
            (setq inf-ruby-implementations
                  (cons `("bundler-compose" .
                          ("bundle" "compose" "Gemfile" ,my/robe-gemfile "--exec" "ruby"))
                        inf-ruby-implementations))
            ;; Start robe with bundler-compose
            (let ((inf-ruby-default-implementation "bundler-compose"))
              (robe-start))
            ;; Restore original implementations
            (setq inf-ruby-implementations original-implementations)))
      (progn
        (message "bundler-compose not available, using regular robe-start")
        (robe-start))))

  (defun my/robe-start-maybe ()
    "Start robe if not already running in Ruby buffers."
    (when (and (not (robe-running-p))
               (derived-mode-p 'ruby-ts-mode 'ruby-mode))
      (my/robe-start-with-compose)))

  :bind (:map robe-mode-map
              ("C-c r j" . robe-jump)
              ("C-c r d" . robe-doc)
              ("C-c r s" . my/robe-start-with-compose)
              ("C-c r r" . robe-rails-refresh)))

;; flycheck : https://github.com/flycheck/flycheck
(use-package flycheck
  :ensure t
  :straight (flycheck :type git :host github :repo "flycheck/flycheck")
  :hook ((prog-mode . flycheck-mode)
         (ruby-ts-mode . my/setup-ruby-flycheck)
         (ruby-mode . my/setup-ruby-flycheck))
  :config
  ;; Set up Ruby checkers to use project-specific tools
  (defun my/setup-ruby-flycheck ()
    "Configure Ruby checkers for current project."
    (when (derived-mode-p 'ruby-ts-mode 'ruby-mode)
      (let ((project-root (or (vc-root-dir) default-directory))
            (has-gemfile (file-exists-p "Gemfile")))

        ;; Use bundler if Gemfile exists
        (if has-gemfile
            (progn
              (setq-local flycheck-ruby-standard-executable "bundle")
              (setq-local flycheck-ruby-standard-executable-args '("exec" "standardrb"))
              (setq-local flycheck-ruby-rubocop-executable "bundle")
              (setq-local flycheck-ruby-rubocop-executable-args '("exec" "rubocop")))
          (progn
            (setq-local flycheck-ruby-standard-executable "standardrb")
            (setq-local flycheck-ruby-rubocop-executable "rubocop")))

        ;; Prefer StandardRB over Rubocop
        (if (or (and has-gemfile (zerop (shell-command "bundle list standard >/dev/null 2>&1")))
                (executable-find "standardrb"))
            (setq-local flycheck-checkers '(ruby-standard))
          (setq-local flycheck-checkers '(ruby-rubocop))))))

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
          (terraform . ("https://github.com/MichaHoffmann/tree-sitter-hcl"))
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
          (kotlin-mode . kotlin-ts-mode)
          (markdown-mode . markdown-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (terraform-mode . terraform-ts-mode)
          (hcl-mode . terraform-ts-mode)
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
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . ruby-ts-mode)) ; ERB is Ruby with HTML
  (add-to-list 'auto-mode-alist '("\\.hcl\\'" . terraform-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tfvars\\'" . terraform-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.pyi\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-ts-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode)) ; Use built-in CSV mode

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

;; editorconfig : https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :ensure t
  :straight (editorconfig :type git :host github :repo "editorconfig/editorconfig-emacs")
  :config
  (editorconfig-mode 1))

;; vterm : https://github.com/akermu/emacs-libvterm
(use-package vterm
  :ensure t
  :straight (vterm :type git :host github :repo "akermu/emacs-libvterm")
  :hook ((vterm-mode . (lambda () (display-line-numbers-mode 0)))
         (vterm-mode . (lambda ()
                         ;; Use JetBrains Mono Nerd Font for icons support
                         (condition-case nil
                             (progn
                               (setq-local buffer-face-mode-face '(:family "JetBrainsMono Nerd Font" :height 120))
                               (buffer-face-mode t))
                           (error
                            ;; Fallback to Menlo if Nerd Font not available
                            (setq-local buffer-face-mode-face '(:family "Menlo" :height 120))
                            (buffer-face-mode t))))))
  :config
  ;; Set shell to zsh (since you're on macOS)
  (setq vterm-shell "/bin/zsh")
  ;; Increase scrollback buffer
  (setq vterm-max-scrollback 10000)
  ;; Kill buffer when terminal process exits
  (setq vterm-kill-buffer-on-exit t)
  ;; Always use current directory for new vterm buffers
  (setq vterm-always-compile-module t)
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

;; My own custom configuration
(use-package emacs
  :hook ((org-mode . (lambda () (display-line-numbers-mode 0)))
         (term-mode . (lambda () (display-line-numbers-mode 0)))
         (shell-mode . (lambda () (display-line-numbers-mode 0)))
         (ibuffer-mode . (lambda () (display-line-numbers-mode 0)))
         (treemacs-mode . (lambda () (display-line-numbers-mode 0)))
         (vterm-mode . (lambda () (display-line-numbers-mode 0)))
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

  (keymap-global-set "M-/" 'comment-or-uncomment-region)
  (keymap-global-set "<f19> <left>" 'previous-buffer)
  (keymap-global-set "<f19> <right>" 'next-buffer)
  (keymap-global-set "<f19> r" 'kmacro-start-macro)
  (keymap-global-set "<f19> e" 'kmacro-end-macro)
  (keymap-global-set "<f19> SPC" 'kmacro-end-or-call-macro))

(provide 'init)
;;; init.el ends here
