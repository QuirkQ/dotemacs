;;; package --- init.el

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
  :init (add-hook 'after-init-hook #'global-mise-mode))

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
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

;; doom-modeline : https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :ensure t
  :straight (doom-modeline :type git :host github :repo "seagle0128/doom-modeline")
  :hook (after-init . doom-modeline-mode))

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
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
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

;; forge : https://github.com/magit/forge
(use-package forge
  :ensure t
  :after magit
  :straight (forge :type git :host github :repo "magit/forge")
  :config
  (setq auth-sources '("/Users/quint.pieters/.authinfo")))

;; forge : https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :ensure t
  :straight (emojify :type git :host github :repo "iqbalansari/emacs-emojify"))

;; code-review : https://github.com/wandersoncferreira/code-review
(use-package code-review
  :ensure t
  :after forge
  :straight (code-review :type git
			 :host github
			 :repo "wandersoncferreira/code-review"
			 :fork (:host github
                                :repo "phelrine/code-review"
				:branch "fix/closql-update"))
  :init
  (add-hook 'code-review-mode-hook #'emojify-mode))

;; Flycheck : https://www.flycheck.org
(use-package flycheck
  :ensure t
  :straight (flycheck :type git :host github :repo "flycheck/flycheck")
  :config
  ;; Inherit Emacs Lisp load path
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Disable Flycheck globally
  (global-flycheck-mode -1)

  ;; Define the configuration file variable for standardrb
  (flycheck-def-config-file-var flycheck-standardrbrc ruby-standardrb ".standard.yml"
    :safe #'stringp)

  ;; Define the lint-only option variable for standardrb
  (flycheck-def-option-var flycheck-standardrb-lint-only nil ruby-standardrb
    "Whether to only report code issues in Standardrb.

  When non-nil, only report code issues in Standardrb, via `--lint'.
  Otherwise report style issues as well."
    :safe #'booleanp
    :type 'boolean)

  ;; Define Flycheck checker for standardrb
  (flycheck-define-checker ruby-standardrb
    "A Ruby syntax and style checker using the standardrb gem."
    :command ("standardrb"
              "--display-cop-names"
              "--force-exclusion"
              "--format" "emacs"
              "--cache" "false"
              (config-file "--config" flycheck-standardrbrc)
              (option-flag "--lint" flycheck-standardrb-lint-only)
              "--stdin" source-original)
    :standard-input t
    :working-directory flycheck-ruby--find-project-root
    :error-patterns
    ((info line-start (file-name) ":" line ":" column ": C: "
           (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": W: "
              (optional (id (one-or-more (not (any ":")))) ": ") (message)
              line-end)
     (error line-start (file-name) ":" line ":" column ": " (or "E" "F") ": "
            (optional (id (one-or-more (not (any ":")))) ": ") (message)
            line-end))
    :modes (ruby-mode)
    :next-checkers ())

  ;; Ensure the globally installed standardrb is used
  (setq flycheck-ruby-standard-executable "standardrb")

  ;; Enable Flycheck and select the standardrb checker in ruby-mode
  :hook ((ruby-mode . flycheck-mode)
         (ruby-mode . (lambda ()
                        (flycheck-select-checker 'ruby-standardrb)))))

;; format-all : https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :ensure t
  :straight (format-all :type git :host github :repo "lassik/emacs-format-all-the-code")
  :config
  (setq format-all-formatters
	'(("Ruby" (standardrb))))

  ;; Ensure formatter is properly set on every new Ruby buffer
  (defun my/format-all-set-ruby-formatter ()
    (setq-local format-all-formatters
                '(("Ruby" (standardrb)))))

  :hook ((ruby-mode . format-all-mode)
	 (ruby-mode . my/format-all-set-ruby-formatter)
         (format-all-mode . format-all-ensure-formatter)))  ; Ensure formatter is set

;; CSV-mode : [built-in]
(use-package csv-mode
  :mode (("\\.csv\\'" . csv-mode)))

;; YAML-mode : [built-in]
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
   ("\\.yaml\\'" . yaml-mode)))

;; Dockerfile-mode : https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :straight (dockerfile-mode :type git :host github :repo "spotify/dockerfile-mode")
  :mode (("Dockerfile\\'" . dockerfile-mode)))

;; Markdown major mode : https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :ensure t
  :straight (markdown-mode :type git :host github :repo "jrblevin/markdown-mode")
  :commands (markdown-mode gfm-view-mode)
  :mode (("README\\.md\\'" . gfm-view-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Groovy major mode : https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
(use-package groovy-mode
  :straight (groovy-mode :type git :host github :repo "Groovy-Emacs-Modes/groovy-emacs-modes")
  :mode (("Jenkinsfile\\'" . groovy-mode)
   ("\\.groovy\\'" . groovy-mode)))

;; Python major mode : https://gitlab.com/python-mode-devs/python-mode
;; (use-package python-mode
;;   :straight (python-mode :type git :host gitlab :repo "python-mode-devs/python-mode")
;;   :mode ("\\.py\\'" . python-mode)
;;   :interpreter ("python" . python-mode))

;; Hashicorp HCL mode : https://github.com/hcl-emacs/hcl-mode
(use-package hcl-mode
  :straight (hcl-mode :type git :host github :repo "hcl-emacs/hcl-mode")
  :mode ("\\.hcl\\'" . hcl-mode))

;; kotlin-mode : https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode
(use-package kotlin-mode
  :straight (kotlin-mode :type git :host github :repo "Emacs-Kotlin-Mode-Maintainers/kotlin-mode")
  :mode ("\\.kt\\'" . kotlin-mode)
  :hook (
      (kotlin-mode . lsp-deferred)
      (kotlin-mode . company-mode)))

;; go-mode.el : https://github.com/dominikh/go-mode.el
(use-package go-mode
  :straight (go-mode :type git :host github :repo "dominikh/go-mode.el")
  :mode ("\\.go\\'" . go-mode)
  :hook ((go-mode . lsp-deferred)))

;; which-key : https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :straight (which-key :type git :host github :repo "justbur/emacs-which-key")
  :config
  (which-key-mode))

;; lsp-mode : https://emacs-lsp.github.io/lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :after asdf
  :diminish (lsp-mode . "LSP")
  :straight (lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode")
  :bind (:map lsp-mode-map)
  :config
  ;; register rust lsp + tramp
  (with-eval-after-load "lsp-rust"
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda ()
			 `(,(or (executable-find
				 (cl-first lsp-rust-analyzer-server-command))
				(lsp-package-path 'rust-analyzer)
				"rust-analyzer")
                           ,@(cl-rest lsp-rust-analyzer-server-args))))
      :remote? t
      :major-modes '(rust-mode rustic-mode)
      :initialization-options 'lsp-rust-analyzer--make-init-options
      :notification-handlers (ht<-alist lsp-rust-notification-handlers)
      :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
      :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
      :after-open-fn (lambda ()
                       (when lsp-rust-analyzer-server-display-inlay-hints
			 (lsp-rust-analyzer-inlay-hints-mode)))
      :ignore-messages nil
      :server-id 'rust-analyzer-remote)))

  (setq lsp-clients-kotlin-server-executable "kotlin-language-server")

  :custom
  (lsp-inlay-hint-enable t)

  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :init)

;; lsp-ui : https://emacs-lsp.github.io/lsp-ui
(use-package lsp-ui
  :commands lsp-ui
  :straight (lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui")
  :hook (lsp-mode . lsp-ui-mode)
  :after lsp-mode)

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

(use-package ruby-mode
  :after (company)
  :straight (:type built-in)
  :config
  (defun mise-enable ()
    "Setup mise for environment by adding its shims directory to exec-path and PATH."
    (interactive)
    (let ((mise-path "/Users/quint.pieters/.local/share/mise/shims"))
      ;; Add to exec-path if not already included
      (unless (member mise-path exec-path)
	(setq exec-path (cons mise-path exec-path)))

      ;; Add to PATH environment variable if not already included
      (unless (string-match-p (regexp-quote mise-path) (getenv "PATH"))
	(setenv "PATH" (concat mise-path ":" (getenv "PATH"))))))

  (defun my-ruby-mode-setup ()
    (setq-local company-backends '((company-capf company-robe company-dabbrev-code))))

  (defun insert-ruby-debug-statement ()
    "Inserts 'require 'pry'; binding.pry' at the current cursor position."
    (interactive)
    (insert "require 'pry'; binding.pry"))

  (define-key ruby-mode-map (kbd "<f19> b") 'insert-ruby-debug-statement)

  (define-key ruby-mode-map (kbd "M-<right>") 'ruby-forward-sexp)
  (define-key ruby-mode-map (kbd "M-<left>") 'ruby-backward-sexp)
  (define-key ruby-mode-map (kbd "M-<up>") 'ruby-beginning-of-block)
  (define-key ruby-mode-map (kbd "M-<down>") 'ruby-end-of-block)

  :hook ((ruby-mode . my-ruby-mode-setup)
	 (ruby-mode . mise-enable)))

;; robe : https://github.com/dgutov/robe
(use-package robe
  :ensure t
  :straight (robe :type git :host github :repo "dgutov/robe")
  :config
  (defun my/start-robe ()
    (interactive)
    (robe-start)
    (company-mode 1))
  :bind (("<f2> <f2>" . my/start-robe)
	 ("<f19> n" . robe-jump)
	 ("<f19> m" . robe-jump-to-module))
  :hook ((ruby-mode . robe-mode)))

;; int-ruby : https://github.com/nonsequitur/inf-ruby
(use-package inf-ruby
  :ensure t
  :after ruby-mode
  :straight (inf-ruby :type git :host github :repo "nonsequitur/inf-ruby")
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'ruby)
  (add-to-list 'tree-sitter-major-mode-language-alist '(ruby-mode . ruby))
  :hook ((ruby-mode . tree-sitter-hl-mode)))

;; smartparens : https://github.com/Fuco1/smartparens
(use-package smartparens-mode
  :ensure smartparens  ;; install the package
  :straight (smartparens :type git :host github :repo "Fuco1/smartparens")
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

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

;; openai : https://github.com/emacs-openai/openai
(use-package openai
  :straight (openai :type git :host github :repo "emacs-openai/openai")
  :config
  (setq org-id "")
  (setq openai-key ""))

;; chatgpt : https://github.com/emacs-openai/chatgpt
(use-package chatgpt
  :straight (chatgpt :type git :host github :repo "emacs-openai/chatgpt")
  :config
  (setq chatgpt-model "gpt-4o"))

;; copilot : https://github.com/copilot-emacs/copilot.el
(use-package copilot
  :ensure t
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :bind (("<f1> <f1>" . copilot-mode)
	 ("<f19> z" . copilot-previous-completion)
	 ("<f19> x" . copilot-next-completion)
	 ("<f19> a" . copilot-accept-completion)))

;; editorconfig : https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :ensure t
  :straight (editorconfig :type git :host github :repo "editorconfig/editorconfig-emacs")
  :config
  (editorconfig-mode 1))

;; rustic : https://github.com/brotzeit/rustic
(use-package rustic
  :after (lsp-mode company)
  :straight (rustic :type git :host github :repo "brotzeit/rustic")
  :config
  (setq exec-path (append exec-path (list (expand-file-name "~/.cargo/bin"))))
  (setq rustic-format-trigger 'on-save)
  (setq rustic-lsp-client 'lsp-mode)
  (defun my-rustic-mode-setup ()
    (setq-local company-backends '((company-capf company-dabbrev-code)))))

;; ================================================================================
;; BEGIN ESHELL
;; ================================================================================

;; capf-autosuggest - https://github.com/emacsmirror/capf-autosuggest
(use-package capf-autosuggest
  :straight (capf-autosuggest :type git :host github :repo "emacsmirror/capf-autosuggest")
  :hook
  (eshell-mode . capf-autosuggest-mode))

;; eshell - build-in
(use-package eshell
  :ensure nil  ; Eshell is built-in, so no need to install
  :init
  ;; Set Eshell directory and history paths
  (setq eshell-directory-name (concat my-emacs-dir "eshell/")
        eshell-history-file-name (concat my-emacs-dir "eshell/history")
        eshell-aliases-file (concat my-emacs-dir "eshell/alias")
        eshell-last-dir-ring-file-name (concat my-emacs-dir "eshell/lastdir")
        eshell-banner-message "")

  ;; Eshell settings
  (setq eshell-highlight-prompt nil
        eshell-buffer-shorthand t
        eshell-cmpl-ignore-case t
        eshell-cmpl-cycle-completions t
        eshell-destroy-buffer-when-process-dies t
        eshell-history-size 10000
        eshell-save-history-on-exit t
        eshell-hist-ignoredups t
        eshell-buffer-maximum-lines 20000
        eshell-error-if-no-glob t
        eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-list-files-after-cd t)

  ;; Visual commands
  (setq eshell-visual-commands '("ranger" "vi" "screen" "top" "less" "more" "lynx"
                                 "ncftp" "pine" "tin" "trn" "elm" "vim"
                                 "nmtui" "alsamixer" "htop" "el" "elinks")
        eshell-visual-subcommands '(("git" "log" "diff" "show")))

  ;; Set the font for icons
  (set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font Mono") nil 'prepend)

  ;; Define macros for prompt customization
  (defmacro with-face (str &rest props)
    "Return STR propertized with PROPS."
    `(propertize ,str 'face (list ,@props)))

  (defmacro esh-section (name icon form &rest props)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,name
           (lambda () (when ,form
                        (-> ,icon
                            (concat esh-section-delim ,form)
                            (with-face ,@props))))))

  ;; Define the accumulator function before it's used
  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))

  ;; Custom function to get the current Git branch name
  (defun get-git-branch-name ()
    "Retrieve the current Git branch name."
    (let ((branch (vc-git--symbolic-ref (eshell/pwd))))
      (when branch
        (string-trim branch))))

  ;; Define sections with nerd-icons
  (esh-section esh-dir
               (nerd-icons-icon-for-dir (eshell/pwd))
               (abbreviate-file-name (eshell/pwd))
               '(:foreground "gold"))

  (esh-section esh-git
               (nerd-icons-devicon "nf-dev-git")
               (get-git-branch-name)
               '(:foreground "pink"))

  (esh-section esh-clock
               (nerd-icons-mdicon "nf-md-clock")
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))

  (esh-section esh-num
               (nerd-icons-mdicon "nf-md-format_list_numbered")
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))

  ;; Separator between esh-sections
  (setq esh-sep "  ")  ; or " | "
  (setq esh-section-delim " ")

  ;; Eshell prompt header
  (setq esh-header "\n")  ; Adds a newline before the prompt

  ;; Eshell prompt regexp and string
  (setq eshell-prompt-regexp "^└─> ")  ; Matches the prompt string
  (setq eshell-prompt-string "└─> ")   ; Sets the prompt arrow

  ;; Choose which eshell-funcs to enable
  (setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))

  ;; Define the eshell prompt function
  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'."
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func)

  :config
  ;; Function to clear Eshell buffer
  (defun eshell-clear-buffer ()
    "Clear terminal."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  ;; Add hook to set key binding for clearing Eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

  ;; Function to open magit-status for the current directory
  (defun eshell/magit ()
    "Function to open magit-status for the current directory."
    (interactive)
    (require 'magit)
    (magit-status-setup-buffer default-directory)
    nil)

  ;; Implement a "prompt number" section
  (setq esh-prompt-num 0)

  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))

  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (cl-incf esh-prompt-num)))))

;; ================================================================================
;; END ESHELL
;; ================================================================================

;; My own custom configuration
(use-package emacs
  :config
  (setq inhibit-startup-message t)

  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room
  (menu-bar-mode -1)            ; Disable the menu bar

  ;; Enable line numbers
  (column-number-mode)
  (global-display-line-numbers-mode t)

  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
		ibuffer-mode
                treemacs-mode-hook
                inf-ruby-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

  ;; Always prefer UTF-8
  (prefer-coding-system 'utf-8-unix)
  (setq x-select-request-type
	'(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; Always delete trailing whitespaces before save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Restore early-init gb collect settings
  (add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

  ;; Set up the visible bell
  (setq visible-bell t)

  ;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Start in full screen
  ; (add-to-list 'default-frame-alist '(fullscreen . fullscreen))

  (keymap-global-set "s-/" 'format-all-region-or-buffer)
  (keymap-global-set "M-/" 'comment-or-uncomment-region)
  (keymap-global-set "<f19> <left>" 'previous-buffer)
  (keymap-global-set "<f19> <right>" 'next-buffer)
  (keymap-global-set "<f19> r" 'kmacro-start-macro)
  (keymap-global-set "<f19> e" 'kmacro-end-macro)
  (keymap-global-set "<f19> SPC" 'kmacro-end-or-call-macro))

(provide 'init)
;;; init.el ends here
