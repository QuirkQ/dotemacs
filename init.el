;;; package --- init.el

;;; Commentary:

; Quint his fantastic init.el

;;; Code:
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Start in full screen
(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

;; Always prefer UTF-8
(prefer-coding-system 'utf-8-unix)
(setq x-select-request-type
    '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

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

;; Set up the visible bell
(setq visible-bell t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialise straight.el : https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-protocol 'ssh)
(setq use-package-verbose nil) ;; use 't' to see execution profile at startup

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
         ("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-ag)
	 ("C-c j" . counsel-git-grep)
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
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode t))

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
(use-package python-mode
  :straight (python-mode :type git :host gitlab :repo "python-mode-devs/python-mode")
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Hashicorp HCL mode : https://github.com/hcl-emacs/hcl-mode
(use-package hcl-mode
  :straight (hcl-mode :type git :host github :repo "hcl-emacs/hcl-mode")
  :mode ("\\.hcl\\'" . hcl-mode))

;; asdf.el : https://github.com/tabfugnic/asdf.el
(use-package asdf
  :ensure t
  :straight (asdf :type git :host github :repo "tabfugnic/asdf.el"
                  :fork (:host github
                         :repo "swkkrdswk/asdf.el"))
  :config
  (asdf-enable))

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
  (defun my/ruby-set-gemfile-local ()
    (let ((local-gemfile (concat (lsp-workspace-root) "/Gemfile.local")))
      (when (file-exists-p local-gemfile)
        (setenv "BUNDLE_GEMFILE" local-gemfile))))
  
  ;; register Sorbet
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                            '("bundle" "exec"
							      "srb" "typecheck"
							      "--lsp" "--disable-watchman")))
                    :major-modes '(ruby-mode)
                    :server-id 'sorbet))
  :custom
  (lsp-solargraph-multi-root t)
  (lsp-solargraph-autoformat t)
  (lsp-solargraph-completion t)
  (lsp-solargraph-use-bundler t)
  (lsp-solargraph-library-directories
   '("~/.asdf/"))
  
  :hook ((ruby-mode . my/ruby-set-gemfile-local)
	 (ruby-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :init)

;; lsp-ui : https://emacs-lsp.github.io/lsp-ui
(use-package lsp-ui
  :commands lsp-ui
  :straight (lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui")
  :hook (lsp-mode . lsp-ui-mode)
  :after lsp-mode)

;; eglot : default in Emacs 29
(use-package eglot
  :ensure t
  :after lsp-mode)
  
;; company-mode : https://github.com/company-mode/company-mode
(use-package company
  :ensure t
  :straight (company :type git :host github :repo "company-mode/company-mode")
  :after (lsp-mode eglot)
  :hook ((lsp-mode . company-mode)
         (eglot-managed-mode . company-mode))
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package ruby-mode
  :after (lsp-mode eglot company)
  :straight (:type built-in)
  :config
  (defun my-ruby-mode-setup ()
    (setq-local company-backends '((company-capf company-dabbrev-code company-treesitter))))  
  :hook ((ruby-mode . my-ruby-mode-setup)
	 (ruby-mode . aggressive-indent-mode)
	 (ruby-mode . tree-sitter-hl-mode)
	 (ruby-mode . eglot))
  :custom
  (ruby-insert-encoding-magic-comment nil "Not needed in Ruby 2")
  :ensure-system-package (solargraph . "gem install solargraph"))

;; robe : https://github.com/dgutov/robe
(use-package robe
  :ensure t
  :after eglot
  :straight (robe :type git :host github :repo "dgutov/robe")
  :bind (("<f2> <f2>" . robe-start)
	 ("<f2> n" . robe-jump)
	 ("<f2> m" . robe-jump-to-module))
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-ts-mode-hook 'robe-mode))

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
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'ruby)
  (add-to-list 'tree-sitter-major-mode-language-alist '(ruby-mode . ruby)))

;; smartparens : https://github.com/Fuco1/smartparens
(use-package smartparens-mode
  :ensure smartparens  ;; install the package
  :straight (smartparens :type git :host github :repo "Fuco1/smartparens")
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

;; yasnippet : https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :straight (yasnippet :type git :host github :repo "joaotavora/yasnippet")
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;; yasnippet-snippets : https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t
  :straight (yasnippet-snippets :type git :host github :repo "AndreaCrotti/yasnippet-snippets"))

;; vterm : https://github.com/akermu/emacs-libvterm
(use-package vterm
    :ensure t
    :straight (vterm :type git :host github :repo "akermu/emacs-libvterm")
    :config
    (setq vterm-shell "/opt/homebrew/bin/fish")
    (setq vterm-always-compile-module t))

;; multi-vterm : https://github.com/suonlight/multi-vterm
(use-package multi-vterm
    :ensure t
    :straight (multi-vterm :type git :host github :repo "suonlight/multi-vterm"))

;; eterm-256color : https://github.com/dieggsy/eterm-256color
(use-package eterm-256color
  :ensure t
  :straight (eterm-256color :type git :host github :repo "dieggsy/eterm-256color"))

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
  (setq openai-key ""))

;; chatgpt : https://github.com/emacs-openai/chatgpt
(use-package chatgpt
  :straight (chatgpt :type git :host github :repo "emacs-openai/chatgpt")
  :config
  (setq chatgpt-model "gpt-3.5-turbo"))

;; copilot : https://github.com/copilot-emacs/copilot.el
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t)

;; editorconfig : https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :ensure t
  :straight (editorconfig :type git :host github :repo "editorconfig/editorconfig-emacs")
  :config
  (editorconfig-mode 1))

(provide 'init)
;;; init.el ends here


