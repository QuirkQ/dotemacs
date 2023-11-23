;;; init.el

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

;; magit : https://github.com/magit/magit
(use-package magit
  :ensure t
  :straight (magit :type git :host github :repo "magit/magit")
  :bind ("C-x g" . magit-status))

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

;; asdf.el : https://github.com/tabfugnic/asdf.el
(use-package asdf
  :ensure t
  :straight (asdf :type git :host github :repo "tabfugnic/asdf.el"
                  :fork (:host github
                         :repo "swkkrdswk/asdf.el"))
  :config
  (asdf-enable))

;; auto-complete : https://github.com/auto-complete/auto-complete
(use-package auto-complete
  :ensure t
  :straight (auto-complete :type git :host github :repo "auto-complete/auto-complete")
  :config
  (ac-config-default))

;; Enhanced Ruby Mode : https://github.com/zenspider/Enhanced-Ruby-Mode
(use-package enh-ruby-mode
  :ensure t
  :straight (enh-ruby-mode :type git :host github :repo "zenspider/Enhanced-Ruby-Mode")
  :config
  (defun my/ruby-mode-hook ()
    (add-hook 'before-save-hook 'my/ruby-mode-before-save nil t))

  (defun my/ruby-mode-before-save ()
    (when (eq major-mode 'enh-ruby-mode)
    (indent-region (point-min) (point-max) nil)))
  :init
  (add-hook 'enh-ruby-mode-hook 'auto-complete-mode)
  (add-hook 'enh-ruby-mode-hook 'my/ruby-mode-hook)
  :mode (("\\.rb\\'" . enh-ruby-mode)
         ("Gemfile\\'" . enh-ruby-mode)
         ("Rakefile\\'" . enh-ruby-mode)
         ("\\.rake\\'" . enh-ruby-mode)
         ("\\.ru\\'" . enh-ruby-mode)))

;; robe : https://github.com/dgutov/robe
(use-package robe
  :ensure t
  :after enh-ruby-mode
  :straight (robe :type git :host github :repo "dgutov/robe")
  :init
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-ts-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup))

;; int-ruby : https://github.com/nonsequitur/inf-ruby
(use-package inf-ruby
  :ensure t
  :after enh-ruby-mode
  :straight (inf-ruby :type git :host github :repo "nonsequitur/inf-ruby")
  :config
  (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
  (setq inf-ruby-default-implementation "pry")
  (setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
  (setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")
  :bind
  (:map enh-ruby-mode-map
        ("<f3> <f3>" . ruby-send-buffer))
  :init
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))

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
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :bind 
  (:map yas-minor-mode-map
        ("<tab>" . yas-expand)))

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

(provide 'init)
;;; init.el ends here
