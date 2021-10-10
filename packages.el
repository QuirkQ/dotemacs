;;; packages --- Summary

;;; Commentary:

; packages.el

;;; Code:

(require 'msb)

(use-package counsel
  :after ivy
  :ensure t
  :config (counsel-mode)
  :bind (("M-x" . counsel-M-x)))

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)
	 ("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
	:ensure t
	:init
  (ivy-rich-mode 1)
	(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
	:config
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
					(:columns ((ivy-rich-switch-buffer-icon (:width 2))
										 (ivy-rich-candidate (:width 30))
										 (ivy-rich-switch-buffer-size (:width 7))
										 (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
										 (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
										 (ivy-rich-switch-buffer-project (:width 15 :face success))
										 (ivy-rich-switch-buffer-path
                      (:width (lambda (x)
																(ivy-rich-switch-buffer-shorten-path
																 x
																 (ivy-rich-minibuffer-width
																	0.3))))))
                    :predicate (lambda (cand)
														     (get-buffer cand)))
					counsel-find-file
					(:columns ((ivy-read-file-transformer)
										 (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
					counsel-M-x
					(:columns ((counsel-M-x-transformer (:width 40))
										 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))) ; return docstring of the command
					counsel-recentf
					(:columns ((ivy-rich-candidate (:width 0.8))
										 (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))) ; return last modified time of the file
					counsel-describe-function
					(:columns
					 ((counsel-describe-function-transformer (:width 40))
						(ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))) ; return docstring of the function
					counsel-describe-variable
					(:columns
					 ((counsel-describe-variable-transformer (:width 40))
						(ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))) ; return docstring of the variable
					)))

(use-package swiper
  :after ivy
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package paren
  :ensure nil
  :config (show-paren-mode t))

;; CSV-mode major mode for manipulating CSV files
;; https://elpa.gnu.org/packages/csv-mode.html
(use-package csv-mode
  :mode (("\\.csv\\'" . csv-mode)))

;; An emacs mode for handling Dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :defer t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

;; Markdown major mode
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Groovy major mode
;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
(use-package groovy-mode
  :defer t
  :mode (("Jenkinsfile\\'" . groovy-mode)
	 ("\\.groovy\\'" . groovy-mode)))

;; Magit - a GIT porcelain in Emacs
;; https://magit.vc/
(use-package magit
  :defer t
  :if (executable-find "git")
  :bind (("C-x g" . 'magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;; Magit-gitflow - GitFlow plugin for magit.el
;; https://github.com/jtatarik/magit-gitflow
(use-package magit-gitflow
  :after magit
  :diminish
  :hook (magit-mode . turn-on-magit-gitflow))

;; Magit-todos
;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-nice nil)
  (magit-todos-mode t))

;; git-gutter-fringe - Display git status in fringe
;; https://github.com/syohex/emacs-git-gutter-fringe
(use-package git-gutter-fringe
  :after magit
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t))

;; Yasnippet - Template system for Emacs
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :after lsp-mode)

;; Flycheck - Syntax checking for GNU Emacs
;; https://www.flycheck.org
(use-package flycheck
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode t))

;; Flycheck-pos-tip - Flycheck errors display in tooltip
;; https://github.com/flycheck/flycheck-pos-tip
(use-package flycheck-pos-tip
  :after flycheck
  :config
  (flycheck-pos-tip-mode t))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-width                           35
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("<f12>"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; company - Modular in-buffer completion framework
;; http://company-mode.github.io
(use-package company
  :defer nil
  :diminish
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.1
	company-selection-wrap-around t)
  :bind (:map company-active-map
	      ("<tab>" . company-complete)))

(use-package auto-complete)

(use-package ibuffer
  :bind (("C-x C-b" . 'ibuffer))
  :init
  (setq ibuffer-formats
	'((mark modified read-only locked
		" " (name 30 30 :left :elide)
		" " (size 9 -1 :right)
		" " (mode 16 16 :left :elide) " " filename-and-process)
	  (mark " " (name 16 -1) " " filename))))

(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

(use-package treemacs-all-the-icons
  :after treemacs)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-xcode t)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

; Magik-mode Major mode for Magik files
; https://github.com/roadrunner1776/magik
(use-package magik-mode
  :after (flycheck msb)
  :hook (magik-mode . (lambda ()
			(add-hook 'before-save-hook (lambda ()
						      (krn/header-file-name-update)
						      (krn/indent-buffer))
				  nil 'local)))
  :config
  (setq magik-lint-jar-file (expand-file-name "magik-lint/magik-lint.jar" user-emacs-directory))
  (magik-menu-set-menus)
  (magik-global-bindings))

;; iflipb - navigate through buffers
;; https://www.emacswiki.org/emacs/iflipb
(use-package iflipb
  :bind (("<C-tab>" . 'iflipb-next-buffer)
	 ("<C-S-tab>" . 'iflipb-previous-buffer)
	 ("M-o" . 'other-window)))

;; enhanced ruby mode
;; https://github.com/zenspider/enhanced-ruby-mode
(use-package enh-ruby-mode)
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)

(use-package rainbow-mode)

;; robe mode
;; https://github.com/dgutov/robe
(use-package robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'yard-mode)

;; flyspell
;; requires brew install ispell on OSX
(require 'flyspell)
(setq flyspell-issue-message-flg nil)
(add-hook 'enh-ruby-mode-hook
          (lambda () (flyspell-prog-mode)))

(add-hook 'web-mode-hook
          (lambda () (flyspell-prog-mode)))
;; flyspell mode breaks auto-complete mode without this.
(ac-flyspell-workaround)

;; multiple cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors)

(unless (package-installed-p 'inf-ruby)
  (package-install 'inf-ruby))
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

(use-package smartparens)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'enh-ruby-mode #'smartparens-mode)

(use-package yasnippet-snippets)

(provide 'packages)
;;; packages.el ends here
