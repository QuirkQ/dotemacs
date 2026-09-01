;;; hyper-assertions.el --- assert the Hyper layer  -*- lexical-binding: t; -*-

;; Loaded by test/check-config.sh straight after lisp/my-hyper.el, under
;; emacs -Q --batch.  my-hyper.el binds into the global map at load time, so
;; by the time this runs the layer is live and can simply be looked up.
;;
;; This file is the executable spec for the layout.  Change it first.

;;; Code:

(defvar my-hyper-test-failures 0)

(defun my-hyper-expect (key command)
  "Assert that KEY is bound to COMMAND in the global map."
  (let ((actual (keymap-lookup (current-global-map) key)))
    (unless (eq actual command)
      (setq my-hyper-test-failures (1+ my-hyper-test-failures))
      (princ (format "hyper: FAIL %-28s expected %s, got %s\n"
                     key command actual)))))

(defconst my-hyper-expected
  ;; (KEY-AS-TYPED . COMMAND) -- the canonical C-M-S-s- spelling.
  '(;; project & file navigation
    ("p"       . project-find-file)
    ("t"       . treemacs)
    ("b"       . ivy-switch-buffer)
    ("k"       . kill-current-buffer)
    ("w"       . save-buffer)
    ("<left>"  . previous-buffer)
    ("<right>" . next-buffer)
    ("d"       . counsel-git)
    ("f"       . counsel-git-grep)
    ;; git
    ("g s" . magit-status)
    ("g c" . magit-commit)
    ("g p" . magit-push)
    ("g l" . magit-log-all)
    ("g b" . magit-blame)
    ("g f" . magit-pull)
    ;; aider
    ("a g" . aidermacs-run)
    ("a s" . aidermacs-question-code)
    ("a f" . aidermacs-add-file)
    ("a b" . aidermacs-add-current-file)
    ("a r" . aidermacs-drop-current-file)
    ("a R" . aidermacs-drop-all-files)
    ("a k" . aidermacs-exit)
    ;; development tools
    ("c c" . compile)
    ("c r" . my/ruby-run-tests)
    ("c d" . docker)
    ("c f" . my/ruby-format-buffer)
    ("c l" . flycheck-list-errors)
    ("c t" . ghostel)
    ("c R" . my/rails-console)
    ("c n" . xref-find-references)
    ("c h" . eldoc-doc-buffer)
    ("c a" . eglot-code-actions)
    ;; window & buffer management
    ("o" . other-window)
    ("1" . delete-other-windows)
    ("2" . split-window-below)
    ("3" . split-window-right)
    ("0" . delete-window)
    ("=" . balance-windows)
    ;; macros
    ("r"   . kmacro-start-macro)
    ("e"   . kmacro-end-macro)
    ("SPC" . kmacro-end-or-call-macro)
    ("m"   . kmacro-name-last-macro)
    ;; quick actions
    (";"   . comment-or-uncomment-region)
    ("u"   . undo)
    ("/"   . swiper)
    ("i"   . imenu)
    ("j"   . avy-goto-char)
    ("l"   . goto-line)
    ("x"   . execute-extended-command)
    ("q"   . keyboard-quit)
    ("h"   . which-key-show-top-level)
    ("ESC" . keyboard-escape-quit)
    ;; LSP navigation -- same physical keys as the Zed keymap
    ("." . xref-find-definitions)
    ("," . xref-go-back))
  "The layout, as the canonical Hyper spelling.")

;; Every entry must resolve under the canonical prefix.
(dolist (entry my-hyper-expected)
  (let* ((parts (split-string (car entry) " " t))
         (key (concat my-hyper-mod (car parts)
                      (mapconcat (lambda (k) (concat " " k)) (cdr parts) ""))))
    (my-hyper-expect key (cdr entry))))

;; And the Shift-folded spelling must resolve too, wherever one exists --
;; this is the half we cannot verify by pressing keys headlessly.
(dolist (entry my-hyper-expected)
  (let* ((parts (split-string (car entry) " " t))
         (folded (my-hyper-folded-key (car parts))))
    (when folded
      (my-hyper-expect
       (concat my-hyper-folded-mod folded
               (mapconcat (lambda (k) (concat " " k)) (cdr parts) ""))
       (cdr entry)))))

;; No two entries may claim the same physical key. Under Hyper, Shift is
;; always held, so "r"/"R" and "/"/"?" are indistinguishable at the chord
;; position -- this catches a layout regression that would silently shadow
;; a binding.
(let ((seen (make-hash-table :test 'equal)))
  (dolist (entry my-hyper-expected)
    (let* ((parts (split-string (car entry) " " t))
           (head (car parts))
           (physical (concat (downcase head)
                             (mapconcat (lambda (k) (concat " " k)) (cdr parts) ""))))
      (when-let* ((prev (gethash physical seen)))
        (setq my-hyper-test-failures (1+ my-hyper-test-failures))
        (princ (format "hyper: FAIL %s and %s share one physical key\n"
                       prev (car entry))))
      (puthash physical (car entry) seen))))

(if (zerop my-hyper-test-failures)
    (princ (format "hyper: ok -- %d bindings asserted\n"
                   (length my-hyper-expected)))
  (princ (format "hyper: %d failure(s)\n" my-hyper-test-failures)))

;;; hyper-assertions.el ends here
