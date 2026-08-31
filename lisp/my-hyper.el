;;; my-hyper.el --- Caps-Lock Hyper key layer -*- lexical-binding: t -*-

;;; Commentary:

;; Karabiner-Elements maps Caps Lock to left_shift + control + option +
;; command, and a bare tap to ESC.  With `ns-command-modifier' at its
;; default of `super', Emacs sees that chord as "C-M-S-s-".
;;
;; Two quirks of folding Shift into the chord shape this file:
;;
;; 1. Shift is always held, so case cannot be distinguished at the chord
;;    position -- Hyper-r and Hyper-R are one event, as are Hyper-/ and
;;    Hyper-?.  Keys *after* the chord are typed unmodified and do
;;    distinguish case, which is why "a r" and "a R" can coexist.
;;
;; 2. Depending on how the keyboard layout resolves the keystroke, Emacs
;;    receives either "C-M-S-s-1" (Shift kept as a modifier) or "C-M-s-!"
;;    (Shift folded into the glyph).  Which one you get cannot be
;;    determined without a window system, so `my-hyper-set' binds both.
;;    If a key ever misbehaves, `C-h k' reports the spelling Emacs
;;    actually saw.
;;
;; Note: macOS owns Hyper-3 (Control-Option-Shift-Command-3 copies a
;; screenshot to the clipboard).  Turn it off under System Settings ->
;; Keyboard -> Keyboard Shortcuts -> Screenshots to get `split-window-right'.
;;
;; This file deliberately requires nothing.  It only builds a keymap, so it
;; loads under `emacs -Q --batch' and test/hyper-assertions.el can verify
;; the whole layout without a package tree or a window system.

;;; Code:

(defconst my-hyper-mod "C-M-S-s-"
  "Modifier prefix Emacs sees from the Karabiner Caps-Lock Hyper key.
Modifiers are in the canonical `key-valid-p' order, A-C-H-M-S-s.")

(defconst my-hyper-folded-mod "C-M-s-"
  "Modifier prefix Emacs sees when the layout folds Shift into the glyph.")

(defconst my-hyper-folded-glyphs
  '(("0" . ")") ("1" . "!") ("2" . "@") ("3" . "#")
    ("=" . "+") (";" . ":") ("/" . "?") ("," . "<") ("." . ">"))
  "US-layout glyph produced when Shift is folded into a Hyper chord.")

(defconst my-hyper-ascii-named-keys
  '("RET" "<return>" "SPC" "ESC" "TAB" "<tab>" "DEL" "<backspace>")
  "Named keys that are really ASCII characters.
Shift does not change the character these produce, so Emacs' shift
translation drops the `S-' bit: Hyper-RET arrives as `C-M-s-<return>',
not `C-M-S-s-<return>'.  Genuinely non-character keys -- arrows, function
keys -- do keep `S-'.")

(defun my-hyper-folded-key (key)
  "Return the Shift-folded spelling of KEY, or nil when it has none.
Punctuation and digits come from `my-hyper-folded-glyphs'; a lone
lowercase letter folds to its uppercase form; the ASCII named keys in
`my-hyper-ascii-named-keys' fold to themselves, because Shift leaves
their character alone and Emacs therefore reports no `S-'."
  (or (cdr (assoc key my-hyper-folded-glyphs))
      (and (= (length key) 1)
           (string-match-p "\\`[a-z]\\'" key)
           (upcase key))
      (car (member key my-hyper-ascii-named-keys))))

(defun my-hyper-set (keys command)
  "Bind KEYS under the Hyper key to COMMAND.
KEYS is a space-separated sequence whose FIRST key carries the Hyper
chord and whose remaining keys are typed plain: \"p\" becomes Hyper-p,
\"g s\" becomes Hyper-g followed by s.

Which spelling macOS actually delivers cannot be predicted per key: it
depends on whether Shift reaches the application, whether the layout
substitutes a shifted glyph, and whether Emacs' shift translation drops
the `S-' bit.  Guessing produced dead keys -- Hyper-RET arrived as
`C-M-s-<return>' while only `C-M-S-s-<return>' was bound.  So bind the
whole cross-product of {`my-hyper-mod', `my-hyper-folded-mod'} against
{KEY, its folded spelling}.  Redundant bindings are harmless: they all
name the same COMMAND, and `keymap-global-set' is idempotent."
  (let* ((parts (split-string keys " " t))
         (head (car parts))
         (tail (mapconcat (lambda (k) (concat " " k)) (cdr parts) ""))
         (folded (my-hyper-folded-key head))
         (heads (delete-dups (delq nil (list head folded)))))
    (dolist (mod (list my-hyper-mod my-hyper-folded-mod))
      (dolist (h heads)
        (let ((spelling (concat mod h tail)))
          (when (key-valid-p spelling)
            (keymap-global-set spelling command)))))))

;; === PROJECT & FILE NAVIGATION ===
(my-hyper-set "p" #'project-find-file)
(my-hyper-set "t" #'treemacs)
(my-hyper-set "b" #'ivy-switch-buffer)
(my-hyper-set "k" #'kill-current-buffer)
(my-hyper-set "w" #'save-buffer)
(my-hyper-set "<left>"  #'previous-buffer)
(my-hyper-set "<right>" #'next-buffer)
(my-hyper-set "d" #'counsel-git)
(my-hyper-set "f" #'counsel-git-grep)

;; === GIT OPERATIONS ===
(my-hyper-set "g s" #'magit-status)
(my-hyper-set "g c" #'magit-commit)
(my-hyper-set "g p" #'magit-push)
(my-hyper-set "g l" #'magit-log-all)
(my-hyper-set "g b" #'magit-blame)
(my-hyper-set "g f" #'magit-pull)

;; === AI ASSISTANCE ===
;; Sub-keys are typed without Hyper, so r and R still differ here.
(my-hyper-set "a g" #'aidermacs-run)
(my-hyper-set "a s" #'aidermacs-question-code)
(my-hyper-set "a f" #'aidermacs-add-file)
(my-hyper-set "a b" #'aidermacs-add-current-file)
(my-hyper-set "a r" #'aidermacs-drop-current-file)
(my-hyper-set "a R" #'aidermacs-drop-all-files)
(my-hyper-set "a k" #'aidermacs-exit)

;; === DEVELOPMENT TOOLS ===
;; The old "<f19> R" Ruby group lives here now: Hyper-r is already
;; kmacro-start-macro and Hyper-R cannot be told apart from it.
(my-hyper-set "c c" #'compile)
(my-hyper-set "c r" #'my/ruby-run-tests)
(my-hyper-set "c d" #'docker)
(my-hyper-set "c f" #'my/ruby-format-buffer)
(my-hyper-set "c l" #'flycheck-list-errors)
(my-hyper-set "c t" #'vterm)
(my-hyper-set "c R" #'my/rails-console)
(my-hyper-set "c n" #'xref-find-references)
(my-hyper-set "c h" #'eldoc-doc-buffer)
(my-hyper-set "c a" #'eglot-code-actions)

;; === WINDOW & BUFFER MANAGEMENT ===
(my-hyper-set "o" #'other-window)
(my-hyper-set "1" #'delete-other-windows)
(my-hyper-set "2" #'split-window-below)
(my-hyper-set "3" #'split-window-right)
(my-hyper-set "0" #'delete-window)
(my-hyper-set "=" #'balance-windows)

;; === MACROS & AUTOMATION ===
(my-hyper-set "r"   #'kmacro-start-macro)
(my-hyper-set "e"   #'kmacro-end-macro)
(my-hyper-set "SPC" #'kmacro-end-or-call-macro)
(my-hyper-set "m"   #'kmacro-name-last-macro)

;; === QUICK ACTIONS ===
(my-hyper-set ";" #'comment-or-uncomment-region)
(my-hyper-set "u" #'undo)
(my-hyper-set "/" #'swiper)
(my-hyper-set "i" #'imenu)
(my-hyper-set "j" #'avy-goto-char)
(my-hyper-set "l" #'goto-line)
(my-hyper-set "x" #'execute-extended-command)
(my-hyper-set "q" #'keyboard-quit)
;; Was "<f19> ?", which is the same physical key as "/" once Shift is folded.
(my-hyper-set "h" #'which-key-show-top-level)
(my-hyper-set "ESC" #'keyboard-escape-quit)

;; === LSP NAVIGATION ===
;; Same physical keys as ~/.config/zed/keymap.json, so muscle memory for
;; jump-to-definition and jump-back carries between the two editors.
(my-hyper-set "." #'xref-find-definitions)
(my-hyper-set "," #'xref-go-back)
;; Zed binds ctrl-alt-shift-cmd-enter to GoToDefinition as a keyboard
;; "click"; mirror it so the same reflex works here.
(my-hyper-set "<return>" #'xref-find-definitions)

(provide 'my-hyper)
;;; my-hyper.el ends here
