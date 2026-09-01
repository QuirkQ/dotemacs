;;; modeline-env-assertions.el --- assert the modeline's version indicators  -*- lexical-binding: t; -*-

;; Loaded by test/check-runtime.sh with -l, which means AFTER the real
;; init.el has run, inside a real Emacs launched with a deliberately bare
;; PATH.  Both halves of that matter:
;;
;;   * After init, because what is asserted here -- the order of
;;     `after-change-major-mode-hook', which functions upstream left on the
;;     language mode hooks -- does not exist until every package has loaded.
;;     None of it is reachable from `emacs -Q --batch', so none of it belongs
;;     in test/check-config.sh.
;;
;;   * With a bare PATH, because that is the whole bug.  doom-modeline
;;     resolved the interpreter from `hack-local-variables-hook', which
;;     `run-mode-hooks' reaches one step BEFORE `after-change-major-mode-hook'
;;     turns `mise-mode' on -- so the version in the modeline was whatever
;;     the *launch environment* happened to offer, cached per buffer and
;;     never resolved again.  Launched from a terminal that looked fine: an
;;     interactive zsh has run `mise activate', so the inherited PATH already
;;     carried a mise ruby.  Launched from the Dock it read "Ruby 2.6.10" --
;;     /usr/bin/ruby -- on a machine where no project uses that Ruby.  A test
;;     that inherits this shell's PATH cannot tell the two apart.
;;
;; Offline and read-only with respect to 1Password: `my-op-executable' is
;; pointed at nothing before any Ruby buffer opens, so this can never raise a
;; biometric prompt.
;;
;; This file is the executable spec for the indicators.  Change it first.

;;; Code:

(require 'seq)

(defvar modeline-env-report
  (or (getenv "MODELINE_ENV_REPORT")
      (expand-file-name "modeline-env-report" temporary-file-directory))
  "File the report is written to.
Written to a file rather than stdout because stdout is a pty here, and
script(1) interleaves it with Emacs' own screen drawing.")

(defvar modeline-env-failures 0)
(defvar modeline-env-assertions 0)
(defvar modeline-env-lines nil)

(defun modeline-env--say (fmt &rest args)
  "Add one line, FMT formatted with ARGS, to the report."
  (push (apply #'format fmt args) modeline-env-lines))

(defun modeline-env-expect (label expected actual)
  "Assert ACTUAL equals EXPECTED, reporting LABEL when it does not."
  (setq modeline-env-assertions (1+ modeline-env-assertions))
  (unless (equal expected actual)
    (setq modeline-env-failures (1+ modeline-env-failures))
    (modeline-env--say "modeline-env: FAIL %-46s expected %S, got %S"
                       label expected actual)))

(defconst modeline-env-languages
  '((python python-mode-hook python-ts-mode-hook)
    (ruby   ruby-mode-hook ruby-ts-mode-hook enh-ruby-mode-hook)
    (perl   perl-mode-hook)
    (go     go-mode-hook go-ts-mode-hook)
    (elixir elixir-mode-hook elixir-ts-mode-hook)
    (rust   rust-mode-hook rust-ts-mode-hook))
  "Every `doom-modeline-def-env' in doom-modeline-env.el, with its `:hooks'.
Written out here rather than read back off init.el on purpose: a test that
asks the configuration which languages it handles cannot notice the
configuration having forgotten one.")

;;; Structural -- the wiring, independent of any buffer -----------------------

;; doom-modeline's own setup must be off every language mode hook. Left on
;; one, that language's indicator still resolves its interpreter a step
;; before mise, and reports the launch environment forever after.
(dolist (entry modeline-env-languages)
  (let ((setup (intern (format "doom-modeline-env-setup-%s" (car entry)))))
    (dolist (hook (cdr entry))
      (modeline-env-expect
       (format "%s is off %s" setup hook)
       nil
       (and (boundp hook) (memq setup (default-value hook)) t)))))

;; ...and the replacement must run from `after-change-major-mode-hook', later
;; in it than the entry that turns `mise-mode' on. `add-hook' prepends, so
;; "later" is not automatic -- it is what APPEND buys, and it is the entire
;; correctness argument.
(let* ((hook (default-value 'after-change-major-mode-hook))
       (ours (seq-position hook 'my/doom-modeline-setup-env))
       (mise (seq-position hook 'global-mise-mode-enable-in-buffer)))
  (modeline-env-expect "the refresh is on after-change-major-mode-hook"
                       t (integerp ours))
  (modeline-env-expect "mise is on after-change-major-mode-hook"
                       t (integerp mise))
  (modeline-env-expect "the refresh runs after mise"
                       t (and (integerp ours) (integerp mise) (> ours mise))))

;;; Behavioural -- open a real Ruby file --------------------------------------

;; Before any Ruby buffer exists: `my/ruby-start-lsp' runs from
;; `ruby-ts-mode-hook' and would reach for the real CLI.
(setq my-op-executable "/nonexistent/op")

;; mise asks before trusting a config it has not seen. Nothing here should
;; reach that -- the temp directory below is covered by the already-trusted
;; global config -- but a test must never be able to block on a prompt.
(defvar modeline-env-questions nil)
(advice-add 'yes-or-no-p :override
            (lambda (&rest args) (push args modeline-env-questions) t))
(advice-add 'y-or-n-p :override
            (lambda (&rest args) (push args modeline-env-questions) t))

(defconst modeline-env-launch-ruby
  (let ((exec-path (default-value 'exec-path)))
    (executable-find "ruby"))
  "The ruby the launch environment offers -- what the bug used to report.")

(defvar modeline-env-dir (make-temp-file "modeline-env-" t))
(defvar modeline-env-file (expand-file-name "probe.rb" modeline-env-dir))

;; dashboard pulls in recentf; keep the scratch file out of it.
(when (boundp 'recentf-exclude)
  (add-to-list 'recentf-exclude (regexp-quote modeline-env-dir)))

(with-temp-file modeline-env-file (insert "puts \"hi\"\n"))

(let ((buffer (find-file-noselect modeline-env-file))
      buffer-ruby command version status)
  (with-current-buffer buffer
    ;; `sit-for' returns immediately here: stdin is at EOF under script(1),
    ;; so Emacs always believes input is pending. Pump the event loop
    ;; explicitly instead, until the asynchronous `ruby --version' filter has
    ;; replaced the load string.
    (let ((deadline (+ (float-time) 30)))
      (while (and (< (float-time) deadline)
                  (member doom-modeline-env--version
                          (list nil doom-modeline-env-load-string)))
        (accept-process-output nil 0.1)))

    (setq buffer-ruby (executable-find "ruby")
          command doom-modeline-env--command
          version doom-modeline-env--version
          status mise--status)

    (modeline-env-expect "mise-mode is on in the Ruby buffer"
                         t (and (bound-and-true-p mise-mode) t))
    (modeline-env-expect "mise replaced the buffer's exec-path"
                         nil (equal exec-path (default-value 'exec-path))))

  ;; The invariant the bug broke: the indicator must measure the interpreter
  ;; this buffer would actually run, not one resolved before mise spoke.
  (modeline-env-expect "the indicator is the buffer's own ruby"
                       buffer-ruby command)
  (modeline-env-expect "a version was resolved"
                       t (and (stringp version)
                              (string-match-p "\\`[0-9]+\\.[0-9]+" version)
                              t))
  (modeline-env-expect "no prompt was raised" nil modeline-env-questions)

  ;; Only meaningful while the launch environment offers a *different* ruby --
  ;; which is exactly what check-runtime.sh's `env -i' arranges. Reported
  ;; rather than asserted when it does not, so a macOS that finally drops
  ;; /usr/bin/ruby weakens this file loudly instead of silently.
  (if (and modeline-env-launch-ruby
           (not (equal modeline-env-launch-ruby buffer-ruby)))
      (modeline-env-expect "the indicator is not the launch environment's ruby"
                           nil (equal command modeline-env-launch-ruby))
    (modeline-env--say
     "modeline-env: note -- the launch environment offers no ruby distinct \
from mise's (%S); the regression case was not exercised"
     modeline-env-launch-ruby))

  (modeline-env--say "")
  (modeline-env--say "  launch-environment ruby : %S" modeline-env-launch-ruby)
  (modeline-env--say "  buffer ruby             : %S" buffer-ruby)
  (modeline-env--say "  indicator command       : %S" command)
  (modeline-env--say "  indicator version       : %S" version)
  (modeline-env--say "  mise status             : %S" status))

(ignore-errors (delete-directory modeline-env-dir t))

(if (zerop modeline-env-failures)
    (modeline-env--say "modeline-env: ok -- %d assertions" modeline-env-assertions)
  (modeline-env--say "modeline-env: FAILED -- %d of %d assertions"
                     modeline-env-failures modeline-env-assertions))

(with-temp-file modeline-env-report
  (insert (string-join (reverse modeline-env-lines) "\n") "\n"))

(kill-emacs (if (zerop modeline-env-failures) 0 1))

;;; modeline-env-assertions.el ends here
