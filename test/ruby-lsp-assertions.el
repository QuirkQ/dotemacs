;;; ruby-lsp-assertions.el --- assert opening a Ruby file never blocks  -*- lexical-binding: t; -*-

;; Loaded by test/check-runtime.sh with -l, which means AFTER the real
;; init.el has run.  What it asserts is the interaction between three things
;; that only exist once every package has loaded -- `ruby-ts-mode-hook',
;; lisp/my-op.el and eglot -- so none of it is reachable from
;; `emacs -Q --batch' and none of it belongs in test/check-config.sh.
;;
;; The bug.  `ruby-ts-mode-hook' read the JFrog Bundler credential out of
;; 1Password with `call-process'.  `op' does not answer until a human has
;; answered a Touch ID prompt, and `call-process' holds Emacs's only Lisp
;; thread for the whole of it: no redisplay, so the frame went blank the
;; moment the 1Password sheet uncovered it, and no keyboard input either, so
;; `C-g' could not get in.  Opening a .rb file could only be escaped by
;; force-quitting Emacs.
;;
;; So the assertion is not "the hook is fast".  It is that the hook returns
;; while `op' is *still out* -- proved by the reader's own `in-flight' state,
;; which no amount of clock noise can fake -- and that ruby-lsp is started
;; afterwards, from the callback, with the credential in hand.
;;
;; Offline and read-only with respect to 1Password: `my-op-executable' is
;; pointed at a stub before any Ruby buffer opens, so this can never raise a
;; biometric prompt.  The stub sleeps, which is the only thing about the real
;; CLI that matters here.
;;
;; This file is the executable spec for that.  Change it first.

;;; Code:

(require 'seq)

(defvar ruby-lsp-report
  (or (getenv "RUBY_LSP_REPORT")
      (expand-file-name "ruby-lsp-report" temporary-file-directory))
  "File the report is written to.
Written to a file rather than stdout because stdout is a pty here, and
script(1) interleaves it with Emacs' own screen drawing.")

(defvar ruby-lsp-failures 0)
(defvar ruby-lsp-assertions 0)
(defvar ruby-lsp-lines nil)

(defun ruby-lsp--say (fmt &rest args)
  "Add one line, FMT formatted with ARGS, to the report."
  (push (apply #'format fmt args) ruby-lsp-lines))

(defun ruby-lsp-expect (label expected actual)
  "Assert ACTUAL equals EXPECTED, reporting LABEL when it does not."
  (setq ruby-lsp-assertions (1+ ruby-lsp-assertions))
  (unless (equal expected actual)
    (setq ruby-lsp-failures (1+ ruby-lsp-failures))
    (ruby-lsp--say "ruby-lsp: FAIL %-48s expected %S, got %S"
                   label expected actual)))

(defun ruby-lsp-pump (predicate seconds)
  "Run the event loop until PREDICATE is non-nil, or SECONDS pass.
`sit-for' returns immediately here -- stdin is at EOF under script(1),
so Emacs always believes input is pending -- so the loop has to wait on
process output explicitly."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))))

;;; A stub 1Password CLI ------------------------------------------------------

(defconst ruby-lsp-op-sleep 5
  "Seconds the stub `op' takes to answer.
Stands in for the one property of the real CLI this file is about: it
does not return until a human has answered a Touch ID prompt.")

(defvar ruby-lsp-dir (make-temp-file "ruby-lsp-" t))
(defvar ruby-lsp-op (expand-file-name "op" ruby-lsp-dir))

(with-temp-file ruby-lsp-op
  (insert (format "\
#!/bin/sh
# Stand-in for `op inject'. Slow on purpose -- see ruby-lsp-op-sleep.
sleep %d
in_file=
prev=
for arg in \"$@\"; do
  if [ \"$prev\" = \"-i\" ]; then in_file=$arg; fi
  prev=$arg
done
if [ -n \"$in_file\" ]; then exec < \"$in_file\"; fi
sed -e 's|{{ op://[^/]*/\\(.*\\) }}|value:\\1|'
" ruby-lsp-op-sleep)))
(set-file-modes ruby-lsp-op #o755)

(setq my-op-executable ruby-lsp-op)

;; Nothing in init.el may have spent the session's one read before we get
;; here, or the whole scenario is a no-op against a warm cache.
(ruby-lsp-expect "the session's secrets are still unread"
                 'untried my-op--cache)

;;; A project that needs the credential ---------------------------------------

(defvar ruby-lsp-project (expand-file-name "project" ruby-lsp-dir))
(defvar ruby-lsp-file (expand-file-name "probe.rb" ruby-lsp-project))

(make-directory ruby-lsp-project t)
(with-temp-file (expand-file-name "Gemfile" ruby-lsp-project)
  (insert "source \"https://nedap.jfrog.io/artifactory/api/gems/gems/\"\n"
          "gem \"rails\"\n"))
(with-temp-file ruby-lsp-file (insert "puts \"hi\"\n"))

;; dashboard pulls in recentf; keep the scratch file out of it.
(when (boundp 'recentf-exclude)
  (add-to-list 'recentf-exclude (regexp-quote ruby-lsp-dir)))

;; mise asks before trusting a config it has not seen. A test must never be
;; able to block on a prompt.
(defvar ruby-lsp-questions nil)
(advice-add 'yes-or-no-p :override
            (lambda (&rest args) (push args ruby-lsp-questions) t))
(advice-add 'y-or-n-p :override
            (lambda (&rest args) (push args ruby-lsp-questions) t))

;;; The connect budget --------------------------------------------------------

;; Before the advice below, and on purpose: `eglot-ensure' is the autoload
;; that pulls eglot in, so overriding it first would leave eglot -- and the
;; `use-package' `:config' block that configures it -- never loaded at all.
(require 'eglot)

;; Not behaviour this file can drive: exceeding the budget takes a real
;; bundler run against a real Artifactory. But the number is load-bearing
;; and reverts to eglot's 30 the moment the `setq' is dropped. ruby-lsp
;; holds `initialize' open for the whole of its composed-bundle setup --
;; measured at 15 seconds in epoxy with the bundle already up to date, and
;; minutes when it has to resolve gems -- and overrunning the budget does
;; not merely give up, it SIGKILLs bundler mid-install.
(ruby-lsp-expect "the connect budget covers a bundler run"
                 t (>= eglot-connect-timeout 120))

;; Record the server start instead of making one. What is under test is
;; *when* eglot is asked to connect, not ruby-lsp itself -- and a real
;; language server would take a bundle, a mise ruby and a network besides.
(defvar ruby-lsp-ensured nil
  "Buffers `eglot-ensure' was called in, newest first.")
(advice-add 'eglot-ensure :override
            (lambda (&rest _) (push (current-buffer) ruby-lsp-ensured)))

;;; The assertions ------------------------------------------------------------

(let* ((started (float-time))
       (buffer (find-file-noselect ruby-lsp-file))
       (elapsed (- (float-time) started)))

  ;; The heart of it. `op' is still out -- the reader says so itself, which
  ;; is a fact about the run rather than about the clock -- and yet
  ;; `find-file-noselect' has already returned. Under the synchronous reader
  ;; this line was not reached until the stub's sleep was over.
  (ruby-lsp-expect "op is still out when the mode hook has returned"
                   'in-flight my-op--cache)
  (ruby-lsp-expect "opening the file did not wait for op"
                   t (< elapsed ruby-lsp-op-sleep))
  (ruby-lsp-expect "the server is not started before the credential"
                   nil ruby-lsp-ensured)

  ;; Emacs is live while the prompt is up: the event loop runs, which is the
  ;; thing `call-process' took away.
  (ruby-lsp-pump (lambda () (consp my-op--cache)) (* 4 ruby-lsp-op-sleep))
  (ruby-lsp-expect "the read settles" t (consp my-op--cache))

  (with-current-buffer buffer
    (ruby-lsp-expect "the buffer is in a Ruby mode"
                     t (and (derived-mode-p 'ruby-ts-mode 'ruby-mode) t))
    (ruby-lsp-expect "the credential reached the buffer's environment"
                      (concat "value:" (alist-get 'jfrog-token my-op-secrets))
                      (getenv my/jfrog-bundle-variable))
    ;; Consed on, never assigned over: mise's PATH has to survive it.
    (ruby-lsp-expect "mise still owns the buffer's exec-path"
                     nil (equal exec-path (default-value 'exec-path))))

  (ruby-lsp-expect "the server is started once the credential is in hand"
                   (list buffer) ruby-lsp-ensured)
  (ruby-lsp-expect "no prompt was raised" nil ruby-lsp-questions)

  (ruby-lsp--say "")
  (ruby-lsp--say "  stub op sleep       : %ds" ruby-lsp-op-sleep)
  (ruby-lsp--say "  find-file returned  : %.2fs" elapsed)
  (ruby-lsp--say "  eglot-ensure calls  : %d" (length ruby-lsp-ensured)))

(ignore-errors (delete-directory ruby-lsp-dir t))

(if (zerop ruby-lsp-failures)
    (ruby-lsp--say "ruby-lsp: ok -- %d assertions" ruby-lsp-assertions)
  (ruby-lsp--say "ruby-lsp: FAILED -- %d of %d assertions"
                 ruby-lsp-failures ruby-lsp-assertions))

(with-temp-file ruby-lsp-report
  (insert (string-join (reverse ruby-lsp-lines) "\n") "\n"))

(kill-emacs (if (zerop ruby-lsp-failures) 0 1))

;;; ruby-lsp-assertions.el ends here
