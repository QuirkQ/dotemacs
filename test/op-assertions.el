;;; op-assertions.el --- assert the 1Password reader  -*- lexical-binding: t; -*-

;; Loaded by test/check-config.sh straight after lisp/my-op.el, under
;; emacs -Q --batch.
;;
;; Nothing here touches the real `op'.  Every case runs against a stub script
;; in a temp directory that records how often it was invoked, so this stays
;; offline, side-effect free, and can never raise a biometric prompt.
;;
;; The stub rewrites each {{ op://VAULT/PATH }} to "value:PATH", so asserting
;; that a key came back as "value:<its own path>" proves the whole chain --
;; template order, output order, and the key each line belongs to.
;;
;; This file is the executable spec for the reader.  Change it first.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar my-op-test-failures 0)
(defvar my-op-test-assertions 0)
(defvar my-op-test-messages nil
  "Messages captured during the scenario currently running.")

(defun my-op-expect (label expected actual)
  "Assert ACTUAL equals EXPECTED, reporting LABEL when it does not."
  (setq my-op-test-assertions (1+ my-op-test-assertions))
  (unless (equal expected actual)
    (setq my-op-test-failures (1+ my-op-test-failures))
    (princ (format "op: FAIL %-44s expected %S, got %S\n"
                   label expected actual))))

(defun my-op-expect-message (label regexp)
  "Assert some message captured this scenario matches REGEXP."
  (my-op-expect label t
                (and (seq-some (lambda (m) (string-match-p regexp m))
                               my-op-test-messages)
                     t)))

;;; The stub -----------------------------------------------------------------

(defvar my-op-test-dir (make-temp-file "my-op-test-" t))
(defvar my-op-test-log (expand-file-name "calls" my-op-test-dir))
(defvar my-op-test-delivery (expand-file-name "delivery" my-op-test-dir))
(defvar my-op-test-stub-path (expand-file-name "op" my-op-test-dir))

(defconst my-op-test-stub "\
#!/bin/sh
# Stand-in for `op inject', driven by MY_OP_TEST_* in the environment.
printf 'x' >> \"$MY_OP_TEST_CALLS\"

# Stands in for the one thing about the real CLI the reader has to survive:
# it takes as long as a human takes to answer a Touch ID prompt. Logged
# before the sleep on purpose -- the call-count assertions want to see the
# run the moment it starts, not only once it finishes.
if [ -n \"$MY_OP_TEST_SLEEP\" ]; then
  sleep \"$MY_OP_TEST_SLEEP\"
fi

# op takes a template two ways and only two: `-i FILE', or on a *pipe*. A
# regular file redirected onto fd 0 -- which is precisely what
# `call-process-region' hands a child, since Emacs spools the region to a
# temp file -- is refused with the error reproduced below. That refusal is
# not a detail a stub may paper over: a stub that reads whatever it is given
# passes a reader that cannot talk to the real CLI at all, which is exactly
# how the first version of this file shipped 17 green assertions against a
# call that failed on the first real secret.
in_file=
prev=
for arg in \"$@\"; do
  if [ \"$prev\" = \"-i\" ]; then in_file=$arg; fi
  prev=$arg
done
if [ -n \"$in_file\" ]; then
  printf 'in-file' > \"$MY_OP_TEST_DELIVERY\"
  exec < \"$in_file\"
elif [ -p /dev/fd/0 ]; then
  printf 'pipe' > \"$MY_OP_TEST_DELIVERY\"
else
  printf 'stdin-not-a-pipe' > \"$MY_OP_TEST_DELIVERY\"
  printf '[ERROR] expected data on stdin but none found\\n' >&2
  exit 1
fi

if [ -n \"$MY_OP_TEST_STDERR\" ]; then
  printf '%s\\n' \"$MY_OP_TEST_STDERR\" >&2
fi
if [ -n \"$MY_OP_TEST_FAIL\" ]; then
  exit 1
fi
if [ -n \"$MY_OP_TEST_BANNER\" ]; then
  printf '%s\\n' \"$MY_OP_TEST_BANNER\"
fi
if [ -n \"$MY_OP_TEST_EMPTY\" ]; then
  sed -e 's|{{ op://[^/]*/\\(.*\\) }}|value:\\1|' \\
      -e \"s|^value:$MY_OP_TEST_EMPTY\\$||\"
else
  sed -e 's|{{ op://[^/]*/\\(.*\\) }}|value:\\1|'
fi
"
  "A fake 1Password CLI.  See the commentary above.")

(with-temp-file my-op-test-stub-path (insert my-op-test-stub))
(set-file-modes my-op-test-stub-path #o755)

(setq my-op-executable my-op-test-stub-path)

(defvar my-op-test-base-env
  (append (list (concat "MY_OP_TEST_CALLS=" my-op-test-log)
                (concat "MY_OP_TEST_DELIVERY=" my-op-test-delivery))
          process-environment)
  "`process-environment' with the stub's two logs pointed at temp files.")

(defun my-op-test-calls ()
  "How many times the stub has run since the scenario began."
  (or (file-attribute-size (file-attributes my-op-test-log)) 0))

(defun my-op-test-delivery ()
  "How the template reached the stub on its last run."
  (if (file-exists-p my-op-test-delivery)
      (with-temp-buffer
        (insert-file-contents my-op-test-delivery)
        (buffer-string))
    "nothing ran"))

(defun my-op-test-wait (predicate seconds)
  "Run the event loop until PREDICATE returns non-nil, or SECONDS pass.
The asynchronous path settles from a process sentinel, and under
`--batch' nothing runs sentinels unless something waits for output."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(defmacro my-op-test-scenario (env &rest body)
  "Run BODY with ENV added to the environment, from a cold cache.
Messages are captured into `my-op-test-messages' rather than printed,
so the diagnostics are assertable and the log stays readable."
  (declare (indent 1))
  `(let ((process-environment (append (list ,@env) my-op-test-base-env))
         (my-op-test-messages nil))
     ;; A cold cache means cold: dropping `in-flight' on the floor would
     ;; orphan a running stub whose sentinel then lands mid-scenario.
     (when (eq my-op--cache 'in-flight) (my-op--wait))
     (setq my-op--cache 'untried)
     (when (file-exists-p my-op-test-log) (delete-file my-op-test-log))
     (when (file-exists-p my-op-test-delivery) (delete-file my-op-test-delivery))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) my-op-test-messages))))
       ,@body)))

;;; Cases --------------------------------------------------------------------

;; Every secret comes back as the value of its own reference. Run generically
;; over `my-op-secrets' so a key added later is covered without editing this.
(my-op-test-scenario ()
  (dolist (cell my-op-secrets)
    (my-op-expect (format "%s resolves to its own reference" (car cell))
                  (concat "value:" (cdr cell))
                  (my-op-get (car cell))))

  ;; Stated as the contract rather than as one strategy: either accepted form
  ;; may be used, a third may not. Without this the suite says nothing about
  ;; whether the reader can reach the real CLI -- see the stub's own comment.
  (my-op-expect "op is handed the template in a form it accepts" t
                (and (member (my-op-test-delivery) '("in-file" "pipe")) t))

  ;; The whole point of the file: one authorization prompt per session, not
  ;; one per secret and not one per caller.
  (dotimes (_ 5) (my-op-get 'jfrog-token) (my-op-get 'github-token))
  (my-op-expect "one op invocation for many gets" 1 (my-op-test-calls))

  ;; ... and `my-op-refresh' is the one thing that may pay for a second.
  (my-op-refresh)
  (my-op-expect "my-op-refresh re-reads" 2 (my-op-test-calls))
  (my-op-expect "value survives a refresh"
                "value:jfrog/token" (my-op-get 'jfrog-token)))

;; The reason `my-op-get-async' exists.
;;
;; `op' does not return until a human has answered a Touch ID prompt, and the
;; first caller of the session is `ruby-ts-mode-hook'. Read synchronously
;; from there, `call-process' held the only Lisp thread for the whole prompt:
;; no redisplay, so the frame went blank the moment the 1Password sheet
;; uncovered it, and no keyboard input either, so `C-g' could not get in.
;; Opening a .rb file could only be escaped by force-quitting Emacs.
;;
;; So the hook-side contract is not "fast", it is "does not wait at all".
(my-op-test-scenario ("MY_OP_TEST_SLEEP=2")
  (let ((landed 'pending)
        (started (float-time)))
    (my-op-get-async 'jfrog-token (lambda (value) (setq landed value)))
    (my-op-expect "an async get returns while op is still running" t
                  (< (- (float-time) started) 0.5))
    (my-op-expect "... without having answered yet" 'pending landed)

    ;; Count only once the stub has logged the run it is sitting in. Taken
    ;; any earlier the count reads zero no matter what the reader did --
    ;; nothing waits for the child to be exec'd any more.
    (my-op-test-wait (lambda () (> (my-op-test-calls) 0)) 10)

    ;; The second buffer opened during the prompt must queue behind the run
    ;; already going, or the whole one-prompt-per-session property is lost.
    (my-op-get-async 'github-token #'ignore)
    (my-op-expect "a second async get raises no second prompt" 1
                  (my-op-test-calls))

    (my-op-test-wait (lambda () (not (eq landed 'pending))) 30)
    (my-op-expect "the callback is handed the value"
                  "value:jfrog/token" landed)
    (my-op-expect "one op invocation for the whole flight" 1
                  (my-op-test-calls))))

;; `auth-source' is synchronous by contract, so the Forge path cannot defer
;; and `my-op-get' still blocks. Arriving mid-flight it must join the run
;; already going rather than start -- and prompt for -- one of its own.
(my-op-test-scenario ("MY_OP_TEST_SLEEP=1")
  (my-op-get-async 'jfrog-token #'ignore)
  (my-op-expect "a blocking get joins the in-flight run"
                "value:github/token" (my-op-get 'github-token))
  (my-op-expect "joining costs no second invocation" 1 (my-op-test-calls)))

;; A non-zero exit yields nil, reports what op said, and -- the reason the
;; cache has three states -- is never retried.
(my-op-test-scenario ("MY_OP_TEST_FAIL=1" "MY_OP_TEST_STDERR=vault locked")
  (my-op-expect "failure yields nil" nil (my-op-get 'jfrog-token))
  (my-op-expect-message "failure quotes op's stderr" "vault locked")
  (dotimes (_ 5) (my-op-get 'github-token))
  (my-op-expect "failure is not retried" 1 (my-op-test-calls)))

;; The regression behind commit 7ac5243: an op banner written on a zero exit
;; was concatenated onto the token and resurfaced as an opaque Bundler 401.
;; The markers around the template are what make this survivable.
(my-op-test-scenario ("MY_OP_TEST_BANNER=A new version of op is available")
  (my-op-expect "a banner on stdout does not shift the values"
                "value:jfrog/token" (my-op-get 'jfrog-token))
  (my-op-expect "a banner on stdout does not corrupt the last value"
                "value:github/token" (my-op-get 'github-token)))

;; stdout and stderr must not share a stream.
(my-op-test-scenario ("MY_OP_TEST_STDERR=[deprecation] --account is changing")
  (my-op-expect "stderr never lands on a value"
                "value:jfrog/token" (my-op-get 'jfrog-token)))

;; An empty field is a broken vault item, not a broken session: it must not
;; take the other secrets down with it.
(my-op-test-scenario ((concat "MY_OP_TEST_EMPTY="
                              (alist-get 'github-token my-op-secrets)))
  (my-op-expect "an empty field yields nil" nil (my-op-get 'github-token))
  (my-op-expect "an empty field spares the other secrets"
                "value:jfrog/token" (my-op-get 'jfrog-token))
  (my-op-expect-message "an empty field is reported" "github-token"))

;; No CLI installed: nil, said once, and no attempt to run anything.
(my-op-test-scenario ()
  (let ((my-op-executable (expand-file-name "absent" my-op-test-dir)))
    (my-op-expect "a missing CLI yields nil" nil (my-op-get 'jfrog-token))
    (my-op-expect-message "a missing CLI is reported" "absent")
    (my-op-expect "a missing CLI runs nothing" 0 (my-op-test-calls))))

;;; ---------------------------------------------------------------------------

(delete-directory my-op-test-dir t)

(if (zerop my-op-test-failures)
    (princ (format "op: ok -- %d assertions\n" my-op-test-assertions))
  (princ (format "op: %d failure(s)\n" my-op-test-failures)))

;;; op-assertions.el ends here
