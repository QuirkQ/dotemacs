;;; my-op.el --- read this session's secrets from 1Password  -*- lexical-binding: t; -*-

;;; Commentary:

;; One 1Password read per Emacs session, for every secret the configuration
;; needs.
;;
;; Two consumers so far: the JFrog Artifactory credential Bundler needs to
;; resolve gems from nedap.jfrog.io, and the GitHub token Forge asks for
;; through `auth-source'.  Both are wired up in init.el.
;;
;; Why one call instead of one per secret.  A GUI Emacs has no shell session,
;; so there is no `op' session token to reuse and *every* invocation can raise
;; its own biometric prompt.  bin/mise-ruby-lsp used to run `op read' on each
;; language-server launch -- one per project, plus one after every server
;; restart -- which meant re-authorizing 1Password on opening what felt like
;; every Ruby file.  ~/.config/zsh/functions/tokens had already reached the
;; same conclusion for the shell and fetches everything through a single
;; `op inject' template: one process, one round trip, one authorization
;; prompt.  This is that, for Emacs.
;;
;; Why the read is asynchronous.  `op' does not return until a human has
;; answered a Touch ID prompt, and the first caller of the session is a mode
;; hook -- opening a .rb file.  Read with `call-process' from there, the wait
;; held Emacs's only Lisp thread for the whole prompt: no redisplay, so the
;; frame went blank as soon as the 1Password sheet uncovered it, and no
;; keyboard input either, so `C-g' could not get in.  The editor could only be
;; force-quit.  `my-op-get-async' is the entry point for anywhere that wait
;; would be unattended; `my-op-get' still blocks, and is for the callers that
;; cannot defer.
;;
;; Nothing here may let a secret escape.  The values never reach a file, a
;; log, a commit or `message'.  Only op:// references are written down --
;; in the 0600 template `my-op--start' hands the CLI.
;;
;; Where the secrets live -- the account, the vault and the item paths --
;; is not hardcoded either.  Those coordinates come from the environment:
;; OP_ACCOUNT, OP_VAULT and one OP_<KEY>_ITEM per entry of
;; `my-op-secrets', which `my/load-dotenv' in early-init.el populates from
;; the gitignored .env next to init.el.  .env.example documents the shape;
;; with none of it set, every read settles as unavailable, exactly like a
;; locked vault.
;;
;; No package depends on this file, so it loads under emacs -Q --batch and
;; test/op-assertions.el can drive it against a stub CLI.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar my-op-executable "/opt/homebrew/bin/op"
  "Absolute path to the 1Password CLI.
Not `executable-find': a GUI Emacs starts with no shell PATH, and the
first call can come from a mode hook.")

(defvar my-op-account (getenv "OP_ACCOUNT")
  "1Password account holding the secrets.
From the environment, populated by `my/load-dotenv' in early-init.el from
the gitignored .env next to init.el -- see .env.example.  Without it every
read settles as unavailable, exactly like a locked vault.")

(defvar my-op-vault (getenv "OP_VAULT")
  "Vault holding the secrets.
A UUID rather than a name so renaming the vault does not silently break
every reference; from the environment like `my-op-account'.")

(defvar my-op-secrets
  (delq nil
        (list (when-let* ((path (getenv "OP_JFROG_ITEM")))
                (cons 'jfrog-token path))
              (when-let* ((path (getenv "OP_GITHUB_ITEM")))
                (cons 'github-token path))))
  "Alist of KEY to its item path below `my-op-vault'.
The paths come from OP_<KEY>_ITEM in the environment, same as the account
and vault above.  Keys are symbols in lowercase; add one by naming an
environment variable OP_<UPPERCASE KEY>_ITEM.

Only paths belong in the environment.  The values they resolve to must
never reach a file, a log, a commit or `message'.")

;; A template the shape of the output makes the reply self-delimiting. That
;; matters because of a bug this configuration has already been bitten by
;; (commit 7ac5243): an `op' banner or deprecation notice printed on a *zero*
;; exit was concatenated onto the token and resurfaced much later as an
;; opaque Bundler 401. Anchoring on the markers means anything op decides to
;; print around our template is discarded instead of shifting every value by
;; a line.
(defconst my-op--begin "-----my-op-begin-----")
(defconst my-op--end "-----my-op-end-----")

(defvar my-op--cache 'untried
  "Memoized result of this session's one `op inject' run.

Four deliberately distinct states: `untried' means `op' has not run yet,
`in-flight' means it is running now, an alist is the secrets, and
`failed' means `op' ran and did not produce them.

`in-flight' is what makes one prompt per session survive an asynchronous
read: everyone who asks while `op' is out queues behind the process
already going instead of starting -- and prompting for -- one of their
own.  Without `failed', a locked 1Password would send every Ruby buffer
and every Forge request back to `op' -- exactly the prompt storm this
file exists to prevent.")

(defvar my-op--process nil
  "The running `op' process while `my-op--cache' is `in-flight'.")

(defvar my-op--stdout nil
  "Buffer collecting the running `op' process's stdout.")

(defvar my-op--stderr nil
  "Buffer collecting the running `op' process's stderr.
Separate from stdout -- see the banner note above `my-op--begin'.  A
buffer rather than the temp file the synchronous version used: `op'
never puts a secret on stderr, and an unfiled buffer cannot outlive the
session on disk.")

(defvar my-op--template-file nil
  "Temp file holding the template the running `op' process is reading.")

(defvar my-op--waiting nil
  "Alist of (KEY . CALLBACK) to answer once the read settles.
Callbacks registered by `my-op-get-async' while `op' is still out.")

(defun my-op--reference (path)
  "Return the op:// secret reference for PATH inside `my-op-vault'."
  (concat "op://" my-op-vault "/" path))

(defun my-op--template ()
  "Return the `op inject' template covering every secret, in key order."
  (mapconcat #'identity
             (append (list my-op--begin)
                     (mapcar (lambda (cell)
                               (format "{{ %s }}" (my-op--reference (cdr cell))))
                             my-op-secrets)
                     (list my-op--end))
             "\n"))

(defun my-op--report-failure (detail)
  "Report a failed `op inject', quoting only what `op' put on stderr.
DETAIL is that stderr, or nil.  No secret ever travels on stderr, so
nothing here can leak one."
  (let ((detail (or detail "")))
    (message "1Password: `op inject' failed%s (unlock 1Password and \
enable its CLI integration, then M-x my-op-refresh)"
             (if (string-empty-p detail) "" (concat " -- " detail)))))

(defun my-op--parse (output)
  "Pair the lines of OUTPUT with the keys of `my-op-secrets'.

Returns an alist, or nil when OUTPUT is not the template we sent -- op
substitutes in place, so the run is only trustworthy if both markers
came back with exactly as many lines between them as there are secrets.

A field that came back empty is reported and left as nil for that key
alone.  A broken vault item should not take the other secrets down with
it: an empty GitHub token must not also cost the Ruby setup its
Bundler credential."
  (let* ((keys (mapcar #'car my-op-secrets))
         (lines (split-string output "\n"))
         (start (seq-position lines my-op--begin))
         (stop (and start (+ start 1 (length keys))))
         (values (and stop
                      (equal (nth stop lines) my-op--end)
                      (seq-subseq lines (1+ start) stop))))
    (when values
      (let ((secrets (seq-mapn (lambda (key value)
                                 ;; `string-trim': a stray space either side
                                 ;; of a credential is another opaque 401.
                                 (let ((value (string-trim value)))
                                   (cons key (and (not (string-empty-p value))
                                                  value))))
                               keys values)))
        (when-let* ((empty (seq-remove #'cdr secrets)))
          (message "1Password: %s came back empty from the %s vault"
                   (mapconcat (lambda (cell) (symbol-name (car cell))) empty ", ")
                   my-op-vault))
        secrets))))

(defun my-op--buffer-text (buffer)
  "Return the contents of BUFFER trimmed, or nil when it is gone."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer (string-trim (buffer-string)))))

(defun my-op--cleanup ()
  "Drop everything the finished run owned.
The template holds op:// references only, never a value, and the two
buffers are killed rather than left around holding one."
  (when my-op--template-file
    (ignore-errors (delete-file my-op--template-file)))
  (when (buffer-live-p my-op--stdout) (kill-buffer my-op--stdout))
  (when (buffer-live-p my-op--stderr) (kill-buffer my-op--stderr))
  (setq my-op--template-file nil
        my-op--stdout nil
        my-op--stderr nil
        my-op--process nil))

(defun my-op--settle (result)
  "Record RESULT as this session's secrets and answer everyone waiting.
RESULT is an alist or `failed'.  Runs from a process sentinel, where a
signal would be swallowed and leave the next caller looking at a cache
still stuck on `in-flight', so every callback is fenced off."
  (setq my-op--cache result)
  (my-op--cleanup)
  (let ((waiting (nreverse my-op--waiting)))
    (setq my-op--waiting nil)
    (dolist (cell waiting)
      (with-demoted-errors "1Password: a secret's callback failed -- %S"
        (funcall (cdr cell) (my-op-get (car cell)))))))

(defun my-op--sentinel (process _event)
  "Parse what PROCESS produced and settle the session's secrets."
  (when (memq (process-status process) '(exit signal))
    (let* ((ok (and (eq (process-status process) 'exit)
                    (eq 0 (process-exit-status process))))
           (output (and ok (my-op--buffer-text my-op--stdout)))
           (secrets (and output (my-op--parse output))))
      (unless ok
        (my-op--report-failure (my-op--buffer-text my-op--stderr)))
      (my-op--settle (or secrets 'failed)))))

(defun my-op--start ()
  "Start the session's one `op inject' run, unless one has already run.
Never signals: the first call can come from a mode hook, where a signal
would abort the rest of the hook -- `eglot-ensure' included."
  (cond
   ((not (eq my-op--cache 'untried)) nil)
   ((or (not my-op-account) (not my-op-vault) (null my-op-secrets))
    (message "1Password: OP_ACCOUNT/OP_VAULT/OP_*_ITEM unset -- copy \
.env.example to .env (it is gitignored), then M-x my-op-refresh")
    (my-op--settle 'failed))
   ((not (file-executable-p my-op-executable))
    (message "1Password: no CLI at %s (brew install --cask 1password-cli)"
             my-op-executable)
    (my-op--settle 'failed))
   (t
    (setq my-op--cache 'in-flight)
    (condition-case err
        (progn
          ;; `-i FILE' rather than stdin, which is how the shell does it.
          ;; `op inject' takes a template on a *pipe* or from `-i'; give it a
          ;; regular file on fd 0 and it answers "expected data on stdin but
          ;; none found".
          ;;
          ;; The file holds op:// references only, never a value, and
          ;; `make-temp-file' creates it 0600.
          (setq my-op--template-file (make-temp-file "my-op-template-")
                my-op--stdout (generate-new-buffer " *my-op-stdout*" t)
                my-op--stderr (generate-new-buffer " *my-op-stderr*" t))
          (write-region (my-op--template) nil my-op--template-file nil 'silent)
          (setq my-op--process
                (make-process
                 :name "my-op"
                 :buffer my-op--stdout
                 :stderr my-op--stderr
                 :noquery t
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :command (list my-op-executable
                                "inject" "--account" my-op-account
                                "-i" my-op--template-file)
                 :sentinel #'my-op--sentinel))
          ;; `:stderr' makes Emacs create a pipe process of its own, and its
          ;; default sentinel writes "Process ... finished" into the very
          ;; buffer the failure message quotes. Silence it, and make sure it
          ;; cannot hold up exiting Emacs either.
          (when-let* ((pipe (get-buffer-process my-op--stderr)))
            (set-process-query-on-exit-flag pipe nil)
            (set-process-sentinel pipe #'ignore)))
      (error
       (message "1Password: could not run `op' -- %s"
                (error-message-string err))
       (my-op--settle 'failed))))))

(defun my-op--wait ()
  "Block until the in-flight read settles.
Only for callers that cannot defer -- `auth-source' is synchronous by
contract.  Unlike the `call-process' this replaced, the wait is inside
`accept-process-output', which keeps servicing input, so `C-g' still
works and the frame is not left unpainted.

Re-entrancy is what `in-flight' is for.  `accept-process-output' runs
timers and other buffers' hooks, so a second `my-op-get' can well happen
inside this loop -- and lands right back here, on the same process,
rather than raising a second authorization prompt."
  (let ((process my-op--process))
    (while (and (eq my-op--cache 'in-flight) (process-live-p process))
      (accept-process-output process 0.1))
    ;; The process can be gone while its sentinel is still queued.
    (let ((deadline (+ (float-time) 2)))
      (while (and (eq my-op--cache 'in-flight) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    ;; Belt and braces: a cache stuck on `in-flight' would wedge every later
    ;; caller in this same loop for good.
    (when (eq my-op--cache 'in-flight)
      (my-op--settle 'failed))))

(defun my-op--load ()
  "Return the session's secrets, running `op' at most once.
Blocks when the read is still out.  See `my-op-get-async' for the
callers that must not."
  (when (eq my-op--cache 'untried) (my-op--start))
  (when (eq my-op--cache 'in-flight) (my-op--wait))
  my-op--cache)

(defun my-op-get (key)
  "Return the secret KEY names in `my-op-secrets', or nil.

Blocks until 1Password answers, which is as long as the user takes over
a Touch ID prompt.  Only for callers that cannot defer -- the
`auth-source' backend Forge goes through, and interactive commands,
where the wait is asked for and the user is sitting in front of it.
Anywhere unattended, and on a mode hook above all, use
`my-op-get-async'.

Nil covers every way this can come up short -- no CLI, a locked vault, a
renamed field -- because every caller is somewhere a signal would do
more damage than a missing credential.  The read happens on the first
call of the session and is not repeated, so callers should ask only when
they actually need the secret."
  (let ((secrets (my-op--load)))
    (and (consp secrets) (alist-get key secrets))))

(defun my-op-get-async (key callback)
  "Call CALLBACK with the secret KEY names, without ever blocking.

CALLBACK is passed the value, or nil for every way the read can come up
short.  It runs on the spot when the secrets are already in hand, and
otherwise from the process sentinel once `op' has answered -- which is
as long as the user takes over a Touch ID prompt, and is exactly the
wait that must not happen on a mode hook.  Nothing about the buffer,
window or point CALLBACK runs in is promised, so a callback that means
to touch a buffer has to capture and check it for itself.

Callers arriving while `op' is still out are queued behind that one
process, so a second Ruby buffer costs no second authorization prompt."
  (if (memq my-op--cache '(untried in-flight))
      (progn
        ;; Registered before starting: `my-op--start' can settle on the spot
        ;; -- no CLI installed -- and CALLBACK has to be there to hear it.
        (push (cons key callback) my-op--waiting)
        (my-op--start))
    (funcall callback (my-op-get key))))

(defun my-op-refresh ()
  "Forget the cached secrets and read them from 1Password again.
For after a token rotates, or after unlocking 1Password following a
failed read.  Buffers already carrying an old value pick the new one up
when their language server next restarts."
  (interactive)
  (when (eq my-op--cache 'in-flight) (my-op--wait))
  (setq my-op--cache 'untried)
  (message (if (consp (my-op--load))
               "1Password: secrets refreshed"
             "1Password: secrets still unavailable")))

(provide 'my-op)
;;; my-op.el ends here
