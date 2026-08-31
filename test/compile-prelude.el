;;; compile-prelude.el --- make init.el byte-compilable in isolation  -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by test/check-config.sh before `batch-byte-compile'.
;;
;; `use-package' is built in, so it expands during compilation -- but it
;; aborts parsing any form carrying `:straight', which straight.el would
;; normally have registered.  When parsing aborts, the form's body is never
;; compiled and the check silently passes on broken code.  And `:ensure t'
;; sends use-package to package.el, which hits the network.
;;
;; So: register `:straight' as a keyword that is parsed and discarded, and
;; neuter installation.  Nothing here is loaded by the real configuration.

;;; Code:

(require 'use-package)

;; No package manager, no network.
(setq use-package-ensure-function #'ignore)
(setq package-archives nil)
(setq use-package-always-ensure nil)

;; Accept and ignore `:straight'. The recipe is data for straight.el; the
;; point of this run is to compile the bodies around it.
(add-to-list 'use-package-keywords :straight t)

(defun use-package-normalize/:straight (_name _keyword args)
  "Pass the `:straight' recipe ARGS through untouched."
  args)

(defun use-package-handler/:straight (name _keyword _args rest state)
  "Ignore the `:straight' recipe and keep processing REST for NAME."
  (use-package-process-keywords name rest state))

(provide 'compile-prelude)
;;; compile-prelude.el ends here
