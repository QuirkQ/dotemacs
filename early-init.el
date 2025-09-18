;;; early-init.el --- Quint's Early Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:

; Quint his fantastic early-init.el

;;; Code:

;; Load environment variables from .env file
(defun my/load-dotenv (&optional env-file)
  "Load environment variables from ENV-FILE (defaults to .env in user-emacs-directory).
Supports basic KEY=value format, ignoring empty lines and lines starting with #."
  (let ((env-file (or env-file (expand-file-name ".env" user-emacs-directory))))
    (when (file-exists-p env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
            (when (and (not (string-empty-p line))
                       (not (string-prefix-p "#" line))
                       (string-match "^\\([^=]+\\)=\\(.*\\)$" line))
              (let ((key (match-string 1 line))
                    (value (match-string 2 line)))
                ;; Remove surrounding quotes if present
                (when (and (> (length value) 1)
                           (or (and (string-prefix-p "\"" value) (string-suffix-p "\"" value))
                               (and (string-prefix-p "'" value) (string-suffix-p "'" value))))
                  (setq value (substring value 1 -1)))
                (setenv key value))))
          (forward-line 1))))))

;; Load .env file at startup
(my/load-dotenv)

(setq package-enable-at-startup nil) ; Required on Emacs => 27 : straight.el
(setq auto-save-default nil) ; Disable auto-save files (#filename#)
(setq make-backup-files nil) ; Disable backup files (filename~)
(setq explicit-shell-file-name "/bin/zsh")

;; Faster startup
(setq gc-cons-threshold 64000000)

(provide 'early-init)
;;; early-init.el ends here
