;;; early-init.el

;;; Commentary:

; Quint his fantastic early-init.el

;;; Code:

(setq package-enable-at-startup nil) ; Required on Emacs => 27 : straight.el
(setq auto-save-default nil) ; Disable auto-save files (#filename#)
(setq make-backup-files nil) ; Disable backup files (filename~)
(setq explicit-shell-file-name "/opt/homebrew/bin/fish")

(provide 'early-init)
;;; early-init.el ends here