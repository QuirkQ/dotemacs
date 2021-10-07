;;; mode-line --- Summary

;;; Commentary:

; mode-line.el

;;; Code:

(defun custom-modeline-modified
    ((let* ((config-alist
             '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
               ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
               ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
            (result (cdr (assoc (format-mode-line "%*") config-alist))))
       (propertize (apply (cadr result) (cddr result))
                   'face `(:family ,(funcall (car result))))))

  (defun custom-modeline-window-number ()
    (propertize (format " %c" (+ 9311 (window-numbering-get-number)))
		'face `(:height ,(/ (* 0.90 powerline/default-height) 100.0))
		'display '(raise 0.0)))

  (defun custom-modeline-region-info ()
    (when mark-active
      (let ((words (count-lines (region-beginning) (region-end)))
            (chars (count-words (region-end) (region-beginning))))
	(concat
	 (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
                     'face `(:family ,(all-the-icons-octicon-family))
                     'display '(raise -0.0))
	 (propertize (format " (%s, %s)" words chars)
                     'face `(:height 0.9))))))

  (defun -custom-modeline-github-vc ()
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
       " · "
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                   'display '(raise -0.1))
       (propertize (format " %s" branch) 'face `(:height 0.9)))))

  (defun -custom-modeline-svn-vc ()
    (let ((revision (cadr (split-string vc-mode "-"))))
      (concat
       (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
       (propertize (format " · %s" revision) 'face `(:height 0.9)))))

  (defun custom-modeline-icon-vc ()
    (when vc-mode
      (cond
       ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
       ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
       (t (format "%s" vc-mode)))))

(defun custom-modeline-flycheck-status ()
  (let* ((text (pcase flycheck-last-status-change
                (`finished (if flycheck-current-errors
                               (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                              (+ (or .warning 0) (or .error 0)))))
                                 (format "✖ %s Issue%s" count (unless (eq 1 count) "s")))
                             "✔ No Issues"))
                (`running     "⟲ Running")
                (`no-checker  "⚠ No Checker")
                (`not-checked "✖ Disabled")
                (`errored     "⚠ Error")
                (`interrupted "⛔ Interrupted")
                (`suspicious  ""))))
     (propertize text
                 'help-echo "Show Flycheck Errors"
                 'mouse-face '(:box 1)
                 'local-map (make-mode-line-mouse-map
                             'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))))

(defvar powerline/upgrades nil)

(defun powerline/count-upgrades ()
  (let ((buf (current-buffer)))
    (package-list-packages-no-fetch)
    (with-current-buffer "*Packages*"
      (setq powerline/upgrades (length (package-menu--find-upgrades))))
    (switch-to-buffer buf)))
(advice-add 'package-menu-execute :after 'powerline/count-upgrades)

(defun custom-modeline-package-updates ()
  (let ((num (or powerline/upgrades (powerline/count-upgrades))))
    (when (> num 0)
      (propertize
        (concat
         (propertize (format "%s" (all-the-icons-octicon "package"))
                     'face `(:family ,(all-the-icons-octicon-family) :height 1.2)
                     'display '(raise -0.1))
         (propertize (format " %d updates " num)
                     'face `(:height 0.9)))
        'help-echo "Open Packages Menu"
        'mouse-face '(:box 1)
        'local-map (make-mode-line-mouse-map
                    'mouse-1 (lambda () (interactive) (package-list-packages)))))))

(defun custom-modeline-time ()
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
    (concat
     (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
     (propertize (format "%s " icon) 'face `(:height 1.0 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))

(setq mode-line-format '("%e" (:eval 
  (concat
    (custom-modeline-modified)
    (custom-modeline-window-number)
    (custom-modeline-mode-icon)
    (custom-modeline-icon-vc)
    (custom-modeline-region-info)
    (custom-modeline-flycheck-status)
    (custom-modeline-time)))))

(provide 'mode-line)
;;; mode-line.el ends here
