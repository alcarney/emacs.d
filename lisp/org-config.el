;;; org-config.el --- My Org Mode Config

(defun me/org-mode-setup-faces ()
  "Customise how org documents are displayed."
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(defun me/org-mode-tweaks ()
  "Function that is executed for each org-mode buffer."
  (setq-local fill-column 88)
  (turn-on-auto-fill)

  (org-indent-mode)
  (variable-pitch-mode 1)
  (display-line-numbers-mode 0)

  (me/org-mode-setup-faces))

(use-package org
  :hook (org-mode . me/org-mode-tweaks)
  :config
  (setq org-directory "~/Documents/org"
        org-startup-indented t))

(provide 'org-config)
;;; org-config.el ends here
