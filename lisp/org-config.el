;;; org-config.el --- My Org Mode Config
(defun me/org-mode-tweaks ()
  "Function that is executed for each org-mode buffer."
  (setq-local fill-column 88)
  (turn-on-auto-fill))

(use-package org
  :hook (org-mode . me/org-mode-tweaks)
  :config
  (setq org-directory "~/Documents/org"
        org-startup-indented t))

(provide 'org-config)
;;; org-config.el ends here
