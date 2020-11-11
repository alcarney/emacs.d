;;; org-config.el --- My Org Mode Config
(defun me/org-mode-tweaks ()
  "Function that is executed for each org-mode buffer."
  (setq-local fill-column 88)
  (turn-on-auto-fill))

(use-package org
  :hook (org-mode . me/org-mode-tweaks))

(provide 'org-config)
;;; org-config.el ends here
