;;; git-config.el --- Git configuration

(use-package git-gutter
  :config
  (global-git-gutter-mode 1)

  (set-face-foreground 'git-gutter:added "forest green")
  (set-face-foreground 'git-gutter:modified "goldenrod")
  (set-face-foreground 'git-gutter:deleted "brown")

  (setq git-gutter:added-sign "▐"
        git-gutter:modified-sign "▐"
        git-gutter:removed-sign "▐"))

(use-package magit
  :bind (("C-x g" . magit-status)))

(provide 'git-config)
;;; git-config.el ends here
