;;; projectile-config.el --- Projectile and other project management stuff.

(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map))
  :config
  (projectile-mode 1)

  (setq projectile-completion-system 'ivy
        projectile-project-search-path '("~/Projects")
        projectile-sort-order 'recently-active))

(use-package ripgrep)

(provide 'projectile-config)
;;; projectile-config.el ends here
