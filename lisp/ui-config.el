;;; ui-config.el --- UI Configuration
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(column-number-mode 1)
(size-indication-mode 1)
(show-paren-mode 1)

;; Line numbers
(global-display-line-numbers-mode 1)

(dolist (mode '(doc-view-mode-hook
                eshell-mode-hook
                org-mode-hook
                shell-mode-hook
	        treemacs-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Ubuntu Mono" :height 125)
(set-face-attribute 'fixed-pitch nil :font "Ubuntu Mono" :height 125)
(set-face-attribute 'variable-pitch nil :font "Ubuntu Medium" :height 125)

(setq inhibit-startup-message t)

(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode t)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-buffer-file-name-size 'relative-to-project
        doom-modeline-major-mode-color-icon t))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nord t)

  (doom-themes-visual-bell-config)

  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :config
  (solaire-global-mode 1)
  (solaire-mode-swap-bg))

(use-package treemacs
  :bind (("C-<tab>" . treemacs))
  :config
  (treemacs-git-mode 'extended)
  (add-to-list 'treemacs-pre-file-insert-predicates
               #'treemacs-is-file-git-ignored?))

(provide 'ui-config)
;;; ui-config.el ends here
