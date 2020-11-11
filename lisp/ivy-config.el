;;; ivy-config.el --- Ivy & Counsel configuration
(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
	 ("M-x" . counsel-M-x)
	 ("C-c i" . counsel-imenu)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)))

(use-package ivy
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-x C-b" . ivy-switch-buffer-other-window))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(provide 'ivy-config)
;;; ivy-config.el ends here
