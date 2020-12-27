;;; md-config.el --- Config for editing markdown files

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :hook (gfm-mode . (lambda ()
                      (setq fill-column 80)
                      (turn-on-auto-fill))))

(provide 'md-config)
;;; md-config.el ends here
