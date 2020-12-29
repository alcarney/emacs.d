;;; lang-c.el --- C Programming Config

(use-package cc-mode
  :bind (:map c-mode-map
         ("C-c d" . gdb)
         ("C-c g" . recompile))
  :config
  (setq-default c-basic-offset 4))

(provide 'lang-c)
;;; lang-c.el --- ends here
