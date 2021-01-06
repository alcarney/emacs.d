;;; lang-c.el --- C Programming Config

(defun me/start-debugging ()
  "Start a debugging session in a new tab"
  (interactive)

  (let ((program (read-string "Debug program: ")))
    (tab-new)
    (setq gdb-many-windows t)
    (gdb (format "gdb -i=mi %s" program))))

;; This complements the function above, where we listen for the debugger
;; exiting and we close the tab.
;;
;; I don't fully understand what's going here, the following was adapted
;; from this blogpost
;; https://www.doof.me.uk/2019/06/09/making-emacs-gud-usable/
(advice-add 'gud-sentinel :after
            (lambda (proc msg)
              (when (memq (process-status proc) '(signal exit))
                (tab-close))))

(use-package cc-mode
  :bind (:map c-mode-map
         ("C-c d" . me/start-debugging)
         ("C-c g" . recompile))
  :config
  (setq-default c-basic-offset 4))

(provide 'lang-c)
;;; lang-c.el --- ends here
