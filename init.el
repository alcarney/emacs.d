;;; init.el --- Emacs Configuration
;;; -*- lexical-binding: t -*-
(defconst emacs-start-time (current-time))

;; Basics
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)

(global-auto-revert-mode 1)

(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(setq whitespace-style '(face empty trailing))
(global-whitespace-mode)
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Completions
(fido-mode)

;; Project management
(global-set-key (kbd "C-c p f") 'project-find-file)

;; Packaging bootstrap
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)

;; Load config from other files.
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'git-config)
(require 'md-config)
(require 'org-config)
(require 'ui-config)

(require 'lang-c)

;; Ensure that any custom settings are placed in their own file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(let ((startup-time
       (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded configuration in %.3fs" startup-time))

(provide 'init)
;;; init.el ends here
