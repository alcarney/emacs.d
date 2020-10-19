;;; init.el --- Emacs Configuration
;;; -*- lexical-binding: t -*-
;;; Commentary:
;; Yet another dip of the toe into the world of Emacs.
;; Who knows how long this one will last! :)
;;; Code:
;; -- Basics --
(defconst emacs-start-time (current-time))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Backups
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)

(global-auto-revert-mode 1)

;; Editing
(setq kill-whole-line t)

(defun init-el/beginning-of-line ()
  "Move point to first non-whitespace character, or start of the line"
  (interactive)
  (let ((original-pos (point)))
    (back-to-indentation)
    (if (= original-pos (point))
        (beginning-of-line))))

(global-set-key (kbd "C-a") 'init-el/beginning-of-line)

;; UI Tweaks
(setq default-frame-alist '((font . "Ubuntu Mono-11"))
      inhibit-startup-screen t
      show-paren-style 'parenthesis)

(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(column-number-mode 1)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)
(size-indication-mode t)
(show-paren-mode 1)

;; Whitespace
(setq-default fill-column 88
              indent-tabs-mode nil
              tab-width 4
              whitespace-line-column 88)

(setq whitespace-style '(face empty trailing lines-tail))
(global-whitespace-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)

;; -- Packages --
(require 'package)
(add-to-list 'package-archives
             (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-enable-imenu-support t)

(use-package all-the-icons :ensure t)
(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :config (all-the-icons-ivy-setup))

(use-package counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)
         ("C-c i" . counsel-imenu)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-buffer-file-name-size 'relative-to-project
        doom-modeline-major-mode-color-icon nil))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nord t)

  (doom-themes-org-config)
  (doom-themes-treemacs-config))

(use-package ein :ensure t)

(use-package elpy
  :ensure t
  :init (elpy-enable)
  :hook (elpy-mode . (lambda ()
                       (highlight-indentation-mode -1)
                       (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))
  :config
  (setq elpy-rpc-python-command "python3"))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 1)

  (set-face-foreground 'git-gutter:added "forest green")
  (set-face-foreground 'git-gutter:modified "goldenrod")
  (set-face-foreground 'git-gutter:deleted "brown")

  (setq git-gutter:added-sign "·"
        git-gutter:modified-sign "·"
        git-gutter:deleted-sign "·"))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out")))

(use-package ivy
  :ensure t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x C-b" . ivy-switch-buffer-other-window)))

;;(use-package leuven-theme :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook (org-mode . (lambda ()
                      (turn-on-auto-fill)))
  :config
  (setq org-agenda-files '("~/Documents/org")
        org-directory "~/Documents/org"
        org-startup-indented t))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
         ("C-c p" . 'projectile-command-map))
  :config
  (projectile-mode 1)

  (setq projectile-completion-system 'ivy
        projectile-project-search-path '("~/Projects")
        projectile-sort-order 'recently-active))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode 1)
  (solaire-mode-swap-bg))

(use-package swiper
  :ensure t)

(use-package treemacs
  :ensure t
  :bind (("C-<tab>" . treemacs))
  :config
  (treemacs-git-mode 'extended)
  (add-to-list 'treemacs-pre-file-insert-predicates 'treemacs-is-file-git-ignored?))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.css\\'" . web-mode)))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

;; Ensure that any custom settings are placed in their own file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(let ((startup-time
       (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Config loaded in %.3fs" startup-time))

(provide 'init)
;;; init.el ends here
