;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-
(defconst emacs-start-time (current-time))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq auto-save-default nil
      create-lockfiles  nil
      make-backup-files nil)

(global-auto-revert-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(use-package whitespace
  :init
  (setq sentence-end-double-space nil
        whitespace-style '(face empty trailing))

  (setq-default indent-tabs-mode nil)

  (global-whitespace-mode)
  (add-hook 'before-save-hook #'whitespace-cleanup))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(setq inhibit-startup-message t)

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package icomplete
  :init
  (fido-mode 1))

(set-face-attribute 'default nil :font "Ubuntu Mono" :height 125)
(set-face-attribute 'fixed-pitch nil :font "Ubuntu Mono" :height 125)
(set-face-attribute 'variable-pitch nil :font "Ubuntu Light" :height 125)

(use-package all-the-icons
  :ensure t)

(global-display-line-numbers-mode 1)

(dolist (hook '(doc-view-mode-hook
                eshell-mode-hook
                gfm-mode-hook
                org-mode-hook
                shell-mode-hook
                term-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (column-number-mode 1)
  (size-indication-mode 1)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-buffer-modification-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-height 25
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon nil
        doom-modeline-minor-modes nil))

(use-package tab-bar
  :config
  (setq tab-bar-show nil))

(use-package modus-themes
  :ensure t
  :bind ("<f5>" . modus-themes-toggle)
  :init
  (setq modus-themes-diffs              'desaturated
        modus-themes-headings           '((t . rainbow-section-no-bold))
        modus-themes-intense-hl-line    t
        modus-themes-lang-checkers      'straight-underline
        modus-themes-links              'faint-neutral-underline
        modus-themes-org-blocks         'grayscale
        modus-themes-paren-match        'intense-bold
        modus-themes-region             'bg-only-no-extend
        modus-themes-scale-headings     t
        modus-themes-slanted-constructs t)

  ;; Default to the light theme
  (modus-themes-load-operandi)

  (show-paren-mode 1))

(use-package window
  :init
  (setq display-buffer-alist
        '(("\\*Help\\*"
           (display-buffer-in-side-window)
           (window-height . 0.20)
           (side . top)
           (slot . 0))
          ("\\*\\(e?shell\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0))
          ("\\*compilation\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0))
          ("\\*Python\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0))))
  :bind (("<f8>" . window-toggle-side-windows)))

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

(defun me/project-search ()
  "Execute a project wide search with ripgrep."
  (interactive)
  (let ((dir   (cdr (project-current t)))
        (query (read-string "Search query: ")))
    (rg query "*" dir)))

(use-package rg
  :ensure t)

(use-package project
  :bind (("C-x p f" . project-find-file)
         ("C-x p s" . me/project-search)))

(defun me/start-debugging ()
  (interactive)
  (let ((program (read-string "Debug program: ")))
    (tab-new)
    (setq gdb-many-windows t)
    (gdb (format "gdb -i=mi %s" program))))

(advice-add 'gud-sentinel :after
            (lambda (proc msg)
              (when (memq (process-status proc) '(signal exit))
                (tab-close))))

(use-package cc-mode
  :bind (:map c-mode-map
              ("C-c d" . me/start-debugging)
              ("C-c g" . recompile))
  :config
  (setq-default c-basic-offset 4)
  (setq compilation-scroll-output t))

(defun me/python-open-repl ()
  "Open a Python REPL in the correct virtualenv for the
  project.

This will first look for Jupyter and will fall back to Python if
it's not installed."
  (interactive)
  (let* ((dir   (cdr (project-current t)))
         (paths (list
                   (concat dir ".env/bin/jupyter")
                   (concat dir ".env/bin/python")))
         (path  (car (seq-filter 'file-exists-p paths))))

    (setq python-shell-interpreter path
          python-shell-prompt-detect-failure-warning nil)

    (if (string-match-p (regexp-quote "jupyter") path)
        (setq python-shell-interpreter-args "console --simple-prompt")
      (setq python-shell-interpreter-args "-i"))

    (run-python)))

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

(use-package python
  :bind (:map python-mode-map
              ("C-c C-p" . me/python-open-repl)
              ("C-c g"   . recompile))
  :config
  (setq compilation-scroll-output t)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

(use-package markdown-mode
  :ensure t
  :hook (gfm-mode . (lambda ()
                      (setq-local fill-column 80)
                      (turn-on-auto-fill)
                      (flyspell-mode)))
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

(defun me/org-mode-tweaks ()
  (setq-local fill-column 80)
  (turn-on-auto-fill)
  (flyspell-mode)

  (org-indent-mode)
  (variable-pitch-mode 1)

  ;; Switch certain elements back to fixed pitch
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . me/org-mode-tweaks)
  :config
  (setq org-directory "~/Documents/org"
        org-startup-indented t))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(let ((startup-time
       (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded configuration in %.3fs" startup-time))
