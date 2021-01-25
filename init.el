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

(setq-default truncate-lines t)

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
  :bind (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-use-other-window t))

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

(defun me/buffer-select-by-major-mode (mode)
  "A filter for use with `display-buffer-alist', will select a
  buffer if it matches the given major-mode"
  (lambda (buffer action)
    (with-current-buffer buffer
      (eq major-mode mode))))

(defun me/display-tabbed-buffer-in-side-window (buffer alist)
  "See `display-buffer-in-side-window'"
  (display-buffer-in-side-window buffer alist)
  (with-current-buffer buffer
    (tab-line-mode)))

(dolist (hook '(shell-mode-hook
                eshell-mode-hook))
  (add-hook hook (lambda () (tab-line-mode))))
(defun me/display-buffer-in-panel (predicate)
  "Display buffers matching the given PREDICATE in the panel.

To borrow terminology from VSCode the panel is that collapsable
window at the bottom of the screen.
"
  `(,predicate
    (me/display-tabbed-buffer-in-side-window)
    (window-height . 0.25)
    (side . bottom)
    (slot . 0)
    (window-parameters . ((no-other-window . t)))))


(use-package window
  :init
  (setq display-buffer-alist `(,(me/display-buffer-in-panel "\\*Help\\*")
                               ,(me/display-buffer-in-panel "\\*\\(e?shell\\)\\*")
                               ,(me/display-buffer-in-panel
                                 (me/buffer-select-by-major-mode 'compilation-mode))
                               ,(me/display-buffer-in-panel "\\*Ibuffer\\*")
                               (,(me/buffer-select-by-major-mode 'dired-mode)
                                (display-buffer-in-side-window)
                                (window-width . 0.15)
                                (side . left)
                                (slot . 0)
                                (window-parameters . ((no-other-window . t))))
                               ,(me/display-buffer-in-panel
                                 (me/buffer-select-by-major-mode 'inferior-python-mode))
                               ))
  :bind (("<f8>" . window-toggle-side-windows)))

(defun me/dired-open-directory ()
  (interactive)
  (dired default-directory))

(defun me/dired-mode-tweaks ()
  (dired-hide-details-mode))

(use-package dired
  :bind (("C-x d" . me/dired-open-directory))
  :hook (dired-mode . me/dired-mode-tweaks))

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
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-directory "~/Documents/org/")
  (setq org-agenda-files (list org-directory))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)))
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "life.org" "Events")
           "* TODO %?\n")
          ("e" "Event" entry (file+headline "life.org" "Events")
           "* %?\nSCHEDULED: %^t")
          ("j" "Journal" entry (file+headline "life.org" "Journal")
           "* %u\n%?\n\n** Exercise\n")))
  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-show-all-today t
        org-log-into-drawer t))

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

(defun me/python-mode-tweaks ()
  (setq-local fill-column 88)
  (add-hook 'after-save-hook 'me/python-flake8-project 0 t))

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
(defun me/python-flake8-project ()
  "Run flake8 on the current project in a compilation buffer.

This function will attempt to find the setup.py file for the
package currently being edited using `me/python-find-setup-py'.
If a filepath is found, its parent directory is assumed to be the
package root and flake8 will be run in a compilation buffer via
`compilation-start'."
  (interactive)
  (let* ((filename (buffer-file-name (current-buffer)))
         (setup-py (me/python-find-setup-py filename)))
    (unless (null setup-py)
      (let ((default-directory (file-name-directory setup-py)))
        (compilation-start "flake8" nil
                           (lambda (_modename)
                             (format "%s: flake8" default-directory)))))))

(defun me/python-find-setup-py--from-dir (dir)
  (if (string= dir "/")
      nil
    (let* ((setup-py (concat dir "setup.py")))
      (if (file-exists-p setup-py)
          setup-py
        (me/python-find-setup-py
         (file-name-directory (directory-file-name dir)))))))

(defun me/python-find-setup-py (filename)
  "Find the setup.py file that corresponds with the package that
contains FILENAME"
  (me/python-find-setup-py--from-dir
   (file-name-directory filename)))


(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

(use-package python
  :bind (:map python-mode-map
              ("C-c C-p" . me/python-open-repl)
              ("C-c g"   . recompile))
  :hook (python-mode . me/python-mode-tweaks)
  :config
  (setq compilation-scroll-output t)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

(defun me/gfm-mode-tweaks ()
  (setq-local fill-column 80)
  (turn-on-auto-fill)
  (flyspell-mode)

  (variable-pitch-mode 1)

  ;; Switch certain elements back to fixed pitch
  (set-face-attribute 'markdown-metadata-key-face nil :inherit 'fixed-pitch)
  (set-face-attribute 'markdown-metadata-value-face nil :inherit 'fixed-pitch))

(use-package markdown-mode
  :ensure t
  :hook (gfm-mode . me/gfm-mode-tweaks)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(let ((startup-time
       (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded configuration in %.3fs" startup-time))
