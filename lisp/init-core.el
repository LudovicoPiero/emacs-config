;;; init-core.el --- Sane Defaults & Helpers -*- lexical-binding: t; -*-

;; -- UTF-8 Everywhere --
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; -- File Management --
(defvar user-emacs-var-directory (expand-file-name "var/" user-emacs-directory))
(unless (file-exists-p user-emacs-var-directory)
  (make-directory user-emacs-var-directory))

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-var-directory)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-emacs-var-directory) t))
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-var-directory)
      create-lockfiles nil)

(setq make-backup-files t
      vc-make-backup-files t
      version-control t
      kept-new-versions 6
      kept-old-versions 2
      delete-old-versions t)

;; -- Custom File Isolation --
(setq custom-file (expand-file-name "custom.el" user-emacs-var-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; -- History --
(use-package recentf
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-exclude '("COMMIT_EDITMSG" ".*-autoloads\\.el"))
  (recentf-mode 1))

(use-package savehist
  :init
  (setq savehist-file (expand-file-name "savehist" user-emacs-var-directory))
  (savehist-mode))

;; -- Quality of Life --
(setq use-short-answers t)
(setq use-dialog-box nil)
(setq ring-bell-function 'ignore)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

;; -- Dired Tweaks --
(setq dired-listing-switches "-alh --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)

;; -- STRIPSPACE --
(use-package stripspace
  :hook (after-init . global-stripspace-mode)
  :bind ("C-c s" . stripspace-strip-buffer)
  :config
  (setq stripspace-skip-modes '(markdown-mode org-mode conf-mode)))

;; -- VUNDO (Visual Undo) --
;; Replaces the confusing "undo-redo" loop with a tree visualizer
(use-package vundo
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; -- MULTIPLE CURSORS --
(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)       ; Add cursor to next match
  ("C-<" . mc/mark-previous-like-this)   ; Add cursor to previous match
  ("C-c C-<" . mc/mark-all-like-this)    ; Add cursor to ALL matches
  ("C-c m" . mc/edit-lines)              ; Add cursor to every line in selection
  :config
  (setq mc/list-file (expand-file-name "mc-lists.el" user-emacs-var-directory)))

;; -- CRUX (Useful Extensions) --
(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line)       ; Toggle between first-char and line-start
  ("C-k" . crux-smart-kill-line)              ; Kill line, or join with next if at end
  ("S-<return>" . crux-smart-open-line)       ; Open line below (smart)
  ("C-S-<return>" . crux-smart-open-line-above) ; Open line above (smart)
  ("C-c D" . crux-delete-file-and-buffer)     ; Delete current file
  ("C-c r" . crux-rename-file-and-buffer))    ; Rename current file

;; -- WHICH-KEY --
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(provide 'init-core)
