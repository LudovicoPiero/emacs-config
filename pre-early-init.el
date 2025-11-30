;;; pre-early-init.el --- Sane Defaults -*- lexical-binding: t; -*-

;; -- UTF-8 Everywhere --
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Disable the default package.el init to prevent conflicts with Straight
(setq minimal-emacs-package-initialize-and-refresh nil)

;; -- File Management --
;; We must define this variable BEFORE using it in directory-alist below
(defvar user-emacs-var-directory (expand-file-name "var/" minimal-emacs-user-directory))
(unless (file-exists-p user-emacs-var-directory)
  (make-directory user-emacs-var-directory t))

;; Point internal engine to var directory
(setq user-emacs-directory user-emacs-var-directory)
(setq package-user-dir (expand-file-name "elpa" user-emacs-var-directory))

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

;; pre-early-init.el ends here
