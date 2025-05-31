;;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: Ludovico Piero <lewdovico@gnuweeb.org>
;; Version: 0.0.1

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This file is loaded after early and pre-init steps. It:
;; - Sets up general Emacs behavior and UX improvements
;; - Starts core minor modes after init
;; - Integrates with `compile-angel` for byte-compilation
;; - Loads all user config files under `~/.emacs.d/mine`

;;; Code:

(use-package compile-angel
  :ensure t
  :demand t
  :custom
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose nil)

  :config
  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; `use-package', you'll need to explicitly add `(require 'use-package)` at
  ;; the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/mine/completion.el" compile-angel-excluded-files)
  (push "/mine/core.el" compile-angel-excluded-files)
  (push "/mine/dired.el" compile-angel-excluded-files)
  (push "/mine/etc.el" compile-angel-excluded-files)
  (push "/mine/git.el" compile-angel-excluded-files)
  (push "/mine/keybinds.el" compile-angel-excluded-files)
  (push "/mine/languages.el" compile-angel-excluded-files)
  (push "/mine/org.el" compile-angel-excluded-files)
  (push "/mine/ui.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files before they are loaded.
  (compile-angel-on-load-mode))

(use-package emacs
  :ensure nil
  :init
  ;; Disable saving to custom.el
  (setq custom-file null-device)

  ;; Treat built-in packages as upgradable
  (setq package-install-upgrade-built-in t)

  ;; Show line and column numbers in the mode line
  (setq line-number-mode t
        column-number-mode t
        display-line-numbers-type 'relative
        mode-line-position-column-line-format '("%l:%C"))

  ;; Confirm Emacs exit with y/n
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; Always Follow Symlinks in VCS Projects
  (setq vc-follow-symlinks t)

  ;; Backup settings
  (setq make-backup-files t
        vc-make-backup-files t
        kept-old-versions 10
        kept-new-versions 10)

  ;; Improve performance by skipping fontification during input
  (setq redisplay-skip-fontification-on-input t)

  ;; Use block cursors in all Evil states
  (setq evil-default-cursor 'box
        evil-insert-state-cursor 'box
        evil-emacs-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-replace-state-cursor 'box
        evil-operator-state-cursor 'box)

  ;; Follow Org-mode links when pressing RET
  (setq org-return-follows-link t)

  ;; Enable relative line numbers globally
  (global-display-line-numbers-mode t)

  ;; Enable line highlighting in all buffers
  (global-hl-line-mode 1)

  ;; Set default font
  (when (display-graphic-p)
  (set-face-attribute 'default nil
                      :height 130 :weight 'semi-bold :family "Iosevka q"))

  ;; Disable any themes that were enabled early
  (mapc #'disable-theme custom-enabled-themes)

  ;; Filter more buffers
  (with-eval-after-load 'consult
    (dolist (pattern
             '("\\`\\*Help\\*\\'"
               "\\`\\*scratch\\*\\'"
               "\\`\\*Messages\\*\\'"
               "\\`\\*Ibuffer\\*\\'"
               "\\`\\*straight-process\\*\\'"
               "\\`\\*Async-native-compile-log\\*\\'"
               "\\`\\*lsp-bridge\\*\\'"
               "\\`\\*lsp-log\\*\\'"
               "\\`\\*nixd-lsp\\*\\'"
               "\\`\\*nixd-lsp::stderr\\*\\'"
               "\\`\\*info\\*\\'"
               "\\`\\*Warnings\\*\\'"
               "\\`\\*Compile-Log\\*\\'"
               "\\`\\*direnv\\*\\'"))
      ;; Add pattern only if it's not already in consult-buffer-filter
      (unless (member pattern consult-buffer-filter)
        (add-to-list 'consult-buffer-filter pattern))))

  ;; Enable recentf-mode silently after init
  (add-hook 'after-init-hook
            (lambda ()
              (let ((inhibit-message t))
                (recentf-mode 1))))

  ;; Add more stuff to recentf-exclude
  (dolist (pattern '(;; Ignore temporary or cache files
                   "/tmp/"
                   "/var/tmp/"
                   "\\.cache/"
                   ;; Ignore tramp (remote editing) paths
                   "^/\\(scp\\|rsync\\|ftp\\|telnet\\|adb\\):"
                   ;; Ignore commits and merge messages
                   "COMMIT_EDITMSG\\'"
                   "MERGE_MSG\\'"
                   ;; Ignore org agenda archive files
                   "\\.org_archive\\'"
                   ;; Ignore bookmarks and other generated files
                   "bookmarks$"
                   "\\.recentf$"
                   ".*-autoloads\\.el\\'"
                   ;; Ignore encrypted files
                   "\\.gpg\\'"
                   ;; Ignore image and binary files
                   "\\.\\(jpg\\|png\\|gif\\|svg\\|xpm\\|pdf\\|exe\\|bin\\|dump\\)\\'"
                   ;; Ignore elpa/straight build dirs
                   "/elpa/"
                   "/straight/"
                   ;; Ignore node_modules and vendor
                   "node_modules"
                   "/vendor/"
                   ;; Ignore nix store
                   "^/nix/store/"))
  (add-to-list 'recentf-exclude pattern))

  ;; Automatically reload files changed on disk
  (add-hook 'after-init-hook #'global-auto-revert-mode)

  ;; Preserve minibuffer history across sessions
  (add-hook 'after-init-hook #'savehist-mode)

  ;; Restore cursor position when reopening files
  (add-hook 'after-init-hook #'save-place-mode)

  ;; Clean recentf list on Emacs exit
  (add-hook 'kill-emacs-hook #'recentf-cleanup)

  ;; Highlight matching parentheses
  (add-hook 'after-init-hook #'show-paren-mode)

  ;; Enable undo/redo for window configuration changes
  (add-hook 'after-init-hook #'winner-mode)

  ;; Enable replacing selected text when typing
  (delete-selection-mode 1)

  ;; Enable draggable window dividers
  (add-hook 'after-init-hook #'window-divider-mode)

  ;; Use block cursor in all modes
  (setq-default cursor-type 'box)

  ;; Silence native-comp warnings in Emacs 28+
  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors nil))

  ;; Turn off automatic reindentation on RET
  (electric-indent-mode -1)

  ;; Enable automatic closing of paired characters
  (electric-pair-mode 1))

(let ((mine-dir (expand-file-name "mine" "~/.emacs.d")))
  (dolist (file (directory-files mine-dir t "\\.el\\'"))
    (minimal-emacs-load-user-init file)))

;;; post-init.el ends here
