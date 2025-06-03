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

;; Required by evil-collection
(use-package evil
  :init
  ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)

  (evil-mode))

(use-package evil-collection
  :after evil
  :diminish
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer magit vterm))
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :diminish
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :diminish
  :config
  (evil-define-key 'normal 'global
    "gcb" 'evilnc-comment-box
    "gcc" 'evilnc-comment-or-uncomment-lines
    "gcp" 'evilnc-comment-or-uncomment-paragraphs
    "gct" 'evilnc-comment-or-uncomment-html-tag
    "gcd" 'evilnc-copy-and-comment-lines))

;; Setting RETURN key in org-mode to follow links
(setq org-return-follows-link t)

(use-package general
  :init
  (defun reload-init-file ()
    "Reload the main Emacs init file."
    (interactive)
    (load-file user-init-file)
    (load-file user-init-file))

  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer airi/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (airi/leader-keys
    "SPC" '(consult-buffer :wk "Consult buffer")
    "."   '(find-file :wk "Find file")
    "TAB TAB" '(comment-line :wk "Comment lines"))

  (airi/leader-keys
    "b" '(:ignore t :wk "Buffer")
    "b b" '(consult-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer"))

  (airi/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired"))

  (airi/leader-keys
    "e" '(:ignore t :wk "Eshell/Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e R" '(eww-reload :wk "Reload current page in EWW")
    "e s" '(eshell :wk "Eshell")
    "e w" '(eww :wk "EWW emacs web browser"))

  (airi/leader-keys
    "f" '(:ignore t :wk "Format")
    "f f" '(format-all-region-or-buffer :wk "Format region or buffer")
    "f l" '(lsp-format-buffer :wk "LSP Format buffer"))

  (airi/leader-keys
    "g" '(:ignore t :wk "Magit")
    "g g" '(magit-status :wk "Open magit status"))

  (airi/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h r r" '(reload-init-file :wk "Reload emacs config")
    "h v" '(describe-variable :wk "Describe variable"))

  (airi/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (airi/leader-keys
    "s" '(:ignore t :wk "Search")
    "s b" '(consult-buffer :wk "Search buffers")
    "s p b" '(consult-project-buffer :wk "Search project buffers")
    "s f" '(consult-fd :wk "Search files")
    "s g" '(consult-ripgrep :wk "Search with ripgrep")
    "s G" '(deadgrep :wk "Search with Deadgrep")
    "s m" '(consult-man :wk "Search man pages")
    "s r" '(consult-recent-file :wk "Search recent files")
    "s t" '(hl-todo-rgrep :wk "Search TODOs"))

  (airi/leader-keys
    "v" '(:ignore t :wk "Vterm")
    "v t" '(vterm-toggle :wk "Vterm Toggle")
    "v c" '(vterm-toggle-cd :wk "Vterm Toggle CD"))

  (airi/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right"))

  (airi/leader-keys
    "y" '(:ignore t :wk "Yasnippet")
    "y i" '(yas-insert-snippet :wk "Insert snippet")
    "y n" '(yas-new-snippet :wk "New snippet")
    "y e" '(yas-expand :wk "Expand snippet")
    "y r" '(yas-reload-all :wk "Reload all snippets")
    "y v" '(yas-visit-snippet-file :wk "Visit snippet file")
    "y l" '(yas-describe-tables :wk "Describe snippet tables")))

;; The undo-fu package is a lightweight wrapper around Emacs' built-in undo
;; system, providing more convenient undo/redo functionality.
(use-package undo-fu
  :defer t
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
  :init
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq evil-undo-system 'undo-fu
        undo-limit 256000           ; 256kb (default is 160kb)
        undo-strong-limit 2000000   ; 2mb   (default is 240kb)
        undo-outer-limit 36000000))  ; 36mb  (default is 24mb)

;; The undo-fu-session package complements undo-fu by enabling the saving
;; and restoration of undo history across Emacs sessions, even after restarting.
(use-package undo-fu-session
  :defer t
  :commands undo-fu-session-global-mode
  :init
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (when (executable-find "zstd")
    (setq undo-fu-session-compression 'zst))
  :hook (after-init . undo-fu-session-global-mode))

(use-package which-key
  :init
  (which-key-mode 1)

  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	    which-key-sort-order #'which-key-key-order-alpha
	    which-key-allow-imprecise-window-fit nil
	    which-key-sort-uppercase-first nil
	    which-key-add-column-padding 1
	    which-key-max-display-columns nil
	    which-key-min-display-lines 6
	    which-key-side-window-slot -10
	    which-key-side-window-max-height 0.25
	    which-key-idle-delay 0.8
	    which-key-max-description-length 25
	    which-key-allow-imprecise-window-fit nil
	    which-key-separator " → " ))

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  (pixel-scroll-precision-mode 1))

(let ((mine-dir (expand-file-name "mine" "~/.emacs.d")))
  (dolist (file (directory-files mine-dir t "\\.el\\'"))
    (minimal-emacs-load-user-init file)))

;;; post-init.el ends here
