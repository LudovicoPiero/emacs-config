;;; keybinds.el --- Keybinds Configuration -*- lexical-binding: t -*-

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

;; This file contains the main keybinding configuration for my Emacs setup,
;; primarily structured around Evil-mode (Vim emulation) and the General
;; package for concise and expressive keybinding definitions.

;;; Code:

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

(provide 'keybinds)
;;; keybinds.el ends here
