;;; evil-setup.el --- Vim emulation & Keybindings -*- lexical-binding: t; -*-

;; Keywords: evil, vim, keybindings, undo

;;; Commentary:
;;
;; Configures Evil mode for Vim emulation and defines custom keybindings
;; and related packages.
;;

;;; Code:

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo undo-fu-only-redo)
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :ensure t
  :commands undo-fu-session-global-mode
  :hook (elpaca-after-init . undo-fu-session-global-mode))

;; Set undo system for Evil
(setq evil-undo-system 'undo-fu)

(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :hook (elpaca-after-init . evil-mode)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-default-cursor       'box)  ; Fallback
  (evil-normal-state-cursor  'box)  ; Normal mode
  (evil-insert-state-cursor  'box)  ; Insert mode (usually bar)
  (evil-visual-state-cursor  'box)  ; Visual mode
  (evil-motion-state-cursor  'box)  ; Motion mode
  (evil-replace-state-cursor 'box)  ; Replace mode (usually underscore)
  (evil-operator-state-cursor 'box) ; Operator pending
  (evil-emacs-state-cursor   'box)  ; Emacs mode
  (evil-ex-visual-char-range t)
  (evil-ex-search-vim-style-regexp t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-echo-state nil)
  (evil-move-cursor-back nil)
  (evil-v$-excludes-newline t)
  (evil-want-C-h-delete t)
  (evil-want-C-u-delete t)
  (evil-want-fine-undo t)
  (evil-move-beyond-eol t)
  (evil-search-wrap nil)
  (evil-want-Y-yank-to-eol t)

  :config
  ;; Define custom operator for commenting
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :ensure t
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))
     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))
     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (elpaca-after-init . global-evil-surround-mode))

(use-package general
  :ensure t
  ;; :after evil
  :config
  (with-eval-after-load 'evil
    (general-evil-setup))

  ;; Create the "SPC" leader definer
  (general-create-definer my-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my-leader-def
    "SPC" '(execute-extended-command :which-key "M-x")
    "."   '(find-file :which-key "Find file")
    "u"   '(universal-argument :which-key "Universal arg")

    ;; Code / LSP (Eglot & Apheleia)
    "c"  '(:ignore t :which-key "code")
    "ca" '(eglot-code-actions :which-key "Code actions")
    "cr" '(eglot-rename :which-key "Rename symbol")
    "cf" '(apheleia-format-buffer :which-key "Format buffer") ; Using Apheleia
    "cd" '(eglot-find-declaration :which-key "Find declaration")
    "ci" '(eglot-find-implementation :which-key "Find implementation")
    "cx" '(consult-flymake :which-key "List errors") ; Flymake list
    "ce" '(flymake-show-buffer-diagnostics :which-key "Buffer diagnostics")

    ;; Files
    "f"  '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file")
    "fr" '(consult-recent-file :which-key "Recent files")
    "fc" '(copy-file :which-key "Copy file")
    "fR" '(rename-file :which-key "Rename/Move file")

    ;; Help (Helpful)
    "h"  '(:ignore t :which-key "help")
    "hf" '(helpful-callable :which-key "Describe function")
    "hv" '(helpful-variable :which-key "Describe variable")
    "hk" '(helpful-key :which-key "Describe key")
    "ht" '(consult-theme :which-key "Load theme")

    ;; Windows
    "w"  '(:ignore t :which-key "window")
    "w;" '(evil-window-vsplit :which-key "Split vertical")
    "wv" '(evil-window-split :which-key "Split horizontal")
    "wl" '(evil-window-right :which-key "Window right")
    "wh" '(evil-window-left :which-key "Window left")
    "wk" '(evil-window-up :which-key "Window up")
    "wj" '(evil-window-down :which-key "Window down")
    "wd" '(delete-window :which-key "Delete window")
    "wo" '(delete-other-windows :which-key "Maximize window")))

(use-package avy
  :ensure t
  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m))

  (avy-timeout-seconds 0.3) ;; Delay before jump (fuzzy-ish feel)
  (avy-style 'pre)          ;; Show label before the target
  (avy-background t)        ;; Dim the background during jump
  (avy-all-windows t)       ;; Search across all visible windows (like Flash)

  :init
  (with-eval-after-load 'general
    (general-def
      :states 'normal
      "s" '(avy-goto-char-timer :which-key "Flash Jump")
      "S" '(avy-goto-word-1 :which-key "Flash Word"))

    (general-def
      :states 'operator
      "r" '(avy-goto-char-timer :which-key "Remote Jump")
      "R" '(avy-goto-word-1 :which-key "Remote Word"))

    (general-def
      :states '(normal insert emacs)
      "C-s" '(avy-isearch :which-key "Flash Search"))

    (my-leader-def
      "j"  '(:ignore t :which-key "jump")
      "jj" '(avy-goto-char-timer :which-key "Jump char")
      "jl" '(avy-goto-line :which-key "Jump line"))))

(provide 'evil-setup)
;;; evil-setup.el ends here
