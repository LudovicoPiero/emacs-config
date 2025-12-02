;;; init-dev.el --- Coding & LSP -*- lexical-binding: t; -*-

;; -- LANGUAGES & MODES --

(use-package nix-mode
  :mode "\\.nix\\'"
  :general
  (my-local-leader-def
    :keymaps 'nix-mode-map
    "b" #'nix-build-buffer))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package go-ts-mode
  :ensure nil
  :straight nil
  :general
  (my-local-leader-def
    :keymaps 'go-ts-mode-map
    "t" #'go-test-current-test
    "r" #'go-run))

(use-package c++-ts-mode
  :ensure nil
  :straight nil
  :general
  (my-local-leader-def
    :keymaps '(c-mode-map c++-mode-map c++-ts-mode-map)
    "c" #'compile
    "d" #'gdb))

;; -- TOOLS --

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-no-match nil)
  :general
  (general-def
    :keymaps 'corfu-map
    "TAB" #'corfu-next
    [tab] #'corfu-next
    "S-TAB" #'corfu-previous
    [backtab] #'corfu-previous))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  ;; Ensure Eglot is triggered for these modes
  :hook
  ((nix-mode python-ts-mode go-ts-mode rust-ts-mode c++-ts-mode lua-mode js-ts-mode) . eglot-ensure)
  :commands (eglot-rename eglot-code-actions eglot-format)
  :config
  ;; -- Server Configuration --
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs '(lua-mode . ("emmylua-ls")))
  (add-to-list 'eglot-server-programs '((go-mode go-ts-mode) . ("gopls")))
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode) . ("clangd")))

  (setq eglot-events-buffer-size 0)

  :general
  ;; Buffer-local keys (only active when in eglot mode)
  (general-def
    :keymaps 'eglot-mode-map
    "C-c l r" #'eglot-rename
    "C-c l a" #'eglot-code-actions
    "C-c l d" #'eldoc
    "C-c l f" #'eglot-format))

(use-package apheleia
  :commands (apheleia-format-buffer)
  :config
  (apheleia-global-mode +1)
  (setq apheleia-on-save nil)
  :general
  (general-def
    "C-c f" #'apheleia-format-buffer))

(use-package flymake
  :commands (flymake-goto-next-error flymake-goto-prev-error flymake-show-buffer-diagnostics)
  :general
  (general-def
    "M-n" #'flymake-goto-next-error
    "M-p" #'flymake-goto-prev-error))

;; -- GLOBAL DEVELOPMENT KEYBINDINGS --
(my-leader-def
  ;; Git
  "g"   '(:ignore t :which-key "git")
  "gg"  #'magit-status
  "gf"  #'magit-file-dispatch
  "gb"  #'magit-blame

  ;; Code / LSP
  "c"   '(:ignore t :which-key "code")
  "ca"  #'eglot-code-actions
  "cr"  #'eglot-rename
  "cd"  #'eldoc
  "cf"  #'apheleia-format-buffer

  ;; Diagnostics
  "x"   '(:ignore t :which-key "diagnostics")
  "xx"  #'consult-flymake
  "xn"  #'flymake-goto-next-error
  "xp"  #'flymake-goto-prev-error
  "xb"  #'flymake-show-buffer-diagnostics
  "ce"  #'consult-flymake)

(provide 'init-dev)
