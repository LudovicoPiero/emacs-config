;;; init-dev.el --- Coding & LSP -*- lexical-binding: t; -*-

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; -- GIT (Magit) --
(use-package magit
  :bind ("C-x g" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; -- COMPLETION (Corfu) --
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-no-match nil)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

;; -- TREESITTER --
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; This automatically switches you to _ts-mode (e.g., python-ts-mode)
  ;; if the grammar is found in your Nix store.
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; -- LSP (Eglot) --
(use-package eglot
  :hook
  ;; trigger Eglot on all these modes
  ((nix-mode
    python-ts-mode
    go-ts-mode
    rust-ts-mode
    c++-ts-mode
    lua-mode
    js-ts-mode) . eglot-ensure)

  :config
  ;; Python -> basedpyright
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio")))

  ;; Nix -> nil
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nil")))

  ;; Lua -> emmylua-ls
  (add-to-list 'eglot-server-programs
               '(lua-mode . ("emmylua-ls")))

  ;; Go -> gopls (Standard, but explicit is safer)
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls")))

  ;; Rust -> rust-analyzer (Standard)
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))

  ;; C++ -> clangd (Standard)
  (add-to-list 'eglot-server-programs
               '((c++-mode c++-ts-mode) . ("clangd")))

  ;; Optimization
  (setq eglot-events-buffer-size 0)

  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c l a" . eglot-code-actions)
        ("C-c l d" . eldoc)
        ("C-c l f" . eglot-format)))

;; -- FORMATTER (Apheleia) --
(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (setq apheleia-on-save nil) ;; Manual trigger only

  :bind
  ("C-c f" . apheleia-format-buffer))

;; -- SYNTAX CHECKING --
(use-package flymake
  :bind
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error))

(provide 'init-dev)
