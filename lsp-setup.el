;;; lsp-setup.el --- LSP setup -*- lexical-binding: t; -*-

;; Keywords: lsp, eglot, apheleia, python, nix, rust, go, cpp
;; Commentary:
;; Configures LSP (Eglot), Formatting (Apheleia), and Syntax Checking (Flymake)
;; for the specific binaries provided by the Nix wrapper.

;;; Code:

;; FORCE update jsonrpc.
;; Eglot requires 1.0.26+, but Emacs 29 ships with 1.0.25.
(use-package jsonrpc
  :ensure t)

(use-package envrc
  :ensure t
  :hook (elpaca-after-init-hook . envrc-global-mode))

(use-package eglot
  :ensure t
  :after general
  :hook
  ((nix-mode
    python-mode python-ts-mode
    rust-mode rust-ts-mode
    go-mode go-ts-mode
    lua-mode
    c-mode c-ts-mode c++-mode c++-ts-mode
    bash-ts-mode
    js-mode js-ts-mode typescript-ts-mode
    markdown-mode
    yaml-ts-mode
    html-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))

  ;; --- Python (Basedpyright) ---
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio")))

  ;; --- C / C++ (Clangd) ---
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ("clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--header-insertion=iwyu")))

  ;; --- Web / Typescript ---
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-ts-mode)
                 . ("typescript-language-server" "--stdio")))

  ;; --- Shell ---
  (add-to-list 'eglot-server-programs
               '((bash-ts-mode sh-mode) . ("bash-language-server" "start")))

  ;; --- Markdown ---
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("marksman" "server")))

  ;; --- TOML ---
  (add-to-list 'eglot-server-programs
               '(toml-ts-mode . ("taplo" "lsp" "stdio")))

  ;; --- Configure Workspace Settings ---
  (setq-default eglot-workspace-configuration
                '(:nil (:formatting (:command ["nixfmt"]))
                       :basedpyright (:analysis (:autoSearchPaths t
                                                                  :diagnosticMode "workspace"
                                                                  :useLibraryCodeForTypes t
                                                                  :typeCheckingMode "standard"))
                       :gopls (:usePlaceholders t
                                                :gofumpt t)
                       :lua (:diagnostics (:globals ["vim" "love"]))))

  ;; Performance tweaks
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0
        eglot-sync-connect 0)

  ;; --- Keybindings ---
  ;; LSP / Code Actions
  (general-def
    :states '(normal visual)
    :keymaps 'eglot-mode-map
    :prefix "SPC"
    "c"   '(:ignore t :which-key "code")
    "ca"  '(eglot-code-actions :which-key "code actions")
    "cr"  '(eglot-rename :which-key "rename")
    "cf"  '(apheleia-format-buffer :which-key "format buffer")
    "cd"  '(eldoc-doc-buffer :which-key "hover doc")
    "ci"  '(eglot-find-implementation :which-key "find impl")
    "gd"  '(xref-find-definitions :which-key "go to def")
    "gr"  '(xref-find-references :which-key "go to refs"))

  ;; Flymake Navigation
  (general-def
    :states 'normal
    :keymaps 'flymake-mode-map
    "]e" '(flymake-goto-next-error :which-key "next error")
    "[e" '(flymake-goto-prev-error :which-key "prev error"))

  ;; Python Specific (Ruff Fix)
  (general-def
    :states 'normal
    :keymaps '(python-mode-map python-ts-mode-map)
    :prefix "SPC"
    "cx" '(my/ruff-fix :which-key "ruff fix")))

(use-package nix-mode :ensure t :mode "\\.nix\\'")
(use-package markdown-mode :ensure t :mode ("\\.md\\'" . markdown-mode))
(use-package lua-mode :ensure t :mode "\\.lua\\'")
(use-package go-mode :ensure t :mode "\\.go\\'")
(use-package rust-mode :ensure t :mode "\\.rs\\'")
(use-package yaml-mode :ensure t :mode "\\.ya?ml\\'")

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)

  ;; Python: Ruff
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))

  ;; Go: Gofumpt
  (setf (alist-get 'gofumpt apheleia-formatters)
        '("gofumpt"))

  ;; Nix: Nixfmt
  (setf (alist-get 'nixfmt apheleia-formatters)
        '("nixfmt" "--strict" "--width=80"))

  ;; Lua: Stylua
  (setf (alist-get 'stylua apheleia-formatters)
        '("stylua" "-"))

  ;; Shell: Shfmt
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-"))

  ;; C/C++: Clang-format
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "-assume-filename" filepath))

  ;; Web: Prettier
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))

  ;; CMake: Cmake-format
  (setf (alist-get 'cmake-format apheleia-formatters)
        '("cmake-format" "-"))

  ;; --- Map Modes to Formatters ---
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'nix-mode apheleia-mode-alist) '(nixfmt))
  (setf (alist-get 'go-mode apheleia-mode-alist) '(gofumpt))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(gofumpt))
  (setf (alist-get 'lua-mode apheleia-mode-alist) '(stylua))
  (setf (alist-get 'rust-mode apheleia-mode-alist) '(rustfmt))
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) '(rustfmt))
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) '(shfmt))
  (setf (alist-get 'c-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c++-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'cmake-ts-mode apheleia-mode-alist) '(cmake-format))
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) '(prettier)))

(defun my/ruff-fix ()
  "Run 'ruff check --fix' on the current buffer."
  (interactive)
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (let ((buffer (current-buffer)))
      (with-current-buffer buffer
        (call-process-region (point-min) (point-max)
                             "ruff" t t nil
                             "check" "--fix" "--stdin-filename" (buffer-file-name) "-"))
      (message "Ruff fix applied."))))

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-font-lock-level 4)
  (global-treesit-auto-mode))

(use-package flymake
  :ensure t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
