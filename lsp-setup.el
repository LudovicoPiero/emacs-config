;;; lsp-setup.el --- LSP setup -*- lexical-binding: t; -*-- Keywords: lsp, eglot, apheleia, python, nix--;; Commentary:;;
;; Configures Language Server Protocol (LSP) clients like Eglot, along with
;; formatting tools like Apheleia.
;;; Code:

;; FORCE update jsonrpc.
;; Eglot requires 1.0.26+, but Emacs 29 ships with 1.0.25.
;; We must tell Elpaca to fetch the newer version from ELPA.
(use-package jsonrpc
  :ensure t)

(use-package eglot
  :ensure t
  :config
  ;; Adds nixd to the list of known servers
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd"))))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure))
(use-package envrc
  :ensure t
  :hook (elpaca-after-init . envrc-global-mode))

;; 1. Register basedpyright as the LSP for python-mode
;; (Not strictly necessary if using a very new Emacs, but good for safety)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio"))))

;; 2. Configure the workspace settings
(setq-default eglot-workspace-configuration
              ;; basedpyright specific settings
              '(:basedpyright (:analysis (:autoSearchPaths t
                                                           :diagnosticMode "workspace"
                                                           :useLibraryCodeForTypes t
                                                           :typeCheckingMode "off"))))

;; 3. Hooks to ensure eglot starts
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)

  ;; Define the "format" command (equivalent to your ruff_format)
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))

  ;; Tell Apheleia to use 'ruff' for python-mode and python-ts-mode
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff)))

;; Define the "fix" command (equivalent to your ruff_fix)
;; Bind this to a key, e.g., (global-set-key (kbd "C-c f") 'my/ruff-fix)
(defun my/ruff-fix ()
  "Run 'ruff check --fix' on the current buffer."
  (interactive)
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (let ((buffer (current-buffer)))
      ;; We use call-process-region to send the buffer content to stdin
      ;; and replace the buffer with stdout
      (with-current-buffer buffer
        (call-process-region (point-min) (point-max)
                             "ruff"            ; command
                             t                 ; delete original text
                             t                 ; output to current buffer
                             nil               ; no redisplay
                             "check" "--fix" "--stdin-filename" (buffer-file-name) "-"))
      (message "Ruff fix applied."))))

;; Tree-sitter
(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-font-lock-level 4)
  (global-treesit-auto-mode))

;; Flymake (Syntax Checking)
(use-package flymake
  :ensure t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-error-bitmap '(flymake-double-exclamation-mark compilation-error))
  (flymake-warning-bitmap '(exclamation-mark compilation-warning))
  (flymake-note-bitmap '(exclamation-mark compilation-info))
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-regexp
   '("^\\([^ :]+\):\\([0-9]+\):\\([0-9]+\): \\(?:.*\\)$"
     1 2 3))

  :init
  ;; Evil bindings for navigation using General
  ;; We use :keymaps 'flymake-mode-map so these only exist when flymake is active
  (general-def
    :states 'normal
    :keymaps 'flymake-mode-map
    "]e" 'flymake-goto-next-error
    "[e" 'flymake-goto-prev-error))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
