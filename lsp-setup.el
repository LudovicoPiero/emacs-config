;;; eglot.el --- Eglot configs -*- lexical-binding: t; -*-

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
