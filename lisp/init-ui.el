;;; init-ui.el --- Visuals -*- lexical-binding: t; -*-

;; -- Font --
(set-face-attribute 'default nil :font "Iosevka q SemiBold" :height 140)

;; -- Theme --
(use-package catppuccin-theme
  :init
  ;; flavors: latte, frappe, macchiato, mocha
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

;; -- Modeline --
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  ;; Mimic Doom Emacs Defaults
  (setq doom-modeline-height 35)      ; slightly taller for the "fat" look
  (setq doom-modeline-bar-width 4)    ; The colored bar on the left
  (setq doom-modeline-hud nil)        ; weird visual hud, usually off
  (setq doom-modeline-window-width-limit fill-column)

  ;; File name style: /p/t/project/src/main.c
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

  ;; Icons
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)

  ;; Segments (declutter)
  (setq doom-modeline-minor-modes nil) ; hide minor modes (clutter)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-checker-simple t) ; simple checks (e.g. flymake)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-lsp t))          ; Show LSP status

;; -- Line Numbers --
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; -- Rainbow Delimiters (Helpful for Lisp) --
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-ui)
