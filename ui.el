;;; ui.el --- Visual configuration -*- lexical-binding: t; -*-

;; Fonts
(set-face-attribute 'default nil
                    :height 130 :weight 'semi-bold :family "Iosevka q")

;; Line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

;; Display line numbers in buffer
(setq-default display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

;; Doom Themes
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-one")
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :hook (elpaca-after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 30)      ;; How tall the bar is
  (doom-modeline-bar-width 4)    ;; The little blue bar on the left
  (doom-modeline-icon t)         ;; Enable icons
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil) ;; Hide minor modes to keep it clean
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project) ;; Smart path display

  ;; Integration settings
  (doom-modeline-lsp t)          ;; Show LSP status
  (doom-modeline-github t)       ;; Show GitHub notifications
  (doom-modeline-modal-icon t))  ;; Show Evil state icon

;; Vim-like Tab Bar
(use-package vim-tab-bar
  :ensure t
  :commands vim-tab-bar-mode
  :hook (elpaca-after-init . vim-tab-bar-mode))

;; Which Key: shows a popup of available keybindings
(use-package which-key
  :ensure t
  :hook (elpaca-after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-allow-imprecise-window-fit nil))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
