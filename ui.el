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

(use-package doom-modeline
  :ensure t
  :hook (elpaca-after-init . doom-modeline-mode))

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
