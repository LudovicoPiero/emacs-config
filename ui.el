;;; ui.el --- Visual configuration -*- lexical-binding: t; -*-

;; Keywords: ui, theme, font, modeline

;;; Commentary:
;;
;; Configures the Emacs user interface, including fonts, themes, modeline,
;; and other visual elements.
;;

;;; Code:

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

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character) ;; Use a character like |
  (highlight-indent-guides-character ?\u2502) ;; The character to use
  (highlight-indent-guides-responsive 'top)   ;; Highlight the current block
  (highlight-indent-guides-delay 0))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  ;; Use diff faces so it looks standard
  (evil-goggles-use-diff-faces))

(use-package rainbow-mode
  :ensure t
  :hook ((prog-mode . rainbow-mode)
         (conf-mode . rainbow-mode)))

;; Dired configuration
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))
(with-eval-after-load 'dired
  (let ((args "--group-directories-first -ahlv"))
    (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        (setq args nil)))
    (when args
      (setq dired-listing-switches args))))


(provide 'ui)
;;; ui.el ends here
