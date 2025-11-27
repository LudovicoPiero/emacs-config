;;; core.el --- Core system configuration -*- lexical-binding: t; -*-

;; Keywords: core, system, configuration, packages, files

;;; Commentary:
;;
;; Contains core Emacs system configurations, including package management,
;; auto-revert, recent files, and other fundamental settings.
;;

;;; Code:

;; Allow Emacs to upgrade built-in packages, such as Org mode
(setq package-install-upgrade-built-in t)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Native compilation (compile-angel)
(use-package compile-angel
  :demand t
  :ensure t
  :diminish compile-angel-on-load-mode
  :custom
  (compile-angel-verbose t)
  :config
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode 1))

(use-package diminish
  :ensure t)

;; Auto-revert buffers when files change on disk
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook (elpaca-after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

;; Recent files history
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook (elpaca-after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; Minibuffer history
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook (elpaca-after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring register-alist mark-ring global-mark-ring search-ring regexp-search-ring)))

;; Save cursor place in files
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook (elpaca-after-init . save-place-mode)
  :custom
  (save-place-limit 400))

;; Auto-save configuration
(setq auto-save-default t)
(setq auto-save-interval 300)
(setq auto-save-timeout 30)

;; Org Mode
(use-package org
  :ensure t
  :commands (org-mode org-version)
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-truncated t))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "nt" "xt" "x" "x")) ; Custom bullets
  (org-modern-hide-stars nil) ; Let org-modern handle stars
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))))

;; Markdown
(use-package markdown-mode
  :commands (gfm-mode gfm-view-mode markdown-mode markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  (my-leader-def
    :keymaps 'markdown-mode-map
    "m"  '(:ignore t :which-key "markdown")
    "me" '(markdown-do :which-key "Do/Export")
    "mp" '(markdown-preview :which-key "Preview")
    "ml" '(markdown-live-preview-mode :which-key "Live Preview")))

(provide 'core)
;;; core.el ends here
