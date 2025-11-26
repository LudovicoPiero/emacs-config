;;; lang.el --- Language specific configs -*- lexical-binding: t; -*-

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
   '("^\\([^ :]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(?:.*\\)$"
     1 2 3))

  :init
  ;; Evil bindings for navigation using General
  ;; We use :keymaps 'flymake-mode-map so these only exist when flymake is active
  (general-def
    :states 'normal
    :keymaps 'flymake-mode-map
    "]e" 'flymake-goto-next-error
    "[e" 'flymake-goto-prev-error))
