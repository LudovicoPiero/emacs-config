;;; git.el --- Git configuration -*- lexical-binding: t; -*-

;; Magit requires a newer version of transient than what is built-in.
(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :init
  ;; We define keys in :init so they are available BEFORE Magit loads.
  ;; Pressing these keys will trigger Magit to load.
  (my-leader-def
    "g"  '(:ignore t :which-key "git")
    "gg" '(magit :which-key "Open Magit")
    "gs" '(magit-status :which-key "Status")
    "gd" '(magit-diff-unstaged :which-key "Diff unstaged")
    "gc" '(magit-branch-or-checkout :which-key "Checkout")
    "gl" '(:ignore t :which-key "log")
    "glc" '(magit-log-current :which-key "Log current")
    "glf" '(magit-log-buffer-file :which-key "Log file")
    "gb" '(magit-blame :which-key "Blame")
    "gp" '(magit-push :which-key "Push")
    "gf" '(magit-fetch :which-key "Fetch")))

(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))
