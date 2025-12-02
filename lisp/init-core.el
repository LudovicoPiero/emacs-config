;;; init-core.el --- Sane Defaults & Helpers -*- lexical-binding: t; -*-

(use-package expand-region
  :commands (er/expand-region)
  :general
  (general-def
    "C-=" #'er/expand-region))

(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :general
  (general-def
    "M-<up>"   #'drag-stuff-up
    "M-<down>" #'drag-stuff-down))

(use-package stripspace
  :hook (after-init . stripspace-global-mode)
  :config (setq stripspace-skip-modes '(markdown-mode org-mode conf-mode))
  :general
  (general-def
    "C-c s" #'stripspace-cleanup))

(use-package vundo
  :commands (vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols)
  :general
  (general-def
    "C-x u" #'vundo)
  (my-leader-def
    "u" #'vundo))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this mc/edit-lines)
  :config (setq mc/list-file (expand-file-name "mc-lists.el" user-emacs-var-directory))
  :general
  (general-def
    "C->"     #'mc/mark-next-like-this
    "C-<"     #'mc/mark-previous-like-this
    "C-c C-<" #'mc/mark-all-like-this
    "C-c m"   #'mc/edit-lines))

(use-package crux
  :commands (crux-move-beginning-of-line crux-smart-kill-line crux-smart-open-line crux-smart-open-line-above crux-delete-file-and-buffer crux-rename-file-and-buffer)
  :general
  (general-def
    "C-a"            #'crux-move-beginning-of-line
    "C-k"            #'crux-smart-kill-line
    "S-<return>"     #'crux-smart-open-line
    "C-S-<return>"   #'crux-smart-open-line-above)

  (my-leader-def
    "fD" #'crux-delete-file-and-buffer
    "fR" #'crux-rename-file-and-buffer))

(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

;; --- Help Mappings (Doom Style) ---
(my-leader-def
  "h"   '(:ignore t :which-key "help")
  "hf"  #'describe-function
  "hv"  #'describe-variable
  "hk"  #'describe-key
  "hm"  #'describe-mode
  "hb"  #'describe-bindings
  "hi"  #'info
  "hI"  #'info-display-manual)

(provide 'init-core)
