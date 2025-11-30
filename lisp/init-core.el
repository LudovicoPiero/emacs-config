;;; init-core.el --- Sane Defaults & Helpers -*- lexical-binding: t; -*-

;; -- EXPAND REGION (Semantic Selection) --
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; -- DRAG STUFF (Move lines up/down) --
(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :bind
  ("M-<up>" . drag-stuff-up)
  ("M-<down>" . drag-stuff-down))

;; -- STRIPSPACE --
(use-package stripspace
  :hook (after-init . global-stripspace-mode)
  :bind ("C-c s" . stripspace-strip-buffer)
  :config
  (setq stripspace-skip-modes '(markdown-mode org-mode conf-mode)))

;; -- VUNDO (Visual Undo) --
(use-package vundo
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; -- MULTIPLE CURSORS --
(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)       ; Add cursor to next match
  ("C-<" . mc/mark-previous-like-this)   ; Add cursor to previous match
  ("C-c C-<" . mc/mark-all-like-this)    ; Add cursor to ALL matches
  ("C-c m" . mc/edit-lines)              ; Add cursor to every line in selection
  :config
  (setq mc/list-file (expand-file-name "mc-lists.el" user-emacs-var-directory)))

;; -- CRUX (Useful Extensions) --
(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line)       ; Toggle between first-char and line-start
  ("C-k" . crux-smart-kill-line)              ; Kill line, or join with next if at end
  ("S-<return>" . crux-smart-open-line)       ; Open line below (smart)
  ("C-S-<return>" . crux-smart-open-line-above) ; Open line above (smart)
  ("C-c D" . crux-delete-file-and-buffer)     ; Delete current file
  ("C-c r" . crux-rename-file-and-buffer))    ; Rename current file

;; -- WHICH-KEY --
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(provide 'init-core)
