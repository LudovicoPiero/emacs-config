;;; init-nav.el --- Vertico, Consult, Avy -*- lexical-binding: t; -*-

(use-package avy
  :commands (avy-goto-char avy-goto-char-2 avy-goto-word-1)
  :general
  (general-def
    "C-:"   #'avy-goto-char
    "C-'"   #'avy-goto-char-2
    "M-g w" #'avy-goto-word-1))

(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-count 15)
  (setq vertico-resize nil)
  (vertico-mode 1)
  (vertico-mouse-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :commands (consult-buffer consult-ripgrep consult-line consult-find consult-imenu consult-recent-file consult-history consult-yank-pop consult-project-buffer)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :general
  ;; Global M-bindings
  (general-def
    "C-x b" #'consult-buffer
    "M-s r" #'consult-ripgrep)

  ;; Leader bindings
  (my-leader-def
    "bb"  #'consult-buffer
    "fr"  #'consult-recent-file

    ;; --- s: SEARCH ---
    "s"   '(:ignore t :which-key "search")
    "ss"  #'consult-line
    "sp"  #'consult-ripgrep
    "sf"  #'consult-find
    "si"  #'consult-imenu
    "sh"  #'consult-history     ; Search command history
    "sk"  #'consult-yank-pop))  ; Search kill-ring (yank history)

(use-package embark
  :commands (embark-act embark-dwim embark-bindings)
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :general
  (general-def
    "C-."   #'embark-act
    "C-;"   #'embark-dwim
    "C-h B" #'embark-bindings))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; --- NEW: Project Bindings (Doom Style) ---
;; Built-in project management (lightweight alternative to Projectile)
(my-leader-def
  "p"   '(:ignore t :which-key "project")
  "pf"  #'project-find-file                ; Find file in project
  "pp"  #'project-switch-project           ; Switch to another project
  "pb"  #'consult-project-buffer           ; Switch to buffer in current project
  "pk"  #'project-kill-buffers             ; Kill all project buffers
  "pr"  #'project-remember-projects-under) ; Discover projects

(provide 'init-nav)
