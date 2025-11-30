;;; init-nav.el --- Vertico, Consult, Avy & Orderless -*- lexical-binding: t; -*-

;; -- AVY (Jump Navigation) --
(use-package avy
  :bind
  ("C-:" . avy-goto-char)       ; Jump to char
  ("C-'" . avy-goto-char-2)     ; Jump to two chars
  ("M-g w" . avy-goto-word-1))  ; Jump to word

;; -- VERTICO (The UI) --
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

;; -- ORDERLESS (Matching) --
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; -- MARGINALIA (Annotations) --
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; -- CONSULT (Commands) --
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("C-c h" . consult-history)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; -- EMBARK (Context Actions) --
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-nav)
