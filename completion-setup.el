;;; completion-setup.el --- Completion framework -*- lexical-binding: t; -*-

;; Keywords: completion, corfu, vertico, orderless, embark, consult

;;; Commentary:
;;
;; Configures the Emacs completion frameworks, including Corfu, Cape, Vertico,
;; Orderless, Marginalia, Embark, and Consult for enhanced minibuffer and
;; in-buffer completion.
;;

;;; Code:

;; In-buffer completion (Corfu)
(use-package corfu
  :ensure t
  :hook (elpaca-after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)         ;; Delay before popup (optional, default is 0.2)
  (corfu-auto-prefix 2)          ;; Char count before popup (optional, default is 3)
  (corfu-cycle t)                ;; Enable cycling through candidates
  (corfu-quit-no-match 'separator) ;; Quit if no match

  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete))

;; Extensions for Corfu (Cape)
(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Minibuffer completion UI (Vertico)
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; Fuzzy matching (Orderless)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Rich annotations (Marginalia)
(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (elpaca-after-init . marginalia-mode))

;; Actions on targets (Embark)
(use-package embark
  :ensure t
  :commands (embark-act embark-dwim embark-export embark-collect embark-bindings)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; 1. Global bindings (Available everywhere, including minibuffer)
  (general-def
    "C-."   'embark-act
    "C-;"   'embark-dwim
    "C-h B" 'embark-bindings)

  ;; 2. Leader bindings (Optional, for Normal mode)
  (my-leader-def
    "a" 'embark-act) ;; "Action"

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Search and navigation (Consult)
(use-package consult
  :ensure t
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Define bindings using General (SPC leader)
  (my-leader-def
    ;; Buffers
    "b"  '(:ignore t :which-key "buffer")
    "bb" '(consult-buffer :which-key "Switch buffer")
    "bk" '(kill-current-buffer :which-key "Kill buffer")
    "br" '(revert-buffer :which-key "Revert buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "bp" '(previous-buffer :which-key "Prev buffer")

    ;; Search group (s)
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "Search line")
    "sl" '(consult-line-multi :which-key "Search line (all)")
    "sg" '(consult-ripgrep :which-key "Ripgrep")
    "sG" '(consult-git-grep :which-key "Git grep")
    "sf" '(consult-find :which-key "Find file (fd)")
    "si" '(consult-imenu :which-key "Imenu")
    "sI" '(consult-imenu-multi :which-key "Imenu (all)")
    "sh" '(consult-history :which-key "Command history")
    "sm" '(consult-mark :which-key "Jump to mark")
    "sM" '(consult-global-mark :which-key "Global mark")
    "sk" '(consult-keep-lines :which-key "Keep lines")
    "su" '(consult-focus-lines :which-key "Focus lines")
    "so" '(consult-outline :which-key "Outline")
    "sy" '(consult-yank-pop :which-key "Yank pop")

    ;; Other
    "hM" '(consult-man :which-key "Man pages")
    "hi" '(consult-info :which-key "Info pages"))

  ;; Remap standard bindings in specific maps using general
  (general-def
    :keymaps 'isearch-mode-map
    "M-e" 'consult-isearch-history
    "M-s l" 'consult-line)

  (general-def
    :keymaps 'minibuffer-local-map
    "M-s" 'consult-history
    "M-r" 'consult-history)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(provide 'completion-setup)
;;; completion-setup.el ends here
