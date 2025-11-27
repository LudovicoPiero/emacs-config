;;; editing.el --- Editing tools -*- lexical-binding: t; -*-

;; Keywords: editing, snippets, whitespace

;;; Commentary:
;;
;; Configures various editing tools, including whitespace handling, snippets,
;; and general editing enhancements.
;;

;;; Code:

(use-package stripspace
  :diminish stripspace-local-mode
  :ensure t
  :commands stripspace-local-mode
  :hook (find-file . stripspace-local-mode)
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t)

  :config
  (with-eval-after-load 'general
    (my-leader-def
      "ts" '(stripspace-local-mode :which-key "Toggle Strip Space"))))

;; Snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package doom-snippets
  :ensure (:host github :repo "doomemacs/snippets" :files ("*.el" "snippets"))
  :after yasnippet
  :config
  (add-to-list 'yas-snippet-dirs
               (file-name-concat (file-name-directory (locate-library "doom-snippets")) "snippets"))
  (yas-reload-all))

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode yas-global-mode)
  :hook (elpaca-after-init . yas-global-mode)
  :custom
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)
  (yas-wrap-around-region nil)
  :init
  (setq yas-verbosity 0)
  :config
  (with-eval-after-load 'general
    (my-leader-def
      "is" '(yas-insert-snippet :which-key "Insert snippet")))
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" minimal-emacs-user-directory))
  (yas-reload-all))

(provide 'editing)
;;; editing.el ends here
