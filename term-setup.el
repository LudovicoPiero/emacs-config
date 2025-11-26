;;; term-setup.el --- Terminal configuration -*- lexical-binding: t; -*-

;; Keywords: vterm, terminal, shell

;;; Commentary:
;; Configures Vterm and Vterm-Toggle.

;;; Code:

(use-package vterm
  :ensure t
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  (vterm-timer-delay 0.01)
  :hook
  (vterm-mode . (lambda ()
                  (display-line-numbers-mode -1)
                  (evil-emacs-state))))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :custom
  (vterm-toggle-hide-method 'delete-window)
  (vterm-toggle-scope 'project)

  :init
  (with-eval-after-load 'general
    (my-leader-def
      "t"  '(:ignore t :which-key "toggle/terminal")
      "tt" '(vterm-toggle :which-key "Toggle Terminal")
      "tv" '(vterm :which-key "Open Vterm Buffer")
      "tV" '(vterm-other-window :which-key "Vterm (Split)")

      ;; Other toggles can live here too
      "ts" '(stripspace-local-mode :which-key "Strip Space"))

    :config
    ;; Fix for vterm-toggle display behavior
    (add-to-list 'display-buffer-alist
                 '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                   (display-buffer-reuse-window display-buffer-at-bottom)
                   (reusable-frames . visible)
                   (window-height . 0.3)))))

(provide 'term-setup)
;;; term-setup.el ends here
