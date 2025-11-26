;;; ai.el --- AI tools -*- lexical-binding: t; -*-

(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist"))
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-idle-delay 0.5)

  :init
  ;; The "Smart Tab" - The secret sauce
  (defun my/copilot-tab ()
    "Tab priority: Corfu > Copilot > Indent"
    (interactive)
    (cond
     ;; 1. If Corfu is open, let Corfu handle it (select candidate)
     ((and (bound-and-true-p corfu-mode)
           (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (corfu-complete))

     ;; 2. If Copilot has a suggestion, accept it
     ((copilot--overlay-visible)
      (copilot-accept-completion))

     ;; 3. Otherwise, do standard indentation/expansion
     (t
      (indent-for-tab-command))))

  ;; Bindings
  (with-eval-after-load 'general
    (general-def
      :states '(insert emacs)
      :keymaps 'copilot-mode-map
      "<tab>" 'my/copilot-tab
      "TAB"   'my/copilot-tab)))

(provide 'ai)
;;; ai.el ends here
