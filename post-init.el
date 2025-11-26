;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; Keywords: configuration, startup

;;; Commentary:
;;
;; Loads various user configuration files after Emacs initialization.
;;

;;; Code:

;; Load Core Systems first (compilation, file handling)
(minimal-emacs-load-user-init "core.el")

;; Load Evil Mode (Must load before some other modes if keybindings depend on it)
(minimal-emacs-load-user-init "evil-setup.el")

;; Load UI components (Themes, Fonts)
(minimal-emacs-load-user-init "ui.el")

;; Load Editing Utilities (Undo, Snippets, Whitespace)
(minimal-emacs-load-user-init "editing.el")

;; Load Completion Framework (Vertico, Corfu)
(minimal-emacs-load-user-init "completion-setup.el")

;; Load Help Tools
(minimal-emacs-load-user-init "help-setup.el")

;; Load Language Specifics
(minimal-emacs-load-user-init "lang.el")

;; Load LSP Configuration
(minimal-emacs-load-user-init "lsp-setup.el")

;; Load Git Configuration
(minimal-emacs-load-user-init "git-setup.el")

;; Load AI configuration
(minimal-emacs-load-user-init "ai.el")

;; Load Vterm configuration
(minimal-emacs-load-user-init "term-setup.el")

(provide 'post-init)
;;; post-init.el ends here
