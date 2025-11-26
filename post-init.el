;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; Load Core Systems first (compilation, file handling)
(minimal-emacs-load-user-init "core.el")

;; Load Evil Mode (Must load before some other modes if keybindings depend on it)
(minimal-emacs-load-user-init "evil.el")

;; Load UI components (Themes, Fonts)
(minimal-emacs-load-user-init "ui.el")

;; Load Editing Utilities (Undo, Snippets, Whitespace)
(minimal-emacs-load-user-init "editing.el")

;; Load Completion Framework (Vertico, Corfu)
(minimal-emacs-load-user-init "completion.el")

;; Load Help Tools
(minimal-emacs-load-user-init "help.el")

;; Load Language Specifics
(minimal-emacs-load-user-init "lang.el")

;; Load LSP Configuration
(minimal-emacs-load-user-init "eglot.el")

;; Load Git Configuration
(minimal-emacs-load-user-init "git.el")
