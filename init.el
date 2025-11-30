;;; init.el --- Entry Point -*- lexical-binding: t; -*-

;; Garbage Collection hack for startup (reset in hook)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

;; -- Load Path Setup --
(defconst my-lisp-dir (expand-file-name "lisp" user-emacs-directory))

;; Sanity Check: Ensure the directory actually exists
(unless (file-directory-p my-lisp-dir)
  (error "CRITICAL: Directory '%s' is missing. Create it and move init-*.el files there." my-lisp-dir))

(add-to-list 'load-path my-lisp-dir)

;; -- Module Loader --
;; Use a simple catch to print better errors if a specific module fails
(condition-case err
    (progn
      (require 'init-pkg)   ; Bootstrap Straight & Use-package
      (require 'init-core)  ; Better defaults
      (require 'init-ui)    ; Theme & Fonts
      (require 'init-nav)   ; Navigation
      (require 'init-dev))  ; Coding tools
  (error
   (message "Failed to load config module: %s" err)))

(message "System Online.")
