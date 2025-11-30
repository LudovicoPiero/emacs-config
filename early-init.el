;;; early-init.el --- Early Initialization  -*- lexical-binding: t; -*-

;; Garbage collection tuning for startup speed
(setq gc-cons-threshold most-positive-fixnum)

;; Kill the UI elements immediately to avoid visual noise/flicker
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; -- Silence Native Compilation (Modern Emacs) --
;; Don't spam warnings buffer when compiling asynchronously
(setq native-comp-async-report-warnings-errors nil)

;; Fundamental inhibition
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message "user")
