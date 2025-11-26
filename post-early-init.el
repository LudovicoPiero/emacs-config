;;; post-early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; Keywords: configuration, early-init, startup, packages

;;; Commentary:
;;
;; Contains settings applied after very early Emacs initialization, often
;; related to package archive priorities.
;;

;;; Code:

;; Obtain the latest packages from MELPA to access new features and
;; improvements. While MELPA packages are generally regarded as less stable,
;; actual breakages are uncommon; over the past year, only a single package
;; (package-lint) out of 146 packages in the minimal-emacs.d author’s
;; configuration experienced a brief disruption, which was quickly resolved.
(setq package-archive-priorities '(("melpa"        . 90)
                                   ("gnu"          . 70)
                                   ("nongnu"       . 60)
                                   ("melpa-stable" . 50)))

;; NOTE: example if any package breakage happen
;; (setq package-pinned-packages
;;       '((evil            . "melpa-stable")
;;         (evil-collection . "melpa-stable")))

(provide 'post-early-init)
;;; post-early-init.el ends here