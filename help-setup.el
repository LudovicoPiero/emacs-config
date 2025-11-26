;;; help-setup.el --- Documentation tools -*- lexical-binding: t; -*-

;; Keywords: help, documentation, helpful

;;; Commentary:
;;
;; Configures various help and documentation tools, such as Helpful, to improve
;; access to Emacs's built-in help system.
;;

;;; Code:

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-key helpful-command helpful-at-point helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

(provide 'help-setup)
;;; help-setup.el ends here