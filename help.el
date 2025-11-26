;;; help.el --- Documentation tools -*- lexical-binding: t; -*-

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
