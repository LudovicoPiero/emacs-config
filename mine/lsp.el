;;; lsp.el --- LSP Configuration -*- lexical-binding: t -*-

;; Author: Ludovico Piero <lewdovico@gnuweeb.org>
;; Version: 0.0.1

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file sets up Language Server Protocol (LSP) integration for multiple
;; programming languages using `lsp-mode` and related packages. It aims to
;; provide a consistent, language-agnostic development environment with
;; features like real-time diagnostics, code navigation, and on-save
;; formatting.

;; Key components:
;;
;; - `lsp-mode` is configured for on-demand activation with `lsp-deferred`,
;;   currently hooked into `nix-mode` and extended per-language.
;; - `lsp-ui` is enabled globally for in-buffer documentation popups and
;;   sideline diagnostics.
;; - `flycheck` provides external syntax checking in all `prog-mode` buffers.

;; This file is modular and expandable — simply add new language-specific
;; blocks to extend LSP support.

;;; Code:

(use-package lsp-mode
  :ensure t
  :hook ((nix-mode . lsp-deferred))
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

;;;; Global LSP UI Enhancements ;;;;
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

;;;; Nix ;;;;
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;;;; Python ;;;;
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 88))

;;;; Go ;;;;
(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package gotest
  :ensure t)

;;;; Rust ;;;;
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package rustic
  :ensure t
  :after rust-mode
  :custom
  (rustic-format-on-save t))

(provide 'lsp)
;;; lsp.el ends here
