;;; languages.el --- Programming languages configuration -*- lexical-binding: t -*-

;; Author: Ludovico Piero <lewdovico@gnuweeb.org>
;; Version: 0.1.1

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

;; TODO

;;; Code:

(use-package eglot
  :ensure t

  :custom
  (eglot-autoshutdown t)

  :config
  ;; Nix LSP server
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  ;; Python: using basedpyright (you could swap with ruff-lsp if desired)
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("basedpyright")))
  ;; Rust: using rust-analyzer
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  ;; JavaScript / ESLint
  (add-to-list 'eglot-server-programs '(js-ts-mode . ("vscode-eslint-language-server" "--stdio")))
  ;; TypeScript
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  ;; (Optional) For CSS and HTML, you can add a stylelint server:
  (add-to-list 'eglot-server-programs '(css-mode . ("vscode-stylelint" "--stdio")))
  (add-to-list 'eglot-server-programs '(web-mode . ("vscode-stylelint" "--stdio"))))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)))

(add-hook 'sh-mode-hook #'flycheck-mode)

;; Nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure))

;; Python
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook (python-ts-mode . eglot-ensure))

(with-eval-after-load 'flycheck
  (flycheck-define-checker python-ruff
    "A Python linter using ruff."
    :command ("ruff" "check" source)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes (python-ts-mode))
  (add-to-list 'flycheck-checkers 'python-ruff))

;; Rust
(use-package rust-mode
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . eglot-ensure))

(with-eval-after-load 'eglot
  ;; Tell rust-analyzer to run clippy on save.
  (setq eglot-workspace-configuration
        '((:rust-analyzer .
           ((checkOnSave . (:command "clippy")))))))

;; Web development
(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.php\\'")
  :hook (web-mode . eglot-ensure)
  :config
  (setq web-mode-enable-current-column-highlight t
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package css-mode
  :ensure nil
  :mode "\\.css\\'"
  :hook (css-mode . eglot-ensure))

(use-package js
  :ensure nil
  :mode ("\\.js\\'" . js-ts-mode)
  :hook (js-ts-mode . eglot-ensure)
  :config
  (setq js-indent-level 2))

(use-package typescript-mode
  :ensure nil
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :hook (typescript-ts-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 2))

(provide 'languages)
;;; languages.el ends here
