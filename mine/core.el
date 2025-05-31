;;; core.el --- Core packages -*- lexical-binding: t -*-

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

(use-package diminish)

(use-package direnv
  :config
  (setq direnv-always-show-summary nil)
  ;; Enable direnv mode globally
  (direnv-mode))

(use-package flycheck
  :after eglot
  :diminish flycheck-mode
  :init (global-flycheck-mode))

(use-package flycheck-eglot
  :after flycheck
  :config (global-flycheck-eglot-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)

  ;; Set your preferred key bindings here
  :bind (("C-c p" . projectile-command-map))

  ;; Additional settings
  :custom
  ;; Define your project root files/directories here
  (projectile-project-root-files '(".projectile" ".git" ".svn" ".hg" "Makefile" "package.json"))

  ;; Enable caching to improve performance
  (projectile-enable-caching t)

  ;; Configure indexing method (default is 'alien for faster indexing)
  (projectile-indexing-method 'alien)

  ;; Display project name in the modeline
  (projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name)))))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0))

(provide 'core)
;;; core.el ends here
