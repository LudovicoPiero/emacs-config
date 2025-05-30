;;; org.el --- ORG Configuration -*- lexical-binding: t -*-

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

;; This file configures `org-mode` for a modern, visually clean, and
;; Evil-friendly Org experience. It enhances both the aesthetics and
;; editing ergonomics of Org documents, while remaining minimal and
;; extensible.

;;; Code:

(use-package org
  :init
  (global-org-modern-mode))

(use-package org-modern
  :config
  ;; Behavior settings
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-agenda-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        ;; Visual/style settings
        org-startup-folded 'content
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'org)
;;; org.el ends here
