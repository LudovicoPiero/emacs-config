;;; git.el --- Git configuration -*- lexical-binding: t -*-

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

;; Git integration and visual diff indicators

;;; Code:

;; Git Porcelain Interface
(use-package magit)

;; Highlight Git changes in fringe and dired
(use-package diff-hl
  :hook ((find-file . global-diff-hl-mode)
         (vc-dir-mode . turn-on-diff-hl-mode)
         (diff-hl-mode . diff-hl-flydiff-mode))

  :init
  (setq-default fringes-outside-margins t)
  (add-hook 'dired-mode-hook
            (lambda ()
              (unless (and (boundp 'diff-hl-disable-on-remote)
                           (file-remote-p default-directory))
                (diff-hl-dired-mode 1))))

  :config
  ;; Various performance and UX tweaks
  (setq diff-hl-global-modes '(not image-mode pdf-view-mode)
        vc-git-diff-switches '("--histogram")
        diff-hl-flydiff-delay 0.5
        diff-hl-update-async t
        diff-hl-show-staged-changes nil)

  ;; Optional: Make diff-hl work in TTY
  (unless (display-graphic-p)
    (add-hook 'global-diff-hl-mode-hook #'diff-hl-margin-mode))

  (with-eval-after-load 'flycheck
    (setq flycheck-indication-mode 'right-fringe)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  (add-hook 'emacs-escape-hook (lambda () (when (bound-and-true-p diff-hl-mode)
                                           (diff-hl-update-once))))

  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'git)
;;; git.el ends here
