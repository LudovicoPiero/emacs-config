;;; dired.el --- Dired configuration -*- lexical-binding: t -*-

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

;; commentary

;;; Code:

(use-package dired
  :ensure nil  ; dired is built-in
  :commands (dired dired-jump)
  :hook ((dired-mode . my/dired-disable-gnu-ls-flags-maybe))
  :bind (:map dired-mode-map
              ("C-c C-e" . wdired-change-to-wdired-mode))

  :init
  ;; General settings
  (setq dired-dwim-target t
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask)

  ;; Allow reuse of Dired buffers
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Setup ls args and fallback for BSD
  (let ((args '("-ahl" "-v" "--group-directories-first")))
    (cond
     ((and (eq system-type 'berkeley-unix) (executable-find "gls"))
      (setq insert-directory-program (executable-find "gls")))
     ((eq system-type 'berkeley-unix)
      (setq args (list "-ahl"))))
    (setq dired-listing-switches (mapconcat #'identity args " "))
    (setq my/dired-ls-args args))

  ;; image-dired paths and thumbnail size
  (setq image-dired-dir (expand-file-name "image-dired/" user-emacs-directory)
        image-dired-db-file (expand-file-name "db.el" image-dired-dir)
        image-dired-gallery-dir (expand-file-name "gallery/" image-dired-dir)
        image-dired-temp-image-file (expand-file-name "temp-image" image-dired-dir)
        image-dired-temp-rotate-image-file (expand-file-name "temp-rotate-image" image-dired-dir)
        image-dired-thumb-size 150)

  ;; Optional: display popup for image-dired buffers
  (add-to-list 'display-buffer-alist
               '("^\\*image-dired"
                 (display-buffer-reuse-window display-buffer-pop-up-window)
                 (window-height . 0.8)
                 (inhibit-same-window . t)))

  ;; Optional: ESC exits wdired (if you define escape-hook)
  (defvar escape-hook nil
    "Hook run when ESC is pressed (Doom-style).")
  (define-key global-map [escape]
    (lambda ()
      (interactive)
      (run-hook-with-args-until-success 'escape-hook)))

  (add-hook 'escape-hook #'my/wdired-exit-on-escape)

  :config
  (defun my/dired-disable-gnu-ls-flags-maybe ()
    "Avoid unsupported ls switches in remote or non-GNU environments."
    (when (or (file-remote-p default-directory)
              (and (boundp 'ls-lisp-use-insert-directory-program)
                   (not ls-lisp-use-insert-directory-program)))
      (setq-local dired-actual-switches (car my/dired-ls-args))))

  (defun my/wdired-exit-on-escape ()
    "Exit wdired-mode cleanly on ESC."
    (when (eq major-mode 'wdired-mode)
      (wdired-exit)
      t))

  (defun my/dired-no-revert-in-virtual-buffers (&rest _)
    "Avoid reverting virtual dired buffers."
    (not (eq revert-buffer-function #'dired-virtual-revert)))
  (advice-add 'dired-buffer-stale-p :before-while #'my/dired-no-revert-in-virtual-buffers))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package dired-open
  :config
  ;; Customize file associations for opening files in Dired
  (setq dired-open-extensions '(("gif" . "imv")
                                ("jpg" . "imv")
                                ("png" . "imv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
  ;; Customize key bindings for peep-dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

(provide 'dired)
;;; dired.el ends here
