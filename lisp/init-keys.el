;;; init-keys.el --- Bindings -*- lexical-binding: t; -*-

(use-package general
  :ensure t
  :config

  ;; -- MAIN LEADER (M-SPC) --
  ;; M-SPC is the prefix. Fast to hit with thumb.
  (general-create-definer my-leader-def
    :prefix "M-SPC"
    :keymaps 'override)

  (my-leader-def
    ""     nil

    ;; Top level junk
    "SPC" '(execute-extended-command :which-key "M-x")
    "."   '(find-file :which-key "find file")
    "u"   '(vundo :which-key "undo tree")

    ;; Files
    "f"   '(:ignore t :which-key "file")
    "ff"  '(find-file :which-key "find")
    "fr"  '(consult-recent-file :which-key "recent")
    "fs"  '(save-buffer :which-key "save")
    "fD"  '(crux-delete-file-and-buffer :which-key "del file")
    "fR"  '(crux-rename-file-and-buffer :which-key "mv file")
    "fc"  '((lambda () (interactive) (find-file (expand-file-name "post-init.el" user-emacs-directory))) :which-key "config")

    ;; Buffers
    "b"   '(:ignore t :which-key "buffer")
    "bb"  '(consult-buffer :which-key "switch")
    "bk"  '(kill-current-buffer :which-key "kill")
    "br"  '(revert-buffer :which-key "reload")
    "bn"  '(next-buffer :which-key "next")
    "bp"  '(previous-buffer :which-key "prev")

    ;; Search
    "s"   '(:ignore t :which-key "search")
    "ss"  '(consult-line :which-key "grep buffer")
    "sp"  '(consult-ripgrep :which-key "grep project") ;; usage: type search term, hit enter
    "sf"  '(consult-find :which-key "find file")
    "si"  '(consult-imenu :which-key "jump symbol")

    ;; Code / LSP
    "c"   '(:ignore t :which-key "code")
    "ca"  '(eglot-code-actions :which-key "actions")
    "cr"  '(eglot-rename :which-key "rename")
    "cf"  '(apheleia-format-buffer :which-key "format")
    "ce"  '(consult-flymake :which-key "errors")

    ;; Git
    "g"   '(:ignore t :which-key "git")
    "gg"  '(magit-status :which-key "status")
    "gl"  '(magit-log-current :which-key "log")
    "gb"  '(magit-blame :which-key "blame")

    ;; Windows
    "w"   '(:ignore t :which-key "window")
    "w/"  '(split-window-right :which-key "split v")
    "w-"  '(split-window-below :which-key "split h")
    "wd"  '(delete-window :which-key "close")
    "wo"  '(delete-other-windows :which-key "only this")
    "wm"  '(maximize-window :which-key "max")
  )

  ;; -- MODE SPECIFIC (C-c) --
  ;; These attach to the specific map.
  ;; e.g., in Go mode, C-c t runs the test.

  ;; Go
  (general-define-key
   :keymaps 'go-ts-mode-map
   :prefix "C-c"
   "t"  '(go-test-current-test :which-key "test func")
   "f"  '(go-test-current-file :which-key "test file")
   "r"  '(go-run :which-key "run")
   "d"  '(godoc-at-point :which-key "doc"))

  ;; C / C++
  (general-define-key
   :keymaps '(c-mode-map c++-mode-map c++-ts-mode-map)
   :prefix "C-c"
   "c"  '(compile :which-key "make")
   "r"  '(recompile :which-key "remake")
   "d"  '(gdb :which-key "debug"))

  ;; Nix
  (general-define-key
   :keymaps 'nix-mode-map
   :prefix "C-c"
   "f"  '(nix-mode-format :which-key "fmt")
   "b"  '(nix-build-buffer :which-key "build"))
)

(provide 'init-keys)
