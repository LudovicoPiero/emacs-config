;;; init-keys.el --- Evil & General Keybinds -*- lexical-binding: t; -*-

;; -- EVIL MODE (Vim Bindings) --
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ;; Required for evil-collection
  (setq evil-want-C-u-scroll t)   ;; Nice to have
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; -- GENERAL (Key Definitions) --
(use-package general
  :after evil
  :config
  ;; Define the "SPC" leader key
  (general-create-definer my-leader-def
    :prefix "SPC"
    :states '(normal visual motion)
    :keymaps 'override)

  ;; Define "SPC m" for local leader (major mode specific)
  (general-create-definer my-local-leader-def
    :prefix "SPC m"
    :states '(normal visual motion)
    :keymaps 'override)

  (my-leader-def
    ""     nil
    "SPC" '(execute-extended-command :which-key "M-x")
    "u"   '(vundo :which-key "undo tree")
    "g"   '(magit-status :which-key "git status")

    ;; --- s: SEARCH ---
    "s"   '(:ignore t :which-key "search")
    "ss"  '(consult-line :which-key "buffer text")
    "sp"  '(consult-ripgrep :which-key "project text")
    "sf"  '(consult-find :which-key "find file in project")
    "si"  '(consult-imenu :which-key "jump to symbol")

    ;; --- c: CODE (LSP) ---
    "c"   '(:ignore t :which-key "code")
    "ca"  '(eglot-code-actions :which-key "action")
    "cr"  '(eglot-rename :which-key "rename")
    "cf"  '(apheleia-format-buffer :which-key "format")
    "cd"  '(eldoc :which-key "doc")
    "ce"  '(consult-flymake :which-key "list errors")

    ;; --- t: TOGGLE ---
    "t"   '(:ignore t :which-key "toggle")
    "tt"  '(consult-theme :which-key "theme")
    "tl"  '(display-line-numbers-mode :which-key "line numbers")
    "tw"  '(toggle-word-wrap :which-key "word wrap")

    ;; --- b: BUFFERS ---
    "b"   '(:ignore t :which-key "buffer")
    "bb"  '(consult-buffer :which-key "switch")
    "bk"  '(kill-current-buffer :which-key "kill")
    "br"  '(revert-buffer :which-key "revert")
    "bn"  '(next-buffer :which-key "next")
    "bp"  '(previous-buffer :which-key "prev")

    ;; --- x: DIAGNOSTICS ---
    "x"   '(:ignore t :which-key "diagnostics")
    "xx"  '(consult-flymake :which-key "list errors")
    "xn"  '(flymake-goto-next-error :which-key "next error")
    "xp"  '(flymake-goto-prev-error :which-key "prev error")
    "xb"  '(flymake-show-buffer-diagnostics :which-key "buffer diag")

    ;; --- f: FILES ---
    "f"   '(:ignore t :which-key "file")
    "ff"  '(find-file :which-key "find")
    "fr"  '(consult-recent-file :which-key "recent")
    "fs"  '(save-buffer :which-key "save")
    "fD"  '(crux-delete-file-and-buffer :which-key "delete")
    "fR"  '(crux-rename-file-and-buffer :which-key "rename")
    "fc"  '((lambda () (interactive) (find-file (expand-file-name "init-keys.el" (concat user-emacs-directory "lisp/")))) :which-key "edit keys")
  )

  ;; --- Local Leader (Major Mode Specific) ---
  ;; Go
  (my-local-leader-def
    :keymaps 'go-ts-mode-map
    "t" '(go-test-current-test :which-key "test func")
    "r" '(go-run :which-key "run"))

  ;; C/C++
  (my-local-leader-def
    :keymaps '(c-mode-map c++-mode-map c++-ts-mode-map)
    "c" '(compile :which-key "compile")
    "d" '(gdb :which-key "debug"))

  ;; Nix
  (my-local-leader-def
    :keymaps 'nix-mode-map
    "b" '(nix-build-buffer :which-key "build"))
)

(provide 'init-keys)
