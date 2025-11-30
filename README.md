# Modern Emacs Configuration

A minimalist, high-performance Emacs configuration built with `straight.el` and designed to integrate seamlessly with **Nix**.

## 🚀 Features

* **Fast Startup**: Tuned garbage collection and `early-init.el` optimizations.
* **Clean Filesystem**: All backups, save-history, and auto-saves are forced into `~/.emacs.d/var/`. No `~` files cluttering your projects.
* **Modern Navigation**: The "Vertico Stack" (Vertico, Consult, Orderless, Marginalia, Embark).
* **LSP & Treesitter**: Eglot and Treesit-auto configured to use binaries provided by Nix.
* **Git Integration**: Magit, the best Git client in existence.
* **Smart Editing**: Multiple Cursors, Avy (jump), Vundo (undo tree), and Crux utilities.

## 🛠️ Installation

1.  **Clone the repository**:
    ```bash
    git clone git@github.com:LudovicoPiero/emacs-config-private.git ~/.emacs.d
    ```

2.  **Ensure Nix Dependencies**:
    This config relies on external binaries for LSP and formatting. Ensure your Nix shell/wrapper provides:
    * **LSP**: `basedpyright`, `nil` (Nix), `gopls`, `rust-analyzer`, `clangd`, `emmylua-ls`.
    * **Formatters**: `ruff`, `black`, `nixfmt`, `gofumpt`, `stylua`.
    * **Tools**: `ripgrep`, `fd`, `git`.

3.  **Launch Emacs**:
    On first run, `straight.el` will bootstrap itself and compile all packages. This might take a few minutes.

## 📂 Structure

```text
~/.emacs.d/
├── early-init.el       # GUI suppression & GC tuning
├── init.el             # Module loader & path setup
└── lisp/
    ├── init-pkg.el     # Straight.el bootstrap
    ├── init-core.el    # Defaults, file hygiene, Crux, Vundo, Multi-cursors
    ├── init-ui.el      # Doom Theme/Modeline, Fonts
    ├── init-nav.el     # Vertico, Consult, Embark, Avy
    └── init-dev.el     # Eglot, Treesitter, Magit, Apheleia
````

## ⌨️ Keybind Cheatsheet

### The Essentials

| Key | Command | Description |
| :--- | :--- | :--- |
| `M-x` | `execute-extended-command` | Run command (Vertico UI). |
| `C-g` | `keyboard-quit` | **Cancel / Panic**. |
| `C-h` | `help-command` | Help prefix. |
| `C-.` | `embark-act` | **Context Action**. Act on the thing under cursor. |
| `C-x u` | `vundo` | **Visual Undo**. Travel through time. |

### Multiple Cursors

| Key | Command | Description |
| :--- | :--- | :--- |
| `C->` | `mc/mark-next-like-this` | Add cursor to next matching text. |
| `C-<` | `mc/mark-previous-like-this` | Add cursor to previous matching text. |
| `C-c C-<` | `mc/mark-all-like-this` | Select **ALL** matches in buffer. |
| `C-c m` | `mc/edit-lines` | Place cursor on every selected line. |

### Navigation (Avy & Consult)

| Key | Command | Description |
| :--- | :--- | :--- |
| `C-:` | `avy-goto-char` | **Jump**. Type char -\> Type hint -\> Teleport. |
| `C-x b` | `consult-buffer` | **Switch Buffer**. |
| `M-s r` | `consult-ripgrep` | **Ripgrep**. Search text across project. |
| `M-s l` | `consult-line` | **Line Search**. Search inside current buffer. |
| `M-s d` | `consult-find` | **Find File**. Recursive file search. |

### Smart Editing (Crux & Core)

| Key | Command | Description |
| :--- | :--- | :--- |
| `C-a` | `crux-move-beginning-of-line` | Toggle Start of Line / First Char. |
| `Shift-Enter` | `crux-smart-open-line` | Open new line below. |
| `Ctrl-Shift-Enter` | `crux-smart-open-line-above` | Open new line above. |
| `C-k` | `crux-smart-kill-line` | Kill line. |
| `C-c r` | `crux-rename-file-and-buffer` | Rename file instantly. |
| `C-c D` | `crux-delete-file-and-buffer` | Delete file instantly. |
| `C-c s` | `stripspace-strip-buffer` | Strip whitespace. |

### Coding (LSP)

| Key | Command | Description |
| :--- | :--- | :--- |
| `C-c f` | `apheleia-format-buffer` | **Format Code**. |
| `C-c l r` | `eglot-rename` | **Rename Symbol**. |
| `C-c l a` | `eglot-code-actions` | **Code Actions**. |
| `M-n/p` | `flymake-goto-next/prev-error` | Jump to errors. |
| `TAB` | `corfu-next` | Auto-complete. |

### Dired (File Manager)

| Key | Description |
| :--- | :--- |
| `C` | **Copy** (Auto-targets split window). |
| `R` | **Rename/Move** (Auto-targets split window). |
| `D` | **Delete**. |
| `+` | **Create Directory**. |

### Git (Magit)

| Key | Description |
| :--- | :--- |
| `C-x g` | **Magit Status**. |
| `s` / `u` | Stage / Unstage. |
| `c c` | Commit. |
| `P p` | Push. |
