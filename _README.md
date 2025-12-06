# Modern Emacs Configuration

A minimalist, high-performance Emacs configuration built with `straight.el` and designed to integrate seamlessly with **Nix**.

## 🚀 Features

- **Fast Startup**: Tuned garbage collection and `early-init.el` optimizations.
- **The "Jail" Architecture**: All plugins, backups, and save files are strictly isolated in `~/.emacs.d/var/`. The root directory stays clean.
- **Modern Navigation**: The "Vertico Stack" (Vertico, Consult, Orderless, Marginalia, Embark).
- **LSP & Treesitter**: Eglot and Treesit-auto configured to use binaries provided by Nix.
- **Project Workflow**: Built-in `project.el` enhanced with bulk search/replace and compilation tools.
- **Git Integration**: Magit with Git Gutter (diff-hl) in the sidebar.
- **Vim Bindings**: Evil Mode + Evil Collection + General.el for a complete modal editing experience.
- **Visuals**: Catppuccin theme, Doom Modeline, Dashboard start screen, and Nerd Fonts.

## 🛠️ Installation

1.  **Clone the repository**:

    ```bash
    git clone git@github.com:LudovicoPiero/emacs-config.git ~/.emacs.d
    ```

2.  **Ensure Nix Dependencies**:
    This config relies on external binaries for LSP and formatting. Ensure your Nix shell/wrapper provides:
    - **LSP**: `basedpyright`, `nil` (Nix), `gopls`, `rust-analyzer`, `clangd`, `emmylua-ls`.
    - **Formatters**: `ruff`, `black`, `nixfmt`, `gofumpt`, `stylua`.
    - **Tools**: `ripgrep`, `fd`, `git`, `cmake`, `libtool` (for Vterm).

3.  **Launch Emacs**:
    On first run, `straight.el` will bootstrap itself and compile all packages. This might take a few minutes.

## 📂 Structure

```text
~/.emacs.d/
├── early-init.el         # GUI suppression & GC tuning
├── init.el               # Module loader
├── pre-early-init.el     # The "Jail" setup (Redirects vars to var/)
├── lisp/
│   ├── init-pkg.el       # Straight.el & General.el (Leader setup)
│   ├── init-core.el      # Defaults, Crux, Vundo, Multi-cursors
│   ├── init-ui.el        # Theme, Modeline, Dashboard, Visuals
│   ├── init-nav.el       # Vertico, Consult, Embark
│   ├── init-dev.el       # Projects, Eglot, Treesitter, Magit, Terminal
│   └── init-keys.el      # Evil setup & Global/Orphan keys
└── var/                  # AUTOMATICALLY CREATED (The Jail)
    ├── elpa/             # Packages
    ├── straight/         # Packages
    ├── undo/             # Undo history
    └── auto-save/        # Auto saves
````

## ⌨️ Keybind Cheatsheet

### 🟢 Leader Key (`SPC`)

#### General & Session

| Key       | Command                     | Description                    |
| :-------- | :-------------------------- | :----------------------------- |
| `SPC SPC` | `execute-extended-command`  | **M-x**. Run any command.      |
| `SPC TAB` | `mode-line-other-buffer`    | **Last Buffer**. Quick switch. |
| `SPC .`   | `find-file`                 | **Find File** (Quick).         |
| `SPC /`   | `evil-commentary-line`      | **Toggle Comment**.            |
| `SPC u`   | `vundo`                     | **Undo Tree**. Travel history. |
| `SPC q q` | `save-buffers-kill-term...` | **Quit** Emacs.                |
| `SPC q R` | `restart-emacs`             | **Restart** Emacs.             |

#### 📂 Files (`SPC f`)

| Key       | Command               | Description                 |
| :-------- | :-------------------- | :-------------------------- |
| `SPC f f` | `find-file`           | Open file.                  |
| `SPC f r` | `consult-recent-file` | Open recent file.           |
| `SPC f s` | `save-buffer`         | Save current file.          |
| `SPC f y` | *(lambda)*            | **Yank** file path.         |
| `SPC f D` | `crux-delete-file...` | **Delete** file and buffer. |
| `SPC f R` | `crux-rename-file...` | **Rename** file and buffer. |
| `SPC f c` | *(lambda)*            | Edit `init-keys.el`.        |

#### 📑 Buffers (`SPC b`)

| Key         | Command                | Description              |
| :---------- | :--------------------- | :----------------------- |
| `SPC b b`   | `consult-buffer`       | **Switch Buffer**.       |
| `SPC b B`   | `switch-to-buffer`     | Switch (Fallback).       |
| `SPC b k`   | `kill-current-buffer`  | Kill buffer.             |
| `SPC b r`   | `revert-buffer`        | Reload buffer from disk. |
| `SPC b n/p` | `next/previous-buffer` | Cycle buffers.           |
| `SPC b Y`   | *(lambda)*             | **Yank** buffer name.    |
| `SPC b N`   | `make-empty-file...`   | Create new empty buffer. |

#### 🚀 Projects (`SPC p`)

| Key       | Command                    | Description                    |
| :-------- | :------------------------- | :----------------------------- |
| `SPC p p` | `project-switch-project`   | **Switch Project**.            |
| `SPC p f` | `project-find-file`        | Find file in project.          |
| `SPC p b` | `consult-project-buffer`   | Switch buffer in project.      |
| `SPC p c` | `project-compile`          | **Compile** (Make/Cargo/etc).  |
| `SPC p s` | `project-find-regexp`      | **Search** Text (Grep).        |
| `SPC p r` | `project-query-replace...` | **Replace** Text (Refactor).   |
| `SPC p t` | `project-shell`            | Open Shell in Root.            |
| `SPC p g` | `magit-project-status`     | Open Magit in Root.            |
| `SPC p D` | `project-remember...`      | **Discover** new projects.     |
| `SPC p x` | `project-forget-project`   | **Forget** project.            |

#### 🔍 Search (`SPC s`)

| Key       | Command            | Description                   |
| :-------- | :----------------- | :---------------------------- |
| `SPC s s` | `consult-line`     | **Line Search** (Buffer).     |
| `SPC s p` | `consult-ripgrep`  | **Project Search** (Ripgrep). |
| `SPC s f` | `consult-find`     | **Find File** (Recursive).    |
| `SPC s i` | `consult-imenu`    | **Jump to Symbol** (Buffer).  |
| `SPC s h` | `consult-history`  | Search command history.       |
| `SPC s k` | `consult-yank-pop` | Search kill ring (Clipboard). |

#### 🌲 Git (`SPC g`)

| Key       | Command               | Description                    |
| :-------- | :-------------------- | :----------------------------- |
| `SPC g g` | `magit-status`        | **Git Status**.                |
| `SPC g f` | `magit-file-dispatch` | **File Actions** (Log, Trace). |
| `SPC g b` | `magit-blame`         | Toggle Blame.                  |

#### 💻 Code & LSP (`SPC c`)

| Key       | Command              | Description               |
| :-------- | :------------------- | :------------------------ |
| `SPC c a` | `eglot-code-actions` | **Code Actions** (Fixes). |
| `SPC c r` | `eglot-rename`       | **Rename Symbol**.        |
| `SPC c d` | `eldoc`              | **Show Docs**.            |
| `SPC c f` | `apheleia-format...` | **Format Buffer**.        |
| `SPC c e` | `consult-flymake`    | **List Errors**.          |

#### 📝 Snippets (`SPC i`)

| Key       | Command            | Description         |
| :-------- | :----------------- | :------------------ |
| `SPC i s` | `yas-insert-snippet` | **Insert** snippet. |
| `SPC i n` | `yas-new-snippet`    | Create new snippet. |
| `SPC i v` | `yas-visit-snippet-file` | Edit snippet.   |

#### 🖥️ UI & Toggles (`SPC t`)

| Key       | Command               | Description          |
| :-------- | :-------------------- | :------------------- |
| `SPC t t` | `consult-theme`       | Switch Theme.        |
| `SPC t l` | `display-line-numb...`| Toggle Line Numbers. |
| `SPC t z` | `olivetti-mode`       | Toggle **Zen Mode**. |

#### 🪟 Windows (`SPC w`)

| Key         | Command                | Description          |
| :---------- | :--------------------- | :------------------- |
| `SPC w s`   | `split-window-below`   | Split Horizontal.    |
| `SPC w v`   | `split-window-right`   | Split Vertical.      |
| `SPC w d`   | `delete-window`        | Close Window.        |
| `SPC w m`   | `delete-other-windows` | **Maximize** Window. |
| `SPC w =`   | `balance-windows`      | Balance Sizes.       |
| `SPC w +/-` | `enlarge/shrink...`    | Resize Width.        |

-----

### 🕹️ Editing (Evil & General)

#### Surround (`g s`)

| Key     | Command                | Description                            |
| :------ | :--------------------- | :------------------------------------- |
| `g s a` | `evil-surround-region` | **Add** surround (e.g. `gsa iw "`).    |
| `g s d` | `evil-surround-delete` | **Delete** surround (e.g. `gsd "`).    |
| `g s r` | `evil-surround-change` | **Replace** surround (e.g. `gsr " '`). |

#### Navigation & Utils

| Key     | Command           | Description                                |
| :------ | :---------------- | :----------------------------------------- |
| `C-:`   | `avy-goto-char`   | **Jump**. Type char -\> Hint -\> Teleport.   |
| `C-'`   | `avy-goto-char-2` | **Jump**. Type 2 chars -\> Hint.            |
| `M-g w` | `avy-goto-word-1` | **Jump**. Type word -\> Hint.               |
| `C-x b` | `consult-buffer`  | Quick Switch Buffer.                       |
| `M-s r` | `consult-ripgrep` | Quick Project Search.                      |
| `C-.`   | `embark-act`      | **Act** on thing under cursor.             |
| `C-h B` | `embark-bindings` | Show available keybindings.                |

#### Multiple Cursors & Lines

| Key         | Command              | Description                        |
| :---------- | :------------------- | :--------------------------------- |
| `C->`       | `mc/mark-next...`    | Add cursor to next match.          |
| `C-<`       | `mc/mark-prev...`    | Add cursor to previous match.      |
| `C-c m`     | `mc/edit-lines`      | Cursor on every selected line.     |
| `M-up/down` | `drag-stuff-up/down` | **Move Line** up/down.             |
| `C-=`       | `er/expand-region`   | **Expand Selection** semantically. |
