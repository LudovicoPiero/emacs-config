# Customizable Emacs Configuration Built on Top of [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d)

## Introduction

**This configuration is built on top of [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d)** a lightweight and optimized Emacs base (`init.el` and `early-init.el`) by [James Cherti](https://www.jamescherti.com). It retains the same principles of minimalism, speed, and extensibility, while adding **customizations, modular structure**, and **integration with additional packages and workflows**.

---

## About minimal-emacs.d

> The following is adapted from the original [minimal-emacs.d README](https://github.com/jamescherti/minimal-emacs.d) to give you context on its goals and philosophy:

The **minimal-emacs.d** project is a lightweight Emacs starter (`init.el` and `early-init.el`) that **avoids the complexity of Doom Emacs or Spacemacs**, while providing a solid, optimized baseline for your configuration.

It includes:

- `early-init.el`: Loaded before the GUI and packages are initialized. Great for setting up performance-related and UI-related parameters.
- `init.el`: Loaded after GUI init, used for personal package loading, keybindings, and mode configurations.

It is:
- **Minimal yet powerful:** No forced packages or frameworks.
- **Customizable:** Designed to be extended, not replaced.
- **Fast:** Startup performance is a primary goal.

## Customizations

All user-specific settings and additions in this configuration are layered **on top of** the original `minimal-emacs.d` core. That means:
- The core `init.el` and `early-init.el` are retained as upstream base files (or adapted directly if needed).
- User customizations, third-party packages, and modular configurations live in separate files or folders.
- If you want to update from upstream, it's easy to diff and merge changes.

---

## Licensing

This project remains under the same license as `minimal-emacs.d`: the GNU General Public License version 3 (or any later version).

Original copyright:

© 2024–2025 [James Cherti](https://www.jamescherti.com)

---

## Links

- [minimal-emacs.d @GitHub](https://github.com/jamescherti/minimal-emacs.d)

### Other Emacs packages by James Cherti:
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el)
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el)
- [easysession.el](https://github.com/jamescherti/easysession.el)
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el)
- [elispcomp](https://github.com/jamescherti/elispcomp)
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el)
- [Ultyas](https://github.com/jamescherti/ultyas/)
- [dir-config.el](https://github.com/jamescherti/dir-config.el)
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el)
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el)
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el)
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el)
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el)
- [stripspace.el](https://github.com/jamescherti/stripspace.el)
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el)
- [pathaction.el](https://github.com/jamescherti/pathaction.el)
