# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Personal cross-platform dotfiles for **Arch Linux** (River/Wayland) and **Windows**, managed with [chezmoi](https://www.chezmoi.io/). chezmoi copies/generates the target files (no live symlinks); OS differences are handled with templates and a templated `.chezmoiignore`.

The chezmoi **source directory is `home/`** (selected by the repo-root `.chezmoiroot` file). The repo itself stays cloned at `~/dotfiles`.

## Installation

```shell
# one-time: pin chezmoi to this repo (keep it at ~/dotfiles on Linux —
# dot_zshrc sources shell/ from there). The --source flag does NOT persist.
mkdir -p ~/.config/chezmoi
echo 'sourceDir = "~/dotfiles"' > ~/.config/chezmoi/chezmoi.toml

chezmoi diff                       # preview changes
chezmoi apply                      # write configs into place
```

Edit configs with `chezmoi edit --apply <target>` (edits the source then applies), or edit files under `home/` directly and run `chezmoi apply`.

**Linux only**, one-time root-level setup chezmoi does not manage (LY display manager, anacron jobs):

```shell
sh ~/dotfiles/bootstrap/linux-system.sh
```

**Migration note:** chezmoi writes real files and will not overwrite pre-existing symlinks. On a machine still using the old Make symlinks, remove them before the first `chezmoi apply`.

## chezmoi source naming (under `home/`)

- `dot_config/<app>/` → `~/.config/<app>/`  •  `dot_gitconfig.tmpl` → `~/.gitconfig`
- `*.tmpl` = Go-template (e.g. `dot_gitconfig.tmpl` picks the credential helper per OS)
- `executable_` prefix preserves the +x bit (e.g. `river/executable_init`)
- `symlink_` creates a symlink (e.g. `Pictures/symlink_swaylock` → repo `swaylock/assets`)
- `dot_` prefix is required to deploy a dotfile (e.g. nvim's `dot_neoconf.json`)
- `run_once_*.ps1` = one-time script (Windows XDG setup; ignored elsewhere)
- `home/.chezmoiignore` drops the Linux-only Wayland stack when `.chezmoi.os != linux`

## Cross-OS strategy

Both nvim and Alacritty read from `~/.config` on every OS: on Windows the `run_once_set-windows-xdg.ps1` script sets `XDG_CONFIG_HOME=%USERPROFILE%\.config`, and Alacritty's import uses a config-relative path (`colors.toml`). Linux-only configs (River, Waybar, Fuzzel, mako, swaylock, zsh, dircolors, spaceship) are excluded on Windows via `.chezmoiignore`.

## Architecture

| Path | Purpose | Scope |
|------|---------|-------|
| `home/dot_config/nvim/` | LazyVim config (plugins in `lua/plugins/`, settings in `lua/config/`) | cross-OS |
| `home/dot_config/alacritty/` | Alacritty config (`alacritty.toml` + `colors.toml`, relative import) | cross-OS |
| `home/dot_gitconfig.tmpl` | Git config; credential helper templated per OS | cross-OS |
| `home/dot_config/{river,waybar,fuzzel,mako,swaylock}/` | Wayland desktop stack | Linux-only |
| `home/dot_zshrc`, `home/dot_dircolors`, `home/dot_spaceshiprc.zsh` | Shell entrypoints | Linux-only |
| `shell/` | Runtime libs sourced by `dot_zshrc` (aliases, `zsh/history.zsh`, spaceship theme, `bash/`) — **not** chezmoi-managed | Linux |
| `swaylock/assets/` | Lock-screen images (symlinked to `~/Pictures/swaylock`) | Linux |
| `dm/ly/`, `jobs/anacron/` | System-level configs installed via `bootstrap/linux-system.sh` | Linux |
| `bootstrap/` | One-time root-level Linux installers | Linux |

## Key Patterns

- `dot_zshrc` keeps `BASE_SHELL_DIR=${HOME}/dotfiles/shell` and sources the runtime libs at startup — so the repo must stay cloned at `~/dotfiles` on Linux.
- NeoVim uses the LazyVim distribution — add plugins as Lua files in `home/dot_config/nvim/lua/plugins/`.
- Shell aliases are centralized in `shell/aliases/`.
- Color theme is "colors2" (neutral grays + green accent `#56c92b`, calmer `#44a022` borders) across Alacritty, Waybar, Fuzzel, River, and mako.
