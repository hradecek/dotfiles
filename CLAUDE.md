# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Personal dotfiles repository for Arch Linux with River (Wayland compositor). Configuration files are symlinked to `~/.config/` via Make targets.

## Installation Commands

Install individual configurations by running `make <target>` from the repository root:

```shell
make alacritty      # Terminal emulator -> ~/.config/alacritty
make nvim           # NeoVim/LazyVim -> ~/.config/nvim
make fuzzel         # Application launcher -> ~/.config/fuzzel
make waybar         # Status bar -> ~/.config/waybar
make mako           # Notification daemon -> ~/.config/mako
make zsh            # ZSH config -> ~/.zshrc (also installs dircolors)
make dircolors      # ls/dir color theme -> ~/.dircolors
make spaceship-prompt  # Spaceship prompt (requires zsh)
make swaylock       # Lock screen assets
make ly             # LY display manager (requires sudo)
make anacron        # Anacron jobs (requires sudo)
```

Remove configurations with `make <target>-clean` (available for: alacritty, nvim, fuzzel, waybar, mako, dircolors).

## Architecture

| Directory | Purpose |
|-----------|---------|
| `nvim/` | LazyVim configuration (plugins in `lua/plugins/`, settings in `lua/config/`) |
| `shell/zsh/` | ZSH configuration with zsh-syntax-highlighting, zsh-autosuggestions, bat, fzf |
| `shell/spaceship/` | Spaceship prompt theme |
| `terminal/alacritty/` | Alacritty terminal config (TOML format with separate colors.toml) |
| `river/` | River WM init script |
| `waybar/` | Waybar config and CSS styling |
| `mako/` | Mako notification daemon config |
| `fuzzel/` | Fuzzel launcher config |
| `git/` | Git configuration |
| `swaylock/` | Lock screen assets |
| `dm/ly/` | LY display manager config |
| `jobs/anacron/` | Scheduled job definitions |

## Key Patterns

- All symlinks point entire directories to `~/.config/<app>/` (not individual files)
- NeoVim uses LazyVim distribution - add plugins as Lua files in `nvim/lua/plugins/`
- Shell aliases are centralized in `shell/aliases/`
