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
- `run_onchange_*` = script re-run whenever its rendered content changes (e.g. `install-packages.sh.tmpl`)
- `home/.chezmoidata/*.yaml` = template data available to all `.tmpl` files (e.g. the package manifest)
- `home/.chezmoiignore` drops the Linux-only Wayland stack when `.chezmoi.os != linux`

## Cross-OS strategy

nvim reads from `~/.config` on every OS — on Windows `run_once_set-windows-xdg.ps1` sets `XDG_CONFIG_HOME=%USERPROFILE%\.config` (needs a relog). **Alacritty on Windows does NOT honor `XDG_CONFIG_HOME`** — it only reads `%APPDATA%\alacritty`, so a Windows-only shim (`home/AppData/Roaming/alacritty/alacritty.toml.tmpl`) imports the real cross-OS config in `~/.config/alacritty` (single source of truth; its relative `colors.toml` import resolves there). The prompt is **Starship** (`home/dot_config/starship.toml`, shared by zsh + PowerShell). Linux-only configs (River, Waybar, Fuzzel, mako, swaylock, zsh, dircolors) are excluded on Windows, and the Windows-only pieces (Documents PowerShell profiles, AppData shim, `set-windows-xdg.ps1`, `install-fonts.ps1`) are excluded elsewhere — all via `.chezmoiignore`.

Windows prerequisites not auto-installed: **PowerShell 7 (MSI build**, not Store/MSIX — Alacritty can't launch the WindowsApps alias; the `alacritty.toml.tmpl` shell points at `C:\Program Files\PowerShell\7\pwsh.exe`) and **Starship** (`winget install Starship.Starship`). Nerd Fonts ARE auto-installed by `run_onchange_install-fonts.ps1` from `.chezmoidata/packages.yaml`'s `nerdfonts` list — it registers them under `HKCU` and calls `AddFontResource` (copying alone leaves them invisible to Alacritty).

## Architecture

| Path | Purpose | Scope |
|------|---------|-------|
| `home/dot_config/nvim/` | LazyVim config (plugins in `lua/plugins/`, settings in `lua/config/`) | cross-OS |
| `home/dot_config/alacritty/alacritty.toml.tmpl` + `colors.toml` | Alacritty config; Windows block sets the `pwsh` shell + `C:\projects` working dir | cross-OS |
| `home/AppData/Roaming/alacritty/alacritty.toml.tmpl` | `%APPDATA%` shim importing the real `~/.config/alacritty` config (Alacritty ignores XDG on Windows) | Windows-only |
| `home/dot_config/starship.toml` | Starship prompt config (time · dir · git · java ☕), shared by zsh + PowerShell | cross-OS |
| `home/dot_gitconfig.tmpl` | Git config; credential helper templated per OS | cross-OS |
| `home/.chezmoitemplates/powershell-profile.ps1` → `home/Documents/{PowerShell,WindowsPowerShell}/Microsoft.PowerShell_profile.ps1.tmpl` | PowerShell profile: git aliases (`gst`, `gco`, …) + `starship init` | Windows-only |
| `home/run_once_set-windows-xdg.ps1`, `home/run_onchange_install-fonts.ps1.tmpl` | Windows setup: set `XDG_CONFIG_HOME`; install + register Nerd Fonts per-user | Windows-only |
| `home/komorebi.json`, `home/komorebi.bar.json`, `home/dot_config/whkdrc` | komorebi tiling WM + status bar (Mononoki, green accent) + whkd hotkeys. WM modifier = `Alt`, tap `Win` = Start. Autostart (`--whkd --bar`) via `run_onchange_windows-startup.ps1` | Windows-only |
| `home/run_onchange_windows-tweaks.ps1` | Per-user (HKCU) desktop tweaks: Explorer dev defaults, dark mode, centered/auto-hidden taskbar, declutter, fast key-repeat | Windows-only |
| `home/dot_config/{river,waybar,fuzzel,mako,swaylock}/` | Wayland desktop stack | Linux-only |
| `home/dot_zshrc`, `home/dot_dircolors` | Shell entrypoints | Linux-only |
| `shell/` | Runtime libs sourced by `dot_zshrc` (aliases, `zsh/history.zsh`, `bash/`) — **not** chezmoi-managed | Linux |
| `swaylock/assets/` | Lock-screen images (symlinked to `~/Pictures/swaylock`) | Linux |
| `dm/ly/`, `jobs/anacron/`, `keyd/` | System-level configs installed via `bootstrap/linux-system.sh` (keyd: lone Super-tap → F13 → fuzzel) | Linux |
| `bootstrap/` | One-time root/admin installers: `linux-system.sh`; `windows-system.ps1` (admin: Caps Lock → Esc scancode map) | Linux + Windows |
| `home/.chezmoidata/packages.yaml` | Package manifest: `pacman`/`aur` (Linux, via `run_onchange_install-packages.sh.tmpl`) + `nerdfonts` (Windows, via `run_onchange_install-fonts.ps1.tmpl`) | both |

## Key Patterns

- `dot_zshrc` keeps `BASE_SHELL_DIR=${HOME}/dotfiles/shell` and sources the runtime libs at startup — so the repo must stay cloned at `~/dotfiles` on Linux.
- NeoVim uses the LazyVim distribution — add plugins as Lua files in `home/dot_config/nvim/lua/plugins/`.
- Shell aliases are centralized in `shell/aliases/`.
- System package dependencies live in `home/.chezmoidata/packages.yaml`. Keep it in sync when a config starts depending on a new binary; `chezmoi apply` installs missing ones on Linux. `chezmoi`/`yay`/`zplug` are prerequisites it can't bootstrap.
- Color theme is "colors2" (neutral grays + green accent `#56c92b`, calmer `#44a022` borders) across Alacritty, Waybar, Fuzzel, River, and mako.
