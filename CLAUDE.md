# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Personal cross-platform dotfiles for **Arch Linux** (River/Wayland) and **Windows**, managed with [chezmoi](https://www.chezmoi.io/). chezmoi copies/generates the target files (no live symlinks); OS differences are handled with templates and a templated `.chezmoiignore`.

The chezmoi **source directory is `home/`** (selected by the repo-root `.chezmoiroot` file). The repo itself stays cloned at `~/dotfiles`.

## Installation

```shell
# one-time: `init` renders home/.chezmoi.toml.tmpl into ~/.config/chezmoi/chezmoi.toml,
# pinning sourceDir to the clone (keep it at ~/dotfiles on Linux — dot_zshrc sources shell/ from there)
chezmoi init --source ~/dotfiles

chezmoi diff                       # preview changes
chezmoi apply                      # write configs into place
```

Edit configs with `chezmoi edit --apply <target>` (edits the source then applies), or edit files under `home/` directly and run `chezmoi apply`.

**Linux only**, one-time root-level setup chezmoi does not manage (LY display manager, keyd):

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
- `run_once_*.ps1` = one-time script (Windows XDG setup; ignored elsewhere); `run_once_before_*` runs before any file is written (yay bootstrap)
- `run_onchange_*` = script re-run whenever its rendered content changes (e.g. `install-packages.sh.tmpl`)
- `home/.chezmoidata/*.yaml` = template data available to all `.tmpl` files (e.g. the package manifest)
- `home/.chezmoi.toml.tmpl` = rendered by `chezmoi init` into the chezmoi config (sets `sourceDir`)
- `home/.chezmoiignore` drops the Linux-only Wayland stack when `.chezmoi.os != linux`

## Cross-OS strategy

nvim reads from `~/.config` on every OS — on Windows `run_once_set-windows-xdg.ps1` sets `XDG_CONFIG_HOME=%USERPROFILE%\.config` (needs a relog). **Alacritty on Windows does NOT honor `XDG_CONFIG_HOME`** — it only reads `%APPDATA%\alacritty`, so a Windows-only shim (`home/AppData/Roaming/alacritty/alacritty.toml.tmpl`) imports the real cross-OS config in `~/.config/alacritty` (single source of truth; on Windows it imports its colors from `~/.config/themes/colors2`). The prompt is **Starship** (`home/dot_config/starship.toml`, shared by zsh + PowerShell). Linux-only configs (River, Waybar, Fuzzel, mako, swaylock, zsh, dircolors) are excluded on Windows, and the Windows-only pieces (Documents PowerShell profiles, AppData shim, `set-windows-xdg.ps1`, `install-fonts.ps1`) are excluded elsewhere — all via `.chezmoiignore`.

Windows prerequisites not auto-installed: **PowerShell 7 (MSI build**, not Store/MSIX — Alacritty can't launch the WindowsApps alias; the `alacritty.toml.tmpl` shell points at `C:\Program Files\PowerShell\7\pwsh.exe`) and **Starship** (`winget install Starship.Starship`). Nerd Fonts ARE auto-installed by `run_onchange_install-fonts.ps1` from `.chezmoidata/packages.yaml`'s `nerdfonts` list — it registers them under `HKCU` and calls `AddFontResource` (copying alone leaves them invisible to Alacritty).

## Architecture

| Path | Purpose | Scope |
|------|---------|-------|
| `home/dot_config/nvim/` | LazyVim config (plugins in `lua/plugins/`, settings in `lua/config/`) | cross-OS |
| `home/dot_config/alacritty/alacritty.toml.tmpl` | Alacritty config (colors + font from the theme); Windows block sets the `pwsh` shell + `C:\projects` working dir | cross-OS |
| `home/AppData/Roaming/alacritty/alacritty.toml.tmpl` | `%APPDATA%` shim importing the real `~/.config/alacritty` config (Alacritty ignores XDG on Windows) | Windows-only |
| `home/dot_config/starship.toml` | Starship prompt config (time · dir · git · java ☕), shared by zsh + PowerShell | cross-OS |
| `home/dot_gitconfig.tmpl` | Git config; credential helper templated per OS | cross-OS |
| `home/.chezmoitemplates/powershell-profile.ps1` → `home/Documents/{PowerShell,WindowsPowerShell}/Microsoft.PowerShell_profile.ps1.tmpl` | PowerShell profile: git aliases (`gst`, `gco`, …) + `starship init` | Windows-only |
| `home/run_once_set-windows-xdg.ps1`, `home/run_onchange_install-fonts.ps1.tmpl`, `home/run_once_install-zig.ps1` | Windows setup: set `XDG_CONFIG_HOME`; install + register Nerd Fonts per-user; install the Zig toolchain (nvim-treesitter's C compiler) to `%LOCALAPPDATA%\Programs\zig` + add to User PATH | Windows-only |
| `home/komorebi.json`, `home/komorebi.bar.json`, `home/dot_config/whkdrc` | komorebi tiling WM + status bar (Mononoki, green accent) + whkd hotkeys. WM modifier = `Alt`, tap `Win` = Start. Autostart (`--whkd --bar`) via `run_onchange_windows-startup.ps1` | Windows-only |
| `home/run_onchange_windows-tweaks.ps1` | Per-user (HKCU) desktop tweaks: Explorer dev defaults, dark mode, centered/auto-hidden taskbar, declutter, fast key-repeat | Windows-only |
| `home/dot_config/{river,waybar,fuzzel,mako,swaylock}/` | Wayland desktop stack | Linux-only |
| `home/dot_config/themes/<theme>/` + `home/dot_local/bin/executable_theme` | Desktop themes (`colors2`, `retro`): per-app colors/fonts, wallpaper; `theme <name>` switches the `~/.config/themes/current` symlink and live-reloads | Linux (Windows: colors2 Alacritty only) |
| `home/dot_zshrc`, `home/dot_dircolors` | Shell entrypoints | Linux-only |
| `shell/` | Runtime libs sourced by `dot_zshrc` (aliases, `zsh/history.zsh`, `ssh/askpass-keyring.sh`) — **not** chezmoi-managed | Linux |
| `swaylock/assets/` | Lock-screen images (symlinked to `~/Pictures/swaylock`) | Linux |
| `dm/ly/`, `keyd/` | System-level configs installed via `bootstrap/linux-system.sh` (keyd: lone Super-tap → F13 → fuzzel) | Linux |
| `bootstrap/` | One-time root/admin installers: `linux-system.sh`; `windows-system.ps1` (admin: Caps Lock → Esc scancode map) | Linux + Windows |
| `home/.chezmoidata/packages.yaml` | Package manifest: `pacman`/`aur` (Linux, via `run_onchange_install-packages.sh.tmpl`) + `nerdfonts` (Windows, via `run_onchange_install-fonts.ps1.tmpl`) | both |

## Key Patterns

- `dot_zshrc` keeps `BASE_SHELL_DIR=${HOME}/dotfiles/shell` and sources the runtime libs at startup — so the repo must stay cloned at `~/dotfiles` on Linux.
- NeoVim uses the LazyVim distribution — add plugins as Lua files in `home/dot_config/nvim/lua/plugins/`.
- Shell aliases are centralized in `shell/aliases/`.
- System package dependencies live in `home/.chezmoidata/packages.yaml`. Keep it in sync when a config starts depending on a new binary; `chezmoi apply` installs missing ones on Linux. `chezmoi` is the only prerequisite it can't bootstrap (`yay` comes from `run_once_before_install-yay.sh`).
- **Themes:** app configs hold structure only; colors, fonts and wallpaper live in `home/dot_config/themes/<theme>/` (one file per app: `alacritty.toml`, `waybar-colors.css` + `waybar.css`, `fuzzel.ini`, `mako`, `river.sh`, `swaylock`, `gtk3.css`, `gtk4.css`, `wallpaper`) and are imported/symlinked from `~/.config/themes/current`. That symlink is owned by the `theme` script, not chezmoi, so `chezmoi apply` keeps the chosen theme. Waybar's `base.css` uses named colors (`@surface`, `@rest`, ...) and Waybar exits on any unsupported GTK3 CSS property (no `text-transform`, no `font-variant-*`). A new theme must provide every file. Themes: `colors2` (default, dark-green surfaces, accent `#5fa876`) and `retro` (green phosphor CRT, Departure Mono; its wallpaper is generated by `themes/retro/wallpaper.py`, not deployed).
- **Cross-OS keybindings are kept in sync** between River (`river/executable_init`) and komorebi/whkd (`whkdrc`): WM modifier = `Alt`; focus `Alt+HJKL`, move `Alt+Shift+HJKL`, workspaces `Alt+1-9`, next/prev window `Alt+Tab`/`Alt+Shift+Tab` (Windows: native switcher, not bound in whkd), next/prev workspace `Ctrl+Alt+L`/`Ctrl+Alt+H`, monitors `Alt+,`/`Alt+.`; lock = `Super+L` (Linux swaylock / Windows native Win+L); keyboard layout `Super+Space` (Linux `river/kb-layout.sh`, also feeds the Waybar layout readout / Windows native); terminal `Alt+Shift+Return`, screenshot `Alt+Shift+S`. When changing a binding on one platform, change its counterpart on the other. Irreducible per-WM extras: `Alt+arrows` main-location (River-only); `Alt+X`/`Alt+Y` flip-layout + `Alt+M` minimize + `Alt+V/B/C/G/U` change-layout algorithm (komorebi-only — rivertile is single-algorithm, so River has no counterpart).
