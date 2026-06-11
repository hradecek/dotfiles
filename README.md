<div align="center">

# ⚙️ dotfiles

#### Cross-platform [chezmoi](https://www.chezmoi.io/) dotfiles for a tiling **River / Wayland** desktop on **Arch Linux** — with a matching setup on **Windows**.

![Arch Linux](https://img.shields.io/badge/Arch_Linux-1793D1?style=for-the-badge&logo=arch-linux&logoColor=white)
![Windows](https://img.shields.io/badge/Windows-0078D6?style=for-the-badge&logo=windows&logoColor=white)
![Wayland](https://img.shields.io/badge/Wayland-FFBC00?style=for-the-badge&logo=wayland&logoColor=black)
![managed with chezmoi](https://img.shields.io/badge/managed_with-chezmoi-44a022?style=for-the-badge)

<sub>🎨 Themed **colors2** — neutral grays + a green accent (`#56c92b`), calmer borders (`#44a022`) — unified across Alacritty, Waybar, Fuzzel, River & mako.</sub>

</div>

---

## 🧩 The stack

|  |  |  |
|---|---|---|
| 🪟 **WM** · [River](./home/dot_config/river/) | 📊 **Bar** · [Waybar](./home/dot_config/waybar/) | 🚀 **Launcher** · [Fuzzel](./home/dot_config/fuzzel/) |
| 🔔 **Notifications** · [mako](./home/dot_config/mako/) | 🐚 **Shell** · [ZSH](./shell/zsh/README.md) | ✨ **Prompt** · [Spaceship](./shell/spaceship/README.md) |
| 🖥️ **Terminal** · [Alacritty](./home/dot_config/alacritty/) | 📝 **Editor** · [NeoVim / LazyVim](./home/dot_config/nvim/README.md) | 🌿 **VCS** · [Git](./home/dot_gitconfig.tmpl) |

---

## 🚀 Install

```shell
git clone <repo-url> ~/dotfiles
# pin chezmoi to the local repo (one-time; --source does not persist)
mkdir -p ~/.config/chezmoi
echo 'sourceDir = "~/dotfiles"' > ~/.config/chezmoi/chezmoi.toml
chezmoi diff          # 👀 preview
chezmoi apply         # ✍️  write configs into place
```

> [!IMPORTANT]
> chezmoi writes real files and won't clobber existing **symlinks** — if migrating from the
> old Make setup, remove the old `~/.config/*` and `~/.*` symlinks before the first `apply`.

### 📦 Packages

On Linux, `apply` also installs any missing system packages declared in
[`home/.chezmoidata/packages.yaml`](./home/.chezmoidata/packages.yaml) — a `run_onchange`
script that re-runs **only** when that list changes and is a no-op once everything is present.

> [!NOTE]
> Prerequisites it can't bootstrap itself: `chezmoi`, `yay` (for the AUR packages), and `zplug`
> (`git clone https://github.com/zplug/zplug ~/.zplug`).

### 🐧 Linux — one-time system setup

Root-level pieces chezmoi doesn't manage (LY display manager, anacron):

```shell
sh ~/dotfiles/bootstrap/linux-system.sh
```

### 🪟 Windows

```powershell
chezmoi init --apply <repo-url>
```

The `run_once` script sets `XDG_CONFIG_HOME` so Neovim and Alacritty read from `~/.config`
(relog afterward). Only the cross-platform configs (Alacritty, Neovim, Git) are applied;
the Wayland stack is skipped automatically via `.chezmoiignore`. Install the
*mononoki nerd font* separately.

---

## 🗂️ Layout

| Path | What lives here |
|------|-----------------|
| 📁 `home/` | chezmoi source (`.chezmoiroot` points here) — see [`CLAUDE.md`](./CLAUDE.md) for naming conventions. Includes `.chezmoidata/packages.yaml` (the package manifest) and `run_onchange_install-packages.sh.tmpl`. |
| 🐚 `shell/` | runtime libs sourced by `~/.zshrc` (aliases, history, themes) |
| 🥾 `bootstrap/` | one-time Linux system installers |
| 🔧 `dm/`, `jobs/`, `swaylock/assets/` | Linux system configs / assets |
