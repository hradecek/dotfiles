# Dotfiles

Cross-platform dotfiles for **Arch Linux** (River/Wayland) and **Windows**, managed with [chezmoi](https://www.chezmoi.io/).

**WM:** [River](./home/dot_config/river/) · **Bar:** [Waybar](./home/dot_config/waybar/) · **Launcher:** [Fuzzel](./home/dot_config/fuzzel/) · **Notifications:** [mako](./home/dot_config/mako/)\
**Shell:** [ZSH](./shell/zsh/README.md) · **Prompt:** [Spaceship](./shell/spaceship/README.md)\
**Terminal:** [Alacritty](./home/dot_config/alacritty/) · **Editor:** [NeoVim/LazyVim](./home/dot_config/nvim/README.md) · **VCS:** [Git](./home/dot_gitconfig.tmpl)

## Install

```shell
git clone <repo-url> ~/dotfiles
# pin chezmoi to the local repo (one-time; --source does not persist)
mkdir -p ~/.config/chezmoi
echo 'sourceDir = "~/dotfiles"' > ~/.config/chezmoi/chezmoi.toml
chezmoi diff          # preview
chezmoi apply         # write configs into place
```

> chezmoi writes real files and won't clobber existing **symlinks** — if migrating from the
> old Make setup, remove the old `~/.config/*` and `~/.*` symlinks before the first `apply`.

### Linux: one-time system setup

Root-level pieces chezmoi doesn't manage (LY display manager, anacron):

```shell
sh ~/dotfiles/bootstrap/linux-system.sh
```

### Windows

```powershell
chezmoi init --apply <repo-url>
```

The `run_once` script sets `XDG_CONFIG_HOME` so Neovim and Alacritty read from `~/.config`
(relog afterward). Only the cross-platform configs (Alacritty, Neovim, Git) are applied;
the Wayland stack is skipped automatically via `.chezmoiignore`. Install the
*mononoki nerd font* separately.

## Layout

- `home/` — chezmoi source (`.chezmoiroot` points here). See `CLAUDE.md` for naming conventions.
- `shell/` — runtime libs sourced by `~/.zshrc` (aliases, history, themes).
- `bootstrap/` — one-time Linux system installers.
- `dm/`, `jobs/`, `swaylock/assets/` — Linux system configs / assets.
