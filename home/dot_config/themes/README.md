# Themes

Each directory here is one theme. The app configs import or symlink their colors, fonts and
wallpaper from `~/.config/themes/current`, a symlink owned by the
[`theme`](../../dot_local/bin/executable_theme) script. chezmoi does not manage it, so
`chezmoi apply` keeps the chosen theme.

```shell
theme           # list themes, * marks the active one
theme <name>    # switch and live-reload
theme --reload  # re-apply the active theme (River init; creates current -> colors2 if missing)
```

## What a theme provides

Every theme must provide every file; a missing one leaves that app unthemed or failing to start.

| File | Used by | Picked up |
|------|---------|-----------|
| `alacritty.toml` | Alacritty `import` (colors + font) | live |
| `waybar-colors.css` | `@define-color` names used by `waybar/base.css` (`@surface`, `@rest`, `@warn`, ...) | live |
| `waybar.css` | Waybar fonts and theme-only styling, imported after `base.css` | live |
| `fuzzel.ini` | Fuzzel `include=` (font, colors, border) | next launch |
| `mako` | mako `include=` (font, colors, per-urgency styling) | live |
| `river.sh` | sourced; `riverctl` background and border colors | live |
| `swaylock` | `~/.config/swaylock/config` symlink | next lock |
| `gtk3.css`, `gtk4.css` | `~/.config/gtk-{3,4}.0/gtk.css` symlinks | app restart |
| `wallpaper` | swaybg (any image format, no extension needed) | live |

## Adding a theme

1. Copy an existing theme directory under a new name and adjust it.
2. Fonts it needs go to [`packages.yaml`](../../.chezmoidata/packages.yaml).
3. `chezmoi apply`, then `theme <name>`.

A theme may keep the source of a generated wallpaper next to it (`*.py`, not deployed). The retro
one is regenerated with `python3 home/dot_config/themes/retro/wallpaper.py` (needs `python-pillow`).

> [!WARNING]
> Waybar exits on any CSS property GTK3 does not know (e.g. `text-transform`, `font-variant-*`)
> instead of skipping it. Run `waybar` in a terminal after editing `waybar.css` and check for
> `[error]` before switching.
