#!/bin/sh
#==============================================================================#
#                                INSTALL YAY                                   #
#                                                                              #
# Bootstraps the yay AUR helper that install-packages.sh needs for the `aur`   #
# list. Runs once, before any files are written; a no-op if yay is present.    #
#==============================================================================#
set -eu

command -v pacman >/dev/null 2>&1 || exit 0
command -v yay >/dev/null 2>&1 && exit 0

echo ":: installing yay (AUR helper)"
sudo pacman -S --needed --noconfirm base-devel git
build_dir=$(mktemp -d)
trap 'rm -rf "$build_dir"' EXIT
git clone --depth 1 https://aur.archlinux.org/yay-bin.git "$build_dir/yay-bin"
cd "$build_dir/yay-bin"
makepkg -si --noconfirm
