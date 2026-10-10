#!/bin/sh
#==============================================================================#
#                            LINUX SYSTEM BOOTSTRAP                            #
#                                                                              #
# One-time, root-level setup that chezmoi does not manage (it is not meant for
# /etc or sudo). Everything else is handled by `chezmoi apply`.
#
# Usage:  sh ~/dotfiles/bootstrap/linux-system.sh
#==============================================================================#
set -e
REPO="${HOME}/dotfiles"

# LY display manager config -> /etc/ly/config.ini
if [ -f "${REPO}/dm/ly/config.ini" ]; then
    echo "Linking LY config to /etc/ly/config.ini (requires sudo)..."
    sudo ln -sf "${REPO}/dm/ly/config.ini" /etc/ly/config.ini
    # The unit is templated per-TTY (ly@.service) and Conflicts with getty@tty2,
    # so enable the tty2 instance. No --now: starting it from inside a running
    # session would switch to the login screen; it comes up on next boot.
    echo "Enabling ly@tty2 display manager (requires sudo)..."
    sudo systemctl enable ly@tty2.service 2>/dev/null || true
fi

# River: pin to 0.3.x. 0.4 dropped riverctl/rivertile, which this config needs
# (home/dot_config/river/init, tag-watch.sh). A fresh `pacman -S river` installs
# 0.4, so downgrade from the pacman cache or the Arch archive, then add it to
# IgnorePkg so `pacman -Syu` leaves it alone. Both steps are idempotent.
RIVER_PKG="river-0.3.13-2-x86_64.pkg.tar.zst"
RIVER_CACHE="/var/cache/pacman/pkg/${RIVER_PKG}"
if command -v pacman >/dev/null; then
    case "$(pacman -Q river 2>/dev/null | awk '{print $2}')" in
        0.3.*) ;;
        *)
            if [ ! -f "${RIVER_CACHE}" ]; then
                echo "Downloading ${RIVER_PKG} from the Arch archive (requires sudo)..."
                sudo curl -fsSL -o "${RIVER_CACHE}" \
                    "https://archive.archlinux.org/packages/r/river/${RIVER_PKG}"
                sudo curl -fsSL -o "${RIVER_CACHE}.sig" \
                    "https://archive.archlinux.org/packages/r/river/${RIVER_PKG}.sig"
            fi
            echo "Downgrading river to 0.3.13 (requires sudo)..."
            sudo pacman -U --needed "${RIVER_CACHE}"
            ;;
    esac
    if ! grep -qE '^IgnorePkg[[:space:]]*=.*(^|[[:space:]])river([[:space:]]|$)' /etc/pacman.conf; then
        echo "Adding river to IgnorePkg in /etc/pacman.conf (requires sudo)..."
        if grep -qE '^IgnorePkg[[:space:]]*=' /etc/pacman.conf; then
            sudo sed -i -E 's/^(IgnorePkg[[:space:]]*=.*)$/\1 river/' /etc/pacman.conf
        elif grep -qE '^#IgnorePkg[[:space:]]*=' /etc/pacman.conf; then
            sudo sed -i -E 's/^#IgnorePkg[[:space:]]*=.*$/IgnorePkg   = river/' /etc/pacman.conf
        else
            echo 'IgnorePkg   = river' | sudo tee -a /etc/pacman.conf >/dev/null
        fi
    fi
fi

# keyd: lone Super-tap -> F13 launcher (River binds F13 to fuzzel). See keyd/.
if [ -f "${REPO}/keyd/default.conf" ]; then
    echo "Linking keyd config to /etc/keyd/default.conf (requires sudo)..."
    sudo mkdir -p /etc/keyd
    sudo ln -sf "${REPO}/keyd/default.conf" /etc/keyd/default.conf
    sudo systemctl enable --now keyd 2>/dev/null || true
    sudo keyd reload 2>/dev/null || true
fi

echo "Linux system bootstrap complete."
