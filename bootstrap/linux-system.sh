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

# Anacron scheduled jobs
if [ -f "${REPO}/jobs/anacron/install.sh" ]; then
    echo "Installing anacron jobs (requires sudo)..."
    sudo sh "${REPO}/jobs/anacron/install.sh"
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
