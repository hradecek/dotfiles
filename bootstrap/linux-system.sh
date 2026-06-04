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
fi

# Anacron scheduled jobs
if [ -f "${REPO}/jobs/anacron/install.sh" ]; then
    echo "Installing anacron jobs (requires sudo)..."
    sudo sh "${REPO}/jobs/anacron/install.sh"
fi

echo "Linux system bootstrap complete."
