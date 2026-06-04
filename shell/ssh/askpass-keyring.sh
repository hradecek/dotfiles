#!/bin/sh
#==============================================================================#
#                              SSH ASKPASS HELPER                              #
#                                                                              #
# Returns the SSH private-key passphrase stored in the login keyring so ssh /  #
# ssh-add can unlock the key non-interactively. Used via SSH_ASKPASS with      #
# SSH_ASKPASS_REQUIRE=prefer (see zshrc.zsh). The login keyring is unlocked    #
# automatically at login by pam_gnome_keyring, so no prompt is ever shown.     #
#                                                                              #
# Store the passphrase once with:                                             #
#   secret-tool store --label='ssh id_ed25519' ssh-key id_ed25519             #
#                                                                              #
# @author: hradecek <ivohradek@gmail.com>                                      #
#==============================================================================#
exec secret-tool lookup ssh-key id_ed25519
