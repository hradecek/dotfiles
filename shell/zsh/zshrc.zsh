#!/bin/sh
#==============================================================================#
#                                     ZSH                                      #
#                                                                              #
# @file:   ~/.zshrc                                                            #
# @author: hradecek <ivohradek@gmail.com>                                      #
#==============================================================================#
BASE_DIR=${HOME}/dotfiles

setopt AUTO_CD
setopt PROMPT_SUBST

export EDITOR='vim'
export JAVA_HOME=/usr/lib/jvm/default

bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search
bindkey "^A" vi-beginning-of-line
bindkey "^E" vi-end-of-line

source $BASE_DIR/shell/aliases/pacman.aliases.sh
source $BASE_DIR/shell/zsh/history.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/lib/spaceship-prompt/spaceship.zsh

