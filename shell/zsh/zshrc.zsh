#!/bin/sh
#==============================================================================#
#                                     ZSH                                      #
#                                                                              #
# @file:   ~/.zshrc                                                            #
# @author: hradecek <ivohradek@gmail.com>                                      #
#==============================================================================#
BASE_DIR=${HOME}/dotfiles
BASE_SHELL_DIR=${BASE_DIR}/shell

load_aliases() {
	local alias_dir="$1"
	for alias_file in $(ls ${alias_dir}/*.aliases.sh); do
		[ -e "${alias_file}" ] && source "${alias_file}"
	done
}

setopt AUTO_CD
setopt PROMPT_SUBST

export EDITOR='vim'
export JAVA_HOME=/usr/lib/jvm/default

bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search
bindkey "^A" vi-beginning-of-line
bindkey "^E" vi-end-of-line

load_aliases "${BASE_SHELL_DIR}/aliases"
source ${BASE_SHELL_DIR}/zsh/history.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/lib/spaceship-prompt/spaceship.zsh

