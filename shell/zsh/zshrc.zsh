#!/bin/sh
#==============================================================================#
#                                     ZSH                                      #
#                                                                              #
# @file:   ~/.zshrc                                                            #
# @author: hradecek <ivohradek@gmail.com>                                      #
#==============================================================================#
BASE_DIR=${HOME}/dotfiles
BASE_SHELL_DIR=${BASE_DIR}/shell

source ~/.zplug/init.zsh

load_aliases() {
    local alias_dir="$1"
    if [ -d "${alias_dir}" ]; then
        for alias_file in "${alias_dir}"/*.aliases.sh(.N); do
            source "${alias_file}"
        done
    else
        echo "Warning: Alias directory '${alias_dir}' does not exist."
    fi
}

setopt AUTO_CD
setopt PROMPT_SUBST

export EDITOR='nvim'
export JAVA_HOME=/usr/lib/jvm/default

bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search
bindkey "^A" vi-beginning-of-line
bindkey "^E" vi-end-of-line

load_aliases "${BASE_SHELL_DIR}/aliases"
source ${BASE_SHELL_DIR}/zsh/history.zsh

# Plugins
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-completions"
zplug "spaceship-prompt/spaceship-prompt", use:spaceship.zsh, from:github, as:theme

if ! zplug check --verbose; then
    zplug install
fi
zplug load

