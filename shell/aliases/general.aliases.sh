alias cd..='cd ..'
alias -- -='cd -'

alias l='ls'
alias sl='ls'
alias ls='ls -F --color=auto'
alias ll='ls -l'
alias lo='ls -o'
alias lh='ls -lh'
alias la='ls -A'
alias lah='ls -lAh'
alias lad="ls -a | grep --color=never '^\.'"
alias lla='ls -lA'
alias lld="ls -ld */"
alias lsd="ls -l --color=always | grep --color=never '^d'"
alias tree="tree -C"

alias rm='rm -i'
alias bc="bc -q -l"
alias grep="grep --color=auto"
alias sortbysize="ls -s | sort -n"
alias diskspace="du -S | sort -n -r | more"
alias psgrep="ps -aux | grep -v grep | grep -i"

