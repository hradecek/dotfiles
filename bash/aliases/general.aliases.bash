# Change directory
alias cd..='cd ..'
alias -- -='cd -'

# Reload
alias reload='source ~/.bashrc'

# List directory
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

# Documents generating
alias docvalidate="xmllint --valid --noout"
alias doc2html1="xsltproc /usr/share/xml/docbook/xhtml/docbook.xsl"
alias doc2multihtml="xsltproc /usr/share/xml/docbook/xhtml/chunk.xsl"

# Other
alias rm='rm -i'
alias bc="bc -q -l"
alias gdb="gdb -q"
alias svim="sudo vim"
alias grep="grep --color=auto"
alias wifi-menu="sudo wifi-menu"
alias suspend="systemctl suspend"
alias sortbysize="ls -s | sort -n"
alias hibernate="systemctl hibernate"
alias diskspace="du -S | sort -n -r | more"
alias psgrep="ps -aux | grep -v grep | grep -i"

# Personal aliases
alias aisa="ssh xhradek@aisa.fi.muni.cz"
alias jboss-clean='jboss_clean'
