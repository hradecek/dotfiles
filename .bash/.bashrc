#=============================================================================#
# File:   ~/.bashrc                                                           #
# Author: hradecek <ivohradek@gmail.com>                                      #
#=============================================================================#

# Directory where everything is stored
BASH="$HOME/dotfiles/.bash"

# Choose theme
THEME="rjorgenson"

# History control
HISTSIZE=2000
HISFILESIZE=2000
HISTCONTROL=ignoreboth

# Press CTRL+D two time to exit shell
IGNOREEOF="1"

# sudo, man completition
complete -cf man
complete -cf sudo

# Auto cd
shopt -s autocd

# Combine multiline commands into one in history
shopt -s cmdhist

# Include .files when globbing
shopt -s dotglob

# Glob expands rather empty
shopt -s nullglob

# Append to history
shopt -s histappend

# Autocorrect typos in `cd`
shopt -s cdspell

# Disable old XON/XOFF flow control
stty -ixon

# Environment variables
export EDITOR="/usr/bin/vim"
export JAVA_HOME="/usr/lib/jvm/java-8-openjdk"
export PATH="$PATH:/opt/android-sdk/platform-tools/:/opt/android-sdk/tools/:/home/ivo/.gem/ruby/2.2.0/bin:/opt/jruby/bin:/usr/local/heroku/bin"

# Add cabal to PATH
if [[ -d "$HOME/.cabal/bin" ]]; then
    PATH="$HOME/.cabal/bin:$PATH"
    export PATH
fi

# Load extension functions
FUNCTIONS="$BASH/.bash.functions.bash"
if [[ -f $FUNCTIONS ]]; then
    source $FUNCTIONS
fi

# Load completions
COMPLETIONS="$BASH/completitions/*.completion.bash"
for file in $COMPLETIONS; do
    if [[ -f $file ]]; then
        source $file
    fi
done

# Load aliases
ALIASES="$BASH/aliases/*.aliases.bash"
for file in $ALIASES; do
    if [[ -f $file ]]; then
        source $file
    fi
done

#Load theme
source "$BASH/themes/colors.theme.bash"
source "$BASH/themes/base.theme.bash"
source "$BASH/themes/$THEME/${THEME}.theme.bash"

# Main prompt
if [ $PROMPT ]; then
    export PS1=$PROMPT
fi

# added by duckpan installer
eval $(perl -I${HOME}/perl5/lib/perl5 -Mlocal::lib)

