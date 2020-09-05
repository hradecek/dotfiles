# port of zork theme

BRACKET_COLOR=${blue}
STRING_COLOR=${green}

SCM_THEME_PROMPT_PREFIX=""
SCM_THEME_PROMPT_SUFFIX=""

SCM_THEME_PROMPT_DIRTY=" ${bold_red}✗${normal}"
SCM_THEME_PROMPT_CLEAN=" ${bold_green}✓${normal}"
SCM_GIT_CHAR="${STRING_COLOR}±${normal}"
SCM_SVN_CHAR="${bold_cyan}⑆${normal}"
SCM_HG_CHAR="${bold_red}☿${normal}"

# Mysql Prompt
export MYSQL_PS1="(\u@\h) [\d]> "

case $TERM in
    xterm*)
        TITLEBAR="\[\033]0;\w\007\]"
        ;;
    *)
        TITLEBAR=""
        ;;
esac

PS3=">> "

is_vim_shell() {
    if [ ! -z "$VIMRUNTIME" ]
    then
        echo "${BRACKET_COLOR}[${STRING_COLOR}vim shell${BRACKET_COLOR}]${normal}"
    fi
}

function is_integer() { # helper function for todo-txt-count
    [ "$1" -eq "$1" ] > /dev/null 2>&1
    return $?
}

todo_txt_count() {
    if `hash todo.sh 2>&-`; then # is todo.sh installed
        count=`todo.sh ls | egrep "TODO: [0-9]+ of ([0-9]+) tasks shown" | awk '{ print $4 }'`
        if is_integer $count; then # did we get a sane answer back
            echo "${BRACKET_COLOR}[${STRING_COLOR}T:$count${BRACKET_COLOR}]$normal"
        fi
    fi
}

modern_scm_prompt() {
    CHAR=$(scm_char)
    if [[ $CHAR = $SCM_NONE_CHAR ]]
    then
        return
    else
        echo "${BRACKET_COLOR}[${CHAR}${BRACKET_COLOR}][${STRING_COLOR}$(scm_prompt_info)${BRACKET_COLOR}]$normal"
    fi
}

my_prompt_char() {
    if [[ $OSTYPE =~ "darwin" ]]; then
        echo "${BRACKET_COLOR}➞  ${normal}"
    else
        echo "${BRACKET_COLOR}➞ ${normal}"
    fi
}

print_result() {
    if [[ $? == 0 ]]; then
        echo "${green}¯\_(ツ)_/¯"
    else
        echo "${red}( ͠° ͟ʖ ͡°)"
    fi
}

files_count() {
    count=$(ls | wc -l)
    echo "${BRACKET_COLOR}[${STRING_COLOR}${count}${normal} files${BRACKET_COLOR}]${normal}";
}

prompt() {
    my_ps_host="${BRACKET_COLOR}[${STRING_COLOR}\h${BRACKET_COLOR}]${normal}";
    my_ps_user="${BRACKET_COLOR}[${STRING_COLOR}\u${BRACKET_COLOR}]${normal}";
    my_ps_path="${BRACKET_COLOR}[${STRING_COLOR}\w${BRACKET_COLOR}]${normal}";
    my_ps_time="${BRACKET_COLOR}[${STRING_COLOR}\t${BRACKET_COLOR}]${normal}";

    top_left="${TITLEBAR}${BRACKET_COLOR}┌─${my_ps_time}${my_ps_user}${my_ps_host}${my_ps_path}$(modern_scm_prompt)$(is_vim_shell)$(files_count)"
    top_right="$(print_result)"
    bottom="${BRACKET_COLOR}└─$(todo_txt_count)$(my_prompt_char)"

    top=$(printf "%s %s" "${top_left}" "${top_right}")
    PS1="$top\n${bottom}"
}

PS2="└─$(my_prompt_char)"

PROMPT_COMMAND=prompt

