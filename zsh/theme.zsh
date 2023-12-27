BRACKET_COLOR=${blue}
STRING_COLOR=${green}
PROMPT_CHAR=${BRACKET_COLOR}➞ ${normal}

function files_count() {
    count=$(ls | wc -l)
    echo "${BRACKET_COLOR}[${STRING_COLOR}${count}${normal} files${BRACKET_COLOR}]${normal}";
}

function print_result() {
    if [[ $? == 0 ]]; then
        echo "${green}¯\_(ツ)_/¯"
    else
        echo "${red}( ͠° ͟ʖ ͡°)"
    fi
}

function get_prompt() {
    my_ps_host="${BRACKET_COLOR}[${STRING_COLOR}\h${BRACKET_COLOR}]${normal}";
    my_ps_user="${BRACKET_COLOR}[${STRING_COLOR}\u${BRACKET_COLOR}]${normal}";
    my_ps_path="${BRACKET_COLOR}[${STRING_COLOR}\w${BRACKET_COLOR}]${normal}";
    my_ps_time="${BRACKET_COLOR}[${STRING_COLOR}\t${BRACKET_COLOR}]${normal}";

    #top_left="${TITLEBAR}${BRACKET_COLOR}┌─${my_ps_time}${my_ps_user}${my_ps_host}${my_ps_path}$(modern_scm_prompt)$(is_vim_shell)$(files_count)"
    top_left="${BRACKET_COLOR}┌─${my_ps_time}${my_ps_user}${my_ps_host}${my_ps_path}$(files_count)"
    top_right="$(print_result)"
    bottom="${BRACKET_COLOR}└─${PROMPT_CHAR}"

    top=$(printf "%s %s" "${top_left}" "${top_right}")
    PS1="${top}\n${bottom}"
}

PS2="└─${PROMPT_CHAR}"

# PROMPT=$(get_prompt)
get_prompt()
