function cl() {
    builtin cd "$@"
    ls -F --color=auto
}

function cla() {
    builtin cd "$@"
    ls -A --color=auto
}

# mkdir, cd into it
function mkcd () {
    mkdir -p "$*"
        cd "$*"
}

function bd () {
    OLDPWD=`pwd`
    NEWPWD=`echo $OLDPWD | sed 's|\(.*/'$1'[^/]*/\).*|\1|'`
    index=`echo $NEWPWD | awk '{ print index($1,"/'$1'"); }'`
    if [ $index -eq 0 ] ; then
        echo "No such occurrence."
    else
        echo $NEWPWD
        cd "$NEWPWD"
    fi
}

function jboss_clean() {
    jboss_folder=$(ls jboss-eap-?.? 2> /dev/null)
    echo $jboss_folder
    if [ ! "$jboss_folder" ]; then
        if [ ! -f jboss-modules.jar ]; then
            echo "You are not in JBoss folder"
            return 1
        else
            cd ..
        fi
    fi

    jboss_full_build=$(ls *full-build* 2> /dev/null)
    if [ ! "$jboss_full_build" ]; then
        echo "Full build doesn't exists"
        return 1
    fi

    echo -n "# Removing JBoss folder"
    rm -rf jboss-eap-?.? 1> /dev/null 2>&1
    echo -ne "\033[1K\r# Extracting full build"
    extract *full-build* 1> /dev/null 2>&1
    echo -ne "\033[1K\r# CD to JBoss folder"
    cd jboss-eap-?.?
    echo -ne "\033[1K\r"
}

function jboss_cd() {
    base_dir="/home/ihradek/EAP/"
    while [[ $# -gt 1 ]]; do
        case $1 in
            -h|--help)
                exit
                ;;
            -e|--eap-version)
                shift
                arg=$1
                # Emtpy or next option was provided
                if [ -z "$arg" ] || [[ $arg == -* ]]; then
                    echo "Verions of EAP was not specified"
                    return 1
                fi
                eaps=$(ls $base_dir)
                # complete -F _complete_version 
                if [[ $arg != JBEAP-* ]]; then
                    arg="JBEAP-$arg/"
                fi
                index=$(search eaps[@] $arg)
                if [[ $index == "-1" ]]; then
                    # TODO:
                    echo "Specified EAP version doesn't exists"
                    return 1
                fi
                cd "$base_dir/${eaps[$found]}"
                ;;
        esac
    done
}

function _complete_version() {
    eaps=$(ls /home/ihradek/EAP)
    local cur prev opts
    COMPREPLY=()
    cur="${COMP WORDS[COMP_CWORD]}"
    prev="${COMP WORDS[COMP_CWORD-1]}"
    opts=$1

    # COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
}

function search() {
    declare -a arr=("${!1}")
    local i=1
    needle="$2/"

    for e in $arr; do
        if [[ "$e" == "$needle" ]]; then
            echo $i
            return
        else
            ((++i))
        fi
    done
    echo "-1"

}

function jboss_get_last_installed_version() {
    base_dir=$1
    if [ ! -d $base_dir ]; then
        echo "ERROR: $base_dir directory doesn't exists"
        return 1
    fi

    eaps=$(ls $base_dir)
}

function extract() {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2) tar xvjf $1   ;;
            *.tar.gz)  tar xvzf $1   ;;
            *.bz2)     bunzip2 $1    ;;
            *.rar)     unrar x $1    ;;
            *.gz)      gunzip $1     ;;
            *.tar)     tar xvf $1    ;;
            *.tbz2)    tar xvjf $1   ;;
            *.tgz)     tar xvzf $1   ;;
            *.zip)     unzip $1      ;;
            *.Z)       uncompress $1 ;;
            *.7z)      7z x $1       ;;
            *)         echo "Don't know how to extract '$1'..." ;;
        esac
    else
        echo "'$1' Not a file!"
    fi
}

