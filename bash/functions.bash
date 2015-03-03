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

function up(){
    local d=""
    limit=$1

    for((i=1; i <= limit; ++i)); do
        d=$d/..
    done
    d=$(echo $d | sed 's/^\///')

    if [[ -z $d ]]; then
        d=..
    fi
    cd $d
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

