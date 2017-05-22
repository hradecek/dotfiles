# Fresh installation - remove jboss_home, extract full-build
function jboss_clean() {
    jboss_folder=$(find . -maxdepth 1 -iname '*jboss-eap-?.?' 2> /dev/null)
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
        return 2
    fi

    echo -n "# Removing JBoss folder"
    rm -rf jboss-eap-?.? &> /dev/null
    echo -ne "\033[1K\r# Extracting full build"
    unzip *full-build* &> /dev/null
    echo -ne "\033[1K\r"
}

# CD into jboss_home for specified version
# If no version - use latest
# If there's no jboss_home - it will be downloaded
function jboss_cd() {
base_dir="/home/ihradek/eap"
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            return 0
            ;;
        -e|--eap-version)
            shift
            arg=$1
            # Emtpy or next option was provided
            if [ -z "$arg" ] || [[ $arg == -* ]]; then
                echo "Verions of EAP was not specified"
                return 1
            fi
            eaps=($(ls $base_dir))
            # complete -F _complete_version
            if [[ $arg != */ ]]; then
                arg="$arg/"
            fi
            index=$(search "$arg" "${eaps[@]}")
            if [[ $index == "-1" ]]; then
                # TODO:
                echo "Specified EAP version doesn't exists"
                return 1
            fi
            cd "$base_dir/${eaps[$index]}"
            shift
            ;;
        *)
            echo "Option $1 is not valid"
            return 1
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

function jboss_get_last_installed_version() {
    base_dir=$1
    if [ ! -d $base_dir ]; then
        echo "ERROR: $base_dir directory doesn't exists"
        return 1
    fi

    eaps=$(ls $base_dir)
}

function search() {
    needle="$1"
    shift
    local i=0

    for e in "${@}"; do
        if [[ "$e" == "$needle" ]]; then
            echo "$i"
            return
        else
            ((++i))
        fi
    done
    echo "-1"
}

