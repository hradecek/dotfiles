#!/bin/bash

# This script enables bash autocomplete feature of manu commands.
#
# To enable its functionality you have to source it (for current shell only)
# by running:
# $ . autocomplete.sh
# or put the line:
# $ . $MANU_HOME/tools/autocomplete.sh
# into the ~/.bashrc or ~/.bash_profile to enable manu autocomplete permanently.

_UseGetOpt-2 () {
  local cur
  # Pointer to current completion word.

  COMPREPLY=()   # Array variable storing the possible completions.
  local cur=${COMP_WORDS[COMP_CWORD]}
  local prev=${COMP_WORDS[COMP_CWORD-1]}

  local MANU_HOME="$(cd "$(cd "$(dirname "$(which -a "$1")")"; pwd -P)/.."; pwd)"

  if [[ x$prev =~ x.*manu$ ]] || [ "$prev" == "help" ]
  then
    COMPREPLY=( $( compgen -W 'check execute help list manudoc show version' -- $cur ) )
  else
      case "$prev" in
        show|list)
            COMPREPLY=( $( compgen -W '--all --classes --units --scripts' -- $cur ) ) ;;
        --scripts|check|execute)
            IN_MANUSCRIPTS_FOLDER=( $(compgen -W "$(for jar in $MANU_HOME/lib/manu*.jar $MANU_HOME/lib/unit*.jar $MANU_HOME/deploy/*.jar; do\
                                                        zipinfo -1 $jar | grep '.\manu$'
                                                    done 2>/dev/null)" -- $cur ) )
            IN_LIB_JARS=( $(compgen -W "$(ls "$MANU_HOME/manuscript/" | xargs)" -- $cur ) )
            ALL_MANUSCRIPTS=( "${IN_MANUSCRIPTS_FOLDER[@]}" "${IN_LIB_JARS[@]}" )
            COMPREPLY=( "${ALL_MANUSCRIPTS[@]// /\ }" ) ;;
        *)
            COMPREPLY=( $( compgen -f -- $cur ) ) ;;
    #   Generate the completion matches and load them into $COMPREPLY array.
    #   xx) May add more cases here.
    #   yy)
    #   zz)
      esac
  fi

  return 0
}

complete -F _UseGetOpt-2 manu
#        ^^ ^^^^^^^^^^^^  Invokes the function _UseGetOpt-2.
