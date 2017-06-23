#!/bin/sh
# Global variables
INSTALL_LOG="${PWD}/install.log"

# Printing functions
function echo_info() {
    echo -e " [ \033[00;34m..\033[0m ] ${1}"
}

function echo_succ() {
    echo -e "\r\033[2K [ \033[00;32mOK\033[0m ] ${1}\n"
}

function echo_fail() {
    echo -e "\r\033[2K [ \033[0;31mFAIL\033[0m ] ${1}\n"
}

function echo_warn() {
    echo -e " [ \033[0;33mWARN\033[0m ] ${1}\n"
}

function bold() {
    echo -e "\033[1m${1}\033[0m"
}

# Make a symlink
function lnif() {
    src="${PWD}/${1}"
    dst="${HOME}/${2}"

    if [ -e "${dst}" ]; then
        bkp="${dst}.bkp"
        echo_warn "File ${dst} already exists. Making backup: ${dst}.bkp"
    fi

    echo_info "Creating symlink: ${src} -> ${dst}"
    ln -s "${src}" "${dst}"
}

# Clone a github repository
function github_clone() {
    what="${1}"
    where="${2}"
    clone="https://github.com/${what}"

    echo_info "Cloning ${clone} to ${where}"
    git clone -q "${clone}" "${where}" &>> "${INSTALL_LOG}"
    if [ ${?} -ne 0 ]; then
        echo_fail "Clone failed. See ${INSTALL_LOG} for details."
        exit 1
    fi
}

# Tasks
function task_vim() {
    vimrc="${HOME}/.vimrc"
    vundle="${HOME}/.vim/bundle/Vundle.vim"
    solarized="${HOME}/.vim/bundle/vim-colors-solarized"

    github_clone "altercation/vim-colors-solarized.git" "solarized"
    github_clone "gmarik/vundle" "vundle"
    github_clone "powerline/fonts.git" "/tmp/powerline_fonts"
}
