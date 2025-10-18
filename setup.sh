#!/bin/sh
set -eu

XDG_CONFIG_DIR="${XDG_CONFIG_HOME:-${HOME}/.config}"
DOTFILE_DIR="$(cd "$(dirname "$0")" && pwd)"

setup_link() {
    _dotfile_name="$1"
    _target_path="$2"
    _dotfile_path="${DOTFILE_DIR}/${_dotfile_name}"

    if ! [ -e "${_dotfile_path}" ]; then
        echo "Error: File ${_dotfile_path} does not exist" >&2
        return 1
    fi

    if [ -L "${_target_path}" ] && [ "$(readlink "${_target_path}")" = "${_dotfile_path}" ]; then
        echo "Already linked: ${_target_path} => ${_dotfile_name}" >&2
        return 0
    fi

    if [ -L "${_target_path}" ]; then
        rm "${_target_path}"
    elif [ -e "${_target_path}" ]; then
        mv "${_target_path}" "${_target_path}.bak"
    fi

    # Create symlink
    echo "Linking: ${_target_path} => ${_dotfile_name}" >&2
    ln -s "${_dotfile_path}" "${_target_path}"
}

link_xdg() {
    setup_link "$1" "${XDG_CONFIG_DIR}/$1"
}

link_home() {
    setup_link "$1" "${HOME}/.${2:-$1}"
}

link_gitconfig() {
    _gitconfig_target="${DOTFILE_DIR}/gitconfig"

    if
        git config --global --get-all include.path 2>/dev/null \
        | grep -q "${_gitconfig_target}"
    then
        echo "Already linked: gitconfig" >&2
        return 0
    fi

    echo "Linking: gitconfig" >&2
    git config --global --add include.path "${_gitconfig_target}"
}

# Link targets
mkdir -p "${XDG_CONFIG_DIR}"
link_xdg "foot"
link_xdg "nvim"
link_xdg "mc"
link_xdg "powershell"
link_xdg "wezterm"
link_home "bashrc" "zshrc"
link_home "bashrc"
link_home "gdbinit"
link_home "inputrc"
link_home "screenrc"
link_home "tmux.conf"
link_home "Xresources"
link_gitconfig

