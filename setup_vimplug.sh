#!/usr/bin/env bash
set -euo pipefail

VIMPLUG_URL=https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
XDG_DATA_HOME="${XDG_DATA_HOME:-"$HOME/.local/share"}"

curl -fLo "$HOME/.vim/autoload/plug.vim" --create-dirs "$VIMPLUG_URL"

if hash nvim 2>/dev/null; then
    mkdir -p "$XDG_DATA_HOME/nvim/site/autoload"
    cp "$HOME/.vim/autoload/plug.vim" "$XDG_DATA_HOME/nvim/site/autoload/plug.vim"
fi

