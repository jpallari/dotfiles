#!/bin/bash

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# history
HISTCONTROL=ignoreboth
HISTIGNORE='bg:fg:history'
HISTSIZE=100000
HISTFILESIZE=100000

# shopts
shopt -s checkwinsize
shopt -s histappend
shopt -s dotglob
shopt -s globstar
shopt -s direxpand

# flow control
stty -ixon

# aliases
alias ls='ls --color -F'
alias ll='ls -lh'
alias grep='grep --color=auto'
alias jk='tmux attach -d'
alias sudo='sudo '
alias emacs='emacs -nw'
alias remacs='emacsclient -n'

# aliases in Linux
if [ "$(uname)" = 'Linux' ]; then
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
    alias open='xdg-open >/dev/null 2>&1'
fi

# functions
loadbashcompl() {
    local files=(
        '/etc/bash_completion'
        '/usr/local/etc/bash_completion'
        '/usr/share/bash-completion/bash_completion'
    )
    local loaded=0

    for f in "${files[@]}"; do
        if [ -f $f ]; then
            . $f
            loaded=1
        fi
    done

    if [ $loaded == 0 ]; then
        echo 'No bash completion available'
    fi
}

httpserver() {
    local curdir="$PWD"
    local port=${1:=10101}
    local dir=${2:=.}
    cd "$dir"
    python -m SimpleHTTPServer "$port"
    cd "$curdir"
}

hr() {
    printf -v line "%${COLUMNS}s" ""
    echo "${line// /=}"
}

calc() {
    echo "${@}" | bc -l
}

join_args() {
    local IFS="$1"
    shift
    echo "$*"
}

reset_path() {
    export PATH="$DEFAULT_PATH"
}

add_path() {
    export PATH="$1:$PATH"
}

# exports
export PS1="\[\e[7m\]\h\$\[\e[0m\] "
export PAGER=less
export EDITOR=emacs
export VISUAL="$EDITOR"

# custom paths. customize in shlocal
export CUSTOM_PATHS=(
    "$HOME/bin"
)

# local configurations
[[ -f $HOME/.local.sh ]] && source $HOME/.local.sh

# lesspipe
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# init paths
if [ -z "$CUSTOM_PATHS_SET" ]; then
    export DEFAULT_PATH="$(join_args : "${CUSTOM_PATHS[@]}"):$PATH"
    export PATH="$DEFAULT_PATH"
    export CUSTOM_PATHS_SET=1
fi

# Show the current host and path when the shell starts.
echo "Host: ${HOSTNAME}"
echo "Path: ${PWD}"
