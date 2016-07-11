#!/bin/bash

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# history
HISTCONTROL=ignoreboth
HISTIGNORE='bg:fg:history'
HISTSIZE=100000
HISTFILESIZE=100000

# shopts
shopt -s checkjobs
shopt -s checkwinsize
shopt -s histappend
shopt -s dotglob
shopt -s globstar
shopt -s direxpand

# flow control
stty -ixon

# aliases
if [ "$(uname)" = 'Linux' ]; then
    alias ls='ls --color=auto -F'
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
    alias open='xdg-open >/dev/null 2>&1'
else
    alias ls='ls -F'
fi

alias ll='ls -lh'
alias grep='grep --color=auto'
alias jk='tmux attach -d'
alias sudo='sudo '
alias emacs="emacs -nw -Q -l $HOME/.emacs.d/init.el"
alias remacs='emacsclient -n'

# functions
loadbashcompl() {
    local files=(
        '/etc/bash_completion'
        '/usr/local/etc/bash_completion'
        '/usr/share/bash-completion/bash_completion'
    )
    local loaded=0

    for f in "${files[@]}"; do
        if [ -f "$f" ]; then
            . "$f"
            loaded=1
        fi
    done

    if [ "$loaded" = 0 ]; then
        echo 'No bash completion available'
    fi
}

httpserver() {
    local curdir="$PWD"
    local port=${1:-10101}
    local dir=${2:-.}
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

add_note() {
    echo "$@" >> "$HOME/.notes.txt"
    echo "Note added!" 1>&2
}

read_notes() {
    cat "$HOME/.notes.txt"
}

__my_prompt_command() {
    local last_exit="$?"
    local default="\[\e[0m\]"
    local status_color="\[\e[102m\]\[\e[30m\]"

    if [ "$last_exit" != 0 ]; then
        status_color="\[\e[101m\]\[\e[30m\]"
    fi

    PS1="$time_color$time"
    PS1+="\[\e[30m\]\[\e[90m\]\A " # time
    PS1+="$status_color \h " # host
    PS1+="\[\e[44m\]\[\e[96m\] \W \$" # directory
    PS1+="$default " # end
}

# exports
export PROMPT_COMMAND=__my_prompt_command
export PAGER=less
export EDITOR="emacs -nw"
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
