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
alias emacs="emacs -nw -Q -l $HOME/.emacs.d/fast-init.el"
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
    if [ ! -f "$NOTES_FILE" ]; then
        echo "$NOTES_FILE doesn't point to a file!" 1>&2
        return 1
    fi

    if [ ! "$@" ]; then
        echo "Nothing to add to notes file!" 1>&2
        return 1
    fi

    echo "$@" >> "$NOTES_FILE"
    echo "Note added!" 1>&2
}

read_notes() {
    if [ ! -f "$NOTES_FILE" ]; then
        echo "$NOTES_FILE doesn't point to a file!" 1>&2
        return 1
    fi

    cat "$HOME/.notes.txt"
}

set_window_title() {
    echo -ne "\033]0;$@\007"
}

__my_prompt_command() {
    local last_exit="$?"
    local status_color="\[\e[102m\]\[\e[30m\]"
    local dir_name=$(basename "$PWD")
    local title="$dir_name - ${USER}@${HOSTNAME}"

    if [ "$PWD" = "$HOME" ]; then
        dir_name="~"
    fi

    if [ "$last_exit" != 0 ]; then
        status_color="\[\e[101m\]\[\e[30m\]"
    fi

    PS1="$status_color\h " # host
    PS1+="\[\e[44m\]\[\e[96m\] \W \$" # directory
    PS1+="\[\e[0m\] " # end

    case "$TERM" in
        xterm*) set_window_title "$title" ;;
        *)
    esac
}

# exports
export PROMPT_COMMAND=__my_prompt_command
export PAGER=less
export EDITOR="emacs -nw"
export VISUAL="$EDITOR"
export NOTES_FILE="$HOME/.notes.txt"

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

# Who? Where?
echo "${USER}@${HOSTNAME}:${PWD}"
