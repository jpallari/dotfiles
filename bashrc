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

# flow control
stty -ixon

# aliases
alias ls='ls -F'
alias ll='ls -lh'
alias grep='grep --color=auto'
alias ..='cd ..'
alias ...='cd ../..'
alias jk='tmux attach -d'
alias sudo='sudo '

# aliases in Linux
if [ "`uname`" = 'Linux' ]; then
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
    alias open='xdg-open >/dev/null 2>&1'
fi

if hash gvim 2>/dev/null || hash mvim 2>/dev/null; then
    guivim=gvim
    guivim=${guivim:=mvim}
    alias vim="$guivim -v"
fi

# functions
function loadbashcompl {
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

function httpserver {
    local port=$1
    port=${port:=10101}
    python -m SimpleHTTPServer $port
}

function hr {
    printf -v line "%${COLUMNS}s" ""
    echo "${line// /=}"
}

function calc {
    echo "${@}" | bc -l
}

function rvim {
    local paramcount="$#"
    local servername="$1"
    shift
    local rest="$@"

    case $paramcount in
        0) vim --serverlist ;;
        1) vim --servername $servername ;;
        *) vim --servername $servername --remote $rest ;;
    esac
}

function join {
    local IFS="$1"
    shift
    echo "$*"
}

function reset_path {
    export PATH="$DEFAULT_PATH"
}

function add_path {
    export PATH="$PATH:$1"
}

# exports
export PS1="\[\e[7m\]\h\$\[\e[0m\] "
export PAGER=less
export EDITOR=vim
export VISUAL="$EDITOR"

# custom paths. customize in shlocal
export CUSTOM_PATHS=(
    "$HOME/.bin"
)

# local configurations
[[ -f ~/.shlocal ]] && source ~/.shlocal

# lesspipe
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# init paths
if [ -z "$CUSTOM_PATHS_SET" ]; then
    export DEFAULT_PATH="$PATH:`join : ${CUSTOM_PATHS[@]}`"
    export PATH="$DEFAULT_PATH"
    export CUSTOM_PATHS_SET=1
fi

# Show the current host and path when the shell starts.
echo "Host: ${HOSTNAME}"
echo "Path: ${PWD}"

