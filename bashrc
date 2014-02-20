#!/bin/bash

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# history
HISTCONTROL=ignoreboth
HISTSIZE=400
HISTFILESIZE=2000

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
alias emacs='emacs -nw'
alias em='emacs --color=no'
alias e='emacsclient -c -t --alternate-editor=""'
alias er='emacsclient -n'
alias httpserver='python -m SimpleHTTPServer 8888'

# functions
function loadbashcompl {
    if [ -f "/etc/bash_completion" ]
    then
        . "/etc/bash_completion" && echo "Bash completion loaded"
    else
        echo "No bash completion available"
    fi
}

function hr {
    printf -v line "%${COLUMNS}s" ""
    echo "${line// /=}"
}

function calc {
    echo "${@}" | bc -l
}

# exports
export PS1="\[\e[7m\]\h\$\[\e[0m\] "
export PAGER=less

# local configurations
[[ -f ~/.shlocal ]] && source ~/.shlocal

# lesspipe
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Show the current host and path when the shell starts.
echo "Host: ${HOSTNAME}"
echo "Path: ${PWD}"
