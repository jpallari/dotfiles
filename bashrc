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
alias httpserver='python -m SimpleHTTPServer 8888'
if [ "`uname`" = 'Linux' ]; then
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
fi

# functions
function loadbashcompl {
    local files=(
        "/etc/bash_completion"
        "/usr/local/etc/bash_completion"
        "/usr/share/bash-completion/bash_completion"
    )
    local loaded=0

    for f in "${files[@]}"; do
        if [ -f $f ]; then
            . $f
            loaded=1
        fi
    done

    if [ $loaded == 0 ]; then
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

