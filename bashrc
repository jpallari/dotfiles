#!/bin/bash

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# history
HISTCONTROL=ignoreboth
HISTSIZE=400
HISTFILESIZE=2000
shopt -s histappend

# window size
shopt -s checkwinsize

# lesspipe
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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
alias e='emacsclient -c -t --alternate-editor=""'
alias er='emacsclient -n'

# functions
function loadbashcompl {
    if [ -f "/etc/bash_completion" ]; then
        . "/etc/bash_completion" && echo "Bash completion loaded"
    else
        echo "No bash completion available"
    fi
}

# prompt
export PS1="\[\e[7m\]\h\$\[\e[0m\] "
case $TERM in
    xterm*)
        export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}\007"'
        ;;
    *) export PROMPT_COMMAND="" ;;
esac

# exports
export PAGER=less

# local configurations
[[ -f ~/.shlocal ]] && source ~/.shlocal

# Show the current host and path when the shell starts.
echo "Host: ${HOSTNAME}"
echo "Path: ${PWD}"
