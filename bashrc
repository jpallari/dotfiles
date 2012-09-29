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

# dircolors
eval "$(dircolors -b)"

# flow control
stty -ixon

# aliases
alias ls='ls --color=auto -F'
alias ll='ls -lh'
alias grep='grep --color=auto'
alias ..="cd .."
alias ...="cd ../.."
alias jk="tmux attach -d"
alias sudo='sudo '
alias h="hostname"
alias e='emacsclient -c -t --alternate-editor=""'
alias em='emacs -Q -nw -l ~/.emacs'
alias er='emacsclient -n'
[[ -f ~/.bash_aliases ]] && source ~/.bash_aliases

# prompt
export PS1="\[\e[1;31m\]\$\[\e[0m\] "

# exports
export PAGER=less

# local configurations
[[ -f ~/.bashrc.local ]] && source ~/.bashrc.local

# Show the current host and path when the shell starts
echo "Host: $(hostname)"
echo "Path: $(pwd)"
