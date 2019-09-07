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

### aliases ###

if [ "$(uname)" = 'Linux' ]; then
    alias ls='ls --color=auto -F'
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
    alias open='xdg-open >/dev/null 2>&1'
else
    alias ls='ls -F'
fi

# emacs in terminal w/ fast init
if [ -f "$HOME/.emacs.d/fast-init.el" ]; then
    alias emacs="emacs -nw -Q -l $HOME/.emacs.d/fast-init.el"
else
    alias emacs="emacs -nw"
fi

# vim in terminal w/ clipboard support etc.
if hash mvim 2>/dev/null; then
    alias vim='mvim -v'
elif hash vimx 2>/dev/null; then
    alias vim='vimx'
elif hash gvim 2>/dev/null; then
    alias vim='gvim -v'
fi

# better gpg
if hash gpg2 2>/dev/null; then
    alias gpg=gpg2
fi

alias ll='ls -lh'
alias grep='grep --color=auto'
alias remacs='emacsclient -n'

### functions ###

# share files over HTTP quickly
httpserver() {
    local curdir="$PWD"
    local port=${1:-10101}
    local dir=${2:-.}
    cd "$dir"
    python -m SimpleHTTPServer "$port"
    cd "$curdir"
}

# print a horizontal line
hr() {
    printf -v line "%${COLUMNS}s" ""
    echo "${line// /=}"
}

# quick calculations using BC
calc() {
    echo "${@}" | bc -l
}

# Python oneliner
pyline() {
    python -c "print $*"
}

# join arguments 2..n with first argument as the separator
join_args() {
    local IFS="$1"
    shift
    echo "$*"
}

# reset PATH variable to DEFAULT_PATH
reset_path() {
    export PATH="$DEFAULT_PATH"
}

# add a new PATH for this session
add_path() {
    export PATH="$1:$PATH"
}

# add a note to $NOTES_FILE
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

# print notes from the $NOTES_FILE
read_notes() {
    if [ ! -f "$NOTES_FILE" ]; then
        echo "$NOTES_FILE doesn't point to a file!" 1>&2
        return 1
    fi

    cat "$HOME/.notes.txt"
}

# print user and location
whereami() {
    echo -e "\e[92m${USER} \e[94m@ \e[96m${HOSTNAME} : \e[36m$(dirname "$PWD")/\e[93m$(basename "$PWD")\e[39m"
}
alias wai=whereami

# set the current window title
set_window_title() {
    echo -ne "\033]0;$@\007"
}

# Current time zone
get_current_time_zone() {
    timedatectl status | \
        grep "Time zone" | \
        sed 's/.*Time zone: \([^ ]\+\) .*/\1/'
}

# OPAM configuration initialiser
init_opam() {
    if [ -f "$HOME/.opam/opam-init/init.sh" ] && hash opam 2>/dev/null; then
       . "$HOME/.opam/opam-init/init.sh" > /dev/null 2> /dev/null || true
       eval $(opam config env)
    fi
}

# SDK man initialiser
init_sdkman() {
    if [ -f "$HOME/.sdkman/bin/sdkman-init.sh" ]; then
        export SDKMAN_DIR="$HOME/.sdkman"
        . "$HOME/.sdkman/bin/sdkman-init.sh"
    fi
}

# Find currently used SDKs from SDK man in $PATH format
find_sdkman_paths() {
    find -L "$HOME/.sdkman/candidates" -maxdepth 3 -type d -path '*/current/bin' -printf ':%p'
}

# Create a Python virtualenv
pyvenv_create() {
    if [ -z "$1" ]; then
        echo "No virtualenv name given!" >&2
        return 1
    fi
    mkdir -p "$PYVENVS_DIR"
    python3 -m venv "$PYVENVS_DIR/$1"
}

# Activate a Python virtualenv
pyvenv_activate() {
    if [ -z "$1" ]; then
        echo "No virtualenv name given!" >&2
        return 1
    fi
    . "$PYVENVS_DIR/$1/bin/activate"
}

# Execute a command in a Python virtualenv
pyvenv_exec() {
    if [ -z "$1" ]; then
        echo "No virtualenv name given!" >&2
        return 1
    fi
    local venv=$1
    shift
    pyvenv_activate "$venv"
    "$@"
}

__my_prompt_command() {
    local last_exit=$?
    local status_color="\[\e[102m\]\[\e[30m\]"
    history -a

    if [ "$last_exit" != 0 ]; then
        status_color="\[\e[101m\]\[\e[30m\]"
    fi

    PS1="${status_color}\h \$\[\e[0m\] "

    # Include VTE specific additions
    if [ "$VTE_VERSION" ] && hash __vte_prompt_command 2>/dev/null; then
        __vte_prompt_command
    fi
}

# VTE -- this must be loaded before the prompt command is set
if [ "$VTE_VERSION" ] && [ -f /etc/profile.d/vte.sh ]; then
    . /etc/profile.d/vte.sh
fi

### exports ###

export PYVENVS_DIR="$HOME/.local/share/pyvenvs"
export NOTES_FILE="$HOME/.notes.txt"
export PROMPT_COMMAND=__my_prompt_command

# Default editor
export EDITOR=vim
export VISUAL="$EDITOR"

if [ "$TERM" != "dumb" ] && hash less 2>/dev/null; then
    export PAGER='less -R'
fi

case "$COLORTERM" in
    "gnome-terminal"|"xfce4-terminal") export TERM="xterm-256color" ;;
esac

### custom paths. customize in ~/.local.sh ###
export CUSTOM_PATH="$HOME/bin:$HOME/.local/bin"

# local configurations
[[ -f $HOME/.local.sh ]] && source $HOME/.local.sh

# load SDK man paths
if [ -d "$HOME/.sdkman/candidates" ]; then
    CUSTOM_PATH+=$(find_sdkman_paths)
fi

# init paths
if [ -z "$CUSTOM_PATH_SET" ]; then
    export DEFAULT_PATH="$CUSTOM_PATH:$PATH"
    export PATH="$DEFAULT_PATH"
    export CUSTOM_PATH_SET=1
fi

### load bunch of stuff ###

# lesspipe
if hash lesspipe.sh >/dev/null; then
    export LESSOPEN="|lesspipe.sh %s"
fi

# Bash completion
BASH_COMPLETION_SOURCES=(
    '/etc/bash_completion'
    '/usr/local/etc/bash_completion'
    '/usr/share/bash-completion/bash_completion'
)
for f in "${BASH_COMPLETION_SOURCES[@]}"; do
    if [ -f "$f" ]; then
        . "$f"
    fi
done

# AWS CLI
if hash aws_completer >/dev/null; then
    complete -C aws_completer aws
fi

# Kubernetes
if hash kubectl >/dev/null; then
    source <(kubectl completion bash)
fi

# Pipenv
if hash pipenv 2>/dev/null; then
    eval "$(pipenv --completion)"
fi

# Direnv
if hash direnv 2>/dev/null; then
    eval "$(direnv hook bash)"
fi

# Who? Where?
whereami

