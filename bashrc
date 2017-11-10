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

# better gpg
if hash gpg2 2>/dev/null; then
    alias gpg=gpg2
fi

alias ll='ls -lh'
alias grep='grep --color=auto'
alias remacs='emacsclient -n'

### functions ###

# load bash completion libraries from popular locations
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
        echo 'No bash completion available' 1>&2
    fi
}

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
    echo -e "User : \e[92m${USER} \e[94m@ \e[96m${HOSTNAME}\e[39m"
    echo -e "Dir  : \e[36m$(dirname "$PWD")/\e[93m$(basename "$PWD")\e[39m"
}

# set the current window title
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

    # Include VTE specific additions
    if hash __vte_prompt_command 2>/dev/null; then
        __vte_prompt_command
    fi
}

# VTE -- this must be loaded before the prompt command is set
if [ "$VTE_VERSION" ] && [ -f /etc/profile.d/vte.sh ]; then
    . /etc/profile.d/vte.sh
fi

### exports ###

export NOTES_FILE="$HOME/.notes.txt"
export PROMPT_COMMAND=__my_prompt_command

# emacs as the default editor... or vim
if hash emacs 2>/dev/null; then
    export EDITOR="emacs -nw"
    export VISUAL="$EDITOR"
fi

if [ "$TERM" != "dumb" ] && hash less 2>/dev/null; then
    export PAGER=less
fi

case "$COLORTERM" in
    "gnome-terminal"|"xfce4-terminal") export TERM="xterm-256color" ;;
esac

# custom paths. customize in ~/.local.sh
export CUSTOM_PATHS=(
    "$HOME/bin"
)

# local configurations
[[ -f $HOME/.local.sh ]] && source $HOME/.local.sh


# init paths
if [ -z "$CUSTOM_PATHS_SET" ]; then
    export DEFAULT_PATH="$(join_args : "${CUSTOM_PATHS[@]}"):$PATH"
    export PATH="$DEFAULT_PATH"
    export CUSTOM_PATHS_SET=1
fi

# OPAM configuration
if [ -f "$HOME/.opam/opam-init/init.sh" ] && hash opam 2>/dev/null; then
   . "$HOME/.opam/opam-init/init.sh" > /dev/null 2> /dev/null || true
   eval $(opam config env)
fi
### load bunch of stuff ###

# SDK man
if [ -f "$HOME/.sdkman/bin/sdkman-init.sh" ]; then
    export SDKMAN_DIR="$HOME/.sdkman"
    . "$HOME/.sdkman/bin/sdkman-init.sh"
fi
# lesspipe
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Bash completion
loadbashcompl

# Who? Where?
whereami

