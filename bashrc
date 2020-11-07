# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# history
HISTFILE="$HOME/.bash_history"
HISTCONTROL=ignoreboth
HISTIGNORE='bg:fg:history'
HISTSIZE=100000
HISTFILESIZE=100000

# bash and zsh configs
if [ -n "$BASH_VERSION" ]; then
    # shopt
    shopt -s checkjobs
    shopt -s checkwinsize
    shopt -s histappend
    shopt -s dotglob
    shopt -s globstar
    shopt -s direxpand

    # flow control
    stty -ixon
elif [ -n "$ZSH_VERSION" ]; then
    # setopt
    setopt hist_ignore_all_dups
    setopt hist_reduce_blanks
    setopt inc_append_history
    setopt share_history
    setopt auto_list
    setopt auto_menu
    setopt always_to_end

    # autoloads
    autoload -U colors && colors
    autoload -U select-word-style && select-word-style bash

    # zstyle
    zstyle ':completion:*' menu select
    zstyle ':completion:*' group-name ''
    zstyle ':completion:::::' completer _expand _complete _ignored _approximate

    # binds
    bindkey -e
    bindkey '^[[3~' delete-char
    bindkey '^[3;5~' delete-char
    bindkey '^[[Z' reverse-menu-complete
fi

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
if hash nvim 2>/dev/null; then
    alias vim='nvim'
    EDITOR="nvim"
elif hash mvim 2>/dev/null; then
    alias vim='mvim -v'
    EDITOR="mvim -v"
elif hash vimx 2>/dev/null; then
    alias vim='vimx'
    EDITOR="vimx"
elif hash gvim 2>/dev/null; then
    alias vim='gvim -v'
    EDITOR="gvim -v"
else
    EDITOR="vim"
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
    python3 -m http.server "$port"
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
    python -c "print($*)"
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

# print out a key-value pair
# used in whereami
__print_kvpair() {
    printf  "${__COLOR_PURPLE}%-12s: ${__COLOR_CYAN}%s${__COLOR_RESTORE}\n" "$@"
}

# print user and location
whereami() {
    __print_kvpair "who & where" "${USER} @ ${HOSTNAME}"
    __print_kvpair "directory" "${PWD}"
    __print_kvpair "time" "$(date)"
    if [ -n "${VIRTUAL_ENV}" ]; then
        __print_kvpair "python venv" "${VIRTUAL_ENV}"
    fi
    if [ -n "${AWS_VAULT}" ]; then
        __print_kvpair "aws vault" "${AWS_VAULT}"
    fi
    if [ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" = "true" ]; then
        __print_kvpair "git branch" "$(git rev-parse --abbrev-ref HEAD)"
    fi
}
alias wai=whereami

# vim update plugins
vim_plug_update() {
    vim -c ':PlugUpdate'
}

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
    find -L "$HOME/.sdkman/candidates" \
        -maxdepth 3 -type d -path '*/current/bin' \
        -printf ':%p'
}

# Execute a command in a Python virtualenv
python_venv_exec() (
    local venvdir
    if [ -d "$1" ]; then
        venvdir=$1
        shift
    elif [ -f ".venv/bin/activate" ]; then
        venvdir=".venv"
    elif [ -f "virtualenv/bin/activate" ]; then
        venvdir="virtualenv"
    else
        echo "No virtualenv name given!" >&2
        return 1
    fi

    . "$venvdir/bin/activate"
    "$@"
)

set_prompt_extra() {
    export PS_EXTRA="$1"
}

precmd() {
    local last_exit=$?
    local status_color=$__PRC_OK
    local basedir topdir fulldir

    # Update history (bash only)
    if [ -n "$BASH_VERSION" ]; then
        history -a
    fi

    if [ "$last_exit" != 0 ]; then
        status_color=$__PRC_FAIL
    fi

    PS1=""
    if [ -z "$NO_LONG_PROMPT" ]; then
        # Current directory
        if [ "$PWD" = "$HOME" ]; then
            fulldir="${__PRC_BASEDIR}~"
        else
            basedir=$(dirname "$PWD")
            basedir=${basedir/${HOME}/"~"}
            topdir=$(basename "$PWD")
            fulldir="${__PRC_BASEDIR}${basedir}/${__PRC_RESTORE}${__PRC_TOPDIR_}${topdir}"
        fi

        PS1+=$'\n'
        PS1+="${__PRC_TIME}$(date "+%H:%M:%S") "
        PS1+="${fulldir}"
        PS1+=$'\n'
    fi
    PS1+="${status_color}${PS_EXTRA}>${__PRC_RESTORE} "

    # Include VTE specific additions
    if [ -n "$VTE_VERSION" ] && hash __vte_prompt_command 2>/dev/null; then
        __vte_prompt_command
    fi
}

# VTE -- this must be loaded before the prompt command is set
if [ "$VTE_VERSION" ] && [ -f /etc/profile.d/vte.sh ]; then
    . /etc/profile.d/vte.sh
fi

### exports ###

# Colors
__COLOR_RESTORE='\033[0m'
__COLOR_RED='\033[00;31m'
__COLOR_GREEN='\033[00;32m'
__COLOR_YELLOW='\033[00;33m'
__COLOR_BLUE='\033[00;34m'
__COLOR_PURPLE='\033[00;35m'
__COLOR_CYAN='\033[00;36m'
__COLOR_LIGHTGRAY='\033[00;37m'
__COLOR_LRED='\033[01;31m'
__COLOR_LGREEN='\033[01;32m'
__COLOR_LYELLOW='\033[01;33m'
__COLOR_LBLUE='\033[01;34m'
__COLOR_LPURPLE='\033[01;35m'
__COLOR_LCYAN='\033[01;36m'
__COLOR_WHITE='\033[01;37m'

if [ -n "$BASH_VERSION" ]; then
    __PRC_OK="\[${__COLOR_GREEN}\]"
    __PRC_FAIL="\[${__COLOR_RED}\]"
    __PRC_RESTORE="\[${__COLOR_RESTORE}\]"
    __PRC_BASEDIR="\[${__COLOR_CYAN}\]"
    __PRC_TOPDIR="\[${__COLOR_YELLOW}\]"
    __PRC_TIME="\[${__COLOR_PURPLE}\]"
elif [ -n "$ZSH_VERSION" ]; then
    __PRC_OK="%{%F{green}%}"
    __PRC_FAIL="%{%F{red}%}"
    __PRC_RESTORE="%{%f%}"
    __PRC_BASEDIR="%{%F{cyan}%}"
    __PRC_TOPDIR="%{%F{yellow}%}"
    __PRC_TIME="%{%F{magenta}%}"
fi

# Colors for ls
export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:'

# Prompt command (bash only)
if [ -n "$BASH_VERSION" ]; then
    export PROMPT_COMMAND=precmd
fi

# Default editor
export EDITOR
export VISUAL="$EDITOR"

# FZF
export FZF_DEFAULT_OPTS='--min-height=5'

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
if hash lesspipe.sh 2>/dev/null; then
    export LESSOPEN="|lesspipe.sh %s"
fi

# Completion
if [ -n "$BASH_VERSION" ]; then
    # Load general bash completion package
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
    if hash aws_completer 2>/dev/null; then
        complete -C aws_completer aws
    fi

    # Kubernetes
    if hash kubectl 2>/dev/null; then
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

    # FZF
    export FZF_DEFAULT_OPTS='--min-height=5'
    if [ -f /usr/share/fzf/shell/key-bindings.bash ]; then
        . /usr/share/fzf/shell/key-bindings.bash
    fi
elif [ -n "$ZSH_VERSION" ]; then
    # Compinit (updated daily)
    autoload -Uz compinit
    typeset -i __compinit_updated_at=$(\
        date +'%j' -r ~/.zcompdump 2>/dev/null \
        || stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null \
    )
    if [ "$(date +'%j')" != "$__compinit_updated_at" ]; then
      compinit -i
    else
      compinit -C -i
    fi

    # Kubernetes
    if hash kubectl 2>/dev/null; then
        source <(kubectl completion zsh)
    fi

    # Direnv
    if hash direnv 2>/dev/null; then
        eval "$(direnv hook zsh)"
    fi

    # FZF
    if [ -f /usr/share/fzf/shell/key-bindings.zsh ]; then
        . /usr/share/fzf/shell/key-bindings.zsh
    fi
fi

