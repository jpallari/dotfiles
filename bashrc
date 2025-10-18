# Configuration for both bash and zsh

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# history
HISTFILE="$HOME/.shell_history"
HISTCONTROL=ignoreboth
HISTIGNORE='bg:fg:history'
HISTSIZE=100000
SAVEHIST=$HISTSIZE
HISTFILESIZE=$HISTSIZE

# detected shell in one variable
if [ -n "$BASH_VERSION" ]; then
    _dotfile_shell=bash
elif [ -n "$ZSH_VERSION" ]; then
    _dotfile_shell=zsh
fi

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
    setopt hist_verify
    setopt hist_ignore_space
    setopt share_history
    setopt auto_list
    setopt auto_menu
    setopt always_to_end
    setopt interactive_comments
    setopt complete_in_word
    setopt extended_history
    setopt prompt_subst

    # autoloads
    autoload -U colors && colors
    autoload -U select-word-style && select-word-style bash

    # zstyle
    zstyle ':completion:*' menu select
    zstyle ':completion:*' group-name ''
    zstyle ':completion:::::' completer _expand _complete _ignored _approximate

    # zsh foreground program
    zle_fg() {
        echo -en "\e[s"
        fg
        echo -en "\e[u\e[0J\e[1A"
        zle redisplay
    }
    zle -N zle_fg

    # binds
    bindkey -e
    bindkey '^[[3~' delete-char
    bindkey '^[3;5~' delete-char
    bindkey '^[[Z' reverse-menu-complete
    bindkey '\e[H' beginning-of-line
    bindkey '\eOH' beginning-of-line
    bindkey '\e[F' end-of-line
    bindkey '\eOF' end-of-line
    bindkey '^Z' zle_fg
fi

### aliases, functions, commands ###

# OS specific
case "${OSTYPE}" in
linux*)
    alias ls='ls --color=auto -F'
    ;;
darwin*)
    # newer python
    if
        command -v python3 >/dev/null &&
            ! command -v python >/dev/null
    then
        alias python=python3
    fi

    alias hexfiend='open -a "Hex Fiend"'
    ;;
*)
    alias ls='ls -F'
    ;;
esac

# GUI specific
if [ -n "${WSLENV:-}" ]; then
    alias pbcopy='clip.exe'
    alias pbpaste='powershell.exe get-clipboard | sed "s/\r//" | head -c -1'
    open() {
        explorer.exe "$(wslpath -w "$1")"
    }

    # keychain setup to remember SSH keys (mostly for WSL)
    init_keychain() {
        eval "$(keychain --eval --agents ssh id_ed25519)"
    }
elif [ -n "${XDG_SESSION_TYPE:-}" ]; then
    alias open='xdg-open >/dev/null 2>&1'
    case "${XDG_SESSION_TYPE}" in
    'x11')
        alias pbcopy='xsel --clipboard --input'
        alias pbpaste='xsel --clipboard --output'
        ;;
    'wayland')
        alias pbcopy='wl-copy'
        alias pbpaste='wl-paste'
        ;;
    esac
fi

# vim in terminal w/ clipboard support etc.
if command -v nvim >/dev/null; then
    alias vim='nvim'
    EDITOR='nvim'
    MANPAGER='nvim +Man!'
elif command -v mvim >/dev/null; then
    alias vim='mvim -v'
    EDITOR='mvim -v'
elif command -v vimx >/dev/null; then
    alias vim='vimx'
    EDITOR='vimx'
elif command -v gvim >/dev/null; then
    alias vim='gvim -v'
    EDITOR='gvim -v'
elif command -v vim >/dev/null; then
    EDITOR='vim'
else
    EDITOR='vi'
fi

# newer gpg
if command -v gpg2 >/dev/null; then
    alias gpg=gpg2
fi

alias ll='ls -lh'
alias grep='grep --color=auto'

# poll a given command every n seconds
poll_cmd() {
    local interval
    case "$1" in
    [0-9]*)
        interval=$1
        shift
        ;;
    *)
        interval=4
        ;;
    esac
    while true; do
        "$@"
        sleep "$interval"
    done
}

# run ripgrep and pipe it to less (with colors)
rgless() {
    rg -p "${@}" | less -R
}

# run jq and pipe it to less (with colors)
jqless() {
    jq -C "${@}" | less -R
}

# live preview jq results
jqpreview() {
    local query
    query="$(echo '' | fzf --layout reverse --info=inline --print-query --preview-window "down,99%,wrap" --preview "jq {q} ${1}")"
    if [ -n "${query}" ]; then
        jq "${query}" "${1}"
    fi
}

GCD_PROJECT_DIRECTORY=$HOME/projects
GCD_PROJECT_DIRLIST="$GCD_PROJECT_DIRECTORY/.projects"
GCD_LAST_JUMP_FILE="$GCD_PROJECT_DIRECTORY/.lastjump"

# update jump list for gcd
gcdu() {
    if
        [ "$1" != "lazy" ] ||
            ! [ -s "$GCD_PROJECT_DIRLIST" ]
    then
        find "$GCD_PROJECT_DIRECTORY" -maxdepth 4 -type d -name '.git' -prune |
            sed -e "s#^$GCD_PROJECT_DIRECTORY/##" -e 's#/\.git$##' |
            sort \
                >"$GCD_PROJECT_DIRLIST"
    fi
}

# jump to a git project directory
gcd() {
    if [ "$1" = '-' ] && [ -s "$GCD_LAST_JUMP_FILE" ]; then
        cd "$(cat "$GCD_LAST_JUMP_FILE")" || return 1
        return 0
    fi

    local dir

    gcdu lazy
    if command -v gorg >/dev/null; then
        dir=$(gorg find -f "$@")
    else
        dir=$(fzf --query="$1" --select-1 <"$GCD_PROJECT_DIRECTORY/.projects")
        dir="${GCD_PROJECT_DIRECTORY}/${dir}"
    fi
    if [ -n "${dir:-}" ]; then
        echo "$dir" > "$GCD_LAST_JUMP_FILE"
        cd "$dir" || return 1
    else
        return 1
    fi
}

# run a command in a directory
gcdr() (
    local query
    if [ "${1}" = '-q' ]; then
        query="${2}"
        shift
        shift
    fi
    if [ -z "${1:-}" ]; then
        echo "no command specified" >&2
        return 1
    fi
    gcd "${query}"
    "${@}"
)

# open the git origin in a browser
gbrowse() (
    if [ -n "${1}" ]; then
        cd "${1}" || return 1
    fi
    open "$(git config --get remote.origin.url)"
)

# print current epoch
unixepoch() {
    date '+%s000'
}

# share files over HTTP quickly
httpserver() (
    local port=${1:-10101}
    local dir=${2:-.}
    cd "$dir" || return 1
    python3 -m http.server "$port"
)

# print a horizontal line
hr() {
    printf -v line "%${COLUMNS}s" ""
    echo "${line// /=}"
}

# python oneliner
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

# display current directory color coded
color_pwd() {
    local basedir topdir fulldir

    if [ "$PWD" = "$HOME" ]; then
        fulldir="${__COLOR_CYAN}~"
    else
        basedir=${PWD%/*}
        basedir=${basedir/${HOME}/"~"}
        topdir=${PWD##*/}
        fulldir="${__COLOR_CYAN}${basedir}/${__COLOR_RESTORE}${__COLOR_YELLOW}${topdir}"
    fi

    printf "${fulldir}${__COLOR_RESTORE}\n"
}

# print out a key-value pair
# used in whereami
__print_kvpair() {
    printf "${__COLOR_LBLUE}%s${__COLOR_LBLACK}:${__COLOR_RESTORE} %s\n" "$@"
}

# print user and location
whereami() {
    __print_kvpair "user " \
        "$(printf "%s ${__COLOR_CYAN}@${__COLOR_RESTORE} %s${__COLOR_RESTORE}" \
            "$USER" "${HOSTNAME:-$HOST}" \
        )"
    __print_kvpair "dir  " "$(color_pwd)"
    if [ -n "${VIRTUAL_ENV}" ]; then
        __print_kvpair "venv " "${VIRTUAL_ENV}"
    fi
    if [ -n "${AWS_VAULT}" ]; then
        __print_kvpair "aws  " "${AWS_VAULT}"
    fi
    if [ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" = "true" ]; then
        __print_kvpair "git  " "$(git rev-parse --abbrev-ref HEAD)"
    fi
}
alias wai=whereami

# set the current window title
set_window_title() {
    echo -ne "\033]0;$@\007"
}

# current time zone
get_current_time_zone() {
    timedatectl status |
        grep "Time zone" |
        sed 's/.*Time zone: \([^ ]\+\) .*/\1/'
}

# sdk man initialiser
init_sdkman() {
    if [ -f "$HOME/.sdkman/bin/sdkman-init.sh" ]; then
        export SDKMAN_DIR="$HOME/.sdkman"
        . "$HOME/.sdkman/bin/sdkman-init.sh"
    fi
}

# find currently used SDKs from SDK man in $PATH format
find_sdkman_paths() {
    local find_cmd=find
    if command -v gfind >/dev/null; then
        find_cmd=gfind
    fi
    "$find_cmd" -L "$HOME/.sdkman/candidates" \
        -maxdepth 3 -type d -path '*/current/bin' \
        -printf ':%p'
}

# setup hook shell scripts for all the relevant tools
shell_setup_tool_hooks() {
    local script_path

    for dotfile_shell in zsh bash; do
        script_path="$HOME/.local_tools.$dotfile_shell"
        echo "tools for shell: $dotfile_shell" >&2

        {
            # init
            echo "# script for loading $dotfile_shell tool hooks"
            echo "# generated on $(date)"
            echo ""

            # pipenv
            if command -v pipenv >/dev/null; then
                echo "pipenv found" >&2
                pipenv --completion
            fi

            # kubernetes
            if command -v kubectl >/dev/null; then
                echo "kubectl found" >&2
                kubectl completion "$dotfile_shell"
            fi

            # k3d
            if command -v k3d >/dev/null; then
                echo "k3d found" >&2
                k3d completion "$dotfile_shell"
            fi

            # direnv
            if command -v direnv >/dev/null; then
                echo "direnv found" >&2
                direnv hook "$dotfile_shell"
            fi

            # hcloud
            if command -v hcloud >/dev/null; then
                echo "hcloud found" >&2
                hcloud completion "$dotfile_shell"
            fi

            # wezterm
            if command -v wezterm >/dev/null; then
                echo "wezterm found" >&2
                wezterm shell-completion --shell "$dotfile_shell"
            fi

            # limactl
            if command -v limactl >/dev/null; then
                echo "limactl found" >&2
                limactl completion "$dotfile_shell"
            fi
        } >"$script_path"
        echo "" >&2
    done
}

# ssh but don't track known hosts (useful for temporary servers)
ssh_no_save() {
    ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no "$@"
}

### prompt ###

# add extra content to the prompt
set_prompt_extra() {
    export PS_EXTRA="$1"
}

chpwd() {
    emulate -L zsh
    color_pwd
}

precmd() {
    local last_exit=$?
    local prompt_status
    local fulldirnocolor

    # update history (bash only)
    if [ -n "$BASH_VERSION" ]; then
        history -a
    fi

    PS1=""
    if [ "$last_exit" != 0 ]; then
        prompt_status="$__PRC_FAIL"
    fi
    PS1+="${PS_EXTRA}${prompt_status:-}\$${__PRC_RESTORE} "

    if [ "$PWD" = "$HOME" ]; then
        fulldirnocolor="~"
    else
        basedir=${PWD%/*}
        basedir=${basedir/${HOME}/"~"}
        topdir=${PWD##*/}
        fulldirnocolor="${basedir}/${topdir}"
    fi

    # include VTE specific additions
    if [ -n "$BASH_VERSION" ] &&
        [ -n "$VTE_VERSION" ] &&
        type __vte_prompt_command >/dev/null 2>&1; then
        __vte_prompt_command
    else
        # print terminal title (vte has its own implementation)
        echo -n -e "\033]0;${USER}@${HOSTNAME:-$HOST}:${fulldirnocolor}\007"
    fi

    # OSC7: New window in the same path
    local pwd_strlen=${#PWD}
    local encoded=""
    local pos c o
    for ((pos = 0; pos < pwd_strlen; pos++)); do
        c=${PWD:$pos:1}
        case "$c" in
        [-/:_.!\'\(\)~[:alnum:]]) o="${c}" ;;
        *) printf -v o '%%%02X' "'${c}" ;;
        esac
        encoded+="${o}"
    done
    printf '\e]7;file://%s%s\e\\' "${HOSTNAME}" "${encoded}"

    # include WSL specific additions
    if [ -n "${WSLENV:-}" ]; then
        printf "\e]9;9;%s\e\\" "$(wslpath -w "$PWD")"
    fi
}

# vte -- this must be loaded before the prompt command is set
if [ -n "$VTE_VERSION" ] &&
    ! type __vte_prompt_command >/dev/null 2>&1 &&
    [ -f /etc/profile.d/vte.sh ]; then
    . /etc/profile.d/vte.sh
fi

### exports ###

# colors
__COLOR_RESTORE='\e[0m'
__COLOR_BLACK='\e[0;30m'
__COLOR_RED='\e[0;31m'
__COLOR_GREEN='\e[0;32m'
__COLOR_YELLOW='\e[0;33m'
__COLOR_BLUE='\e[0;34m'
__COLOR_PURPLE='\e[0;35m'
__COLOR_CYAN='\e[0;36m'
__COLOR_WHITE='\e[0;37m'
__COLOR_LBLACK='\e[1;30m'
__COLOR_LRED='\e[1;31m'
__COLOR_LGREEN='\e[1;32m'
__COLOR_LYELLOW='\e[1;33m'
__COLOR_LBLUE='\e[1;34m'
__COLOR_LPURPLE='\e[1;35m'
__COLOR_LCYAN='\e[1;36m'
__COLOR_LWHITE='\e[1;37m'

if [ -n "$BASH_VERSION" ]; then
    __PRC_OK="\[${__COLOR_GREEN}\]"
    __PRC_FAIL="\[${__COLOR_RED}\]"
    __PRC_RESTORE="\[${__COLOR_RESTORE}\]"
elif [ -n "$ZSH_VERSION" ]; then
    __PRC_OK="%{%F{green}%}"
    __PRC_FAIL="%{%F{red}%}"
    __PRC_RESTORE="%{%f%}"
fi

# colors for ls
export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:'
export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd
export CLICOLOR=1

# prompt command (bash only)
if [ -n "$BASH_VERSION" ]; then
    export PROMPT_COMMAND=precmd
fi

# default editor
export EDITOR
export VISUAL="$EDITOR"
[ -n "${MANPAGER}" ] && export MANPAGER

# fzf
export FZF_DEFAULT_OPTS='--min-height=5'

# python
export PIPENV_VENV_IN_PROJECT=1

# node
export NPM_PACKAGES="$HOME/.local/share/npm-global"
export NVM_DIR="$HOME/.nvm"
resolve_nvm_dir() {
    local nvm_bin_paths nvm_bin_path nvm_version

    if ! [ -d "$NVM_DIR/versions/node" ]; then
        return
    fi

    if [ -s "$NVM_DIR/alias/default" ]; then
        nvm_version=$(cat "$NVM_DIR/alias/default")
        nvm_bin_path="$NVM_DIR/versions/node/$nvm_version/bin"
        if [ -d "$nvm_bin_path" ]; then
            echo "$nvm_bin_path"
        else
            echo "nvm default version ($nvm_version) not installed" >&2
        fi
    else
        nvm_bin_paths=("$NVM_DIR"/versions/node/*/bin) 2>/dev/null
        echo "${NVM_BIN_PATHS[-1]}"
    fi
}
DEFAULT_NVM_BIN_PATH=$(resolve_nvm_dir)

# go
if [ -x "$HOME/Apps/go/bin/go" ]; then
    export GOROOT="$HOME/Apps/go"
fi
export GOPATH="$HOME/go"

# aws
if [ -n "${WSLENV:-}" ]; then
    export AWS_VAULT_BACKEND=wincred
fi
export AWS_SESSION_TOKEN_TTL=8h
export AWS_ASSUME_ROLE_TTL=8h

### custom paths. customize in ~/.local.sh ###
CUSTOM_PATH=""

# add ~/.local/bin if not added already
if ! [[ "$PATH" =~ .*"$HOME/.local/bin".* ]]; then
    CUSTOM_PATH="$HOME/.local/bin"
fi

# default nvm node bin
if [ -n "$DEFAULT_NVM_BIN_PATH" ]; then
    CUSTOM_PATH+=":$DEFAULT_NVM_BIN_PATH"
fi

# go path and custom go root in path
if [ -n "$GOROOT" ]; then
    CUSTOM_PATH+=":$GOROOT/bin"
fi
if [ -d "$GOPATH/bin" ]; then
    CUSTOM_PATH+=":$GOPATH/bin"
fi

# local configurations
[[ -f $HOME/.local.sh ]] && source "$HOME/.local.sh"

# load SDK man paths
if [ -d "$HOME/.sdkman/candidates" ]; then
    CUSTOM_PATH+=$(find_sdkman_paths)
fi

# drop hanging semicolons from custom path
CUSTOM_PATH=${CUSTOM_PATH%:}
CUSTOM_PATH=${CUSTOM_PATH#:}

# init paths
if [ -z "$CUSTOM_PATH_SET" ] && [ -n "$CUSTOM_PATH" ]; then
    export DEFAULT_PATH="$CUSTOM_PATH:$PATH"
    export PATH="$DEFAULT_PATH"
    export CUSTOM_PATH_SET=1
fi

### load bunch of stuff ###

# lesspipe
if command -v lesspipe.sh >/dev/null; then
    export LESSOPEN="|lesspipe.sh %s"
fi

# nvm
init_nvm() {
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
}

### completion ###

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
elif [ -n "$ZSH_VERSION" ]; then
    # compinit
    autoload -Uz compinit && compinit -C
    autoload -Uz bashcompinit && bashcompinit
fi

if [ -n "$_dotfile_shell" ]; then
    # build local tool hooks, if not present
    if [ ! -s "$HOME/.local_tools.$_dotfile_shell" ]; then
        shell_setup_tool_hooks
    fi

    # load local tool hooks
    if [ -s "$HOME/.local_tools.$_dotfile_shell" ]; then
        . "$HOME/.local_tools.$_dotfile_shell"
    fi

    # fzf
    if [ -f "/usr/share/fzf/shell/key-bindings.$_dotfile_shell" ]; then
        . "/usr/share/fzf/shell/key-bindings.$_dotfile_shell"
    elif [ -f "$HOME/.local/share/fzf/shell/key-bindings.$_dotfile_shell" ]; then
        . "$HOME/.local/share/fzf/shell/key-bindings.$_dotfile_shell"
    fi
fi

# aws cli
if command -v aws_completer >/dev/null; then
    complete -C aws_completer aws
fi

# terraform
if [ -n "$TERRAFORM_PATH" ]; then
    complete -C "$TERRAFORM_PATH" terraform
    complete -o nospace -C "$TERRAFORM_PATH" terraform
fi

### display information every new shell ###

whereami
