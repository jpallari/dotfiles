#!/bin/zsh

# options
setopt COMPLETE_IN_WORD
setopt APPEND_HISTORY
setopt MENU_COMPLETE
setopt INC_APPEND_HISTORY
setopt HIST_FIND_NO_DUPS
setopt bash_autolist
setopt autopushd pushdsilent
setopt beep extendedglob nomatch
unsetopt autocd notify

# variables
HISTFILE=~/.histfile
HISTSIZE=400
SAVEHIST=400
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'
eval `dircolors -b`

# zstyles
zstyle ':completion:*' completer _expand _complete _approximate
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' hosts off
zstyle ':completion:*' ignore-parents pwd
zstyle ':completion:*' last-prompt
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*' file-sort name
zstyle ':completion:*' select-prompt %SMenu active. Location: %p %s
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s

# Fix the annoying globbing issue with urls and scp
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
zstyle -e :urlglobber url-other-schema \
'[[ $words[1] == scp ]] && reply=("*") || reply=(http https ftp)'

autoload -Uz compinit
compinit

# Emacs mode
bindkey -e # Emacs mode

# http://zshwiki.org/home/zle/bindkeys
typeset -A key

key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char

function zle-line-init () {
    echoti smkx
}
function zle-line-finish () {
    echoti rmkx
}
zle -N zle-line-init
zle -N zle-keymap-select
zle -N zle-line-finish

# Prompt
autoload -U colors && colors
PROMPT="%{$fg_bold[red]%}\$ %{$reset_color%}"
RPROMPT=""

# Disable flow control
stty -ixon

# Exports
export PAGER=less

# Aliases
alias ls="ls --color=auto -F"
alias ll="ls -lh"
alias grep='grep --color=auto'
alias ..="cd .."
alias ..."cd ../.."
alias jk="tmux attach -d"
alias h="hostname"
alias e='emacsclient -c -t --alternate-editor=""'
alias em='emacs -Q -nw -l ~/.emacs'
alias er='emacsclient -n'
[[ -f $HOME/.zshaliases ]] && source $HOME/.zshaliases

# Local commands
[[ -f $HOME/.shlocal ]] && source $HOME/.shlocal

# Emacs shells and terminals
if [[ $TERM == "eterm-color" \
   || $TERM == "dumb" \
   || $EMACS != "" \
   || $INSIDE_EMACS != "" ]]; then
    PS1='$ '
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
fi

# Show the current host and path when the shell starts
echo "Host: $(hostname)"
echo "Path: $(pwd)"
