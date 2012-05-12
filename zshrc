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

# Basic key bindings
bindkey "\e[1~" beginning-of-line # Home
bindkey "\e[4~" end-of-line # End
bindkey "\e[5~" beginning-of-history # PageUp
bindkey "\e[6~" end-of-history # PageDown
bindkey "\e[2~" quoted-insert # Ins
bindkey "\e[3~" delete-char # Del
bindkey "\e[5C" forward-word
bindkey "\eOc" emacs-forward-word
bindkey "\e[5D" backward-word
bindkey "\eOd" emacs-backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
bindkey "\e[Z" reverse-menu-complete # Shift+Tab
bindkey "\e[A" up-line-or-history
bindkey "\e[B" down-line-or-history
# for rxvt
bindkey "\e[7~" beginning-of-line # Home
bindkey "\e[8~" end-of-line # End

# ncurses fogyatekos
[[ "$terminfo[kcuu1]" == "O"* ]] && bindkey -M viins "${terminfo[kcuu1]/O/[}" up-line-or-history
[[ "$terminfo[kcud1]" == "O"* ]] && bindkey -M viins "${terminfo[kcud1]/O/[}" down-line-or-history
[[ "$terminfo[kcuf1]" == "O"* ]] && bindkey -M viins "${terminfo[kcuf1]/O/[}" vi-forward-char
[[ "$terminfo[kcub1]" == "O"* ]] && bindkey -M viins "${terminfo[kcub1]/O/[}" vi-backward-char
[[ "$terminfo[khome]" == "O"* ]] && bindkey -M viins "${terminfo[khome]/O/[}" beginning-of-line
[[ "$terminfo[kend]" == "O"* ]] && bindkey -M viins "${terminfo[kend]/O/[}" end-of-line
[[ "$terminfo[khome]" == "O"* ]] && bindkey -M emacs "${terminfo[khome]/O/[}" beginning-of-line
[[ "$terminfo[kend]" == "O"* ]] && bindkey -M emacs "${terminfo[kend]/O/[}" end-of-line

# Search
bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '^r' history-incremental-search-backward
bindkey -M viins '^s' history-incremental-search-forward
bindkey -M vicmd '^s' history-incremental-search-forward
bindkey -M vicmd '^s' history-incremental-search-forward

# Other keybindings
bindkey -M viins '^w' backward-delete-word
bindkey -M viins '^h' backward-delete-char
bindkey -M viins "^?" backward-delete-char
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins '^n' down-line-or-history
bindkey -M viins '^p' up-line-or-history
bindkey -M viins '^k' kill-line
bindkey -M viins '^f' forward-char
bindkey -M viins '^b' backward-char
bindkey -M viins 'f' forward-word
bindkey -M viins 'b' backward-word

# Vi or Emacs mode
bindkey -v # Vi mode
#bindkey -e # Emacs mode

# Prompt
autoload -U colors && colors
#PROMPT="%{$fg[red]%}>%{$fg_bold[red]%}> %{$reset_color%}"
PROMPT="%{$fg_bold[red]%}\$ %{$reset_color%}"
RPROMPT=""

# Fun time VI mode prompt for great justice
VIMODE=""
function zle-line-init zle-keymap-select {
    VIMODE="${${KEYMAP/vicmd/-CMD-}/(main|viins)/}"
    RPROMPT="%{$fg_bold[red]%}${VIMODE}%{$reset_color%}"
    zle reset-prompt
}

zle -N zle-keymap-select
zle -N zle-line-init

# Disable flow control
stty -ixon

# Exports
export PYTHONSTARTUP="$HOME/.pyrc"
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
alias em='emacs -nw'
[[ -f $HOME/.zshaliases ]] && source $HOME/.zshaliases

if [[ $TERM == "dumb" || $EMACS != "" ]]; then
    PS1='$ '
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
fi
