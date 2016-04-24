#!/bin/sh

# usage: mailapp.sh [mailto]

EMACS="/usr/bin/emacs"
EMACSCLIENT="/usr/bin/emacsclient"
SOCKET_NAME="email"
SOCKET_PATH="/tmp/emacs$(id -u)/$SOCKET_NAME"

log() {
    echo "$@" 1>&2
}

mail_start_elisp() {
    local after_start="${1:-nil}"
    local start_bg="${2:-nil}"
    echo "
(progn (setq server-name \"$SOCKET_NAME\")
       (server-start)
       (mu4e $start_bg)
       $after_start)"
}

open_mailapp() {
    local mailto="${1:-mailto:}"
    local compose_cmd="(browse-url-mail \"$mailto\")"
    local start_bg="nil"
    local additional_elisp="nil"

    if [ "$1" ]; then
        start_bg="t"
        additional_elisp="$compose_cmd"
    fi

    local start_elisp="$(mail_start_elisp "$additional_elisp" "$start_bg")"

    if [ -e "$SOCKET_PATH" ]; then
        log "Socket already open. Launching remotely with parameter: $1"
        log "Eval: $compose_cmd"
        "$EMACSCLIENT" -n -s "$SOCKET_NAME" --eval "$compose_cmd"
    else
        log "No socket open. Launching local with parameter: $1"
        log "Eval: $start_elisp"
        "$EMACS" --eval "$start_elisp"
    fi
}

open_mailapp "$@"
