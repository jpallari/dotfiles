#!/bin/sh
set -eu

log_msg() {
    echo "$1" 1>&2
}

cmd_exists() {
    hash "$1" 2>&1
}

no_clipboard_found() {
    log_msg "No clipboard command found!"
    return 1
}

copy_from_stdin() {
    if cmd_exists "xsel"; then
        xsel -i -p -b
    elif cmd_exists "pbcopy"; then
        pbcopy
    elif [ -n "$WSLENV" ] && cmd_exists "clip.exe"; then
        clip.exe
    else
        no_clipboard_found
    fi
}

paste_to_stdout() {
    if cmd_exists "xsel"; then
        xsel -o -b
    elif cmd_exists "pbpaste"; then
        pbpaste
    elif [ -n "$WSLENV" ] && cmd_exists powershell.exe; then
        powershell.exe get-clipboard | sed 's/\r//' | head -c -1
    else
        no_clipboard_found
    fi
}

main() {
    case "${1:-}" in
        c|copy)
            copy_from_stdin ;;
        p|paste)
            paste_to_stdout ;;
        *)
            log_msg "usage: $(basename "$0") (copy|paste)"
            return 1
            ;;
    esac
}

main "$@"

