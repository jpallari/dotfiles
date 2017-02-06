#!/bin/sh

SESSION="${1:-main}"
DEVDIR="$HOME/Projects"

if tmux has-session -t "$SESSION" 2>/dev/null; then
    echo "Tmux session $SESSION already exists" 1>&2
    exit 1
fi

tmux new-session -c "$HOME" -s "$SESSION" \; \
     rename-window "main" \; \
     split-window -h \; \
     new-window -c "$DEVDIR" -n "dev" \; \
     split-window -c "$DEVDIR" -h
