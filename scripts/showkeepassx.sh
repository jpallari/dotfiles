#!/bin/sh -e

function xdokeepassx {
    xdotool search --maxdepth 3 --classname keepassx $@
}

function show_keepassx {
    local desktop="`xdotool get_desktop`"
    xdokeepassx \
        set_desktop_for_window %@ $desktop \
        windowactivate %@ \
        > /dev/null 2>&1
}

function hide_keepassx {
    xdokeepassx windowminimize %@ > /dev/null 2>&1
}

function is_keepassx_shown {
    local active_id="`xdotool getactivewindow`"
    xdokeepassx | grep -F $active_id > /dev/null 2>&1
}

if is_keepassx_shown; then
    hide_keepassx
else
    show_keepassx
fi

