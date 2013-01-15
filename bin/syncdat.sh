#!/bin/bash -e

# rsync and configuration file locations
RSYNC="$(which rsync)"
RS_CONFNAME=".syncdat"

# These are the default options for pulling pushing
PUSH_PARAMS="-avz --delete"
PULL_PARAMS="-avz"

# Exclude these files by default
EXCLUDE_FILES=(
    $RS_CONFNAME
    ".git"
    ".svn"
    ".hg"
)


## functions

function include_config {
    if ! source "$RS_CONFNAME" 2>/dev/null
    then
        echo "No configuration file found. Run: $(basename $0) init"
        exit 1
    fi
}


function mk_commandparams {
    local excludeparams
    excludeparams=$(mk_excludeparams EXCLUDE_FILES[@])
    case $1 in
        push)
            echo $excludeparams $PUSH_PARAMS . $REMOTE_REPO
            ;;
        pull)
            echo $excludeparams $PULL_PARAMS $REMOTE_REPO .
            ;;
    esac
}


function syncdat {
    include_config

    if [ -z "$REMOTE_REPO" ]
    then
        echo "You must specify a remote repository in your configuration file."
    else
        $RSYNC $(mk_commandparams $1)
    fi
}


function syncdat_debug {
    include_config
    echo "push = $RSYNC $(mk_commandparams push)"
    echo "pull = $RSYNC $(mk_commandparams pull)"
}


function mk_initfile {
    cat <<EOF > "$RS_CONFNAME"
## syncdat config

# Remote repository
REMOTE_REPO="$1"

# Uncomment these to override default rsync push and pull parameters
# PUSH_PARAMS="${PUSH_PARAMS}"
# PULL_PARAMS="${PULL_PARAMS}"

# Exclude these files from sync
EXCLUDE_FILES=(
    "$RS_CONFNAME"
)
EOF
}


function mk_excludeparams {
    declare -a paramArr=("${!1}")
    for var in "${paramArr[@]}"; do
        echo -n "--exclude ${var} "
    done
}


function show_help {
    echo "usage: $(basename $0) push | pull | init [file] | debug"
}


## run it!

case $1 in
    push|pull) syncdat $1 ;;
    init) mk_initfile $2 ;;
    debug) syncdat_debug ;;
    *) show_help ;;
esac
