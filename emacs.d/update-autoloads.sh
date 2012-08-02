#!/bin/bash

LOADPATHS=("vendor")
TEMPFILE="loaddefs.el.temp"
REALFILE="loaddefs.el"

for lp in $LOADPATHS; do
    temp="$lp/$TEMPFILE"
    real="$lp/$REALFILE"
    echo "" > $temp
    find "$lp" -name "*-autoloads.el" -print0 | xargs -0 cat >> $temp
    [[ -f $real ]] && rm $real
    mv $temp $real
done
