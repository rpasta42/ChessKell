#!/bin/sh

tmp=`mktemp "${TMPDIR:-/tmp}/pxboard.$$.XXXXXX"` || exit 1
cat > "$tmp"
( xboard -initialMode MachineWhite -ncp -lgf "$tmp" "$@" ; rm "$tmp" ) &

