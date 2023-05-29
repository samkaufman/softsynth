#!/usr/bin/env bash
#
# Runs a script on given SSH hosts.

set -e

REMOTE_DEST="/home/skaufman/softmax_box"
TMUX_SESSION_NAME=softmax-synth

readonly MAIN_HOST="$1"
shift
declare -a EXTRA_ARGS=( "$@" )

# Sync project directory.
rsync -vhra ./ "$MAIN_HOST:$REMOTE_DEST" --include='**.gitignore' \
  --exclude='/.git' --filter=':- .gitignore' --delete-after

ssh "${MAIN_HOST}" "cd $REMOTE_DEST && \
     tmux new-session -d -s '$TMUX_SESSION_NAME' \
    'racket softmax-interpreter.rkt ${EXTRA_ARGS[*]}' ';' \
    setw remain-on-exit on ';'"
