#!/usr/bin/env bash
#
# Runs collect_times.py on a remote machine. Takes the SSH host
# and remote folder as arguments.

set -e

readonly MAIN_HOST="$0"
readonly REMOTE_DEST="$1"
readonly TMUX_SESSION_NAME=softsynth

shift
shift
declare -a EXTRA_ARGS=( "$@" )

# Copy .gitignore so it's is respected at the destination during the rsync.
ssh "${MAIN_HOST}" "mkdir -p \"$REMOTE_DEST\""
scp .gitignore "$MAIN_HOST:$REMOTE_DEST/.gitignore"

# Sync project directory.
rsync -vhra ./ "$MAIN_HOST:$REMOTE_DEST" --include='**.gitignore' \
  --exclude='/.git' --filter=':- .gitignore' --delete-after

ssh "${MAIN_HOST}" "cd $REMOTE_DEST && \
     tmux new-session -d -s '$TMUX_SESSION_NAME' \
    'python3 ./collect_times.py ${EXTRA_ARGS[*]}' ';' \
    setw remain-on-exit on ';'"
