#!/usr/bin/env bash
# scripts/repl.sh — REPL проекта в сессии tmux «worm».
#
# Если сессия уже есть, просто подключается к ней: разрыв SSH её не убивает,
# и после переподключения из PuTTY достаточно снова запустить этот скрипт.
# Если сессии нет, запускает SBCL (из worm-image.core, когда он собран),
# загружает систему worm и поднимает сервер на порту WORM_PORT (по умолчанию 8740).
#
#   scripts/repl.sh             подключиться или запустить и подключиться
#   scripts/repl.sh --detached  запустить в фоне, не подключаясь
#   scripts/repl.sh --stop      остановить сессию
#
# Переменные: WORM_PORT (8740), WORM_HEAP_MB (1536).

set -euo pipefail
cd "$(dirname "$0")/.."
SESSION=worm

case "${1:-}" in
  --stop)
    tmux kill-session -t "$SESSION" 2>/dev/null && echo "Сессия $SESSION остановлена." || echo "Сессии $SESSION нет."
    exit 0 ;;
esac

if ! tmux has-session -t "$SESSION" 2>/dev/null; then
  cmd=(sbcl --noinform)
  [ -f worm-image.core ] && cmd+=(--core worm-image.core)
  cmd+=(--dynamic-space-size "${WORM_HEAP_MB:-1536}"
        --no-sysinit --no-userinit
        --load scripts/init.lisp --load scripts/repl-start.lisp
        --eval "(in-package :worm-user)")
  command -v rlwrap >/dev/null && cmd=(rlwrap "${cmd[@]}")
  tmux new-session -d -s "$SESSION" -e "WORM_PORT=${WORM_PORT:-8740}" "$(printf '%q ' "${cmd[@]}")"
  echo "Запущена сессия $SESSION."
fi

if [ "${1:-}" = "--detached" ]; then
  exit 0
fi
exec tmux attach -t "$SESSION"
