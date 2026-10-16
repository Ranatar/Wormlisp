#!/usr/bin/env bash
# scripts/run-sweep.sh — посчитать развёртку рабочими процессами.
#
#   scripts/run-sweep.sh sweeps/proprio-viscosity.sexp [число-рабочих]
#
# Готовые задания (runs/<id>/status = done) пропускаются; сводка — в
# runs/sweeps/<имя>/summary.csv. Рабочие стартуют из worm-image.core, если
# он собран (make image).
set -euo pipefail
cd "$(dirname "$0")/.."
sweep="${1:?путь к развёртке}"
workers="${2:-nil}"
cmd=(sbcl --noinform)
[ -f worm-image.core ] && cmd+=(--core worm-image.core)
cmd+=(--non-interactive --no-sysinit --no-userinit --load scripts/init.lisp
      --eval '(asdf:load-system "worm")'
      --eval "(worm:run-sweep \"$sweep\" :workers $workers)")
exec "${cmd[@]}"
