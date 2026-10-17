#!/bin/sh
# scripts/boyle-wormsim.sh — эталонные прогоны исходной модели Boyle, Berri,
# Cohen 2012 (WormSim) для сверки второго уровня (план, этап 15).
#
#   scripts/boyle-wormsim.sh <рабочий каталог> [длительность, с]
#
# Клонирует openworm/CelegansNeuromechanicalGaitModulation на закреплённом
# коммите, собирает Sundials 2.3 и программу, заменяет в worm.cc константы
# DURATION и MEDIUM чтением переменных окружения (больше ничего не меняет) и
# считает среды из MEDIA. Результат — <каталог>/m<среда>/simdata.csv;
# в WCON его переводит scripts/make-boyle-reference.lisp.
set -e
WORK=${1:?нужен рабочий каталог}
DURATION=${2:-15}
COMMIT=c2455768a7c01b9c30b7fd3329121fbe3cec748f
MEDIA=${MEDIA:-"0 0.001 0.01 0.1 1"}
mkdir -p "$WORK"
cd "$WORK"
if [ ! -d src ]; then
  git clone -q https://github.com/openworm/CelegansNeuromechanicalGaitModulation src
fi
cd src && git checkout -q "$COMMIT" && cd WormSim
if [ ! -f Sundials/lib/libsundials_ida.a ]; then
  (cd sundials-2.3.0 && ./configure CC=g++ --prefix="$(pwd)/../Sundials" --disable-mpi --disable-fcmix >/dev/null \
     && make >/dev/null && make install >/dev/null)
fi
cd Model
sed -i 's|^#define DURATION  10.*|#define DURATION (getenv("DURATION")?atof(getenv("DURATION")):10.0)|; s|^#define MEDIUM 1.0.*|#define MEDIUM (getenv("MEDIUM")?atof(getenv("MEDIUM")):1.0)|' worm.cc
make >/dev/null 2>&1
PROGRAM="$(pwd)/program"
for m in $MEDIA; do
  mkdir -p "$WORK/m$m"
  (cd "$WORK/m$m" && DURATION=$DURATION MEDIUM=$m "$PROGRAM" > run.log 2>&1) &
done
wait
echo "готово: $WORK/m*/simdata.csv"
