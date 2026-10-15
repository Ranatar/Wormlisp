#!/usr/bin/env bash
# tools/firefox/install.sh — собрать Firefox 115 из частей и распаковать.
#
#   tools/firefox/install.sh [каталог]     по умолчанию /opt/ff115
#
# Проверяет контрольные суммы частей и собранного архива, распаковывает
# в <каталог>/firefox. Библиотеки, нужные Firefox на Ubuntu 24.04:
#   sudo apt install libdbus-glib-1-2 libgtk-3-0t64 libasound2t64 libx11-xcb1 libxtst6
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
dest="${1:-/opt/ff115}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

( cd "$here" && grep 'part[0-9]*$' SHA256SUMS | sha256sum -c --quiet - )
cat "$here"/firefox-115.0.tar.bz2.part* > "$tmp/firefox-115.0.tar.bz2"
( cd "$tmp" && grep ' firefox-115.0.tar.bz2$' "$here/SHA256SUMS" | sha256sum -c --quiet - )
mkdir -p "$dest"
tar xjf "$tmp/firefox-115.0.tar.bz2" -C "$dest"
"$dest/firefox/firefox" --version
