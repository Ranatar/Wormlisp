# Wormlisp

Симулятор локомоции *C. elegans* на Common Lisp: трёхмерное гидростатическое
тело, мышцы с кальциевой кинетикой, три уровня нервного управления и среда по
теории резистивных сил.

Сейчас сделан **этап 0**: каркас репозитория, все системы-заглушки, тесты,
REPL в tmux и проверочная страница на порту 8740.

## Окружение

| Что | Версия | Откуда |
| --- | --- | --- |
| ОС | Ubuntu 24.04 | виртуалка без графики |
| Common Lisp | **SBCL 2.2.9.debian** (минимальная проверенная версия) | `apt` |
| Библиотеки | закреплены в `vendor/` | см. [vendor/README.md](vendor/README.md) |
| Браузер | Firefox 115 ESR на хосте | |

Quicklisp не нужен: все библиотеки лежат в `vendor/`, а `scripts/init.lisp`
велит ASDF искать системы только там и в корне репозитория.

## Установка в виртуалке

```sh
sudo apt install sbcl tmux rlwrap git make
git clone https://github.com/Ranatar/Wormlisp.git ~/wormlisp
cd ~/wormlisp
make test     # первая сборка компилирует библиотеки, около полуминуты
make image    # образ worm-image.core, чтобы REPL стартовал мгновенно
```

`make test` заканчивается строкой `Fail: 0` и кодом возврата 0.

## Работа

```sh
scripts/repl.sh            # подключиться к REPL или запустить его
```

Скрипт держит SBCL в сессии tmux `worm` и сразу поднимает сервер на порту 8740.

- **Отключиться, не останавливая REPL:** `Ctrl-b`, затем `d`. Можно и просто
  закрыть PuTTY: при обрыве SSH сессия продолжает работать.
- **Вернуться:** снова `scripts/repl.sh`.
- **Остановить:** `scripts/repl.sh --stop` или `(sb-ext:exit)` в REPL.
  `Ctrl-d` в REPL тоже завершает SBCL, а с ним и сессию.
- **Запустить в фоне:** `scripts/repl.sh --detached`.
- Порт и память задаются переменными: `WORM_PORT=8741 WORM_HEAP_MB=1024 scripts/repl.sh`.

В REPL активен пакет `WORM-USER`. Сейчас там есть `(about)`, `(serve :port 8740)`
и `(stop-serving)`.

После `git pull` образ пересобирать не обязательно: ASDF сам перекомпилирует
изменённые файлы при старте. `make image` ускоряет следующий запуск.

## Проверка из браузера хоста

В настройках проброса портов гипервизора: порт хоста **8740** → порт гостя
**8740**. Затем в Firefox на хосте откройте <http://localhost:8740/>.

Страница «Wormlisp на связи» показывает версию SBCL, ревизию, память и ядра
процесса и проверяет, что в браузере есть то, на что опирается просмотрщик:
WebGL2, WebSocket, DataView с 64-битными целыми и Blob.slice.

## Раскладка

```text
worm.asd            все системы; слои — архитектура, «Модули и зависимости»
Makefile            test, test-full, image, repl, clean
src/<система>/      worm/platform, worm/core, … worm/analysis
src/package.lisp    фасадный пакет WORM и пакет WORM-USER
tests/              FiveAM; tests/structure.lisp сверяет слои с архитектурой
scripts/            init.lisp, repl.sh, run-tests.lisp, build-image.lisp
vendor/             библиотеки с закреплёнными коммитами
anatomy/ params/ configs/ sweeps/ viewer/ bench/   заполняются на своих этапах
```

## Документы проекта

Дизайн модели, архитектура, план этапов и параметры модели ведутся отдельно
в Claude Docs. Работа идёт по плану этапов.
