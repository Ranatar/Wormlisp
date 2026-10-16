# Wormlisp

Симулятор локомоции *C. elegans* на Common Lisp: трёхмерное гидростатическое
тело, мышцы с кальциевой кинетикой, три уровня нервного управления и среда по
теории резистивных сил.

Сделаны **этапы 0–7**: каркас репозитория, REPL в tmux, проверочная страница
на порту 8740; нижние слои — типы и условия, журнал, реестр параметров с
единицами, трубка N × K с величинами покоя и 95 мышц с привязкой к рёбрам;
решатель — ленточная LU, линейно-неявный шаг с расщеплением, контрольные
точки и перезапуски при численных сбоях; тело и среда на ручных ядрах
(tests/oracle/): червь плывёт и ползёт под заданной волной натяжения;
запись прогонов в runs/, метрики походки и офлайн-просмотрщик в браузере;
мышцы с кальциевой кинетикой под генератором ритма, подобранные по кинематике
живых червей (1,76 Гц и 1,54 L в воде, 0,30 Гц и 0,62 L на агаре); язык
модели — IR с символьной производной, `define-component` и `defmodel` с
проверками и эталонным интерпретатором; кодогенерация — модель `worm-3d` на
языке модели собирается в типизированные функции без выделения памяти на
шаге, с кэшем fasl, и идёт шаг в шаг с ручными ядрами.

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

`make test` заканчивается строкой `Fail: 0` и кодом возврата 0. `make test-full`
добавляет физические тесты тела и среды и прогоны сгенерированной модели шаг в
шаг с ручной — около девяти минут. `make bench` замеряет шаг и пишет результат
в `bench/`.

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

## Модель на языке модели

```lisp
(asdf:load-system "worm/runtime")
(defvar *m* (worm.codegen:compile-model 'worm.components:worm-3d :params "agar-a-v1"))
(defvar *it* (worm.runtime:make-model-integrator *m* :h 1d-3))
(worm.solver:run-steps *it* 1000)                  ; секунда модели, около 5 с
(worm.codegen:show-kernel *m* 'worm.components::pressure)
(worm.runtime:benchmark-step *m* :h 1d-3)
```

Первая сборка — около 3 с, повторная из `cache/codegen/` — доли секунды. В воде
шаг пока 1·10⁻⁴ (архитектура, «Открытые вопросы»).

## Проверка из браузера хоста

В настройках проброса портов гипервизора: порт хоста **8740** → порт гостя
**8740**. Затем в Firefox на хосте откройте <http://localhost:8740/>.

Страница «Wormlisp на связи» показывает версию SBCL, ревизию, память и ядра
процесса и проверяет, что в браузере есть то, на что опирается просмотрщик:
WebGL2, WebSocket, DataView с 64-битными целыми и Blob.slice.

## Просмотр прогонов

Просмотрщик — `viewer/worm-viewer.html`, один файл без сборки; Three.js r128
лежит рядом в `viewer/lib/`. До сервера живого просмотра (этап 9) прогоны
смотрят с диска:

1. Записать прогоны ручной модели (кинематическое натяжение и мышцы, в воде
   и на агаре, около трёх минут):

   ```sh
   sbcl --non-interactive --no-userinit --load scripts/init.lisp \
        --load scripts/record-oracle-runs.lisp
   ```

   Появятся `runs/oracle-water/`, `runs/oracle-agar/`,
   `runs/oracle-water-muscles/` и `runs/oracle-agar-muscles/`.
2. Скопировать на хост через WinSCP каталог `viewer/` целиком и нужные
   каталоги из `runs/`.
3. Открыть `worm-viewer.html` в Firefox, нажать «Открыть frames.bin и
   meta.json…» и выбрать оба файла каталога прогона.

Пробел — пуск и пауза, стрелки — кадр назад и вперёд. Самопроверка
просмотрщика: открыть `worm-viewer.html?selftest`; в заголовке окна
появится `WORM SELFTEST PASS` или `FAIL`, на странице — таблица проверок.

## Раскладка

```text
worm.asd            все системы; слои — архитектура, «Модули и зависимости»
Makefile            test, test-full, bench, image, repl, clean
src/<система>/      worm/platform, worm/core, … worm/analysis
src/package.lisp    фасадный пакет WORM и пакет WORM-USER
tests/              FiveAM; tests/structure.lisp сверяет слои с архитектурой
scripts/            init.lisp, repl.sh, run-tests.lisp, build-image.lisp, make-muscles.lisp,
                    make-selftest.lisp, record-oracle-runs.lisp, bench.lisp
viewer/             worm-viewer.html, lib/ (Three.js r128), selftest-data.js
vendor/             библиотеки с закреплёнными коммитами
tools/firefox/      Firefox 115 для Linux частями; tools/firefox/install.sh
docs/               документация проекта (главная копия)
params/             наборы параметров: base-v1, water-v1, agar-a-v1, agar-c-v1
anatomy/            muscles.sexp — 95 мышц (порождается scripts/make-muscles.lisp)
bench/              замеры шага (make bench)
configs/ sweeps/    заполняются на своих этапах
```

## Документы проекта

В [docs/](docs/README.md): [дизайн модели](docs/design.md),
[архитектура](docs/architecture.md), [план этапов](docs/plan.md) и
[параметры модели](docs/parameters.md). Работа идёт по плану этапов.
