# Исходная модель Бойла: прогоны WormSim

`medium-<m>.wcon` — средняя линия исходной модели Boyle, Berri, Cohen (2012)
в средах MEDIUM = 0 (вода), 0,001, 0,01, 0,1 и 1 (агар), 15 с, 25 кадров/с,
49 точек; эталон для сверки второго уровня (план, этап 15).

- **Модель:** Boyle J.H., Berri S., Cohen N. (2012) Gait modulation in
  *C. elegans*: an integrated neuromechanical model. *Front. Comput.
  Neurosci.* 6:10. Код WormSim — приложение к статье, репозиторий
  github.com/openworm/CelegansNeuromechanicalGaitModulation, коммит
  `c2455768a7c01b9c30b7fd3329121fbe3cec748f`.
- **Среда:** MEDIUM линейно ведёт сопротивление от воды к агару:
  C_L = 3,3·10⁻³ + (3,2 − 3,3·10⁻³)·m, C_N = 5,2·10⁻³ + (128 − 5,2·10⁻³)·m,
  в Па·с (мкН·с/мм²) на единицу длины.
- **Что изменено в коде:** константы `DURATION` и `MEDIUM` в `worm.cc`
  читаются из переменных окружения; больше ничего. Прогоны детерминированы:
  повторный запуск даёт тот же `simdata.csv` побайтно.
- **Как получены:** `scripts/boyle-wormsim.sh <каталог> 15` (клонирует,
  собирает Sundials 2.3 и программу, считает среды), затем
  `WORM_BOYLE_RUNS=<каталог>` и `scripts/make-boyle-reference.lisp` (метры →
  мм, точность 0,1 мкм).

Метрики — `(worm.analysis:track-gait (worm.analysis:read-track
"reference/boyle2012-wormsim/medium-1.wcon") :from 5 :window 5 :min-speed 0)`;
закреплены тестом `boyle-reference-gait` (`tests/track.lisp`).
