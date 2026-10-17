# Трек живого червя: N2 на агаре

`track.wcon` — отрывок трека одной особи в формате WCON, для сверки метрик
походки модели с живым червём (план, этап 14).

- **Запись:** лаборатория Уильяма Шафера (MRC LMB, Кембридж), Laura Grundy,
  15.09.2009; N2 (Bristol), взрослый гермафродит, агар NGM с низким
  содержанием пептона и едой (OP50), чашка 35 мм, червь перенесён за 30 мин
  до записи. Трекер — Tierpsy; средняя линия из 49 точек, около 25 кадров/с.
- **Источник:** Open Worm Movement Database — Javer A. et al. (2018) An
  open-source platform for analyzing and sharing worm-behavior data. *Nature
  Methods* 15:645–646; записи базы на Zenodo, лицензия
  [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Полный трек
  (`N2 on food L_2009_09_15__11_01_01___8___1.wcon`, 30 мин, 110 МБ) взят из
  примеров github.com/openworm/WCONViewer (`examples/…wcon.zip`); его MD5 —
  в поле `@Wormlisp.sourceMd5`.
- **Что изменено:** взяты 7 отрезков без пропусков длиннее 6 кадров, не
  короче 30 с, по порядку времени — всего 310 с (их границы в
  `@Wormlisp.bouts`); каждый второй кадр; координаты переведены в мм и
  округлены до 1 мкм. Пропуски сегментации внутри отрезков оставлены `null`.
  Метаданные записи сохранены как в источнике.
- **Как получен:** `scripts/make-reference-track.lisp` (путь к полному треку —
  в `WORM_TRACK_SOURCE`).

Метрики трека — `(worm.analysis:track-gait (worm.analysis:read-track
"reference/schafer-n2-agar/track.wcon"))`; закреплены тестом
`reference-track-gait` (`tests/track.lisp`).
