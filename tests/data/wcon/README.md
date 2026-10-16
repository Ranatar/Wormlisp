# Схема и примеры WCON

Из репозитория [openworm/tracker-commons](https://github.com/openworm/tracker-commons),
коммит e5a12d3 (лицензия MIT, `tracker-commons-LICENSE.md`):

- `wcon_schema.json` — JSON-схема формата WCON;
- `minimal.wcon`, `maximal_0.wcon`, `multiworm.wcon` — примеры файлов из `tests/`.

Тест `tests/wcon.lisp` проверяет по схеме эти примеры и экспорт прогона.
