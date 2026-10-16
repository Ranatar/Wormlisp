# vendor/

Сторонние библиотеки Common Lisp, нужные проекту, лежат здесь в исходниках и
закреплены на конкретных коммитах. Quicklisp не используется: `git pull` даёт
ровно тот же набор библиотек, что и у разработчика, а загрузчик
`scripts/init.lisp` настраивает ASDF так, чтобы он искал системы только в
`vendor/` и в корне репозитория.

Библиотеки не правятся. Обновление — заменой каталога целиком и правкой
таблицы ниже.

| Библиотека | Источник | Коммит | Дата коммита | Лицензия |
| --- | --- | --- | --- | --- |
| alexandria | пакет Ubuntu 24.04 `cl-alexandria` 20211025.gita67c3a6-1 (апстрим на gitlab.common-lisp.net) | `a67c3a6` | 2021-10-25 | public domain / MIT-подобная, см. `COPYRIGHT.debian` |
| asdf-flv | https://github.com/didierverna/asdf-flv | `3f1de416f7f4` | 2023-09-12 | GNU All Permissive |
| babel | https://github.com/cl-babel/babel | `4eaf3f228be4` | 2025-09-05 | MIT |
| bordeaux-threads | https://github.com/sionescu/bordeaux-threads | `92da6b9d4f2d` | 2026-03-24 | MIT |
| chunga | https://github.com/edicl/chunga | `1310e96cc3c0` | 2025-08-14 | BSD-2-Clause |
| cl-base64 | https://github.com/darabi/cl-base64 (зеркало git.kpe.io) | `80496b74293e` | 2018-01-29 | BSD-подобная |
| cl-fad | https://github.com/edicl/cl-fad | `714257f064cb` | 2026-08-13 | BSD-2-Clause |
| cl-ppcre | https://github.com/edicl/cl-ppcre | `a2ea581c23fd` | 2025-06-06 | BSD-2-Clause |
| fiveam | https://github.com/lispci/fiveam | `e43d6c8e7da5` | 2024-09-27 | BSD-подобная |
| flexi-streams | https://github.com/edicl/flexi-streams | `4951d575b8f7` | 2024-04-27 | BSD-2-Clause |
| global-vars | https://github.com/lmj/global-vars | `c749f32c9b60` | 2014-10-12 | MIT |
| hunchentoot | https://github.com/edicl/hunchentoot | `65a3a3cca7e6` | 2026-05-04 | BSD-2-Clause |
| md5 | https://github.com/pmai/md5 | `906593fa6f4c` | 2021-06-22 | public domain |
| rfc2388 | https://github.com/jdz/rfc2388 | `591bcf7e77f2` | 2018-08-30 | BSD-2-Clause |
| split-sequence | https://github.com/sharplispers/split-sequence | `89a10b4d697f` | 2021-12-05 | MIT |
| trivial-backtrace | https://github.com/hraban/trivial-backtrace | `7f90b4a41447` | 2023-01-11 | MIT |
| trivial-features | https://github.com/trivial-features/trivial-features | `828246a1c1ef` | 2026-09-08 | MIT |
| trivial-garbage | https://github.com/trivial-garbage/trivial-garbage | `f0663f7b0c90` | 2026-08-29 | public domain |
| trivial-gray-streams | https://github.com/trivial-gray-streams/trivial-gray-streams | `257d73ec3695` | 2026-08-17 | MIT |
| usocket | https://github.com/usocket/usocket | `d492f746d33f` | 2026-05-05 | MIT |
| closer-mop | пакет Ubuntu 24.04 `cl-closer-mop` 2:20220227.giteabdf6a-1 (апстрим github.com/pcostanza/closer-mop недоступен из песочницы) | `eabdf6a` | 2022-02-27 | MIT-подобная, см. `COPYRIGHT.debian` |
| com.inuoe.jzon | https://github.com/Zulu-Inuoe/jzon (без каталогов тестов) | `18959fbef1af` | 2026-03-03 | MIT |
| documentation-utils | https://github.com/Shinmera/documentation-utils | `fcbd927dee7f` | 2025-08-13 | zlib |
| float-features | https://github.com/Shinmera/float-features (без системы тестов) | `cd5dab00d2f4` | 2025-08-13 | zlib |
| trivial-indent | https://github.com/Shinmera/trivial-indent | `87b35ff9202b` | 2025-08-13 | zlib |

Hunchentoot собирается без поддержки TLS (признак `:hunchentoot-no-ssl`
выставляет `scripts/init.lisp`): сервер слушает только сеть «виртуалка — хост»,
а `cl+ssl` потянул бы `cffi` и `libssl`.

`com.inuoe.jzon` с зависимостями (closer-mop, float-features,
documentation-utils, trivial-indent) добавлен на этапе 4 для meta.json.
`hunchensocket` добавится тем же способом на этапе 9.
