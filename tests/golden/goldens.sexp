;;;; Эталонные прогоны (архитектура, «Тестирование»; этап 13). Каждый
;;;; эталон — короткий прогон модели на 50 колец с записью кадров; его
;;;; frames.bin сверяется побитово с хэшами в tests/golden/<имя>.sexp.
;;;; Хэши обновляет (worm.tests:update-golden :<имя> :reason "…"), причина
;;;; пишется в tests/golden/journal.txt.

(golden level-1-water :model worm-3d :params "water-v1" :dt 5d-4 :steps 1000 :frames-every 0.01)
(golden level-1-agar  :model worm-3d :params "agar-a-v1" :dt 1d-3 :steps 1000 :frames-every 0.01)
(golden level-2-agar  :model worm-3d-proprio :params "agar-a-v1" :dt 1d-3 :steps 1000 :frames-every 0.01)
(golden level-2-water :model worm-3d-proprio :params "water-v1" :dt 5d-4 :steps 1000 :frames-every 0.01)
(golden level-3-agar  :model worm-3d-connectome :params "agar-a-v1" :dt 1d-3 :steps 1000 :frames-every 0.01)
