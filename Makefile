# Wormlisp — цели сборки и проверки.
#   make test       быстрые тесты (уровни 1-3 и быстрая часть 5)
#   make test-full  все тесты, включая физику тела и среды (около минуты)
#   make bench      замер шага моделей, результаты в bench/
#   make image      образ worm-image.core для REPL и рабочих процессов
#   make repl       REPL в tmux с сервером на порту 8740 (scripts/repl.sh)
#   make clean      удалить образ и журналы

SBCL ?= sbcl
LISP  = $(SBCL) --noinform --non-interactive --no-sysinit --no-userinit --load scripts/init.lisp

.PHONY: test test-full bench image repl clean

test:
	$(LISP) --load scripts/run-tests.lisp

test-full:
	WORM_FULL=1 $(LISP) --load scripts/run-tests.lisp

bench:
	$(LISP) --load scripts/bench.lisp

image:
	$(LISP) --load scripts/build-image.lisp

repl:
	scripts/repl.sh

clean:
	rm -f worm-image.core
	rm -rf log
