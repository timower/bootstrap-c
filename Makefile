CC ?= clang
CFLAGS ?= -g -Wall -fsanitize=address
LDFLAGS ?= -fsanitize=address

export ASAN_OPTIONS=detect_leaks=0

bootstrap: bootstrap.o

test: bootstrap
	./bootstrap test.c

self: bootstrap
	./bootstrap bootstrap.c

.PHONY: clean self test
clean:
	rm -f *.o bootstrap
