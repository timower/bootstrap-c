CC ?= clang
CFLAGS ?= -g

bootstrap: bootstrap.o

test: bootstrap
	./bootstrap test.c

self: bootstrap
	./bootstrap bootstrap.c

.PHONY: clean
clean:
	rm -f *.o bootstrap
