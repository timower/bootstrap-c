CC ?= clang
CFLAGS ?= -g -Wall -fsanitize=address
LDFLAGS ?= -fsanitize=address

LLCFLAGS = --relocation-model=pic -filetype=obj

export ASAN_OPTIONS=detect_leaks=0

bootstrap: bootstrap.o

%.o: %.ll
	llc $(LLCFLAGS)  $< -o $@

self: bootstrap
	./bootstrap bootstrap.c

stage0.ll: bootstrap
	./bootstrap bootstrap.c > stage0.ll

stage0: stage0.o

test: stage0
	./stage0 test.c

stage1.ll: stage0
	./stage0 bootstrap.c > stage1.ll

stage1: stage1.o

stage2.ll: stage1
	./stage1 bootstrap.c > stage2.ll

stage2: stage2.o

.PHONY: clean self test
clean:
	rm -f *.o bootstrap stage*
