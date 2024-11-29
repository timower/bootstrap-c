CFLAGS ?= -g

bootstrap: bootstrap.o

.PHONY: clean
clean:
	rm -f *.o bootstrap
