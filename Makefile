CC, ?= clang
CFLAGS ?= -g -Wall -fsanitize=address
LDFLAGS ?= -fsanitize=address

LLCFLAGS ?= -O0 --relocation-model=pic -filetype=obj

export ASAN_OPTIONS=detect_leaks=0

PARENT_COMMMIT = $(shell git rev-parse --short HEAD^)

# Stores build artifacts (.ll, .o). Binaries are stored in the root.
BUILD_DIR ?= $(CURDIR)/build

# Stores bootstrap stages from parent commits, cached to not rebuild them
CACHE_DIR ?= $(CURDIR)/cache

PARENT_STAGE ?= $(CACHE_DIR)/stage-$(PARENT_COMMMIT)

# Sources of the compiler
# TODO: when bootstrap can emit dep files, we can just list the main src here.
ALL_SRC = $(shell find src/ -type f -name '*.b')

# We call the bootstrap compiler on the first source file.
MAIN_SRC = src/bootstrap.b
OBJ = $(BUILD_DIR)/bootstrap.o

bootstrap: $(OBJ)
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

format: $(BUILD_DIR)/format.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

$(BUILD_DIR)/%.ll: src/%.b $(ALL_SRC) bootstrap
	./bootstrap $< -o $@

%.o: %.ll
	llc $(LLCFLAGS) $< -o $@

$(BUILD_DIR)/bootstrap.ll: $(PARENT_STAGE) $(ALL_SRC)
	$(PARENT_STAGE) $(MAIN_SRC) > $@

$(PARENT_STAGE):
	$(eval TMP := $(shell mktemp -d))
	git clone . $(TMP)
	git -C $(TMP) reset --hard $(PARENT_COMMMIT)
	cd $(TMP) && $(MAKE) CACHE_DIR=$(CACHE_DIR) stage2
	mv $(TMP)/stage2 $@
	rm -rf $(TMP)

self: bootstrap
	./bootstrap $(MAIN_SRC)

test: lit lit-stage2

lit: bootstrap
	lit -v test/

lit-stage%: stage%
	env BOOTSTRAP=$< lit -v test/

$(BUILD_DIR)/stage1.ll: bootstrap
	./bootstrap $(MAIN_SRC) > $@

$(BUILD_DIR)/stage2.ll: stage1
	./stage1 $(MAIN_SRC) > $@

stage%: $(BUILD_DIR)/stage%.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

format-all: format
	for source in $(ALL_SRC); do \
		./format $$source > /tmp/file.b ; cp /tmp/file.b $$source ; \
	done

.PHONY: distclean clean self test lit lit-stage% format-all
clean:
	rm -f build/* bootstrap stage*

distclean: clean
	rm -f cache/*

