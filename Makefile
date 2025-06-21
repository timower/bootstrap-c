CC, ?= clang
CFLAGS ?= -g -Wall -fsanitize=address
LDFLAGS ?= -fsanitize=address

LLCFLAGS ?= -O0 --relocation-model=pic -filetype=obj

# Auto-detect platform and set appropriate target
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	BOOTSTRAP_FLAGS ?= -target darwin
else
	BOOTSTRAP_FLAGS ?=
endif

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

all: bootstrap bootstrap-sema format

bootstrap: $(OBJ)
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

bootstrap-sema: $(BUILD_DIR)/bootstrap-sema.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

format: $(BUILD_DIR)/format.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

$(BUILD_DIR)/%.ll: src/%.b $(ALL_SRC) bootstrap
	./bootstrap $(BOOTSTRAP_FLAGS) $< -o $@

%.o: %.ll
	llc $(LLCFLAGS) $< -o $@

$(BUILD_DIR)/bootstrap.ll: $(PARENT_STAGE) $(ALL_SRC)
	$(PARENT_STAGE) $(BOOTSTRAP_FLAGS) $(MAIN_SRC) -o $@

$(PARENT_STAGE):
	$(eval TMP := $(shell mktemp -d))
	git clone . $(TMP)
	git -C $(TMP) reset --hard $(PARENT_COMMMIT)
	cd $(TMP) && $(MAKE) CACHE_DIR=$(CACHE_DIR) stage2
	mv $(TMP)/stage2 $@
	rm -rf $(TMP)

self: bootstrap
	./bootstrap $(BOOTSTRAP_FLAGS) $(MAIN_SRC)

test: format-check lit lit-stage2

lit: bootstrap
	lit -v test/

lit-stage%: stage%
	env BOOTSTRAP=$< lit -v test/

$(BUILD_DIR)/stage1.ll: bootstrap
	./bootstrap $(BOOTSTRAP_FLAGS) $(MAIN_SRC) -o $@

$(BUILD_DIR)/stage2.ll: stage1
	./stage1 $(BOOTSTRAP_FLAGS) $(MAIN_SRC) -o $@

stage%: $(BUILD_DIR)/stage%.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

format-all: format
	@for source in $(ALL_SRC); do \
		./format $$source > /tmp/file.b ; cp /tmp/file.b $$source ; \
	done

format-check: format
	@for source in $(ALL_SRC); do \
		if ! ./format $$source | diff -q $$source - > /dev/null 2>&1; then \
			echo "File $$source is not properly formatted"; \
			exit 1; \
		fi; \
	done
	@echo "All files are properly formatted"

.PHONY: distclean clean self test lit lit-stage% format-all format-check all
clean:
	rm -f build/* bootstrap bootstrap-sema format stage*

distclean: clean
	rm -f cache/*

