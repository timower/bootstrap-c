CC, ?= clang
CFLAGS ?= -g -Wall -fsanitize=address
LDFLAGS ?= -fsanitize=address

LLCFLAGS ?= -O0 --relocation-model=pic -filetype=obj

# Auto-detect platform and set appropriate target
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	BOOTSTRAP_FLAGS ?= -target darwin
	TRIPLE = arm64-apple-macosx15.0.0
else
	BOOTSTRAP_FLAGS ?=
	TRIPLE = x86_64-unknown-linux-gnu
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

all: bootstrap

bootstrap: $(OBJ)
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

bootstrap-coverage: bootstrap
	./bootstrap $(BOOTSTRAP_FLAGS) $(MAIN_SRC) | \
	opt --mtriple $(TRIPLE) -S -p simplifycfg -o $(BUILD_DIR)/coverage.ll
	opt -S $(BUILD_DIR)/coverage.ll -p pgo-instr-gen,instrprof | \
	clang -Xclang -disable-llvm-passes -x ir - -o $@ -fprofile-instr-generate

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
	rm -rf test/**/Output
	lit -v test/

lit-stage%: stage%
	rm -rf test/**/Output
	env BOOTSTRAP=$< lit -v test/

lit-coverage: bootstrap-coverage
	rm -f $(BUILD_DIR)/coverage/*
	env BOOTSTRAP=$< lit -v test/
	llvm-profdata merge -o $(BUILD_DIR)/coverage/merged.profdata $(BUILD_DIR)/coverage
	opt --mtriple $(TRIPLE) -p pgo-instr-use -o /dev/null $(BUILD_DIR)/coverage.ll \
		-pgo-test-profile-file=$(BUILD_DIR)/coverage/merged.profdata -pgo-view-raw-counts=text 2> $(BUILD_DIR)/coverage/coverage.txt
	python3 ./test/parse_coverage.py $(BUILD_DIR)/coverage/coverage.txt

$(BUILD_DIR)/stage1.ll: bootstrap
	./bootstrap $(BOOTSTRAP_FLAGS) $(MAIN_SRC) -o $@

$(BUILD_DIR)/stage2.ll: stage1
	./stage1 $(BOOTSTRAP_FLAGS) $(MAIN_SRC) -o $@

stage%: $(BUILD_DIR)/stage%.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

format-all: bootstrap
	@for source in $(ALL_SRC); do \
		./bootstrap -format -i $$source ; \
	done

format-check: bootstrap
	@for source in $(ALL_SRC); do \
		if ! ./bootstrap -format $$source | diff -q $$source - > /dev/null 2>&1; then \
			echo "File $$source is not properly formatted"; \
			exit 1; \
		fi; \
	done
	@echo "All files are properly formatted"

.PHONY: distclean clean self test lit lit-stage% lit-coverage format-all format-check all
clean:
	rm -rf build/* bootstrap bootstrap-coverage stage*

distclean: clean
	rm -f cache/*

