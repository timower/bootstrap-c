CC ?= clang
CFLAGS ?= -g -Wall -fsanitize=address
LDFLAGS ?= -fsanitize=address

LLCFLAGS = --relocation-model=pic -filetype=obj

export ASAN_OPTIONS=detect_leaks=0

PARENT_COMMMIT = $(shell git rev-parse --short HEAD^)

# Stores build artifacts (.ll, .o). Binaries are stored in the root.
BUILD_DIR ?= $(CURDIR)/build

# Stores bootstrap stages from parent commits, cached to not rebuild them
CACHE_DIR ?= $(CURDIR)/cache

PARENT_STAGE = $(CACHE_DIR)/stage-$(PARENT_COMMMIT)

# Sources of the compiler
# TODO: when bootstrap can emit dep files, we can just list the main src here.
SRC = bootstrap.b
TEST_SRC = test.b

# We call the bootstrap compiler on the first source file.
MAIN_SRC = $(firstword $(SRC))

LL = $(BUILD_DIR)/$(MAIN_SRC:.b=.ll)
OBJ = $(BUILD_DIR)/$(MAIN_SRC:.b=.o)

bootstrap: $(OBJ)
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

%.o: %.ll
	llc $(LLCFLAGS) $< -o $@

$(LL): $(PARENT_STAGE) $(SRC)
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

test: bootstrap
	./bootstrap $(TEST_SRC)

$(BUILD_DIR)/stage1.ll: bootstrap
	./bootstrap $(MAIN_SRC) > $@

$(BUILD_DIR)/stage2.ll: stage1
	./stage1 $(MAIN_SRC) > $@

stage%: $(BUILD_DIR)/stage%.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

.PHONY: distclean clean self test
clean:
	rm -f build/* bootstrap stage*

distclean: clean
	rm -f cache/*

