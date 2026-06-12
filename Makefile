CC ?= clang
CFLAGS ?= -g -Wall
LDFLAGS ?=

OPTLEVEL ?= -O0
LLCBASEFLAGS ?= --frame-pointer=all --relocation-model=pic -filetype=obj
LLCFLAGS ?=

prefix ?= /usr/local

# Auto-detect platform and set appropriate target
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	BRIO_FLAGS ?= -target darwin
	TRIPLE = arm64-apple-macosx15.0.0
else
	BRIO_FLAGS ?=
	TRIPLE = x86_64-unknown-linux-gnu
endif

export ASAN_OPTIONS=detect_leaks=0

PARENT_COMMMIT = $(shell git rev-parse --short HEAD^)

# Stores build artifacts (.ll, .o). Binaries are stored in the root.
BUILD_DIR ?= $(CURDIR)/build

# Stores brio stages from parent commits, cached to not rebuild them
CACHE_DIR ?= $(CURDIR)/cache
CACHE_SRC_DIR = $(CACHE_DIR)/src

PARENT_STAGE ?= $(CACHE_DIR)/stage-$(PARENT_COMMMIT)

# Sources of the compiler
# TODO: when brio can emit dep files, we can just list the main src here.
ALL_SRC = $(shell find src/ -type f -name '*.b')
LSP_SRC = $(shell find brio-lsp/ -type f -name '*.go')

# We call the brio compiler on the first source file.
MAIN_SRC = src/brio.b
OBJ = $(BUILD_DIR)/brio.o

.PHONY: all
all: brio ## Build the main brio compiler

brio: $(OBJ) ## Build the brio compiler
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

.PHONY: self
self: brio ## Compile the compiler with itself (verification)
	./brio $(BRIO_FLAGS) $(MAIN_SRC)


brio-coverage: brio ## Build brio with coverage instrumentation
	./brio $(BRIO_FLAGS) $(MAIN_SRC) | \
		sed 's/declare void @exit(i32 %arg0)/declare void @exit(i32 %arg0) noreturn/' | \
	  opt -S -p 'function-attrs,function(simplifycfg,instcombine<no-verify-fixpoint>,simplifycfg)' -o $(BUILD_DIR)/coverage.ll
	opt -S $(BUILD_DIR)/coverage.ll -p pgo-instr-gen,instrprof | \
	  clang -Xclang -disable-llvm-passes -x ir - -o $@ -fprofile-instr-generate

.PHONY: test
test: format-check lit-coverage lit-stage2 tree-sitter-check lit-mutate ## Run all tests (format check + lit tests)

.PHONY: lit
lit: brio ## Run LLVM lit tests with current brio compiler
	rm -rf test/**/Output
	lit -v test/

.PHONY: lit-stage%
lit-stage%: stage%
	rm -rf test/**/Output
	lit -DBRIO=$< -v test/

.PHONY: lit-mutate
lit-mutate: brio-coverage ## Run lit tests with mutated compiler
	rm -rf test/**/Output
	python3 ./test/mutation_test.py $(BUILD_DIR)/coverage.ll

.PHONY: lit-coverage
lit-coverage: brio-coverage ## Run tests with coverage analysis
	rm -f $(BUILD_DIR)/coverage/*
	lit -DBRIO=$< -v test/
	llvm-profdata merge -o $(BUILD_DIR)/coverage/merged.profdata $(BUILD_DIR)/coverage
	opt -p pgo-instr-use -o /dev/null $(BUILD_DIR)/coverage.ll \
		-pgo-test-profile-file=$(BUILD_DIR)/coverage/merged.profdata \
		-pgo-view-raw-counts=text 2> $(BUILD_DIR)/coverage/coverage.txt
	python3 ./test/parse_coverage.py $(BUILD_DIR)/coverage/coverage.txt $(BUILD_DIR)/coverage.ll

.PHONY: format-all
format-all: brio ## Format all .b source files in the project
	@echo "$(ALL_SRC)" | tr ' ' '\n' | xargs -P0 -I{} ./brio -format -i {}

.PHONY: format-check
format-check: brio ## Check if all source files are properly formatted
	@echo "$(ALL_SRC)" | tr ' ' '\n' | xargs -P0 -I{} sh -c \
		'if ! ./brio -format {} | diff -q {} - > /dev/null 2>&1; then \
			echo "File {} is not properly formatted"; exit 1; \
		fi'
	@echo "All files are properly formatted"


.PHONY: lsp
lsp: brio-lsp/brio-lsp ## Build the LSP server
brio-lsp/brio-lsp: $(LSP_SRC)
	cd brio-lsp && go build

.PHONY: tree-sitter tree-sitter-check
tree-sitter: tree-sitter-brio/brio.so ## Regenerate the tree-sitter grammar
tree-sitter-brio/brio.so: tree-sitter-brio/grammar.js
	cd tree-sitter-brio && tree-sitter generate && make

tree-sitter-check: tree-sitter ## Verify that all tests and sources parse
	rm -rf test/**/Output
	@find test/ -name '*.b' -not -path 'test/parsing/fail/*' -not -path 'test/fuzz/*' | \
		xargs -I{} sh -c \
		'export HOME=$PWD/build; cd tree-sitter-brio; \
		 if ! tree-sitter parse ../{} > /dev/null 2>&1; then \
			echo "File {} failed to parse"; exit 1; \
		fi'
	@echo "$(ALL_SRC)" | tr ' ' '\n' | xargs -I{} sh -c \
		'export HOME=$PWD/build; cd tree-sitter-brio; \
		 if ! tree-sitter parse ../{} > /dev/null 2>&1; then \
			echo "File {} failed to parse"; exit 1; \
		fi'
	@echo "All files parse with tree-sitter"

$(BUILD_DIR)/fuzz-parser: $(BUILD_DIR)/fuzz.ll
	clang -g -O1 -fno-omit-frame-pointer -fsanitize=fuzzer,address $< -o $@

fuzz: $(BUILD_DIR)/fuzz-parser
	mkdir -p corpus/
	find test src -name '*.b' -exec cp {} corpus/ \;
	env ASAN_OPTIONS=detect_leaks=1 $(BUILD_DIR)/fuzz-parser -fork=6 -close_fd_mask=2 corpus/

.PHONY: install
install: brio ## Install into $prefix
	mkdir -p $(prefix)/bin $(prefix)/lib/brio/
	install -m 0755 brio $(prefix)/bin/ || install -m 0755 brio.exe $(prefix)/bin/
	cp -r ./stdlib $(prefix)/lib/brio/

.PHONY: installcheck
installcheck: ## Verifies the install is correct by trying to import stdlib.
	cd $(prefix) && echo 'import stdlib.libc;' | ./bin/brio - -o /dev/null

.PHONY: clean
clean: ## Remove build artifacts and binaries
	rm -rf build/* brio brio-coverage stage*

distclean: clean ## Remove build artifacts and cached stages
	rm -f cache/*

$(BUILD_DIR)/stage1.ll: brio
	./brio $(BRIO_FLAGS) $(MAIN_SRC) -o $@

$(BUILD_DIR)/stage2.ll: stage1
	./stage1 $(BRIO_FLAGS) $(MAIN_SRC) -o $@

stage%: $(BUILD_DIR)/stage%.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

$(BUILD_DIR)/%.ll: src/%.b $(ALL_SRC) brio
	./brio $(BRIO_FLAGS) $< -o $@

%.o: %.ll
	llc $(OPTLEVEL) $(LLCBASEFLAGS) $(LLCFLAGS) $< -o $@

$(BUILD_DIR)/brio.ll: $(PARENT_STAGE) $(ALL_SRC)
	$(PARENT_STAGE) -stdlib . $(BRIO_FLAGS) $(MAIN_SRC) -o $@

$(CACHE_DIR)/stage-%:
	$(eval COMMIT_HASH := $(patsubst $(CACHE_DIR)/stage-%,%,$@))
	$(eval PARENT_COMMIT := $(shell git rev-parse --short $(COMMIT_HASH)^ || echo ""))
	$(eval PARENT_STAGE_DEP := $(CACHE_DIR)/stage-$(PARENT_COMMIT))
	@if [ ! -f $(PARENT_STAGE_DEP) ] && [ ! -z $(PARENT_COMMIT) ]; \
	then $(MAKE) CACHE_DIR=$(CACHE_DIR) $(PARENT_STAGE_DEP); fi
	@trap 'git worktree remove $(CACHE_SRC_DIR) 2>/dev/null || true' EXIT; \
	git worktree add $(CACHE_SRC_DIR) $(COMMIT_HASH); \
	cd $(CACHE_SRC_DIR) && $(MAKE) CACHE_DIR=$(CACHE_DIR) PARENT_STAGE=$(PARENT_STAGE_DEP) stage2; \
	mv $(CACHE_SRC_DIR)/stage2 $@; \
	git worktree remove $(CACHE_SRC_DIR)

.PHONY: help
help: ## Show this help message
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}'
