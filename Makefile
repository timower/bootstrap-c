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
CACHE_SRC_DIR = $(CACHE_DIR)/src

PARENT_STAGE ?= $(CACHE_DIR)/stage-$(PARENT_COMMMIT)

# Sources of the compiler
# TODO: when bootstrap can emit dep files, we can just list the main src here.
ALL_SRC = $(shell find src/ -type f -name '*.b')
LSP_SRC = $(shell find bootstrap-lsp/ -type f -name '*.go')

# We call the bootstrap compiler on the first source file.
MAIN_SRC = src/bootstrap.b
OBJ = $(BUILD_DIR)/bootstrap.o

.PHONY: all
all: bootstrap ## Build the main bootstrap compiler

bootstrap: $(OBJ) ## Build the bootstrap compiler
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

.PHONY: self
self: bootstrap ## Compile the compiler with itself (verification)
	./bootstrap $(BOOTSTRAP_FLAGS) $(MAIN_SRC)


bootstrap-coverage: bootstrap ## Build bootstrap with coverage instrumentation
	./bootstrap $(BOOTSTRAP_FLAGS) $(MAIN_SRC) | \
		sed 's/declare void @exit(i32 %arg0)/declare void @exit(i32 %arg0) noreturn/' | \
	  opt -S -p 'function-attrs,function(simplifycfg,instcombine<no-verify-fixpoint>,simplifycfg)' -o $(BUILD_DIR)/coverage.ll
	opt -S $(BUILD_DIR)/coverage.ll -p pgo-instr-gen,instrprof | \
	  clang -Xclang -disable-llvm-passes -x ir - -o $@ -fprofile-instr-generate

.PHONY: test
test: format-check lit-coverage lit-stage2 tree-sitter-check lit-mutate ## Run all tests (format check + lit tests)

.PHONY: lit
lit: bootstrap ## Run LLVM lit tests with current bootstrap compiler
	rm -rf test/**/Output
	lit -v test/

.PHONY: lit-stage%
lit-stage%: stage%
	rm -rf test/**/Output
	lit -DBOOTSTRAP=$< -v test/

.PHONY: lit-mutate
lit-mutate: bootstrap-coverage ## Run lit tests with mutated compiler
	rm -rf test/**/Output
	python3 ./test/mutation_test.py $(BUILD_DIR)/coverage.ll

.PHONY: lit-coverage
lit-coverage: bootstrap-coverage ## Run tests with coverage analysis
	rm -f $(BUILD_DIR)/coverage/*
	lit -DBOOTSTRAP=$< -v test/
	llvm-profdata merge -o $(BUILD_DIR)/coverage/merged.profdata $(BUILD_DIR)/coverage
	opt -p pgo-instr-use -o /dev/null $(BUILD_DIR)/coverage.ll \
		-pgo-test-profile-file=$(BUILD_DIR)/coverage/merged.profdata \
		-pgo-view-raw-counts=text 2> $(BUILD_DIR)/coverage/coverage.txt
	python3 ./test/parse_coverage.py $(BUILD_DIR)/coverage/coverage.txt $(BUILD_DIR)/coverage.ll

.PHONY: format-all
format-all: bootstrap ## Format all .b source files in the project
	@echo "$(ALL_SRC)" | tr ' ' '\n' | xargs -P0 -I{} ./bootstrap -format -i {}

.PHONY: format-check
format-check: bootstrap ## Check if all source files are properly formatted
	@echo "$(ALL_SRC)" | tr ' ' '\n' | xargs -P0 -I{} sh -c \
		'if ! ./bootstrap -format {} | diff -q {} - > /dev/null 2>&1; then \
			echo "File {} is not properly formatted"; exit 1; \
		fi'
	@echo "All files are properly formatted"


.PHONY: lsp
lsp: bootstrap-lsp/bootstrap-lsp ## Build the LSP server
bootstrap-lsp/bootstrap-lsp: $(LSP_SRC)
	cd bootstrap-lsp && go build

.PHONY: tree-sitter tree-sitter-check
tree-sitter: tree-sitter-bootstrap/bootstrap.so ## Regenerate the tree-sitter grammar
tree-sitter-bootstrap/bootstrap.so: tree-sitter-bootstrap/grammar.js
	cd tree-sitter-bootstrap && tree-sitter generate && make

tree-sitter-check: tree-sitter ## Verify that all tests and sources parse
	rm -rf test/**/Output
	@find test/ -name '*.b' -not -path 'test/parsing/fail/*' -not -path 'test/fuzz/*' | \
		xargs -I{} sh -c \
		'export HOME=$PWD/build; cd tree-sitter-bootstrap; \
		 if ! tree-sitter parse ../{} > /dev/null 2>&1; then \
			echo "File {} failed to parse"; exit 1; \
		fi'
	@echo "$(ALL_SRC)" | tr ' ' '\n' | xargs -I{} sh -c \
		'export HOME=$PWD/build; cd tree-sitter-bootstrap; \
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
install: bootstrap ## Install into $prefix
	mkdir -p $(prefix)/bin $(prefix)/lib/bootstrap/
	install -m 0755 bootstrap $(prefix)/bin/ || install -m 0755 bootstrap.exe $(prefix)/bin/
	cp -r ./stdlib $(prefix)/lib/bootstrap/

.PHONY: installcheck
installcheck: ## Verifies the install is correct by trying to import stdlib.
	cd $(prefix) && echo 'import stdlib.libc;' | ./bin/bootstrap - -o /dev/null

.PHONY: clean
clean: ## Remove build artifacts and binaries
	rm -rf build/* bootstrap bootstrap-coverage stage*

distclean: clean ## Remove build artifacts and cached stages
	rm -f cache/*

$(BUILD_DIR)/stage1.ll: bootstrap
	./bootstrap $(BOOTSTRAP_FLAGS) $(MAIN_SRC) -o $@

$(BUILD_DIR)/stage2.ll: stage1
	./stage1 $(BOOTSTRAP_FLAGS) $(MAIN_SRC) -o $@

stage%: $(BUILD_DIR)/stage%.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LOADLIBES) $(LDLIBS)

$(BUILD_DIR)/%.ll: src/%.b $(ALL_SRC) bootstrap
	./bootstrap $(BOOTSTRAP_FLAGS) $< -o $@

%.o: %.ll
	llc $(OPTLEVEL) $(LLCBASEFLAGS) $(LLCFLAGS) $< -o $@

$(BUILD_DIR)/bootstrap.ll: $(PARENT_STAGE) $(ALL_SRC)
	$(PARENT_STAGE) $(BOOTSTRAP_FLAGS) $(MAIN_SRC) -o $@

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
