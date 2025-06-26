# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Bootstrap is a self-hosting compiler project where each commit adds a new language feature and compiles itself using the previous commit's compiler. The compiler is written in its own Bootstrap language (`.b` files) and generates LLVM IR.

## Build Commands

### Core Build
- `make bootstrap` - Build the main compiler from source
- `make stage1` - Create stage1 compiler using bootstrap
- `make stage2` - Create stage2 compiler using stage1
- `make self` - Compile the compiler with itself (verification)
- `make` - Compile bootstrap

### Testing
- `make test` - Run all tests using lit (LLVM Integrated Tester)
- `make lit` - Run tests with current bootstrap compiler
- `make lit-coverage` - Run tests with coverage
- `lit -v test/` - Run tests with verbose output

### Code Formatting
- `make format-all` - Format all .b source files in the project
- `./bootstrap -format -i <file.b>` - Format a specific source file in place
- `./bootstrap -format <file.b> -o <file.b>` - Format with explicit output file

### Syntax Checking
- `./bootstrap -sema <file>` - Check syntax of individual files

### Cleanup
- `make clean` - Remove build artifacts and binaries

## Architecture

### Compiler Pipeline
1. **Parsing** (`src/parse.b`, `src/parse/`) - Recursive descent parser with token-based lexing
2. **AST** (`src/ast.b`, `src/ast/`) - Union-based abstract syntax tree with comprehensive type system
3. **Semantic Analysis** (`src/sema.b`, `src/sema/`) - Type checking, symbol resolution, multi-pass analysis
4. **IR Generation** (`src/irgen.b`, `src/irgen/`) - LLVM IR generation from AST
5. **IR Representation** (`src/ir.b`, `src/ir/`) - Internal IR with LLVM printing

### Key Components
- `src/bootstrap.b` - Main compiler entry point with integrated formatter and semantic analyzer
- `src/libc.b` - Platform abstraction layer (POSIX/Windows)
- `src/util.b` - Common utilities and data structures including file I/O

### Language Features
- Strong typing: i8/i16/i32/i64, u8/u16/u32/u64, bool, pointers, arrays
- Tagged unions and type-safe enums
- Struct expressions and initialization
- Constants and compile-time evaluation
- Multi-file imports and modules
- Cross-platform compilation (POSIX/Windows via `-target` flag)

### Memory Management
- The compiler is a short-running process and does not free allocated memory
- Memory cleanup is not necessary as the OS reclaims all memory on process exit
- Functions like `free()` are not imported or used in the codebase

## Testing Framework

Uses LLVM's `lit` testing framework:
- Test files in `test/` directory with `.b` extension
- Tests use `// RUN:` commands with FileCheck for validation
- Common pattern: `// RUN: %bootstrap %s | lli` (compile and execute)
- Platform-specific tests use `// REQUIRES: system-<platform>`

## Development Workflow

### Making Changes
1. Modify source files (`.b` files in `src/`)
2. Run `make bootstrap` to build with previous stage
3. Use `make format-all` to maintain code style
4. Run `make test` to verify correctness

### Adding New Features
- Each commit should add exactly one language feature
- Update corresponding parser, semantic analyzer, and IR generator
- Add comprehensive tests for the new feature
- Ensure backward compatibility with existing code
- Make sure to format all modified code using `format-all`

## Build System Details

### Bootstrap Process
- Uses cached stages in `cache/stage-<commit-hash>` to avoid rebuilding
- Parent commit compiler is automatically built and cached
- LLVM backend generates object files via `llc`
- Clang used for final linking with AddressSanitizer enabled

### Cross-Platform Support
- Nix flake provides reproducible builds
- Support for Linux, Windows (mingw64), Darwin (macOS), and static builds
- Platform-specific code in `src/libc/impl.posix.b`, `src/libc/impl.windows.b`, and `src/libc/impl.darwin.b`

### Platform-Specific Build Instructions
- **macOS/Darwin**: Automatically detected - `make bootstrap` will use `-target darwin`
- **Linux**: Use default `make bootstrap` (uses POSIX target)
- **Windows**: Use `make bootstrap BOOTSTRAP_FLAGS="-target windows"`

Note: The Makefile automatically detects Darwin and sets the appropriate target due to different stdout/stderr symbol names (`__stdoutp`/`__stderrp` vs `stdout`/`stderr`)

### Environment Variables
- `BOOTSTRAP_FLAGS` - Additional compiler flags (use `-target <platform>` for cross-compilation)
- `BOOTSTRAP` - Override bootstrap compiler path for testing
- `LIT_FILTER` - Name of the test to execute in lit and related targets

## Code Snippets
```bootstrap
extern func printf(format: i8*, ...) -> i32;

// Enum
enum Status {
    ACTIVE,
    INACTIVE,
};

// Struct
struct Point {
    x: i32;
    y: i32;
    status: Status;
};

// Tagged union
union Result {
    Ok { value: i32; }
    Err { message: i8*; }
};

// Function with pointers
func processPoint(point: Point*) -> i32 {
    if (point->status == Status::ACTIVE) {
        return point->x + point->y;
    }
    return 0;
}

func main() -> i32 {
    // Array initialization and access
    let numbers = {1, 2, 3, 4, 5};
    printf("Array: [%d, %d, %d, %d, %d]\n", numbers[0], numbers[1], numbers[2], numbers[3], numbers[4]);

    // Array with pointer arithmetic
    let ptr_array = &numbers[0];
    printf("Via pointer: %d, %d\n", *ptr_array, *(ptr_array + 2));

    // Struct initialization
    let point = Point { x = 10, y = 20, status = Status::ACTIVE };

    // Pointer operations
    let ptr = &point;
    let sum = processPoint(ptr);

    // Tagged union construction
    let result: Result = Result::Ok { value = sum };

    // Pattern matching on tagged union
    switch (result) {
        case Result::Ok as ok:
            printf("Sum: %d\n", ok.value);
        case Result::Err as err:
            printf("Error: %s\n", err.message);
    }

    // Conditional expression and member access
    let max = ptr->x > ptr->y ? ptr->x : ptr->y;
    printf("Max coordinate: %d\n", max);

    return 0;
}
```
