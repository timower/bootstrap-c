# Brio Language Reference

Brio is a self-hosting systems programming language that compiles to LLVM IR. It combines C-like performance and control with modern type safety features, making it suitable for systems programming while providing better safety guarantees than traditional C.

## Table of Contents

- [Type System](#type-system)
- [Variables and Constants](#variables-and-constants)
- [Literals](#literals)
- [Expressions](#expressions)
- [Control Flow](#control-flow)
- [Functions](#functions)
- [Structs](#structs)
- [Unions (Tagged Unions)](#unions-tagged-unions)
- [Enums](#enums)
- [Arrays](#arrays)
- [Pointers](#pointers)
- [Import System](#import-system)
- [Comments](#comments)

## Type System

### Primitive Types

Brio provides a comprehensive set of primitive types:

#### Integer Types
- **Signed integers**: `i8`, `i16`, `i32`, `i64`
- **Unsigned integers**: `u8`, `u16`, `u32`, `u64`

#### Other Primitives
- **Boolean**: `bool` with literals `true` and `false`
- **Void**: `void` for functions that don't return values
- **Character**: Character literals use single quotes: `'c'`, `'\n'`, `'\t'`

#### Compound Types
- **Pointers**: `Type*` (e.g., `i32*`, `void*`)
- **Arrays**: `Type[size]` for fixed-size, `Type[]` for unsized arrays
- **Functions**: `func(param: Type, ...) -> ReturnType`
- **Structs**: User-defined aggregate types
- **Unions**: Tagged unions with pattern matching
- **Enums**: Simple enumeration types

#### Type Modifiers
- **Const**: `const Type` for immutable types
- **Extern**: `extern` for external declarations

## Variables and Constants

### Local Variables

```brio
let x: i32 = 42;        // Explicit type with initialization
let y = 42;             // Type inferred from value
let z: i32;             // Uninitialized variable
```

### Constants

Constants are evaluated at compile-time:

```brio
const MAX_SIZE = 100;
const PI = 3.14;
const MESSAGE = "Hello, Brio!";
```

### Global Variables

```brio
let globalCounter: i32 = 0;
```

### External Declarations

```brio
extern let errno: i32;
extern func malloc(size: u64) -> void*;
```

## Literals

### Integer Literals

```brio
42          // Decimal
0x2A        // Hexadecimal
0o52        // Octal
0b101010    // Binary
```

### Other Literals

```brio
true        // Boolean true
false       // Boolean false
"Hello!"    // String literal
'A'         // Character literal
'\n'        // Escape sequence
```

## Expressions

### Arithmetic Operations

```brio
a + b       // Addition
a - b       // Subtraction
a * b       // Multiplication
a / b       // Division
a % b       // Modulo
```

### Comparison Operations

```brio
a == b      // Equality
a != b      // Inequality
a < b       // Less than
a <= b      // Less than or equal
a > b       // Greater than
a >= b      // Greater than or equal
```

### Logical Operations

```brio
a && b      // Logical AND
a || b      // Logical OR
!a          // Logical NOT
```

### Bitwise Operations

```brio
a & b       // Bitwise AND
a | b       // Bitwise OR
a ^ b       // Bitwise XOR
~a          // Bitwise NOT
a << b      // Left shift
a >> b      // Right shift
```

### Assignment Operations

```brio
a = b       // Basic assignment
a += b      // Add and assign
a -= b      // Subtract and assign
a *= b      // Multiply and assign
a /= b      // Divide and assign
a %= b      // Modulo and assign
a <<= b     // Left shift and assign
a >>= b     // Right shift and assign
a &= b      // Bitwise AND and assign
a |= b      // Bitwise OR and assign
a ^= b      // Bitwise XOR and assign
```

### Unary Operations

```brio
+a          // Unary plus
-a          // Unary minus
&a          // Address of
*a          // Dereference
++a         // Pre-increment
a++         // Post-increment
--a         // Pre-decrement
a--         // Post-decrement
```

### Member Access and Indexing

```brio
obj.field       // Direct member access
ptr->field      // Pointer member access
arr[index]      // Array indexing
```

### Function Calls

```brio
func()              // No arguments
func(a, b, c)       // Multiple arguments
func(a, b, ...)     // Variadic functions
```

### Type Operations

```brio
value as Type       // Explicit type cast
sizeof(Type)        // Size of type
sizeof(expr)        // Size of expression
```

### Conditional Expression

```brio
condition ? true_expr : false_expr
```

### Let Expressions

Let expressions allow variable declaration within expressions:

```brio
if (let ptr = getValue() as SomeType*) {
    // ptr is non-null, can use it here
    use(ptr);
}
```

## Control Flow

### If Statements

```brio
if (condition) {
    // statements
}

if (condition) {
    // statements
} else {
    // statements
}

if (condition) {
    // statements
} else if (other_condition) {
    // statements
} else {
    // statements
}
```

### While Loops

```brio
while (condition) {
    // statements
}
```

### For Loops

```brio
for (init; condition; update) {
    // statements
}

// Example
for (let i = 0; i < 10; i += 1) {
    printf("i = %d\n", i);
}
```

### Switch Statements

```brio
switch (expr) {
    case value1:
        // statements
        break;
    case value2, value3:    // Multiple values
        // statements
        break;
    default:
        // statements
}
```

### Break and Return

```brio
break;          // Exit loop or switch
return;         // Return from void function
return value;   // Return value from function
```

## Functions

### Function Definition

```brio
func functionName(param1: Type1, param2: Type2) -> ReturnType {
    // function body
    return value;
}
```

### Void Functions

```brio
func procedure(param: Type) {
    // no return statement needed
}
```

### Variadic Functions

```brio
func printf(format: i8*, ...) -> i32 {
    // implementation
}
```

### External Functions

```brio
extern func malloc(size: u64) -> void*;
extern func free(ptr: void*);
```

## Structs

### Struct Definition

```brio
struct Point {
    x: i32;
    y: i32;
};
```

### Struct Initialization

```brio
let p = Point {
    x = 10,
    y = 20,
};
```

### Nested Structs

```brio
struct Rectangle {
    topLeft: Point;
    bottomRight: Point;
};

let rect = Rectangle {
    topLeft = Point { x = 0, y = 0 },
    bottomRight = Point { x = 100, y = 50 },
};
```

## Unions (Tagged Unions)

Brio supports tagged unions with pattern matching for type-safe variant types.

### Union Definition

```brio
union Option {
    None {}
    Some {
        value: i32;
    }
}
```

### Union Construction

```brio
let opt = Option::Some { value = 42 };
let empty = Option::None {};
```

### Pattern Matching

```brio
switch (opt) {
    case Option::None:
        printf("No value\n");
    case Option::Some as some:
        printf("Value: %d\n", some.value);
}
```

### Type Checking with Casts

```brio
if (let some = opt as Option::Some*) {
    // some is non-null if opt is Some variant
    printf("Value: %d\n", some->value);
}
```

## Enums

### Enum Definition

```brio
enum Color {
    RED,
    GREEN,
    BLUE,
};
```

### Enum Usage

```brio
let color = Color::RED;

switch (color) {
    case Color::RED:
        printf("Red\n");
    case Color::GREEN:
        printf("Green\n");
    case Color::BLUE:
        printf("Blue\n");
}
```

## Arrays

### Array Types

```brio
let numbers: i32[10];        // Fixed-size array
let dynamicArray: i32[];     // Unsized array parameter
```

### Array Initialization

```brio
let arr = {1, 2, 3, 4, 5};   // Array literal
```

### Array Access

```brio
arr[0] = 10;                 // Set element
let first = arr[0];          // Get element
```

## Pointers

### Pointer Declaration

```brio
let ptr: i32*;               // Pointer to i32
```

### Pointer Operations

```brio
ptr = &variable;             // Address of variable
let value = *ptr;            // Dereference pointer
let element = *(ptr + index); // Pointer arithmetic
```

## Import System

### Basic Import

```brio
import module_name;          // Import module_name.b
```

### Nested Imports

```brio
import dir.subdir.module;    // Import from subdirectory
```

### Platform-Specific Imports

Brio automatically resolves platform-specific implementations using file naming conventions. When you write a platform-agnostic import:

```brio
import libc.impl;            // Resolves to appropriate platform file
```

The compiler automatically looks for platform-specific files based on the compilation target:
- **POSIX target**: `libc/impl.posix.b`
- **Windows target**: `libc/impl.windows.b` 
- **Darwin target**: `libc/impl.darwin.b`

The target is specified via the `-target` command-line flag:
```bash
brio -target posix main.b     # Uses .posix.b files
brio -target windows main.b   # Uses .windows.b files  
brio -target darwin main.b    # Uses .darwin.b files
```

## Comments

```brio
// Single-line comment
```

## Scope Resolution

Access items from parent scopes using the `::` operator:

```brio
ParentType::ChildType
EnumType::VARIANT
UnionType::Tag
```

## Key Language Features

1. **Type Safety**: Strong static typing with type inference where possible
2. **Memory Safety**: Explicit pointer management with clear ownership semantics
3. **Pattern Matching**: Tagged unions with comprehensive switch-case pattern matching
4. **Zero-cost Abstractions**: Compile-time evaluation of constants and expressions
5. **C Interoperability**: External function declarations and familiar syntax
6. **Cross-platform Support**: Target-specific compilation with platform flags
7. **Self-hosting**: The compiler is implemented in Brio itself

## Platform-Specific Compilation

Brio supports cross-platform compilation using target flags:

```bash
# Linux/POSIX (default)
make brio

# macOS/Darwin (auto-detected)
make brio

# Windows
make brio BRIO_FLAGS="-target windows"
```

## Examples

### Hello World

```brio
import libc.io;

func main() -> i32 {
    printf("Hello, Brio!\n");
    return 0;
}
```

### Working with Tagged Unions

```brio
union Result {
    Ok { value: i32; }
    Err { message: i8*; }
};

func divide(a: i32, b: i32) -> Result {
    if (b == 0) {
        return Result::Err { message = "Division by zero" };
    }
    return Result::Ok { value = a / b };
}

func main() -> i32 {
    let result = divide(10, 2);
    switch (result) {
        case Result::Ok as ok:
            printf("Result: %d\n", ok.value);
        case Result::Err as err:
            printf("Error: %s\n", err.message);
    }
    return 0;
}
```

This reference covers the core syntax and semantics of the Brio programming language. The language is designed to be both powerful and safe, providing low-level control while maintaining type safety and modern language features.
