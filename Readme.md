Bootstrap
=========

A project exploring compiler bootstrapping.
The first commit is a c-subset compiler that can compile itself.
Each commit after adds a new feature, and is compiled by the previous commit.


TODO
----
 - [x] Add correct integer types (i8, i16, i32, i64) and (u8, u16, u32, u64).
 - [x] Add `as` cast operator.
 - [x] Remove implicit casts.
 - [x] Support multiple file imports.
 - [x] Type safe enums.
    * `enum Foo { A, B }; enum Foo x = Foo::A;`
 - [x] Change type syntax to `let foo: type` and func to `func foo() -> type`.
    * `let x: i32 = 5;`
    * `func foo() -> i32 { return 5; }`
 - [x] Fix static arrays, and decay from array to pointer.
    * Index only for array & future slice types, or pointers to them.
    * Pointer to array can be converted to pointer to first elem for C interop.
 - [x] Switch statements that make sense.
 - [x] Struct init and struct expressions.
    * `Foo{x = 1, y = 2}`?
 - [x] Avoid aggregates in LLVM registers.
    * Aggregates are represented as `ptr` to them on stack.
    * Except for function args, returns, struct members.
    * `a = b` for aggregate creates memcpy.
    * `Foo{a = 1}` generates `alloca`
    * Function args and ret need to be store/loaded.
 - [x] Type safe unions.
 - [x] Rename `NULL` to `null`
 - [x] Let expressions, `if (let a = x as foo)` support.
 - [x] Auto `&` on union -> struct ptr casts?
 - [ ] Require extern for external functions.
 - [ ] Remove function declarations, supporting use before define.
 - [ ] Move decl, stmt and expr to Unions.
 - [ ] `bool` (i1) type.

 - [ ] Generics.
     * `func foo<T>(a: T, b: T) -> T { return a + b; }`

 - [ ] `is<T>(uinion) -> bool` function.
     * `is<T>(union: T::parent*) -> bool { return union as T* != NULL; }`

 - [ ] Fix relative imports, split source to `src/sema/...`
 - [ ] Model LLVM IR.
 - [ ] Add references?
 - [ ] Add slice type.
    * syntax: `[i8]`
    * stored as `{ ptr: T*, len: isize }`
    * arrays become `[1, 2, 3]`
    * array to slice: `array[start:end]` start & end are optional
    * array or pointer to it? can be converted to slice implicitly
 - [ ] Actual constant expressions and decls.
 - [ ] Correctly padded structs.
 - [ ] Universal function call syntax
    * `a.foo(...)` -> `foo(a, ...)`
 - [ ] Add target pointer size, Add iptr and uptr types.
 - [ ] armv7 or aarch64 backend
 - [ ] continue statement.

Formatter TODO
--------------

 - [ ] Preserve char constants.
 - [ ] Fix trailing comments in block scopes.
 - [ ] Preserve newline between comments, and comments & code.
 - [ ] Fix newline bugs
   `let x = y + z` Don't split after `=` if `+` is moved to new line.
 - [ ] Auto split based on max line length (88 chars?)
