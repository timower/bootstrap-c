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
 - [ ] Add slice type.
    * syntax: `[i8]`
    * stored as `{ ptr: T*, len: isize }`
    * arrays become `[1, 2, 3]`
    * array to slice: `array[start:end]` start & end are optional
    * array or pointer to it? can be converted to slice implictly
 - [ ] Switch statements that make sense.
 - [ ] Struct init and struct expressions.
    * `Foo{.x = 1, .y = 2}`?
 - [ ] Type safe unions.
 - [ ] Require extern for external functions.
 - [ ] Remove function declarations, supporting use before define.
 - [ ] Actual constant expressions and decls.
 - [ ] Remove statement and expression distinction.
 - [ ] Add tuples
 - [ ] Add Unit type, remove void.
 - [ ] Correctly padded structs.
 - [ ] Add target pointer size, Add iptr and uptr types.
 - [ ] Generics.
 - [ ] armv7 or aarch64 backend
 - [ ] continue statement.
