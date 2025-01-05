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
 - [ ] Change type syntax to `let foo: type` and func to `func foo() -> type`.
 - [ ] Require extern for external functions.
 - [ ] Remove function declarations, supporting use before define.
 - [ ] Actual constant expressions and decls.
 - [ ] Remove statement and expression distinction.
 - [ ] Type safe enums.
 - [ ] Add tuples
 - [ ] Add Unit type, remove void.
 - [ ] Correctly padded structs.
 - [ ] Add target pointer size, Add iptr and uptr types.
 - [ ] Generics.
 - [ ] armv7 or aarch64 backend
 - [ ] continue statement.
