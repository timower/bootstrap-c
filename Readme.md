Bootstrap
=========

A project exploring compiler bootstrapping.
The first commit is a c-subset compiler that can compile itself.
Each commit after adds a new feature, and is compiled by the previous commit.


TODO
----
 - [ ] Add correct integer types (i8, i16, i32, i64) and (u8, u16, u32, u64).
 - [ ] Support multiple file imports.
 - [ ] Add `as` cast operator.
 - [ ] Remove implicit casts.
 - [ ] Type safe enums.
 - [ ] Change type syntax to `let foo: type`.
 - [ ] Require extern for external functions.
 - [ ] Add tuples
 - [ ] Add Unit type, remove void.
 - [ ] Remove function declarations, supporting use before define.
 - [ ] Correctly padded structs.
 - [ ] Generics.
