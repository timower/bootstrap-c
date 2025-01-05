import libc;

enum Kind { A, B, C };

struct Foo {
  kind : Kind;
  value : i32;
};

// union Type {
//   // clang-format off
//   Void {};
//   Int {
//     size: i32;
//     isSigned: i32;
//   };
//   Pointer {
//     arg: Type*;
//     isConst: i32;
//   };
//   Array {
//     arg: Type*;
//     size: i32;
//   };
//   Struct {
//     tag: Token;
//     fields: TypeList*;
//   };
//   Func {
//     result: Type*;
//     args: TypeList*;
//   };
//   Enum {
//     tag: Token;
//   };
//
//   Tag {
//     tag: Token;
//   };
//   // clang-format on
// };
//
// struct TypeList {
//   type : Type;
//   next : TypeList *;
// };
//
// func test(fn : Type::Func *) { puts("Func"); }

func main(argc : i32, argv : i8 **) -> i32 {
  // let foo = Type::Void{};
  // let bar = Type::Array{arg = &foo, size = 2};
  // let ptr = &bar;

  // switch (bar) {
  // case Type::Array as array:
  //   printf("size: %d\n", array.size);
  // case Type::Pointer as ptr:
  //   puts("pointer");
  // case Type::Void:
  //   puts("Void!");
  // case Type::Func as fn:
  //   test(&fn);
  // }

  // switch (ptr) {
  // case Type::Array as array:
  //   printf("size: %d\n", array->size);
  // case Type::Pointer as ptr:
  //   puts("pointer");
  // case Type::Void:
  //   puts("Void!");
  // case Type::Func as fn:
  //   test(fn);
  // }

  let x = Kind::C;
  switch (x) {
  case Kind::A, Kind::B:
    puts("A or B");
  case Kind::C:
    puts("C");
  }

  return 0;
}
