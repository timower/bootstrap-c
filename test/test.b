import libc;

enum Kind {
  A,
  B,
  C,
};

struct Foo {
  kind: Kind;
  value: i32;
};

struct Token {
};


union Type {
  Void {
  }
  Pointer {
    arg: Type*;
    isConst: i32;
  }
  Array {
    arg: Type*;
    size: i32;
  }
  Func {
    result: Type*;
    args: TypeList*;
  }
};

struct TypeList {
  type: Type*;
  next: TypeList*;
};

func test(fn: Type::Func*) {
  puts("Func");
}


// TODO:
// func getType() -> Type {
//   return Type::Void{};
// }
func main(argc: i32, argv: i8**) -> i32 {
  let foo: Type = Type::Void{};

  let bar: Type = Type::Func{
    result = &foo,
  };

  let ptr = &bar;
  switch (bar) {
    case Type::Array as array:
      printf("size: %d\n", array.size);
    case Type::Pointer as ptr:
      puts("pointer");
    case Type::Void:
      puts("Void!");
    case Type::Func as fn:
      test(&fn);
  }

  switch (*ptr) {
    case Type::Array as array:
      printf("size: %d\n", array.size);
    case Type::Pointer as ptr:
      puts("pointer");
    case Type::Void:
      puts("Void!");
    case Type::Func as fn:
      test(&fn);
  }

  let buz = Type::Void{} as Type;

  let vptr = &buz as Type::Void*;
  if (vptr == NULL) {
    puts("Null");
  } else {
    puts("Void!");
  }

  let nptr = &buz as Type::Func*;
  if (nptr == NULL) {
    puts("Null");
  } else {
    puts("Func!");
  }

  return 0;
}
