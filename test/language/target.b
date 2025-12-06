// RUN: mkdir -p %t
// RUN: %bootstrap %s -target x86_64-unknown-linux-gnu | grep 'ret i64 8'
// RUN: %bootstrap %s -target armv7l-unknown-linux-gnueabihf | grep 'ret i32 4'
// RUN: %bootstrap %s -target x86_64-w64-mingw32 | grep 'ret i64 8'
func foo() -> uptr {
  return sizeof(iptr);
}

func bar() -> i8* {
  return &_TARGET_;
}
