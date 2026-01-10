// RUN: split-file %s %t
// RUN: %bootstrap %t/bar.b -target x86_64-unknown-linux-gnu | grep 'ret i64 8'
// RUN: %bootstrap %t/bar.b -target arm64-unknown-linux-gnu | grep 'ret i64 8'
// RUN: %bootstrap %t/bar.b -target aarch64-unknown-linux-gnu | grep 'ret i64 8'
// RUN: %bootstrap %t/bar.b -target armv7l-unknown-linux-gnueabihf | grep 'ret i32 4'
// RUN: %bootstrap %t/bar.b -target x86_64-w64-mingw32 | grep 'ret i64 8'
// RUN: %bootstrap %t/bar.b -target x86_64-w64-musl | grep 'ret i64 8'
// RUN: %bootstrap %t/bar.b -target arm64-apple-darwin | grep 'ret i64 8'
//
// RUN: %bootstrap %t/bar.b -target darwin | grep 'arm64-apple-darwin'
// RUN: %bootstrap %t/bar.b -target windows | grep 'x86_64-w64-mingw32'
//--- bar.b
import foo;

func bar() -> [i8] {
  return &_TARGET_;
}


//--- foo.b
func foo() -> uptr {
  return sizeof(iptr);
}
