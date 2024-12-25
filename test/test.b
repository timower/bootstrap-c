import libc;

struct foo {
  x : i32;
  next : bar *;
};

struct bar {
  y : i32;
  next : foo *;
};

func main() -> i32 {
  let f : foo = {0};
  let b : bar = {0};
  f.next = &b;
  b.next = &f;

  return b.next->x;
}
