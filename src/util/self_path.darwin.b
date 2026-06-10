import alloc;
import stdlib.libc;

extern func _NSGetExecutablePath(buf: i8*, size: u32*) -> i32;

func getSelfPath(a: Allocator*) -> [i8] {
  const max_path = 512;
  let buf = alloc(a, max_path);
  let len: u32 = max_path;
  let res = _NSGetExecutablePath(buf, &len);
  if (res == -1) {
    return (null as i8*)[:0];
  }

  return (buf as i8*)[:len];
}
