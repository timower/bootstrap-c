import alloc;
import stdlib.libc;

func getSelfPath(a: Allocator*) -> [i8] {
  const max_path = 512;
  let buf = alloc(a, max_path);
  let len = readlink("/proc/self/exe", buf, max_path);
  if (len == -1) {
    return (null as i8*)[:0];
  }

  return (buf as i8*)[:len];
}
