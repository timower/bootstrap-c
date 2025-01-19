func putchar(c : i32)->i32;
func puts(s : const i8 *) -> i32;
func printf(format : const i8 *, ...) -> i32;
func sprintf(s : i8 *, format : const i8 *, ...) -> i32;
func exit(status : i32)->void;
func strcmp(s1 : const i8 *, s2 : const i8 *) -> i32;
func open(file : const i8 *, oflags : i32, ...) -> i32;

func memcmp(s1 : const void *, s2 : const void *, n : u64) -> i32;
func strlen(s : const i8 *) -> u64;
func malloc(size : u64)->void *;
func calloc(count : u64, size : u64) -> void *;
func realloc(ptr: void*, size: u64) -> void*;

// long
func strtol(ptr : const i8 *, end : i8 **, base : i32) -> i64;
func lseek(fd : i32, offset : i64, whence : i32)->i64;
func read(fd : i32, buf : void *, nbytes : u64) -> i64;


struct Buf {
  mem: i8*;
  size: i64;
};

func readFile(name: i8*) -> Buf {
  let fd = open(name, 0); //  O_RDONLY
  if (fd == -1) {
    puts("open failed!");
    return Buf{};
  }

  let size = lseek(fd, 0, 2); //  SEEK_END
  if (size == -1) {
    puts("seek failed!");
    return Buf{};
  }

  if (lseek(fd, 0, 0) == -1) { // SEEK_SET
    puts("seek failed!");
    return Buf{};
  }

  let fileMem : i8 * = malloc(size as u64);

  let off : i64 = 0;
  while (off != size) {
    let r = read(fd, fileMem + off, (size - off) as u64);
    if (r == -1) {
      puts("read failed!");
      return Buf{};
    }
    off += r;
  }

  return Buf {
    mem = fileMem,
    size = size,
  };
}
