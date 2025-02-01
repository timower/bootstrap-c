import libc;

func getEscaped(c: i8) -> i8 {
  switch (c as i32) {
    case 'n':
      return '\n';
    case 't':
      return '\t';
    case 'r':
      return '\r';
    case '0':
      return '\0';
    default:
      return c;
  }
}

struct Buf {
  mem: i8*;
  size: i64;
};

func readFile(name: i8*) -> Buf {
  let fd = open(name, 0);  //  O_RDONLY
  if (fd == -1) {
    printf("open failed: %s!\n", name);
    return Buf {};
  }

  let size = lseek(fd, 0, 2);  //  SEEK_END
  if (size == -1) {
    puts("seek failed!");
    return Buf {};
  }

  if (lseek(fd, 0, 0) == -1) {
    // SEEK_SET
    puts("seek failed!");
    return Buf {};
  }

  let fileMem: i8* = malloc(size as u64);

  let off: i64 = 0;
  while (off != size) {
    let r = read(fd, fileMem + off, (size - off) as u64);
    if (r == -1) {
      puts("read failed!");
      return Buf {};
    }
    off += r;
  }

  return Buf {
    mem = fileMem,
    size = size,
  };
}
