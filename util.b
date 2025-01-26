import libc;

func getEscaped(c: i8) -> i8 {
  switch (c as i32) {
    case 110:
      return 10 as i8;
    case 116:
      return 9 as i8;
    case 114:
      return 13 as i8;
    case 48:
      return 0 as i8;
    default:
      return c;
  }
}


struct Buf {
  mem: i8*;
  size: i64;
};

func readFile(name: i8*) -> Buf {
  let fd = open(name, 0);   //  O_RDONLY
  if (fd == -1) {
    puts("open failed!");
    return Buf {};
  }

  let size = lseek(fd, 0, 2);   //  SEEK_END
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
