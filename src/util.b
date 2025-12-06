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
  size: iptr;
}

func readFile(name: i8*) -> Buf {
  let fd = open(name, 0);  //  O_RDONLY
  if (fd == -1) {
    fprintf(getStderr(), "open failed: %s!\n", name);
    return Buf {};
  }

  let size = lseek(fd, 0, SEEK_END);
  if (size == -1) {
    fprintf(getStderr(), "seek failed!\n");
    return Buf {};
  }

  if (lseek(fd, 0, SEEK_SET) == -1) {
    fprintf(getStderr(), "seek failed!\n");
    return Buf {};
  }

  let fileMem: i8* = malloc(size as uptr);

  let off: iptr = 0;
  while (off != size) {
    let r = read(fd, fileMem + off, (size - off) as uptr);
    if (r == -1) {
      fprintf(getStderr(), "read failed!\n");
      return Buf {};
    }
    off += r;
  }

  return Buf {
    mem = fileMem,
    size = size,
  };
}

func readStdin() -> Buf {
  let bufSize: uptr = 1024;
  let mem: i8* = calloc(1, bufSize);

  let res: iptr = 0;
  let offset: uptr = 0;
  while (res = read(0, mem + offset, bufSize - offset), res > 0) {
    offset += res as uptr;
    if (offset + 128 > bufSize) {
      bufSize *= 2;
      mem = realloc(mem, bufSize);
    }
  }

  if (res != 0) {
    puts("Read Failed");
    return Buf {};
  }

  return Buf {
    mem = mem,
    size = offset as iptr,
  };
}
