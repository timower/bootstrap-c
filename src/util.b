import libc;

func unreachable(msg: i8*) {
  printf("UNREACHABLE: %s\n", msg);
  exit(3);
}

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

func nullBuf() -> [i8] {
  return (null as i8*)[:0];
}

func newBuf(len: iptr) -> [i8] {
  let ptr = calloc(1, len as uptr) as i8*;
  return ptr[:len];
}

func reallocBuf(buf: [i8], len: iptr) -> [i8] {
  let newBuf = realloc(&buf[0], len as uptr) as i8*;
  return newBuf[:len];
}

func readFile(name: i8*) -> [i8] {
  let fd = open(name, 0);  //  O_RDONLY
  if (fd == -1) {
    fprintf(getStderr(), "open failed: %s!\n", name);
    return nullBuf();
  }

  let size = lseek(fd, 0, SEEK_END);
  if (size == -1) {
    fprintf(getStderr(), "seek failed!\n");
    return nullBuf();
  }

  if (lseek(fd, 0, SEEK_SET) == -1) {
    // The first one would've failed, so this seems unreachable.
    unreachable("seek failed!");
    return nullBuf();
  }

  let result = newBuf(size + sizeof(i64))[:size];

  let off: iptr = 0;
  while (off != size) {
    let rest = result[off:];
    let r = read(fd, &rest[0], (rest.len as iptr) as uptr);
    if (r == -1) {
      fprintf(getStderr(), "read failed!\n");
      return nullBuf();
    }
    off += r;
  }

  return result;
}

func readStdin() -> [i8] {
  let mem = newBuf(1024);

  let res: iptr = 0;
  let offset: iptr = 0;
  let remaining = mem;
  while (res = read(0, &remaining[0], (remaining.len as iptr) as uptr), res > 0) {
    offset += res;
    if (offset + 128 > mem.len as iptr) {
      mem = reallocBuf(mem, (mem.len * 2) as iptr);
    }
    remaining = mem[offset:];
  }

  if (res != 0) {
    puts("Read Failed");
    return nullBuf();
  }

  return mem[:offset];
}
