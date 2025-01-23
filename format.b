import libc;
import util;
import ast;
import parse;

func readStdin() -> Buf {
  let bufSize: u64 = 1024;
  let mem: i8* = calloc(1, bufSize);

  let res: i64 = 0;
  let offset: u64 = 0;
  while (res = read(0, mem + offset, bufSize - offset), res > 0) {
    offset += res as u64;
    if (offset + 128 > bufSize) {
      bufSize *= 2;
      mem = realloc(mem, bufSize);
    }
  }

  if (res != 0) {
    puts("Read Failed");
    return Buf{};
  }

  return Buf{
    mem = mem,
    size = offset as i64,
  };
}

func main(argc: i32, argv: i8**) -> i32 {
  if (argc > 2) {
    puts("Usage: compile file.c");
    return -1;
  }

  let parseOpts = ParseOptions{
    concrete = 1,
  };

  let name: i8* = "stdin";
  let buf = Buf{};
  if (argc == 2) {
    name = *(argv + 1);
    buf = readFile(name);
  } else {
    buf = readStdin();
  }

  if (buf.mem == NULL) {
    return 1;
  }

  let decls = parseBufOpts(name, buf, parseOpts);

  printTopLevel(decls);

  return 0;
}
