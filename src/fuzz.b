import parse;

func LLVMFuzzerTestOneInput(data: i8*, size: uptr) -> i32 {
  let parseOpts = ParseOptions {
    concrete = false,
  };

  let bufPtr = calloc(1, size + sizeof(i64)) as i8*;
  memcpy(bufPtr, data, size);
  let buf = bufPtr[:size];

  initTokenSystem();
  printFile = getStdout();

  let decls = parseBufOpts("fuzz", buf, parseOpts);
  return 0;
}
