import parse;
import sema;

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
  if (decls == null) {
    return 0;
  }

  let target = Target {
    triple = "foo",
    arch = Arch::Aarch64,
    platform = Platform::Linux,
    abi = ABI::Gnu,
  };

  let state = initSemaState(target, false);
  decls = semaTopLevel(&state, decls);
  return 0;
}
