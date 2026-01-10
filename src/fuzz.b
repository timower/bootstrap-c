import parse;
import sema;
import irgen;
import ir.print;

func LLVMFuzzerTestOneInput(data: i8*, size: uptr) -> i32 {
  let parseOpts = ParseOptions {
    concrete = false,
  };

  let bufPtr = calloc(1, size + sizeof(i64)) as i8*;
  memcpy(bufPtr, data, size);
  let buf = bufPtr[:size];

  initTokenSystem();

  printFile = getStdout();
  outFile = getStdout();

  let decls = parseBufOpts("fuzz", buf, parseOpts);
  if (decls == null) {
    return -1;
  }

  printTopLevel(decls);

  let target = Target {
    triple = "foo",
    arch = Arch::Aarch64,
    platform = Platform::Linux,
    abi = ABI::Gnu,
  };

  let state = initSemaState(target, false);
  decls = semaTopLevel(&state, decls);

  let module = genModule(decls, target);
  if (module == null) {
    return 0;
  }

  printModule(module);

  return 0;
}
