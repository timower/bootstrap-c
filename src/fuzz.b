import parse;
import sema;
import irgen;
import ir.print;

func LLVMFuzzerTestOneInput(data: i8*, size: uptr) -> i32 {
  let bufPtr = calloc(1, size + sizeof(i64)) as i8*;
  defer free(bufPtr);

  memcpy(bufPtr, data, size);
  let buf = bufPtr[:size];

  initTokenSystem();

  printFile = getStdout();
  outFile = getStdout();

  let globalAlloc = Allocator {};
  defer freeAll(&globalAlloc);

  let fileName: [i8] = "fuzz";
  let parseState = ParseState {
    concrete = false,
    buf = buf,
    fileName = &fileName[0],
    astAlloc = &globalAlloc,
  };

  let decls = parse(&parseState);
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

  decls = sema(&globalAlloc, target, false, decls);

  let module = genModule(&globalAlloc, decls, target);
  if (module == null) {
    return 0;
  }

  printModule(module);

  return 0;
}
