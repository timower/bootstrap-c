import libc;

import ast;
import parse;
import sema;
import emit;

import irgen;
import ir.print;
import cmdline;

func main(argc: i32, argv: i8**) -> i32 {
  let args = parseOpts(argc, argv);

  let decls = parseFile(args.inputFile);
  if (decls == null) {
    puts("Failed to parse file");
    return -1;
  }

  let semaState = initSemaState(args.target);
  decls = semaTopLevel(&semaState, decls);
  let module = genModule(decls);

  if (args.outputFile != null) {
    let file = fopen(args.outputFile, "wb");
    if (file == null) {
      puts("Failed to open output file");
      return -1;
    }
    outFile = file;
  } else {
    outFile = getStdout();
  }

  if (args.outputKind == OutputKind::LLVM) {
    printModule(&module);
  } else {
    emitAsm(&module, args.target);
  }

  return 0;
}
