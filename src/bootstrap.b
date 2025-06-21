import libc;

import ast;
import ast.print;
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

  debug("Begin sema");
  decls = semaTopLevel(&semaState, decls);
  debug("End sema");

  debug("Begin irgen");
  let module = genModule(decls);
  debug("End irgen");

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
    debug("Begin print ir");
    printModule(&module);
  } else {
    debug("Begin emit");
    emitAsm(&module, args.target);
  }

  return 0;
}
