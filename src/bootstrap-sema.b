import libc;

import ast;
import parse;
import sema;
import cmdline;

func main(argc: i32, argv: i8**) -> i32 {
  let args = parseOpts(argc, argv);

  debugMode = true;

  debug("Starting parse...");
  let decls = parseFile(args.inputFile);
  if (decls == null) {
    debug("Failed to parse file");
    return -1;
  }
  debug("Parse completed successfully");

  debug("Starting semantic analysis...");
  let semaState = initSemaState(args.target);
  decls = semaTopLevel(&semaState, decls);
  debug("Semantic analysis completed successfully");

  return 0;
}
