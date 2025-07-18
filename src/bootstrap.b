import libc;
import util;

import ast;
import ast.print;
import parse;
import sema;
import emit;

import irgen;
import ir.print;
import cmdline;

func getOutOrInplaceFileName(args: CommandLineArgs*) -> i8* {
  if (args->inPlace) {
    if (args->inputFile == null) {
      puts("Cannot use -i with stdin input");
      exit(-1);
    }
    if (args->outputFile != null) {
      puts("Cannot use both -i and -o");
      exit(-1);
    }

    let len = strlen(args->inputFile);
    let tempFile = malloc(len + 20);
    sprintf(tempFile, "%s.tmp.%d", args->inputFile, getpid());
    return tempFile;
  }

  return args->outputFile;
}

func finishInPlace(args: CommandLineArgs*, fileName: i8*, file: void*) {
  if (!args->inPlace) {
    return;
  }

  fclose(file);

  if (rename(fileName, args->inputFile) != 0) {
    puts("Failed to replace original file");
    exit(-1);
  }
}

func main(argc: i32, argv: i8**) -> i32 {
  initTokenSystem();
  let args = parseOpts(argc, argv);
  printFile = getStdout();

  let name: i8* = args.inputFile;
  let buf = Buf {};
  if (!args.readFromStdin) {
    buf = readFile(name);
  } else {
    buf = readStdin();
  }

  if (buf.mem == null) {
    puts("Failed to read input");
    return -1;
  }

  let parseOpts = ParseOptions {
    concrete = (args.mode == Mode::Format),
  };

  let decls = parseBufOpts(name, buf, parseOpts);
  if (decls == null) {
    puts("Failed to parse file");
    return -1;
  }

  if (args.mode == Mode::Format) {
    let outFileName = getOutOrInplaceFileName(&args);
    printFile = getOutFile(outFileName);
    printTopLevel(decls);
    finishInPlace(&args, outFileName, printFile);
    return 0;
  }

  let semaState = initSemaState(args.target, args.mode == Mode::SemaLsp);

  debug("Begin sema");
  decls = semaTopLevel(&semaState, decls);
  if (decls == null) {
    return 1;
  }
  debug("End sema");

  if (args.mode == Mode::SemaLsp) {
    // TODO: dump dep graph, dump refs, ...
    return 0;
  }

  if (args.mode == Mode::Sema) {
    return 0;
  }

  debug("Begin irgen");
  let module = genModule(decls);
  debug("End irgen");

  outFile = getOutFile(args.outputFile);

  if (args.outputKind == OutputKind::LLVM) {
    debug("Begin print ir");
    printModule(&module);
  } else {
    debug("Begin emit");
    emitAsm(&module, args.target);
  }
  return 0;
}
