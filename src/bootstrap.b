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

func getOutOrInplaceFileName(a: Allocator*, args: CommandLineArgs*) -> i8* {
  if (args->inPlace) {
    if (args->readFromStdin) {
      puts("Cannot use -i with stdin input");
      exit(1);
      return null;
    }
    if (args->outputFile != null) {
      puts("Cannot use both -i and -o");
      exit(1);
      return null;
    }

    let tempFile = alloc(a, args->inputFile.len + 20);
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

  if (rename(fileName, &args->inputFile[0]) != 0) {
    // The opening of the output, or reading of the input would fail before
    // this fails.
    unreachable("Failed to replace original file");
    exit(1);
  }
}

func main(argc: i32, argv: i8**) -> i32 {
  initTokenSystem();

  let args = parseOpts(argv[:argc]);
  printFile = getStdout();

  let globalAlloc = Allocator {};
  defer freeAll(&globalAlloc);
  let name: i8* = &args.inputFile[0];
  let buf = args.readFromStdin
       ? readStdin(&globalAlloc) : readFile(&globalAlloc, name);

  if (&buf[0] == null) {
    puts("Failed to read input");
    return 1;
  }

  let parseState = ParseState {
    concrete = (args.mode == Mode::Format),
    buf = buf,
    fileName = name,
    astAlloc = &globalAlloc,
  };

  let decls = parse(&parseState);
  if (decls == null) {
    puts("Failed to parse file");
    return 1;
  }

  if (args.mode == Mode::Format) {
    let outFileName = getOutOrInplaceFileName(&globalAlloc, &args);
    printFile = getOutFile(outFileName);
    printTopLevel(decls);
    finishInPlace(&args, outFileName, printFile);
    return 0;
  }

  {
    let semaState = initSemaState(args.target, args.mode == Mode::SemaLsp, &globalAlloc);
    defer freeSemaState(&semaState);
    debug("Begin sema");
    decls = semaTopLevel(&semaState, decls);
    if (decls == null) {
      return 1;
    }
    debug("End sema");
  }

  if (args.mode == Mode::SemaLsp) {
    return 0;
  }

  if (args.mode == Mode::Sema) {
    return 0;
  }

  debug("Begin irgen");
  let module = genModule(&globalAlloc, decls, args.target);
  if (module == null) {
    return 1;
  }
  debug("End irgen");

  outFile = getOutFile(args.outputFile);

  if (args.outputKind == OutputKind::LLVM) {
    debug("Begin print ir");
    printModule(module);
  } else {
    debug("Begin emit");
    emitAsm(module, args.target);
  }

  return 0;
}
