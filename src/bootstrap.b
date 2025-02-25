import libc;

import ast;
import parse;
import sema;

import irgen;
import ir.print;

enum OutputKind {
  LLVM,
  Asm,
};

struct CommandLineArgs {
  // required.
  inputFile: i8*;

  // stdout if null.
  outputFile: i8*;

  // defaults to LLVM.
  outputKind: OutputKind;

  // Posix or windows for now.
  target: i8*;
};

func usage() {
  puts("Usage: compile [-emit-llvm | -emit-asm] [-o output] file.b");
  exit(1);
}

func parseOpts(argc: i32, argv: i8**) -> CommandLineArgs {
  let args = CommandLineArgs {
    inputFile = null,
    outputFile = null,
    outputKind = OutputKind::LLVM,
    target = "posix",
  };

  for (let i = 1; i < argc; i += 1) {
    let arg = *(argv + i);
    if (strcmp(arg, "-o") == 0) {
      if (i + 1 >= argc) {
        puts("Expected output file after -o");
        usage();
      }
      args.outputFile = *(argv + i + 1);
      i++;
    } else if (strcmp(arg, "-emit-llvm") == 0) {
      args.outputKind = OutputKind::LLVM;
    } else if (strcmp(arg, "-emit-asm") == 0) {
      args.outputKind = OutputKind::Asm;
    } else if (strcmp(arg, "-target") == 0) {
      if (i + 1 >= argc) {
        puts("Expected target after -target");
        usage();
      }
      args.target = *(argv + i + 1);
      i++;
    } else {
      if (args.inputFile != null) {
        puts("Multiple input files not supported");
        usage();
      }
      args.inputFile = arg;
    }
  }

  if (args.inputFile == null) {
    puts("No input file specified");
    usage();
  }

  return args;
}

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
    // Numeric value of
    // 577 = O_WRONLY | O_CREAT | O_TRUNC
    // 504 = S_IRWXU | S_IRWXG
    let fd = open(args.outputFile, 577, 504);
    if (fd < 0) {
      puts("Failed to open output file");
      return -1;
    }
    outFd = fd;
  }

  if (args.outputKind == OutputKind::LLVM) {
    printModule(&module);
  } else {
    puts("TODO");
    return -1;
  }

  return 0;
}
