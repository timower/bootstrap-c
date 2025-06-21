import libc;
import debug;

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
  puts("Usage: compile [-debug] [-target target] [-emit-llvm | -emit-asm] [-o output] file.b");
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
    } else if (strcmp(arg, "-debug") == 0) {
      debugMode = true;
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
