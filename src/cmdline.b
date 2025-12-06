import libc;
import debug;

import target;
import target.parse;

enum OutputKind {
  LLVM,
  Asm,
}

enum Mode {
  Compile,
  Format,
  Sema,
  SemaLsp,
}

struct CommandLineArgs {
  // required.
  inputFile: i8*;

  // stdout if null.
  outputFile: i8*;

  // defaults to LLVM.
  outputKind: OutputKind;

  // Parsed target triple.
  target: Target;

  // defaults to Compile.
  mode: Mode;

  // format in-place.
  inPlace: bool;

  // true if reading from stdin (inputFile is "-").
  readFromStdin: bool;
}

func usage() {
  puts("Bootstrap Compiler");
  puts("");
  puts("Usage: bootstrap [OPTIONS] input.b");
  puts("");
  puts("OPTIONS:");
  puts("  -o <file>           Write output to <file> (default: stdout)");
  puts("  -emit-llvm          Emit LLVM IR (default)");
  puts("  -emit-asm           Emit assembly code");
  puts("  -target <target>    Target platform (posix, windows, darwin)");
  puts("  -format             Format source code");
  puts("  -sema               Run semantic analysis only");
  puts("  -i                  Format in-place (use with -format)");
  puts("  -stdin-filename <f> Set filename when reading from stdin");
  puts("  -debug              Enable debug mode");
  puts("  -                   Read from stdin");
  puts("");
  puts("Examples:");
  puts("  bootstrap hello.b              # Compile to LLVM IR");
  puts("  bootstrap -format -i hello.b   # Format file in-place");
  puts("  bootstrap -sema hello.b        # Check syntax only");
  exit(1);
}

func parseOpts(argc: i32, argv: i8**) -> CommandLineArgs {
  // TODO: _TARGET_
  let args = CommandLineArgs {
    inputFile = null,
    outputFile = null,
    outputKind = OutputKind::LLVM,
    target = parseTriple("x86_64-unknown-linux-gnu"),
    mode = Mode::Compile,
    inPlace = false,
    readFromStdin = false,
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
      args.target = parseTriple(*(argv + i + 1));
      i++;
    } else if (strcmp(arg, "-debug") == 0) {
      debugMode = true;
    } else if (strcmp(arg, "-format") == 0) {
      args.mode = Mode::Format;
    } else if (strcmp(arg, "-sema") == 0) {
      args.mode = Mode::Sema;
    } else if (strcmp(arg, "-sema-lsp") == 0) {
      args.mode = Mode::SemaLsp;
    } else if (strcmp(arg, "-i") == 0) {
      args.inPlace = true;
    } else if (strcmp(arg, "-stdin-filename") == 0) {
      if (i + 1 >= argc) {
        puts("Expected filename after -stdin-filename");
        usage();
      }
      args.inputFile = *(argv + i + 1);
      args.readFromStdin = true;
      i++;
    } else {
      if (args.inputFile != null) {
        puts("Multiple input files not supported");
        usage();
      }
      if (strcmp(arg, "-") == 0) {
        args.readFromStdin = true;
        args.inputFile = "stdin";
      } else {
        args.inputFile = arg;
      }
    }
  }

  if (args.inputFile == null) {
    puts("No input file specified");
    usage();
  }

  return args;
}

func getOutFile(fileName: i8*) -> void* {
  if (fileName == null) {
    return getStdout();
  }

  let file = fopen(fileName, "wb");
  if (file == null) {
    puts("Failed to open output file");
    exit(-1);
  }
  return file;
}
