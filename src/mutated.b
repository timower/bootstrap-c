import cmdline;


// Fake bootstrap compiler, used by the lit-mutate target.
// This makes sure that a compiler that returns dummy output causes tests to fail.
func main(argc: i32, argv: i8**) -> i32 {
  let args = parseOpts(argc, argv);
  switch (args.mode) {
    case Mode::Compile:
      let outFile = getOutFile(args.outputFile);
      fprintf(outFile, "define i32 @main() {\n");
      fprintf(outFile, "  ret i32 0\n");
      fprintf(outFile, "}\n");
    case Mode::Format:
      return 0;
    case Mode::Sema:
      return 0;
  }
  return 0;
}
