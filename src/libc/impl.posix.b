extern let stdout: void*;

extern let stderr: void*;

func getStdout() -> void* {
  return stdout;
}

func getStderr() -> void* {
  return stderr;
}

extern func realpath(path: i8*, resolved_path: i8*) -> i8*;
extern func access(path: const i8*, mode: i32) -> i32;
