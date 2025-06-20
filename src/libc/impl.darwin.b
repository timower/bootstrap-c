extern let __stdoutp: void*;

extern let __stderrp: void*;

func getStdout() -> void* {
  return __stdoutp;
}

func getStderr() -> void* {
  return __stderrp;
}

extern func realpath(path: i8*, resolved_path: i8*) -> i8*;
