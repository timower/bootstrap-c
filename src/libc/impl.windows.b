extern func __acrt_iob_func(index: i32) -> void*;
extern func _fullpath(absPath: i8*, relPath: i8*, maxLen: uptr) -> i8*;
extern func GetFileAttributesA(path: i8*) -> i32;

func getStderr() -> void* {
  return __acrt_iob_func(2);
}

func getStdout() -> void* {
  return __acrt_iob_func(1);
}

func realpath(path: i8*, resolved_path: i8*) -> i8* {
  let result = _fullpath(resolved_path, path, 4096 as uptr);
  if (GetFileAttributesA(result) == -1) {
    return null;
  }
  return result;
}

func access(path: const i8*, mode: i32) -> i32 {
  if (GetFileAttributesA(path as i8*) == -1) {
    return -1;
  }
  return 0;
}
