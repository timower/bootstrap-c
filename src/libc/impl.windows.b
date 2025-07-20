extern func __acrt_iob_func(index: i32) -> void*;
extern func _fullpath(absPath: i8*, relPath: i8*, maxLen: u64) -> i8*;
extern func GetFileAttributesA(path: i8*) -> i32;

func getStderr() -> void* {
  return __acrt_iob_func(2);
}

func getStdout() -> void* {
  return __acrt_iob_func(1);
}

func realpath(path: i8*, resolved_path: i8*) -> i8* {
  let result = _fullpath(resolved_path, path, 4096 as u64);
  if (GetFileAttributesA(result) == -1) {
    return null;
  }
  return result;
}
