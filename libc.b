func putchar(c : i32)->i32;
func puts(s : const i8 *) -> i32;
func printf(format : const i8 *, ...) -> i32;
func sprintf(s : i8 *, format : const i8 *, ...) -> i32;
func exit(status : i32)->void;
func strcmp(s1 : const i8 *, s2 : const i8 *) -> i32;
func open(file : const i8 *, oflags : i32, ...) -> i32;

func memcmp(s1 : const void *, s2 : const void *, n : u64) -> i32;
func strlen(s : const i8 *) -> u64;
func malloc(size : u64)->void *;
func calloc(count : u64, size : u64)->void *;

// long
func strtol(ptr : const i8 *, end : i8 **, base : i32) -> i64;
func lseek(fd : i32, offset : i64, whence : i32)->i64;
func read(fd : i32, buf : void *, nbytes : u64) -> i64;
