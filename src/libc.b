extern func putchar(c: i32) -> i32;
extern func puts(s: const i8*) -> i32;

extern func printf(format: const i8*, ...) -> i32;
extern func sprintf(s: i8*, format: const i8*, ...) -> i32;
extern func dprintf(fd: i32, format: const i8*, ...) -> i32;

extern func exit(status: i32);
extern func strcmp(s1: const i8*, s2: const i8*) -> i32;
extern func open(file: const i8*, oflags: i32, ...) -> i32;
extern func memcmp(s1: const void*, s2: const void*, n: u64) -> i32;
extern func strlen(s: const i8*) -> u64;
extern func malloc(size: u64) -> void*;
extern func calloc(count: u64, size: u64) -> void*;
extern func realloc(ptr: void*, size: u64) -> void*;
extern func strtol(ptr: const i8*, end: i8**, base: i32) -> i64;
extern func lseek(fd: i32, offset: i64, whence: i32) -> i64;
extern func read(fd: i32, buf: void*, nbytes: u64) -> i64;
extern func dirname(path: i8*) -> i8*;
extern func strdup(s: i8*) -> i8*;
extern func realpath(path: i8*, resolved_path: i8*) -> i8*;


// TODO: consts
let STDERR = 2;
