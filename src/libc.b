import libc.impl;

extern func putchar(c: i32) -> i32;
extern func puts(s: const i8*) -> i32;

extern func printf(format: const i8*, ...) -> i32;
extern func sprintf(s: i8*, format: const i8*, ...) -> i32;

extern func exit(status: i32);
extern func strcmp(s1: const i8*, s2: const i8*) -> i32;
extern func open(file: const i8*, oflags: i32, ...) -> i32;
extern func memcmp(s1: const void*, s2: const void*, n: uptr) -> i32;
extern func strlen(s: const i8*) -> uptr;
extern func malloc(size: uptr) -> void*;
extern func calloc(count: uptr, size: uptr) -> void*;
extern func realloc(ptr: void*, size: uptr) -> void*;
extern func strtol(ptr: const i8*, end: i8**, base: i32) -> uptr; // ret long
extern func lseek(fd: i32, offset: iptr, whence: i32) -> iptr;
extern func read(fd: i32, buf: void*, nbytes: uptr) -> iptr;
extern func dirname(path: i8*) -> i8*;
extern func strdup(s: i8*) -> i8*;
extern func memcpy(dest: void*, src: void*, len: uptr) -> i8*;

extern func fopen(path: i8*, mode: i8*) -> void*;
extern func fclose(file: void*) -> i32;
extern func fprintf(file: void*, format: const i8*, ...) -> i32;
extern func rename(oldname: i8*, newname: i8*) -> i32;
extern func getpid() -> i32;

const F_OK: i32 = 0;

const SEEK_SET: i32 = 0;

const SEEK_END: i32 = 2;
