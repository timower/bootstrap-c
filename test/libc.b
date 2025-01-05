i32 putchar(i32 __c);
i32 puts(const i8 *s);
i32 printf(const i8 *format, ...);
i32 sprintf(i8 *s, const i8 *format, ...);
void exit(i32 status);
i32 strcmp(const i8 *s1, const i8 *s2);
i32 open(const i8 *file, i32 oflags, ...);

i32 memcmp(const void *s1, const void *s2, u64 n);
u64 strlen(const i8 *s);
void *malloc(u64 size);
void *calloc(u64 count, u64 size);

// long
i64 strtol(const i8 *ptr, i8 **end, i32 base);
i64 lseek(i32 fd, i64 offset, i32 whence);
i64 read(i32 fd, void *buf, u64 nbytes);
