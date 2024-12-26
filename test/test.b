import libc;

func main(argc : i32, argv : i8 **) -> i32 {
  let x : i8 * = argc == 0 ? "test" : "test2";
  puts(x);
  return 0;
}
