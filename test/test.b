import libc;

let test = {"test", "foo"};

func main(argc : i32, argv : i8 **) -> i32 {
  let x = argc == 0 ? "a" as i8 * : "bc" as i8 *;
  puts(x);
  return 0;
}
