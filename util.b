func getEscaped(c: i8) -> i8 {
  switch (c as i32) {
    case 110:
      return 10 as i8;
    case 116:
      return 9 as i8;
    case 114:
      return 13 as i8;
    case 48:
      return 0 as i8;
    default:
      return c;
  }
}
