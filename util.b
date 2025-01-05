i8 getEscaped(i8 c) {
  switch (c as i32) {
  case 'n':
    return '\n' as i8;
  case 't':
    return '\t' as i8;
  case 'r':
    return '\r' as i8;
  case '0':
    return '\0' as i8;
  default:
    return c;
  }
}
