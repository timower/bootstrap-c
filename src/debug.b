import stdlib.libc;

let debugMode = false;

func debug(msg: i8*) {
  if (!debugMode) {
    return;
  }

  fprintf(getStderr(), "DEBUG: %s\n", msg);
}
