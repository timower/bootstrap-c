import target;

func cmpTriplePart(start: i8*, target: i8*) -> i8* {
  let cur = start;

  while (*cur != 0 && *target != 0) {
    if (*cur != *target) {
      return null;
    }
    cur++;
    target++;
  }

  if (*target == 0 && (*cur == 0 || *cur == '-')) {
    return cur;
  }

  return null;
}

func parseArch(str: i8*, arch: Arch*) -> i8* {
  let res = cmpTriplePart(str, "armv7l");
  if (res != null) {
    *arch = Arch::Armv7;
    return res;
  }

  res = cmpTriplePart(str, "arm64");
  if (res != null) {
    *arch = Arch::Aarch64;
    return res;
  }
  res = cmpTriplePart(str, "aarch64");
  if (res != null) {
    *arch = Arch::Aarch64;
    return res;
  }

  res = cmpTriplePart(str, "x86_64");
  if (res != null) {
    *arch = Arch::X86_64;
    return res;
  }

  return null;
}

func parsePlatform(str: i8*, platform: Platform*) -> i8* {
  let res = cmpTriplePart(str, "unknown-linux");
  if (res != null) {
    *platform = Platform::Linux;
    return res;
  }

  res = cmpTriplePart(str, "apple-darwin");
  if (res != null) {
    *platform = Platform::Darwin;
    return res;
  }

  res = cmpTriplePart(str, "w64");
  if (res != null) {
    *platform = Platform::Windows;
    return res;
  }

  return null;
}

func parseAbi(str: i8*, abi: ABI*) -> i8* {
  if (*str == 0) {
    *abi = ABI::None;
    return str;
  }

  let res = cmpTriplePart(str, "gnueabihf");
  if (res != null) {
    *abi = ABI::GnuEabiHf;
    return res;
  }

  res = cmpTriplePart(str, "gnu");
  if (res != null) {
    *abi = ABI::Gnu;
    return res;
  }

  res = cmpTriplePart(str, "musl");
  if (res != null) {
    *abi = ABI::Musl;
    return res;
  }

  res = cmpTriplePart(str, "mingw32");
  if (res != null) {
    *abi = ABI::Mingw32;
    return res;
  }

  return null;
}


// Triples have 3 parts separated by '-'.
// The middle (platform) part can contain a '-'.
// Examples:
//  - x86_64-unknown-linux-gnu
//  - x86_64-apple-darwin
//  - x86_64-w64-mingw32
func parseTriple(triple: i8*) -> Target {
  let result = Target {
    triple = triple,
  };

  if (strcmp(triple, "darwin") == 0) {
    return Target {
      triple = "arm64-apple-darwin",
      arch = Arch::Aarch64,
      platform = Platform::Darwin,
      abi = ABI::None,
    };
  }
  if (strcmp(triple, "windows") == 0) {
    return Target {
      triple = "x86_64-w64-mingw32",
      arch = Arch::X86_64,
      platform = Platform::Windows,
      abi = ABI::Mingw32,
    };
  }

  let cur = parseArch(triple, &result.arch);
  if (cur == null) {
    fprintf(getStderr(), "Failed to parse arch\n");
    exit(1);
  }

  if (*cur != '-') {
    fprintf(getStderr(), "Expected - in triple: %s\n", cur);
    exit(1);
  }

  cur = parsePlatform(cur + 1, &result.platform);
  if (cur == null) {
    fprintf(getStderr(), "Failed to parse platform\n");
    exit(1);
  }

  // ABI is optional.
  if (*cur == '-') {
    cur++;
  }

  cur = parseAbi(cur, &result.abi);
  if (cur == null) {
    fprintf(getStderr(), "Failed to parse abi\n");
    exit(1);
  }

  return result;
}
