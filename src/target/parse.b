import target;
import util;

func cmpTriplePart(str: [i8]*, target: i8*) -> bool {
  let targetSlice = target[:strlen(target)];

  let i = 0;
  for (; i < str->len && i < targetSlice.len; i++) {
    if ((*str)[i] != targetSlice[i]) {
      return false;
    }
  }

  if (i == targetSlice.len && (i == str->len || (*str)[i] == '-')) {
    *str = (*str)[i:];
    return true;
  }

  return false;
}

func parseArch(str: [i8]*, arch: Arch*) -> bool {
  if (cmpTriplePart(str, "armv7l")) {
    *arch = Arch::Armv7;
    return true;
  }
  if (cmpTriplePart(str, "arm64")) {
    *arch = Arch::Aarch64;
    return true;
  }
  if (cmpTriplePart(str, "aarch64")) {
    *arch = Arch::Aarch64;
    return true;
  }

  if (cmpTriplePart(str, "x86_64")) {
    *arch = Arch::X86_64;
    return true;
  }

  return false;
}

func parsePlatform(str: [i8]*, platform: Platform*) -> bool {
  if (cmpTriplePart(str, "unknown-linux")) {
    *platform = Platform::Linux;
    return true;
  }

  if (cmpTriplePart(str, "apple-darwin")) {
    *platform = Platform::Darwin;
    return true;
  }

  if (cmpTriplePart(str, "w64")) {
    *platform = Platform::Windows;
    return true;
  }

  return false;
}

func parseAbi(str: [i8]*, abi: ABI*) -> bool {
  if (str->len == 0) {
    *abi = ABI::None;
    return true;
  }

  if (cmpTriplePart(str, "gnueabihf")) {
    *abi = ABI::GnuEabiHf;
    return true;
  }

  if (cmpTriplePart(str, "gnu")) {
    *abi = ABI::Gnu;
    return true;
  }

  if (cmpTriplePart(str, "musl")) {
    *abi = ABI::Musl;
    return true;
  }

  if (cmpTriplePart(str, "mingw32")) {
    *abi = ABI::Mingw32;
    return true;
  }

  return false;
}


// Triples have 3 parts separated by '-'.
// The middle (platform) part can contain a '-'.
// Examples:
//  - x86_64-unknown-linux-gnu
//  - x86_64-apple-darwin
//  - x86_64-w64-mingw32
func parseTriple(triple: [i8]) -> Target {
  let result = Target {
    triple = triple,
  };

  if (strcmp(&triple[0], "darwin") == 0) {
    return Target {
      triple = "arm64-apple-darwin",
      arch = Arch::Aarch64,
      platform = Platform::Darwin,
      abi = ABI::None,
    };
  }

  if (strcmp(&triple[0], "windows") == 0) {
    return Target {
      triple = "x86_64-w64-mingw32",
      arch = Arch::X86_64,
      platform = Platform::Windows,
      abi = ABI::Mingw32,
    };
  }

  if (!parseArch(&triple, &result.arch)) {
    fprintf(getStderr(), "Failed to parse arch\n");
    exit(1);
  }

  if (triple[0] != '-') {
    fprintf(getStderr(), "Expected - in triple: %s\n", &triple[0]);
    exit(1);
  }
  triple = triple[1:];

  if (!parsePlatform(&triple, &result.platform)) {
    fprintf(getStderr(), "Failed to parse platform\n");
    exit(1);
  }

  // ABI is optional.
  if (triple[0] == '-') {
    triple = triple[1:];
  }

  if (!parseAbi(&triple, &result.abi)) {
    fprintf(getStderr(), "Failed to parse abi\n");
    exit(1);
  }

  return result;
}
