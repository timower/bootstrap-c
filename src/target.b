import stdlib.libc;

enum Arch {
  Armv7,
  Aarch64,
  X86_64,
}

enum Platform {
  Linux,
  Darwin,
  Windows,
}

enum ABI {
  None,
  Gnu,
  GnuEabiHf,
  Musl,
  Mingw32,
}

struct Target {
  triple: [i8];

  arch: Arch;
  platform: Platform;
  abi: ABI;
}

func getImportName(target: Target*) -> [i8] {
  switch (target->platform) {
    case Platform::Linux:
      return "posix";
    case Platform::Darwin:
      return "darwin";
    case Platform::Windows:
      return "windows";
  }
}

func is32Bit(target: Target*) -> bool {
  return target->arch == Arch::Armv7;
}

func getIntSize(target: Target*) -> i32 {
  return is32Bit(target) ? 32 : 64;
}

func getPtrSize(target: Target*) -> i32 {
  return is32Bit(target) ? 4 : 8;
}
