import libc;

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
  triple: i8*;

  arch: Arch;
  platform: Platform;
  abi: ABI;
}

func getImportName(target: Target*) -> i8* {
  switch (target->platform) {
    case Platform::Linux:
      return "posix";
    case Platform::Darwin:
      return "darwin";
    case Platform::Windows:
      return "windows";
  }
}

func getIntSize(target: Target*) -> i32 {
  switch (target->arch) {
    case Arch::Armv7:
      return 32;
    case Arch::Aarch64, Arch::X86_64:
      return 64;
  }
}
