{
  # Arguments
  bootstrap_rev ? "dev",
  enable_lsp ? true,

  # From inputs
  parent-bootstrap,

  # From nixpkgs
  stdenv,
  lib,
  lit,
  llvmPackages_19,
  pkgsCross,
  qemu-user,
  go,
  gopls,
}:
let
  targettriple = stdenv.hostPlatform.config;

  # Default is posix, special case for windows & darwin.
  windowsFlag = lib.optionalString stdenv.hostPlatform.isWindows "-target windows";
  darwinFlag = lib.optionalString stdenv.hostPlatform.isDarwin "-target darwin";
in
stdenv.mkDerivation {
  pname = "bootstrap";
  version = bootstrap_rev;

  src = ./.;
  nativeBuildInputs =
    [
      llvmPackages_19.llvm
      parent-bootstrap
    ]
    ++ lib.optionals enable_lsp [
      go
      gopls
    ];

  nativeCheckInputs = [
    lit
    pkgsCross.aarch64-multiplatform.buildPackages.gcc
    qemu-user
    llvmPackages_19.clang
  ];

  PARENT_STAGE = "${parent-bootstrap}/bin/bootstrap";
  ASAN_OPTIONS = "detect_leaks=0";
  # LLCFLAGS = "--mtriple=${targettriple} --relocation-model=pic -O0 -filetype=obj";
  # LDFLAGS = "";

  BOOTSTRAP_FLAGS = windowsFlag + darwinFlag;

  preConfigure = ''
    export GOCACHE="$TMPDIR/go-cache"
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./bootstrap $out/bin || cp ./bootstrap.exe $out/bin
    cp ./bootstrap-lsp/bootstrap-lsp $out/bin
  '';
}
