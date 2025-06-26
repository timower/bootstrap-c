{
  # Arguments
  bootstrap_rev ? "dev",

  # From inputs
  parent-bootstrap,

  # From nixpkgs
  stdenv,
  lib,
  lit,
  llvmPackages_19,
  pkgsCross,
  qemu-user,
}:
let
  targettriple = stdenv.hostPlatform.config;
in
stdenv.mkDerivation {
  pname = "bootstrap";
  version = bootstrap_rev;

  src = ./.;
  nativeBuildInputs = [
    llvmPackages_19.llvm
    parent-bootstrap
  ];
  nativeCheckInputs = [
    lit
    pkgsCross.aarch64-multiplatform.buildPackages.gcc
    qemu-user
    llvmPackages_19.clang
  ];

  PARENT_STAGE = "${parent-bootstrap}/bin/bootstrap";
  LLCFLAGS = "--mtriple=${targettriple} --relocation-model=pic -O0 -filetype=obj";
  LDFLAGS = "";
  BOOTSTRAP_FLAGS = lib.optionalString stdenv.hostPlatform.isWindows "-target windows";

  installPhase = ''
    mkdir -p $out/bin
    cp ./bootstrap $out/bin || cp ./bootstrap.exe $out/bin
  '';
}
