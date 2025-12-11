{
  # Arguments
  bootstrap_rev ? "dev",
  enable_lsp ? true,
  asan ? false,

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

  binutils,
}:
let
  targetTriple = stdenv.hostPlatform.config;

  # Hack stolen from lix.
  # See https://github.com/NixOS/nixpkgs/issues/177129
  empty-libgcc_eh = stdenv.mkDerivation {
    pname = "empty-libgcc_eh";
    version = "0";
    dontUnpack = true;
    installPhase = ''
      mkdir -p "$out"/lib
      "${binutils}"/bin/ar r "$out"/lib/libgcc_eh.a
    '';
  };
in
llvmPackages_19.stdenv.mkDerivation {
  pname = "bootstrap";
  version = bootstrap_rev;

  src = ./.;

  propagatedBuildInputs = lib.optional stdenv.targetPlatform.isStatic empty-libgcc_eh;

  nativeBuildInputs = [
    llvmPackages_19.llvm
    parent-bootstrap
  ]
  ++ lib.optionals enable_lsp [
    go
  ];

  buildFlags = [ "all" ] ++ lib.optionals enable_lsp [ "lsp" ];

  nativeCheckInputs =
    lib.optionals (!stdenv.hostPlatform.isDarwin) [
      qemu-user
      pkgsCross.armv7l-hf-multiplatform.buildPackages.gcc
    ]
    ++ [
      lit
      llvmPackages_19.clang
    ];

  PARENT_STAGE = "${parent-bootstrap}/bin/bootstrap";

  ASAN_OPTIONS = "detect_leaks=0";
  LD_FLAGS = lib.optionalString asan "-fsanitize=address";

  BOOTSTRAP_FLAGS = "-target ${targetTriple}";

  preConfigure = ''
    export GOCACHE="$TMPDIR/go-cache"
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./bootstrap $out/bin || cp ./bootstrap.exe $out/bin
  ''
  + lib.optionalString enable_lsp ''
    cp ./bootstrap-lsp/bootstrap-lsp $out/bin
  '';
}
