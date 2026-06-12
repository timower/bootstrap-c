{
  # Arguments
  brio_rev ? "dev",
  enable_lsp ? true,
  asan ? false,

  # From inputs
  parent-brio,

  # From nixpkgs
  stdenv,
  lib,
  lit,
  llvmPackages_19,
  pkgsCross,
  qemu-user,
  go,
  tree-sitter,
  nodejs,
  python3Packages,
  valgrind-light,

  pkgsBuildBuild,
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
      "${pkgsBuildBuild.binutils}"/bin/ar r "$out"/lib/libgcc_eh.a
    '';
  };

  lit-with-psutil = lit.overrideAttrs (old: {
    propagatedBuildInputs = old.propagatedBuildInputs ++ [ python3Packages.psutil ];
  });
in
llvmPackages_19.stdenv.mkDerivation {
  pname = "brio";
  version = brio_rev;

  src = ./.;

  propagatedBuildInputs = lib.optional stdenv.targetPlatform.isStatic empty-libgcc_eh;

  nativeBuildInputs = [
    llvmPackages_19.llvm
    parent-brio
  ]
  ++ lib.optionals enable_lsp [
    go
  ];

  buildFlags = [ "all" ] ++ lib.optionals enable_lsp [ "lsp" ];

  nativeCheckInputs =
    lib.optionals (!stdenv.hostPlatform.isDarwin) [
      qemu-user
      pkgsCross.armv7l-hf-multiplatform.buildPackages.gcc
      valgrind-light
    ]
    ++ [
      lit-with-psutil
      llvmPackages_19.clang
      llvmPackages_19.lld
      tree-sitter
      nodejs
    ];

  PARENT_STAGE = "${lib.getExe parent-brio}";

  ASAN_OPTIONS = "detect_leaks=0";
  LD_FLAGS = lib.optionalString asan "-fsanitize=address";

  BRIO_FLAGS = "-target ${targetTriple}";

  preConfigure = ''
    export GOCACHE="$TMPDIR/go-cache"
  '';

  installFlags = [ "prefix=$${out}" ];
  installCheckFlags = [ "prefix=$${out}" ];

  meta.mainProgram = "brio";
}
