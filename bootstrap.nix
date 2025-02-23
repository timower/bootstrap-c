{
  # Arguments
  bootstrap_rev ? "dev",

  # From inputs
  parent-bootstrap,

  # From nixpkgs
  stdenv,
  lit,
  llvmPackages_19,
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
  nativeCheckInputs = [ lit ];

  PARENT_STAGE = "${parent-bootstrap}/bin/bootstrap";
  LLCFLAGS = "--mtriple=${targettriple} --relocation-model=pic -O0 -filetype=obj";
  LDFLAGS = "";

  installPhase = ''
    mkdir -p $out/bin
    cp ./bootstrap $out/bin || cp ./bootstrap.exe $out/bin
  '';
}
