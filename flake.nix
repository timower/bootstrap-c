{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      rec {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "bootstrap";
          version = self.shortRev or self.dirtyShortRev;
          src = ./.;
          buildInputs = with pkgs; [
            llvmPackages_19.llvm
            packages.parent
          ];
          PARENT_STAGE = "${packages.parent}/bin/bootstrap-parent";
          installPhase = ''
            mkdir -p $out/bin
            cp bootstrap $out/bin
          '';
        };

        packages.parent = pkgs.stdenv.mkDerivation {
          pname = "bootstrap";
          version = "dev";
          buildInputs = with pkgs; [
            git
            llvmPackages_19.llvm
          ];
          src = pkgs.fetchurl {
            url = "https://github.com/timower/bootstrap-c/releases/download/bootstrap-f7619be/bootstrap-f7619be.tar.gz";
            hash = "sha256-MEZfzNj6ar3eIy6SpJj/NoeGLzwUY1ao3m29nafwxGE=";
          };
          installPhase = ''
            mkdir -p $out/bin
            cp bootstrap $out/bin/bootstrap-parent
          '';
        };
      }
    );
}
