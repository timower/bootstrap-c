{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    bootstrap-parent = {
      url = "github:timower/bootstrap-c?ref=dev";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      bootstrap-parent,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        pkgs-mingw = pkgs.pkgsCross.mingwW64.pkgs;
        pkgs-static = pkgs.pkgsStatic.pkgs;
        pkgs-arm = pkgs.pkgsCross.armv7l-hf-multiplatform;

        parent-bootstrap = bootstrap-parent.packages.${system}.default;
        bootstrap_rev = self.shortRev or self.dirtyShortRev;

        bootstrap = pkgs.callPackage ./bootstrap.nix {
          inherit bootstrap_rev parent-bootstrap;
        };
        bootstrap-checked = bootstrap.overrideAttrs (_oldAttrs: {
          doCheck = true;
        });
      in
      {
        packages = {
          default = bootstrap;

          static = pkgs-static.callPackage ./bootstrap.nix {
            inherit bootstrap_rev parent-bootstrap;
            enable_lsp = false;
          };

          cross-arm = pkgs-arm.callPackage ./bootstrap.nix {
            inherit bootstrap_rev parent-bootstrap;
            enable_lsp = false;
          };

          cross-mingw64 = pkgs-mingw.callPackage ./bootstrap.nix {
            inherit bootstrap_rev parent-bootstrap;
            enable_lsp = false;
          };
        };

        checks.bootstrap = bootstrap-checked;

        devShells.default = pkgs.mkShell {
          inputsFrom = [ bootstrap-checked ];
          packages = with pkgs; [
            gopls
            lldb
            ruff
          ];
          shellHook = ''
            # export PARENT_STAGE="${nixpkgs.lib.getExe parent-bootstrap}"
          '';
        };

        # packages.parent = pkgs.stdenv.mkDerivation {
        #   pname = "bootstrap";
        #   version = "dev";
        #   buildInputs = with pkgs; [
        #     git
        #     llvmPackages_19.llvm
        #   ];
        #   src = pkgs.fetchurl {
        #     url = "https://github.com/timower/bootstrap-c/releases/download/bootstrap-f7619be/bootstrap-f7619be.tar.gz";
        #     hash = "sha256-MEZfzNj6ar3eIy6SpJj/NoeGLzwUY1ao3m29nafwxGE=";
        #   };
        #   installPhase = ''
        #     mkdir -p $out/bin
        #     cp bootstrap $out/bin/bootstrap-parent
        #   '';
        # };
      }
    );
}
