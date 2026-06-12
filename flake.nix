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

        parent-brio = bootstrap-parent.packages.${system}.default;
        brio_rev = self.shortRev or self.dirtyShortRev;

        brio = pkgs.callPackage ./brio.nix {
          inherit brio_rev parent-brio;
        };
        brio-checked = brio.overrideAttrs (_oldAttrs: {
          doCheck = true;
          doInstallCheck = true;
        });
      in
      {
        packages = {
          default = brio;

          static = pkgs-static.callPackage ./brio.nix {
            inherit brio_rev parent-brio;
            enable_lsp = false;
          };

          cross-arm = pkgs-arm.callPackage ./brio.nix {
            inherit brio_rev parent-brio;
            enable_lsp = false;
          };

          cross-arm-static = pkgs-arm.pkgsStatic.callPackage ./brio.nix {
            inherit brio_rev; # parent-brio;
            parent-brio = brio;
            enable_lsp = false;
          };

          cross-mingw64 = pkgs-mingw.callPackage ./brio.nix {
            inherit brio_rev parent-brio;
            enable_lsp = false;
          };
        };

        checks.brio = brio-checked;

        devShells.default = pkgs.mkShell {
          inputsFrom = [ brio-checked ];
          packages = with pkgs; [
            gopls
            lldb
            ruff
          ];
          shellHook = ''
            export LDFLAGS='-fsanitize=address'
            export prefix=build/out

            # export ASAN_OPTIONS='detect_leaks=0'
            # export PARENT_STAGE="${nixpkgs.lib.getExe parent-brio}"
          '';
        };

        # packages.parent = pkgs.stdenv.mkDerivation {
        #   pname = "brio";
        #   version = "dev";
        #   buildInputs = with pkgs; [
        #     git
        #     llvmPackages_19.llvm
        #   ];
        #   src = pkgs.fetchurl {
        #     url = "https://github.com/timower/brio-c/releases/download/brio-f7619be/brio-f7619be.tar.gz";
        #     hash = "sha256-MEZfzNj6ar3eIy6SpJj/NoeGLzwUY1ao3m29nafwxGE=";
        #   };
        #   installPhase = ''
        #     mkdir -p $out/bin
        #     cp brio $out/bin/brio-parent
        #   '';
        # };
      }
    );
}
