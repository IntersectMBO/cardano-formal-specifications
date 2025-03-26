{
  description = "Generate the various artefacts including the development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.11";

    deltaq = {
      url  = "github:DeltaQ-SD/deltaq";
      flake = false;
    };

    # Others
    flake-utils.url   = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, deltaq }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "praos1";
        ghcVersion = "ghc96";

        pkgs = import nixpkgs {inherit system overlays;};

        haskellPackages = pkgs.haskell.packages.${ghcVersion};

        overlays = [
          (final: prev: {
            haskell = prev.haskell // {
              packages = prev.haskell.packages // rec {
                "${ghcVersion}" = prev.haskell.packages.${ghcVersion}.override {
                  # perform our overrides
                  overrides = final: super: {
                    # reference the DeltaQ repository directly from Github
                    probability-polynomial = final.callCabal2nix "probability-polynomial" "${deltaq}/lib/probability-polynomial" {};
                    deltaq = final.callCabal2nix "deltaq" "${deltaq}/lib/deltaq" {};


                    # checks for influxdb perform network operations, not permitted in this context
                    # influxdb = prev.haskell.lib.dontCheck (final.callCabal2nix "influxdb" "${pnsol-influxdb}" {});

                    # want a later hsql-pool that is bundled with the nixpkgs
                    # snapshot of hackage (should be checked if still needed
                    # at major update time)
                    # hasql-pool = prev.haskell.lib.dontCheck (final.callHackage "hasql-pool" "1.1" {});

                    # the cabal project containing the (locally) associated library
                    # "nhc-qed-support" = final.callCabal2nix "nhc-qed-support" "${self}/nhc-qed-support" {};
                  };
                };
              };
            };
          })
        ];
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName "${self}/src/performance" rec {
                # local dependencies go here
          };


        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          # haskell related tools needed
          buildInputs = with haskellPackages; [
            cabal-install
            ghcid
            haskell-language-server
          ];
          # other inputs needed (typically to allow cabal to work properly)
          nativeBuildInputs = [
            pkgs.zlib
          ] ;
          # all the inputs being used by the target package
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
