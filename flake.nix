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

                    # want a later hsql-pool that is bundled with the nixpkgs
                    # snapshot of hackage (should be checked if still needed at major update time)
                    # hasql-pool = prev.haskell.lib.dontCheck (final.callHackage "hasql-pool" "1.1" {});

                    # the cabal project containing the (locally) associated library
                    praos1 = final.callCabal2nix "praos1" "${self}/praos1" {};
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
            lhs2tex
          ];
          # other inputs needed (typically to allow cabal to work properly)
          nativeBuildInputs = [
            pkgs.svg2pdf
            pkgs.zlib
            (pkgs.texlive.combine {
              inherit (pkgs.texlive)
                scheme-medium
                adjustbox
                collectbox
                environ
                enumitem
                pdfcol
                tcolorbox
                titling
                ucs
                upquote
                polytable
                lazylist
                multirow
                tikzfill
                listingsutf8
                        ;
      })
          ] ;
          # all the inputs being used by the target package
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
