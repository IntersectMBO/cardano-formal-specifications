{
  description = "Generate the various artefacts including the development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.11";

    # Others
    flake-utils.url   = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_linux" "aarch64-linux" "aarch64-darwin" ] (system:
      let
      in {

        packages.hello = nixpkgs.legacyPackages.${system}.hello;

        packages.default = self.packages.${system}.hello;

  });
}
