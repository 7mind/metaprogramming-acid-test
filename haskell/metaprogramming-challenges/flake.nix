{
  description = "Haskell Metaprogramming Challenges";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "metaprogramming-challenges";
      in
      {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self {};

        packages.default = self.packages.${system}.${packageName};

        devShells.default = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghc
            cabal-install
            haskell-language-server
            hlint
            ormolu
          ];

          inputsFrom = [
            self.packages.${system}.${packageName}.env
          ];

          shellHook = ''
            echo "Haskell Metaprogramming Challenges development environment"
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version | head -1)"
          '';
        };
      }
    );
}
