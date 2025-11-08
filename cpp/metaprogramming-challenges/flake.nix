{
  description = "C++ Metaprogramming Challenges - All three challenges implementation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cmake
            gcc
            clang
            ninja
            gdb
          ];

          shellHook = ''
            echo "C++ Metaprogramming Challenges Development Environment"
            echo "Available commands:"
            echo "  cmake -B build -G Ninja"
            echo "  cmake --build build"
            echo "  ctest --test-dir build"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "metaprogramming-challenges";
          version = "0.1.0";

          src = ./.;

          nativeBuildInputs = with pkgs; [ cmake ninja ];

          buildPhase = ''
            cmake -B build -G Ninja
            cmake --build build
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp build/example_* $out/bin/
            cp build/test_* $out/bin/
          '';

          doCheck = true;
          checkPhase = ''
            cd build
            ctest --output-on-failure
          '';
        };
      }
    );
}
