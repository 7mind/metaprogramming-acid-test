{
  description = "Zig Metaprogramming Challenges - All three challenges implementation";

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
            zig
          ];

          shellHook = ''
            echo "Zig Metaprogramming Challenges Development Environment"
            echo "Available commands:"
            echo "  zig build test         - Run tests"
            echo "  zig build run-examples - Run all examples"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "zig-metaprogramming-challenges";
          version = "0.1.0";

          src = ./.;

          nativeBuildInputs = with pkgs; [ zig ];

          buildPhase = ''
            export HOME=$TMPDIR
            zig build -Doptimize=ReleaseSafe
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp -r zig-out/bin/* $out/bin/
          '';

          doCheck = true;
          checkPhase = ''
            export HOME=$TMPDIR
            zig build test
          '';
        };
      }
    );
}
