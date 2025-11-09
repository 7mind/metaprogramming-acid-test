{
  description = "Swift Metaprogramming Challenges - All three challenges implementation";

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
            swiftPackages.swift
            swiftPackages.Foundation
            swiftPackages.swiftpm
            swiftPackages.Dispatch
            swiftPackages.XCTest
          ];

          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
              pkgs.swiftPackages.Dispatch
              pkgs.swiftPackages.Foundation
              pkgs.swiftPackages.swift
            ]}:$LD_LIBRARY_PATH"
            echo "Swift Metaprogramming Challenges Development Environment"
            echo "Available commands:"
            echo "  swift build                      - Build the project"
            echo "  swift test                       - Run tests"
            echo "  swift run StructuredLoggingExample - Run structured logging example"
            echo "  swift run TypeReflectionExample    - Run type reflection example"
            echo "  swift run FunctoidExample          - Run functoid example"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "swift-metaprogramming-challenges";
          version = "0.1.0";

          src = ./.;

          nativeBuildInputs = with pkgs; [
            swiftPackages.swift
            swiftPackages.swiftpm
          ];

          buildPhase = ''
            export HOME=$TMPDIR
            swift build -c release
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp -r .build/release/* $out/bin/ 2>/dev/null || true
          '';

          doCheck = true;
          checkPhase = ''
            export HOME=$TMPDIR
            swift test
          '';
        };
      }
    );
}
