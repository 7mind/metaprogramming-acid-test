{
  description = "Functoid Kotlin implementation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # JDK
            jdk21

            # Kotlin
            kotlin
            kotlin-language-server

            # Build tools
            gradle

            # Development tools
            git
          ];

          shellHook = ''
            echo "Functoid Kotlin Development Environment"
            echo "JDK version: $(java -version 2>&1 | head -n 1)"
            echo "Kotlin version: $(kotlin -version 2>&1 | head -n 1)"
            echo "Gradle version: $(gradle --version | grep Gradle)"
          '';
        };
      }
    );
}
