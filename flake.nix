{
  description = "RCL";

  inputs.nixpkgs.url = "nixpkgs/nixos-23.05";

  outputs = { self, nixpkgs }: 
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      # Ridiculous boilerplate required to make flakes somewhat usable.
      forEachSystem = f:
        nixpkgs.lib.zipAttrsWith
          (name: values: builtins.foldl' (x: y: x // y) {} values)
          (map
            (k: builtins.mapAttrs (name: value: { "${k}" = value; }) (f k))
            supportedSystems
          );
    in
      forEachSystem (system:
        let
          name = "rcl";
          version = "0.0.0";
          pkgs = import nixpkgs { inherit system; };
        in
          rec {
            devShells.default = pkgs.mkShell {
              nativeBuildInputs = [
                (with pkgs.python3.pkgs; toPythonApplication pygments)
                pkgs.black
                pkgs.mkdocs
                pkgs.mypy
                pkgs.python3
                pkgs.rustup
              ];
            };

            checks = rec {
              debugBuild = packages.default.overrideAttrs (old: {
                name = "check-test";
                buildType = "debug";
              });

              golden = pkgs.runCommand
                "check-golden"
                { buildInputs = [ pkgs.python3 ]; }
                ''
                cd ${pkgs.lib.sourceFilesBySuffices ./. [".py" ".test"]}
                RCL_BIN=${debugBuild}/bin/rcl python3 golden/run.py
                touch $out
                '';

              grammar = pkgs.runCommand
                "check-grammar"
                { buildInputs = [ pkgs.bison ]; }
                ''
                bison -Wcounterexamples,error=all ${./src/grammar.y} --output $out
                '';

              fmt = pkgs.runCommand
                "check-fmt"
                { buildInputs = [ pkgs.cargo pkgs.rustfmt ]; }
                ''
                cargo fmt --manifest-path ${./.}/Cargo.toml -- --check
                touch $out
                '';
            };

            packages = {
              default = pkgs.rustPlatform.buildRustPackage rec {
                inherit name version;
                src = pkgs.lib.sourceFilesBySuffices ./. [
                  ".rs"
                  "Cargo.lock"
                  "Cargo.toml"
                ];
                cargoLock.lockFile = ./Cargo.lock;
              };
            };
          }
      );
}
