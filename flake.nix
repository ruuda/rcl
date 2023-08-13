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

          rustSources = pkgs.lib.sourceFilesBySuffices ./. [
            ".rs"
            "Cargo.lock"
            "Cargo.toml"
          ];

          pythonSources = pkgs.lib.sourceFilesBySuffices ./. [ ".py" ];

          goldenSources = ./golden;

          rcl = pkgs.rustPlatform.buildRustPackage rec {
            inherit name version;
            src = rustSources;
            cargoLock.lockFile = ./Cargo.lock;
          };

          coverageBuild = rcl.overrideAttrs (old: {
            name = "rcl-coverage";
            buildType = "debug";
            RUSTFLAGS = "-C instrument-coverage -C link-dead-code -C debug-assertions";
          });
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
                RCL_BIN=${debugBuild}/bin/rcl python3 ${goldenSources}/run.py
                touch $out
                '';

              grammar = pkgs.runCommand
                "check-grammar"
                { buildInputs = [ pkgs.bison ]; }
                ''
                bison -Wcounterexamples,error=all ${./src/grammar.y} --output $out
                '';

              fmtRust = pkgs.runCommand
                "check-fmt-rust"
                { buildInputs = [ pkgs.cargo pkgs.rustfmt ]; }
                ''
                cargo fmt --manifest-path ${rustSources}/Cargo.toml -- --check
                touch $out
                '';

              fmtPython = pkgs.runCommand
                "check-fmt-python"
                { buildInputs = [ pkgs.black ]; }
                ''
                black --check --diff ${pythonSources}
                touch $out
                '';

              typecheckPython = pkgs.runCommand
                "check-typecheck-python"
                { buildInputs = [ pkgs.mypy ]; }
                ''
                mypy --strict ${pythonSources}
                touch $out
                '';
            };

            packages = {
              inherit rcl;

              default = rcl;

              coverage = pkgs.runCommand
                "rcl-coverage"
                { buildInputs = [ pkgs.python3 pkgs.grcov ]; }
                ''
                export bintools=${pkgs.rustc.llvmPackages.bintools-unwrapped}/bin

                RCL_BIN=${coverageBuild}/bin/rcl python3 ${goldenSources}/run.py

                # For debugging purposes, output raw LLVM coverage to the logs.
                # This can help identify which files are traced, etc.
                $bintools/llvm-profdata merge -sparse *.profraw -o rcl.profdata
                $bintools/llvm-cov report \
                  --instr-profile=rcl.profdata \
                  --object ${coverageBuild}/bin/rcl

                # During the build, source file names get included as
                # "source/src/lib.rs" etc. But when grcov runs, even if we
                # provide --source-dir, inside that source dir is only a
                # directory "src", not "sources/src", so it fails to find any
                # files. To work around that, make a directory link "sources".
                ln -s ${rustSources} source

                grcov . \
                  --source-dir source \
                  --binary-path ${coverageBuild}/bin \
                  --llvm-path $bintools \
                  --prefix-dir source \
                  --llvm \
                  --output-types html \
                  --output-path $out
                '';
            };
          }
      );
}
