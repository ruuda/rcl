{
  description = "RCL";

  # Pin to a Nixpkgs version that has the same rustc as in rust-toolchain.toml.
  inputs.nixpkgs.url = "nixpkgs/dfcffbd74fd6f0419370d8240e445252a39f4d10";

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
          version = "0.1.0";
          pkgs = import nixpkgs { inherit system; };

          python = pkgs.python311.override {
            packageOverrides = self: super: {
              # This package is not in Nixpkgs, define it here.
              # I should consider upstreaming it.
              types-pygments = self.buildPythonPackage rec {
                pname = "types-Pygments";
                version = "2.14.0.0";
                format = "setuptools";
                nativeBuildInputs = with self; [
                  types-setuptools
                  types-docutils
                ];
                src = self.fetchPypi {
                  inherit pname version;
                  hash = "sha256-G5R3cD3VeyzCU6Frii7WppK/zDO7OQWdEAiqnLA/xng=";
                };
              };

              # Build a custom version of Pygments that has our lexer enabled.
              # This enables MkDocs to highlight RCL code blocks.
              pygments = super.pygments.overridePythonAttrs (attrs: {
                postPatch = (attrs.postPatch or "") +
                  ''
                  # Copy our RCL lexer into the Pygments source tree, next to
                  # the other lexers.
                  cp ${./etc/pygments/rcl.py} pygments/lexers/rcl.py

                  # Regenerate pygments/lexers/_mapping.py, which contains all
                  # supported languages.
                  python scripts/gen_mapfiles.py
                  '';
              });
            };
          };

          pythonEnv = python.withPackages (ps: [
            ps.mkdocs
            ps.mypy
            ps.pygments
            ps.types-pygments
            # These two need to be in here for PyCharm to be able to find
            # dependencies from our fake virtualenv.
            ps.pip
            ps.setuptools
          ]);

          rustSources = pkgs.lib.sourceFilesBySuffices ./. [
            ".rs"
            "Cargo.lock"
            "Cargo.toml"
          ];

          pythonSources = pkgs.lib.sourceFilesBySuffices ./. [ ".py" ".pyi" ];

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

            # The tests already get executed by default when we build a Rust
            # package, and because of the RUSTFLAGS we set, the tests already
            # produce coverage too. We just need to copy those files into the
            # output such that the coverage report can include them. We also
            # need the test binaries for this, and the Rust installPhase sets
            # $releaseDir to the target directory.
            postInstall =
              ''
              mkdir -p $out/prof
              cp *.profraw $out/prof
              find $releaseDir/deps \
                -maxdepth 1 \
                -type f \
                -executable \
                -print0 | xargs -0 cp --target-directory=$out/bin
              '';
          });

          pyrcl = pkgs.rustPlatform.buildRustPackage rec {
            inherit version;
            name = "pyrcl";
            src = rustSources;
            nativeBuildInputs = [python];
            cargoLock.lockFile = ./Cargo.lock;
            buildAndTestSubdir = "pyrcl";
            postInstall =
              ''
              mv $out/lib/libpyrcl.so $out/lib/rcl.so
              cp ${./pyrcl}/rcl.pyi $out/lib/rcl.pyi
              '';
          };
        in
          rec {
            devShells.default = pkgs.mkShell {
              nativeBuildInputs = [
                # For consistency we could take `python.pkgs.black`, but it
                # rebuilds half the Python universe, so instead we take the
                # cached version that does not depend on our patched pygments.
                pkgs.python311Packages.black
                pkgs.maturin
                pkgs.rustup
                pythonEnv
              ];

              # Put something in .venv that looks enough like a traditional
              # virtualenv that it works with PyCharm autocomplete and jump to
              # definition and such.
              shellHook =
                ''
                mkdir -p .venv/bin
                ln -sf ${pythonEnv}/bin/python .venv/bin/python
                cat <<EOF > .venv/pyvenv.cfg
                home = ${pythonEnv}/bin
                executable = ${pythonEnv}/bin/python
                version = ${pythonEnv.python.version}
                include-system-site-packages = false
                EOF
                '';
            };

            checks = rec {
              debugBuild = packages.default.overrideAttrs (old: {
                name = "check-test";
                buildType = "debug";
                RUSTFLAGS = "-C debug-assertions";
              });

              fuzzers = pkgs.rustPlatform.buildRustPackage rec {
                name = "rcl-fuzzers";
                inherit version;
                src = rustSources;
                cargoLock.lockFile = ./Cargo.lock;
                buildAndTestSubdir = "fuzz";
              };

              golden = pkgs.runCommand
                "check-golden"
                { buildInputs = [ python ]; }
                ''
                RCL_BIN=${debugBuild}/bin/rcl python3 ${goldenSources}/run.py
                touch $out
                '';

              examples = pkgs.runCommand
                "check-examples"
                { buildInputs = []; }
                ''
                for f in ${./examples}/*; do
                  ${debugBuild}/bin/rcl evaluate $f
                done
                touch $out
                '';

              grammar = pkgs.runCommand
                "check-grammar"
                { buildInputs = [ pkgs.bison ]; }
                ''
                bison -Wcounterexamples,error=all ${./etc/bison/grammar.y} --output $out
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
                { buildInputs = [ pythonEnv ]; }
                ''
                # We split this check in two because there are multiple modules
                # named `rcl`, and they conflict if we typecheck in one go.
                mypy --strict --exclude pyrcl ${pythonSources}
                mypy --strict ${pythonSources}/pyrcl
                touch $out
                '';

              pyrclTest = pkgs.runCommand
                "check-pyrcl-test"
                { buildInputs = [ pkgs.python3 ]; }
                ''
                cd ${./pyrcl}
                PYTHONPATH=${pyrcl}/lib python3 ./test.py
                touch $out
                '';
            };

            packages = {
              inherit rcl pyrcl;

              default = rcl;

              coverage = pkgs.runCommand
                "rcl-coverage"
                { buildInputs = [ python pkgs.grcov ]; }
                ''
                export bintools=${pkgs.rustc.llvmPackages.bintools-unwrapped}/bin

                # Run the golden tests to generate the .profraw files.
                RCL_BIN=${coverageBuild}/bin/rcl python3 ${goldenSources}/run.py

                # Copy in the .profraw files from the tests.
                cp ${coverageBuild}/prof/*.profraw .

                # During the build, source file names get included as
                # "source/src/lib.rs" etc. But when grcov runs, even if we
                # provide --source-dir, inside that source dir is only a
                # directory "src", not "sources/src", so it fails to find any
                # files. To work around that, make a directory link "sources".
                ln -s ${rustSources} source

                grcov . \
                  --source-dir source \
                  --binary-path ${coverageBuild}/bin \
                  --excl-line '(#\[derive|unreachable!|panic!|std::process::exit|debug_assert_ne!)\(' \
                  --excl-start 'coverage:off' \
                  --excl-stop 'coverage:on' \
                  --llvm-path $bintools \
                  --prefix-dir source \
                  --llvm \
                  --output-types html \
                  --output-path $out

                # Also output the raw LLVM summary. This can be useful for
                # diffing, or for debugging to identify which files are traced.
                $bintools/llvm-profdata merge -sparse *.profraw -o rcl.profdata
                $bintools/llvm-cov report \
                  --instr-profile=rcl.profdata \
                  --ignore-filename-regex=/cargo-vendor-dir \
                  ${coverageBuild}/bin/rcl* \
                  > $out/summary.txt
                '';
            };
          }
      );
}
