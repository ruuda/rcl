{
  description = "RCL";

  # Pin to a Nixpkgs version that has the same rustc as in rust-toolchain.toml.
  inputs.nixpkgs.url = "nixpkgs/dfcffbd74fd6f0419370d8240e445252a39f4d10";

  inputs.rust-overlay.url = "github:oxalica/rust-overlay";
  inputs.rust-overlay.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, rust-overlay }:
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
      # The source of truth for the version number is Cargo.rcl.
      cargoToml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
      version = cargoToml.package.version;
    in
      forEachSystem (system:
        let
          name = "rcl";
          overlays = [ rust-overlay.overlays.default ];
          pkgs = import nixpkgs { inherit overlays system; };

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
                  cp ${./grammar/pygments/rcl.py} pygments/lexers/rcl.py

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

          # Define a custom toolchain from our toolchain file. For most
          # derivations we don't use it, we instead use the rustc from Nixpkgs,
          # to avoid unnecessary fetching/rebuilding. But for WASM, we need a
          # specific nightly toolchain with support for this target.
          rustWasm = pkgs.rust-bin.selectLatestNightlyWith (toolchain:
            toolchain.default.override {
              extensions = [ "rust-src" ];
              targets = [ "wasm32-unknown-unknown" ];
            }
          );

          rustSources = pkgs.lib.sourceFilesBySuffices ./. [
            ".rs"
            "Cargo.lock"
            "Cargo.toml"
          ];

          treeSitterSources = pkgs.lib.sourceFilesBySuffices ./grammar/tree-sitter-rcl [
            ".json"
            ".txt"
            ".scm"
            "Cargo.toml"
            "grammar.js"
          ];

          pythonSources = pkgs.lib.sourceFilesBySuffices ./. [ ".py" ".pyi" ];

          rclTomlSources = pkgs.lib.sourceFilesBySuffices ./. [ ".rcl" ".toml" ];

          goldenSources = ./golden;

          treeSitterRcl = pkgs.stdenv.mkDerivation {
            pname = "tree-sitter-rcl";
            inherit version;
            src = treeSitterSources;
            nativeBuildInputs = [ pkgs.nodejs pkgs.tree-sitter ];
            doCheck = true;
            buildPhase = "tree-sitter generate";
            checkPhase =
              ''
              # Tree sitter wants to write to ~/.config by default, but that
              # does not exist in the sandbox. Give it a directory to write to.
              mkdir tree-sitter-home
              export TREE_SITTER_DIR=tree-sitter-home
              export TREE_SITTER_LIBDIR=tree-sitter-home
              tree-sitter generate --build
              tree-sitter test
              '';
            installPhase =
              ''
              mkdir -p $out/lib
              cp tree-sitter-home/rcl.so $out/lib

              mkdir -p $out/dev/bindings
              cp -r bindings/rust $out/dev/bindings
              cp -r src     $out/dev
              cp -r queries $out/dev
              cp Cargo.toml $out/dev
              '';
          };

          rustSourcesAll = pkgs.runCommand "rcl-src-all" {}
            ''
            mkdir -p $out/grammar/tree-sitter-rcl/src/tree_sitter
            cp -r ${treeSitterRcl}/dev/src/{parser.c,node-types.json} $out/grammar/tree-sitter-rcl/src
            cp -r ${treeSitterRcl}/dev/src/tree_sitter/parser.h $out/grammar/tree-sitter-rcl/src/tree_sitter/parser.h
            cp -r ${treeSitterRcl}/dev/queries $out/grammar/tree-sitter-rcl
            cp -r ${rustSources}/* $out
            '';

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

          rcl-wasm = pkgs.rustPlatform.buildRustPackage rec {
            inherit version;
            name = "rcl-wasm";
            src = rustSources;
            cargoLock.lockFile = ./Cargo.lock;
            buildAndTestSubdir = "wasm";
            doCheck = false; # We already test the non-wasm build.
            nativeBuildInputs = [
              pkgs.binaryen
              pkgs.esbuild
              pkgs.wasm-bindgen-cli
              rustWasm
            ];

            buildPhase =
              ''
              cargo build \
                --manifest-path wasm/Cargo.toml \
                --profile=release-wasm \
                --target=wasm32-unknown-unknown \
                -Z build-std=std,panic_abort \
                -Z build-std-features=panic_immediate_abort

              wasm-opt -Oz \
                target/wasm32-unknown-unknown/release-wasm/rcl_wasm.wasm \
                --output target/rcl.wasm

              wasm-bindgen \
                --out-dir $out \
                --target no-modules \
                --no-typescript \
                target/rcl.wasm

              cat ${./wasm/src/rcl_dom.js} $out/rcl.js | esbuild --minify > $out/bundle.js
              mv $out/bundle.js $out/rcl.js
              '';
            installPhase = "echo 'Skipping default install phase.'";
          };

          website = pkgs.stdenv.mkDerivation {
            pname = "rcl-website";
            inherit version;
            src = ./website;
            nativeBuildInputs = [ pkgs.brotli ];
            doCheck = false;
            buildPhase =
              ''
              mkdir -p $out
              cp $src/* $out

              # Put the artifacts at an input-addressible path, so we don't
              # have issues with stale cache entries. We can use anything that
              # changes on release, and one of those things is the Nix base32
              # hash of the wasm module. This has the advantage that it remains
              # unchanged if we change the webpage. 8 characters is probably
              # enough to avoid collisions. To make the path less cryptic, we
              # also put the human-readable version name in there.
              hash="v${version}-$(basename ${rcl-wasm} | cut --bytes 1-8)"
              mkdir -p $out/$hash
              cp ${rcl-wasm}/* $out/$hash
              sed --in-place "s|rcl\.js|$hash/rcl.js|" $out/index.html

              # Pre-compress all assets for use with brotli_static in Nginx.
              for f in $(find $out -type f); do brotli -9 $f; done
              '';
          };

          fuzzers = pkgs.rustPlatform.buildRustPackage rec {
            name = "rcl-fuzzers";
            inherit version;
            src = rustSourcesAll;
            cargoLock.lockFile = ./Cargo.lock;
            buildAndTestSubdir = "fuzz";
          };

          fuzzers-coverage = fuzzers.overrideAttrs (old: {
            name = "rcl-fuzzers-coverage";
            buildType = "debug";
            RUSTFLAGS = "-C instrument-coverage -C link-dead-code -C debug-assertions";
          });

        in
          rec {
            devShells.default = pkgs.mkShell {
              name = "rcl";
              nativeBuildInputs = [
                # For consistency we could take `python.pkgs.black`, but it
                # rebuilds half the Python universe, so instead we take the
                # cached version that does not depend on our patched pygments.
                pkgs.python311Packages.black
                pkgs.binaryen
                pkgs.esbuild
                pkgs.grcov
                pkgs.maturin
                pkgs.nodejs  # Required for tree-sitter.
                pkgs.rustup
                pkgs.tree-sitter
                pkgs.wasm-bindgen-cli
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
              inherit fuzzers;

              debugBuild = packages.default.overrideAttrs (old: {
                name = "check-test";
                buildType = "debug";
                RUSTFLAGS = "-C debug-assertions";
              });

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
                cd ${./examples}/;
                for f in *.rcl; do
                  ${debugBuild}/bin/rcl evaluate $f
                done
                touch $out
                '';

              grammar = pkgs.runCommand
                "check-grammar"
                { buildInputs = [ pkgs.bison ]; }
                ''
                bison -Wcounterexamples,error=all ${./grammar/bison/grammar.y} --output $out
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

              fmtRcl = pkgs.runCommand
                "check-fmt-rcl"
                { buildInputs = [ debugBuild ]; }
                ''
                rcl format --check ${rclTomlSources}/**.rcl | tee $out
                '';

              buildRcl = pkgs.runCommand
                "check-rcl-build"
                { buildInputs = [ debugBuild ]; }
                ''
                rcl build --check --directory ${rclTomlSources} | tee $out
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
              inherit fuzzers-coverage rcl pyrcl treeSitterRcl website;

              default = rcl;
              wasm = rcl-wasm;

              coverage = pkgs.runCommand
                "rcl-coverage"
                { buildInputs = [ python pkgs.grcov ]; }
                ''
                export bintools=${pkgs.rustc.llvmPackages.bintools-unwrapped}/bin

                # Run the golden tests to generate the .profraw files.
                RCL_BIN=${coverageBuild}/bin/rcl python3 ${goldenSources}/run.py

                # Also run `rcl build` to make sure we cover that part of the application.
                ${coverageBuild}/bin/rcl build --check --directory ${rclTomlSources}

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
