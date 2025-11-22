# Releasing

This chapter describes how to release new versions of <abbr>RCL</abbr>. It is
intended primarily as a playbook/checklist for the author. Ideally, more of
these steps would be automated over time.

## Preparation

### Ensure the changelog is up to date

Check the Git log to confirm that all user-facing changes are documented in the
changelog. Add the release date.

### Bump the version

 * Update the version in `Cargo.rcl`, run `rcl build` to update generated files.
 * Run `cargo check --all` to update lockfiles.
 * Update version references in `docs/`.
 * Double-check the diff against a previous version bump commit.
 * Commit the version bump.

## Verification

We do this after bumping the version to ensure that we verify the final state
of the repository. If anything fails, fix it, and redo the version bump.

### Ensure generated files are up to date

```console
rcl build
tools/generate_keywords.py
```

### Verify that everything builds

This is verified on <abbr>CI</abbr> by [Garnix], so pushing to GitHub and
waiting for Garnix works, but it can also be checked locally. `nix build` and
`nix flake check` are insufficient; as a workaround to [Nix issue 7165][#7165],
we can build all flake attributes like so:

```console
nix flake show --json | rcl rq '{
  for k in ["packages", "checks", "devShells"]:
  for p in input[k].x86_64-linux.keys():
  f".#{k}.x86_64-linux.{p}"
}' | xargs nix build --no-link
```

[Garnix]: https://garnix.io/repo/ruuda/rcl
[#7165]:  https://github.com/NixOS/nix/issues/7165

### Ensure fuzzers are converged

Run every [fuzzer](testing.md#fuzz-tests) until it no longer discovers new code
paths. This should already have been done during feature development, but it’s
good to confirm:

    tools/fuzz_all.py

### Check dependency licenses

Ensure no problematic dependencies snuck in:

    tools/check_deps.py

It would be nice to turn this into a Nix flake check, but unfortunately that’s
not easy due to tools downloading crate metadata on the fly.

Also, verify that dependencies listed in `rcl --about` match the crate.

## Publishing

### Repository

 * Create a signed, annotated, Git tag.
 * Push to GitHub and Codeberg.

### Documentation and website

```console
mkdocs gh-deploy --remote-name «webserver»
nix build .#website
rsync -av $(nix path-info .#website)/ «webserver»:/var/www/rcl-lang.org/
```

### Pypi

```console
nix build .#pyrcl-wheel
twine upload $(nix path-info .#pyrcl-wheel)/*
```

### GitHub

We host the prebuilt binaries on GitHub. Build them:

```console
nix build .#binaries
```

Then create a release on GitHub and attach them.

### Extensions

For releases that have impact on the grammars or extensions:

 * Update external repositories: `tools/update_repos.py`.
 * Push to GitHub and Codeberg.
 * Make a pull request to update [zed-industries/extensions][zed-ext].

[zed-ext]: https://github.com/zed-industries/extensions

### Arch User Repository

 * Bump `pkgver` in the `PGKBUILD`.
 * Update shasums: `makepkg --geninteg >> PKGBUILD`, edit file.
 * Update srcinfo: `makepkg --printsrcinfo > .SRCINFO`.
 * Confirm that the package builds: `makepkg --install`.
 * Commit and push.
