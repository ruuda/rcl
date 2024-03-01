# Changelog

## Versioning policy

RCL versions are named `MAJOR.MINOR.PATCH`.

 * The major version number is purely cosmetic and represents the author’s
   sentiment about feature-completeness and stability.
 * The minor version is bumped for new features and changes that are not bugfixes.
 * The patch version is bumped for bugfixes.

The version number is not a [semantic version][semver]. Changes that have
compatibility impact will be clearly marked as such in the changelog.

[semver]: https://semver.org/

## Next

Unreleased.

 * Add new methods on `String`: `remove_prefix`, `remove_suffix`, `to_lowercase`,
   `to_uppercase`.
 * Fix a compatibility problem in the toml output format. Improve tests and add
   a fuzzer to rule out similar compatibility problems.

## 0.1.0

Released 2024-02-26.

This is the initial tag. I have been working on this project for almost 7 months
now, and pieces of <abbr>RCL</abbr> code have made it into various repositories.
I keep making breaking changes, so it is useful to be able to say “this code is
compatible with <abbr>RCL</abbr> version x.y.z”. Also to update such code, and
to reflect, it is useful to have a list of changes at a coarser level than the
Git history. This point is also a good point for a release, shortly after
writing [a blog post][blogpost] about <abbr>RCL</abbr>, and shortly after
merging a static typechecker that I am happy with.

RCL is by no means ready or stable now — I have many more ideas for features and
changes. But let’s start giving revisions a version number.

[blogpost]: https://ruudvanasseldonk.com/2024/a-reasonable-configuration-language
