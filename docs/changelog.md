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

## Unreleased

 * Add [`--banner`](rcl_evaluate.md#-banner-message) option to `rcl evaluate`
   and `rcl query`. This is helpful to include a prefix in generated files.

## 0.4.0

Released 2024-07-13.

 * Add [`List.map`](type_list.md#map),
   [`List.flat_map`](type_list.md#flat_map),
   [`List.filter`](type_list.md#filter),
   and [`List.sum`](type_list.md#sum) methods.
 * Add [`Set.map`](type_set.md#map),
   [`Set.flat_map`](type_set.md#flat_map),
   [`Set.filter`](type_set.md#filter),
   and [`Set.sum`](type_set.md#sum) methods.
 * Add support for [`--color=html`](rcl.md#-color-mode) to output
   <abbr>HTML</abbr> spans compatible with Pandoc.

## 0.3.0

Released 2024-06-23.

**Changes with compatibility impact:**

 * The output of the formatter can differ significantly. This may cause large
   diffs in codebases where formatting is enforced. See below for more details.
 * Certain syntactic constructs are no longer allowed inside the condition after
   `if` and collection after `for ... in`. If this causes a problem, simply wrap
   the condition or collection in parentheses. The stricter syntax ensures that
   these constructs can always be formatted in a reasonable way without loss in
   expressiveness.

Release highlights:

 * Add [union types](types.md#union-types).
 * The formatter now handles real-world code much better. In particular, it no
   longer puts things on one line as aggressively, it improves how chains of
   field lookups and method calls get line-wrapped, and the formatting of
   dictionaries is now consistent between `evaluate` and `format` output (both
   now use inner padding).
 * `rcl format` now supports [`--in-place`](rcl_format.md#-i-in-place) and
   [`--check`](rcl_format.md#-check). These were documented but marked to-do
   previously, now they are fully supported.
 * Errors about unserializable values now report more accurate source locations,
   and errors that emerge from dicts now report the dict key even when it is not
   a string.
 * The webassembly module can now output colored spans.
 * Fuzz testing is now more effient due to the addition of the [smith
   fuzzer](testing.md#fuzz-tests), and the fuzzers now test more invariants.

Independent of this release, <abbr>RCL</abbr> now has an official website:
<https://rcl-lang.org>.

## 0.2.0

Released 2024-03-15.

 * Add new methods on `String`:
   [`remove_prefix`](type_string.md#remove_prefix),
   [`remove_suffix`](type_string.md#remove_suffix),
   [`to_lowercase`](type_string.md#to_lowercase),
   [`to_uppercase`](type_string.md#to_uppercase).
 * Allow `\}` in strings as an escape sequence for `}`.
 * Fix a compatibility problem in the toml output format. Improve tests and add
   a fuzzer to rule out similar compatibility problems.
 * Output of `rcl format` is now colored when printing to a terminal.
 * The repository now includes a Tree-sitter grammar. This brings syntax
   highlighting to Helix and Neovim.

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
