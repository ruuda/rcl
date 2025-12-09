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

 * Add [`Set.transitive_closure`](type_set.md#transitive_closure), which is
   useful for flattening trees.

## 0.11.0

Released 2025-11-23.

 * Add [unpack](syntax.md#unpack): `..` and `...` syntax that can be used inside
   lists, sets, and dicts to unpack one collection into another.
 * The `|` operator is now deprecated, and will be removed in a future version.
   The new unpack feature can be used to express dict and set unions instead.
 * [Assertions](syntax.md#assertions) now use a `:` to separate the condition
   and the message: `assert cond: "message";` rather than
   `assert cond, "message";`. For compatibility, <abbr>RCL</abbr> still accepts
   a comma for now. The formatter will automatically update documents to the new
   syntax. The standard format is now more compact for multi-line assertions.
 * A new CLI option, `--about`, prints license and dependency information.
 * [Prebuilt binaries](installation.md#prebuilt-binaries) are now available for
   several platforms. They are hosted [on Github](https://github.com/ruuda/rcl/releases).

The changes and deprecations are backwards compatible. If a future version
removes compatibility with the older syntax, that will be clearly marked in the
changelog.

## 0.10.0

Released 2025-08-30.

Command-line:

* The new [`rcl patch`](rcl_patch.md) command enables automation to safely edit
  <abbr>RCL</abbr> documents.

Language:

 * Add [`String.parse_number`](type_string.md#parse_number) method.
 * Comments are now allowed in more places.

Python module:

 * The Python module is now published to Pypi as [`rcl-lang`][pypi-rcl], see
   also the updated [installation instructions](installation.md#python-module).

[pypi-rcl]: https://pypi.org/project/rcl-lang/

Plugins:

 * Syntax highlighters and editor plugins now highlight the `#!`-line.
 * Update the Zed extension to leverage Tree-sitter-based bracket matching and
   indentation.
 * Update the [Helix configuration](syntax_highlighting.md#helix) to include
   `comment-tokens`.

Bugfixes:

 * Document the `--check` option in the help output of `rcl build` and
   `rcl format`. Previously they were only documented in the manual.
 * Numbers with exponent 0, but without decimal dot, now format as integers
   without exponent, rather than as numbers with decimal dot. For example,
   `1e0` now formats as `1` rather than `0.1e1`.
 * Correct the error message for empty `0b` and `0x` literals.

Compatibility:

 * RCL is now developed against Rust 1.75, up from 1.70 before. Compatibility
   with toolchains older than 1.75 is not guaranteed.

Note, the Git tag is signed with [a different key][gpg-2025] than previous tags.

[gpg-2025]: https://ruudvanasseldonk.com/2025/rotating-my-pgp-key

## 0.9.0

Released 2025-07-10.

 * Add [`std.format_json`](stdlib.md#format_json) function.
 * Add support for the [json-lines output format](rcl_evaluate.md#-f-format-format).

## 0.8.0

Released 2025-03-02.

**Changes with compatibility impact:**

 * The `Int` type has been removed in favor of the `Number` type. The behavior
   of integer values remains unchanged, but type annotations need to be updated.

Release highlights:

 * Add support for numbers with a decimal point or exponent. This finally makes
   <abbr>RCL</abbr> a proper json superset. See the [_numbers_](numbers.md)
   chapter in the language reference for more details. Arithmetic on integers
   remains unchanged. Arithmetic on numbers with a decimal point is now possible
   as well, but for multiplication, formatting of the result is still fairly
   naive. This will likely change in a future version.
 * Add [`Number.round`](type_number.md#round) method.
 * Add [`List.sort_by`](type_list.md#sort_by) and
   [`Set.sort_by`](type_set.md#sort_by) methods.
 * Add [`Set.sort`](type_set.md#sort) method for symmetry with `List`.
 * When running `rcl` without arguments, it now prints only the most important
   parts of the help text. Use `rcl --help` for the full command reference.

## 0.7.0

Released 2024-12-31.

 * Add [`std.empty_set`](stdlib.md#empty_set) constant.
 * Add [`List.sort`](type_list.md#sort) method.
 * Add [`List.all`](type_list.md#all), [`List.any`](type_list.md#any),
   [`Set.all`](type_set.md#all), and [`Set.any`](type_set.md#any) methods.
 * Comparisons (`<`, `<=`, `>`, `>=`) are now allowed between any two values,
   not just integers. In particular this enables comparing strings.
   This behavior is not final; comparisons between values of different types are
   allowed for now, but this might be tightened in the future.
 * Add [`--check`](rcl_build.md#-check) mode to `rcl build`, to confirm that
   generated files are up to date.
 * Bugfix: `rcl highlight` now respects `--color` again.

Thanks to Sergey Mishin for contributing to this release.

## 0.6.0

Released 2024-12-01.

 * If-else expressions now optionally accept a colon after `else`, and this is
   the new recommended form used by the autoformatter. What used to be
   `if cond: then-expr else else-expr` is now `if cond: then-expr else: else-expr`.
   For compatibility the colon is optional for now. It will likely become
   mandatory in a future release. In that case it will be clearly marked as
   a change with compatibility impact in the release notes.
 * Add `rcl re` as a shorthand for `rcl evaluate --format=raw` and `rcl rq` as
   a shorthand for `rcl query --format=raw`.
 * An [extension for the Zed editor](syntax_highlighting.md#zed) is now available.
 * Ensure compatibility with Rust 1.77 and later. While <abbr>RCL</abbr> could
   already be compiled with these versions, [a breaking change in 1.77][rust-align]
   made the tests fail, and impacted memory usage.

Thanks to Dennis Frenken and areskill for contributing to this release.

[rust-align]: https://blog.rust-lang.org/2024/03/30/i128-layout-update.html

## 0.5.0

Released 2024-07-28.

 * Add a new [`build` subcommand](rcl_build.md) that acts as a built-in,
   lightweight alternative to a full build system, for updating generated files.
 * Add [`--banner`](rcl_evaluate.md#-banner-message) option to `rcl evaluate`
   and `rcl query`. This is helpful to include a prefix in generated files.

## 0.4.0

Released 2024-07-13.

 * Add methods on `List` and `Set` that can act as alternatives to list
   comprehensions. These are especially useful for writing queries for
   [`rcl query`](rcl_query.md) on the command line, because you don’t
   have to move the cursor to insert `[` in the middle of an expression
   to wrap something in a comprehension.
   New methods:
   [`List.map`](type_list.md#map),
   [`List.flat_map`](type_list.md#flat_map),
   [`List.filter`](type_list.md#filter),
   [`Set.map`](type_set.md#map),
   [`Set.flat_map`](type_set.md#flat_map), and
   [`Set.filter`](type_set.md#filter).
 * Add a `sum` method on `List` and `Set` to sum integers. Summing was already
   possible with `fold`, but `sum` makes it a lot more ergonomic.
   New methods:
   [`List.sum`](type_list.md#sum)
   and [`Set.sum`](type_set.md#sum).
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
