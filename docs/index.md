# RCL

Ruud’s Configuration Language, RCL for short, is a domain-specific language
optimized for specifying human-written data with just enough abstraction
features to avoid repetition. It is a superset of json that extends it into a
simple functional programming language that resembles [Python][python] and
[Nix][nix]. Use cases include:

 * Querying json documents, like [`jq`][jq], but with a more familiar language.
 * Generating repetitive configuration files, such as GitHub Actions workflows
   or Terraform configuration.
 * Enabling large repositories to split configuration into small reusable pieces
   that can be referenced from a single consistent entry point, in the same way
   that Nix enables this for [Nixpkgs][nixpkgs].

[python]:  https://www.python.org/
[nix]:     https://nixos.org/manual/nix/stable/language/
[jq]:      https://jqlang.github.io/jq/manual/
[nixpkgs]: https://github.com/nixos/nixpkgs

_**Warning:** While RCL is usable, it is still in an early exploratory stage
with frequent breaking changes. This is a hobby project without stability
promise._

## Example

```rcl
// Configuration for a hypothetical infrastructure-as-code schema.
{
  backup_buckets = [
    let retention_days = { hourly = 4, daily = 30, monthly = 365 };
    for database in ["alpha", "bravo"]:
    for period, days in retention_days: {
      name = f"{database}-{period}",
      region = "eu-west",
      lifecycle_policy = { delete_after_seconds = days * 24 * 3600 },
    }
  ],
}
```
