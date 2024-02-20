# RCL

Ruud’s Configuration Language, <abbr>RCL</abbr> for short, is a domain-specific
language optimized for specifying human-written data with just enough abstraction
features to avoid repetition. It is a superset of json that extends it into a
simple functional programming language that resembles [Python][python] and
[Nix][nix]. Use cases include:

 * Querying json documents, like [`jq`][jq], but with a more familiar language.
 * Generating repetitive configuration files, such as GitHub Actions workflows
   or Terraform configuration.
 * Enabling large repositories to split configuration into small reusable pieces
   that can be referenced from a single consistent entry point, in the same way
   that Nix enables this for [Nixpkgs][nixpkgs].
 * Sharing configuration between tools that do not natively share data. For
   example, import the same user account definitions into Terraform, Tailscale,
   Kubernetes, and Ansible.

RCL can be used through the `rcl` command-line tool that can export documents
to <abbr>JSON</abbr> and [other formats][output]. It can also be used through
a native Python module, with an interface similar to the `json` module.

[python]:  https://www.python.org/
[nix]:     https://nixos.org/manual/nix/stable/language/
[jq]:      https://jqlang.github.io/jq/manual/
[nixpkgs]: https://github.com/nixos/nixpkgs
[output]:  rcl_evaluate.md#-o-output-format

_**Warning:** While RCL is usable, it is still in an early exploratory stage
with frequent breaking changes. This is a hobby project without stability
promise._

## Example

```rcl
// Configuration that defines cloud storage buckets for storing backups of two
// databases, "alpha" and "bravo", at three different frequencies. This schema
// is for a hypothetical infrastructure-as-code tool. See the tutorial for more
// details.
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
