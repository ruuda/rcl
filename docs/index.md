# The RCL Configuration Language

RCL is a domain-specific language optimized for specifying human-written data
with just enough abstraction features to avoid repetition. It is a superset
of <abbr>JSON</abbr> that extends it into a simple functional programming
language that resembles [Python][python] and [Nix][nix]. Use cases include:

 * Querying <abbr>JSON</abbr> documents, like [`jq`][jq], but with a more
   familiar language.
 * Generating repetitive configuration files, such as GitHub Actions workflows
   or Terraform configuration.
 * Sharing configuration between tools that do not natively share data. For
   example, import the same user account definitions into Terraform, Tailscale,
   Kubernetes, and Ansible.

RCL can be used through [the `rcl` command-line tool][rcl] that can export
documents to <abbr>JSON</abbr>, <abbr>YAML</abbr>, <abbr>TOML</abbr>, and [other
formats][output]. It can also be used through [a native Python module][pythonm],
with an interface similar to the `json` module.

RCL is a hobby project without stability promise.

[jq]:      https://jqlang.github.io/jq/manual/
[nix]:     https://nixos.org/manual/nix/stable/language/
[output]:  rcl_evaluate.md#-f-format-format
[python]:  https://www.python.org/
[pythonm]: python_bindings.md
[rcl]:     rcl.md

## Demo

For an interactive demo in your browser, see <https://rcl-lang.org>.

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
    for period, days in retention_days:
    {
      name = f"{database}-{period}",
      region = "eu-west",
      lifecycle_policy = { delete_after_seconds = days * 24 * 3600 },
    }
  ],
}
```
