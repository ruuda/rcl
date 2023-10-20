# RCL

Ruud’s Configuration Language, RCL for short, is a domain-specific language
optimized for specifying human-written data with just enough abstraction
features to avoid repetition. It is a superset of json that extends it into a
simple functional programming language that resembles Python and [Nix][nix]. Use
cases include:

 * Querying json documents, like [`jq`][jq], but with a more familiar language.
 * Generating repetitive configuration files, such as GitHub Actions workflows
   or Terraform configuration.
 * Enabling large repositories to split configuration into small reusable pieces
   that can be referenced from a single consistent entry point, in the same way
   that Nix enables this for [Nixpkgs][nixpkgs].

[nix]:     https://nixos.org/manual/nix/stable/language/
[jq]:      https://jqlang.github.io/jq/manual/
[nixpkgs]: https://github.com/nixos/nixpkgs

_Vaporware warning:
This is a proof-of-concept toy project. I will likely lose interest in it before
it is mature enough for serious use. Some of the content in this manual is
hypothetical._

## Example

```rcl
 // TODO: Add a better example.
 let sevices_at = {
   server01 = ["ssh", "http"],
   server02 = ["ssh", "imap"],
   server03 = ["ssh", "pop3"],
 };
 let ports_for = {
   ssh = [22],
   http = [80, 443],
   imap = [993],
   pop3 = [995],
 };
 let firewall_rules = {
   for server, services in services_at:
   server: [
     for service in services:
     for port in ports_for[service]:
     { rule = "allow", port = port }
   ]
 };
 firewall_rules
```
