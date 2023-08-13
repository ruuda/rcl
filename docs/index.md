# RCL

Ruud’s Configuration Language, RCL for short, is a domain-specific language
optimized for specifing human-written data with just enough abstraction features
to avoid repetition.

_Vaporware warning:
This is a proof-of-concept toy project. I will probably lose interest in it
before it reaches a point where it is usable and useful. Much of the content in
this manual is hypothetical._

## Example

    // TODO: Add a better example.
    let sevices_at = {
      server01 = ["ssh", "http"];
      server02 = ["ssh", "imap"];
      server03 = ["ssh", "pop3"];
    };
    let ports_for = {
      ssh = [22];
      http = [80, 443];
      imap = [993];
      pop3 = [995];
    };
    let firewall_rules = {
      for server, services in services_at:
      server: [
        for service in services:
        for port in ports_for[service]:
        { rule = "allow"; port = port; }
      ]
    };
    firewall_rules

