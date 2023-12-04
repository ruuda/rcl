# Tutorial

The main purpose of <abbr>RCL</abbr> is to reduce boilerplate in configuration.
In this tutorial we will explore that use case through an example: defining
cloud storage buckets for backups.

## Setting

In this tutorial we have two databases that need to be backed up to cloud
storage: Alpha and Bravo. For both of them, we want to define three buckets: one
for hourly, one for daily, and one for monthly backups. Each of them should have
a lifecycle policy that deletes objects after 4, 30, and 365 days respectively.

Furthermore, let’s say we have a script or tool that can set up the buckets from
a <abbr>JSON</abbr> configuration file. That tool might be [Terraform][terraform]
in practice, but in this tutorial we’ll assume a simpler schema to avoid
distractions.

[terraform]: https://www.terraform.io/

The configuration file that defines our buckets might look like this:

```yaml
{
  "buckets": [
    {
      "name": "alpha_hourly",
      "region": "eu-west",
      "lifecycle_policy": {
        "delete_after_seconds": 345600
      }
    },
    {
      "name": "alpha_daily",
      "region": "eu-west",
      "lifecycle_policy": {
        "delete_after_seconds": 2592000
      }
    },
    {
      "name": "alpha_monthly",
      "region": "eu-west",
      "lifecycle_policy": {
        "delete_after_seconds": 31536000
      }
    },
    {
      "name": "bravo_hourly",
      "region": "eu-west",
      "lifecycle_policy": {
        "delete_after_seconds": 34560
      }
    },
    {
      "name": "bravo_daily",
      "region": "us-west",
      "lifecycle_policy": {
        "delete_after_seconds": 2592000
      }
    },
    {
      "name": "bravo_monthly",
      "region": "eu-west",
      "lifecycle_policy": {
        "delete_after_seconds": 31536000
      }
    }
  ]
}
```

A configuration file like this is suboptimal for several reasons. It is
repetitive, difficult to read, and error-prone to edit. In fact, the above
example contains two bugs that may not be obvious:

 * The `bravo_daily` bucket is located in `us-west` rather than `eu-west` like
   the other buckets.
 * The `delete_after_seconds` of `bravo_hourly` is missing a zero and keeps
   objects for only 10 hours, instead of the intended 4 days.

We're going to improve this by rewriting the configuration in <abbr>RCL</abbr>.

## Installation

Before we can start, follow the [installation instructions](installation.md),
and if you like, [set up syntax highlighting](syntax_highlighting.md) for your
editor.
