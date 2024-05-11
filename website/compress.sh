#!/usr/bin/sh

set -euo pipefail

for f in index.html rcl.js rcl_bg.wasm vultr.json favicon.svg; do
  brotli --force -9 $f
done
