# rcl evaluate

## Synopsis

    rcl evaluate <input>

Shorthands:

    rcl eval
    rcl e

## Description

Read an RCL expression from the file `<input>`, and evaluate it. When `<input>`
is `-`, read from stdin instead. Print the evaluated result as json to stdout.

## Options

TODO: This should take an `--output` option to switch between pretty json and
compact json. And possibly we should output yaml, HCL, ...
