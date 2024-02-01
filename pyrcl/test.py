# RCL -- A reasonable configuration language.
# Copyright 2024 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.
# test

# This file tests the RCL Python module.

import rcl

# Test that all the primitive types get converted correctly.
assert rcl.loads("10 + 32") == 42
assert rcl.loads("null") == None
assert rcl.loads("false") == False
assert rcl.loads("true") == True
assert rcl.loads('["foobar"]') == ["foobar"]
assert rcl.loads("{1, 2, 3}") == {1, 2, 3}
assert rcl.loads('{1: "one", "two": 2}') == {1: "one", "two": 2}

# Test that loading files works. This needs to be executed from this directory.
assert rcl.load_file("test.rcl") == {
    "name": "Import Test Data",
    "description": "This is only here to test `rcl.load_file` in Python.",
}
