// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

module.exports = grammar({
  name: 'rcl',

  rules: {
    source_file: $ => $._prefixed_expr,

    // Hmm, do I really need to define an external scanner already? ...
    // Ah wait no, only for some tokens. I need to look into this ...
    _prefixed_expr: $ => "TODO",
  }
});
