// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

function append_span(node, class_, text) {
  if (class_ === "text") {
    node.append(text);
  } else {
    const span = document.createElement("span");
    span.className = class_;
    span.append(text);
    node.appendChild(span);
  }
}
