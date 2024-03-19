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
    // In error messages we use box drawing characters, but Chromium
    // does not display the long one in the correct weight, so replace
    // it with a normal pipe, which renders better.
    span.append(
      (class_ === "error") || (class_ === "warning")
      ? text.replace("│", "|")
      : text
    );
    node.appendChild(span);
  }
}
