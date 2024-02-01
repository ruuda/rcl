# RCL -- A reasonable configuration language.
# Copyright 2024 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from typing import Any, Union

def load_file(path: str) -> Any: ...
def loads(src: str) -> Any: ...
