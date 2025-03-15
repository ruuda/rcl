# RCL -- A reasonable configuration language.
# Copyright 2023 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

# This is a Pygments lexer that can be dropped into a Pygments source tree to
# add support for RCL. See also docs/syntax_highlighting.md.

# To test this lexer:
# python -m pygments -x -l grammar/pygments/rcl.py:RclLexer examples/tags.rcl
# See also <https://pygments.org/docs/lexerdevelopment/>.

from pygments.lexer import RegexLexer, words
from pygments import token

__all__ = ["RclLexer"]

KEYWORDS = (
    "and",
    "assert",
    "else",
    "false",
    "for",
    "if",
    "import",
    "in",
    "let",
    "not",
    "null",
    "or",
    "trace",
    "true",
)

BUILTINS = (
    # Methods
    "all",
    "any",
    "chars",
    "contains",
    "ends_with",
    "enumerate",
    "except",
    "filter",
    "flat_map",
    "fold",
    "get",
    "group_by",
    "join",
    "key_by",
    "keys",
    "len",
    "map",
    "parse_int",
    "remove_prefix",
    "remove_suffix",
    "replace",
    "reverse",
    "round",
    "sort",
    "sort_by",
    "split",
    "split_lines",
    "starts_with",
    "sum",
    "to_lowercase",
    "to_uppercase",
    "values",
    # std and its members
    "empty_set",
    "range",
    "read_file_utf8",
    "std",
)

TYPES = (
    "Any",
    "Bool",
    "Collection",
    "Dict",
    "List",
    "Null",
    "Number",
    "Set",
    "String",
    "Union",
    "Void",
)

# Due to the handling of string interpolation, we have a few states that are
# almost identical to the root state, but differ in the handling of `}`.
# Therefore we extract the common tokens here.
_root_base = [
    (r"#!.*?$", token.Comment.Hashbang),
    (r"//.*?$", token.Comment),
    (r'f"""', token.String, "format_triple"),
    (r'"""', token.String, "string_triple"),
    (r'f"', token.String, "format_double"),
    (r'"', token.String, "string_double"),
    # Note, the `}` we handle in each of the specializations of this state.
    (r"0b[01_]+", token.Number.Bin),
    (r"0x[0-9a-fA-F_]+", token.Number.Hex),
    (r"[0-9_]+(\.[0-9_]+)?([eE][+-]?[0-9_]+)?", token.Number),
    # In the Rust lexer, there is one state for identifiers, and in
    # there we recognize the keywords. We don't recognize builtins at
    # all in the Rust lexer. Here, we do split all those out by token
    # type.
    (
        words(KEYWORDS, suffix=r"\b"),
        token.Keyword,
    ),
    (
        words(BUILTINS, suffix=r"\b"),
        token.Name.Builtin,
    ),
    (
        words(TYPES, suffix=r"\b"),
        token.Keyword.Type,
    ),
    (r"[_a-z][_a-z0-9-]*", token.Name),
    # There is a dedicated whitespace token, but if we use it, the html output
    # (and console output too) gets very polluted, so make whitespace a regular
    # unclassified token.
    (r"\s+", token.Token),
    # In the Rust lexer the punctuation is split out, and then further
    # into digraphs and monographs. Here we instead split them out by
    # token type.
    (r"<=|>=|==|!=|=>|<|>|\+|-|\*|/|\|", token.Operator),
    (r"[)(\]\[=,.:;]", token.Token),
    (r"{", token.Token, "in_brace"),
    (r"#", token.Error),
]


class RclLexer(RegexLexer):
    """
    This lexer should mirror the lexer in src/lexer.rs.
    """

    name = "RCL"
    aliases = ["rcl"]
    filenames = ["*.rcl"]

    tokens = {
        # The root state here corresponds to `Lexer::next` in Rust.
        "root": [
            (r"}", token.Error),
            *_root_base,
        ],
        "in_brace": [
            (r"}", token.Token, "#pop"),
            *_root_base,
        ],
        "in_interpolation": [
            (r"}", token.String.Interpol, "#pop"),
            *_root_base,
        ],
        "format_double": [
            (r'[^\\"{]+', token.String),
            (r'"', token.String, "#pop"),
            (r"{", token.String.Interpol, "in_interpolation"),
            (r"\\", token.String.Escape, "escape"),
        ],
        "format_triple": [
            (r'[^\\"{]+', token.String),
            (r'"""', token.String, "#pop"),
            # Only """ ends the string, but " cannot occur in the above state,
            # so list those two explicitly.
            (r'""|"', token.String),
            (r"\\", token.String.Escape, "escape"),
            (r"{", token.String.Interpol, "in_interpolation"),
        ],
        "string_double": [
            (r'[^\\"]+', token.String),
            (r'"', token.String, "#pop"),
            (r"\\", token.String.Escape, "escape"),
        ],
        "string_triple": [
            (r'[^\\"]+', token.String),
            (r'"""', token.String, "#pop"),
            # Only """ ends the string, but " cannot occur in the above state,
            # so list those two explicitly.
            (r'""|"', token.String),
            (r"\\", token.String.Escape, "escape"),
        ],
        "escape": [
            (r'["\\/bfnrt}{]', token.String.Escape, "#pop"),
            (r"u\{[0-9a-fA-F]+\}", token.String.Escape, "#pop"),
            (r"u[0-9a-fA-F]{4}", token.String.Escape, "#pop"),
        ],
    }
