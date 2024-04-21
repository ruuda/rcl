#!/usr/bin/env python3

"""
Expand the fuzz corpus for the `main` fuzzer such that every input has a version
for every fuzz mode.
"""

import hashlib
import os.path
import sys


def expand_file(in_fname: str, out_dir: str) -> None:
    with open(in_fname, "rb") as f_in:
        sample_in = f_in.read()

    if not sample_in.startswith(b"//"):
        return

    nlpos = sample_in.find(b"\n")

    for mode in b"ajkt":
        # We set the width parameter to 100 on all samples. The fuzzer may
        # discover other variants but they shouldn’t matter so much.
        sample_out = (
            b"//"
            + mode.to_bytes(length=1, byteorder="little")
            + b"n\n"
            + sample_in[nlpos + 1 :]
        )

        # Libfuzzer names the fuzz samples after their SHA1 hash.
        name = hashlib.sha1(sample_out).hexdigest()
        with open(os.path.join(out_dir, name), "wb") as f_out:
            f_out.write(sample_out)

    print(name, sample_out[:3].decode("ascii"))


def main(in_dir: str, out_dir: str) -> None:
    for fname in os.listdir(in_dir):
        expand_file(os.path.join(in_dir, fname), out_dir)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
