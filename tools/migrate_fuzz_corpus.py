#!/usr/bin/env python3

"""
Migrate a fuzz corpus from the old format (letter at end) to the new format
(comment at the start).
"""

import hashlib
import os.path
import sys


def migrate(sample: bytes) -> bytes:
    if sample.endswith(b"a"):
        return b"//a\n" + sample[:-1]
    elif sample.endswith(b"b"):
        return b"//b\n" + sample[:-1]
    elif sample.endswith(b"c"):
        return b"//c\n" + sample[:-1]
    elif sample.endswith(b"d"):
        return b"//d\x42\n" + sample[:-1]
    elif sample.endswith(b"e"):
        return b"//e\x42\n" + sample[:-1]
    else:
        return b"//e\x42\n" + sample


def migrate_file(in_fname: str, out_dir: str) -> None:
    with open(in_fname, "rb") as f_in:
        sample_in = f_in.read()

    sample_out = migrate(sample_in)
    # Libfuzzer names the fuzz samples after their SHA1 hash.
    name = hashlib.sha1(sample_out).hexdigest()
    with open(os.path.join(out_dir, name), "wb") as f_out:
        f_out.write(sample_out)

    print(name, sample_out[:3].decode("ascii"))


def main(in_dir: str, out_dir: str) -> None:
    for fname in os.listdir(in_dir):
        migrate_file(os.path.join(in_dir, fname), out_dir)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
