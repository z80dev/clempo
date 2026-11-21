#!/usr/bin/env python3
"""Export an inclusive CLBK archive range as concatenated raw block RLP."""
import argparse
import os
from pathlib import Path
import struct
from tempfile import NamedTemporaryFile

CHUNK_SIZE = 10_000


def export(archive, first, last, output):
    with NamedTemporaryFile(dir=output.parent, delete=False) as stream:
        temp = Path(stream.name)
        try:
            for base in range(first // CHUNK_SIZE * CHUNK_SIZE, last + 1, CHUNK_SIZE):
                path = archive / f"chunk-{base:010d}.blk"
                with path.open("rb") as chunk:
                    magic, number, count = struct.unpack(">4sQI", chunk.read(16))
                    lo, hi = max(first - base, 0), min(last - base + 1, CHUNK_SIZE)
                    if magic != b"CLBK" or number != base or not lo < hi <= count <= CHUNK_SIZE:
                        raise ValueError(f"invalid chunk or missing block: {path}")
                    chunk.seek(16 + 8 * lo)
                    start = struct.unpack(">Q", chunk.read(8))[0]
                    chunk.seek(16 + 8 * hi)
                    end = struct.unpack(">Q", chunk.read(8))[0]
                    data = 16 + 8 * (count + 1)
                    if not start < end <= os.fstat(chunk.fileno()).st_size - data:
                        raise ValueError(f"invalid payload bounds: {path}")
                    chunk.seek(data + start)
                    remaining = end - start
                    while remaining:
                        batch = chunk.read(min(remaining, 1024 * 1024))
                        if not batch:
                            raise ValueError(f"truncated chunk: {path}")
                        stream.write(batch)
                        remaining -= len(batch)
            stream.close()
            temp.replace(output)
        finally:
            temp.unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--archive", type=Path, default=Path.home() / "cache/clempo/mainnet")
    parser.add_argument("--from", type=int, required=True, dest="first")
    parser.add_argument("--to", type=int, required=True, dest="last")
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    if not 0 <= args.first <= args.last:
        parser.error("expected 0 <= --from <= --to")
    export(args.archive, args.first, args.last, args.out)


if __name__ == "__main__":
    main()
