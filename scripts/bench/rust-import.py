#!/usr/bin/env python3
"""Measure tempo import on an isolated, disposable datadir at the exact parent."""
import argparse
from datetime import datetime
import json
import mmap
import os
from pathlib import Path
import re
import subprocess
import sys
import time


def item(data, pos):
    """Return payload start, end and list flag of a canonical RLP item."""
    prefix = data[pos]
    if prefix < 0x80:
        return pos, pos + 1, False
    is_list = prefix >= 0xC0
    length = prefix - (0xC0 if is_list else 0x80)
    start = pos + 1
    if length >= 56:
        stop = start + length - 55
        length = int.from_bytes(data[start:stop], 'big')
        start = stop
    return start, start + length, is_list


def corpus_gas(path, first, last):
    with path.open('rb') as stream, mmap.mmap(stream.fileno(), 0, access=mmap.ACCESS_READ) as data:
        end = gas = 0
        for height in range(first, last + 1):
            block, end, block_list = item(data, end)
            field, header_end, header_list = item(data, block)
            for _ in range(3):  # Tempo gas limits and timestamp milliseconds
                _, field, _ = item(data, field)
            field, eth_end, eth_list = item(data, field)
            for _ in range(8):
                _, field, _ = item(data, field)
            number, next_field, _ = item(data, field)
            _, gas_pos, _ = item(data, next_field)
            value, stop, _ = item(data, gas_pos)
            if not (block_list and header_list and eth_list and stop <= eth_end <= header_end <= end <= len(data)
                    and int.from_bytes(data[number:next_field], 'big') == height):
                raise ValueError(f'wrong block or invalid header at height {height}')
            gas += int.from_bytes(data[value:stop], 'big')
        if end != len(data):
            raise ValueError('corpus has blocks beyond --to')
        return gas


STAGE = re.compile(r'^(\S+).*? (Executing|Finished) stage .*?stage=(\w+) checkpoint=(\d+)')


def stage_summary(path):
    started, totals, terminal = {}, {}, {}
    with path.open() as stream:
        for line in stream:
            match = STAGE.match(line)
            if match:
                stamp, event, stage, height = match.groups()
                stamp = datetime.fromisoformat(stamp.replace('Z', '+00:00'))
                if event == 'Executing':
                    started.setdefault(stage, stamp)
                else:
                    totals[stage] = totals.get(stage, 0) + (stamp - started.pop(stage)).total_seconds()
                    terminal[stage] = int(height)
    return totals, terminal

def check_parent(tempo, root, expected):
    # Finish unwinds first, so Finish alone can also describe a partial unwind.
    stages = ('Headers', 'Bodies', 'SenderRecovery', 'Execution', 'MerkleUnwind',
              'AccountHashing', 'StorageHashing', 'MerkleExecute', 'TransactionLookup',
              'IndexStorageHistory', 'IndexAccountHistory', 'Prune', 'Finish')
    text = subprocess.check_output([tempo, '--color', 'never', 'db', '--datadir', root,
                                    'stage-checkpoints', 'get'], text=True)
    heights = {name: int(height) for name, height in re.findall(
        r'^(\w+): StageCheckpoint \{ block_number: (\d+)', text, re.M)}
    mismatches = [f'{stage}={heights.get(stage)}' for stage in stages
                  if heights.get(stage, 0 if expected == 0 else None) != expected]
    if mismatches:
        raise ValueError(f'datadir has not fully reached parent {expected}: {", ".join(mismatches)}')



def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--tempo', type=Path, required=True)
    parser.add_argument('--datadir', type=Path, required=True, help='disposable working datadir; import mutates it')
    parser.add_argument('--from', type=int, required=True, dest='first')
    parser.add_argument('--to', type=int, required=True, dest='last')
    parser.add_argument('--rlp', type=Path, required=True)
    parser.add_argument('--log', type=Path, required=True)
    parser.add_argument('--out', type=Path, required=True)
    args = parser.parse_args()
    if not 1 <= args.first <= args.last:
        parser.error('expected 1 <= --from <= --to')
    root = args.datadir.resolve()
    if args.first != 1 or root.exists():
        try:
            check_parent(args.tempo, root, args.first - 1)
        except ValueError as error:
            parser.error(str(error))
    gas = corpus_gas(args.rlp, args.first, args.last)
    command = [str(args.tempo), 'import', '--datadir', str(root), '--chain', 'mainnet',
               '--fail-on-invalid-block', '--color', 'never', '--log.file.max-files', '0', str(args.rlp)]
    with args.log.open('xb') as log:
        start = time.monotonic()
        child = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT)
        _, status, usage = os.wait4(child.pid, 0)
        child.returncode = os.waitstatus_to_exitcode(status)
        wall = time.monotonic() - start
    stages, terminal = stage_summary(args.log)
    count = args.last - args.first + 1
    verified = terminal.get('MerkleExecute') == terminal.get('Finish') == args.last
    success = child.returncode == 0 and verified
    out = dict(command=command, first=args.first, last=args.last, stage_seconds=stages,
               terminal_checkpoints=terminal, terminal_root_verified=verified, complete=success,
               status=child.returncode, seconds=wall, cpu_seconds=usage.ru_utime + usage.ru_stime,
               peak_rss_bytes=usage.ru_maxrss * (1 if sys.platform == 'darwin' else 1024),
               blocks=count if success else None, gas=gas if success else None,
               blocks_per_s=count / wall if success else None, mgas_per_s=gas / wall / 1e6 if success else None)
    args.out.write_text(json.dumps(out, indent=2) + '\n')
    return 0 if success else 1


if __name__ == '__main__':
    sys.exit(main())
