#!/usr/bin/env python3
"""Sample committed head and live state using read-only RPC requests."""
import argparse
import json
import time
import urllib.request

HISTORY = '0x0000f90827f1c53a10cb7a02335b175320002935'


def call(url, method, *params):
    body = json.dumps({'jsonrpc': '2.0', 'id': 1, 'method': method, 'params': params}).encode()
    req = urllib.request.Request(url, data=body, headers={'Content-Type': 'application/json'})
    with urllib.request.urlopen(req, timeout=3) as response:
        result = json.load(response)
    if 'error' in result:
        raise ValueError(f'{method}: {result["error"]}')
    return result['result']


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('node')
    parser.add_argument('upstream')
    args = parser.parse_args()
    while True:
        start = time.monotonic()
        sample = {'at': time.time(), 'monotonic': start}
        try:
            head = call(args.node, 'eth_getBlockByNumber', 'latest', False)
            number = head['number']
            sample['head'] = int(number, 16)
            slot = hex((sample['head'] - 1) % 8191)
            stored = call(args.node, 'eth_getStorageAt', HISTORY, slot, number)
            remote = call(args.upstream, 'eth_blockNumber')
            sample['upstream'] = int(remote, 16)
            sample['lag'] = sample['upstream'] - sample['head']
            canonical = call(args.upstream, 'eth_getBlockByNumber', number, False)
            sample['canonical'] = all(head[k] == canonical[k] for k in ('hash', 'stateRoot'))
            ref_stored = call(args.upstream, 'eth_getStorageAt', HISTORY, slot, number)
            sample['state_ok'] = stored == ref_stored == head['parentHash']
        except (OSError, ValueError, KeyError, TypeError) as error:
            sample['error'] = str(error)
        print(json.dumps(sample, separators=(',', ':')), flush=True)
        time.sleep(max(0.0, 5 - (time.monotonic() - start)))


if __name__ == '__main__':
    main()
