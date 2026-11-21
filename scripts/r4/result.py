#!/usr/bin/env python3
"""Grade a recorded R4 run (see GOAL.md for the pass criteria)."""
import json
import pathlib
import sys


def grade(node, samples):
    def valid(row):
        return row.get('canonical') is True and row.get('state_ok') is True and 'lag' in row

    start = next((i for i, row in enumerate(samples) if row.get('lag') == 0 and valid(row)), len(samples))
    live = samples[start:]
    ok = [row for row in live if valid(row)]
    wrong = [row for row in live if row.get('canonical') is False or row.get('state_ok') is False]
    span = ok[-1]['monotonic'] - ok[0]['monotonic'] if len(ok) > 1 else 0
    max_gap = max((b['monotonic'] - a['monotonic'] for a, b in zip(ok, ok[1:])), default=None)
    lags = sorted(row['lag'] for row in ok)
    max_lag = lags[-1] if lags else None
    rpc_complete = (span >= 3600 and max_gap is not None and max_gap <= 20
                    and 0 <= lags[0] <= max_lag <= 64 and not wrong)
    execution_complete = (node['liveSeconds'] >= 3600 and node['liveBlocks'] > 0
                          and node['executed'] == node['certified'] == node['head'] - node['anchor'])
    return {'score': 'yes' if rpc_complete and execution_complete else 'no',
            'node': node,
            'rpc': {'samples': len(ok), 'attempts': len(live), 'incorrect': len(wrong),
                    'sampledSeconds': span, 'maxGapSeconds': max_gap,
                    'catchupHead': ok[0]['head'] if ok else None,
                    'lastSampledHead': ok[-1]['head'] if ok else None,
                    'maxObservedLag': max_lag, 'endObservedLag': ok[-1]['lag'] if ok else None,
                    'zeroLagSamples': lags.count(0),
                    'p95ObservedLag': lags[(95 * len(lags) + 99) // 100 - 1] if lags else None,
                    'blocksAfterCatchup': node['head'] - ok[0]['head'] if ok else None}}


def main():
    run = pathlib.Path(sys.argv[1])
    node = json.loads((run / 'node.json').read_bytes())
    samples = [json.loads(line) for line in (run / 'rpc.jsonl').read_bytes().splitlines()]
    result = grade(node, samples)
    result['revision'] = (run / 'revision').read_text().strip()
    result['command'] = (run / 'command').read_text().strip()
    rendered = json.dumps(result, indent=2) + '\n'
    (run / 'r4.json').write_text(rendered)
    sys.stdout.write(rendered)
    sys.exit(0 if result['score'] == 'yes' else 1)


if __name__ == '__main__':
    main()
