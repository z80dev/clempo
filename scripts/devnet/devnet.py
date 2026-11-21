#!/usr/bin/env python3
"""Isolated loopback reference harness; never used by the clempo node."""
import argparse
import contextlib
import fcntl
import json
import os
from pathlib import Path
import re
import signal
import socket
import subprocess
import sys
import time
import urllib.request

HERE = Path(__file__).resolve().parent
HISTORY = '0000f90827f1c53a10cb7a02335b175320002935'


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, request, fp, code, msg, headers, newurl):
        return None


LOCAL_HTTP = urllib.request.build_opener(urllib.request.ProxyHandler({}), NoRedirect())


def save(path, value):
    temporary = path.with_suffix(path.suffix + '.tmp')
    temporary.write_text(json.dumps(value, indent=2) + '\n')
    temporary.replace(path)


def run(argv, timeout=120, **kwargs):
    return subprocess.run([str(x) for x in argv], check=True, timeout=timeout, **kwargs)


def provision(a):
    root = a.root
    root.mkdir(parents=True, exist_ok=False)
    ports = [a.base_port + 100 * i for i in range(4)]
    if a.base_port < 20000 or ports[-1] + 4 > 65535 or any(p <= 30311 <= p + 4 for p in ports):
        raise ValueError('choose isolated ports >=20000, excluding mainnet 30311')
    output = root / 'network'
    command = [a.xtask, 'generate-localnet', '--output', output, '--seed', str(a.seed),
               '--accounts', '5', '--epoch-length', str(a.epoch_length), '--hardfork', a.hardfork,
               '--validators', ','.join(f'127.0.0.1:{p}' for p in ports)]
    with (root / 'provision.log').open('xb') as log:
        run(command, timeout=a.timeout, stdout=log, stderr=subprocess.STDOUT)
    genesis = output / 'genesis.json'
    allocation = json.loads(genesis.read_text())['alloc']
    history = next((v for k, v in allocation.items() if k.lower().removeprefix('0x') == HISTORY), None)
    if not history or history.get('code', '0x') in ('', '0x'):
        raise ValueError('xtask genesis lacks EIP-2935 history storage code; update reference generator')
    helper = root / 'key-export'
    helper.mkdir()
    # Isolated reference-only crate, never added to the Lisp node or Tempo workspace.
    (helper / 'Cargo.toml').write_text(
        '[package]\nname="clempo-devnet-key-export"\nversion="0.0.0"\nedition="2024"\n'
        '[workspace]\n[[bin]]\nname="clempo-devnet-key-export"\npath=' + json.dumps(str(HERE / 'export-key.rs')) + '\n'
        '[dependencies]\ntempo-consensus-config={path=' + json.dumps(str(a.tempo_source / 'crates/consensus-config')) + '}\n')
    # Preserve reference pins, including yanked versions already selected by Tempo.
    # Cargo may prune this copy for the standalone helper; never change the source lock.
    (helper / 'Cargo.lock').write_bytes((a.tempo_source / 'Cargo.lock').read_bytes())
    with (root / 'key-export-build.log').open('xb') as log:
        run(['cargo', 'build', '--offline', '--manifest-path', helper / 'Cargo.toml'], timeout=a.timeout,
            stdout=log, stderr=subprocess.STDOUT)
    exporter = helper / 'target/debug/clempo-devnet-key-export'
    validators = []
    for slot, port in enumerate(ports):
        directory = output / f'127.0.0.1:{port}'
        seed = directory / 'signing.seed'
        public = directory / 'signing.public'
        run([exporter, directory / 'signing.key', seed, public])
        encoded = seed.read_text().strip()
        raw = bytes.fromhex(encoded.removeprefix('0x'))
        if len(raw) != 32:
            raise ValueError('reference key exporter did not produce a 32-byte seed')
        seed.write_bytes(raw)
        validators.append(dict(slot=slot, public_key=public.read_text().strip().removeprefix('0x'),
            signing_key=str(directory / 'signing.key'), signing_seed=str(seed),
            signing_share=str(directory / 'signing.share'), datadir=str(directory / 'data'),
            consensus_address=f'127.0.0.1:{port}', execution_port=port + 1,
            metrics_url=f'http://127.0.0.1:{port + 2}', authrpc_port=port + 3,
            rpc_url=f'http://127.0.0.1:{port + 4}', enode_key=str(directory / 'enode.key'),
            enode_identity=(directory / 'enode.identity').read_text().strip()))
    (root / 'localnet.secret').write_text('tempo-localnet-signing-key-secret\n')
    save(root / 'manifest.json', dict(version=1, genesis=str(genesis), epoch_length=a.epoch_length,
         hardfork=a.hardfork, seed=a.seed, tempo_binary=str(a.tempo), validators=validators))
    save(root / 'launch-history.json', [])
    print(root / 'manifest.json')


def load(root):
    m = json.loads((root / 'manifest.json').read_text())
    if m['version'] != 1 or sorted(v['slot'] for v in m['validators']) != list(range(4)):
        raise ValueError('expected version 1 four-validator manifest')
    if Path(m['genesis']).resolve() != (root / 'network/genesis.json').resolve():
        raise ValueError('expected the generated genesis for this devnet')
    ports = set()
    for v in m['validators']:
        address = re.fullmatch(r'127[.]0[.]0[.]1:([0-9]{1,5})', v['consensus_address'])
        if not address:
            raise ValueError('only loopback devnet manifests accepted')
        port = int(address[1])
        group = set(range(port, port + 5))
        if (port < 20000 or port + 4 > 65535 or 30311 in group or ports & group or
                v['execution_port'] != port + 1 or v['authrpc_port'] != port + 3 or
                v['rpc_url'] != f'http://127.0.0.1:{port + 4}' or
                v['metrics_url'] != f'http://127.0.0.1:{port + 2}' or
                not re.fullmatch(r'[a-fA-F0-9]{128}', v['enode_identity'])):
            raise ValueError('invalid or overlapping local devnet endpoint')
        ports.update(group)
    return m


def identity(pid):
    result = subprocess.run(['ps', '-p', str(pid), '-o', 'lstart=', '-o', 'command='],
                            capture_output=True, text=True, timeout=5)
    return result.stdout.strip() if result.returncode == 0 else ''


def alive(record):
    return bool(record.get('identity')) and identity(record['pid']) == record['identity']


def records(root):
    path = root / 'processes.json'
    return json.loads(path.read_text()) if path.exists() else {}



def log_lines(path, offset):
    """Read complete records only; a concurrent writer may leave a partial line."""
    with path.open('rb') as log:
        log.seek(offset)
        lines = []
        end = os.fstat(log.fileno()).st_size
        while log.tell() < end:
            line = log.readline(end - log.tell())
            if not line.endswith(b'\n'):
                break
            lines.append(line.decode('utf-8', errors='replace'))
            offset = log.tell()
    return lines, offset


def remember_launch(root, slot, implementation):
    path = root / 'launch-history.json'
    # Older roots cannot establish that an excluded identity was never launched.
    if path.exists():
        history = json.loads(path.read_text())
        history.append(dict(slot=slot, implementation=implementation, time=time.time()))
        save(path, history)


def quorum_state(a, m, expected=None):
    history_path = a.root / 'launch-history.json'
    if len(a.slots) != 3 or not a.clempo or a.from_height != 0 or not history_path.exists():
        raise RuntimeError('quorum requires a fresh tracked net, three slots, clempo, and --from-height 0')
    excluded = set(range(4)) - set(a.slots)
    history = json.loads(history_path.read_text())
    if any(row['slot'] in excluded for row in history):
        raise RuntimeError('excluded quorum slot was previously launched on this genesis')
    owned = records(a.root)
    active = {int(slot): row for slot, row in owned.items() if alive(row)}
    if set(active) != set(a.slots):
        raise RuntimeError('quorum requires exactly the three selected owned processes alive')
    for slot, row in active.items():
        if row.get('implementation', 'rust') != ('clempo' if slot in a.clempo else 'rust'):
            raise RuntimeError('capture implementation selection differs from launched processes')
    if expected is not None and active != expected:
        raise RuntimeError('quorum process identity changed during capture')
    for v in m['validators']:
        if v['slot'] in excluded:
            if Path(v['datadir']).exists() or (a.root / f"clempo-{v['slot']}").exists():
                raise RuntimeError('excluded quorum slot has a datadir')
            with socket.socket() as sock:
                sock.settimeout(0.2)
                if sock.connect_ex(('127.0.0.1', int(v['consensus_address'].rsplit(':', 1)[1]))) == 0:
                    raise RuntimeError('excluded quorum consensus port is occupied')
    return active

def rpc(v, method, params, timeout=3):
    body = json.dumps(dict(jsonrpc='2.0', id=1, method=method, params=params)).encode()
    request = urllib.request.Request(v['rpc_url'], body, {'Content-Type': 'application/json'})
    with LOCAL_HTTP.open(request, timeout=timeout) as response:
        result = json.load(response)
    if 'error' in result:
        raise RuntimeError(f"slot {v['slot']} RPC: {result['error']}")
    return result['result']


def stop(root, selected, timeout):
    owned = records(root)
    for slot in selected:
        record = owned.get(str(slot))
        if record and alive(record):
            os.kill(record['pid'], signal.SIGTERM)
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if not any(alive(owned[str(s)]) for s in selected if str(s) in owned):
            return
        time.sleep(0.2)
    raise RuntimeError('shutdown deadline exceeded; no SIGKILL sent, inspect logs and status')


def start(a, m):
    owned = records(a.root)
    selected = [v for v in m['validators'] if v['slot'] in a.slots]
    with contextlib.ExitStack() as sockets:
        for v in selected:
            if str(v['slot']) in owned and alive(owned[str(v['slot'])]):
                raise RuntimeError(f"slot {v['slot']} already running")
            port = int(v['consensus_address'].rsplit(':', 1)[1])
            for p in range(port, port + 5):
                sock = sockets.enter_context(socket.socket())
                # A restarted node leaves TIME_WAIT connections; a live listener still fails.
                sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                sock.bind(('127.0.0.1', p))
    peers = ','.join(f"enode://{v['enode_identity']}@127.0.0.1:{v['execution_port']}" for v in m['validators'])
    spawned = []
    children = {}
    try:
        for v in selected:
            implementation = 'clempo' if v['slot'] in a.clempo else 'rust'
            command = [m['tempo_binary'], 'node', '--chain', m['genesis'], '--datadir', v['datadir'],
                '--consensus.signing-key', v['signing_key'], '--consensus.secret', str(a.root / 'localnet.secret'),
                '--consensus.signing-share', v['signing_share'], '--consensus.listen-address', v['consensus_address'],
                '--consensus.metrics-address', v['metrics_url'].removeprefix('http://'),
                '--consensus.use-local-defaults',
                '--trusted-peers', peers, '--addr', '127.0.0.1', '--port', str(v['execution_port']),
                '--disable-discovery', '--p2p-secret-key', v['enode_key'],
                '--authrpc.addr', '127.0.0.1', '--authrpc.port', str(v['authrpc_port']),
                '--http', '--http.addr', '127.0.0.1', '--http.port', v['rpc_url'].rsplit(':', 1)[1],
                '--http.api', 'eth,net,web3,debug,consensus', '--ipcdisable']
            if implementation == 'clempo':
                # The same identity, share and ports as the slot's Rust node, on its own
                # datadir; restarting resumes from it.
                command = [str(HERE.parents[1] / 'bin/clempo'), 'validator', '--chain', m['genesis'],
                    '--datadir', str(a.root / f"clempo-{v['slot']}"),
                    '--consensus.signing-key', v['signing_key'],
                    '--consensus.secret', str(a.root / 'localnet.secret'),
                    '--consensus.signing-share', v['signing_share'],
                    '--consensus.listen-address', v['consensus_address'],
                    '--consensus.use-local-defaults',
                    '--metrics', v['metrics_url'].removeprefix('http://'),
                    '--http', '--http.addr', '127.0.0.1', '--http.port', v['rpc_url'].rsplit(':', 1)[1]]
            log_path = a.root / f"{implementation}-{v['slot']}.log"
            log_offset = log_path.stat().st_size if log_path.exists() else 0
            remember_launch(a.root, v['slot'], implementation)
            with log_path.open('ab') as log:
                child = subprocess.Popen(command, stdin=subprocess.DEVNULL, stdout=log,
                                         stderr=subprocess.STDOUT, start_new_session=True)
            children[v['slot']] = child
            record = dict(pid=child.pid, identity=identity(child.pid), command=command,
                          implementation=implementation, log_offset=log_offset)
            owned[str(v['slot'])] = record
            save(a.root / 'processes.json', owned)
            spawned.append(v['slot'])
        deadline = time.monotonic() + a.timeout
        pending = list(selected)
        readiness_offsets = {v['slot']: owned[str(v['slot'])]['log_offset'] for v in selected}
        while pending and time.monotonic() < deadline:
            for v in pending[:]:
                child = children[v['slot']]
                if child.poll() is not None:
                    raise RuntimeError(f"slot {v['slot']} exited; inspect its validator log")
                # The CLI launcher execs SBCL in-place; this unreaped child is still ours.
                owned[str(v['slot'])]['identity'] = identity(child.pid)
                save(a.root / 'processes.json', owned)
                try:
                    if v['slot'] in a.clempo:
                        lines, readiness_offsets[v['slot']] = log_lines(
                            a.root / f"clempo-{v['slot']}.log", readiness_offsets[v['slot']])
                        for line in lines:
                            if line.lstrip().startswith('{'):
                                event = json.loads(line)
                                if (event.get('event') == 'epoch' and event.get('signing') is True
                                        or a.ready == 'started' and event.get('event') == 'started'):
                                    pending.remove(v)
                                    break
                    elif rpc(v, 'eth_chainId', []):
                        pending.remove(v)
                except (OSError, RuntimeError):
                    pass
            if pending:
                time.sleep(0.25)
        if pending:
            raise RuntimeError('validator readiness deadline exceeded')
    except BaseException:
        for slot, child in children.items():
            if child.poll() is None:
                owned[str(slot)]['identity'] = identity(child.pid)
        save(a.root / 'processes.json', owned)
        stop(a.root, spawned, 10)
        raise
    print('Validators ready:', ','.join(str(v['slot']) for v in selected))


def check_mixed(a, m, mixed, reference, boundary_data, emit):
    keys = {v['slot']: v['public_key'].lower().removeprefix('0x') for v in m['validators']}
    for slot, state in mixed.items():
        state.setdefault('last_progress', time.monotonic())
        lines, state['offset'] = log_lines(a.root / f'clempo-{slot}.log', state['offset'])
        for line in lines:
            if not line.lstrip().startswith('{'):
                continue
            event = json.loads(line)
            kind = event.get('event')
            if kind in ('finalized', 'proposed'):
                height = int(event['height'])
                digest = event['hash'].lower().removeprefix('0x')
                if kind == 'finalized':
                    prior = state['finalized'].get(height)
                    if prior is not None and prior['hash'] != digest:
                        raise RuntimeError(f'clempo slot {slot} finalized conflicting hashes at {height}')
                    state['finalized'][height] = dict(hash=digest, epoch=int(event['epoch']),
                        proposer=event['proposer'].lower().removeprefix('0x'))
                else:
                    state['proposed'].add((height, digest))
            elif kind == 'dkg':
                epoch = int(event['epoch'])
                if epoch > 0:
                    if event.get('share') is not True or event.get('reshared') is not True:
                        raise RuntimeError(f'clempo slot {slot} failed to reshare at epoch {epoch}')
                    dealers = [key.lower().removeprefix('0x') for key in event['dealers']]
                    if keys[slot] not in dealers:
                        raise RuntimeError(f'clempo slot {slot} missing as dealer for epoch {epoch}')
                    state['dkg'][epoch] = dealers
            elif kind == 'epoch' and event.get('signing') is True:
                state['signing'].add(int(event['epoch']))
        for height, finalized in state['finalized'].items():
            if height not in reference:
                continue
            if finalized['hash'] != reference[height]:
                raise RuntimeError(f'clempo slot {slot} differs from Rust finalized hash at {height}')
            if height not in state['checked']:
                state['checked'].add(height)
                state['last_progress'] = time.monotonic()
                emit(dict(kind='clempo_finalized', slot=slot, height=height, **finalized))
            proposal = (height, finalized['hash'])
            if proposal in state['proposed'] and finalized['proposer'] == keys[slot] and proposal not in state['own']:
                state['own'].add(proposal)
                emit(dict(kind='clempo_own_proposal', slot=slot, height=height, hash=finalized['hash']))
            epoch = finalized['epoch']
            boundary = epoch * m['epoch_length'] - 1
            if (epoch > 0 and epoch not in state['epochs'] and epoch in state['dkg']
                    and epoch in state['signing'] and epoch in boundary_data
                    and boundary in state['checked'] and height > boundary):
                if boundary_data.get(epoch - 1) == boundary_data[epoch]:
                    raise RuntimeError(f'unchanged DKG boundary outcome at epoch {epoch}')
                state['epochs'].add(epoch)
                emit(dict(kind='clempo_dkg_epoch', slot=slot, epoch=epoch,
                          boundary_height=boundary, post_boundary_height=height,
                          outcome=boundary_data[epoch], dealers=state['dkg'][epoch]))
        if time.monotonic() - state['last_progress'] > a.stall:
            raise RuntimeError(f'clempo slot {slot} finalized progress stalled')


def capture(a, m):
    validators = [v for v in m['validators'] if v['slot'] in a.slots and v['slot'] not in a.clempo]
    if not validators:
        raise RuntimeError('capture requires at least one independent Rust validator')
    if a.clempo and a.from_height != 0:
        raise RuntimeError('mixed capture requires --from-height 0 to verify every recorded finalization')
    quorum = quorum_state(a, m) if a.quorum else None
    reference = {}
    boundary_data = {}
    owned = records(a.root)
    for slot in a.clempo:
        if owned.get(str(slot), {}).get('implementation') != 'clempo':
            raise RuntimeError(f'slot {slot} is not a tracked clempo validator')
    mixed = {slot: dict(offset=records(a.root).get(str(slot), {}).get('log_offset', 0),
                        finalized={}, proposed=set(), dkg={}, signing=set(), checked=set(),
                        own=set(), epochs=set()) for slot in a.clempo}
    deadline = time.monotonic() + a.seconds
    next_height = a.from_height
    last_progress = time.monotonic()
    compared = boundaries = 0
    successes = {}
    offsets = {v['slot']: 0 for v in validators}
    log_counts = {v['slot']: [0, 0] for v in validators}
    with a.report.open('x') as output:
        def emit(row):
            output.write(json.dumps(row) + '\n')
            output.flush()
        emit(dict(kind='provenance', manifest=str(a.root / 'manifest.json'), slots=a.slots,
                  epoch_length=m['epoch_length'], started_at=time.time(),
                  clempo=a.clempo, quorum_requested=a.quorum))
        while time.monotonic() < deadline:
            for slot in a.clempo:
                if not alive(owned[str(slot)]):
                    raise RuntimeError(f'clempo slot {slot} exited or changed identity')
            if a.quorum:
                quorum_state(a, m, quorum)
            heads = [rpc(v, 'eth_getBlockByNumber', ['finalized', False]) for v in validators]
            if any(h is None for h in heads):
                if compared or time.monotonic() - last_progress > a.stall:
                    raise RuntimeError('missing finalized head')
                time.sleep(min(1, max(0, deadline - time.monotonic())))
                continue
            tip = min(int(h['number'], 16) for h in heads)
            while next_height <= tip and time.monotonic() < deadline:
                blocks = [rpc(v, 'eth_getBlockByNumber', [hex(next_height), False]) for v in validators]
                if any(b is None for b in blocks):
                    raise RuntimeError(f'missing finalized block {next_height}')
                row = dict(kind='block', height=next_height,
                           slots=[dict(slot=v['slot'], hash=b['hash'], extra_data=b['extraData'])
                                  for v, b in zip(validators, blocks)])
                emit(row)
                if len({b['hash'] for b in blocks}) != 1:
                    raise RuntimeError(f'finalized hash divergence at {next_height}')
                reference[next_height] = blocks[0]['hash'].lower().removeprefix('0x')
                if next_height == 0:
                    boundary_data[0] = blocks[0]['extraData']
                if (next_height + 1) % m['epoch_length'] == 0:
                    if any(b['extraData'] in ('0x', '') for b in blocks):
                        raise RuntimeError(f'missing boundary DKG outcome at {next_height}')
                    boundaries += 1
                    boundary_data[(next_height + 1) // m['epoch_length']] = blocks[0]['extraData']
                compared += 1
                next_height += 1
                last_progress = time.monotonic()
            for v in validators:
                slot = v['slot']
                # The reference emits terminal ceremony results even when its
                # metrics exporter does not include the DKG actor's counters.
                lines, offsets[slot] = log_lines(a.root / f'rust-{slot}.log', offsets[slot])
                for line in lines:
                    log_counts[slot][0] += ('local DKG ceremony was a failure' in line or
                                            'DKG ceremony failed and the previous epoch' in line)
                    log_counts[slot][1] += 'local DKG ceremony was a success' in line
                failures, successes[slot] = log_counts[slot]
                emit(dict(kind='dkg_log', slot=slot, finalized=int(heads[validators.index(v)]['number'], 16),
                          ceremony_failures=failures, ceremony_successes=successes[slot]))
                if failures:
                    raise RuntimeError(f"slot {slot} reports DKG ceremony failures")
            check_mixed(a, m, mixed, reference, boundary_data, emit)
            if time.monotonic() - last_progress > a.stall:
                raise RuntimeError('finality stalled beyond configured deadline')
            time.sleep(min(1, max(0, deadline - time.monotonic())))
        if compared == 0 or boundaries == 0:
            raise RuntimeError('capture did not observe a finalized epoch boundary; extend duration')
        if any(successes.get(v['slot'], 0) <= 0 for v in validators):
            raise RuntimeError('not every Rust validator reported a successful DKG ceremony')
        if a.quorum:
            quorum_state(a, m, quorum)
        for slot, state in mixed.items():
            if not state['checked'] or not state['own'] or not state['epochs']:
                raise RuntimeError(f'slot {slot} lacks finalized, own-proposal, or post-DKG epoch evidence')
        emit(dict(kind='summary', compared=compared, boundary_outcomes=boundaries,
                  next_height=next_height, dkg_successes=successes, clempo_slots=a.clempo,
                  clempo_proposals={slot: len(state['own']) for slot, state in mixed.items()},
                  clempo_reshare_epochs={slot: len(state['epochs']) for slot, state in mixed.items()},
                  clempo={slot: dict(finalized=len(state['checked']), own_proposals=len(state['own']),
                                    dkg_epochs=sorted(state['epochs'])) for slot, state in mixed.items()},
                  quorum=bool(quorum), quorum_requested=a.quorum,
                  quorum_active_slots=a.slots if quorum else [],
                  quorum_excluded_slots=sorted(set(range(4)) - set(a.slots)) if quorum else []))

# The well-known first funded localnet account (xtask's test mnemonic); genesis makes it the
# ValidatorConfigV2 owner. Never use it outside an isolated loopback devnet.
OWNER_KEY = 'ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80'
# Funded localnet accounts 1 and 2 (the owner's nonce stays free for registrations).
SENDER_KEYS = ['59c6995e998f97a5a0044966f0945389dc9e86dae88c7a8412f4603b6b78690d',
               '5de4111afa1a4b94908f83103eb1f1706367c2e68ca870fc3fb9a804cdab365a']


def joiner_paths(root):
    base = root / 'joiner'
    return dict(base=base, key=base / 'signing.key', owner=base / 'owner.key',
                datadir=root / 'clempo-joiner', log=root / 'clempo-joiner.log')


def join(a, m):
    """Register a fifth, clempo, validator through `clempo keys add-validator --send` and
    start it without a share: it joins as a player in the next DKG."""
    clempo = str(HERE.parents[1] / 'bin/clempo')
    paths = joiner_paths(a.root)
    owned = records(a.root)
    if alive(owned.get('joiner', {})):
        raise RuntimeError('the joiner is already running')
    port = a.port
    rust = next(v for v in m['validators'] if v['slot'] == a.via)
    if not paths['key'].exists():
        paths['base'].mkdir(mode=0o700, exist_ok=True)
        run([clempo, 'keys', 'generate', '--output', str(paths['key'])], timeout=600)
        paths['owner'].write_text(OWNER_KEY + '\n')
        chain_id = int(rpc(rust, 'eth_chainId', []), 16)
        result = run([clempo, 'keys', 'add-validator', '--consensus.signing-key', str(paths['key']),
                      '--chain-id', str(chain_id), '--validator', a.validator,
                      '--ingress', f'127.0.0.1:{port}', '--egress', '127.0.0.1',
                      '--fee-recipient', a.validator, '--send', '--rpc', rust['rpc_url'],
                      '--owner-key', str(paths['owner'])], timeout=600, capture_output=True, text=True)
        registration = json.loads(result.stdout.strip().splitlines()[-1])
        save(paths['base'] / 'registration.json', registration)
        receipt = registration.get('receipt', {})
        print(json.dumps(dict(public_key=registration['publicKey'], status=receipt.get('status'),
                              block=receipt.get('blockNumber'))))
        if receipt.get('status') != '0x1':
            raise RuntimeError('addValidator was not executed successfully; see joiner/registration.json')
    command = [clempo, 'validator', '--chain', m['genesis'], '--datadir', str(paths['datadir']),
               '--consensus.signing-key', str(paths['key']),
               '--consensus.listen-address', f'127.0.0.1:{port}', '--consensus.use-local-defaults',
               '--metrics', f'127.0.0.1:{port + 2}',
               '--http', '--http.addr', '127.0.0.1', '--http.port', str(port + 4)]
    log_offset = paths['log'].stat().st_size if paths['log'].exists() else 0
    with paths['log'].open('ab') as log:
        child = subprocess.Popen(command, stdin=subprocess.DEVNULL, stdout=log,
                                 stderr=subprocess.STDOUT, start_new_session=True)
    owned['joiner'] = dict(pid=child.pid, identity=identity(child.pid), command=command,
                           implementation='clempo', log_offset=log_offset)
    save(a.root / 'processes.json', owned)
    print('Joiner started, pid', child.pid)


def send(a, m):
    """Submit no-op calls to clempo validators' JSON-RPC (their transaction pools), so their
    proposals carry transactions; prints each hash."""
    targets = [v for v in m['validators'] if v['slot'] in a.clempo]
    deadline = time.monotonic() + a.seconds
    sent = 0
    while time.monotonic() < deadline:
        for v in targets:
            for key in SENDER_KEYS:
                result = subprocess.run(['cast', 'send', '--async', '--private-key', '0x' + key,
                                         '--rpc-url', v['rpc_url'], '0x70997970C51812dc3A010C7d01b50e0d17dc79C8'],
                                        timeout=60, capture_output=True, text=True)
                if result.returncode == 0:
                    sent += 1
                    print(json.dumps(dict(slot=v['slot'], tx=result.stdout.strip())), flush=True)
                else:
                    print(json.dumps(dict(slot=v['slot'], error=result.stderr.strip()[-300:])), flush=True)
        time.sleep(a.interval)
    print(json.dumps(dict(sent=sent)))


def clempo_runs(path):
    """Split a clempo validator log into process runs (each begins with a 'started' event)."""
    runs = []
    for line in path.read_text(errors='replace').splitlines():
        if not line.startswith('{'):
            continue
        try:
            event = json.loads(line)
        except ValueError:
            continue
        kind = event.get('event')
        if kind == 'started':
            runs.append(dict(start_head=event['head'], key=event['key'], behind=None, caught_up=None,
                             replayed=None, signing_epochs=[], dkg=[], proposed=[], stopped=None))
        elif not runs:
            continue
        run_ = runs[-1]
        if kind == 'replaying':
            run_['replayed'] = [event['from'], event['to']]
        elif kind == 'behind':
            run_['behind'] = max(run_['behind'] or 0, event['target'] - event['head'])
        elif kind == 'caught-up':
            run_['caught_up'] = event['head']
        elif kind == 'epoch' and event.get('signing') is True:
            run_['signing_epochs'].append(event['epoch'])
        elif kind == 'dkg':
            run_['dkg'].append(dict(epoch=event['epoch'], players=len(event.get('players', [])),
                                    share=event.get('share')))
        elif kind == 'proposed':
            run_['proposed'].append((event['height'], event['hash'], event.get('transactions', 0)))
        elif kind == 'stopped':
            run_['stopped'] = event['head']
    return runs


def audit(a, m):
    """Every node's finalized chain agrees with the Rust validators' at every height, and each
    clempo validator's process runs (restarts, catch-up, signing epochs, DKG, its own proposals
    finalized by Rust, with transaction counts) as its log records them."""
    rust = [v for v in m['validators'] if v['slot'] not in a.clempo]
    nodes = {f"rust-{v['slot']}": v for v in rust}
    nodes.update({f"clempo-{v['slot']}": v for v in m['validators'] if v['slot'] in a.clempo})
    if a.joiner:
        nodes['clempo-joiner'] = dict(slot='joiner', rpc_url=f'http://127.0.0.1:{a.port + 4}')
    heads = {}
    for name, v in nodes.items():
        try:
            heads[name] = int(rpc(v, 'eth_getBlockByNumber', ['finalized', False])['number'], 16)
        except (OSError, RuntimeError, TypeError):
            heads[name] = None
    reference = nodes[f"rust-{rust[0]['slot']}"]
    top = min(h for h in heads.values() if h is not None)
    with a.report.open('x') as output:
        def emit(row):
            output.write(json.dumps(row, sort_keys=True) + '\n')
            output.flush()
        emit(dict(kind='heads', heads=heads, compared_to=top))
        canonical = {}
        for height in range(0, top + 1):
            block = rpc(reference, 'eth_getBlockByNumber', [hex(height), False])
            canonical[height] = block['hash']
            for name, v in nodes.items():
                if v is reference or heads[name] is None:
                    continue
                other = rpc(v, 'eth_getBlockByNumber', [hex(height), False])
                if other is None or other['hash'] != block['hash']:
                    raise RuntimeError(f'{name} differs from Rust at height {height}')
        logs = {f'clempo-{slot}': a.root / f'clempo-{slot}.log' for slot in a.clempo}
        if a.joiner:
            logs['clempo-joiner'] = joiner_paths(a.root)['log']
        summary = {}
        for name, path in logs.items():
            runs = clempo_runs(path)
            rows = []
            for index, run_ in enumerate(runs):
                own = [(h, txs) for h, digest, txs in run_['proposed'] if canonical.get(h) == digest]
                row = dict(run=index, start_head=run_['start_head'], replayed=run_['replayed'],
                           behind=run_['behind'], caught_up=run_['caught_up'], stopped=run_['stopped'],
                           signing_epochs=sorted(set(run_['signing_epochs'])), dkg=run_['dkg'],
                           finalized_own_proposals=len(own),
                           finalized_own_with_transactions=len([h for h, txs in own if txs > 0]),
                           transactions_in_own_proposals=sum(txs for h, txs in own),
                           first_own_finalized=min((h for h, txs in own), default=None))
                emit(dict(kind='run', node=name, key=run_['key'], **row))
                rows.append(row)
            summary[name] = rows
        emit(dict(kind='summary', compared=top + 1, heads=heads, runs=summary))
    print(json.dumps(dict(compared=top + 1, heads=heads)))


def slots(value):
    result = [int(x) for x in value.split(',')]
    if not result or len(set(result)) != len(result) or any(x not in range(4) for x in result):
        raise argparse.ArgumentTypeError('slots must be distinct comma-separated values 0..3')
    return result


def main():
    os.umask(0o077)
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--root', type=lambda x: Path(x).expanduser().resolve(), required=True)
    sub = p.add_subparsers(dest='command', required=True)
    gen = sub.add_parser('provision')
    gen.add_argument('--tempo-source', type=lambda x: Path(x).expanduser().resolve(), default=Path.home() / 'dev/tempo')
    gen.add_argument('--tempo', type=lambda x: Path(x).expanduser().resolve(), required=True)
    gen.add_argument('--xtask', type=lambda x: Path(x).expanduser().resolve(),
                     default=Path.home() / 'dev/tempo/target/debug/tempo-xtask')
    gen.add_argument('--base-port', type=int, default=21000)
    gen.add_argument('--seed', type=int, default=42)
    gen.add_argument('--epoch-length', type=int, default=60)
    gen.add_argument('--hardfork', default='t13')
    gen.add_argument('--timeout', type=float, default=1800)
    for name in ('start', 'stop', 'status', 'capture'):
        cmd = sub.add_parser(name)
        cmd.add_argument('--slots', type=slots, default=[0, 1, 2, 3])
        if name in ('start', 'capture'):
            cmd.add_argument('--clempo', type=slots, default=[])
        if name == 'start':
            # 'started': a restarted or joining clempo validator need not sign yet.
            cmd.add_argument('--ready', choices=('signing', 'started'), default='signing')
        if name in ('start', 'stop'):
            cmd.add_argument('--timeout', type=float, default=60)
        if name in ('stop', 'status'):
            cmd.add_argument('--joiner', action='store_true', help='also the joined fifth validator')
        if name == 'capture':
            cmd.add_argument('--quorum', action='store_true')
            cmd.add_argument('--seconds', type=float, default=120)
            cmd.add_argument('--stall', type=float, default=30)
            cmd.add_argument('--from-height', type=int, default=0)
            cmd.add_argument('--report', type=Path, required=True)
    probe = sub.add_parser('probe')
    probe.add_argument('--slot', type=int, choices=range(4), required=True)
    probe.add_argument('--seconds', type=int, default=20)
    joiner = sub.add_parser('join')
    joiner.add_argument('--port', type=int, default=None, help='consensus port (default: base port + 400)')
    joiner.add_argument('--via', type=int, choices=range(4), default=0, help='Rust slot whose RPC takes the registration')
    joiner.add_argument('--validator', default='0x9965507D1a55bcC2695C58ba16FB37d819B0A4dc',
                        help='the validator (and fee recipient) address')
    sender = sub.add_parser('send')
    sender.add_argument('--clempo', type=slots, required=True)
    sender.add_argument('--seconds', type=float, default=60)
    sender.add_argument('--interval', type=float, default=2)
    auditor = sub.add_parser('audit')
    auditor.add_argument('--clempo', type=slots, default=[])
    auditor.add_argument('--joiner', action='store_true')
    auditor.add_argument('--port', type=int, default=None)
    auditor.add_argument('--report', type=Path, required=True)
    a = p.parse_args()
    if any(getattr(a, key, 1) <= 0 for key in ('timeout', 'seconds', 'stall', 'epoch_length')):
        p.error('durations and epoch length must be positive')
    if hasattr(a, 'clempo') and hasattr(a, 'slots'):
        a.clempo = [slot for slot in a.clempo if slot in a.slots]
    if getattr(a, 'from_height', 0) < 0:
        p.error('--from-height must not be negative')
    if a.command == 'provision':
        provision(a)
        return
    m = load(a.root)
    if getattr(a, 'port', 0) is None:
        a.port = int(m['validators'][0]['consensus_address'].rsplit(':', 1)[1]) + 400
    with (a.root / 'operation.lock').open('a') as lock:
        if a.command in ('start', 'stop', 'probe', 'join') or getattr(a, 'quorum', False):
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        if a.command == 'start':
            start(a, m)
        elif a.command == 'stop':
            stop(a.root, a.slots + (['joiner'] if a.joiner else []), a.timeout)
        elif a.command == 'status':
            owned = records(a.root)
            for slot in a.slots + (['joiner'] if a.joiner else []):
                record = owned.get(str(slot), {})
                print(json.dumps(dict(slot=slot, pid=record.get('pid'), running=alive(record))))
        elif a.command == 'probe':
            record = records(a.root).get(str(a.slot), {})
            if alive(record):
                raise RuntimeError('stop the Rust slot before borrowing its identity')
            # Capture is deliberately independent; it is never a fake validator substitute.
            remember_launch(a.root, a.slot, 'probe')
            run([HERE.parents[1] / 'bin/clempo-dev', 'devnet-probe', '-manifest', a.root / 'manifest.json',
                 '-slot', str(a.slot), '-seconds', str(a.seconds)], timeout=a.seconds + 60)
        elif a.command == 'capture':
            capture(a, m)
        elif a.command == 'join':
            join(a, m)
        elif a.command == 'send':
            send(a, m)
        elif a.command == 'audit':
            audit(a, m)


if __name__ == '__main__':
    try:
        main()
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as error:
        print(f'devnet: {error}', file=sys.stderr)
        sys.exit(1)
