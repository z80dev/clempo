These wire vectors are data from [goempo at `92e996c`](https://github.com/gakonst/goempo/tree/92e996cab6b733c841301c032235eed1ddbec773):

- `frames.txt` and `handshake.txt`: `internal/p2p/rlpx/testdata`, via zempo's RLPx frame and EIP-8 vectors. The frame rows use a shared running cipher/MAC state.
- `vectors.txt`: `internal/p2p/snappy/testdata`, Snappy raw blocks produced by a reference encoder, via zempo.
- `sake_cups_vectors.json`: `internal/cwp2p/testdata`, emitted by its Rust harness using `commonware-stream` and `commonware-cryptography` 2026.10.0. Includes V0/V1 SAKE transcripts, derived keys, and CUPS ciphertexts.
- `tempo_devnet_frames.json`: `internal/cwp2p/testdata`, raw Commonware lookup channel payloads (including mux prefixes) from a Tempo `c8b89f9` Rust devnet interoperability capture. These are not encrypted CUPS records or complete outer lookup frames.
