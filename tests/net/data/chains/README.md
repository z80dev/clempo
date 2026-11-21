# Certificate chains

Captured 2026-11-21 with `openssl s_client -connect <host>:443 -servername <host> -showcerts`
(`<host>.pem`, the chain exactly as the server sent it). `<host>.root.pem` is the trust anchor
that chain validates to in the macOS system roots keychain
(`/System/Library/Keychains/SystemRootCertificates.keychain`), found by clempo's path builder
and cross-checked with `openssl x509 -subject` against the anchor OpenSSL reported.
The tests validate at a fixed time (2026-11-21), so expiry of these certificates does not
break them.

`../rfc8448.sexp` is extracted from RFC 8448 (sections 3, 5, 6, 7).
