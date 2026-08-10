# Vendored: Pavona `prim_sha2` SHA-2 compression core

Third-party RTL imported from the Pavona project. The `.sv` files and
`LICENSE` in this directory are byte-identical copies of the upstream
sources. Do not edit them here — fixes belong in the off-switch wrapper
(`sha2_wrap.sv`), or in a re-vendor at a newer pinned commit that updates
this README.

- Upstream:  <https://github.com/pavona/pavona>
- Commit:    `fbdfde6335a15a5c4c4ad3accd43727135cd8cc7`
- Retrieved: 2026-08-10
- License:   Apache-2.0 (`LICENSE` and `NOTICE`, copied from the upstream
  repository root; NOTICE is redistributed per Apache-2.0 section 4(d))

| File | Upstream path |
|------|---------------|
| `prim_sha2_pkg.sv` | `hw/ip/prim/rtl/prim_sha2_pkg.sv` |
| `prim_sha2_compression.sv` | `hw/ip/prim/rtl/prim_sha2_compression.sv` |
| `LICENSE` | `LICENSE` |
| `NOTICE` | `NOTICE` |

SHA-256 checksums (`sha256sum`):

```
1417eba6a77a539b96a8a37962de3edcb8ef71a673c2896317599946c37d5445  prim_sha2_pkg.sv
8768d377a9cf10a21fd6414bdf06862e5397abc956513b9473d1be5087724b32  prim_sha2_compression.sv
cfc7749b96f63bd31c3c42b5c471bf756814053e847c10f3eb003417bc523d30  LICENSE
d0dad4e68b54edf0b3c6dba179e2c0d3f497ce5b29204ec35570eb442170a9ed  NOTICE
```

To re-verify provenance against upstream:

```
curl -sL https://raw.githubusercontent.com/pavona/pavona/fbdfde6335a15a5c4c4ad3accd43727135cd8cc7/hw/ip/prim/rtl/prim_sha2_compression.sv \
  | diff - prim_sha2_compression.sv
```

Notes:

- `prim_sha2_compression.sv` contains `` `include "prim_assert.sv" ``. In
  this repository that include is satisfied by the local stub
  `verilog/rtl/prim_assert.sv` (an off-switch file, not from upstream),
  which defines the `` `ASSERT `` macro away. Synthesized/simulated RTL
  behaviour is unaffected; off-switch simulation assertions live in
  `verilog/tb/rtl_sva.sv`.
- Only the compression core is vendored. Message padding stays in off-switch
  RTL (`hss_verify.sv` supplies pre-padded 512-bit blocks), and the
  `MultimodeEn` parameter is left at 0 (SHA-256-only datapath) by the
  off-switch instantiation.
