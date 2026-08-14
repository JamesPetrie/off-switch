# Vendored: Pavona `prim_sha2` SHA-2 compression core

Third-party RTL imported from the Pavona project. The `.sv`/`.svh` files and
`LICENSE`/`NOTICE` in this directory are byte-identical copies of the
upstream sources. Do not edit them here — fixes belong in the off-switch
wrapper (`sha2_wrap.sv`), or in a re-vendor at a newer pinned commit, which
lands in a new `pavona_<shorthash>` directory.

- Upstream:  <https://github.com/pavona/pavona>
- Commit:    `fbdfde6335a15a5c4c4ad3accd43727135cd8cc7` (short: `fbdfde633`, in the directory name)
- Retrieved: 2026-08-10 (assert collateral 2026-08-14)
- License:   Apache-2.0 (`LICENSE` and `NOTICE`, copied from the upstream
  repository root; NOTICE is redistributed per Apache-2.0 section 4(d))

| File | Upstream path |
|------|---------------|
| `prim_sha2_pkg.sv` | `hw/ip/prim/rtl/prim_sha2_pkg.sv` |
| `prim_sha2_compression.sv` | `hw/ip/prim/rtl/prim_sha2_compression.sv` |
| `prim_assert.sv` | `hw/ip/prim/rtl/prim_assert.sv` |
| `prim_assert_dummy_macros.svh` | `hw/ip/prim/rtl/prim_assert_dummy_macros.svh` |
| `prim_assert_yosys_macros.svh` | `hw/ip/prim/rtl/prim_assert_yosys_macros.svh` |
| `prim_assert_standard_macros.svh` | `hw/ip/prim/rtl/prim_assert_standard_macros.svh` |
| `prim_assert_sec_cm.svh` | `hw/ip/prim/rtl/prim_assert_sec_cm.svh` |
| `prim_flop_macros.sv` | `hw/ip/prim/rtl/prim_flop_macros.sv` |
| `LICENSE` | `LICENSE` |
| `NOTICE` | `NOTICE` |

SHA-256 checksums (`sha256sum`):

```
1417eba6a77a539b96a8a37962de3edcb8ef71a673c2896317599946c37d5445  prim_sha2_pkg.sv
8768d377a9cf10a21fd6414bdf06862e5397abc956513b9473d1be5087724b32  prim_sha2_compression.sv
d717d5dbcba3b5aa8a731ef9f8af18b036b49edef282f0a43a51f5ad2dd9bb40  prim_assert.sv
cac4a930105da662547de873f0b80246074fb22df9a398663e1c8ac3e7998218  prim_assert_dummy_macros.svh
d1fd8c350785a7c6d8cccc0b0c385392b71575397e7dc58c70f07716c8f9f3a3  prim_assert_yosys_macros.svh
4835706249e017eae999256ea807c526a39a49729196fc0aa18bb8818de86ab6  prim_assert_standard_macros.svh
25db89fe5f250c1bcbd808d0b4808206b9e85ac337b26e4ebb23f2ad569e1a13  prim_assert_sec_cm.svh
2e8e6c2ee484899ae5d0020eaf0c9732c31537c60c30cb2a0a0acd2e92baee03  prim_flop_macros.sv
cfc7749b96f63bd31c3c42b5c471bf756814053e847c10f3eb003417bc523d30  LICENSE
d0dad4e68b54edf0b3c6dba179e2c0d3f497ce5b29204ec35570eb442170a9ed  NOTICE
```

To re-verify provenance against upstream:

```
curl -sL https://raw.githubusercontent.com/pavona/pavona/fbdfde6335a15a5c4c4ad3accd43727135cd8cc7/hw/ip/prim/rtl/prim_sha2_compression.sv \
  | diff - prim_sha2_compression.sv
```

Notes:

- `prim_assert.sv` provides the `` `ASSERT `` macros used by the compression
  core: three wipe-secret assertions (inactive here, `wipe_secret_i` is tied
  low) and one digest-mode validity assertion. The file selects its
  implementation per tool: dummy (empty) macros under Verilator/synthesis,
  yosys macros under yosys, full SVA otherwise — same behaviour upstream.
- Only the compression core is vendored. Message padding stays in off-switch
  RTL (`hss_verify.sv` supplies pre-padded 512-bit blocks), and the
  `MultimodeEn` parameter is left at 0 (SHA-256-only datapath) by the
  off-switch instantiation.
