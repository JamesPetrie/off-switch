# Add FORS sub-FSM (SPHINCS+-256f) inside renamed `hash_verify.sv`

## Context

`verilog/rtl/hss_verify.sv` currently implements RFC 8554 HSS-LMS
verification. The next signature scheme on the roadmap is SPHINCS+, whose
FORS (Forest Of Random Subsets) component is structurally a fan-out of
`K` Merkle trees, each verified leaf-up via an auth path — very close to
the existing per-chain WOTS walk and per-level Merkle climb. This change
lays down the **FORS verification block** so SPHINCS+ work can land
incrementally; the WOTS/Merkle reuse for the SPHINCS+ hypertree is a
follow-up.

Decisions confirmed with the user:

- **Module rename** `hss_verify.sv` → `hash_verify.sv` (and module
  `hss_verify` → `hash_verify`). TB filenames (`tb_hss.sv`,
  `tb_hss_sign_pkg.sv`, …) stay as-is — they exercise LMS.
- **New package** `verilog/rtl/sphincs_pkg.sv`, slotted in
  `own_files.vc` immediately after `hss_pkg.sv`. Both packages get
  imported into `hash_verify.sv`.
- **Scheme select** is a module *parameter* (`scheme_e SCHEME = LMS`),
  not a runtime port. Synth elaborates one branch and prunes the other.
- **Scope of this step**: FORS path RTL — sub-FSM + per-tree root store
  + roots-aggregation hash (`StFors` → `StForsRoot`). After
  `StForsRoot`, the FSM transitions into the existing `StWots`. In
  SPHINCS mode the rest of the pipeline runs end-to-end but produces
  meaningless output; this is flagged inline by a TODO. `verif_passed`
  is gated to `SCHEME == LMS`, so SPHINCS mode emits
  `verif_passed = 0`.
- **No TB / reference vectors this round.** Lint-only. TB and
  `test/reference_sphincs.py` arrive with the WOTS/hypertree
  unification work.

Outcome:

- `sphincs_pkg.sv` defines FORS params and `fors_license_t`.
- `hash_verify` exposes `SCHEME` (default `LMS`) and a `fors_license`
  port that LMS instantiations tie to `'0`.
- LMS mode is byte-identical to today's behaviour (same FSM, same
  outputs, same waveforms apart from instance-label noise).
- `make lint TOOL=verilator` clean; `make TB=hss sim` and
  `make TB=top_hss sim` continue to pass under the rename.

## Approach

### Per-tree FORS flow (mirrors WOTS' chain loop)

```
for i in 0..FORS_K-1:                # iterated by FORS sub-FSM, NOT main FSM
    leaf  = H(PUB_SEED || ADRS_FORS_TREE || i || q[i] || sig.sk[i])
    node  = leaf
    for lvl in 0..FORS_A-1:
        sib  = sig.auth[i][lvl]
        node = H(PUB_SEED || ADRS_FORS_TREE || i || parent_idx || L || R)
    roots[i] = node
# main FSM step after fors_complete: aggregate
hash_reg_q = H(PUB_SEED || ADRS_FORS_ROOTS || roots[0] || ... || roots[K-1])
# then fall through to StWots (TODO: WOTS not yet SPHINCS-ready)
```

The `K` outer loop and the per-tree leaf hashing live entirely inside
the FORS sub-FSM, exactly as the per-chain loop and per-step hashing
live inside the WOTS sub-FSM. The main FSM only adds two new states:
`StFors` (delegates to the sub-FSM, awaits `fors_complete`) and
`StForsRoot` (single multi-block hash that aggregates the K stored
roots into `hash_reg_q`).

### Critical files

#### 1. [verilog/rtl/sphincs_pkg.sv](verilog/rtl/sphincs_pkg.sv) — NEW

```systemverilog
package sphincs_pkg;
    import arith_pkg::*;   // brings in WIDTH = 256

    // Verification scheme selector (used as a parameter type on hash_verify)
    typedef enum logic [0:0] { LMS = 1'b0, SPHINCS = 1'b1 } scheme_e;

    // SPHINCS+-256f FORS parameters
    localparam int unsigned FORS_K          = 35;
    localparam int unsigned FORS_A          = 9;
    localparam int unsigned FORS_T          = 1 << FORS_A;             // 512
    localparam int unsigned FORS_TREE_CNT_W = $clog2(FORS_K);          // 6
    localparam int unsigned FORS_LVL_W      = $clog2(FORS_A + 1);      // 4
    localparam int unsigned FORS_NODE_W     = FORS_A + 1;              // 10 (heap idx)

    // Public seed (placeholder; finalised when reference generator lands)
    localparam logic [WIDTH-1:0] PUB_SEED = 256'd0;

    // ADRS-type tags used by FORS hashing.
    // NOTE: this is a flat substitute, NOT the canonical 22-/32-byte ADRS.
    // Test-only deviation; revisit when SPHINCS+ interop becomes a goal.
    localparam logic [31:0] ADRS_FORS_TREE  = 32'd3;
    localparam logic [31:0] ADRS_FORS_ROOTS = 32'd4;

    // FORS signature: per-tree sk element + auth path + leaf index.
    typedef struct packed {
        logic [FORS_K-1:0]                [WIDTH-1:0]  sk;
        logic [FORS_K-1:0][FORS_A-1:0]    [WIDTH-1:0]  auth;
        logic [FORS_K-1:0]                [FORS_A-1:0] q;
    } fors_license_t;
endpackage
```

#### 2. Rename `verilog/rtl/hss_verify.sv` → `verilog/rtl/hash_verify.sv`

`git mv` the file, then update **all four** references. The Vivado
`.Xil/...security_block.tcl` file is a generated cache artefact and is
left alone.

| Site | Action |
|---|---|
| [verilog/rtl/hss_verify.sv:27](verilog/rtl/hss_verify.sv#L27) | `module hss_verify` → `module hash_verify` |
| [verilog/rtl/security_block.sv:138](verilog/rtl/security_block.sv#L138) | `hss_verify u_hss` → `hash_verify u_hss` (instance label `u_hss` left alone — keeps waveform paths stable) |
| [verilog/tb/tb_hss.sv:32](verilog/tb/tb_hss.sv#L32) | `hss_verify u_dut` → `hash_verify u_dut` |
| [verilog/tb/tb_hss_sign_pkg.sv:3](verilog/tb/tb_hss_sign_pkg.sv#L3) | comment "mirrors hss_verify.sv" → "mirrors hash_verify.sv" |
| [verilog/rtl/own_files.vc:12](verilog/rtl/own_files.vc#L12) | `./hss_verify.sv` → `./hash_verify.sv` |

#### 3. [verilog/rtl/hash_verify.sv](verilog/rtl/hash_verify.sv) — FORS additions

##### Imports & parameter

Add `sphincs_pkg::*;` to the import block and a `SCHEME` parameter:

```systemverilog
module hash_verify
    import arith_pkg::*;
    import hss_pkg::*;
    import sphincs_pkg::*;
#(
    parameter scheme_e SCHEME = LMS
) (
    // existing LMS ports unchanged ...
    input  fors_license_t  fors_license   // tied to '0 in LMS instances
);
```

`fors_license` always exists on the port list (parametric ports are
fiddly in SystemVerilog); LMS instantiations tie it to `'0` and synth
prunes the load.

##### Main-FSM enum (already 4 bits wide → room for 9 of 16)

[hss_verify.sv:45-47](verilog/rtl/hss_verify.sv#L45-L47) currently:

```systemverilog
typedef enum logic [3:0] {
    StIdle, StQ, StWots, StKc, StLeaf, StMerkle, StDone
} seq_state_e;
```

Extend to:

```systemverilog
typedef enum logic [3:0] {
    StIdle, StQ, StWots, StKc, StLeaf, StMerkle,  // existing LMS path
    StFors, StForsRoot,                            // NEW (SPHINCS path)
    StDone
} seq_state_e;
```

##### FORS sub-FSM (peer block to the existing WOTS sub-FSM ~L440-504 and Merkle sub-FSM ~L510-550)

```systemverilog
typedef enum logic [1:0] {
    StForsInit, StForsLeaf, StForsHash, StForsRootStore
} fors_state_e;

fors_state_e                fors_q,        fors_d;
logic [FORS_TREE_CNT_W-1:0] fors_tree_q,   fors_tree_d;     // 0..K-1
logic [FORS_LVL_W-1:0]      fors_level_q,  fors_level_d;    // 0..A
logic [FORS_NODE_W-1:0]     fors_node_q,   fors_node_d;     // heap-encoded node
logic [WIDTH-1:0]           fors_roots_q [FORS_K];          // unpacked
logic [WIDTH-1:0]           fors_roots_d [FORS_K];
logic                       fors_sha_valid, fors_complete;
```

| State | Behaviour |
|---|---|
| `StForsInit` | Set `fors_node_d = (1<<FORS_A) \| fors_license.q[fors_tree_q]`, `fors_level_d = 0`. → `StForsLeaf`. |
| `StForsLeaf` | Drive `fors_sha_valid=1` with `FORS_LEAF_DATA`. On `hash_complete` → `StForsHash`. `hash_reg_q` now holds the leaf hash. |
| `StForsHash` | Drive `fors_sha_valid=1` with `FORS_NODE_DATA`. On `hash_complete`: if `fors_level_q == FORS_A-1` → `StForsRootStore`; else `fors_level_d = fors_level_q + 1`, `fors_node_d = fors_node_q >> 1`. |
| `StForsRootStore` | `fors_roots_d[fors_tree_q] = hash_reg_q`. If `fors_tree_q == FORS_K-1`: assert `fors_complete`, → `StForsInit`. Else `fors_tree_d = fors_tree_q + 1`, → `StForsInit` (which re-seeds node/level). |

Auth-path helpers (parallel to the LMS Merkle helpers at
[hss_verify.sv:288-297](verilog/rtl/hss_verify.sv#L288-L297) — kept
separate so the auth-path mux isn't shared between LMS and FORS):

```systemverilog
wire [FORS_NODE_W-2:0] fors_parent   = fors_node_q >> 1;
wire                   fors_is_right = fors_node_q[0];
wire [WIDTH-1:0]       fors_sib      = fors_license.auth[fors_tree_q][fors_level_q];
logic [WIDTH-1:0]      fors_l, fors_r;
assign {fors_l, fors_r} = fors_is_right ? {fors_sib, hash_reg_q}
                                        : {hash_reg_q, fors_sib};
```

##### Hash payloads

Three new `*_DATA` macros, in the same shape as `Q_*` /
`WOTS_DATA` / `MRKL_DATA` (see
[hss_verify.sv:220-303](verilog/rtl/hss_verify.sv#L220-L303)). Note
that LMS macros prefix with `cur_I` (a 128-bit per-layer identifier)
while SPHINCS uses the static `PUB_SEED` (256 bits) — same concat
shape, different model.

```systemverilog
// Leaf:        H(PUB_SEED || ADRS_FORS_TREE  || tree_idx || q[tree]      || sk[tree])
// Internal:    H(PUB_SEED || ADRS_FORS_TREE  || tree_idx || parent_idx   || L || R)
// Aggregation: H(PUB_SEED || ADRS_FORS_ROOTS || roots[0] || ... || roots[K-1])

`define FORS_LEAF_DATA  {PUB_SEED, ADRS_FORS_TREE,  32'(fors_tree_q), \
                         32'(fors_license.q[fors_tree_q]), fors_license.sk[fors_tree_q]}
`define FORS_NODE_DATA  {PUB_SEED, ADRS_FORS_TREE,  32'(fors_tree_q), \
                         32'(fors_parent), fors_l, fors_r}
`define FORS_ROOTS_DATA {PUB_SEED, ADRS_FORS_ROOTS, fors_roots_concat}
```

`fors_roots_concat` is a `FORS_K*WIDTH`-bit packed bitvector built in
an `always_comb` block exactly mirroring the existing `pk_concat`
shift-and-accumulate at
[hss_verify.sv:178-186](verilog/rtl/hss_verify.sv#L178-L186):

```systemverilog
logic [FORS_K*WIDTH-1:0] fors_roots_concat;
always_comb begin
    logic [WIDTH-1:0] roots_discard;
    fors_roots_concat = '0;
    for (int i = 0; i < FORS_K; i++) begin
        {roots_discard, fors_roots_concat} = {fors_roots_concat, fors_roots_q[i]};
    end
end
```

##### Block-count localparams

Each FORS payload exceeds 447 bits, so all three are **multi-block**.
Use the existing
[`calc_sha_blocks` / `calc_sha_pad_zeros`](verilog/rtl/hss_verify.sv#L203-L208)
file-local helpers, identical to `MRKL_BLOCKS` / `MRKL_PAD_ZEROS` /
`mrkl_padded`:

```systemverilog
localparam int unsigned FORS_LEAF_BITS  = WIDTH + 32 + 32 + 32 + WIDTH;          // 608
localparam int unsigned FORS_NODE_BITS  = WIDTH + 32 + 32 + 32 + WIDTH + WIDTH;  // 864
localparam int unsigned FORS_ROOTS_BITS = WIDTH + 32 + FORS_K * WIDTH;           // 9248

localparam int unsigned FORS_LEAF_BLOCKS  = calc_sha_blocks(FORS_LEAF_BITS);    //  2
localparam int unsigned FORS_NODE_BLOCKS  = calc_sha_blocks(FORS_NODE_BITS);    //  2
localparam int unsigned FORS_ROOTS_BLOCKS = calc_sha_blocks(FORS_ROOTS_BITS);   // 19

// pad-zero localparams + padded-vector wires defined the same way
// MRKL_PAD_ZEROS / mrkl_padded are.
```

##### Block-select case (extends the always_comb at [hss_verify.sv:349-391](verilog/rtl/hss_verify.sv#L349-L391))

Use the same `<<blk_shift` shift-mux pattern that `StMerkle` uses.
**The plan's earlier draft said "1 block" for `StForsLeaf`/`StForsHash`
— that's wrong; both are `FORS_*_BLOCKS`-many.**

```systemverilog
StFors: begin
    case (fors_q)
        StForsLeaf: begin
            num_blocks = FORS_LEAF_BLOCKS;
            {sha_block, fors_leaf_discard} = {fors_leaf_padded, 512'b0} << blk_shift;
        end
        StForsHash: begin
            num_blocks = FORS_NODE_BLOCKS;
            {sha_block, fors_node_discard} = {fors_node_padded, 512'b0} << blk_shift;
        end
        default:    ;     // Init / RootStore don't issue a hash
    endcase
end
StForsRoot: begin
    num_blocks = FORS_ROOTS_BLOCKS;
    {sha_block, fors_roots_discard} = {fors_roots_padded, 512'b0} << blk_shift;
end
```

##### `hash_reg_q` capture path — no changes needed

[hss_verify.sv:398](verilog/rtl/hss_verify.sv#L398):
`hash_reg_d = (!wots_loading) ? sha_digest : cur_sig_chain;`
Outside StWots/StWotsLoad, `hash_reg_q` always captures `sha_digest`
on `hash_complete`. FORS gets this for free; no extra
register-enable terms. After `StForsRoot` completes, `hash_reg_q`
holds the FORS public key.

##### Main FSM additions ([hss_verify.sv:565-622](verilog/rtl/hss_verify.sv#L565-L622))

```systemverilog
StIdle: if (valid) begin
    if (SCHEME == LMS) begin
        layer_d = LAYER_CNT_W'(HSS_LEVELS - 1);
        seq_d   = StQ;
    end else begin // SPHINCS
        seq_d   = StFors;
    end
end
StFors: begin
    sha_valid = fors_sha_valid;
    if (fors_complete) seq_d = StForsRoot;
end
StForsRoot: begin
    sha_valid = 1'b1;
    if (hash_complete) begin
        // hash_reg_q now carries the FORS public key. Fall through into
        // the existing WOTS pipeline as the SPHINCS+ continuation.
        // TODO(SPHINCS+): the WOTS / Kc / Leaf / Merkle states still
        //   reference hss_pkg license fields and LMS-specific addressing.
        //   They will produce meaningless results in SPHINCS mode until
        //   adapted in a follow-up plan that introduces the SPHINCS+
        //   hypertree signature inputs.
        seq_d = StWots;
    end
end
```

The existing `StWots` → `StKc` → `StLeaf` → `StMerkle` → `StDone`
path stays exactly as-is. `StDone`'s LMS compare gets a SCHEME gate:

```systemverilog
StDone: begin
    ready        = 1'b1;
    verif_passed = (SCHEME == LMS) && (hash_reg_q == ROOT_PUB_KEY);
    seq_d        = StIdle;
end
```

(SPHINCS mode currently emits `verif_passed = 0`; flagged by the same
TODO above.)

##### Sequential block ([hss_verify.sv:632-654](verilog/rtl/hss_verify.sv#L632-L654))

Extend reset list with FORS state (`fors_q ← StForsInit`,
`fors_tree_q`, `fors_level_q`, `fors_node_q` to `'0`,
`fors_roots_q[*]` to `'0`) and feed the corresponding `_d` nexts in
the non-reset branch.

#### 4. [verilog/rtl/security_block.sv](verilog/rtl/security_block.sv) (line 138)

- `hss_verify u_hss (...)` → `hash_verify u_hss (...)`. No `SCHEME`
  override → defaults to `LMS`.
- Add `import sphincs_pkg::*;` so `fors_license_t` is in scope.
- Add `.fors_license ('0)` to the port list. `LICENSE_W` calc
  ([L24-L25](verilog/rtl/security_block.sv#L24-L25)) does **not**
  change — SPHINCS isn't a runtime crypto type yet.

#### 5. [verilog/tb/tb_hss.sv](verilog/tb/tb_hss.sv) (line 32)

- `hss_verify u_dut (...)` → `hash_verify u_dut (...)`.
- Add `.fors_license ('0)` to the port list. (Add
  `import sphincs_pkg::*;` if `fors_license_t` is referenced
  explicitly; with `'0` literal it's not strictly needed, but adding
  it keeps things tidy.)

#### 6. [verilog/rtl/own_files.vc](verilog/rtl/own_files.vc)

- Replace `./hss_verify.sv` with `./hash_verify.sv` (line 12).
- Insert `./sphincs_pkg.sv` immediately after `./hss_pkg.sv`
  (line 11) — user-confirmed slot.

No new TB target this round; `Makefile` itself is unchanged.

### Things explicitly NOT in this plan

- ADRS encoding fidelity — using a flat
  `{tree_idx, parent_idx}` instead of the canonical SPHINCS+ ADRS
  layout. TODO when interop matters.
- Adapting `Q` / `WOTS` / `Kc` / `Leaf` / `Merkle` to SPHINCS+ inputs.
  Until that lands, SPHINCS mode after `StForsRoot` produces
  meaningless results downstream — flagged inline by the TODO above.
- `test/reference_sphincs.py`, `tb_fors.sv`, FORS DPI signer.
- Renaming `tb_hss*` files.

## Verification

- `cd verilog && make lint TOOL=verilator` — clean lint (FORS state
  and signals all driven; new package compiles; rename consistent
  across RTL + TB + own_files.vc).
- `cd verilog && make TB=hss sim` and `make TB=top_hss sim` — still
  pass. The LMS path is byte-identical; only the wrapper instantiation
  lines and module name changed.
- No FORS functional verification this round — that lands with the
  follow-up plan that adapts WOTS / hypertree for SPHINCS+ and adds
  the reference + TB.
