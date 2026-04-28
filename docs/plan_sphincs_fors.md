# Add FORS sub-FSM (SPHINCS+-256f) inside renamed `hash_verify.sv`

## Context

`verilog/rtl/hss_verify.sv` currently implements RFC 8554 HSS-LMS
verification. The next signature scheme on the roadmap is SPHINCS+,
whose FORS (Forest Of Random Subsets) component is structurally a
fan-out of `K` Merkle trees, each verified leaf-up via an auth path —
very close to the existing per-chain WOTS walk and per-level Merkle
climb. This change lays down the **FORS verification block** so
SPHINCS+ work can land incrementally; the SPHINCS+ hypertree (built on
the existing WOTS / Kc / Leaf / Merkle pipeline) is a follow-up.

Decisions confirmed with the user (post-review):

- **Module rename** `hss_verify.sv` → `hash_verify.sv` (and module
  `hss_verify` → `hash_verify`). TB filenames (`tb_hss.sv`,
  `tb_hss_sign_pkg.sv`, …) stay as-is — they exercise LMS.
- **New package** `verilog/rtl/sphincs_pkg.sv`, slotted in
  `own_files.vc` immediately after `hss_pkg.sv`. Both packages get
  imported into `hash_verify.sv`. The package contains FORS params
  and `fors_license_t` only; the scheme selector lives in
  `hash_verify` itself.
- **Scheme select** is a plain `parameter bit SCHEME` on
  `hash_verify` (`0 = LMS`, `1 = SPHINCS`), modelled exactly on
  `security_block.CRYPTO_TYPE`.
- **Single license port** (no separate `fors_license`). Width is
  `LICENSE_W = SCHEME ? $bits(fors_license_t) : $bits(license_t)`,
  same shape security_block uses for its `license` bus. Internal
  typed views are declared via a `generate` block.
- **Storage reuse**: the existing `pk_store_q` / `pk_concat`
  bookkeeping is shared between WOTS chain pks (LMS) and FORS roots
  (SPHINCS) — depth widens to `max(WOTS_P, FORS_K)` = `35`.
- **q comes from the Q hash, not the signature** — in SPHINCS+ the
  K leaf-indices into the FORS trees are derived from the message
  digest (mirroring how LMS's `q_digits` are derived from `aux_reg_q`).
  `fors_license_t` carries only `sk` and `auth`.
- **Unified Idle/Q init**: SPHINCS also starts with hashing the
  message (a "slightly different" Q variant) and will eventually run
  multiple WOTS layers like HSS. So `StIdle` and `StQ` stay scheme-
  agnostic; the dispatch happens *after* `StQ` (LMS → `StWots`,
  SPHINCS → `StFors`). The Q payload selection inside the block-mux
  picks `Q_SPHINCS_DATA` when `SCHEME == 1`.
- **Scope of this step**: FORS path RTL only — sub-FSM + per-tree pk
  store + `KC_FORS_DATA` aggregation (`StFors` → `StForsKc`). After
  `StForsKc`, the FSM transitions into the existing `StWots` to set
  up the SPHINCS hypertree slot; that path produces meaningless
  output until the follow-up plan adapts WOTS to SPHINCS+ inputs
  (TODO comment in code). `StDone`'s
  `verif_passed = (hash_reg_q == ROOT_PUB_KEY)` stays unchanged — no
  SCHEME gate.
- **No TB / reference vectors this round.** Lint-only. TB and
  `test/reference_sphincs.py` arrive with the WOTS / hypertree
  unification work.

Outcome:

- `sphincs_pkg.sv` defines FORS params and `fors_license_t` (sk +
  auth — no q).
- `hash_verify` exposes `parameter bit SCHEME = 1'b0` and a single
  parametric `license` port.
- LMS mode is byte-identical to today's behaviour modulo the state-
  enum rename `StKc` → `StWotsKc` (no functional change).
- `make lint TOOL=verilator` clean; `make TB=hss sim` and
  `make TB=top_hss sim` continue to pass under the rename.

## Approach

### Per-tree FORS flow (mirrors WOTS' chain loop)

```
# After StQ, hash_reg_q = Q hash; aux_reg captures it on entry to StFors.
# fors_q_idx[i] = aux_reg_q sliced into K chunks of A bits each
#                 (TODO: SPHINCS+ uses hash extension; placeholder for now).

for i in 0..FORS_K-1:                # iterated by FORS sub-FSM, NOT main FSM
    leaf  = H(PUB_SEED || ADRS_FORS_TREE || i || fors_q_idx[i] || sig.sk[i])
    node  = leaf
    for lvl in 0..FORS_A-1:
        sib  = sig.auth[i][lvl]
        node = H(PUB_SEED || ADRS_FORS_TREE || i || parent_idx || L || R)
    pk_store[i] = node               # reuses LMS pk_store array
# main FSM step after fors_complete:
hash_reg_q = H(PUB_SEED || ADRS_FORS_ROOTS || pk_store[0] || ... || pk_store[K-1])
# then fall through to StWots (TODO: SPHINCS hypertree not yet wired in)
```

### Critical files

#### 1. [verilog/rtl/sphincs_pkg.sv](verilog/rtl/sphincs_pkg.sv) — NEW

```systemverilog
package sphincs_pkg;
    import arith_pkg::*;   // brings in WIDTH = 256

    // SPHINCS+-256f FORS parameters
    localparam int unsigned FORS_K          = 35;
    localparam int unsigned FORS_A          = 9;
    localparam int unsigned FORS_T          = 1 << FORS_A;        // 512
    localparam int unsigned FORS_TREE_CNT_W = $clog2(FORS_K);     // 6
    localparam int unsigned FORS_LVL_W      = $clog2(FORS_A + 1); // 4
    localparam int unsigned FORS_NODE_W     = FORS_A + 1;         // 10 (heap idx)

    // Public seed (placeholder; finalised when reference generator lands)
    localparam logic [WIDTH-1:0] PUB_SEED = 256'd0;

    // ADRS-type tags used by FORS hashing.
    // NOTE: this is a flat substitute, NOT the canonical 22-/32-byte ADRS.
    // Test-only deviation; revisit when SPHINCS+ interop becomes a goal.
    localparam logic [31:0] ADRS_FORS_TREE  = 32'd3;
    localparam logic [31:0] ADRS_FORS_ROOTS = 32'd4;

    // FORS signature: per-tree sk element + auth path. q is derived from
    // the message hash (Q step), not part of the signature.
    typedef struct packed {
        logic [FORS_K-1:0]                 [WIDTH-1:0]  sk;
        logic [FORS_K-1:0][FORS_A-1:0]     [WIDTH-1:0]  auth;
    } fors_license_t;
endpackage
```

#### 2. Rename `verilog/rtl/hss_verify.sv` → `verilog/rtl/hash_verify.sv`

`git mv` the file, then update all references. The Vivado
`.Xil/...security_block.tcl` file is a generated cache artefact and is
left alone.

| Site | Action |
|---|---|
| `verilog/rtl/hss_verify.sv:27` | `module hss_verify` → `module hash_verify` |
| `verilog/rtl/security_block.sv:138` | `hss_verify u_hss` → `hash_verify u_hss` (instance label `u_hss` left alone — keeps waveform paths stable) |
| `verilog/tb/tb_hss.sv:32` | `hss_verify u_dut` → `hash_verify u_dut` |
| `verilog/tb/tb_hss_sign_pkg.sv:3` | comment "mirrors hss_verify.sv" → "mirrors hash_verify.sv" |
| `verilog/rtl/own_files.vc:12` | `./hss_verify.sv` → `./hash_verify.sv` |

#### 3. [verilog/rtl/hash_verify.sv](verilog/rtl/hash_verify.sv) — FORS additions

##### Imports, parameter, and parametric license port

Modelled on `security_block.sv` ([L24-L25, L119-L147](verilog/rtl/security_block.sv#L24-L147)):

```systemverilog
module hash_verify
    import arith_pkg::*;
    import hss_pkg::*;
    import sphincs_pkg::*;
#(
    parameter bit SCHEME = 1'b0,    // 0 = LMS, 1 = SPHINCS

    localparam int unsigned LICENSE_W = SCHEME ? $bits(fors_license_t)
                                               : $bits(license_t)
) (
    input  logic                  clk,
    input  logic                  rst_n,
    input  logic                  valid,
    input  logic [WIDTH-1:0]      message,
    input  logic [LICENSE_W-1:0]  license,

    output logic                  ready,
    output logic                  verif_passed
);
```

Inside the module, two typed views are exposed via `generate`, exactly
mirroring `security_block`'s `g_ecdsa` / `g_hss_lms` pattern. References
elsewhere in the module read from whichever view is active:

```systemverilog
generate
    if (SCHEME == 1'b0) begin : g_lms
        license_t lms_license;
        assign lms_license = license;
    end else begin : g_sphincs
        fors_license_t fors_license;
        assign fors_license = license;
    end
endgenerate
```

The unused branch is pruned at elaboration; only one license type is
ever live in a given instance.

##### Main-FSM enum (already 4 bits wide; renames `StKc` → `StWotsKc`, adds two)

[hss_verify.sv:45-47](verilog/rtl/hss_verify.sv#L45-L47) currently:

```systemverilog
typedef enum logic [3:0] {
    StIdle, StQ, StWots, StKc, StLeaf, StMerkle, StDone
} seq_state_e;
```

Becomes:

```systemverilog
typedef enum logic [3:0] {
    StIdle, StQ,
    StWots, StWotsKc, StLeaf, StMerkle,   // LMS / SPHINCS-hypertree path
    StFors, StForsKc,                      // SPHINCS FORS path
    StDone
} seq_state_e;
```

`StKc` → `StWotsKc` is a pure rename (no behavioural change). `StForsKc`
replaces what the v1 plan called `StForsRoot` and is the per-FORS roots
aggregation hash.

##### FORS sub-FSM (peer block to the existing WOTS sub-FSM ~L440-504 and Merkle sub-FSM ~L510-550)

```systemverilog
typedef enum logic [1:0] {
    StForsInit, StForsLeaf, StForsHash, StForsPkStore
} fors_state_e;

fors_state_e                fors_q,        fors_d;
logic [FORS_TREE_CNT_W-1:0] fors_tree_q,   fors_tree_d;     // 0..K-1
logic [FORS_LVL_W-1:0]      fors_level_q,  fors_level_d;    // 0..A
logic [FORS_NODE_W-1:0]     fors_node_q,   fors_node_d;     // heap-encoded node
logic                       fors_sha_valid, fors_complete;
```

No new pk storage — `pk_store_q` is reused (see "Shared pk store"
below).

| State | Behaviour |
|---|---|
| `StForsInit` | Set `fors_node_d = (1<<FORS_A) \| fors_q_idx[fors_tree_q]`, `fors_level_d = 0`. → `StForsLeaf`. |
| `StForsLeaf` | Drive `fors_sha_valid=1` with `FORS_LEAF_DATA`. On `hash_complete` → `StForsHash`. `hash_reg_q` now holds the leaf hash. |
| `StForsHash` | Drive `fors_sha_valid=1` with `FORS_NODE_DATA`. On `hash_complete`: if `fors_level_q == FORS_A-1` → `StForsPkStore`; else `fors_level_d = fors_level_q + 1`, `fors_node_d = fors_node_q >> 1`. |
| `StForsPkStore` | `pk_store_d[fors_tree_q] = hash_reg_q` (reusing the LMS pk array). If `fors_tree_q == FORS_K-1`: assert `fors_complete`, → `StForsInit`. Else `fors_tree_d = fors_tree_q + 1`, → `StForsInit`. |

Auth-path helpers (parallel to the LMS Merkle helpers at
[hss_verify.sv:288-297](verilog/rtl/hss_verify.sv#L288-L297) — kept
separate so the auth-path mux isn't shared between LMS and FORS):

```systemverilog
wire [FORS_NODE_W-2:0] fors_parent   = fors_node_q >> 1;
wire                   fors_is_right = fors_node_q[0];
wire [WIDTH-1:0]       fors_sib      = g_sphincs.fors_license.auth[fors_tree_q][fors_level_q];
logic [WIDTH-1:0]      fors_l, fors_r;
assign {fors_l, fors_r} = fors_is_right ? {fors_sib, hash_reg_q}
                                        : {hash_reg_q, fors_sib};
```

##### `fors_q_idx` — derived from the Q hash

The K FORS leaf indices come from the message digest, not the
signature. Mirrors how LMS's `q_digits` are derived combinationally
from `aux_reg_q` ([hss_verify.sv:144-170](verilog/rtl/hss_verify.sv#L144-L170)):

```systemverilog
logic [FORS_A-1:0] fors_q_idx [FORS_K];

always_comb begin
    logic [WIDTH-1:0] hash;
    hash = aux_reg_q;                       // captured at StFors entry
    for (int i = 0; i < FORS_K; i++) begin
        // NOTE: SPHINCS+ formally extends the message hash via MGF1-style
        // expansion (K*A = 315 bits > 256). For the FORS skeleton this
        // takes low-order chunks of aux_reg_q with wraparound; full
        // hash-extension is a follow-up. Correctness check waits for the
        // SPHINCS+ reference and TB.
        fors_q_idx[i] = hash[(i * FORS_A) % WIDTH +: FORS_A];
    end
end
```

`aux_reg_q` already captures `hash_reg_q` on `wots_init`
([hss_verify.sv:414-422](verilog/rtl/hss_verify.sv#L414-L422)); the
sequential block needs to also capture on `(seq_q == StFors) &&
(fors_q == StForsInit)` (gated by `fors_init` analogous to `wots_init`).

##### Hash payloads

Three new `*_DATA` macros, in the same shape as `Q_*` /
`WOTS_DATA` / `KC_DATA` (see
[hss_verify.sv:220-303](verilog/rtl/hss_verify.sv#L220-L303)). The
roots-aggregation macro is named `KC_FORS_DATA` (peer to the existing
`KC_DATA` which becomes `KC_WOTS_DATA` for symmetry):

```systemverilog
// Leaf:        H(PUB_SEED || ADRS_FORS_TREE  || tree_idx || fors_q_idx[tree] || sk[tree])
// Internal:    H(PUB_SEED || ADRS_FORS_TREE  || tree_idx || parent_idx       || L || R)
// Aggregation: H(PUB_SEED || ADRS_FORS_ROOTS || pk[0] || ... || pk[K-1])

`define FORS_LEAF_DATA  {PUB_SEED, ADRS_FORS_TREE,  32'(fors_tree_q), \
                         32'(fors_q_idx[fors_tree_q]),                \
                         g_sphincs.fors_license.sk[fors_tree_q]}
`define FORS_NODE_DATA  {PUB_SEED, ADRS_FORS_TREE,  32'(fors_tree_q), \
                         32'(fors_parent), fors_l, fors_r}
`define KC_FORS_DATA    {PUB_SEED, ADRS_FORS_ROOTS, pk_concat}

wire [$bits(`FORS_LEAF_DATA)-1 : 0] fors_leaf_data = `FORS_LEAF_DATA;
wire [$bits(`FORS_NODE_DATA)-1 : 0] fors_node_data = `FORS_NODE_DATA;
wire [$bits(`KC_FORS_DATA)-1   : 0] kc_fors_data   = `KC_FORS_DATA;
```

The existing `KC_DATA` (LMS WOTS roots aggregation) is renamed
`KC_WOTS_DATA` for clarity alongside `KC_FORS_DATA`.

##### Block sizes — use `$bits` like the existing macros do

Match the style used by the existing `Q_*` / `WOTS` / `KC` /
`LEAF` / `MRKL` blocks ([hss_verify.sv:231-310](verilog/rtl/hss_verify.sv#L231-L310)):

```systemverilog
localparam int unsigned FORS_LEAF_BLOCKS  = calc_sha_blocks  ($bits(fors_leaf_data));
localparam int unsigned FORS_LEAF_PAD_Z   = calc_sha_pad_zeros($bits(fors_leaf_data));
localparam int unsigned FORS_NODE_BLOCKS  = calc_sha_blocks  ($bits(fors_node_data));
localparam int unsigned FORS_NODE_PAD_Z   = calc_sha_pad_zeros($bits(fors_node_data));
localparam int unsigned KC_FORS_BLOCKS    = calc_sha_blocks  ($bits(kc_fors_data));
localparam int unsigned KC_FORS_PAD_Z     = calc_sha_pad_zeros($bits(kc_fors_data));

wire [FORS_LEAF_BLOCKS*512-1:0] fors_leaf_padded =
        {fors_leaf_data, 1'b1, {FORS_LEAF_PAD_Z{1'b0}}, 64'($bits(fors_leaf_data))};
wire [FORS_NODE_BLOCKS*512-1:0] fors_node_padded =
        {fors_node_data, 1'b1, {FORS_NODE_PAD_Z{1'b0}}, 64'($bits(fors_node_data))};
wire [KC_FORS_BLOCKS*512-1:0]   kc_fors_padded   =
        {kc_fors_data,   1'b1, {KC_FORS_PAD_Z{1'b0}},   64'($bits(kc_fors_data))};
```

##### Shared pk store

Widen `pk_store_q` / `pk_store_d` from `[WOTS_P]` (= 34) to
`[PK_STORE_DEPTH]` where `PK_STORE_DEPTH = (FORS_K > WOTS_P) ?
FORS_K : WOTS_P` (= 35). Likewise widen `pk_concat` to
`PK_STORE_DEPTH * WIDTH` and the `pk_concat` build-loop to
`PK_STORE_DEPTH`. The reset loop and sequential update loop already
iterate `0..WOTS_P` and become `0..PK_STORE_DEPTH`. The LMS
`KC_WOTS_DATA` slices the first `WOTS_P*WIDTH` bits of `pk_concat`;
`KC_FORS_DATA` slices the first `FORS_K*WIDTH` bits. (Both use the
high bits because `pk_concat` is left-shift accumulated.)

##### Block-select case (extends the always_comb at [hss_verify.sv:349-391](verilog/rtl/hss_verify.sv#L349-L391))

`StQ` gains a SCHEME branch for the SPHINCS Q variant
(`Q_SPHINCS_DATA` — a TODO placeholder until the SPHINCS+ message-
hash recipe lands). FORS adds three branches following the
`<<blk_shift` shift-mux pattern that `StMerkle` uses:

```systemverilog
StQ: begin
    if (SCHEME == 1'b1) begin
        num_blocks = Q_SPHINCS_BLOCKS;       // TODO: define when recipe lands
        {sha_block, q_sphincs_discard} = {q_sphincs_padded, 512'b0} << blk_shift;
    end else if (is_msg_layer) begin /* unchanged */ end
    else                          { /* unchanged */ }
end
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
        default:    ;     // Init / PkStore don't issue a hash
    endcase
end
StForsKc: begin
    num_blocks = KC_FORS_BLOCKS;
    {sha_block, kc_fors_discard} = {kc_fors_padded, 512'b0} << blk_shift;
end
```

##### `hash_reg_q` capture path — no changes needed

[hss_verify.sv:398](verilog/rtl/hss_verify.sv#L398):
`hash_reg_d = (!wots_loading) ? sha_digest : cur_sig_chain;`
Outside StWots/StWotsLoad, `hash_reg_q` always captures `sha_digest`
on `hash_complete`. FORS gets this for free. After `StForsKc`
completes, `hash_reg_q` holds the FORS public key.

##### Main FSM additions ([hss_verify.sv:565-622](verilog/rtl/hss_verify.sv#L565-L622))

`StIdle` and `StQ` stay scheme-agnostic (per the user's "init should
be the same"). The dispatch happens at the end of `StQ`:

```systemverilog
StIdle: if (valid) begin
    layer_d = LAYER_CNT_W'(HSS_LEVELS - 1);
    seq_d   = StQ;
end

StQ: begin
    sha_valid = 1'b1;
    if (hash_complete) begin
        seq_d = (SCHEME == 1'b1) ? StFors : StWots;
    end
end

StFors: begin
    // The FORS step has multiple iterations, delegate hash control to FORS sub-FSM
    sha_valid = fors_sha_valid;
    if (fors_complete) seq_d = StForsKc;
end

StForsKc: begin
    sha_valid = 1'b1;
    if (hash_complete) begin
        // hash_reg_q now carries the FORS public key. Fall through into
        // the existing WOTS pipeline as the SPHINCS+ continuation.
        // TODO(SPHINCS+): the WOTS / WotsKc / Leaf / Merkle states still
        //   reference hss_pkg license fields and LMS-specific addressing.
        //   They will produce meaningless results in SPHINCS mode until
        //   adapted in a follow-up plan that introduces the SPHINCS+
        //   hypertree signature inputs.
        seq_d = StWots;
    end
end
```

The existing `StWots` → `StWotsKc` → `StLeaf` → `StMerkle` → `StDone`
path stays as-is (just the rename `StKc` → `StWotsKc`). `StDone`'s
compare is unchanged — no SCHEME gate:

```systemverilog
StDone: begin
    ready        = 1'b1;
    verif_passed = (hash_reg_q == ROOT_PUB_KEY);
    seq_d        = StIdle;
end
```

In SPHINCS mode the comparison currently fails (the WOTS pipeline
isn't yet SPHINCS-aware), but that's the same TODO already flagged in
`StForsKc`.

##### Sequential block ([hss_verify.sv:632-654](verilog/rtl/hss_verify.sv#L632-L654))

Extend reset list with FORS state (`fors_q ← StForsInit`,
`fors_tree_q`, `fors_level_q`, `fors_node_q` to `'0`) and feed the
corresponding `_d` nexts in the non-reset branch. Also add the
`fors_init`-gated capture of `aux_reg_q ← hash_reg_q` next to the
existing `wots_init`-gated capture
([hss_verify.sv:414-422](verilog/rtl/hss_verify.sv#L414-L422)).

#### 4. [verilog/rtl/security_block.sv](verilog/rtl/security_block.sv) (line 138)

- `hss_verify u_hss (...)` → `hash_verify u_hss (...)`. No `SCHEME`
  override — defaults to `LMS`.
- `LICENSE_W` calc and license bus wiring are unchanged. SPHINCS
  isn't a runtime crypto type yet; when it becomes one,
  `CRYPTO_TYPE` will gain a third value and a third `generate` arm
  that instantiates `hash_verify #(.SCHEME(1'b1))`.

#### 5. [verilog/tb/tb_hss.sv](verilog/tb/tb_hss.sv) (line 32)

`hss_verify u_dut (...)` → `hash_verify u_dut (...)`. No other
changes — defaults to `SCHEME = LMS`, single `license` port stays.

#### 6. [verilog/rtl/own_files.vc](verilog/rtl/own_files.vc)

- Replace `./hss_verify.sv` with `./hash_verify.sv` (line 12).
- Insert `./sphincs_pkg.sv` immediately after `./hss_pkg.sv`
  (line 11).

No new TB target this round.

### Things explicitly NOT in this plan

- ADRS encoding fidelity — using a flat
  `{tree_idx, parent_idx}` instead of the canonical SPHINCS+ ADRS
  layout. TODO when interop matters.
- SPHINCS+ message-hash extension (full `H_msg` / MGF1) — `fors_q_idx`
  uses a placeholder slicing of `aux_reg_q`.
- Adapting `Q` (full SPHINCS Q recipe) / WOTS / WotsKc / Leaf / Merkle
  to SPHINCS+ inputs — produces meaningless results in SPHINCS mode
  until the follow-up plan lands.
- `test/reference_sphincs.py`, `tb_fors.sv`, FORS DPI signer.
- Renaming `tb_hss*` files.

## Verification

- `cd verilog && make lint TOOL=verilator` — clean lint (FORS state
  and signals all driven; new package compiles; rename consistent
  across RTL + TB + own_files.vc).
- `cd verilog && make TB=hss sim` and `make TB=top_hss sim` — still
  pass. The LMS path is byte-identical apart from the `StKc` →
  `StWotsKc` rename and the parametric license cast (synth-pruned in
  LMS mode).
- No FORS functional verification this round — that lands with the
  follow-up plan that adapts WOTS / hypertree for SPHINCS+ and adds
  the reference + TB.
