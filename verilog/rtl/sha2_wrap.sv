// SHA-2 wrapper — per block valid/ready handshake around the vendored
// Pavona prim_sha2_compression core (vendor/pavona_fbdfde633/).
//
// Contract: present a pre-padded 512-bit block on valid/block, with last
// marking the closing block of the message, and hold the inputs stable
// until ready. ready pulses for exactly one cycle per block, and at the
// last block's ready pulse digest holds the final value. Messages need no
// start signal: the first block after reset or after a completed message
// starts a new one. All outputs depend on registered state only, so ready
// never combinationally follows valid/last — hss_verify closes that loop
// combinationally on its side. Inward the coupling is combinational: valid
// passes straight through to the core's start/load inputs, which is safe
// because the core's ready cone depends on its registered state only.
//
// A message may also be suspended and resumed later, which lets a caller
// run other messages on the core in between:
//   - save marks a non-last block as the last one of this instalment. At
//     its ready pulse digest holds the running hash state (rather than a
//     final digest), which the caller must latch as the context.
//   - restore marks the first block of an instalment that continues a
//     suspended message: ctx is written back as the running state instead
//     of the initialisation vector. Both may be set on the same block.
// The caller keeps track of the total message length, so the padding it
// builds is unaffected by where the instalment boundaries fall.

module sha2_wrap
    import prim_sha2_pkg::*;
(
    input  logic           clk,
    input  logic           rst_n,

    // Inputs
    input  logic           valid,
    input  logic [511:0]   block,
    input  logic           last,

    // Suspend / resume
    input  logic           save,
    input  logic           restore,
    input  logic [255:0]   ctx,

    // Outputs
    output logic           ready,
    output logic [255:0]   digest
);

    // -------------------------------------------------------------------------
    // State
    // -------------------------------------------------------------------------

    typedef enum logic [2:0] {
        StIdle, StPass, StSusp, StFinish, StDone
    } state_e;

    state_e state_q, state_d;

    logic accepted_q;         // core took a continuation block last cycle
    logic parked_q;           // sha_en has been low for a full cycle
    logic [15:0] blk_cnt_q;   // blocks taken of the running instalment

    logic sha_en;
    logic hash_start;
    logic hash_continue;
    logic msg_block_valid;
    logic msg_block_done;
    wire  msg_block_ready;
    wire  hash_done;
    sha_word64_t [7:0] ctx_words;
    logic [7:0]        ctx_we;
    sha_word64_t [7:0] digest_words;
    wire  sha_st_e      sha_st;

    // core takes the presented block in this cycle
    wire accept = msg_block_valid & msg_block_ready;

    // -------------------------------------------------------------------------
    // SHA-2 compression core (SHA-256-only configuration, MultimodeEn=0)
    // -------------------------------------------------------------------------

    // The core consumes W0 from msg_block_data_i[31:0]; the off-switch block
    // convention is W0 in block[511:480], so the sixteen 32-bit words are
    // order-reversed (no byte swap within words).
    logic [511:0] block_rev;
    for (genvar gi = 0; gi < 16; gi++) begin : gen_block_rev
        assign block_rev[32*gi +: 32] = block[511 - 32*gi -: 32];
    end

    prim_sha2_compression #(
        .MultimodeEn (1'b0)
    ) u_prim_sha2 (
        .clk_i             (clk),
        .rst_ni            (rst_n),

        // Secret wiping is not used
        .wipe_secret_i     (1'b0),
        .wipe_v_i          (32'h0),

        .msg_block_data_i  (block_rev),
        .msg_block_valid_i (msg_block_valid),
        .msg_block_done_i  (msg_block_done),
        .msg_block_ready_o (msg_block_ready),

        .sha_en_i          (sha_en),
        .hash_start_i      (hash_start),
        .hash_continue_i   (hash_continue),
        .digest_mode_i     (SHA2_256),

        .hash_done_o       (hash_done),
        .hash_o            (),      // a..h working variables; digest_o suffices

        .message_length_i  ({39'b0, blk_cnt_q, 9'b0}),  // bits of this instalment
        .digest_i          (ctx_words),
        .digest_we_i       (ctx_we),
        .digest_o          (digest_words),
        .digest_on_blk_o   (),      // digest-at-block-boundary marker
        .sha_st_o          (sha_st),
        .hash_running_o    (),      // compression rounds active
        .idle_o            ()       // core FSM idle (combinational on hash_go)
    );

    // H0..H7 sit in the low halves of digest_o[0..7]; repack big-endian.
    // ctx is packed the same way, so a saved digest is restored unchanged.
    for (genvar gi = 0; gi < 8; gi++) begin : gen_digest
        assign digest[255 - 32*gi -: 32] = digest_words[gi][31:0];
        assign ctx_words[gi] = {32'b0, ctx[255 - 32*gi -: 32]};
    end

    // -------------------------------------------------------------------------
    // FSM
    // -------------------------------------------------------------------------
    //   StIdle:   no message in progress, so the core is held disabled. The
    //             first block's valid doubles as the start (or, with restore,
    //             the context write and continue), and the core starts
    //             accepting one cycle later.
    //   StPass:   pass valid through to the core, which takes a block whenever
    //             its ready is high (waiting for data, or back-to-back in its
    //             digest-update cycle). Each taken continuation block is
    //             acknowledged by a registered one-cycle ready pulse.
    //   StSusp:   a block marked save has been taken; wait for the core's
    //             digest-update cycle, after which digest_o holds the running
    //             state to hand back as the context.
    //   StFinish: the last block has been taken (msg_block_done accompanied
    //             it in the handshake cycle); wait for hash_done.
    //   StDone:   digest_o is final one cycle after the core's digest update,
    //             so the closing block's ready is asserted here.
    //
    // Disabling the core is what parks a suspended message: it clears the
    // core's digest on the sha_en falling edge and forces its FSM back to
    // idle, leaving nothing of the instalment behind. Two consequences for
    // the first cycle back in StIdle, both covered by the guards below:
    // that clear would swallow a context write, and a start pulse issued
    // while the core is still being forced to idle is swallowed as well.

    assign sha_en = (state_q != StIdle);

    // The core's FSM has settled in idle, so a start/continue pulse sticks
    wire core_idle = (sha_st == ShaIdle);

    assign hash_start      = (state_q == StIdle) & valid & ~restore & core_idle;
    // parked_q also covers core_idle: the core is forced to idle by the same
    // sha_en edge whose digest clear parked_q waits out.
    assign hash_continue   = (state_q == StIdle) & valid &  restore & parked_q;
    assign ctx_we          = {8{hash_continue}};
    assign msg_block_valid = (state_q == StPass) & valid;
    // done is qualified by the handshake: it accompanies the last block's
    // valid/ready cycle. A level not gated by ready would latch early while
    // the previous block is still compressing (the last block already sits
    // on the bus back-to-back).
    assign msg_block_done  = accept & last;

    assign ready = (state_q == StDone) | accepted_q;

    always_comb begin
        state_d = state_q;

        unique case (state_q)
            StIdle: begin
                if (hash_start | hash_continue) state_d = StPass;
            end
            StPass: begin
                if      (accept & last) state_d = StFinish;
                else if (accept & save) state_d = StSusp;
            end
            StSusp:   if (msg_block_ready) state_d = StDone;
            StFinish: if (hash_done)       state_d = StDone;
            StDone:                        state_d = StIdle;
            default:                       state_d = StIdle;
        endcase
    end

    // -------------------------------------------------------------------------
    // Sequential
    // -------------------------------------------------------------------------

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q <= StIdle;
        end else begin
            state_q <= state_d;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            accepted_q <= 1'b0;
        end else begin
            accepted_q <= accept & ~last & ~save;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            parked_q <= 1'b1;   // a resumed message may be the first after reset
        end else begin
            parked_q <= ~sha_en;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            blk_cnt_q <= '0;
        end else if (hash_start | hash_continue) begin
            blk_cnt_q <= '0;
        end else if (accept) begin
            blk_cnt_q <= blk_cnt_q + 16'd1;
        end
    end

endmodule
