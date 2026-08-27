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

module sha2_wrap
    import prim_sha2_pkg::*;
(
    input  logic           clk,
    input  logic           rst_n,

    // Inputs
    input  logic           valid,
    input  logic [511:0]   block,
    input  logic           last,

    // Outputs
    output logic           ready,
    output logic [255:0]   digest
);

    // -------------------------------------------------------------------------
    // State
    // -------------------------------------------------------------------------

    typedef enum logic [1:0] {StIdle, StPass, StFinish, StDone} state_e;

    state_e state_q, state_d;

    logic accepted_q;         // core took a continuation block last cycle
    logic [15:0] blk_cnt_q;   // blocks taken of the running message

    logic hash_start;
    logic msg_block_valid;
    logic msg_block_done;
    wire  msg_block_ready;
    wire  hash_done;
    sha_word64_t [7:0] digest_words;

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

        .sha_en_i          (1'b1),
        .hash_start_i      (hash_start),
        .hash_continue_i   (1'b0),  // hash-context restore is not used
        .digest_mode_i     (SHA2_256),

        .hash_done_o       (hash_done),
        .hash_o            (),      // a..h working variables; digest_o suffices

        .message_length_i  ({39'b0, blk_cnt_q, 9'b0}),  // bits taken so far
        .digest_i          ('0),    // digest write-back (context restore) not used
        .digest_we_i       ('0),
        .digest_o          (digest_words),
        .digest_on_blk_o   (),      // digest-at-block-boundary marker
        .sha_st_o          (),      // core FSM state
        .hash_running_o    (),      // compression rounds active
        .idle_o            ()       // core FSM idle
    );

    // H0..H7 sit in the low halves of digest_o[0..7]; repack big-endian
    for (genvar gi = 0; gi < 8; gi++) begin : gen_digest
        assign digest[255 - 32*gi -: 32] = digest_words[gi][31:0];
    end

    // -------------------------------------------------------------------------
    // FSM
    // -------------------------------------------------------------------------
    //   StIdle:   wait for the first block of a message; its valid doubles as
    //             hash_start, and the core starts accepting one cycle later.
    //   StPass:   pass valid through to the core, which takes a block whenever
    //             its ready is high (waiting for data, or back-to-back in its
    //             digest-update cycle). Each taken continuation block is
    //             acknowledged by a registered one-cycle ready pulse.
    //   StFinish: the last block has been taken (msg_block_done accompanied
    //             it in the handshake cycle); wait for hash_done.
    //   StDone:   digest_o is final one cycle after hash_done pulses, so the
    //             last block's ready is asserted here.

    assign hash_start      = (state_q == StIdle) & valid;
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
            StIdle:   if (valid)         state_d = StPass;
            StPass:   if (accept & last) state_d = StFinish;
            StFinish: if (hash_done)     state_d = StDone;
            StDone:                      state_d = StIdle;
            default:                     state_d = StIdle;
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
            accepted_q <= accept & ~last;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            blk_cnt_q <= '0;
        end else if (hash_start) begin
            blk_cnt_q <= '0;
        end else if (accept) begin
            blk_cnt_q <= blk_cnt_q + 16'd1;
        end
    end

endmodule
