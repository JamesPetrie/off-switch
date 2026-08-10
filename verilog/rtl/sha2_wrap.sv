// SHA-2 wrapper — valid/ready handshake around the vendored Pavona
// prim_sha2_compression core (vendor/pavona_prim_sha2/, MultimodeEn=0).
//
// Same contract as the former secworks-based sha256_wrap: present a
// pre-padded 512-bit block with valid/last; ready pulses one cycle when the
// block is absorbed, and digest is final at the last block's ready pulse.
// First vs continuation blocks are tracked internally (first_q).
//
// Core-facing notes:
//   - Core wants W0 in msg_block_data_i[31:0]; off-switch puts W0 in
//     block[511:480], so the 16 words are order-reversed (no byte swap).
//   - digest_o is final one cycle after hash_done_o, hence StDone.
//   - Outputs are Moore (registered state only): no combinational path
//     from valid/last to ready, which would loop through hss_verify.

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

    typedef enum logic [2:0] {StIdle, StStart, StLoad, StBusy, StDone} state_e;

    state_e state_q, state_d;
    logic   first_q;   // next accepted block is first of a new message
    logic   last_q;    // block currently in flight closes the message

    // -------------------------------------------------------------------------
    // SHA-2 compression core
    // -------------------------------------------------------------------------

    logic              hash_start;
    logic              msg_block_valid;
    logic              msg_block_done;
    wire               msg_block_ready;
    wire               hash_done;
    sha_word64_t [7:0] digest_words;

    // Observability outputs of the core that the wrapper does not need
    sha_word64_t [7:0] hash_unused;
    logic              digest_on_blk_unused;
    sha_st_e           sha_st_unused;
    logic              hash_running_unused;
    logic              idle_unused;

    // Order-reverse the 16 words: W0 from block[511:480] to block_rev[31:0]
    logic [511:0] block_rev;
    for (genvar gi = 0; gi < 16; gi++) begin : gen_block_rev
        assign block_rev[32*gi +: 32] = block[511 - 32*gi -: 32];
    end

    prim_sha2_compression #(
        .MultimodeEn (1'b0)
    ) u_prim_sha2 (
        .clk_i             (clk),
        .rst_ni            (rst_n),

        .wipe_secret_i     (1'b0),
        .wipe_v_i          (32'h0),

        .msg_block_data_i  (block_rev),
        .msg_block_valid_i (msg_block_valid),
        .msg_block_done_i  (msg_block_done),
        .msg_block_ready_o (msg_block_ready),

        .sha_en_i          (1'b1),
        .hash_start_i      (hash_start),
        .hash_continue_i   (1'b0),
        .digest_mode_i     (SHA2_256),

        .hash_done_o       (hash_done),
        .hash_o            (hash_unused),

        .message_length_i  (64'h0),
        .digest_i          ('0),
        .digest_we_i       ('0),
        .digest_o          (digest_words),
        .digest_on_blk_o   (digest_on_blk_unused),
        .sha_st_o          (sha_st_unused),
        .hash_running_o    (hash_running_unused),
        .idle_o            (idle_unused)
    );

    // H0..H7 sit in the low halves of digest_o[0..7]; repack big-endian
    for (genvar gi = 0; gi < 8; gi++) begin : gen_digest
        assign digest[255 - 32*gi -: 32] = digest_words[gi][31:0];
    end

    // -------------------------------------------------------------------------
    // FSM
    // -------------------------------------------------------------------------
    //   StIdle:  wait for a block; new messages go via StStart.
    //   StStart: pulse hash_start; core is ready to load one cycle later.
    //   StLoad:  drive the block until the core takes it (captures last_q).
    //   StBusy:  continuation block done when msg_block_ready re-asserts;
    //            final block done when hash_done pulses.
    //   StDone:  extra cycle until digest is final, then ready.

    assign hash_start      = (state_q == StStart);
    assign msg_block_valid = (state_q == StLoad);
    assign msg_block_done  = (state_q == StBusy) & last_q;

    assign ready = (state_q == StDone) |
                   ((state_q == StBusy) & ~last_q & msg_block_ready);

    always_comb begin
        state_d = state_q;

        unique case (state_q)
            StIdle: begin
                if (valid) begin
                    state_d = first_q ? StStart : StLoad;
                end
            end

            StStart: begin
                state_d = StLoad;
            end

            StLoad: begin
                if (msg_block_ready) begin
                    state_d = StBusy;
                end
            end

            StBusy: begin
                if (last_q) begin
                    if (hash_done) state_d = StDone;
                end else if (msg_block_ready) begin
                    state_d = StIdle;
                end
            end

            StDone: begin
                state_d = StIdle;
            end

            default: state_d = StIdle;
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
            last_q <= 1'b0;
        end else begin
            last_q <= (msg_block_valid & msg_block_ready) ? last : last_q;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            first_q <= 1'b1;
        end else begin
            first_q <= (valid & ready) ? last : first_q;
        end
    end

endmodule
