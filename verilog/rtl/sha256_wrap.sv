// SHA-256 wrapper — valid/ready handshake around secworks sha256_core
//
//   The wrapper auto-tracks first vs continuation blocks: after reset or after
//   a last block completes, the next accepted block uses init. Otherwise next.

module sha256_wrap (
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

    typedef enum logic {StIdle, StBusy} state_e;

    state_e state_q, state_d;
    logic   first_q;   // next accepted block is first of a new message

    // -------------------------------------------------------------------------
    // SHA-256 core
    // -------------------------------------------------------------------------

    logic         sha_init;
    logic         sha_next;
    wire          sha_ready;            // core ready to accept a block
    wire          sha_digest_valid;     // core has valid digest output

    sha256_core u_sha256 (
        .clk          (clk),
        .reset_n      (rst_n),
        .init         (sha_init),
        .next         (sha_next),
        .mode         (1'b1),
        .block        (block),
        .ready        (sha_ready),
        .digest       (digest),
        .digest_valid (sha_digest_valid)
    );

    // -------------------------------------------------------------------------
    // FSM to track when the SHA is working (between sha_init/next and sha_ready/digest_valid)
    // -------------------------------------------------------------------------

    always_comb begin
        state_d  = state_q;

        sha_init = 1'b0;
        sha_next = 1'b0;

        ready    = 1'b0;

        unique case (state_q)
            StIdle: begin
                if (valid & sha_ready) begin
                    sha_init = first_q;
                    sha_next = ~first_q;
                    state_d  = StBusy;
                end
            end

            StBusy: begin
                if (sha_digest_valid) begin
                    ready   = 1'b1;
                    state_d = StIdle;
                end
            end

            default: ;
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
            first_q <= 1'b1;
        end else begin
            first_q <= (valid & ready) ? last : first_q;
        end
    end

endmodule
