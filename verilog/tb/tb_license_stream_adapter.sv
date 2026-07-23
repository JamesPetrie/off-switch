// Testbench-only bridge from a packed license to the 256-bit stream wrapper.
// Existing integration-test state machines can therefore remain unchanged.

module tb_license_stream_adapter #(
    parameter int unsigned LICENSE_W = 512,
    localparam int unsigned WORD_W = 256,
    localparam int unsigned WORDS = (LICENSE_W + WORD_W - 1) / WORD_W,
    localparam int unsigned PAD_W = WORDS * WORD_W,
    localparam int unsigned PAD_BITS = PAD_W - LICENSE_W,
    localparam int unsigned WORD_IDX_W = (WORDS > 1) ? $clog2(WORDS) : 1
) (
    input  logic                 clk,
    input  logic                 rst_n,

    input  logic                 license_valid,
    output logic                 license_ready,
    input  logic [LICENSE_W-1:0] license_packed,

    output logic              stream_valid,
    input  logic              stream_ready,
    output logic [WORD_W-1:0] stream_word
);

    typedef enum logic [2:0] {
        StIdle,
        StSend,
        StGap,
        StWaitBusy,
        StWaitDone,
        StWaitRelease
    } state_e;

    state_e state_q, state_d;
    logic [PAD_W-1:0] image_q, image_d;
    logic [WORD_IDX_W-1:0] word_idx_q, word_idx_d;

    always_comb begin
        state_d    = state_q;
        image_d    = image_q;
        word_idx_d = word_idx_q;

        license_ready = 1'b0;
        stream_valid  = 1'b0;
        stream_word   = image_q[
            PAD_W - 1 - int'(word_idx_q) * WORD_W -: WORD_W
        ];

        unique case (state_q)
            StIdle: begin
                if (license_valid) begin
                    image_d    = PAD_W'(license_packed) << PAD_BITS;
                    word_idx_d = '0;
                    state_d    = StSend;
                end
            end

            StSend: begin
                stream_valid = 1'b1;
                if (stream_ready) begin
                    if (word_idx_q == WORD_IDX_W'(WORDS - 1)) begin
                        state_d = StWaitBusy;
                    end else begin
                        word_idx_d = word_idx_q + 1'b1;
                        state_d    = StGap;
                    end
                end
            end

            // Insert a one-cycle source gap between words.
            StGap: begin
                state_d = StSend;
            end

            StWaitBusy: begin
                if (!stream_ready)
                    state_d = StWaitDone;
            end

            // Recreate the original completion pulse for the existing tests.
            StWaitDone: begin
                if (stream_ready) begin
                    license_ready = 1'b1;
                    state_d       = StWaitRelease;
                end
            end

            StWaitRelease: begin
                if (!license_valid)
                    state_d = StIdle;
            end

            default: state_d = StIdle;
        endcase
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q    <= StIdle;
            image_q    <= 0;
            word_idx_q <= '0;
        end else begin
            state_q    <= state_d;
            image_q    <= image_d;
            word_idx_q <= word_idx_d;
        end
    end

endmodule
