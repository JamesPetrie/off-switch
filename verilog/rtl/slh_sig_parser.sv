// Fixed-profile SLH-DSA signature stream parser.
//
// The parser validates exact framing and emits one top-aligned n-byte element
// at a time. It deliberately performs no cryptographic work. Separating byte
// transport from FORS/WOTS+/XMSS control makes malformed input fail before it
// can affect the verification state machines.

module slh_sig_parser
    import slh_dsa_pkg::*;
#(
    parameter int unsigned PROFILE_N_BYTES        = N_BYTES,
    parameter int unsigned PROFILE_FORS_SIG_BYTES = FORS_SIG_BYTES,
    parameter int unsigned PROFILE_HT_SIG_BYTES   = HT_SIG_BYTES,
    parameter int unsigned PROFILE_SIG_BYTES      = SIG_BYTES
) (
    input  logic                         clk,
    input  logic                         rst_n,

    input  logic                         start,
    output logic                         start_ready,
    output logic                         busy,

    input  logic                         sig_valid,
    output logic                         sig_ready,
    input  logic [STREAM_BITS-1:0]       sig_data,
    input  logic [STREAM_BYTES-1:0]      sig_keep,
    input  logic                         sig_last,

    output logic                         element_valid,
    input  logic                         element_ready,
    output logic [MAX_NODE_BITS-1:0]     element_data,
    output slh_region_e                  element_region,
    output logic [15:0]                  element_index,
    output logic                         element_last,

    output logic                         done,
    output logic                         error,
    output slh_error_e                   error_code,
    output logic [31:0]                  byte_count
);

    localparam int unsigned PROFILE_FORS_ELEMENTS =
        PROFILE_FORS_SIG_BYTES / PROFILE_N_BYTES;
    localparam int unsigned PROFILE_SIG_ELEMENTS =
        PROFILE_SIG_BYTES / PROFILE_N_BYTES;
    localparam int unsigned BEATS_PER_ELEMENT =
        PROFILE_N_BYTES / STREAM_BYTES;
    localparam int unsigned BEAT_COUNT_W =
        (BEATS_PER_ELEMENT > 1) ? $clog2(BEATS_PER_ELEMENT) : 1;

    typedef enum logic [2:0] {
        StIdle,
        StReceive,
        StEmit,
        StDone,
        StError
    } state_e;

    state_e                         state_q;
    logic [BEAT_COUNT_W-1:0]        beat_count_q;
    logic [15:0]                    element_index_q;
    logic [31:0]                    byte_count_q;
    logic [MAX_NODE_BITS-1:0]       element_q;
    slh_error_e                     error_code_q;

    wire expected_last_word =
        (byte_count_q + STREAM_BYTES == PROFILE_SIG_BYTES);
    wire accepted_word = sig_valid && sig_ready;

    assign start_ready   = (state_q == StIdle);
    assign busy          = (state_q != StIdle);
    assign sig_ready     = (state_q == StReceive);
    assign element_valid = (state_q == StEmit);
    assign element_data  = element_q;
    assign element_index = element_index_q;
    assign element_last  =
        (int'(element_index_q) == PROFILE_SIG_ELEMENTS - 1);
    assign error_code    = error_code_q;
    assign byte_count    = byte_count_q;

    always_comb begin
        if (element_index_q == 0) begin
            element_region = SlhRegionR;
        end else if (int'(element_index_q) <= PROFILE_FORS_ELEMENTS) begin
            element_region = SlhRegionFors;
        end else if (int'(element_index_q) < PROFILE_SIG_ELEMENTS) begin
            element_region = SlhRegionHt;
        end else begin
            element_region = SlhRegionInvalid;
        end
    end

    initial begin
        if (PROFILE_N_BYTES == 0 || PROFILE_N_BYTES > MAX_N_BYTES) begin
            $error("PROFILE_N_BYTES is outside the reserved node width");
        end
        if (PROFILE_N_BYTES % STREAM_BYTES != 0) begin
            $error("PROFILE_N_BYTES must be divisible by STREAM_BYTES");
        end
        if (PROFILE_SIG_BYTES % PROFILE_N_BYTES != 0) begin
            $error("PROFILE_SIG_BYTES must contain complete n-byte elements");
        end
        if (PROFILE_SIG_BYTES % STREAM_BYTES != 0) begin
            $error("PROFILE_SIG_BYTES must contain complete stream words");
        end
        if (PROFILE_SIG_BYTES != PROFILE_N_BYTES
            + PROFILE_FORS_SIG_BYTES + PROFILE_HT_SIG_BYTES) begin
            $error("profile signature regions do not sum to the signature size");
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin : p_parser
        integer lane;

        if (!rst_n) begin
            state_q         <= StIdle;
            beat_count_q    <= '0;
            element_index_q <= '0;
            byte_count_q    <= '0;
            element_q       <= '0;
            error_code_q    <= SlhErrNone;
            done            <= 1'b0;
            error           <= 1'b0;
        end else begin
            done  <= 1'b0;
            error <= 1'b0;

            unique case (state_q)
                StIdle: begin
                    if (start) begin
                        beat_count_q    <= '0;
                        element_index_q <= '0;
                        byte_count_q    <= '0;
                        element_q       <= '0;
                        error_code_q    <= SlhErrNone;
                        state_q         <= StReceive;
                    end
                end

                StReceive: begin
                    if (accepted_word) begin
                        if (sig_keep != {STREAM_BYTES{1'b1}}) begin
                            error_code_q <= SlhErrBadKeep;
                            state_q      <= StError;
                        end else if (sig_last && !expected_last_word) begin
                            error_code_q <= SlhErrEarlyLast;
                            state_q      <= StError;
                        end else if (!sig_last && expected_last_word) begin
                            error_code_q <= SlhErrMissingLast;
                            state_q      <= StError;
                        end else begin
                            for (lane = 0; lane < STREAM_BYTES; lane++) begin
                                element_q[MAX_NODE_BITS - 1
                                    - 8 * (beat_count_q * STREAM_BYTES + lane)
                                    -: 8] <= sig_data[8 * lane +: 8];
                            end

                            byte_count_q <= byte_count_q + STREAM_BYTES;
                            if (int'(beat_count_q) == BEATS_PER_ELEMENT - 1) begin
                                beat_count_q <= '0;
                                state_q      <= StEmit;
                            end else begin
                                beat_count_q <= beat_count_q + 1'b1;
                            end
                        end
                    end
                end

                StEmit: begin
                    if (element_ready) begin
                        element_q <= '0;
                        if (int'(element_index_q) == PROFILE_SIG_ELEMENTS - 1) begin
                            state_q <= StDone;
                        end else begin
                            element_index_q <= element_index_q + 1'b1;
                            state_q         <= StReceive;
                        end
                    end
                end

                StDone: begin
                    if (byte_count_q != PROFILE_SIG_BYTES) begin
                        error_code_q <= SlhErrLength;
                        state_q      <= StError;
                    end else begin
                        done    <= 1'b1;
                        state_q <= StIdle;
                    end
                end

                StError: begin
                    error   <= 1'b1;
                    state_q <= StIdle;
                end

                default: begin
                    error_code_q <= SlhErrProtocol;
                    state_q      <= StError;
                end
            endcase
        end
    end

endmodule
