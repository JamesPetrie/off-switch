// Adapter from the legacy Off-Switch license valid/ready transaction to the
// streamed SLH-DSA verifier. The license input must remain stable while valid
// is asserted, matching the existing security_block contract.

module slh_license_verify
    import slh_dsa_pkg::*;
#(
    parameter int unsigned APP_MESSAGE_BYTES = OFFSWITCH_MESSAGE_BYTES,
    parameter int unsigned LICENSE_BITS = 8 * SIG_BYTES
) (
    input  logic                           clk,
    input  logic                           rst_n,
    input  logic                           valid,
    input  logic [8*N_BYTES-1:0]           pk_seed,
    input  logic [8*N_BYTES-1:0]           pk_root,
    input  logic [8*APP_MESSAGE_BYTES-1:0] message,
    input  logic [LICENSE_BITS-1:0]         license,

    output logic                           ready,
    output logic                           verif_passed
);

    localparam int unsigned SIGNATURE_WORDS = SIG_BYTES / STREAM_BYTES;
    localparam int unsigned WORD_INDEX_BITS = $clog2(SIGNATURE_WORDS);
    localparam logic [WORD_INDEX_BITS-1:0] LAST_WORD =
        WORD_INDEX_BITS'(SIGNATURE_WORDS - 1);

    typedef enum logic [1:0] {
        StIdle,
        StStream,
        StWait,
        StReport
    } state_e;

    state_e                         state_q;
    logic [WORD_INDEX_BITS-1:0]     word_index_q;
    logic                           result_q;

    logic                           verifier_start;
    wire                            verifier_start_ready;
    logic                           verifier_sig_valid;
    wire                            verifier_sig_ready;
    logic [STREAM_BITS-1:0]         verifier_sig_data;
    logic                           verifier_sig_last;
    wire                            verifier_busy;
    wire                            verifier_done;
    wire                            verifier_error;
    slh_error_e                     verifier_error_code;
    wire                            verifier_valid_signature;
    wire [MAX_NODE_BITS-1:0]        verifier_computed_root;

    integer                         lane;

    initial begin
        if (LICENSE_BITS != 8 * SIG_BYTES) begin
            $error("SLH-DSA license width must equal the fixed signature size");
        end
    end

    slh_dsa_verify #(
        .APP_MESSAGE_BYTES(APP_MESSAGE_BYTES)
    ) u_verify (
        .clk,
        .rst_n,
        .start               (verifier_start),
        .start_ready         (verifier_start_ready),
        .pk_seed,
        .pk_root,
        .application_message (message),
        .sig_valid           (verifier_sig_valid),
        .sig_ready           (verifier_sig_ready),
        .sig_data            (verifier_sig_data),
        .sig_keep            ({STREAM_BYTES{1'b1}}),
        .sig_last            (verifier_sig_last),
        .busy                (verifier_busy),
        .done                (verifier_done),
        .error               (verifier_error),
        .error_code          (verifier_error_code),
        .valid_signature     (verifier_valid_signature),
        .computed_root       (verifier_computed_root)
    );

    assign ready        = (state_q == StReport);
    assign verif_passed = (state_q == StReport) && result_q;

    always_comb begin
        verifier_start     = (state_q == StIdle) && valid
                             && verifier_start_ready && !verifier_busy;
        verifier_sig_valid = (state_q == StStream);
        verifier_sig_last  = (word_index_q == LAST_WORD);
        verifier_sig_data  = '0;
        for (lane = 0; lane < STREAM_BYTES; lane = lane + 1) begin
            verifier_sig_data[8*lane +: 8] = license[
                LICENSE_BITS - 1
                - 8*(word_index_q*STREAM_BYTES + lane) -: 8];
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q      <= StIdle;
            word_index_q <= '0;
            result_q     <= 1'b0;
        end else begin
            unique case (state_q)
                StIdle: begin
                    if (verifier_start) begin
                        word_index_q <= '0;
                        result_q     <= 1'b0;
                        state_q      <= StStream;
                    end
                end

                StStream: begin
                    if (verifier_sig_valid && verifier_sig_ready) begin
                        if (word_index_q == LAST_WORD) begin
                            state_q <= StWait;
                        end else begin
                            word_index_q <= word_index_q + 1'b1;
                        end
                    end
                end

                StWait: begin
                    if (verifier_done) begin
                        result_q <= verifier_valid_signature
                                    && !verifier_error
                                    && (verifier_error_code == SlhErrNone)
                                    && (verifier_computed_root[127:0]
                                        == '0)
                                    && (verifier_computed_root[
                                            MAX_NODE_BITS-1 -: 8*N_BYTES]
                                        == pk_root);
                        state_q <= StReport;
                    end
                end

                StReport: begin
                    if (!valid) begin
                        state_q <= StIdle;
                    end
                end

                default: state_q <= StIdle;
            endcase
        end
    end

endmodule
