// Adapter from the Off-Switch license control transaction to a streamed
// SLH-DSA verifier. Unlike slh_license_verify, signature bytes remain on a
// 64-bit valid/ready stream and are never assembled into a giant register.

module slh_stream_verify
    import slh_dsa_pkg::*;
#(
    parameter int unsigned APP_MESSAGE_BYTES = OFFSWITCH_MESSAGE_BYTES
) (
    input  logic                           clk,
    input  logic                           rst_n,
    input  logic                           valid,
    input  logic [8*N_BYTES-1:0]           pk_seed,
    input  logic [8*N_BYTES-1:0]           pk_root,
    input  logic [8*APP_MESSAGE_BYTES-1:0] message,

    input  logic                           sig_valid,
    output logic                           sig_ready,
    input  logic [STREAM_BITS-1:0]         sig_data,
    input  logic [STREAM_BYTES-1:0]        sig_keep,
    input  logic                           sig_last,

    output logic                           ready,
    output logic                           verif_passed
);

    typedef enum logic [1:0] {
        StIdle,
        StActive,
        StReport
    } state_e;

    state_e                         state_q;
    logic                           result_q;
    logic                           verifier_start;
    wire                            verifier_start_ready;
    wire                            verifier_sig_ready;
    wire                            verifier_busy;
    wire                            verifier_done;
    wire                            verifier_error;
    slh_error_e                     verifier_error_code;
    wire                            verifier_valid_signature;
    wire [MAX_NODE_BITS-1:0]        verifier_computed_root;

    wire root_is_aligned =
        (verifier_computed_root[(MAX_N_BYTES-N_BYTES)*8-1:0] == '0);

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
        .sig_valid           (sig_valid && (state_q == StActive)),
        .sig_ready           (verifier_sig_ready),
        .sig_data,
        .sig_keep,
        .sig_last,
        .busy                (verifier_busy),
        .done                (verifier_done),
        .error               (verifier_error),
        .error_code          (verifier_error_code),
        .valid_signature     (verifier_valid_signature),
        .computed_root       (verifier_computed_root)
    );

    assign verifier_start = (state_q == StIdle) && valid
                            && verifier_start_ready && !verifier_busy;
    assign sig_ready       = (state_q == StActive) && verifier_sig_ready;
    assign ready           = (state_q == StReport);
    assign verif_passed    = (state_q == StReport) && result_q;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q  <= StIdle;
            result_q <= 1'b0;
        end else begin
            unique case (state_q)
                StIdle: begin
                    if (verifier_start) begin
                        result_q <= 1'b0;
                        state_q  <= StActive;
                    end
                end

                StActive: begin
                    if (verifier_done) begin
                        result_q <= verifier_valid_signature
                                    && !verifier_error
                                    && (verifier_error_code == SlhErrNone)
                                    && root_is_aligned
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

                default: begin
                    result_q <= 1'b0;
                    state_q  <= StIdle;
                end
            endcase
        end
    end

endmodule
