// End-to-end FIPS 205 SLH-DSA-SHA2-128s verifier for the Off-Switch message.
//
// Verification-only architecture:
//   64-bit signature stream -> exact-length parser -> H_msg -> FORS -> HT
//
// A well-framed but cryptographically invalid signature completes with
// valid_signature=0 and error=0. Transport/framing/protocol failures complete
// with error=1 and a non-zero error_code.

module slh_dsa_verify
    import slh_dsa_pkg::*;
#(
    parameter int unsigned APP_MESSAGE_BYTES = 72
) (
    input  logic                           clk,
    input  logic                           rst_n,

    input  logic                           start,
    output logic                           start_ready,
    input  logic [8*N_BYTES-1:0]           pk_seed,
    input  logic [8*N_BYTES-1:0]           pk_root,
    input  logic [8*APP_MESSAGE_BYTES-1:0] application_message,

    input  logic                           sig_valid,
    output logic                           sig_ready,
    input  logic [STREAM_BITS-1:0]         sig_data,
    input  logic [STREAM_BYTES-1:0]        sig_keep,
    input  logic                           sig_last,

    output logic                           busy,
    output logic                           done,
    output logic                           error,
    output slh_error_e                     error_code,
    output logic                           valid_signature,
    output logic [MAX_NODE_BITS-1:0]       computed_root
);

    localparam logic [15:0] LAST_FORS_ELEMENT = 16'(FORS_ELEMENTS);

    typedef enum logic [3:0] {
        StIdle,
        StWaitR,
        StHmsgStart,
        StHmsgWait,
        StForsStart,
        StForsFeed,
        StForsWait,
        StHtStart,
        StHtFeed,
        StHtWait,
        StErrorDrain
    } state_e;

    state_e                           state_q;
    wire operation_rst_n = rst_n && (state_q != StErrorDrain);
    logic [8*N_BYTES-1:0]             pk_seed_q;
    logic [8*N_BYTES-1:0]             pk_root_q;
    logic [8*APP_MESSAGE_BYTES-1:0]   application_message_q;
    logic [8*N_BYTES-1:0]             randomizer_q;
    logic                             parser_complete_q;
    slh_error_e                       error_code_q;

    logic                             parser_start;
    wire                              parser_start_ready;
    wire                              parser_busy;
    wire                              parser_element_valid;
    logic                             parser_element_ready;
    wire [MAX_NODE_BITS-1:0]          parser_element_data;
    slh_region_e                      parser_element_region;
    wire [15:0]                       parser_element_index;
    wire                              parser_element_last;
    wire                              parser_done;
    wire                              parser_error;
    slh_error_e                       parser_error_code;
    wire [31:0]                       parser_byte_count;

    logic                             hmsg_start;
    wire                              hmsg_busy;
    wire                              hmsg_done;
    wire [255:0]                      hmsg_digest;
    wire [HMSG_FORS_BITS-1:0]         hmsg_fors_message;
    wire [HMSG_TREE_BYTES*8-1:0]      hmsg_tree_index;
    wire [HMSG_LEAF_BYTES*8-1:0]      hmsg_leaf_index;

    logic                             fors_start;
    logic                             fors_element_valid;
    wire                              fors_element_ready;
    wire                              fors_busy;
    wire                              fors_done;
    wire                              fors_error;
    wire [MAX_NODE_BITS-1:0]          fors_public_key;

    logic                             ht_start;
    logic                             ht_element_valid;
    wire                              ht_element_ready;
    wire                              ht_busy;
    wire                              ht_done;
    wire                              ht_error;
    wire [MAX_NODE_BITS-1:0]          ht_root;

    wire parser_element_accepted =
        parser_element_valid && parser_element_ready;

    wire hmsg_outputs_consistent =
        (hmsg_digest[255:88] == hmsg_fors_message)
        && ((hmsg_digest[87:32] & 56'h3fffffffffffff)
            == hmsg_tree_index)
        && ((hmsg_digest[31:16] & 16'h01ff) == hmsg_leaf_index)
        && (hmsg_digest[15:0] == 16'b0);

    wire fors_key_aligned =
        (fors_public_key[(MAX_N_BYTES-N_BYTES)*8-1:0] == '0);
    wire ht_root_aligned =
        (ht_root[(MAX_N_BYTES-N_BYTES)*8-1:0] == '0);

    slh_sig_parser u_parser (
        .clk,
        .rst_n          (operation_rst_n),
        .start          (parser_start),
        .start_ready    (parser_start_ready),
        .busy           (parser_busy),
        .sig_valid,
        .sig_ready,
        .sig_data,
        .sig_keep,
        .sig_last,
        .element_valid  (parser_element_valid),
        .element_ready  (parser_element_ready),
        .element_data   (parser_element_data),
        .element_region (parser_element_region),
        .element_index  (parser_element_index),
        .element_last   (parser_element_last),
        .done           (parser_done),
        .error          (parser_error),
        .error_code     (parser_error_code),
        .byte_count     (parser_byte_count)
    );

    slh_hmsg #(
        .APP_MESSAGE_BYTES(APP_MESSAGE_BYTES)
    ) u_hmsg (
        .clk,
        .rst_n               (operation_rst_n),
        .start               (hmsg_start),
        .randomizer_r        (randomizer_q),
        .pk_seed             (pk_seed_q),
        .pk_root             (pk_root_q),
        .application_message (application_message_q),
        .busy                (hmsg_busy),
        .done                (hmsg_done),
        .digest              (hmsg_digest),
        .fors_message        (hmsg_fors_message),
        .tree_index          (hmsg_tree_index),
        .leaf_index          (hmsg_leaf_index)
    );

    slh_fors_pk_from_sig u_fors (
        .clk,
        .rst_n         (operation_rst_n),
        .start         (fors_start),
        .pk_seed       (pk_seed_q),
        .fors_message  (hmsg_fors_message),
        .tree_index    (hmsg_tree_index[HMSG_TREE_BITS-1:0]),
        .leaf_index    (hmsg_leaf_index[HMSG_LEAF_BITS-1:0]),
        .element_valid (fors_element_valid),
        .element       (parser_element_data),
        .element_ready (fors_element_ready),
        .busy          (fors_busy),
        .done          (fors_done),
        .error         (fors_error),
        .public_key    (fors_public_key)
    );

    slh_ht_verify u_ht (
        .clk,
        .rst_n         (operation_rst_n),
        .start         (ht_start),
        .pk_seed       (pk_seed_q),
        .message       (fors_public_key[MAX_NODE_BITS-1 -: 8*N_BYTES]),
        .tree_index    (hmsg_tree_index[HMSG_TREE_BITS-1:0]),
        .leaf_index    (hmsg_leaf_index[HMSG_LEAF_BITS-1:0]),
        .element_valid (ht_element_valid),
        .element       (parser_element_data),
        .element_ready (ht_element_ready),
        .busy          (ht_busy),
        .done          (ht_done),
        .error         (ht_error),
        .root          (ht_root)
    );

    assign busy          = (state_q != StIdle);
    assign start_ready   = (state_q == StIdle) && parser_start_ready
                           && !hmsg_busy && !fors_busy && !ht_busy;
    assign parser_start  = start && start_ready;
    assign error_code    = error_code_q;

    always_comb begin
        parser_element_ready = 1'b0;
        fors_element_valid   = 1'b0;
        ht_element_valid     = 1'b0;

        if ((state_q == StWaitR)
            && (parser_element_region == SlhRegionR)
            && (parser_element_index == 0)) begin
            parser_element_ready = 1'b1;
        end else if ((state_q == StForsFeed)
                     && (parser_element_region == SlhRegionFors)) begin
            parser_element_ready = fors_element_ready;
            fors_element_valid   = parser_element_valid;
        end else if ((state_q == StHtFeed)
                     && (parser_element_region == SlhRegionHt)) begin
            parser_element_ready = ht_element_ready;
            ht_element_valid     = parser_element_valid;
        end

        hmsg_start = (state_q == StHmsgStart) && !hmsg_busy;
        fors_start = (state_q == StForsStart) && !fors_busy;
        ht_start   = (state_q == StHtStart) && !ht_busy;
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q               <= StIdle;
            pk_seed_q             <= '0;
            pk_root_q             <= '0;
            application_message_q <= '0;
            randomizer_q           <= '0;
            parser_complete_q      <= 1'b0;
            error_code_q           <= SlhErrNone;
            done                   <= 1'b0;
            error                  <= 1'b0;
            valid_signature        <= 1'b0;
            computed_root          <= '0;
        end else begin
            done  <= 1'b0;
            error <= 1'b0;
            if (parser_done) begin
                parser_complete_q <= 1'b1;
            end

            if (parser_error && (state_q != StErrorDrain)) begin
                error_code_q <= parser_error_code;
                state_q      <= StErrorDrain;
            end else begin
                unique case (state_q)
                    StIdle: begin
                        if (start && start_ready) begin
                            pk_seed_q              <= pk_seed;
                            pk_root_q              <= pk_root;
                            application_message_q <= application_message;
                            parser_complete_q      <= 1'b0;
                            error_code_q           <= SlhErrNone;
                            valid_signature        <= 1'b0;
                            computed_root          <= '0;
                            state_q                <= StWaitR;
                        end
                    end

                    StWaitR: begin
                        if (parser_element_accepted) begin
                            randomizer_q <= parser_element_data[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            state_q <= StHmsgStart;
                        end
                    end

                    StHmsgStart: state_q <= StHmsgWait;

                    StHmsgWait: begin
                        if (hmsg_done) begin
                            if (!hmsg_outputs_consistent) begin
                                error_code_q <= SlhErrProtocol;
                                state_q      <= StErrorDrain;
                            end else begin
                                state_q <= StForsStart;
                            end
                        end
                    end

                    StForsStart: state_q <= StForsFeed;

                    StForsFeed: begin
                        if (parser_element_accepted
                            && (parser_element_index
                                == LAST_FORS_ELEMENT)) begin
                            state_q <= StForsWait;
                        end
                    end

                    StForsWait: begin
                        if (fors_done) begin
                            if (fors_error || !fors_key_aligned) begin
                                error_code_q <= SlhErrProtocol;
                                state_q      <= StErrorDrain;
                            end else begin
                                state_q <= StHtStart;
                            end
                        end
                    end

                    StHtStart: state_q <= StHtFeed;

                    StHtFeed: begin
                        if (parser_element_accepted && parser_element_last) begin
                            state_q <= StHtWait;
                        end
                    end

                    StHtWait: begin
                        if (ht_done) begin
                            if (ht_error || !ht_root_aligned
                                || !(parser_complete_q || parser_done)
                                || (parser_byte_count != SIG_BYTES)) begin
                                error_code_q <= SlhErrProtocol;
                                state_q      <= StErrorDrain;
                            end else begin
                                computed_root   <= ht_root;
                                valid_signature <=
                                    (ht_root[MAX_NODE_BITS-1 -: 8*N_BYTES]
                                     == pk_root_q);
                                done    <= 1'b1;
                                state_q <= StIdle;
                            end
                        end
                    end

                    StErrorDrain: begin
                        if (!parser_busy && !hmsg_busy
                            && !fors_busy && !ht_busy) begin
                            error           <= 1'b1;
                            done            <= 1'b1;
                            valid_signature <= 1'b0;
                            state_q          <= StIdle;
                        end
                    end

                    default: begin
                        error_code_q <= SlhErrProtocol;
                        state_q      <= StErrorDrain;
                    end
                endcase
            end
        end
    end

endmodule
