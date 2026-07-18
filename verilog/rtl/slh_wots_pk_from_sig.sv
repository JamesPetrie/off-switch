// FIPS 205 Algorithm 8: WOTS+ public-key recovery for SHA2-128s.
//
// For each of the 35 signature values, the controller derives the base-16
// message/checksum digit and hashes only from that revealed chain position to
// position 15. T_35 then compresses all chain endpoints into the WOTS+ key.

module slh_wots_pk_from_sig
    import slh_dsa_pkg::*;
(
    input  logic                         clk,
    input  logic                         rst_n,

    input  logic                         start,
    input  logic [8*N_BYTES-1:0]         pk_seed,
    input  logic [8*N_BYTES-1:0]         message,
    input  logic [2:0]                   layer,
    input  logic [HMSG_TREE_BITS-1:0]    tree_index,
    input  logic [HMSG_LEAF_BITS-1:0]    leaf_index,

    input  logic                         element_valid,
    input  logic [MAX_NODE_BITS-1:0]     element,
    output logic                         element_ready,

    output logic                         busy,
    output logic                         done,
    output logic                         error,
    output logic [MAX_NODE_BITS-1:0]     public_key
);

    localparam logic [5:0] LAST_CHAIN = 6'(WOTS_LEN - 1);
    localparam logic [5:0] MESSAGE_CHAINS = 6'(WOTS_LEN1);
    localparam logic [3:0] LAST_HASH_ADDRESS = 4'(W - 2);
    localparam logic [3:0] CHAIN_END = 4'(W - 1);

    typedef enum logic [3:0] {
        StIdle,
        StWaitElement,
        StStartHash,
        StWaitHash,
        StTlStart,
        StTlFeed,
        StTlWait
    } state_e;

    state_e                         state_q;
    logic [8*N_BYTES-1:0]           pk_seed_q;
    logic [8*N_BYTES-1:0]           message_q;
    logic [2:0]                     layer_q;
    logic [HMSG_TREE_BITS-1:0]      tree_index_q;
    logic [HMSG_LEAF_BITS-1:0]      leaf_index_q;
    logic [11:0]                    checksum_q;
    logic [5:0]                     chain_index_q;
    logic [3:0]                     hash_address_q;
    logic [5:0]                     tl_index_q;
    logic [8*N_BYTES-1:0]           node_q;
    logic [8*N_BYTES-1:0]           endpoints_q [WOTS_LEN];

    integer                         checksum_accumulator;
    integer                         digit_index;
    logic [11:0]                    checksum_value;
    logic [3:0]                     current_digit;
    slh_adrs_t                      hash_adrs;
    slh_adrs_t                      tl_adrs;

    logic                           fh_start;
    logic [175:0]                   fh_adrs_c;
    logic [MAX_NODE_BITS-1:0]       fh_message;
    wire                            fh_busy;
    wire                            fh_done;
    wire [MAX_NODE_BITS-1:0]        fh_result;

    logic                           tl_start;
    logic                           tl_element_valid;
    logic [MAX_NODE_BITS-1:0]       tl_element;
    wire                            tl_element_ready;
    wire                            tl_busy;
    wire                            tl_done;
    wire                            tl_error;
    wire [MAX_NODE_BITS-1:0]        tl_result;

    slh_hash_fh u_hash_f (
        .clk,
        .rst_n,
        .start   (fh_start),
        .use_h   (1'b0),
        .pk_seed (pk_seed_q),
        .adrs_c  (fh_adrs_c),
        .message (fh_message),
        .busy    (fh_busy),
        .done    (fh_done),
        .result  (fh_result)
    );

    slh_hash_tl u_hash_tl (
        .clk,
        .rst_n,
        .start         (tl_start),
        .pk_seed       (pk_seed_q),
        .adrs_c        (slh_compress_adrs(tl_adrs)),
        .element_count (6'(WOTS_LEN)),
        .element_valid (tl_element_valid),
        .element       (tl_element),
        .element_ready (tl_element_ready),
        .busy          (tl_busy),
        .done          (tl_done),
        .error         (tl_error),
        .result        (tl_result)
    );

    assign busy          = (state_q != StIdle);
    assign element_ready = (state_q == StWaitElement);

    wire element_aligned = (element[127:0] == '0);
    wire fh_result_aligned = (fh_result[127:0] == '0);

    always_comb begin
        checksum_accumulator = 0;
        for (digit_index = 0; digit_index < WOTS_LEN1;
             digit_index = digit_index + 1) begin
            checksum_accumulator = checksum_accumulator
                + (W - 1)
                - int'(message[8*N_BYTES - 1 - LG_W*digit_index -: LG_W]);
        end
        checksum_value = 12'(checksum_accumulator);

        if (chain_index_q < MESSAGE_CHAINS) begin
            current_digit = message_q[
                8*N_BYTES - 1 - LG_W*chain_index_q -: LG_W];
        end else begin
            unique case (chain_index_q)
                6'd32: current_digit = checksum_q[11:8];
                6'd33: current_digit = checksum_q[7:4];
                default: current_digit = checksum_q[3:0];
            endcase
        end

        hash_adrs            = '0;
        hash_adrs.layer      = {29'b0, layer_q};
        hash_adrs.tree       = {42'b0, tree_index_q};
        hash_adrs.type_field = {24'b0, SlhAdrsWotsHash};
        hash_adrs.key_pair   = {23'b0, leaf_index_q};
        hash_adrs.word6      = {26'b0, chain_index_q};
        hash_adrs.word7      = {28'b0, hash_address_q};

        tl_adrs            = '0;
        tl_adrs.layer      = {29'b0, layer_q};
        tl_adrs.tree       = {42'b0, tree_index_q};
        tl_adrs.type_field = {24'b0, SlhAdrsWotsPk};
        tl_adrs.key_pair   = {23'b0, leaf_index_q};

        fh_start   = (state_q == StStartHash) && !fh_busy;
        fh_adrs_c  = slh_compress_adrs(hash_adrs);
        fh_message = {node_q, 128'b0};

        tl_start         = (state_q == StTlStart) && !tl_busy;
        tl_element_valid = (state_q == StTlFeed);
        tl_element       = {endpoints_q[tl_index_q], 128'b0};
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q        <= StIdle;
            pk_seed_q      <= '0;
            message_q      <= '0;
            layer_q        <= '0;
            tree_index_q   <= '0;
            leaf_index_q   <= '0;
            checksum_q     <= '0;
            chain_index_q  <= '0;
            hash_address_q <= '0;
            tl_index_q     <= '0;
            node_q         <= '0;
            done           <= 1'b0;
            error          <= 1'b0;
            public_key     <= '0;
        end else begin
            done  <= 1'b0;
            error <= 1'b0;

            unique case (state_q)
                StIdle: begin
                    if (start) begin
                        pk_seed_q     <= pk_seed;
                        message_q     <= message;
                        layer_q       <= layer;
                        tree_index_q  <= tree_index;
                        leaf_index_q  <= leaf_index;
                        checksum_q    <= checksum_value;
                        chain_index_q <= '0;
                        public_key    <= '0;
                        state_q       <= StWaitElement;
                    end
                end

                StWaitElement: begin
                    if (element_valid) begin
                        if (!element_aligned) begin
                            error   <= 1'b1;
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else begin
                            node_q <= element[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            hash_address_q <= current_digit;
                            if (current_digit == CHAIN_END) begin
                                endpoints_q[chain_index_q] <= element[
                                    MAX_NODE_BITS-1 -: 8*N_BYTES];
                                if (chain_index_q == LAST_CHAIN) begin
                                    tl_index_q <= '0;
                                    state_q    <= StTlStart;
                                end else begin
                                    chain_index_q <= chain_index_q + 1'b1;
                                end
                            end else begin
                                state_q <= StStartHash;
                            end
                        end
                    end
                end

                StStartHash: state_q <= StWaitHash;

                StWaitHash: begin
                    if (fh_done) begin
                        if (!fh_result_aligned) begin
                            error   <= 1'b1;
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else if (hash_address_q == LAST_HASH_ADDRESS) begin
                            node_q <= fh_result[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            endpoints_q[chain_index_q] <= fh_result[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            if (chain_index_q == LAST_CHAIN) begin
                                tl_index_q <= '0;
                                state_q    <= StTlStart;
                            end else begin
                                chain_index_q <= chain_index_q + 1'b1;
                                state_q       <= StWaitElement;
                            end
                        end else begin
                            node_q <= fh_result[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            hash_address_q <= hash_address_q + 1'b1;
                            state_q        <= StStartHash;
                        end
                    end
                end

                StTlStart: state_q <= StTlFeed;

                StTlFeed: begin
                    if (tl_element_ready) begin
                        if (tl_index_q == LAST_CHAIN) begin
                            state_q <= StTlWait;
                        end else begin
                            tl_index_q <= tl_index_q + 1'b1;
                        end
                    end
                end

                StTlWait: begin
                    if (tl_done) begin
                        public_key <= tl_result;
                        error      <= tl_error;
                        done       <= 1'b1;
                        state_q    <= StIdle;
                    end
                end

                default: begin
                    error   <= 1'b1;
                    done    <= 1'b1;
                    state_q <= StIdle;
                end
            endcase
        end
    end

endmodule
