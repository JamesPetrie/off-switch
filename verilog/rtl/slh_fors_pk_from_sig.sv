// FIPS 205 Algorithm 17: FORS public-key recovery for SHA2-128s.
//
// The controller consumes exactly K*(A+1) top-aligned n-byte signature
// elements: one revealed FORS secret followed by A authentication nodes for
// each of K trees. It uses the shared F/H member-function engine to recover
// every tree root, then T_K to compress the roots into the FORS public key.

module slh_fors_pk_from_sig
    import slh_dsa_pkg::*;
(
    input  logic                         clk,
    input  logic                         rst_n,

    input  logic                         start,
    input  logic [8*N_BYTES-1:0]         pk_seed,
    input  logic [HMSG_FORS_BITS-1:0]    fors_message,
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

    localparam logic [3:0] LAST_TREE = 4'(K - 1);
    localparam logic [3:0] LAST_HEIGHT = 4'(A - 1);

    typedef enum logic [3:0] {
        StIdle,
        StWaitSecret,
        StStartLeaf,
        StWaitLeaf,
        StWaitAuth,
        StStartParent,
        StWaitParent,
        StTlStart,
        StTlFeed,
        StTlWait
    } state_e;

    state_e                         state_q;
    logic [8*N_BYTES-1:0]           pk_seed_q;
    logic [HMSG_FORS_BITS-1:0]      fors_message_q;
    logic [HMSG_TREE_BITS-1:0]      tree_index_q;
    logic [HMSG_LEAF_BITS-1:0]      leaf_index_q;
    logic [3:0]                     tree_number_q;
    logic [3:0]                     height_q;
    logic [3:0]                     tl_index_q;
    logic [8*N_BYTES-1:0]           secret_q;
    logic [8*N_BYTES-1:0]           auth_q;
    logic [8*N_BYTES-1:0]           node_q;
    logic [8*N_BYTES-1:0]           roots_q [0:K-1];

    logic [A-1:0]                   local_index;
    logic [15:0]                    global_index;
    logic [3:0]                     parent_height;
    logic [15:0]                    parent_tree_index;
    slh_adrs_t                      hash_adrs;
    slh_adrs_t                      tl_adrs;

    logic                           fh_start;
    logic                           fh_use_h;
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

    slh_hash_fh u_hash_fh (
        .clk,
        .rst_n,
        .start   (fh_start),
        .use_h   (fh_use_h),
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
        .element_count (6'(K)),
        .element_valid (tl_element_valid),
        .element       (tl_element),
        .element_ready (tl_element_ready),
        .busy          (tl_busy),
        .done          (tl_done),
        .error         (tl_error),
        .result        (tl_result)
    );

    assign busy          = (state_q != StIdle);
    assign element_ready = (state_q == StWaitSecret)
                           || (state_q == StWaitAuth);

    wire element_aligned = (element[127:0] == '0);
    wire fh_result_aligned = (fh_result[127:0] == '0);

    always_comb begin
        local_index = fors_message_q[
            HMSG_FORS_BITS - 1 - A*tree_number_q -: A];
        global_index = {tree_number_q, local_index};
        parent_height = height_q + 1'b1;
        parent_tree_index = global_index >> parent_height;

        hash_adrs            = '0;
        hash_adrs.tree       = {42'b0, tree_index_q};
        hash_adrs.type_field = {24'b0, SlhAdrsForsTree};
        hash_adrs.key_pair   = {23'b0, leaf_index_q};
        hash_adrs.word6      = '0;
        hash_adrs.word7      = {16'b0, global_index};
        if (state_q == StStartParent) begin
            hash_adrs.word6 = {28'b0, parent_height};
            hash_adrs.word7 = {16'b0, parent_tree_index};
        end

        tl_adrs            = '0;
        tl_adrs.tree       = {42'b0, tree_index_q};
        tl_adrs.type_field = {24'b0, SlhAdrsForsRoots};
        tl_adrs.key_pair   = {23'b0, leaf_index_q};

        fh_start   = ((state_q == StStartLeaf)
                      || (state_q == StStartParent))
                     && !fh_busy;
        fh_use_h   = (state_q == StStartParent);
        fh_adrs_c  = slh_compress_adrs(hash_adrs);
        fh_message = {secret_q, 128'b0};
        if (state_q == StStartParent) begin
            if (local_index[height_q] == 1'b0) begin
                fh_message = {node_q, auth_q};
            end else begin
                fh_message = {auth_q, node_q};
            end
        end

        tl_start         = (state_q == StTlStart) && !tl_busy;
        tl_element_valid = (state_q == StTlFeed);
        tl_element       = {roots_q[tl_index_q], 128'b0};
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q        <= StIdle;
            pk_seed_q      <= '0;
            fors_message_q <= '0;
            tree_index_q   <= '0;
            leaf_index_q   <= '0;
            tree_number_q  <= '0;
            height_q       <= '0;
            tl_index_q     <= '0;
            secret_q       <= '0;
            auth_q         <= '0;
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
                        pk_seed_q      <= pk_seed;
                        fors_message_q <= fors_message;
                        tree_index_q   <= tree_index;
                        leaf_index_q   <= leaf_index;
                        tree_number_q  <= '0;
                        height_q       <= '0;
                        public_key     <= '0;
                        state_q        <= StWaitSecret;
                    end
                end

                StWaitSecret: begin
                    if (element_valid) begin
                        if (!element_aligned) begin
                            error   <= 1'b1;
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else begin
                            secret_q <= element[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            state_q <= StStartLeaf;
                        end
                    end
                end

                StStartLeaf: state_q <= StWaitLeaf;

                StWaitLeaf: begin
                    if (fh_done) begin
                        if (!fh_result_aligned) begin
                            error   <= 1'b1;
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else begin
                            node_q   <= fh_result[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            height_q <= '0;
                            state_q  <= StWaitAuth;
                        end
                    end
                end

                StWaitAuth: begin
                    if (element_valid) begin
                        if (!element_aligned) begin
                            error   <= 1'b1;
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else begin
                            auth_q <= element[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            state_q <= StStartParent;
                        end
                    end
                end

                StStartParent: state_q <= StWaitParent;

                StWaitParent: begin
                    if (fh_done) begin
                        if (!fh_result_aligned) begin
                            error   <= 1'b1;
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else if (height_q == LAST_HEIGHT) begin
                            node_q <= fh_result[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            roots_q[tree_number_q] <= fh_result[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            if (tree_number_q == LAST_TREE) begin
                                tl_index_q <= '0;
                                state_q    <= StTlStart;
                            end else begin
                                tree_number_q <= tree_number_q + 1'b1;
                                height_q      <= '0;
                                state_q       <= StWaitSecret;
                            end
                        end else begin
                            node_q <= fh_result[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            height_q <= height_q + 1'b1;
                            state_q  <= StWaitAuth;
                        end
                    end
                end

                StTlStart: state_q <= StTlFeed;

                StTlFeed: begin
                    if (tl_element_ready) begin
                        if (tl_index_q == LAST_TREE) begin
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
