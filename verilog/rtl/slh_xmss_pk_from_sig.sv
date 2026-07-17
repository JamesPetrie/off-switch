// FIPS 205 Algorithm 11: XMSS public-key recovery for SHA2-128s.
//
// The first WOTS_LEN signature elements recover the WOTS+ leaf. The following
// h'=9 elements are its Merkle authentication path. Address height/index and
// left/right ordering are derived from the signed leaf index.

module slh_xmss_pk_from_sig
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
    output logic [MAX_NODE_BITS-1:0]     root
);

    localparam logic [5:0] LAST_WOTS_ELEMENT = 6'(WOTS_LEN - 1);
    localparam logic [3:0] LAST_AUTH_HEIGHT = 4'(HP - 1);

    typedef enum logic [3:0] {
        StIdle,
        StWotsStart,
        StWotsFeed,
        StWotsWait,
        StWaitAuth,
        StStartParent,
        StWaitParent
    } state_e;

    state_e                         state_q;
    logic [8*N_BYTES-1:0]           pk_seed_q;
    logic [8*N_BYTES-1:0]           message_q;
    logic [2:0]                     layer_q;
    logic [HMSG_TREE_BITS-1:0]      tree_index_q;
    logic [HMSG_LEAF_BITS-1:0]      leaf_index_q;
    logic [5:0]                     wots_element_q;
    logic [3:0]                     auth_height_q;
    logic [8*N_BYTES-1:0]           node_q;
    logic [8*N_BYTES-1:0]           auth_q;

    logic                           wots_start;
    logic                           wots_element_valid;
    wire                            wots_element_ready;
    wire                            wots_busy;
    wire                            wots_done;
    wire                            wots_error;
    wire [MAX_NODE_BITS-1:0]        wots_public_key;

    logic [3:0]                     parent_height;
    logic [HMSG_LEAF_BITS-1:0]      parent_index;
    slh_adrs_t                      tree_adrs;
    logic                           fh_start;
    logic [MAX_NODE_BITS-1:0]       fh_message;
    wire                            fh_busy;
    wire                            fh_done;
    wire [MAX_NODE_BITS-1:0]        fh_result;

    slh_wots_pk_from_sig u_wots (
        .clk,
        .rst_n,
        .start         (wots_start),
        .pk_seed       (pk_seed_q),
        .message       (message_q),
        .layer         (layer_q),
        .tree_index    (tree_index_q),
        .leaf_index    (leaf_index_q),
        .element_valid (wots_element_valid),
        .element,
        .element_ready (wots_element_ready),
        .busy          (wots_busy),
        .done          (wots_done),
        .error         (wots_error),
        .public_key    (wots_public_key)
    );

    slh_hash_fh u_hash_h (
        .clk,
        .rst_n,
        .start   (fh_start),
        .use_h   (1'b1),
        .pk_seed (pk_seed_q),
        .adrs_c  (slh_compress_adrs(tree_adrs)),
        .message (fh_message),
        .busy    (fh_busy),
        .done    (fh_done),
        .result  (fh_result)
    );

    assign busy = (state_q != StIdle);
    wire wots_key_aligned = (wots_public_key[127:0] == '0);

    always_comb begin
        wots_start = (state_q == StWotsStart) && !wots_busy;
        wots_element_valid = (state_q == StWotsFeed) && element_valid;

        element_ready = 1'b0;
        if (state_q == StWotsFeed) begin
            element_ready = wots_element_ready;
        end else if (state_q == StWaitAuth) begin
            element_ready = 1'b1;
        end

        parent_height = auth_height_q + 1'b1;
        parent_index = leaf_index_q >> parent_height;
        tree_adrs            = '0;
        tree_adrs.layer      = {29'b0, layer_q};
        tree_adrs.tree       = {42'b0, tree_index_q};
        tree_adrs.type_field = {24'b0, SlhAdrsTree};
        tree_adrs.word6      = {28'b0, parent_height};
        tree_adrs.word7      = {23'b0, parent_index};

        fh_start = (state_q == StStartParent) && !fh_busy;
        if (leaf_index_q[auth_height_q] == 1'b0) begin
            fh_message = {node_q, auth_q};
        end else begin
            fh_message = {auth_q, node_q};
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q        <= StIdle;
            pk_seed_q      <= '0;
            message_q      <= '0;
            layer_q        <= '0;
            tree_index_q   <= '0;
            leaf_index_q   <= '0;
            wots_element_q <= '0;
            auth_height_q  <= '0;
            node_q         <= '0;
            auth_q         <= '0;
            done           <= 1'b0;
            error          <= 1'b0;
            root           <= '0;
        end else begin
            done  <= 1'b0;
            error <= 1'b0;

            unique case (state_q)
                StIdle: begin
                    if (start) begin
                        pk_seed_q      <= pk_seed;
                        message_q      <= message;
                        layer_q        <= layer;
                        tree_index_q   <= tree_index;
                        leaf_index_q   <= leaf_index;
                        wots_element_q <= '0;
                        root           <= '0;
                        state_q        <= StWotsStart;
                    end
                end

                StWotsStart: state_q <= StWotsFeed;

                StWotsFeed: begin
                    if (element_valid && wots_element_ready) begin
                        if (wots_element_q == LAST_WOTS_ELEMENT) begin
                            state_q <= StWotsWait;
                        end else begin
                            wots_element_q <= wots_element_q + 1'b1;
                        end
                    end
                end

                StWotsWait: begin
                    if (wots_done) begin
                        if (wots_error || !wots_key_aligned) begin
                            error   <= 1'b1;
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else begin
                            node_q        <= wots_public_key[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            auth_height_q <= '0;
                            state_q       <= StWaitAuth;
                        end
                    end
                end

                StWaitAuth: begin
                    if (element_valid) begin
                        auth_q <= element[
                            MAX_NODE_BITS-1 -: 8*N_BYTES];
                        state_q <= StStartParent;
                    end
                end

                StStartParent: state_q <= StWaitParent;

                StWaitParent: begin
                    if (fh_done) begin
                        node_q <= fh_result[
                            MAX_NODE_BITS-1 -: 8*N_BYTES];
                        if (auth_height_q == LAST_AUTH_HEIGHT) begin
                            root    <= fh_result;
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else begin
                            auth_height_q <= auth_height_q + 1'b1;
                            state_q       <= StWaitAuth;
                        end
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
