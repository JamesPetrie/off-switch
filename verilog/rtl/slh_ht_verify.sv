// FIPS 205 Algorithm 13: seven-layer SHA2-128s hypertree recovery.
//
// One XMSS engine is reused for all D=7 layers. The first layer signs the FORS
// public key. Each recovered root becomes the next layer's message; the next
// leaf is the low h'=9 bits of the current tree index before that index shifts.

module slh_ht_verify
    import slh_dsa_pkg::*;
(
    input  logic                         clk,
    input  logic                         rst_n,

    input  logic                         start,
    input  logic [8*N_BYTES-1:0]         pk_seed,
    input  logic [8*N_BYTES-1:0]         message,
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

    localparam logic [2:0] LAST_LAYER = 3'(D - 1);
    localparam logic [5:0] LAST_XMSS_ELEMENT = 6'(WOTS_LEN + HP - 1);

    typedef enum logic [2:0] {
        StIdle,
        StXmssStart,
        StXmssFeed,
        StXmssWait
    } state_e;

    state_e                         state_q;
    logic [8*N_BYTES-1:0]           pk_seed_q;
    logic [8*N_BYTES-1:0]           message_q;
    logic [HMSG_TREE_BITS-1:0]      tree_index_q;
    logic [HMSG_LEAF_BITS-1:0]      leaf_index_q;
    logic [2:0]                     layer_q;
    logic [5:0]                     xmss_element_q;

    logic                           xmss_start;
    logic                           xmss_element_valid;
    wire                            xmss_element_ready;
    wire                            xmss_busy;
    wire                            xmss_done;
    wire                            xmss_error;
    wire [MAX_NODE_BITS-1:0]        xmss_root;

    slh_xmss_pk_from_sig u_xmss (
        .clk,
        .rst_n,
        .start         (xmss_start),
        .pk_seed       (pk_seed_q),
        .message       (message_q),
        .layer         (layer_q),
        .tree_index    (tree_index_q),
        .leaf_index    (leaf_index_q),
        .element_valid (xmss_element_valid),
        .element,
        .element_ready (xmss_element_ready),
        .busy          (xmss_busy),
        .done          (xmss_done),
        .error         (xmss_error),
        .root          (xmss_root)
    );

    assign busy               = (state_q != StIdle);
    assign element_ready      = (state_q == StXmssFeed)
                                && xmss_element_ready;
    assign xmss_start         = (state_q == StXmssStart) && !xmss_busy;
    assign xmss_element_valid = (state_q == StXmssFeed) && element_valid;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q        <= StIdle;
            pk_seed_q      <= '0;
            message_q      <= '0;
            tree_index_q   <= '0;
            leaf_index_q   <= '0;
            layer_q        <= '0;
            xmss_element_q <= '0;
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
                        tree_index_q   <= tree_index;
                        leaf_index_q   <= leaf_index;
                        layer_q        <= '0;
                        xmss_element_q <= '0;
                        root           <= '0;
                        state_q        <= StXmssStart;
                    end
                end

                StXmssStart: state_q <= StXmssFeed;

                StXmssFeed: begin
                    if (element_valid && xmss_element_ready) begin
                        if (xmss_element_q == LAST_XMSS_ELEMENT) begin
                            state_q <= StXmssWait;
                        end else begin
                            xmss_element_q <= xmss_element_q + 1'b1;
                        end
                    end
                end

                StXmssWait: begin
                    if (xmss_done) begin
                        if (xmss_error) begin
                            error   <= 1'b1;
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else if (layer_q == LAST_LAYER) begin
                            root    <= xmss_root;
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else begin
                            message_q      <= xmss_root[
                                MAX_NODE_BITS-1 -: 8*N_BYTES];
                            leaf_index_q   <= tree_index_q[
                                HMSG_LEAF_BITS-1:0];
                            tree_index_q   <= tree_index_q >> HP;
                            layer_q        <= layer_q + 1'b1;
                            xmss_element_q <= '0;
                            state_q        <= StXmssStart;
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
