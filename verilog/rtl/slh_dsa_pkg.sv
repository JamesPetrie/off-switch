package slh_dsa_pkg;

    // FIPS 205 SLH-DSA-SHA2-128s. Keep all algorithm dimensions here so the
    // controllers do not accumulate hard-coded bounds and byte offsets.
    localparam int unsigned N_BYTES       = 16;
    localparam int unsigned H             = 63;
    localparam int unsigned D             = 7;
    localparam int unsigned HP            = 9;
    localparam int unsigned A             = 12;
    localparam int unsigned K             = 14;
    localparam int unsigned LG_W          = 4;
    localparam int unsigned W             = 1 << LG_W;
    localparam int unsigned M_BYTES       = 30;

    localparam int unsigned WOTS_LEN1     = (8 * N_BYTES + LG_W - 1) / LG_W;
    localparam int unsigned WOTS_LEN2     = 3;
    localparam int unsigned WOTS_LEN      = WOTS_LEN1 + WOTS_LEN2;

    localparam int unsigned PK_SEED_BYTES = N_BYTES;
    localparam int unsigned PK_ROOT_BYTES = N_BYTES;
    localparam int unsigned PK_BYTES      = PK_SEED_BYTES + PK_ROOT_BYTES;

    localparam int unsigned SIG_R_BYTES    = N_BYTES;
    localparam int unsigned FORS_SIG_BYTES = K * (A + 1) * N_BYTES;
    localparam int unsigned XMSS_SIG_BYTES = (WOTS_LEN + HP) * N_BYTES;
    localparam int unsigned HT_SIG_BYTES   = D * XMSS_SIG_BYTES;
    localparam int unsigned SIG_BYTES      = SIG_R_BYTES
                                                    + FORS_SIG_BYTES
                                                    + HT_SIG_BYTES;

    localparam int unsigned SIG_ELEMENTS   = SIG_BYTES / N_BYTES;
    localparam int unsigned FORS_ELEMENTS  = FORS_SIG_BYTES / N_BYTES;
    localparam int unsigned HT_ELEMENTS    = HT_SIG_BYTES / N_BYTES;

    localparam int unsigned HMSG_FORS_BITS  = K * A;
    localparam int unsigned HMSG_FORS_BYTES = (HMSG_FORS_BITS + 7) / 8;
    localparam int unsigned HMSG_TREE_BITS  = H - HP;
    localparam int unsigned HMSG_TREE_BYTES = (HMSG_TREE_BITS + 7) / 8;
    localparam int unsigned HMSG_LEAF_BITS  = HP;
    localparam int unsigned HMSG_LEAF_BYTES = (HMSG_LEAF_BITS + 7) / 8;

    // Reserve the category-5 node width at module boundaries. Only the top
    // 8*N_BYTES bits carry data for the selected profile.
    localparam int unsigned MAX_N_BYTES    = 32;
    localparam int unsigned MAX_NODE_BITS  = 8 * MAX_N_BYTES;

    localparam int unsigned STREAM_BITS    = 64;
    localparam int unsigned STREAM_BYTES   = STREAM_BITS / 8;

    localparam int unsigned OFFSWITCH_MESSAGE_BYTES = 72;
    localparam logic [127:0] OFFSWITCH_DOMAIN =
        128'h4f46465357495443482d534c482d5631; // "OFFSWITCH-SLH-V1"
    localparam logic [127:0] OFFSWITCH_DEFAULT_DEVICE_ID =
        128'h00112233445566778899aabbccddeeff;

    typedef enum logic [1:0] {
        SlhRegionR,
        SlhRegionFors,
        SlhRegionHt,
        SlhRegionInvalid
    } slh_region_e;

    // FIPS 205 Section 4.3 address types. The full ADRS is kept in the
    // controller-friendly 32-byte form; SHA-2 member functions consume the
    // 22-byte compressed form returned by slh_compress_adrs().
    typedef enum logic [7:0] {
        SlhAdrsWotsHash  = 8'd0,
        SlhAdrsWotsPk    = 8'd1,
        SlhAdrsTree      = 8'd2,
        SlhAdrsForsTree  = 8'd3,
        SlhAdrsForsRoots = 8'd4,
        SlhAdrsWotsPrf   = 8'd5,
        SlhAdrsForsPrf   = 8'd6
    } slh_adrs_type_e;

    typedef struct packed {
        logic [31:0] layer;
        logic [95:0] tree;
        logic [31:0] type_field;
        logic [31:0] key_pair;
        logic [31:0] word6;
        logic [31:0] word7;
    } slh_adrs_t;

    function automatic logic [175:0] slh_compress_adrs(
        input slh_adrs_t adrs
    );
        // ADRSc = ADRS[3] || ADRS[8:16] || ADRS[19:32]. All fields in
        // slh_adrs_t are represented in host-independent numeric order, so
        // concatenation below emits the required big-endian byte string.
        return {
            adrs.layer[7:0],
            adrs.tree[63:0],
            adrs.type_field[7:0],
            adrs.key_pair,
            adrs.word6,
            adrs.word7
        };
    endfunction

    function automatic slh_adrs_t slh_adrs_set_type_and_clear(
        input slh_adrs_t current,
        input slh_adrs_type_e new_type
    );
        slh_adrs_t updated;
        updated = current;
        updated.type_field = {24'b0, new_type};
        updated.key_pair   = '0;
        updated.word6      = '0;
        updated.word7      = '0;
        return updated;
    endfunction

    typedef enum logic [2:0] {
        SlhHashSha256,
        SlhHashSha512,
        SlhHashReserved2,
        SlhHashReserved3,
        SlhHashReserved4,
        SlhHashReserved5,
        SlhHashReserved6,
        SlhHashReserved7
    } slh_hash_algorithm_e;

    typedef enum logic [2:0] {
        SlhHashHmsg,
        SlhHashF,
        SlhHashH,
        SlhHashTl,
        SlhHashMgf1,
        SlhHashFunctionReserved5,
        SlhHashFunctionReserved6,
        SlhHashFunctionReserved7
    } slh_hash_function_e;

    typedef enum logic [3:0] {
        SlhErrNone,
        SlhErrBadKeep,
        SlhErrEarlyLast,
        SlhErrMissingLast,
        SlhErrLength,
        SlhErrProtocol,
        SlhErrReserved6,
        SlhErrReserved7,
        SlhErrReserved8,
        SlhErrReserved9,
        SlhErrReserved10,
        SlhErrReserved11,
        SlhErrReserved12,
        SlhErrReserved13,
        SlhErrReserved14,
        SlhErrReserved15
    } slh_error_e;

endpackage
