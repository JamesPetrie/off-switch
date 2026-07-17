
module off_switch_axi_slave_lite_v1_0_S00_AXI #
(
    parameter integer C_S_AXI_DATA_WIDTH = 32,
    parameter integer C_S_AXI_ADDR_WIDTH = 7,
    parameter integer CRYPTO_TYPE = 0
)
(
    input wire  S_AXI_ACLK,
    input wire  S_AXI_ARESETN,
    input wire [C_S_AXI_ADDR_WIDTH-1 : 0] S_AXI_AWADDR,
    input wire [2 : 0] S_AXI_AWPROT,
    input wire  S_AXI_AWVALID,
    output wire  S_AXI_AWREADY,
    input wire [C_S_AXI_DATA_WIDTH-1 : 0] S_AXI_WDATA,
    input wire [(C_S_AXI_DATA_WIDTH/8)-1 : 0] S_AXI_WSTRB,
    input wire  S_AXI_WVALID,
    output wire  S_AXI_WREADY,
    output wire [1 : 0] S_AXI_BRESP,
    output wire  S_AXI_BVALID,
    input wire  S_AXI_BREADY,
    input wire [C_S_AXI_ADDR_WIDTH-1 : 0] S_AXI_ARADDR,
    input wire [2 : 0] S_AXI_ARPROT,
    input wire  S_AXI_ARVALID,
    output wire  S_AXI_ARREADY,
    output wire [C_S_AXI_DATA_WIDTH-1 : 0] S_AXI_RDATA,
    output wire [1 : 0] S_AXI_RRESP,
    output wire  S_AXI_RVALID,
    input wire  S_AXI_RREADY,

    // Hooked to security_block in the wrapper.
    input  wire [255:0] nonce,
    input  wire         nonce_ready,
    output wire [511:0] license,
    output wire         license_valid,
    input  wire         license_ready,
    input  wire         license_passed,

    output wire         slh_sig_valid,
    input  wire         slh_sig_ready,
    output wire [63:0]  slh_sig_data,
    output wire [7:0]   slh_sig_keep,
    output wire         slh_sig_last,

    input  wire [63:0]  allowance,
    input  wire         enabled
);


    // AXI4LITE handshake state
    reg [C_S_AXI_ADDR_WIDTH-1 : 0]  axi_awaddr;
    reg                             axi_awready;
    reg                             axi_wready;
    reg [1 : 0]                     axi_bresp;
    reg                             axi_bvalid;
    reg [C_S_AXI_ADDR_WIDTH-1 : 0]  axi_araddr;
    reg                             axi_arready;
    reg [1 : 0]                     axi_rresp;
    reg                             axi_rvalid;

    // ADDR_LSB = 2 for 32-bit data; OPT_MEM_ADDR_BITS=4 -> 5-bit word index (32 slots).
    // Address map (byte offset / word index):
    //   0x00..0x1C  ( 0..7 )  NONCE     (RO, 256 b)
    //   0x20..0x3C  ( 8..15)  LICENSE.s (RW, 256 b)
    //   0x40..0x5C  (16..23)  LICENSE.r (RW, 256 b)
    //   0x60        (   24 )  CONTROL   (WO, bit 0 start, bit 1 clear status)
    //   0x64        (   25 )  STATUS    (RO, mode/stream/result flags)
    //   0x68        (   26 )  SIG_LO    (RW, first four signature bytes)
    //   0x6C        (   27 )  SIG_HI    (RW, next four signature bytes)
    //   0x70        (   28 )  SIG_PUSH  (WO, bit 0 push, bit 1 last)
    //   0x74        (   29 )  SIG_COUNT (RO, transferred 64-bit words)
    //   0x78..0x7C  (30..31)  ALLOWANCE (RO, 64 b)
    localparam integer ADDR_LSB          = (C_S_AXI_DATA_WIDTH/32) + 1;
    localparam integer OPT_MEM_ADDR_BITS = 4;

    ecdsa_pkg::license_t license_q;
    reg         license_valid_q;
    reg [31:0]  sig_data_lo_q;
    reg [31:0]  sig_data_hi_q;
    reg [63:0]  sig_word_q;
    reg         sig_valid_q;
    reg         sig_last_q;
    reg         done_sticky_q;
    reg         passed_sticky_q;
    reg         overflow_sticky_q;
    reg [15:0]  sig_word_count_q;
    integer     byte_index;
    integer     sig_byte_index;

    assign license       = license_q;
    assign license_valid = license_valid_q;
    assign slh_sig_valid = sig_valid_q;
    assign slh_sig_data  = sig_word_q;
    assign slh_sig_keep  = 8'hff;
    assign slh_sig_last  = sig_last_q;

    wire sig_slot_available = !sig_valid_q || slh_sig_ready;

    assign S_AXI_AWREADY = axi_awready;
    assign S_AXI_WREADY  = axi_wready;
    wire unused_axi_inputs = 1'b0 & ^{
        S_AXI_AWPROT, S_AXI_ARPROT,
        axi_awaddr[ADDR_LSB-1:0], axi_araddr[ADDR_LSB-1:0]
    };
    assign S_AXI_BRESP   = axi_bresp | {2{unused_axi_inputs}};
    assign S_AXI_BVALID  = axi_bvalid;
    assign S_AXI_ARREADY = axi_arready;
    assign S_AXI_RRESP   = axi_rresp;
    assign S_AXI_RVALID  = axi_rvalid;

    reg [1:0] state_write;
    reg [1:0] state_read;
    localparam Idle = 2'b00, Raddr = 2'b10, Rdata = 2'b11, Waddr = 2'b10, Wdata = 2'b11;

    // -------------------------------------------------------------------------
    // Write address/handshake FSM (preserved from generated template)
    // -------------------------------------------------------------------------
    always @(posedge S_AXI_ACLK) begin
        if (S_AXI_ARESETN == 1'b0) begin
            axi_awready <= 0;
            axi_wready  <= 0;
            axi_bvalid  <= 0;
            axi_bresp   <= 0;
            axi_awaddr  <= 0;
            state_write <= Idle;
        end else begin
            case (state_write)
                Idle: begin
                    if (S_AXI_ARESETN == 1'b1) begin
                        axi_awready <= 1'b1;
                        axi_wready  <= 1'b1;
                        state_write <= Waddr;
                    end
                end
                Waddr: begin
                    if (S_AXI_AWVALID && S_AXI_AWREADY) begin
                        axi_awaddr <= S_AXI_AWADDR;
                        if (S_AXI_WVALID) begin
                            axi_awready <= 1'b1;
                            state_write <= Waddr;
                            axi_bvalid  <= 1'b1;
                        end else begin
                            axi_awready <= 1'b0;
                            state_write <= Wdata;
                            if (S_AXI_BREADY && axi_bvalid) axi_bvalid <= 1'b0;
                        end
                    end else begin
                        if (S_AXI_BREADY && axi_bvalid) axi_bvalid <= 1'b0;
                    end
                end
                Wdata: begin
                    if (S_AXI_WVALID) begin
                        state_write <= Waddr;
                        axi_bvalid  <= 1'b1;
                        axi_awready <= 1'b1;
                    end else begin
                        if (S_AXI_BREADY && axi_bvalid) axi_bvalid <= 1'b0;
                    end
                end
                default: begin
                    axi_awready <= 1'b0;
                    axi_wready  <= 1'b0;
                    axi_bvalid  <= 1'b0;
                    state_write <= Idle;
                end
            endcase
        end
    end

    // -------------------------------------------------------------------------
    // Write decode
    // -------------------------------------------------------------------------
    wire [OPT_MEM_ADDR_BITS:0] waddr_word =
        (S_AXI_AWVALID) ? S_AXI_AWADDR[ADDR_LSB+OPT_MEM_ADDR_BITS:ADDR_LSB]
                        : axi_awaddr [ADDR_LSB+OPT_MEM_ADDR_BITS:ADDR_LSB];

    // LICENSE writes: s in words 8..15 (0x20..0x3C), r in words 16..23 (0x40..0x5C)
    always @(posedge S_AXI_ACLK) begin
        if (S_AXI_ARESETN == 1'b0) begin
            license_q <= '0;
        end else if (S_AXI_WVALID) begin
            for (byte_index = 0; byte_index <= (C_S_AXI_DATA_WIDTH/8)-1; byte_index = byte_index + 1) begin
                if (S_AXI_WSTRB[byte_index]) begin
                    if (waddr_word >= 5'd8 && waddr_word <= 5'd15) begin
                        license_q.s[(int'(waddr_word) - 8)*32 + byte_index*8 +: 8]
                            <= S_AXI_WDATA[byte_index*8 +: 8];
                    end else if (waddr_word >= 5'd16 && waddr_word <= 5'd23) begin
                        license_q.r[(int'(waddr_word) - 16)*32 + byte_index*8 +: 8]
                            <= S_AXI_WDATA[byte_index*8 +: 8];
                    end
                end
            end
        end
    end

    // CONTROL start is held until security_block reports verification complete.
    always @(posedge S_AXI_ACLK) begin
        if (S_AXI_ARESETN == 1'b0) begin
            license_valid_q <= 1'b0;
        end else if (license_ready) begin
            license_valid_q <= 1'b0;
        end else if (S_AXI_WVALID && waddr_word == 5'd24
                     && S_AXI_WSTRB[0] && S_AXI_WDATA[0]) begin
            license_valid_q <= 1'b1;
        end
    end

    // SLH-DSA staging registers. Software writes both halves, then pushes one
    // 64-bit word into a one-entry valid/ready FIFO.
    always @(posedge S_AXI_ACLK) begin
        if (S_AXI_ARESETN == 1'b0) begin
            sig_data_lo_q <= 32'b0;
            sig_data_hi_q <= 32'b0;
        end else if (S_AXI_WVALID) begin
            for (sig_byte_index = 0;
                 sig_byte_index <= (C_S_AXI_DATA_WIDTH/8)-1;
                 sig_byte_index = sig_byte_index + 1) begin
                if (S_AXI_WSTRB[sig_byte_index]) begin
                    if (waddr_word == 5'd26) begin
                        sig_data_lo_q[sig_byte_index*8 +: 8]
                            <= S_AXI_WDATA[sig_byte_index*8 +: 8];
                    end else if (waddr_word == 5'd27) begin
                        sig_data_hi_q[sig_byte_index*8 +: 8]
                            <= S_AXI_WDATA[sig_byte_index*8 +: 8];
                    end
                end
            end
        end
    end

    always @(posedge S_AXI_ACLK) begin
        if (S_AXI_ARESETN == 1'b0) begin
            sig_word_q  <= 64'b0;
            sig_valid_q <= 1'b0;
            sig_last_q  <= 1'b0;
        end else begin
            if (sig_valid_q && slh_sig_ready) begin
                sig_valid_q <= 1'b0;
                sig_last_q  <= 1'b0;
            end
            if (S_AXI_WVALID && waddr_word == 5'd28
                && S_AXI_WSTRB[0] && S_AXI_WDATA[0]
                && sig_slot_available) begin
                sig_word_q  <= {sig_data_hi_q, sig_data_lo_q};
                sig_valid_q <= 1'b1;
                sig_last_q  <= S_AXI_WDATA[1];
            end
        end
    end

    // Sticky result state prevents software from missing one-cycle pulses.
    always @(posedge S_AXI_ACLK) begin
        if (S_AXI_ARESETN == 1'b0) begin
            done_sticky_q     <= 1'b0;
            passed_sticky_q   <= 1'b0;
            overflow_sticky_q <= 1'b0;
            sig_word_count_q  <= 16'b0;
        end else begin
            if (S_AXI_WVALID && waddr_word == 5'd24
                && S_AXI_WSTRB[0]
                && (S_AXI_WDATA[0] || S_AXI_WDATA[1])) begin
                done_sticky_q     <= 1'b0;
                passed_sticky_q   <= 1'b0;
                overflow_sticky_q <= 1'b0;
                sig_word_count_q  <= 16'b0;
            end
            if (sig_valid_q && slh_sig_ready) begin
                sig_word_count_q <= sig_word_count_q + 1'b1;
            end
            if (S_AXI_WVALID && waddr_word == 5'd28
                && S_AXI_WSTRB[0] && S_AXI_WDATA[0]
                && !sig_slot_available) begin
                overflow_sticky_q <= 1'b1;
            end
            if (license_ready) begin
                done_sticky_q   <= 1'b1;
                passed_sticky_q <= license_passed;
            end
        end
    end

    // -------------------------------------------------------------------------
    // Read FSM (preserved)
    // -------------------------------------------------------------------------
    always @(posedge S_AXI_ACLK) begin
        if (S_AXI_ARESETN == 1'b0) begin
            axi_arready <= 1'b0;
            axi_rvalid  <= 1'b0;
            axi_rresp   <= 2'b0;
            state_read  <= Idle;
        end else begin
            case (state_read)
                Idle: begin
                    if (S_AXI_ARESETN == 1'b1) begin
                        state_read  <= Raddr;
                        axi_arready <= 1'b1;
                    end
                end
                Raddr: begin
                    if (S_AXI_ARVALID && S_AXI_ARREADY) begin
                        state_read  <= Rdata;
                        axi_araddr  <= S_AXI_ARADDR;
                        axi_rvalid  <= 1'b1;
                        axi_arready <= 1'b0;
                    end
                end
                Rdata: begin
                    if (S_AXI_RVALID && S_AXI_RREADY) begin
                        axi_rvalid  <= 1'b0;
                        axi_arready <= 1'b1;
                        state_read  <= Raddr;
                    end
                end
                default: begin
                    axi_arready <= 1'b0;
                    axi_rvalid  <= 1'b0;
                    state_read  <= Idle;
                end
            endcase
        end
    end

    // -------------------------------------------------------------------------
    // Read decode
    // -------------------------------------------------------------------------
    wire [OPT_MEM_ADDR_BITS:0] raddr_word =
        axi_araddr[ADDR_LSB+OPT_MEM_ADDR_BITS:ADDR_LSB];

    reg [C_S_AXI_DATA_WIDTH-1:0] rdata_mux;
    always @(*) begin
        if (raddr_word <= 5'd7) begin
            rdata_mux = nonce[raddr_word*32 +: 32];
        end else if (raddr_word >= 5'd8 && raddr_word <= 5'd15) begin
            rdata_mux = license_q.s[(int'(raddr_word) - 8)*32 +: 32];
        end else if (raddr_word >= 5'd16 && raddr_word <= 5'd23) begin
            rdata_mux = license_q.r[(int'(raddr_word) - 16)*32 +: 32];
        end else if (raddr_word == 5'd25) begin
            rdata_mux = {
                22'b0,
                CRYPTO_TYPE[1:0],
                slh_sig_ready,
                enabled,
                overflow_sticky_q,
                passed_sticky_q,
                done_sticky_q,
                sig_slot_available,
                license_valid_q,
                nonce_ready
            };
        end else if (raddr_word == 5'd26) begin
            rdata_mux = sig_data_lo_q;
        end else if (raddr_word == 5'd27) begin
            rdata_mux = sig_data_hi_q;
        end else if (raddr_word == 5'd29) begin
            rdata_mux = {16'b0, sig_word_count_q};
        end else if (raddr_word == 5'd30) begin
            rdata_mux = allowance[31:0];
        end else if (raddr_word == 5'd31) begin
            rdata_mux = allowance[63:32];
        end else begin
            rdata_mux = 32'b0;
        end
    end

    assign S_AXI_RDATA = rdata_mux;

endmodule
