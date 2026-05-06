
`timescale 1 ns / 1 ps

module off_switch_axi_slave_lite_v1_0_S00_AXI #
(
    parameter integer C_S_AXI_DATA_WIDTH = 32,
    parameter integer C_S_AXI_ADDR_WIDTH = 7
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
    output wire [511:0] license,
    output wire         license_valid,
    input  wire         license_ready
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
    //   0x60        (   24 )  START     (WO, any write asserts license_valid)
    localparam integer ADDR_LSB          = (C_S_AXI_DATA_WIDTH/32) + 1;
    localparam integer OPT_MEM_ADDR_BITS = 4;

    ecdsa_pkg::license_t license_q;
    reg         license_valid_q;
    integer     byte_index;

    assign license       = license_q;
    assign license_valid = license_valid_q;

    assign S_AXI_AWREADY = axi_awready;
    assign S_AXI_WREADY  = axi_wready;
    assign S_AXI_BRESP   = axi_bresp;
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
                        license_q.s[(waddr_word - 5'd8)*32 + byte_index*8 +: 8]
                            <= S_AXI_WDATA[byte_index*8 +: 8];
                    end else if (waddr_word >= 5'd16 && waddr_word <= 5'd23) begin
                        license_q.r[(waddr_word - 5'd16)*32 + byte_index*8 +: 8]
                            <= S_AXI_WDATA[byte_index*8 +: 8];
                    end
                end
            end
        end
    end

    // START (word 24, offset 0x60): pulse license_valid until license_ready clears it
    always @(posedge S_AXI_ACLK) begin
        if (S_AXI_ARESETN == 1'b0) begin
            license_valid_q <= 1'b0;
        end else if (license_ready) begin
            license_valid_q <= 1'b0;
        end else if (S_AXI_WVALID && waddr_word == 5'd24) begin
            license_valid_q <= 1'b1;
        end
    end

    // -------------------------------------------------------------------------
    // Read FSM (preserved)
    // -------------------------------------------------------------------------
    always @(posedge S_AXI_ACLK) begin
        if (S_AXI_ARESETN == 1'b0) begin
            axi_arready <= 1'b0;
            axi_rvalid  <= 1'b0;
            axi_rresp   <= 1'b0;
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
            rdata_mux = license_q.s[(raddr_word - 5'd8)*32 +: 32];
        end else if (raddr_word >= 5'd16 && raddr_word <= 5'd23) begin
            rdata_mux = license_q.r[(raddr_word - 5'd16)*32 +: 32];
        end else begin
            rdata_mux = 32'b0;
        end
    end

    assign S_AXI_RDATA = rdata_mux;

endmodule
