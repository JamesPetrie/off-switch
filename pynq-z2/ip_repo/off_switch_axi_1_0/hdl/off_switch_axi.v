
`timescale 1 ns / 1 ps

module off_switch_axi #
(
    parameter integer C_S00_AXI_DATA_WIDTH = 32,
    parameter integer C_S00_AXI_ADDR_WIDTH = 7
)
(
    input wire  s00_axi_aclk,
    input wire  s00_axi_aresetn,
    input wire [C_S00_AXI_ADDR_WIDTH-1 : 0] s00_axi_awaddr,
    input wire [2 : 0] s00_axi_awprot,
    input wire  s00_axi_awvalid,
    output wire  s00_axi_awready,
    input wire [C_S00_AXI_DATA_WIDTH-1 : 0] s00_axi_wdata,
    input wire [(C_S00_AXI_DATA_WIDTH/8)-1 : 0] s00_axi_wstrb,
    input wire  s00_axi_wvalid,
    output wire  s00_axi_wready,
    output wire [1 : 0] s00_axi_bresp,
    output wire  s00_axi_bvalid,
    input wire  s00_axi_bready,
    input wire [C_S00_AXI_ADDR_WIDTH-1 : 0] s00_axi_araddr,
    input wire [2 : 0] s00_axi_arprot,
    input wire  s00_axi_arvalid,
    output wire  s00_axi_arready,
    output wire [C_S00_AXI_DATA_WIDTH-1 : 0] s00_axi_rdata,
    output wire [1 : 0] s00_axi_rresp,
    output wire  s00_axi_rvalid,
    input wire  s00_axi_rready,

    output wire led
);

    wire [255:0] nonce;
    wire [511:0] license;
    wire         license_valid;
    wire         license_ready;

    off_switch_axi_slave_lite_v1_0_S00_AXI # (
        .C_S_AXI_DATA_WIDTH(C_S00_AXI_DATA_WIDTH),
        .C_S_AXI_ADDR_WIDTH(C_S00_AXI_ADDR_WIDTH)
    ) off_switch_axi_slave_lite_v1_0_S00_AXI_inst (
        .S_AXI_ACLK(s00_axi_aclk),
        .S_AXI_ARESETN(s00_axi_aresetn),
        .S_AXI_AWADDR(s00_axi_awaddr),
        .S_AXI_AWPROT(s00_axi_awprot),
        .S_AXI_AWVALID(s00_axi_awvalid),
        .S_AXI_AWREADY(s00_axi_awready),
        .S_AXI_WDATA(s00_axi_wdata),
        .S_AXI_WSTRB(s00_axi_wstrb),
        .S_AXI_WVALID(s00_axi_wvalid),
        .S_AXI_WREADY(s00_axi_wready),
        .S_AXI_BRESP(s00_axi_bresp),
        .S_AXI_BVALID(s00_axi_bvalid),
        .S_AXI_BREADY(s00_axi_bready),
        .S_AXI_ARADDR(s00_axi_araddr),
        .S_AXI_ARPROT(s00_axi_arprot),
        .S_AXI_ARVALID(s00_axi_arvalid),
        .S_AXI_ARREADY(s00_axi_arready),
        .S_AXI_RDATA(s00_axi_rdata),
        .S_AXI_RRESP(s00_axi_rresp),
        .S_AXI_RVALID(s00_axi_rvalid),
        .S_AXI_RREADY(s00_axi_rready),

        .nonce         (nonce),
        .license       (license),
        .license_valid (license_valid),
        .license_ready (license_ready)
    );

    security_block #(
        .CRYPTO_TYPE(1'b0), // ECDSA build (CRYPTO_TYPE=0)
        .NUM_SIGNERS(1)     // Single signer
    ) u_security (
        .clk             (s00_axi_aclk),
        .rst_n           (s00_axi_aresetn),
        .license_valid   (license_valid),
        .license_ready   (license_ready),
        .license         (license),
        .workload_valid  (1'b0),            // Workload path unused
        .workload_a      (8'b0),
        .workload_b      (8'b0),
        .trng_seed       (256'b0),
        .trng_load_seed  (1'b0),            // TRNG runs free
        .nonce           (nonce),
        .nonce_ready     (),                // The client only checks the nonce value right now (e.g. non-zero, changed vs last sample)
        .workload_result (),
        .result_valid    (),                // Workload path unused
        .allowance       (),                // Allowance not monitored for now
        .enabled         (led)
    );

endmodule
