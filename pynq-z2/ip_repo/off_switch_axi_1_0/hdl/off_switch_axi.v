
module off_switch_axi #
(
    parameter integer C_S00_AXI_DATA_WIDTH = 32,
    parameter integer C_S00_AXI_ADDR_WIDTH = 7,
    parameter integer CRYPTO_TYPE = 2,
    // Demo public key. Its secret half remains only in the external authority.
    parameter [127:0] SLH_PK_SEED =
        128'h60a3dbd593dcc6e3bffd214c7603259b,
    parameter [127:0] SLH_PK_ROOT =
        128'h835c3ada1c98b54a217b46638bf956eb,
    parameter [127:0] SLH_DEVICE_ID =
        128'h00112233445566778899aabbccddeeff,
    parameter [63:0] SLH_POLICY_EPOCH = 64'd1
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

    localparam integer SECURITY_LICENSE_WIDTH =
        (CRYPTO_TYPE == 2) ? 1 : 512;

    wire [255:0] nonce;
    wire         nonce_ready;
    wire [511:0] ecdsa_license;
    wire [SECURITY_LICENSE_WIDTH-1:0] security_license;
    wire         license_valid;
    wire         license_ready;
    wire         license_passed;
    wire         slh_sig_valid;
    wire         slh_sig_ready;
    wire [63:0]  slh_sig_data;
    wire [7:0]   slh_sig_keep;
    wire         slh_sig_last;
    wire [63:0]  allowance;
    wire         enabled;
    wire [7:0]   unused_workload_result;
    wire         unused_result_valid;

    generate
        if (CRYPTO_TYPE == 0) begin : g_ecdsa_license
            assign security_license = ecdsa_license;
        end else begin : g_stream_license
            assign security_license = 1'b0 & ^ecdsa_license;
        end
    endgenerate

    initial begin
        if ((CRYPTO_TYPE != 0) && (CRYPTO_TYPE != 2)) begin
            $error("PYNQ wrapper supports CRYPTO_TYPE 0 (ECDSA) or 2 (SLH-DSA)");
        end
    end

    off_switch_axi_slave_lite_v1_0_S00_AXI # (
        .C_S_AXI_DATA_WIDTH(C_S00_AXI_DATA_WIDTH),
        .C_S_AXI_ADDR_WIDTH(C_S00_AXI_ADDR_WIDTH),
        .CRYPTO_TYPE(CRYPTO_TYPE)
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

        .nonce          (nonce),
        .nonce_ready    (nonce_ready),
        .license        (ecdsa_license),
        .license_valid  (license_valid),
        .license_ready  (license_ready),
        .license_passed (license_passed),
        .slh_sig_valid  (slh_sig_valid),
        .slh_sig_ready  (slh_sig_ready),
        .slh_sig_data   (slh_sig_data),
        .slh_sig_keep   (slh_sig_keep),
        .slh_sig_last   (slh_sig_last),
        .allowance      (allowance),
        .enabled        (enabled)
    );

    security_block #(
        .CRYPTO_TYPE      (CRYPTO_TYPE),
        .NUM_SIGNERS      (1),
        .SLH_STREAM_INPUT (CRYPTO_TYPE == 2),
        .SLH_PK_SEED      (SLH_PK_SEED),
        .SLH_PK_ROOT      (SLH_PK_ROOT),
        .SLH_DEVICE_ID    (SLH_DEVICE_ID),
        .SLH_POLICY_EPOCH (SLH_POLICY_EPOCH)
    ) u_security (
        .clk             (s00_axi_aclk),
        .rst_n           (s00_axi_aresetn),
        .license_valid   (license_valid),
        .license_ready   (license_ready),
        .license         (security_license),
        .license_passed  (license_passed),
        .slh_sig_valid   (slh_sig_valid),
        .slh_sig_ready   (slh_sig_ready),
        .slh_sig_data    (slh_sig_data),
        .slh_sig_keep    (slh_sig_keep),
        .slh_sig_last    (slh_sig_last),
        .workload_valid  (1'b0),            // Workload path unused
        .workload_a      (8'b0),
        .workload_b      (8'b0),
        .trng_seed       (256'b0),
        .trng_load_seed  (1'b0),            // TRNG runs free
        .nonce           (nonce),
        .nonce_ready     (nonce_ready),
        .workload_result (unused_workload_result),
        .result_valid    (unused_result_valid),
        .allowance       (allowance),
        .enabled         (enabled)
    );

    assign led = enabled | (1'b0 & ^{
        unused_workload_result, unused_result_valid
    });

endmodule
