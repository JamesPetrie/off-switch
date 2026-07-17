module tb (
    input logic clk,
    input logic rst_n
);

    localparam int ADDR_WIDTH = 7;
    localparam logic [ADDR_WIDTH-1:0] REG_CONTROL   = 7'h60;
    localparam logic [ADDR_WIDTH-1:0] REG_STATUS    = 7'h64;
    localparam logic [ADDR_WIDTH-1:0] REG_SIG_LO    = 7'h68;
    localparam logic [ADDR_WIDTH-1:0] REG_SIG_HI    = 7'h6c;
    localparam logic [ADDR_WIDTH-1:0] REG_SIG_PUSH  = 7'h70;
    localparam logic [ADDR_WIDTH-1:0] REG_SIG_COUNT = 7'h74;
    localparam logic [ADDR_WIDTH-1:0] REG_ALLOW_LO  = 7'h78;
    localparam logic [ADDR_WIDTH-1:0] REG_ALLOW_HI  = 7'h7c;

    logic [ADDR_WIDTH-1:0] awaddr;
    logic [2:0]            awprot;
    logic                  awvalid;
    wire                   awready;
    logic [31:0]           wdata;
    logic [3:0]            wstrb;
    logic                  wvalid;
    wire                   wready;
    wire [1:0]             bresp;
    wire                   bvalid;
    logic                  bready;
    logic [ADDR_WIDTH-1:0] araddr;
    logic [2:0]            arprot;
    logic                  arvalid;
    wire                   arready;
    wire [31:0]            rdata;
    wire [1:0]             rresp;
    wire                   rvalid;
    logic                  rready;

    logic [255:0]          nonce;
    logic                  nonce_ready;
    wire [511:0]           license;
    wire                   license_valid;
    logic                  license_ready;
    logic                  license_passed;
    wire                   slh_sig_valid;
    logic                  slh_sig_ready;
    wire [63:0]            slh_sig_data;
    wire [7:0]             slh_sig_keep;
    wire                   slh_sig_last;
    logic [63:0]           allowance;
    logic                  enabled;
    int                    tests_passed;

    off_switch_axi_slave_lite_v1_0_S00_AXI #(
        .C_S_AXI_DATA_WIDTH(32),
        .C_S_AXI_ADDR_WIDTH(ADDR_WIDTH),
        .CRYPTO_TYPE(2)
    ) dut (
        .S_AXI_ACLK     (clk),
        .S_AXI_ARESETN  (rst_n),
        .S_AXI_AWADDR   (awaddr),
        .S_AXI_AWPROT   (awprot),
        .S_AXI_AWVALID  (awvalid),
        .S_AXI_AWREADY  (awready),
        .S_AXI_WDATA    (wdata),
        .S_AXI_WSTRB    (wstrb),
        .S_AXI_WVALID   (wvalid),
        .S_AXI_WREADY   (wready),
        .S_AXI_BRESP    (bresp),
        .S_AXI_BVALID   (bvalid),
        .S_AXI_BREADY   (bready),
        .S_AXI_ARADDR   (araddr),
        .S_AXI_ARPROT   (arprot),
        .S_AXI_ARVALID  (arvalid),
        .S_AXI_ARREADY  (arready),
        .S_AXI_RDATA    (rdata),
        .S_AXI_RRESP    (rresp),
        .S_AXI_RVALID   (rvalid),
        .S_AXI_RREADY   (rready),
        .nonce,
        .nonce_ready,
        .license,
        .license_valid,
        .license_ready,
        .license_passed,
        .slh_sig_valid,
        .slh_sig_ready,
        .slh_sig_data,
        .slh_sig_keep,
        .slh_sig_last,
        .allowance,
        .enabled
    );

    task automatic axi_write(
        input logic [ADDR_WIDTH-1:0] address,
        input logic [31:0] value
    );
        int wait_cycles;
        begin
            wait_cycles = 0;
            @(negedge clk);
            awaddr  = address;
            awvalid = 1'b1;
            wdata   = value;
            wstrb   = 4'hf;
            wvalid  = 1'b1;
            bready  = 1'b1;
            while (!(awready && wready)) begin
                @(negedge clk);
                wait_cycles++;
                if (wait_cycles > 50) $fatal(1, "AXI write-ready timeout");
            end
            @(negedge clk);
            awvalid = 1'b0;
            wvalid  = 1'b0;
            while (!bvalid) begin
                @(negedge clk);
                wait_cycles++;
                if (wait_cycles > 100) $fatal(1, "AXI write-response timeout");
            end
            if (bresp != 2'b00) $fatal(1, "AXI write response error");
            @(negedge clk);
            bready = 1'b0;
        end
    endtask

    task automatic axi_read(
        input  logic [ADDR_WIDTH-1:0] address,
        output logic [31:0] value
    );
        int wait_cycles;
        begin
            wait_cycles = 0;
            @(negedge clk);
            araddr  = address;
            arvalid = 1'b1;
            rready  = 1'b1;
            while (!arready) begin
                @(negedge clk);
                wait_cycles++;
                if (wait_cycles > 50) $fatal(1, "AXI read-ready timeout");
            end
            @(negedge clk);
            arvalid = 1'b0;
            while (!rvalid) begin
                @(negedge clk);
                wait_cycles++;
                if (wait_cycles > 100) $fatal(1, "AXI read-data timeout");
            end
            value = rdata;
            if (rresp != 2'b00) $fatal(1, "AXI read response error");
            @(negedge clk);
            rready = 1'b0;
        end
    endtask

    logic [31:0] read_value;

    initial begin
        awaddr         = '0;
        awprot         = '0;
        awvalid        = 1'b0;
        wdata          = '0;
        wstrb          = '0;
        wvalid         = 1'b0;
        bready         = 1'b0;
        araddr         = '0;
        arprot         = '0;
        arvalid        = 1'b0;
        rready         = 1'b0;
        nonce          = 256'h0123456789abcdef;
        nonce_ready    = 1'b1;
        license_ready  = 1'b0;
        license_passed = 1'b0;
        slh_sig_ready  = 1'b0;
        allowance      = 64'h01234567_89abcdef;
        enabled        = 1'b0;
        tests_passed   = 0;

        wait (!rst_n);
        wait (rst_n);
        repeat (3) @(negedge clk);

        axi_read(REG_STATUS, read_value);
        if (read_value[9:8] != 2'd2 || !read_value[2]
            || !read_value[0]) begin
            $fatal(1, "initial AXI status mismatch: %08x", read_value);
        end
        tests_passed++;
        $display("PASS [PYNQ SLH status exposes mode and empty FIFO]");

        axi_write(REG_SIG_LO, 32'h44332211);
        axi_write(REG_SIG_HI, 32'h88776655);
        axi_write(REG_SIG_PUSH, 32'h00000001);
        if (!slh_sig_valid || slh_sig_data != 64'h88776655_44332211
            || slh_sig_keep != 8'hff || slh_sig_last) begin
            $fatal(1, "SLH FIFO word/lane mapping mismatch");
        end
        tests_passed++;
        $display("PASS [PYNQ SLH 64-bit lane mapping]");

        axi_write(REG_SIG_LO, 32'haaaaaaaa);
        axi_write(REG_SIG_HI, 32'hbbbbbbbb);
        axi_write(REG_SIG_PUSH, 32'h00000001);
        axi_read(REG_STATUS, read_value);
        if (!read_value[5] || slh_sig_data != 64'h88776655_44332211) begin
            $fatal(1, "full FIFO push did not fail closed");
        end
        tests_passed++;
        $display("PASS [PYNQ SLH FIFO overflow is sticky and non-destructive]");

        @(negedge clk);
        slh_sig_ready = 1'b1;
        @(negedge clk);
        slh_sig_ready = 1'b0;
        axi_read(REG_SIG_COUNT, read_value);
        if (slh_sig_valid || read_value != 32'd1) begin
            $fatal(1, "SLH FIFO transfer count mismatch");
        end

        axi_write(REG_CONTROL, 32'h00000002);
        axi_read(REG_STATUS, read_value);
        if (read_value[5:3] != 3'b000) begin
            $fatal(1, "sticky status clear failed: %08x", read_value);
        end
        axi_read(REG_SIG_COUNT, read_value);
        if (read_value != 0) $fatal(1, "signature count clear failed");
        tests_passed++;
        $display("PASS [PYNQ SLH sticky status and count clear]");

        axi_write(REG_CONTROL, 32'h00000001);
        if (!license_valid) $fatal(1, "license start was not held");
        axi_write(REG_SIG_LO, 32'h04030201);
        axi_write(REG_SIG_HI, 32'h08070605);
        axi_write(REG_SIG_PUSH, 32'h00000003);
        if (!slh_sig_valid || !slh_sig_last
            || slh_sig_data != 64'h08070605_04030201) begin
            $fatal(1, "final signature word mismatch");
        end
        @(negedge clk);
        slh_sig_ready = 1'b1;
        @(negedge clk);
        slh_sig_ready = 1'b0;
        license_passed = 1'b1;
        license_ready  = 1'b1;
        @(negedge clk);
        license_passed = 1'b0;
        license_ready  = 1'b0;

        axi_read(REG_STATUS, read_value);
        if (license_valid || !read_value[4] || !read_value[3]
            || read_value[5]) begin
            $fatal(1, "sticky completion/pass status mismatch: %08x",
                read_value);
        end
        tests_passed++;
        $display("PASS [PYNQ SLH start/completion/pass protocol]");

        axi_read(REG_ALLOW_LO, read_value);
        if (read_value != 32'h89abcdef) $fatal(1, "allowance low mismatch");
        axi_read(REG_ALLOW_HI, read_value);
        if (read_value != 32'h01234567) $fatal(1, "allowance high mismatch");
        tests_passed++;
        $display("PASS [PYNQ allowance readback]");

        if (tests_passed != 6) begin
            $fatal(1, "unexpected test count: %0d", tests_passed);
        end
        $display("All %0d PYNQ SLH AXI register tests passed.", tests_passed);
        $finish;
    end

    initial begin
        #100_000;
        $fatal(1, "timeout");
    end

endmodule
