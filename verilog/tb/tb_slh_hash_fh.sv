module tb (
    input logic clk,
    input logic rst_n
);

    import slh_dsa_pkg::*;

    import "DPI-C" function void dpi_sha256(
        input byte unsigned data[128], input int byte_len,
        output bit [255:0] digest
    );

    localparam logic [127:0] PK_SEED =
        128'h000102030405060708090a0b0c0d0e0f;
    localparam logic [175:0] ADRS_C =
        176'h101112131415161718191a1b1c1d1e1f202122232425;
    localparam logic [127:0] F_INPUT =
        128'h303132333435363738393a3b3c3d3e3f;
    localparam logic [255:0] H_INPUT =
        256'h404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f;

    logic                         start;
    logic                         use_h;
    logic [127:0]                 pk_seed;
    logic [175:0]                 adrs_c;
    logic [MAX_NODE_BITS-1:0]     message;
    logic                         busy;
    logic                         done;
    logic [MAX_NODE_BITS-1:0]     result;

    byte unsigned                 sha_buf[128];
    int                           tests_passed;

    slh_hash_fh dut (
        .clk,
        .rst_n,
        .start,
        .use_h,
        .pk_seed,
        .adrs_c,
        .message,
        .busy,
        .done,
        .result
    );

    function automatic logic [255:0] expected_digest(input logic select_h);
        logic [255:0] digest;
        int index;
        int byte_index;
        begin
            for (index = 0; index < 128; index++) begin
                sha_buf[index] = 0;
            end
            index = 0;

            for (byte_index = 0; byte_index < N_BYTES; byte_index++) begin
                sha_buf[index++] = PK_SEED[8*N_BYTES - 1 - 8*byte_index -: 8];
            end
            while (index < 64) begin
                sha_buf[index++] = 0;
            end
            for (byte_index = 0; byte_index < 22; byte_index++) begin
                sha_buf[index++] = ADRS_C[175 - 8*byte_index -: 8];
            end
            if (select_h) begin
                for (byte_index = 0; byte_index < 2*N_BYTES; byte_index++) begin
                    sha_buf[index++] = H_INPUT[255 - 8*byte_index -: 8];
                end
            end else begin
                for (byte_index = 0; byte_index < N_BYTES; byte_index++) begin
                    sha_buf[index++] = F_INPUT[127 - 8*byte_index -: 8];
                end
            end

            dpi_sha256(sha_buf, index, digest);
            return digest;
        end
    endfunction

    task automatic run_case(input logic select_h, input string label_text);
        logic [255:0] expected;
        begin
            expected = expected_digest(select_h);
            while (busy) @(posedge clk);
            @(negedge clk);
            use_h  = select_h;
            message = select_h ? H_INPUT : {F_INPUT, 128'b0};
            start  = 1'b1;
            @(negedge clk);
            start = 1'b0;

            wait (done);
            if (result[255:128] !== expected[255:128]) begin
                $fatal(1, "%s digest mismatch: got=%032x expected=%032x",
                    label_text, result[255:128], expected[255:128]);
            end
            if (result[127:0] !== '0) begin
                $fatal(1, "%s result is not top-aligned", label_text);
            end
            tests_passed++;
            $display("PASS [%s] result=%032x", label_text, result[255:128]);
            @(posedge clk);
        end
    endtask

    initial begin
        start        = 1'b0;
        use_h        = 1'b0;
        pk_seed      = PK_SEED;
        adrs_c       = ADRS_C;
        message      = '0;
        tests_passed = 0;

        wait (rst_n == 1'b0);
        wait (rst_n == 1'b1);
        repeat (2) @(posedge clk);

        run_case(1'b0, "F member function");
        run_case(1'b1, "H member function");
        run_case(1'b0, "F repeated transaction");

        if (tests_passed != 3) begin
            $fatal(1, "unexpected pass count: %0d", tests_passed);
        end
        $display("All %0d SLH-DSA F/H hash tests passed.", tests_passed);
        $finish;
    end

    initial begin
        #2_000_000;
        $fatal(1, "timeout");
    end

endmodule
