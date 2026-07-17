module tb (
    input logic clk,
    input logic rst_n
);

    import slh_dsa_pkg::*;

    import "DPI-C" function void dpi_sha256(
        input byte unsigned data[256], input int byte_len,
        output bit [255:0] digest
    );

    localparam logic [127:0] R_BASE =
        128'h00112233445566778899aabbccddeeff;
    localparam logic [127:0] PK_SEED =
        128'h102132435465768798a9bacbdcedfe0f;
    localparam logic [127:0] PK_ROOT =
        128'hffeeddccbbaa99887766554433221100;
    localparam logic [127:0] DOMAIN =
        128'h4f46465357495443482d534c482d5631;
    localparam logic [127:0] DEVICE_ID =
        128'h00112233445566778899aabbccddeeff;
    localparam logic [255:0] NONCE =
        256'h54ff53a510e527f9b05688c1f83d9ab5be0cd19a48b2de3a4b6d0c7fbc69a79a;
    localparam logic [63:0] POLICY_EPOCH = 64'd1;
    localparam logic [575:0] APP_MESSAGE =
        {DOMAIN, DEVICE_ID, NONCE, POLICY_EPOCH};

    logic                     start;
    logic [127:0]             randomizer_r;
    logic [127:0]             pk_seed;
    logic [127:0]             pk_root;
    logic [575:0]             application_message;
    logic                     busy;
    logic                     done;
    logic [255:0]             digest;
    logic [167:0]             fors_message;
    logic [55:0]              tree_index;
    logic [15:0]              leaf_index;

    byte unsigned             sha_buf[256];
    int                       tests_passed;

    slh_hmsg dut (
        .clk,
        .rst_n,
        .start,
        .randomizer_r,
        .pk_seed,
        .pk_root,
        .application_message,
        .busy,
        .done,
        .digest,
        .fors_message,
        .tree_index,
        .leaf_index
    );

    function automatic logic [255:0] expected_hmsg(input logic [127:0] r_value);
        logic [255:0] inner_digest;
        logic [255:0] outer_digest;
        int index;
        int byte_index;
        begin
            for (index = 0; index < 256; index++) sha_buf[index] = 0;
            index = 0;
            for (byte_index = 0; byte_index < 16; byte_index++)
                sha_buf[index++] = r_value[127 - 8*byte_index -: 8];
            for (byte_index = 0; byte_index < 16; byte_index++)
                sha_buf[index++] = PK_SEED[127 - 8*byte_index -: 8];
            for (byte_index = 0; byte_index < 16; byte_index++)
                sha_buf[index++] = PK_ROOT[127 - 8*byte_index -: 8];
            sha_buf[index++] = 8'h00;
            sha_buf[index++] = 8'h00;
            for (byte_index = 0; byte_index < 72; byte_index++)
                sha_buf[index++] = APP_MESSAGE[575 - 8*byte_index -: 8];
            dpi_sha256(sha_buf, index, inner_digest);

            for (index = 0; index < 256; index++) sha_buf[index] = 0;
            index = 0;
            for (byte_index = 0; byte_index < 16; byte_index++)
                sha_buf[index++] = r_value[127 - 8*byte_index -: 8];
            for (byte_index = 0; byte_index < 16; byte_index++)
                sha_buf[index++] = PK_SEED[127 - 8*byte_index -: 8];
            for (byte_index = 0; byte_index < 32; byte_index++)
                sha_buf[index++] = inner_digest[255 - 8*byte_index -: 8];
            repeat (4) sha_buf[index++] = 8'h00;
            dpi_sha256(sha_buf, index, outer_digest);
            return outer_digest;
        end
    endfunction

    task automatic run_case(input logic [127:0] r_value, input string label_text);
        logic [255:0] expected;
        begin
            expected = expected_hmsg(r_value);
            while (busy) @(posedge clk);
            @(negedge clk);
            randomizer_r = r_value;
            start = 1'b1;
            @(negedge clk);
            start = 1'b0;

            wait (done);
            if (digest[255:16] !== expected[255:16] || digest[15:0] !== 0)
                $fatal(1, "%s H_msg mismatch", label_text);
            if (fors_message !== expected[255:88])
                $fatal(1, "%s FORS digest split mismatch", label_text);
            if (tree_index !== (expected[87:32] & 56'h3fffffffffffff))
                $fatal(1, "%s tree index split mismatch", label_text);
            if (leaf_index !== (expected[31:16] & 16'h01ff))
                $fatal(1, "%s leaf index split mismatch", label_text);

            tests_passed++;
            $display("PASS [%s] hmsg=%060x tree=%014x leaf=%03x",
                label_text, digest[255:16], tree_index, leaf_index);
            @(posedge clk);
        end
    endtask

    initial begin
        start               = 1'b0;
        randomizer_r        = R_BASE;
        pk_seed             = PK_SEED;
        pk_root             = PK_ROOT;
        application_message = APP_MESSAGE;
        tests_passed        = 0;

        wait (rst_n == 1'b0);
        wait (rst_n == 1'b1);
        repeat (2) @(posedge clk);

        run_case(R_BASE, "Pure empty-context H_msg");
        run_case(R_BASE ^ 128'd1, "changed randomizer");

        if (tests_passed != 2)
            $fatal(1, "unexpected pass count: %0d", tests_passed);
        $display("All %0d SLH-DSA H_msg tests passed.", tests_passed);
        $finish;
    end

    initial begin
        #2_000_000;
        $fatal(1, "timeout");
    end

endmodule
