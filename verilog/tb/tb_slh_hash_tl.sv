module tb (
    input logic clk,
    input logic rst_n
);

    import slh_dsa_pkg::*;

    import "DPI-C" function void dpi_sha256(
        input byte unsigned data[768], input int byte_len,
        output bit [255:0] digest
    );

    localparam logic [127:0] PK_SEED =
        128'h000102030405060708090a0b0c0d0e0f;

    logic                         start;
    logic [127:0]                 pk_seed;
    logic [175:0]                 adrs_c;
    logic [5:0]                   element_count;
    logic                         element_valid;
    logic [MAX_NODE_BITS-1:0]     element;
    logic                         element_ready;
    logic                         busy;
    logic                         done;
    logic                         error;
    logic [MAX_NODE_BITS-1:0]     result;

    byte unsigned                 sha_buf[768];
    logic [127:0]                 test_elements[0:WOTS_LEN-1];
    int                           tests_passed;

    slh_hash_tl dut (
        .clk,
        .rst_n,
        .start,
        .pk_seed,
        .adrs_c,
        .element_count,
        .element_valid,
        .element,
        .element_ready,
        .busy,
        .done,
        .error,
        .result
    );

    function automatic slh_adrs_t make_adrs(
        input slh_adrs_type_e adrs_type
    );
        slh_adrs_t value;
        begin
            value.layer      = 32'h00000006;
            value.tree       = 96'h00000000_01234567_89abcdef;
            value.type_field = {24'b0, adrs_type};
            value.key_pair   = 32'h10203040;
            value.word6      = 32'h50607080;
            value.word7      = 32'h90a0b0c0;
            return value;
        end
    endfunction

    function automatic logic [255:0] expected_digest(input int count);
        logic [255:0] digest;
        int index;
        int item;
        int item_byte;
        begin
            for (index = 0; index < 768; index++) begin
                sha_buf[index] = 0;
            end
            index = 0;
            for (item_byte = 0; item_byte < N_BYTES; item_byte++) begin
                sha_buf[index++] = PK_SEED[
                    8*N_BYTES - 1 - 8*item_byte -: 8];
            end
            while (index < 64) begin
                sha_buf[index++] = 0;
            end
            for (item_byte = 0; item_byte < 22; item_byte++) begin
                sha_buf[index++] = adrs_c[175 - 8*item_byte -: 8];
            end
            for (item = 0; item < count; item++) begin
                for (item_byte = 0; item_byte < N_BYTES; item_byte++) begin
                    sha_buf[index++] = test_elements[item][
                        8*N_BYTES - 1 - 8*item_byte -: 8];
                end
            end
            dpi_sha256(sha_buf, index, digest);
            return digest;
        end
    endfunction

    task automatic run_case(input int count, input string label_text);
        logic [255:0] expected;
        int item;
        begin
            while (busy) @(posedge clk);
            element_count = count[5:0];
            expected = expected_digest(count);

            @(negedge clk);
            start = 1'b1;
            @(negedge clk);
            start = 1'b0;

            for (item = 0; item < count; item++) begin
                if ((item % 5) == 2) begin
                    @(negedge clk);
                end
                while (!element_ready) @(negedge clk);
                element = {test_elements[item], 128'b0};
                element_valid = 1'b1;
                @(negedge clk);
                element_valid = 1'b0;
            end

            wait (done);
            if (error) begin
                $fatal(1, "%s unexpectedly reported an error", label_text);
            end
            if (result[255:128] !== expected[255:128]) begin
                $fatal(1, "%s mismatch: got=%032x expected=%032x",
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

    task automatic run_invalid_count;
        begin
            while (busy) @(posedge clk);
            @(negedge clk);
            element_count = 0;
            start = 1'b1;
            @(negedge clk);
            start = 1'b0;
            wait (done);
            if (!error || busy) begin
                $fatal(1, "zero element count was not rejected");
            end
            tests_passed++;
            $display("PASS [invalid zero element count]");
            @(posedge clk);
        end
    endtask

    initial begin
        start         = 1'b0;
        pk_seed       = PK_SEED;
        adrs_c        = '0;
        element_count = '0;
        element_valid = 1'b0;
        element       = '0;
        tests_passed  = 0;

        for (int item = 0; item < WOTS_LEN; item++) begin
            for (int item_byte = 0; item_byte < N_BYTES; item_byte++) begin
                test_elements[item][127 - 8*item_byte -: 8] =
                    8'(item * N_BYTES + item_byte);
            end
        end

        wait (rst_n == 1'b0);
        wait (rst_n == 1'b1);
        repeat (2) @(posedge clk);

        adrs_c = slh_compress_adrs(make_adrs(SlhAdrsForsRoots));
        if (adrs_c !== 176'h060123456789abcdef04102030405060708090a0b0c0) begin
            $fatal(1, "compressed ADRS mismatch: got=%044x", adrs_c);
        end
        tests_passed++;
        $display("PASS [compressed ADRS byte layout] adrs_c=%044x", adrs_c);

        run_case(1, "T_1 agrees with F construction");
        run_case(K, "T_14 FORS roots");

        adrs_c = slh_compress_adrs(make_adrs(SlhAdrsWotsPk));
        run_case(WOTS_LEN, "T_35 WOTS+ endpoints");
        run_invalid_count();

        if (tests_passed != 5) begin
            $fatal(1, "unexpected pass count: %0d", tests_passed);
        end
        $display("All %0d SLH-DSA ADRS/T_l tests passed.", tests_passed);
        $finish;
    end

    initial begin
        #8_000_000;
        $fatal(1, "timeout");
    end

endmodule
