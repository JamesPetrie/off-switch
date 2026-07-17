module tb (
    input logic clk,
    input logic rst_n
);

    import slh_dsa_pkg::*;

    localparam string VECTOR_DIR = "vectors/slh_dsa_sha2_128s_smoke";
    localparam int unsigned HT_FIRST_ELEMENT = 1 + FORS_ELEMENTS;

    logic                         start;
    logic [127:0]                 pk_seed;
    logic [127:0]                 message;
    logic [2:0]                   layer;
    logic [HMSG_TREE_BITS-1:0]    tree_index;
    logic [HMSG_LEAF_BITS-1:0]    leaf_index;
    logic                         element_valid;
    logic [MAX_NODE_BITS-1:0]     element;
    logic                         element_ready;
    logic                         busy;
    logic                         done;
    logic                         error;
    logic [MAX_NODE_BITS-1:0]     public_key;

    logic [127:0]                 public_key_elements[0:1];
    logic [127:0]                 signature_elements[0:SIG_ELEMENTS-1];
    logic [M_BYTES*8-1:0]         hmsg_value[0:0];
    logic [127:0]                 fors_public_key[0:0];
    logic [127:0]                 expected_wots_key[0:0];
    int                           tests_passed;

    slh_wots_pk_from_sig dut (
        .clk,
        .rst_n,
        .start,
        .pk_seed,
        .message,
        .layer,
        .tree_index,
        .leaf_index,
        .element_valid,
        .element,
        .element_ready,
        .busy,
        .done,
        .error,
        .public_key
    );

    task automatic run_case(
        input logic corrupt_first_chain,
        input logic expect_match,
        input string label_text
    );
        logic [127:0] next_element;
        int element_index;
        begin
            while (busy) @(posedge clk);
            @(negedge clk);
            start = 1'b1;
            @(negedge clk);
            start = 1'b0;

            for (element_index = 0;
                 element_index < WOTS_LEN;
                 element_index++) begin
                if ((element_index % 7) == 3) begin
                    @(negedge clk);
                end
                while (!element_ready) @(negedge clk);
                next_element = signature_elements[
                    HT_FIRST_ELEMENT + element_index];
                if (corrupt_first_chain && (element_index == 0)) begin
                    next_element[127] = ~next_element[127];
                end
                element = {next_element, 128'b0};
                element_valid = 1'b1;
                @(negedge clk);
                element_valid = 1'b0;
            end

            wait (done);
            if (error) begin
                $fatal(1, "%s unexpectedly reported an error", label_text);
            end
            if (expect_match
                && (public_key[255:128] !== expected_wots_key[0])) begin
                $fatal(1, "%s mismatch: got=%032x expected=%032x",
                    label_text, public_key[255:128], expected_wots_key[0]);
            end
            if (!expect_match
                && (public_key[255:128] === expected_wots_key[0])) begin
                $fatal(1, "%s corruption did not change the WOTS+ key",
                    label_text);
            end
            if (public_key[127:0] !== '0) begin
                $fatal(1, "%s result is not top-aligned", label_text);
            end
            tests_passed++;
            $display("PASS [%s] WOTS_PK=%032x",
                label_text, public_key[255:128]);
            @(posedge clk);
        end
    endtask

    initial begin
        start         = 1'b0;
        pk_seed       = '0;
        message       = '0;
        layer         = '0;
        tree_index    = '0;
        leaf_index    = '0;
        element_valid = 1'b0;
        element       = '0;
        tests_passed  = 0;

        $readmemh({VECTOR_DIR, "/public_key_elements128.hex"},
            public_key_elements);
        $readmemh({VECTOR_DIR, "/signature_elements128.hex"},
            signature_elements);
        $readmemh({VECTOR_DIR, "/hmsg_digest.hex"}, hmsg_value);
        $readmemh({VECTOR_DIR, "/fors_public_key.hex"}, fors_public_key);
        $readmemh({VECTOR_DIR, "/ht_layer0_wots_public_key.hex"},
            expected_wots_key);

        pk_seed     = public_key_elements[0];
        message     = fors_public_key[0];
        layer       = 0;
        tree_index  = hmsg_value[0][69:16];
        leaf_index  = hmsg_value[0][8:0];

        wait (rst_n == 1'b0);
        wait (rst_n == 1'b1);
        repeat (2) @(posedge clk);

        run_case(1'b0, 1'b1, "real liboqs WOTS+ signature");
        run_case(1'b1, 1'b0, "corrupted WOTS+ chain value");

        if (tests_passed != 2) begin
            $fatal(1, "unexpected pass count: %0d", tests_passed);
        end
        $display("All %0d real-vector WOTS+ recovery tests passed.",
            tests_passed);
        $finish;
    end

    initial begin
        #250_000_000;
        $fatal(1, "timeout");
    end

endmodule
