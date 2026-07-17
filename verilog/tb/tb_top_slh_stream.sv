module tb (
    input logic clk,
    input logic rst_n
);

    import arith_pkg::*;
    import slh_dsa_pkg::*;

    localparam string VECTOR_DIR =
        "vectors/slh_dsa_sha2_128s_offswitch_zero_nonce";
    localparam int unsigned SIGNATURE_WORDS = SIG_BYTES / STREAM_BYTES;
    localparam logic [127:0] TEST_PK_SEED =
        128'h140a60345050c8ad34dcb27afba07d2c;
    localparam logic [127:0] TEST_PK_ROOT =
        128'h5667682788f82ca47b48902c993c9d6a;

    logic                         license_valid;
    logic                         license_ready;
    logic                         license_passed;
    logic                         slh_sig_valid;
    logic                         slh_sig_ready;
    logic [STREAM_BITS-1:0]       slh_sig_data;
    logic [STREAM_BYTES-1:0]      slh_sig_keep;
    logic                         slh_sig_last;
    logic                         workload_valid;
    logic [7:0]                   workload_a;
    logic [7:0]                   workload_b;
    logic [WIDTH-1:0]             trng_seed;
    logic                         trng_load_seed;
    logic [WIDTH-1:0]             nonce;
    logic                         nonce_ready;
    logic [7:0]                   workload_result;
    logic                         result_valid;
    logic [63:0]                  allowance;
    logic                         enabled;
    logic [STREAM_BITS-1:0]       signature_words[0:SIGNATURE_WORDS-1];
    int                           tests_passed;

    security_block #(
        .CRYPTO_TYPE        (2),
        .NUM_SIGNERS        (1),
        .SLH_STREAM_INPUT   (1'b1),
        .SLH_PK_SEED        (TEST_PK_SEED),
        .SLH_PK_ROOT        (TEST_PK_ROOT),
        .SLH_DEVICE_ID      (OFFSWITCH_DEFAULT_DEVICE_ID),
        .SLH_POLICY_EPOCH   (64'd1),
        .ALLOWANCE_INCREMENT(64'd1_000_000_000)
    ) dut (
        .clk,
        .rst_n,
        .license_valid,
        .license_ready,
        .license         (1'b0),
        .license_passed,
        .slh_sig_valid,
        .slh_sig_ready,
        .slh_sig_data,
        .slh_sig_keep,
        .slh_sig_last,
        .workload_valid,
        .workload_a,
        .workload_b,
        .trng_seed,
        .trng_load_seed,
        .nonce,
        .nonce_ready,
        .workload_result,
        .result_valid,
        .allowance,
        .enabled
    );

    task automatic send_signature(input logic corrupt_signature);
        logic [STREAM_BITS-1:0] next_word;
        int word_index;
        begin
            for (word_index = 0; word_index < SIGNATURE_WORDS;
                 word_index++) begin
                while (!slh_sig_ready) @(negedge clk);
                next_word = signature_words[word_index];
                if (corrupt_signature && (word_index == 2)) begin
                    next_word[0] = ~next_word[0];
                end
                slh_sig_data  = next_word;
                slh_sig_keep  = '1;
                slh_sig_last  = (word_index == SIGNATURE_WORDS - 1);
                slh_sig_valid = 1'b1;
                @(negedge clk);
                slh_sig_valid = 1'b0;
                slh_sig_last  = 1'b0;
            end
        end
    endtask

    task automatic submit_stream(
        input logic corrupt_signature,
        input logic expected_pass,
        input string label_text
    );
        int wait_cycles;
        begin
            wait_cycles = 0;
            while (!nonce_ready) begin
                @(negedge clk);
                wait_cycles++;
                if (wait_cycles > 1_000) begin
                    $fatal(1, "%s nonce timeout", label_text);
                end
            end

            @(negedge clk);
            license_valid = 1'b1;
            fork
                send_signature(corrupt_signature);
                begin
                    while (!license_ready) begin
                        @(negedge clk);
                        wait_cycles++;
                        if (wait_cycles > 50_000_000) begin
                            $fatal(1, "%s verification timeout", label_text);
                        end
                    end
                end
            join

            if (license_passed !== expected_pass) begin
                $fatal(1, "%s result mismatch: got=%0b expected=%0b",
                    label_text, license_passed, expected_pass);
            end
            @(negedge clk);
            license_valid = 1'b0;
            @(posedge clk);
            tests_passed++;
            $display("PASS [%s] passed=%0b", label_text, expected_pass);
        end
    endtask

    initial begin
        license_valid  = 1'b0;
        slh_sig_valid  = 1'b0;
        slh_sig_data   = '0;
        slh_sig_keep   = '0;
        slh_sig_last   = 1'b0;
        workload_valid = 1'b0;
        workload_a     = '0;
        workload_b     = '0;
        trng_seed      = '0;
        trng_load_seed = 1'b0;
        tests_passed   = 0;

        $readmemh({VECTOR_DIR, "/signature_words64.hex"},
            signature_words);

        wait (rst_n == 1'b0);
        wait (rst_n == 1'b1);
        @(negedge clk);
        trng_seed      = '0;
        trng_load_seed = 1'b1;
        @(negedge clk);
        trng_load_seed = 1'b0;

        while (!nonce_ready) @(negedge clk);
        if (nonce !== '0 || allowance !== 0 || enabled) begin
            $fatal(1, "stream-mode initial state mismatch");
        end
        tests_passed++;
        $display("PASS [SLH stream initial zero nonce and disabled state]");

        submit_stream(1'b1, 1'b0, "tampered streamed SLH license");
        if (allowance !== 0 || enabled) begin
            $fatal(1, "tampered streamed license changed allowance");
        end

        submit_stream(1'b0, 1'b1, "valid streamed SLH license");
        if (allowance == 0 || !enabled) begin
            $fatal(1, "valid streamed license did not enable workload");
        end

        @(negedge clk);
        workload_a     = 8'd40;
        workload_b     = 8'd2;
        workload_valid = 1'b1;
        @(negedge clk);
        workload_valid = 1'b0;
        if (!result_valid || (workload_result != 8'd42)) begin
            $fatal(1, "stream-enabled workload mismatch");
        end
        tests_passed++;
        $display("PASS [stream-enabled workload result=42]");

        if (tests_passed != 4) begin
            $fatal(1, "unexpected pass count: %0d", tests_passed);
        end
        $display("All %0d streamed SLH-DSA Off-Switch tests passed.",
            tests_passed);
        $finish;
    end

    initial begin
        #250_000_000;
        $fatal(1, "timeout");
    end

endmodule
