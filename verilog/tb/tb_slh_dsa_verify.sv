module tb (
    input logic clk,
    input logic rst_n
);

    import slh_dsa_pkg::*;

    localparam string VECTOR_DIR = "vectors/slh_dsa_sha2_128s_smoke";
    localparam int unsigned SIGNATURE_WORDS = SIG_BYTES / STREAM_BYTES;

    logic                         force_reset;
    wire                          dut_rst_n = rst_n && !force_reset;
    logic                         start;
    logic                         start_ready;
    logic [127:0]                 pk_seed;
    logic [127:0]                 pk_root;
    logic [72*8-1:0]              application_message;
    logic                         sig_valid;
    logic                         sig_ready;
    logic [63:0]                  sig_data;
    logic [7:0]                   sig_keep;
    logic                         sig_last;
    logic                         busy;
    logic                         done;
    logic                         error;
    slh_error_e                   error_code;
    logic                         valid_signature;
    logic [MAX_NODE_BITS-1:0]     computed_root;

    logic [127:0]                 public_key_elements[0:1];
    logic [72*8-1:0]              message_value[0:0];
    logic [63:0]                  signature_words[0:SIGNATURE_WORDS-1];
    int                           tests_passed;

    slh_dsa_verify dut (
        .clk,
        .rst_n(dut_rst_n),
        .start,
        .start_ready,
        .pk_seed,
        .pk_root,
        .application_message,
        .sig_valid,
        .sig_ready,
        .sig_data,
        .sig_keep,
        .sig_last,
        .busy,
        .done,
        .error,
        .error_code,
        .valid_signature,
        .computed_root
    );

    task automatic begin_verification(
        input logic corrupt_message,
        input logic corrupt_public_key
    );
        begin
            while (!start_ready) @(negedge clk);
            application_message = message_value[0];
            pk_root = public_key_elements[1];
            if (corrupt_message) begin
                application_message[72*8-1] =
                    ~application_message[72*8-1];
            end
            if (corrupt_public_key) begin
                pk_root[127] = ~pk_root[127];
            end
            @(negedge clk);
            start = 1'b1;
            @(negedge clk);
            start = 1'b0;
        end
    endtask

    task automatic send_signature(input logic corrupt_signature);
        logic [63:0] next_word;
        int word_index;
        begin
            for (word_index = 0; word_index < SIGNATURE_WORDS;
                 word_index++) begin
                if ((word_index % 17) == 8) begin
                    @(negedge clk);
                end
                while (!sig_ready) @(negedge clk);
                next_word = signature_words[word_index];
                if (corrupt_signature && (word_index == 2)) begin
                    next_word[0] = ~next_word[0];
                end
                sig_data  = next_word;
                sig_keep  = '1;
                sig_last  = (word_index == SIGNATURE_WORDS - 1);
                sig_valid = 1'b1;
                @(negedge clk);
                sig_valid = 1'b0;
                sig_last  = 1'b0;
            end
        end
    endtask

    task automatic run_crypto_case(
        input logic corrupt_signature,
        input logic corrupt_message,
        input logic corrupt_public_key,
        input logic expect_valid,
        input string label_text
    );
        begin
            begin_verification(corrupt_message, corrupt_public_key);
            fork
                send_signature(corrupt_signature);
                wait (done);
            join

            if (error) begin
                $fatal(1, "%s unexpectedly reported framing error %0d",
                    label_text, error_code);
            end
            if (valid_signature !== expect_valid) begin
                $fatal(1, "%s validity mismatch: got=%0b expected=%0b",
                    label_text, valid_signature, expect_valid);
            end
            if (computed_root[127:0] !== '0) begin
                $fatal(1, "%s computed root is not top-aligned", label_text);
            end
            if (expect_valid
                && (computed_root[255:128] !== public_key_elements[1])) begin
                $fatal(1, "%s root mismatch: got=%032x expected=%032x",
                    label_text, computed_root[255:128], public_key_elements[1]);
            end
            tests_passed++;
            $display("PASS [%s] valid=%0b root=%032x",
                label_text, valid_signature, computed_root[255:128]);
            @(posedge clk);
        end
    endtask

    task automatic run_bad_keep_case;
        int word_index;
        begin
            begin_verification(1'b0, 1'b0);
            for (word_index = 0; word_index <= 4; word_index++) begin
                while (!sig_ready) @(negedge clk);
                sig_data  = signature_words[word_index];
                sig_keep  = (word_index == 4) ? 8'hfe : 8'hff;
                sig_last  = 1'b0;
                sig_valid = 1'b1;
                @(negedge clk);
                sig_valid = 1'b0;
            end
            wait (done);
            if (!error || valid_signature
                || (error_code != SlhErrBadKeep)) begin
                $fatal(1,
                    "bad keep result mismatch: error=%0b valid=%0b code=%0d",
                    error, valid_signature, error_code);
            end
            tests_passed++;
            $display("PASS [malformed signature keep] error_code=%0d",
                error_code);
            @(posedge clk);
        end
    endtask

    task automatic run_reset_case;
        int word_index;
        begin
            begin_verification(1'b0, 1'b0);
            for (word_index = 0; word_index < 4; word_index++) begin
                while (!sig_ready) @(negedge clk);
                sig_data  = signature_words[word_index];
                sig_keep  = '1;
                sig_last  = 1'b0;
                sig_valid = 1'b1;
                @(negedge clk);
                sig_valid = 1'b0;
            end
            @(negedge clk);
            force_reset = 1'b1;
            repeat (2) @(negedge clk);
            force_reset = 1'b0;
            repeat (2) @(posedge clk);
            if (busy || !start_ready || valid_signature || error) begin
                $fatal(1,
                    "reset recovery mismatch: busy=%0b ready=%0b valid=%0b error=%0b",
                    busy, start_ready, valid_signature, error);
            end
            tests_passed++;
            $display("PASS [reset clears partial SLH-DSA transaction]");
            @(posedge clk);
        end
    endtask

    initial begin
        force_reset         = 1'b0;
        start               = 1'b0;
        pk_seed             = '0;
        pk_root             = '0;
        application_message = '0;
        sig_valid           = 1'b0;
        sig_data            = '0;
        sig_keep            = '0;
        sig_last            = 1'b0;
        tests_passed        = 0;

        $readmemh({VECTOR_DIR, "/public_key_elements128.hex"},
            public_key_elements);
        $readmemh({VECTOR_DIR, "/message.hex"}, message_value);
        $readmemh({VECTOR_DIR, "/signature_words64.hex"}, signature_words);
        pk_seed = public_key_elements[0];
        pk_root = public_key_elements[1];

        wait (rst_n == 1'b0);
        wait (rst_n == 1'b1);
        repeat (2) @(posedge clk);

        run_crypto_case(1'b0, 1'b0, 1'b0, 1'b1,
            "real liboqs SLH-DSA signature");
        run_crypto_case(1'b1, 1'b0, 1'b0, 1'b0,
            "tampered SLH-DSA signature");
        run_crypto_case(1'b0, 1'b1, 1'b0, 1'b0,
            "tampered Off-Switch message");
        run_crypto_case(1'b0, 1'b0, 1'b1, 1'b0,
            "wrong SLH-DSA public key");
        run_bad_keep_case();
        run_reset_case();

        if (tests_passed != 6) begin
            $fatal(1, "unexpected pass count: %0d", tests_passed);
        end
        $display("All %0d end-to-end SLH-DSA verification tests passed.",
            tests_passed);
        $finish;
    end

    initial begin
        #190_000_000;
        $fatal(1, "timeout");
    end

endmodule
