module tb (
    input logic clk,
    input logic rst_n
);

    import slh_dsa_pkg::*;

    logic                     start;
    logic                     start_ready;
    logic                     busy;
    logic                     sig_valid;
    logic                     sig_ready;
    logic [STREAM_BITS-1:0]   sig_data;
    logic [STREAM_BYTES-1:0]  sig_keep;
    logic                     sig_last;
    logic                     element_valid;
    logic                     element_ready;
    logic [MAX_NODE_BITS-1:0] element_data;
    slh_region_e              element_region;
    logic [15:0]              element_index;
    logic                     element_last;
    logic                     done;
    logic                     error;
    slh_error_e               error_code;
    logic [31:0]              byte_count;

    int unsigned tests_passed;

    slh_sig_parser dut (
        .clk,
        .rst_n,
        .start,
        .start_ready,
        .busy,
        .sig_valid,
        .sig_ready,
        .sig_data,
        .sig_keep,
        .sig_last,
        .element_valid,
        .element_ready,
        .element_data,
        .element_region,
        .element_index,
        .element_last,
        .done,
        .error,
        .error_code,
        .byte_count
    );

    function automatic logic [63:0] signature_word(input int unsigned word_index);
        logic [63:0] result;
        int unsigned lane;
        begin
            result = '0;
            for (lane = 0; lane < STREAM_BYTES; lane++) begin
                result[8 * lane +: 8] =
                    byte'(word_index * STREAM_BYTES + lane);
            end
            return result;
        end
    endfunction

    function automatic slh_region_e expected_region(input int unsigned index);
        if (index == 0) begin
            return SlhRegionR;
        end
        if (index <= FORS_ELEMENTS) begin
            return SlhRegionFors;
        end
        return SlhRegionHt;
    endfunction

    task automatic pulse_start;
        begin
            while (!start_ready) @(posedge clk);
            @(negedge clk);
            start = 1'b1;
            @(negedge clk);
            start = 1'b0;
        end
    endtask

    task automatic drive_word(
        input logic [63:0] data,
        input logic [7:0]  keep,
        input logic        last
    );
        begin
            @(negedge clk);
            sig_valid = 1'b1;
            sig_data  = data;
            sig_keep  = keep;
            sig_last  = last;
            do begin
                @(posedge clk);
            end while (!sig_ready);
            @(negedge clk);
            sig_valid = 1'b0;
            sig_data  = '0;
            sig_keep  = '0;
            sig_last  = 1'b0;
        end
    endtask

    task automatic check_element(input int unsigned expected_index);
        int unsigned byte_index;
        logic [7:0] expected_byte;
        begin
            if (element_index !== expected_index[15:0]) begin
                $fatal(1, "element index mismatch: got %0d expected %0d",
                    element_index, expected_index);
            end
            if (element_region !== expected_region(expected_index)) begin
                $fatal(1, "element region mismatch at index %0d", expected_index);
            end
            if (element_last !== (expected_index == SIG_ELEMENTS - 1)) begin
                $fatal(1, "element_last mismatch at index %0d", expected_index);
            end
            for (byte_index = 0; byte_index < N_BYTES; byte_index++) begin
                expected_byte =
                    byte'(expected_index * N_BYTES + byte_index);
                if (element_data[MAX_NODE_BITS - 1 - 8 * byte_index -: 8]
                        !== expected_byte) begin
                    $fatal(1,
                        "element byte mismatch: element=%0d byte=%0d got=%02x expected=%02x",
                        expected_index, byte_index,
                        element_data[MAX_NODE_BITS - 1 - 8 * byte_index -: 8],
                        expected_byte);
                end
            end
            if (element_data[MAX_NODE_BITS - 8 * N_BYTES - 1:0] !== '0) begin
                $fatal(1, "unused low node bits are not zero at element %0d",
                    expected_index);
            end
        end
    endtask

    task automatic test_valid_stream_with_backpressure;
        int unsigned word_index;
        int unsigned accepted_elements;
        int unsigned ready_pattern;
        begin
            accepted_elements = 0;
            ready_pattern     = 0;
            pulse_start();

            fork
                begin : producer
                    for (word_index = 0;
                         word_index < SIG_BYTES / STREAM_BYTES;
                         word_index++) begin
                        drive_word(
                            signature_word(word_index),
                            8'hff,
                            word_index == SIG_BYTES / STREAM_BYTES - 1
                        );
                    end
                end

                begin : consumer
                    while (accepted_elements < SIG_ELEMENTS) begin
                        @(negedge clk);
                        element_ready = (ready_pattern % 4 != 0);
                        ready_pattern++;
                        @(posedge clk);
                        if (element_valid && element_ready) begin
                            check_element(accepted_elements);
                            accepted_elements++;
                        end
                    end
                    @(negedge clk);
                    element_ready = 1'b0;
                end
            join

            wait (done || error);
            if (error || !done) begin
                $fatal(1, "valid stream failed with error code %0d", error_code);
            end
            if (byte_count != SIG_BYTES) begin
                $fatal(1, "valid stream byte count mismatch: %0d", byte_count);
            end
            tests_passed++;
            $display("PASS [valid stream with element backpressure]");
            @(posedge clk);
        end
    endtask

    task automatic expect_error(input slh_error_e expected_error);
        begin
            wait (error);
            if (error_code !== expected_error) begin
                $fatal(1, "error mismatch: got %0d expected %0d",
                    error_code, expected_error);
            end
            tests_passed++;
            @(posedge clk);
        end
    endtask

    task automatic test_early_last;
        begin
            pulse_start();
            drive_word(signature_word(0), 8'hff, 1'b1);
            expect_error(SlhErrEarlyLast);
            $display("PASS [early last rejected]");
        end
    endtask

    task automatic test_bad_keep;
        begin
            pulse_start();
            drive_word(signature_word(0), 8'hfe, 1'b0);
            expect_error(SlhErrBadKeep);
            $display("PASS [partial word rejected]");
        end
    endtask

    task automatic test_missing_last;
        int unsigned word_index;
        begin
            element_ready = 1'b1;
            pulse_start();
            for (word_index = 0;
                 word_index < SIG_BYTES / STREAM_BYTES;
                 word_index++) begin
                drive_word(signature_word(word_index), 8'hff, 1'b0);
            end
            expect_error(SlhErrMissingLast);
            element_ready = 1'b0;
            $display("PASS [missing last rejected]");
        end
    endtask

    initial begin
        start         = 1'b0;
        sig_valid     = 1'b0;
        sig_data      = '0;
        sig_keep      = '0;
        sig_last      = 1'b0;
        element_ready = 1'b0;
        tests_passed  = 0;

        // sim_main.cpp drives reset for every repository testbench.
        wait (rst_n == 1'b0);
        wait (rst_n == 1'b1);
        repeat (2) @(posedge clk);

        if (!start_ready || busy) begin
            $fatal(1, "parser did not return to idle after reset");
        end

        test_valid_stream_with_backpressure();
        test_early_last();
        test_bad_keep();
        test_missing_last();

        if (tests_passed != 4) begin
            $fatal(1, "unexpected pass count: %0d", tests_passed);
        end
        $display("All %0d SLH-DSA signature parser tests passed.", tests_passed);
        $finish;
    end

    initial begin
        #2_000_000;
        $fatal(1, "timeout");
    end

endmodule
