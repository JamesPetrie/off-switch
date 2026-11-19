// tb_slh.sv — SLH-DSA-SHA2-128s signature verification test
//
// hss_verify as an SLH-DSA verifier, against signatures made at run time by
// the slhdsa-c reference signer through DPI-C (tb_slh_sign_pkg). A genuine
// signature is accepted, with and without pauses in the beat stream; one with
// a single bit inverted, a wrong message or a wrong public key is rejected.

`include "tb_slh_sign_pkg.sv"

module tb (
    input logic clk,
    input logic rst_n
);
    import arith_pkg::*;
    import slh_pkg::*;
    import tb_slh_sign_pkg::*;

    // -------------------------------------------------------------------------
    // Test vector: arbitrary 256-bit message
    // -------------------------------------------------------------------------

    localparam logic [WIDTH-1:0] MESSAGE =
        256'had5b59dc22a7e2db48d5b495057128b500f9121983c2ee6a4508047fee48e0bb;

    // -------------------------------------------------------------------------
    // Tests
    // -------------------------------------------------------------------------

    // What is wrong with the licence
    typedef enum logic [2:0] {
        GENUINE,      // nothing
        BAD_BEAT,     // one bit of a signature beat is inverted
        BAD_MESSAGE,  // one bit of the message is inverted
        BAD_SEED,     // one bit of PK.seed is inverted
        BAD_ROOT,     // one bit of PK.root is inverted
        OTHER_KEY     // the public key is the other signer's
    } fault_e;

    typedef struct packed {
        logic        signer;  // whose signature is sent
        fault_e      fault;
        int unsigned beat;    // BAD_BEAT: the beat
        logic        pauses;  // the producer pauses between beats
    } test_t;

    localparam int unsigned NUM_TESTS = 19;
    localparam test_t TESTS [NUM_TESTS] = '{
        '{signer: 1'b0, fault: GENUINE,     beat: 0, pauses: 1'b0},
        '{signer: 1'b0, fault: GENUINE,     beat: 0, pauses: 1'b1},

        // One inverted bit in each part of the signature: the randomizer
        '{signer: 1'b0, fault: BAD_BEAT, beat: R_BEAT, pauses: 1'b0},
        // FORS: the first tree's secret value and the bottom of its auth
        // path, the top of the last tree's auth path
        '{signer: 1'b0, fault: BAD_BEAT, beat: FORS_BEAT,     pauses: 1'b0},
        '{signer: 1'b0, fault: BAD_BEAT, beat: FORS_BEAT + 1, pauses: 1'b0},
        '{signer: 1'b0, fault: BAD_BEAT, beat: HT_BEAT - 1,   pauses: 1'b0},
        // Bottom layer: the first chain, the last checksum chain and the
        // bottom of the auth path
        '{signer: 1'b0, fault: BAD_BEAT, beat: HT_BEAT,               pauses: 1'b0},
        '{signer: 1'b0, fault: BAD_BEAT, beat: HT_BEAT + SLH_LEN - 1, pauses: 1'b0},
        '{signer: 1'b0, fault: BAD_BEAT, beat: HT_BEAT + SLH_LEN,     pauses: 1'b0},
        // A chain of a middle layer, and the top of the top layer's auth path
        '{signer: 1'b0, fault: BAD_BEAT, beat: HT_BEAT + 3 * LAYER_BEATS + 17, pauses: 1'b0},
        '{signer: 1'b0, fault: BAD_BEAT, beat: LAST_BEAT,                      pauses: 1'b0},

        // A genuine one straight after a rejection
        '{signer: 1'b0, fault: GENUINE,     beat: 0, pauses: 1'b0},

        '{signer: 1'b0, fault: BAD_MESSAGE, beat: 0, pauses: 1'b0},
        '{signer: 1'b0, fault: BAD_SEED,    beat: 0, pauses: 1'b0},
        '{signer: 1'b0, fault: BAD_ROOT,    beat: 0, pauses: 1'b0},
        '{signer: 1'b0, fault: OTHER_KEY,   beat: 0, pauses: 1'b0},

        // The other signer
        '{signer: 1'b1, fault: GENUINE,     beat: 0, pauses: 1'b1},
        '{signer: 1'b1, fault: BAD_BEAT,    beat: LAST_BEAT - LAYER_BEATS + 1, pauses: 1'b0},
        '{signer: 1'b1, fault: OTHER_KEY,   beat: 0, pauses: 1'b0}
    };

    function automatic string describe(input test_t t);
        string what;
        case (t.fault)
            GENUINE:     if (t.pauses) what = "genuine, with pauses";
                         else          what = "genuine";
            BAD_BEAT:    what = $sformatf("bit inverted in beat %0d", t.beat);
            BAD_MESSAGE: what = "wrong message";
            BAD_SEED:    what = "wrong PK.seed";
            BAD_ROOT:    what = "wrong PK.root";
            OTHER_KEY:   what = "the other signer's key";
            default:     what = "?";
        endcase
        return $sformatf("signer %0d, %s", t.signer, what);
    endfunction

    int unsigned test_idx = 0;
    test_t       test;
    assign test = TESTS[test_idx];

    // The test's public key and message
    slh_key_t         key;
    logic [WIDTH-1:0] message;

    always_comb begin
        key     = SLH_KEYS[(test.fault == OTHER_KEY) ? !test.signer : test.signer];
        message = MESSAGE;
        if (test.fault == BAD_SEED)    key.seed[0] = !key.seed[0];
        if (test.fault == BAD_ROOT)    key.root[0] = !key.root[0];
        if (test.fault == BAD_MESSAGE) message[0]  = !message[0];
    end

    // -------------------------------------------------------------------------
    // License beat stream
    // -------------------------------------------------------------------------
    // Producer contract: a strict valid/ready handshake -- a presented beat
    // stays presented, unchanged, until it is accepted (see rtl_sva.sv). The
    // producer may pause before it presents the next one, and releases valid
    // once the last beat has been accepted.

    // Each signer's signature over MESSAGE, signed once before the first test
    license_t licenses [SLH_NUM_KEYS];

    logic        sending  = 1'b0;  // a licence is under way
    logic        pause    = 1'b0;  // no beat is presented this cycle
    int unsigned beat_idx = 0;

    wire beats_done = (beat_idx == TOTAL_BEATS);
    wire sig_valid  = sending && !pause && !beats_done;
    wire sig_ready;

    logic [SLH_BEAT_W-1:0] sig_data;

    always_comb begin
        sig_data = license_beat(licenses[test.signer], int'(beat_idx));
        if ((test.fault == BAD_BEAT) && (beat_idx == test.beat))
            sig_data[beat_idx % SLH_BEAT_W] = !sig_data[beat_idx % SLH_BEAT_W];
    end

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            beat_idx <= 0;
            pause    <= 1'b0;
        end else if (!sending) begin
            beat_idx <= 0;
            pause    <= 1'b0;
        end else begin
            if (sig_valid && sig_ready) beat_idx <= beat_idx + 1;
            // A pause starts or ends only between beats
            if (!sig_valid || sig_ready) pause <= test.pauses && ($urandom_range(3) != 0);
        end
    end

    // -------------------------------------------------------------------------
    // DUT
    // -------------------------------------------------------------------------

    logic dut_done;
    logic dut_verif_passed;

    hss_verify #(
        .SCH (hbsv_ctrl_pkg::SCHEME_SLH_128S)
    ) u_dut (
        .clk          (clk),
        .rst_n        (rst_n),
        .message      (message),
        .valid        (sig_valid),
        .ready        (sig_ready),
        .data         (sig_data),
        .identifier   (key.seed),
        .root_pub_key (key.root),
        .verify_done  (dut_done),
        .verif_passed (dut_verif_passed)
    );

    // -------------------------------------------------------------------------
    // Test sequencer
    // -------------------------------------------------------------------------

    localparam int TIMEOUT = 1_000_000;

    typedef enum logic [2:0] {
        PH_INIT,
        PH_START,
        PH_WAIT,
        PH_NEXT,
        PH_DONE
    } ph_e;

    // sim_main.cpp clocks the testbench once before it resets it; the
    // sequencer stays put until the reset has been seen
    logic reset_done = 1'b0;

    ph_e phase      = PH_INIT;
    int  wait_cnt   = 0;
    int  pass_count = 0;
    int  fail_count = 0;

    wire expect_pass = (test.fault == GENUINE);

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            phase      <= PH_INIT;
            reset_done <= 1'b1;
            sending    <= 1'b0;
            test_idx   <= 0;
            wait_cnt   <= 0;
            pass_count <= 0;
            fail_count <= 0;
        end else if (reset_done) begin
            case (phase)
                PH_INIT: begin
                    $display("=== SLH-DSA-SHA2-128s Verification Test ===");
                    for (int s = 0; s < int'(SLH_NUM_KEYS); s++)
                        licenses[s] <= slh_sign(MESSAGE, s);
                    phase <= PH_START;
                end

                PH_START: begin
                    sending  <= 1'b1;
                    wait_cnt <= 0;
                    phase    <= PH_WAIT;
                end

                PH_WAIT: begin
                    wait_cnt <= wait_cnt + 1;

                    if (dut_done) begin
                        sending <= 1'b0;
                        // Whatever the verdict, the whole licence is consumed
                        if ((dut_verif_passed == expect_pass) && beats_done) begin
                            if (expect_pass)
                                $display("PASS  [T%0d %s] accepted in %0d cycles",
                                         test_idx + 1, describe(test), wait_cnt);
                            else
                                $display("PASS  [T%0d %s] rejected in %0d cycles",
                                         test_idx + 1, describe(test), wait_cnt);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T%0d %s] verif_passed=%0b, %0d of %0d beats taken",
                                     test_idx + 1, describe(test), dut_verif_passed,
                                     beat_idx, TOTAL_BEATS);
                            fail_count <= fail_count + 1;
                        end
                        phase <= PH_NEXT;
                    end else if (wait_cnt > TIMEOUT) begin
                        $display("FAIL  [T%0d %s] timeout after %0d cycles, %0d of %0d beats",
                                 test_idx + 1, describe(test), wait_cnt, beat_idx, TOTAL_BEATS);
                        $fatal;
                    end
                end

                // The beat pointer rewinds while the next test is selected
                PH_NEXT: begin
                    if (test_idx == NUM_TESTS - 1) begin
                        phase <= PH_DONE;
                    end else begin
                        test_idx <= test_idx + 1;
                        phase    <= PH_START;
                    end
                end

                PH_DONE: begin
                    $display("");
                    if (fail_count == 0) begin
                        $display("All %0d SLH-DSA verification tests passed.", pass_count);
                        $finish;
                    end else begin
                        $display("SLH-DSA verification: %0d passed, %0d FAILED.",
                                 pass_count, fail_count);
                        $fatal;
                    end
                end

                default: ;
            endcase
        end
    end

endmodule
