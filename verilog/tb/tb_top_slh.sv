// tb_top_slh.sv — Security block integration test with SLH-DSA (CRYPTO_TYPE=2)
//
// The licences are signed at run time by the slhdsa-c reference signer
// through DPI-C (tb_slh_sign_pkg), over the nonce the block publishes.
//
// Covered is what depends on the scheme -- the beat stream into the verifier,
// the public key per signer, the nonce as the signed message and the replay
// protection. tb_top_hss.sv and tb_top_ecdsa.sv cover the rest of the block.

`include "tb_slh_sign_pkg.sv"

module tb (
    input logic clk,
    input logic rst_n
);
    import arith_pkg::*;
    import slh_pkg::*;
    import tb_slh_sign_pkg::*;

    // Note: number of signers is hardcoded in the testcases!
    localparam int unsigned NUM_SIGNERS = 2;

    // -------------------------------------------------------------------------
    // Tests
    // -------------------------------------------------------------------------
    // One licence per step. The block does not show which signer it expects,
    // so a licence that must not count is followed by the one that would
    // complete the pair if it had.

    typedef struct {
        string name;
        int    signer;   // whose licence is submitted
        logic  stale;    // it signs the nonce of before the last rotation
        logic  corrupt;  // one bit of it is inverted
        logic  pauses;   // the producer pauses between beats
        logic  grants;   // completes the pair: allowance granted, nonce rotated
    } step_t;

    localparam int unsigned NUM_STEPS = 11;
    // A const variable, not a parameter: Verilator 5.044 crashes on a
    // parameter of an unpacked struct type (5.046 does not)
    const step_t STEPS [NUM_STEPS] = '{
        // Only signer 0's licence opens the pair: had either of the first
        // two counted, signer 1's would complete it in the third
        '{name: "signer 1 before signer 0",
          signer: 1, stale: 1'b0, corrupt: 1'b0, pauses: 1'b0, grants: 1'b0},
        '{name: "signer 0, one bit inverted",
          signer: 0, stale: 1'b0, corrupt: 1'b1, pauses: 1'b0, grants: 1'b0},
        '{name: "signer 1, still before signer 0",
          signer: 1, stale: 1'b0, corrupt: 1'b0, pauses: 1'b0, grants: 1'b0},
        // Signer 0's licence counts once, signer 1's completes the pair
        '{name: "signer 0",
          signer: 0, stale: 1'b0, corrupt: 1'b0, pauses: 1'b0, grants: 1'b0},
        '{name: "signer 0 again, as signer 1",
          signer: 0, stale: 1'b0, corrupt: 1'b0, pauses: 1'b0, grants: 1'b0},
        '{name: "signer 1",
          signer: 1, stale: 1'b0, corrupt: 1'b0, pauses: 1'b0, grants: 1'b1},
        // The nonce has rotated. The licence that opened the granted pair is
        // a replay: had it counted, signer 1's would complete a pair
        '{name: "signer 0, replayed on the new nonce",
          signer: 0, stale: 1'b1, corrupt: 1'b0, pauses: 1'b0, grants: 1'b0},
        '{name: "signer 1 after the replay",
          signer: 1, stale: 1'b0, corrupt: 1'b0, pauses: 1'b0, grants: 1'b0},
        // A second pair, on the new nonce and with pauses in the beat
        // stream; the replay of signer 1's licence does not complete it
        '{name: "signer 0 on the new nonce, with pauses",
          signer: 0, stale: 1'b0, corrupt: 1'b0, pauses: 1'b1, grants: 1'b0},
        '{name: "signer 1, replayed on the new nonce",
          signer: 1, stale: 1'b1, corrupt: 1'b0, pauses: 1'b0, grants: 1'b0},
        '{name: "signer 1 on the new nonce, with pauses",
          signer: 1, stale: 1'b0, corrupt: 1'b0, pauses: 1'b1, grants: 1'b1}
    };

    // The first two tests are the initial state and the nonce
    localparam int unsigned FIRST_STEP_TEST = 3;

    int unsigned step_idx = 0;
    step_t       step;
    assign step = STEPS[step_idx];

    // -------------------------------------------------------------------------
    // DUT signals
    // -------------------------------------------------------------------------

    logic             license_valid  = 1'b0;
    license_t         license        = 0;
    logic [WIDTH-1:0] trng_seed      = '0;
    logic             trng_load_seed = 1'b0;

    logic [WIDTH-1:0] nonce;
    logic             nonce_ready;
    logic [7:0]       workload_result;
    logic             result_valid;
    logic [63:0]      allowance;
    logic             enabled;

    // ---- License beat stream ----
    // Producer contract: a licence is a fixed number of beats on a strict
    // valid/ready handshake -- a presented beat stays presented, unchanged,
    // until it is accepted (see rtl_sva.sv) -- and valid is released once the
    // last beat has been accepted. The producer may pause before it presents
    // the next beat. license_valid is held for the whole transaction; the
    // pointer rewinds between transactions.
    localparam int unsigned CORRUPT_BEAT = TOTAL_BEATS / 2;

    logic        pause    = 1'b0;  // no beat is presented this cycle
    int unsigned beat_idx = 0;
    wire         license_ready;
    wire         beats_done = (beat_idx == TOTAL_BEATS);
    wire         beat_valid = license_valid && !pause && !beats_done;

    logic [SLH_BEAT_W-1:0] license_data;

    always_comb begin
        license_data = license_beat(license, int'(beat_idx));
        if (step.corrupt && (beat_idx == CORRUPT_BEAT)) license_data[0] = !license_data[0];
    end

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            beat_idx <= 0;
            pause    <= 1'b0;
        end else if (!license_valid) begin
            beat_idx <= 0;
            pause    <= 1'b0;
        end else begin
            if (beat_valid && license_ready) beat_idx <= beat_idx + 1;
            // A pause starts or ends only between beats
            if (!beat_valid || license_ready) pause <= step.pauses && ($urandom_range(3) != 0);
        end
    end

    // nonce_ready falls when the block takes the first beat and rises again
    // when the verification cycle ends -- that rising edge is the completion
    // event the sequencer waits on.
    logic nonce_ready_q;
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) nonce_ready_q <= 1'b0;
        else        nonce_ready_q <= nonce_ready;
    end
    wire cycle_done = nonce_ready && !nonce_ready_q;

    security_block #(
        .CRYPTO_TYPE(2),
        .NUM_SIGNERS(NUM_SIGNERS)
    ) u_dut (
        .clk            (clk),
        .rst_n          (rst_n),
        .license_valid  (beat_valid),
        .license_ready  (license_ready),
        .license_data   (license_data),
        .workload_valid (1'b0),
        .workload_a     ('0),
        .workload_b     ('0),
        .trng_seed      (trng_seed),
        .trng_load_seed (trng_load_seed),
        .nonce          (nonce),
        .nonce_ready    (nonce_ready),
        .workload_result(workload_result),
        .result_valid   (result_valid),
        .allowance      (allowance),
        .enabled        (enabled)
    );

    // -------------------------------------------------------------------------
    // Constants
    // -------------------------------------------------------------------------

    localparam logic [WIDTH-1:0] TRNG_SEED = (
        // random looking 256-bit value (SHA-256 initial hash state)
        256'h6a09e667_bb67ae85_3c6ef372_a54ff53a_510e527f_9b05688c_1f83d9ab_5be0cd19
    );
    localparam int NONCE_TIMEOUT  = 300;
    localparam int VERIFY_TIMEOUT = 1_000_000;

    // -------------------------------------------------------------------------
    // Test sequencer
    // -------------------------------------------------------------------------

    typedef enum logic [2:0] {
        PH_INIT,
        PH_NONCE,
        PH_SUBMIT,
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

    // State before the licence under test, and the nonce of before the last
    // rotation
    logic             saved_enabled;
    logic [63:0]      saved_allow;
    logic [WIDTH-1:0] saved_nonce;
    logic [WIDTH-1:0] stale_nonce = '0;

    // A granting licence enables the block, adds to the allowance and rotates
    // the nonce; any other leaves the nonce alone and the allowance does not
    // grow.
    wire step_ok = step.grants
        ? (enabled && (allowance > saved_allow) && (nonce != saved_nonce))
        : ((enabled == saved_enabled) && (allowance <= saved_allow) && (nonce == saved_nonce));

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            phase          <= PH_INIT;
            reset_done     <= 1'b1;
            trng_seed      <= TRNG_SEED;
            trng_load_seed <= 1'b1;
            license_valid  <= 1'b0;
            license        <= 0;
            step_idx       <= 0;
            wait_cnt       <= 0;
            pass_count     <= 0;
            fail_count     <= 0;
        end else if (reset_done) begin
            case (phase)
                // -------------------------------------------------------
                // T1: Initial state
                // -------------------------------------------------------
                PH_INIT: begin
                    $display("=== SLH-DSA security_block Test (%0d signers) ===", NUM_SIGNERS);
                    trng_load_seed <= 1'b0;
                    if (allowance == '0 && enabled == 1'b0) begin
                        $display("PASS  [T1 initial state] allowance=0 enabled=0");
                        pass_count <= pass_count + 1;
                    end else begin
                        $display("FAIL  [T1 initial state] allowance=%0d enabled=%0b",
                                 allowance, enabled);
                        fail_count <= fail_count + 1;
                    end
                    wait_cnt <= 0;
                    phase    <= PH_NONCE;
                end

                // -------------------------------------------------------
                // T2: The block publishes a nonce
                // -------------------------------------------------------
                PH_NONCE: begin
                    wait_cnt <= wait_cnt + 1;
                    if (nonce_ready) begin
                        $display("PASS  [T2 nonce_ready] nonce=0x%h", nonce);
                        pass_count <= pass_count + 1;
                        phase      <= PH_SUBMIT;
                    end else if (wait_cnt > NONCE_TIMEOUT) begin
                        $fatal(1, "FAIL  [T2 nonce_ready] timeout");
                    end
                end

                // -------------------------------------------------------
                // T3 onwards: one licence per step, signed over the nonce
                // the block publishes -- or, for a replay, over the one it
                // published before its last rotation: the signature is
                // deterministic, so that is the licence granted then
                // -------------------------------------------------------
                PH_SUBMIT: begin
                    if (nonce_ready) begin
                        saved_enabled <= enabled;
                        saved_allow   <= allowance;
                        saved_nonce   <= nonce;
                        license       <= slh_sign(step.stale ? stale_nonce : nonce, step.signer);
                        license_valid <= 1'b1;
                        wait_cnt      <= 0;
                        phase         <= PH_WAIT;
                    end
                end

                PH_WAIT: begin
                    wait_cnt <= wait_cnt + 1;
                    if (cycle_done) begin
                        license_valid <= 1'b0;
                        if (nonce != saved_nonce) stale_nonce <= saved_nonce;
                        if (step_ok && beats_done) begin
                            if (step.grants)
                                $display("PASS  [T%0d %s] allowance granted, nonce rotated",
                                         FIRST_STEP_TEST + step_idx, step.name);
                            else
                                $display("PASS  [T%0d %s] no grant, nonce kept",
                                         FIRST_STEP_TEST + step_idx, step.name);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T%0d %s] %0d of %0d beats taken",
                                     FIRST_STEP_TEST + step_idx, step.name,
                                     beat_idx, TOTAL_BEATS);
                            $display("      enabled=%0b (was %0b) allowance=%0d (was %0d)",
                                     enabled, saved_enabled, allowance, saved_allow);
                            $display("      nonce=0x%h (was 0x%h)", nonce, saved_nonce);
                            fail_count <= fail_count + 1;
                        end
                        phase <= PH_NEXT;
                    end else if (wait_cnt > VERIFY_TIMEOUT) begin
                        $fatal(1, "FAIL  [T%0d %s] timeout, %0d of %0d beats taken",
                               FIRST_STEP_TEST + step_idx, step.name, beat_idx, TOTAL_BEATS);
                    end
                end

                // The beat pointer rewinds while the next step is selected
                PH_NEXT: begin
                    if (step_idx == NUM_STEPS - 1) begin
                        phase <= PH_DONE;
                    end else begin
                        step_idx <= step_idx + 1;
                        phase    <= PH_SUBMIT;
                    end
                end

                PH_DONE: begin
                    $display("");
                    if (fail_count == 0) begin
                        $display("All %0d SLH-DSA security_block tests passed.", pass_count);
                        $finish;
                    end else begin
                        $display("SLH-DSA security_block: %0d passed, %0d FAILED.",
                                 pass_count, fail_count);
                        $fatal;
                    end
                end

                default: ;
            endcase
        end
    end

endmodule
