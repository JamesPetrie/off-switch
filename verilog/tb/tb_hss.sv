// tb_hss.sv — HSS-LMS signature verification test
//
// Test vectors from reference_lms.py via test_hss_verify.ml.
// Loads full license, pulses valid, waits for ready, checks verif_passed.

module tb (
    input logic clk,
    input logic rst_n
);
    import arith_pkg::*;
    import hss_pkg::*;

    // -------------------------------------------------------------------------
    // Test vector constants
    // -------------------------------------------------------------------------

    localparam logic [WIDTH-1:0] MESSAGE =
        256'h0b98309ccea6343bf486b4b04ec7d7b7a5b5adda1edf46e43a15b5e99edc21b4;

    localparam license_t LICENSE = '{
        leaf_index: 32'd5,
        randomizer: 256'hcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc,
        sig_chains: '{
             0: 256'h3a480283407063ca6a57ad7201dce50eed9d7ffe3eb8eeff0d8729ab6c69138e,
             1: 256'hbda0576ef69ede2a299e5909cdf76c4beb21c752584ed14aeaab9a832176ad82,
             2: 256'had53eea01adbc6cc2843dd2339e21bc3db8d7631fafc3c90172b9e349afc00fd,
             3: 256'h3b49a4edd1a859efde2412d990101c5295c43a747e0bfedaf6316ce9dfa2df37,
             4: 256'ha31c08a41d14877e0900e1edfcfd9cda520d2099be46a19fdfd98314c97b7a6c,
             5: 256'hed985ecad9eff85bb39578751b35ac960605155f7087f1bd6e93c54a4c475431,
             6: 256'hb0a8911e31adc20f59baed39d27cfea6263a904f57313d394870edd2fccea3c4,
             7: 256'he905292f112d21bbdf2f09e512f81e20321974d3b89f6b37a1f60dd99f7382db,
             8: 256'h8646c92a0c0e4b5c483dbd8cf27b93406bcc80fb53aa252115a5542350bee06b,
             9: 256'hb2e5aef13580eea74b5d923672f6cc6e7aa84a62bd90cd673a501a6897c49acd,
            10: 256'h9f57d5bd1a6f74584f7f60ffea7a9d06c99377783b09868a515b884230b7c6a9,
            11: 256'hb9a0e9c4dc02ea6619a0353fb0e5b30dfad2038a5b3b6736a03f97cd8de79854,
            12: 256'hdc86e794edbb8a7103ab5b2837e1f4c1bab260d6399bb4d8e698bffbba5f2663,
            13: 256'hca689a94cb780d89cf76906b98c88bbd2a953d65eb9200695b8d7d937678c9f5,
            14: 256'h8294eee9ee409fc48839b6eaae7f3f6dab91bdc039879333754a8b58d636929c,
            15: 256'h10dd2807334d8df653995b3bab70d66f79e5ba2a5ef751992706c7aad52989f2,
            16: 256'hb41f98dea875f68edffece8623e2de6225f9e2ddd8e982d5f11342371662bc51,
            17: 256'h747bdb5fb0b81e3b0db585e6d0065024cf2146a5a21f76686da03e8faf850a79,
            18: 256'h848544d345860ab1e2d8518a45dd26dcd924d74b088b8ef21e73d59dc996adcb,
            19: 256'ha88c0b422826e37d1815eb1e6b3da2129e26c0b992fd4f0112c362a160b7b59e,
            20: 256'hfe117086153eae51639f815ce11aa2df07f969b37a6712879499da1e445bcb63,
            21: 256'h3eff23af41275547be3cb3021b57872ef1c9855c323bb3826afccc7a4dfbcf2c,
            22: 256'hbe723107964340550c5d5e79a37fbf9c893ad88060310387dfbfb12800b28080,
            23: 256'h3bc358a50ac437946044353efc8922f1925dc4b05f43979fdf8e03b3045493e0,
            24: 256'h771c28c482520e0fd639e0076e2c6b2d054124f90acb2690154555116e8d796f,
            25: 256'h5cbfcc9e4287814f56573038875ba5539ea72c028636f55489c014cee30df046,
            26: 256'hb0086d805aa6698d8d0e25dcc26e5a6924c4842ece8666fd1dfaa8feaabf236d,
            27: 256'h59ccc128fc44877abbf74b50ec940f73ef1a405d36b1917e50bbcaac0c044db7,
            28: 256'h8fb3c75fc722075b140236c472062befafd6e27fc8f9e9eb75cff151b29e957c,
            29: 256'h75e2b540b4eee1d133180c386658833d5ebcd0655d7b3f5d53dd547c4c050d90,
            30: 256'h9ff47e0c4d3410a85214ddba0b769d8af436eb54943a2e788f076d8a257d525b,
            31: 256'heb3e49cc6204bf7b4493a756bd5a3eb9551fd2009464b3161f2f7917c83049b5,
            32: 256'he7b8728e14a0c7e19b0881311b01383329e4a64a7bf88cb1de2a002c547da905,
            33: 256'hebd37232f7e264d3fce5c4731cbcfcf83a1080380f4a53d0a48a634b98632f01
        },
        auth_path: '{
            0: 256'h88e8870b6c5c462d0b438df1d9ec4fbf0da2dc0968b7908c943840d19342f63b,
            1: 256'hbb2e3fd7fa4a51914aec2b24dafcdf68c292f2b019021f47cc2320dbf8174bb9,
            2: 256'h04e412c0e49cf794208062b8917d60d21d4410f738792bf3f3e2472a0de192ba,
            3: 256'hae749d878fe57433df878f2b4155c32cfec2358d46b64d8bca3759e5802a6a2d,
            default: '0
        }
    };

    // -------------------------------------------------------------------------
    // DUT
    // -------------------------------------------------------------------------

    logic             dut_valid = 1'b0;
    logic             dut_ready;
    logic             dut_verif_passed;
    logic             saved_verif_passed = 1'b0;

    hss_verify u_dut (
        .clk          (clk),
        .rst_n        (rst_n),
        .valid        (dut_valid),
        .message      (MESSAGE),
        .license      (LICENSE),
        .ready        (dut_ready),
        .verif_passed (dut_verif_passed)
    );

    // -------------------------------------------------------------------------
    // Test sequencer
    // -------------------------------------------------------------------------

    localparam int TIMEOUT = 2_000_000;

    typedef enum logic [2:0] {
        PH_INIT,
        PH_START,
        PH_WAIT,
        PH_CHECK,
        PH_DONE
    } ph_e;

    ph_e phase   = PH_INIT;
    int  wait_cnt = 0;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            phase     <= PH_INIT;
            dut_valid <= 1'b0;
            wait_cnt  <= 0;
        end else begin
            case (phase)
                PH_INIT: begin
                    $display("=== HSS-LMS Verification Test ===");
                    phase <= PH_START;
                end

                PH_START: begin
                    dut_valid <= 1'b1;
                    phase     <= PH_WAIT;
                    wait_cnt  <= 0;
                end

                PH_WAIT: begin
                    dut_valid <= 1'b0;
                    wait_cnt  <= wait_cnt + 1;

                    if (dut_ready) begin
                        $display("  Completed in %0d cycles", wait_cnt);
                        saved_verif_passed <= dut_verif_passed;
                        phase <= PH_CHECK;
                    end else if (wait_cnt > TIMEOUT) begin
                        $display("FAIL  [timeout] after %0d cycles", wait_cnt);
                        $fatal;
                    end
                end

                PH_CHECK: begin
                    if (saved_verif_passed) begin
                        $display("PASS  [HSS-LMS verification] signature valid");
                    end else begin
                        $display("FAIL  [HSS-LMS verification] signature rejected");
                    end
                    phase <= PH_DONE;
                end

                PH_DONE: begin
                    $finish;
                end

                default: ;
            endcase
        end
    end

endmodule
