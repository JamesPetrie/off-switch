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
             0: 256'h0e2de492299e8156dfacbf1ec03bbcb52599ede943788bed3920d55728eb1e34,
             1: 256'h82c3921882f45aa372c5a0a179026a37ab37c4d38eaf362c3bd00c66fd9334df,
             2: 256'hadba34ba09cd0ccfa87b06db382ae56c01c33dda9353101b97ebed53cd72f34b,
             3: 256'hac7e002764095bde7aad441861a35c1b43160707c7c694c43cadd512ff2ee2e3,
             4: 256'h717fa7feab85a0530ff203c357d7f4dfea8718aa1140ac4483bb0bd4b53aea38,
             5: 256'hd7c241fd4ccc716ccaf74d875f412cf36abd0d8904ade866ad80f145ec5407d0,
             6: 256'hb29c3dcc944f13f1b1f070c83924a0582896473d5844f5694c6a1cf5d6a866f0,
             7: 256'h8ca6470cc80da05012cc7afd833a77c529824c9e3927264585b864259b3ece80,
             8: 256'hcdbf66f9838c333acb89393b2b91071f12fd4ff440adc971e9100296201c9c5c,
             9: 256'h65ba8116a0e1a92be8218d5706bb47487f4f2e3e03002e9f6d6f481542e1c9df,
            10: 256'hda62bd591104856f9a2ec77bcf0aa8cf70c6dd2ab27412ac56717a45372bf441,
            11: 256'hc05fab0cac40f5ce84d4666c392efe7199dd204f075a7ded2a7a0523960240bc,
            12: 256'hf26762ea763c1733a1a30a7af78230182e38d8abbfa0e7d1337f66aee3eba688,
            13: 256'had46cdfded589a821ec611c38544f3e2d3aa79e20c9caf06e46365d65d2b9d3a,
            14: 256'h7c3307dcf2d58ed3de594d77cfafd07f955f8e935a60c04e27e272aa5a403721,
            15: 256'he8ea5539f2e2a97385fc67ee58e06452f5814927784fd9994882d5d88bd1ff6e,
            16: 256'h4bb1d2e0943b956aacdcdca044bef49302a95760f094ebd55b9848149c129917,
            17: 256'h2d9d6a92ffebbf39a75f9cb06ded43e6b457d0ddfa0f7fd19f68d8c2fc269bd1,
            18: 256'h5248aca1d28ba43e8d2f06c609bffa700e1355860209b1b5fd9df89274f61548,
            19: 256'h4b989de6a2f1375c356414a0390b429c87ac31e297354bfa550c7ca97b13d9e8,
            20: 256'h1cf524de022aa1d3ccd6655797dd99e692e9471cc898765d3bdbd0394b6f827f,
            21: 256'h5ecb607fdd644fe1e7054aee620ed3866d5d8c5246d93b3cc4321ac3bacc2b56,
            22: 256'h999497c46e6a987771e44851fc89fdbc7c16c70535aec9d74d3d9f476cca50bc,
            23: 256'hc448b625002da22f57385838a0af7ff19c9f1b2949552e08f9e7028f43e1b948,
            24: 256'h13db9981798fd544385e44334390434bcd07885f527829557eefefc105f324d8,
            25: 256'h6940fe1e194db390947835f5596529a9b950078f902d77408e979f2621333ec5,
            26: 256'h5b3e28e058b54acaa76341672dbe9c823ba8194af396ab8d1cffd58f108c67e2,
            27: 256'hf71544dd12ec9659b579871d341b0e48e4de10e4ed84d3c001f1e3e2021c1555,
            28: 256'h96d08adc070a0c963057a63ac6b6550c5cff32470c25d945ba541ca9bb36f14b,
            29: 256'h4ec09ccc34246687955f05cb00b899c97a81027e1168432a127061ce23c65461,
            30: 256'hb6b414251cf1e68a90e59d6d69873cde55a8a27263a580522743f2658094920b,
            31: 256'h34f7c494ecdf5bdb3b691d4e0bb7b1719b89b44c7305b18afb00f58a3c1777e6,
            32: 256'h6716bdca8af8d614d4fb002009ebdbb988a893ce3bc92d168df119a99e467366,
            33: 256'h5699094123021c7920326789738bb988fee851655a8f81572b239e77bddbe5cc
        },
        auth_path: '{
            0: 256'h16a07337b0e9698fba159da2615cd486c9aff5c3c158a17afb83c5fc7a7893f8,
            1: 256'h73606768f67c693edc536ff1c75f233ee9e597bc83dc089d9acdb8c772ec449c,
            2: 256'h71cb251154f21ba3ce401ffbc63c430ea98eccf5a3b9b7f3a70e872958ba7ea2,
            3: 256'h66f7d9a0d2c34f8eeedd0e73a75b39518bbfed788ebe83103bcc97b8cb1b09a2,
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
