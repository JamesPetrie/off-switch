// TRNG - True Random Number Generator (counter-based prototype)
//
// Generates 256-bit nonces for use as ECDSA message hashes.
// In production this would use a ring oscillator (e.g. Vasyltsov et al.);
// here a free-running counter is used for deterministic simulation.
//
// Protocol:
//   1. Assert enable to run the counter
//   2. Pulse request_new to latch the current counter value; nonce_valid rises
//   3. nonce is stable until the next request_new pulse
//   4. For simulation: assert load_seed for one cycle to seed the counter

module trng
    import arith_pkg::*; // import in module header to be used in port list
(
    input  logic             clk,
    input  logic             rst_n,
    input  logic             enable,
    input  logic             request_new,
    input  logic             load_seed,
    input  logic [WIDTH-1:0] seed,

    output logic [WIDTH-1:0] nonce,
    output logic             nonce_valid
);

    logic [WIDTH-1:0] counter_q;
    logic [WIDTH-1:0] nonce_q;
    logic             nonce_valid_q;

    // counter
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            counter_q     <= '0;
        end else begin
            if      (load_seed) counter_q <= seed;
            else if (enable)    counter_q <= counter_q + 1;
        end
    end

    // sampling
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            nonce_q       <= '0;
            nonce_valid_q <= 1'b0;
        end else begin
            if (request_new) begin
                nonce_q       <= counter_q;
                nonce_valid_q <= 1'b1;
            end
        end
    end

    assign nonce       = nonce_q;
    assign nonce_valid = nonce_valid_q;

endmodule
