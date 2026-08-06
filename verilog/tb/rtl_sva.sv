// Handshake assertions for the license stream.
//
// Bound into the RTL rather than carried inside it, so the synthesisable
// sources stay free of verification-only code. Pulled in by the simulation
// build; the lint targets do not see it.

module security_block_sva (
    input logic clk,
    input logic license_valid,
    input logic license_ready,
    input logic publishing
);
    // Beats are only taken once a verification is under way.
    assert property (@(posedge clk) publishing |-> !license_ready);
endmodule

module hss_verify_sva (
    input logic clk,
    input logic ready,
    input logic verify_done
);
    // Completing a verification and asking for another beat are different
    // events; they must never coincide. This belongs here rather than at the
    // top level: for ECDSA the two are deliberately the same signal.
    assert property (@(posedge clk) verify_done |-> !ready);
endmodule

bind security_block security_block_sva u_sva (
    .clk           (clk),
    .license_valid (license_valid),
    .license_ready (license_ready),
    .publishing    (state_q == StPublishAndWait)
);

bind hss_verify hss_verify_sva u_sva (
    .clk         (clk),
    .ready       (ready),
    .verify_done (verify_done)
);
